library(shiny) 
library(shinydashboard)
library(shinyWidgets)
library(rvest)
library(dplyr) 
library(ggplot2)
library(googlesheets4)
library(DT)
library(lubridate)
library(readr)
library(tidyverse)
library(highcharter)

# Use a Google sevice account to allow remote editing of google sheets 
# this is where data is stored for shiny app

# Authenticate using the service account key from the environment variable
json_path <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY")

# Authenticate using the service account credentials
token <- gargle::credentials_service_account(path = Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY"))

scopes <- c(
  "https://www.googleapis.com/auth/spreadsheets"  # Full access to sheets
)

# Authenticate with the service account and specify the scopes
gs4_auth(
  path = Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY"),
  scopes = scopes
) 


# Google sheet URL (this is where the weekly picks are stored)
sheet_url <- "https://docs.google.com/spreadsheets/d/1rdaKGprdxuOKntnZYZrcsvU6Th9HNBumQSD13BhIXlI/edit?pli=1&gid=0#gid=0" 

# 
# jsonlines <- readLines("service_key.json")
# jsonstring <- paste(jsonlines, collapse = "")
# jsonstring <- gsub('"', '\\"', jsonstring)
# cat(jsonstring)


#############################################################
# Necessary functions
#############################################################

# Function to get the Player ID's of the top 200 golfers (needed to scrape earnings data on espn.com)
scrape_top_200_payer_ids <- function(){
  
  # get urls for top 200 players in world
  top_200_page <- read_html("https://www.espn.com/golf/rankings")
  
  player_links <- top_200_page %>%
    html_nodes(".AnchorLink") %>%
    html_attr("href") %>%
    .[grepl("/golf/player/_/id/", .)]
  
  return(player_links)
}

# Function to scrape prize money data 
scrape_pga_prize_money <- function(golfer_name, player_links) { 
  
  # convert input golfer name to url style
  golfer_name_lower <- gsub(" ", "-", tolower(golfer_name))
  
  # read the player page on espn.com
  player_link <- player_links[grepl(golfer_name_lower, player_links)]
  player_page <- read_html(player_link)
  
  # get earnings data for this player
  earnings <- player_page %>% 
    html_nodes(".tar.Table__TD") %>%
    html_text()
  
  # get corresponding tournament name
  tournaments <- player_page %>%
    html_nodes(".LastTournamentTable__name") %>%
    html_nodes(".AnchorLink") %>%
    html_text() 
  
  # combine
  data <- data.frame(golfer_name = golfer_name, event_name = tournaments, earnings = earnings) %>%
    mutate(earnings = if_else(earnings == "--", "0", earnings), # handle NAs (from missed cuts)
           earnings = as.numeric(gsub("[\\$,]", "", earnings))) 
  
  return(data)
  }

# Function to update Google Sheet 
update_google_sheet <- function(new_data) { 
  sheet_append(sheet_url, new_data) } 

event_list <- read_csv("data/events.csv")$event_name


#############################################################
# UI 
#############################################################
ui <- dashboardPage(
  
  # Dashboard header
  dashboardHeader(title = "Golf Knowledge: 2025 Season Earnings Game", titleWidth = 450),
  
  # Dashboard sidebar
  dashboardSidebar(width = 200,
    tags$div(
      style = "padding: 15px;",
      "Enter tournament picks:",
      hr(),
      selectInput("event_name", "Tournament", choices = event_list),
      selectInput("player_name", "Name", choices = c("Conor", "Shane", "Sean", "Chris")),
      textInput("golfer1", "Golfer 1", ""),
      textInput("golfer2", "Golfer 2", ""),
      actionButton("submit", "Submit Picks"),
      textOutput("thank_you_msg"),
      hr(),
      div(style = "height: 20vh;"),
      actionButton("update_data", "Update Prize Money"),
      h5("Press this to get latest tournament prize money from the web..."),
      progressBar(id = "progress", value = 0, total = 100, display_pct = TRUE)
    )
  ),
  
  # Dashboard body
  dashboardBody(
    # Add custom CSS to change sidebar color
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    
    fluidRow(
      # Collapsible boxes for each player's picks
      box(
        title = "Conor's Picks",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        tableOutput("conor_picks_table")
      ),
      box(
        title = "Shane's Picks",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        tableOutput("shane_picks_table")
      ),
      box(
        title = "Sean's Picks",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        tableOutput("sean_picks_table")
      ),
      box(
        title = "Chris's Picks",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        tableOutput("chris_picks_table")
      )
    ),
    fluidRow(
      box(
        title = "Leaderboard",
        status = "primary",
        solidHeader = TRUE,
        highchartOutput("leaderboard_plot")
      ),
      box(
        title = "Time Series",
        status = "primary",
        solidHeader = TRUE,
        highchartOutput("time_series_plot")
      )
    )
  )
)

#############################################################
# Server 
#############################################################
server <- function(input, output, session) {
  # Load data from Google Sheets 
  data <- reactiveVal(read_sheet(sheet_url)) 
  
  # hide thank you text before button is pressed
  thank_you_text <- reactiveVal((""))

  
  # Render tables for each player's picks
  output$conor_picks_table <- renderTable({
    data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(player_name == "Conor",
             earnings_g1 == 0,
             earnings_g2 == 0) %>%
      select(event_name, golfer1, golfer2)
  })
  
  output$shane_picks_table <- renderTable({
    data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(player_name == "Shane",
             earnings_g1 == 0,
             earnings_g2 == 0) %>%
      select(event_name, golfer1, golfer2)
  })
  
  output$sean_picks_table <- renderTable({
    data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(player_name == "Sean",
             earnings_g1 == 0,
             earnings_g2 == 0) %>%
      select(event_name, golfer1, golfer2)
  })
  
  output$chris_picks_table <- renderTable({
    data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(player_name == "Chris",
             earnings_g1 == 0,
             earnings_g2 == 0) %>%
      select(event_name, golfer1, golfer2)
  })
  
  
  
  observeEvent(input$submit, { 
    
    
    new_entry <- tibble( Date = Sys.Date(), 
                         event_name = input$event_name, 
                         player_name = input$player_name, 
                         golfer1 = input$golfer1, 
                         golfer2 = input$golfer2)
    
    update_google_sheet(new_entry) 
    
    data(read_sheet(sheet_url)) 
    
    # show message once submitted
    thank_you_text("Picks submitted!")
    
    output$thank_you_msg <- 
      renderText({thank_you_text()})
    
    }) 
  
  observeEvent(input$update_data, 
               {
               
               showNotification("Scraping PGA earnings data...Please wait!", type = "message", duration = 2)
                 
                 # start progress bar
                 updateProgressBar(session, id = "progress", value = 0)
                 
                    for (i in seq(1, 100, by = 10)){
                      Sys.sleep(5)
                      
                      updateProgressBar(session, id = "progress", value = i)
                    }
                 
               # Get earnings data
               # loop the web scraping function over all golfers selected for this week
               all_golfers_selected <- data() %>%
                 select(golfer1, golfer2) %>%
                 pivot_longer(cols = c("golfer1", "golfer2")) %>%
                 unique() %>%
                 pull()
               
               # get specific url for golfer
               player_links <- scrape_top_200_payer_ids()
                
               earnings_list <- list() 
               for(g in all_golfers_selected){
                 result <- scrape_pga_prize_money(g, player_links)
                 earnings_list[[g]] <- result
               }
               earnings <- do.call(rbind, earnings_list) 
                 
      
               # Update earnings 
               df <- data() %>% 
                 left_join(earnings, by = c("event_name", "golfer1" = "golfer_name")) %>%
                 mutate(earnings_g1 = earnings) %>%
                 select(-earnings) %>%
                 left_join(earnings, by = c("event_name", "golfer2" = "golfer_name")) %>%
                 mutate(earnings_g2 = earnings) %>%
                 select(-earnings) %>%
                 replace_na(list(earnings_g1 = 0, earnings_g2 = 0))
                             
                             
               sheet_write(df, ss = sheet_url, sheet = "Sheet1") 
               
               # remove duplicates: only keep latest picks for each player and event
               df <- df %>%
                 group_by(player_name, event_name) %>%
                 slice_max(order_by = input_date, n = 1, with_ties = FALSE)
               
               # Save updated data 
               data(df) 
               
               #simulate final scraped progress bar
               showNotification("Scraping complete!...For any technical support please contact Rajeesh Masala Peshwari Naan Prescott",
                                type = "message", 
                                duration = 10)
               }
               ) 
  # Leaderboard 
  output$leaderboard_plot <- renderHighchart({ 
    df <- data() %>% 
      group_by(player_name) %>% 
      summarise(Total_Earnings = sum(earnings_g1 + earnings_g2, na.rm = TRUE)) %>% 
      arrange(desc(Total_Earnings)) 
    
    hchart(df, "column", 
           hcaes(x = player_name, y = Total_Earnings, group = player_name)) %>%
      hc_title(text = "Total") %>%
      hc_xAxis(title = list(text = "Player")) %>%
      hc_yAxis(title = list(text = "Earnings")) %>%
      hc_plotOptions(column = list(
        stacking = "normal"
      ))
    }
    ) 
  
  # Time series plot 
  output$time_series_plot <- renderHighchart({ 
    df <- data() %>%
      left_join(read.csv("data/events.csv"), by = "event_name") %>%
      group_by(order, event_name, player_name) %>% 
      summarise(TotalEarnings = sum(earnings_g1 +earnings_g2, na.rm = TRUE)) %>%
      filter(TotalEarnings > 0) %>%
      arrange(order) %>%
      mutate(event_name = factor(event_name, levels = unique(event_name))) 
    
    hchart(df, "line", 
           hcaes(x = event_name, y = TotalEarnings, group = player_name, color = player_name)) %>%
      hc_title(text = "Earnings Over Time") %>%
      hc_xAxis(title = list(text = "Event")) %>%
      hc_yAxis(title = list(text = "Total Earnings")) %>%
      hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
      hc_tooltip(shared = TRUE, valueSuffix = " $") 
    }) 
  
} 

shinyApp(ui = ui, server = server)

# rsconnect::deployApp(appName = "name-of-app", appDir = "directory/where/my/app.R")

# Add dependency file: his tells the cloud which version of R to use
#rsconnect::writeManifest()
