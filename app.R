library(shiny) 
library(shinyWidgets)
library(rvest)
library(dplyr) 
library(ggplot2)
library(googlesheets4)
library(DT)
library(lubridate)
library(readr)
library(tidyverse)

# authorize google sheet
gs4_auth(path = Sys.getenv("GCP_CREDENTIALS"))

# Google sheet URL (this is where the weekly picks are stored)
sheet_url <- "https://docs.google.com/spreadsheets/d/1rdaKGprdxuOKntnZYZrcsvU6Th9HNBumQSD13BhIXlI/edit?pli=1&gid=0#gid=0" 

test <- read_sheet(sheet_url)

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
scrape_pga_prize_money <- function(golfer_name) { 
  
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
ui <- fluidPage( 
  titlePanel("Golf Knowledge: 2025 Season Earnings Game"), 
  sidebarLayout( 
    sidebarPanel("Enter tournament picks:",
      hr(),           
      selectInput("event_name", "Tournament", choices = event_list), 
      selectInput("player_name", "Name", choices = c("Conor", "Shane", "Sean", "Chris")), 
      textInput("golfer1", "Golfer 1", ""), 
      textInput("golfer2", "Golfer 2", ""), 
      actionButton("submit", "Submit Picks"),
      textOutput("thank_you_msg"),
      div(style = "height: 30vh;"),
      actionButton("update_data", "Update Prize Money"),
      h5("Press this to get latest tournament prize money from the web..."),
      progressBar(id = "progress", value = 0, total = 100, display_pct = TRUE)),
    mainPanel(plotOutput("leaderboard_plot"),
              hr(),
              plotOutput("time_series_plot"), 
              hr(),
              DTOutput("picks_table")
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
               all_golfers_selected <- test %>%
                 select(golfer1, golfer2) %>%
                 pivot_longer(cols = c("golfer1", "golfer2")) %>%
                 unique() %>%
                 pull()
               
               # get specific url for golfer
               player_links <- scrape_top_200_payer_ids()
                
               earnings_list <- list() 
               for(g in all_golfers_selected){
                 result <- scrape_pga_prize_money(g)
                 earnings_list[[g]] <- result
               }
               earnings <- do.call(rbind, earnings_list) 
                 
      
               # Update earnings 
               df <- test %>% 
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
               showNotification("Scraping complete!...For any technical support please contact Rajeesh Masala pashwari naan Prescott",
                                type = "message", 
                                duration = 10)
               }
               ) 
  # Leaderboard 
  output$leaderboard_plot <- renderPlot({ 
    df <- data() %>% 
      group_by(player_name) %>% 
      summarise(TotalEarnings = sum(earnings_g1, na.rm = TRUE)) %>% 
      arrange(desc(TotalEarnings)) 
    
    ggplot(df, aes(x = reorder(player_name, TotalEarnings), y = TotalEarnings)) + 
      geom_bar(stat = "identity", fill = "blue") + 
      coord_flip() + 
      labs(title = "Leaderboard", x = "Player", y = "Total Earnings") + 
      theme_minimal() 
    }
    ) 
  
  # Time series plot 
  output$time_series_plot <- renderPlot({ 
    df <- data() %>%
      group_by(event_name, player_name) %>% 
      summarise(TotalEarnings = sum(earnings_g1, na.rm = TRUE)) 
    
    ggplot(df, aes(x = event_name, y = TotalEarnings, color = player_name, group = player_name)) +
      geom_line() + 
      geom_point() + 
      labs(title = "Earnings Over Time", x = "Date", y = "Total Earnings") + 
      theme_minimal() 
    }) 
  
  # Weekly picks table
  
  output$picks_table <- renderDT({ 
    datatable(data()) 
    }
    ) 
} 

rsconnect::deployApp(appName = "name-of-app", appDir = "directory/where/my/app.R")

# Add dependency file: his tells the cloud which version of R to use
rsconnect::writeManifest()
