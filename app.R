library(shiny) 
library(shinydashboard)
library(shinyWidgets)
library(rvest)
library(dplyr) 
library(DT)
library(ggplot2)
library(googlesheets4)
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

# test <- read_sheet(sheet_url)
#############################################################
# Necessary functions
#############################################################

# Function to get the Player ID's of the top 200 golfers (needed to scrape earnings data on espn.com)
# no longer needed as a csv file has been created containinng urls for the data golf rankings top players
# I did this due to the many incorrect spelling names in espn urls, so a function with a player name as the input didnt work all the time
# scrape_top_200_payer_ids <- function(){
#   
#   # get urls for top 200 players in world
#   top_200_page <- read_html("https://www.espn.com/golf/rankings")
#   
#   player_links <- top_200_page %>%
#     html_nodes(".AnchorLink") %>%
#     html_attr("href") %>%
#     .[grepl("/golf/player/_/id/", .)]
#   
#   return(player_links)
# }

# Function to scrape prize money data 
scrape_pga_prize_money <- function(golfer_name) { 
  
  # find player name in player url file (player_urls.csv)
  # this has the espn.com urls for the top 300 players according to data golf (so includes liv players)
  player_link <- read.csv("data/player_urls.csv") %>%
    filter(dg_ranking_name == golfer_name) %>%
    select(url) %>%
    pull()
  
  # read the player page on espn.com
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

event_list <- read_csv("data/events_test.csv")$event_name

masters_theme <- create_theme(
  adminlte_color(
    light_blue = "#004D40", # Change the primary color to Masters Green
  ),
  adminlte_sidebar(
    dark_bg = "#004D40",    # Sidebar background color
    dark_hover_bg = "#00332A" # Sidebar hover color
  ),
  adminlte_global(
    content_bg = "#FFFFFF"  # Background color for the content area
  )
)
#############################################################
# UI 
#############################################################
ui <- dashboardPage(
 
  # Dashboard header
  dashboardHeader(
    title = tags$span(
      tags$img(src = "pif.png", height = "30px", style = "margin-right: 30px;"),
      tags$img(src = "pga.png", height = "30px", style = "margin-right: 30px;")
  )),
  
  # Dashboard sidebar
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Enter Picks", tabName = "picks", icon = icon("user")),
      menuItem("Dashboard", tabName = "main", icon = icon("dashboard")),
      menuItem("Game Rules", tabName = "game_rules", icon = icon("info-circle"))
    )
  ),
  
  # Dashboard body
  dashboardBody(
    # apply master green to background
    use_theme(masters_theme),
    # Add custom CSS to change sidebar color
    tags$head(
      tags$style(HTML("
        /* Change the box header color to Masters Yellow */
        .box.box-solid.box-primary > .box-header {
          background-color: #A5D6A7;
          color: #000000; /* Text color for the box header */
        }
        /* Change the box border color to Masters Yellow */
        .box.box-solid.box-primary {
          border: 1px solid #A5D6A7;
        }
      ")),
      tags$style(HTML("
      .circle-image {
        border-radius: 50%; /* Makes the image circular */
        width: 50px;       /* Adjust the width to make the image smaller */
        height: 50px;      /* Adjust the height to make the image smaller */
        object-fit: cover; /* Ensures the image covers the circular area */
        margin: 5px;       /* Adds some spacing between images */
      }
    "))
    ),
    
    tabItems(
      #################
      # Picks tab
      #################
      tabItem(
        tabName = "picks",
        h1("Enter tournament picks:"),
        selectInput("event_name", "Tournament", choices = event_list),
        selectInput("player_name", "Name", choices = c("Conor", "Shane", "Sean", "Chris", "Phil", "Eddie")),
        selectizeInput(inputId = "golfer1",
                       label = "Golfer 1", 
                       choices = read.csv("data/datagolf_rankingsFEB2025.csv")$player_name,
                       options = list(placeholder = 'Type to search...', maxOptions = 10)),
        selectizeInput(inputId = "golfer2",
                       label = "Golfer 2", 
                       choices = read.csv("data/datagolf_rankingsFEB2025.csv")$player_name,
                       options = list(placeholder = 'Type to search...', maxOptions = 10)),
        actionButton("submit", "Submit Picks"),
        textOutput("thank_you_msg"),
        hr(),
        div(style = "height: 20vh;"),
        actionButton("update_data", "Update Prize Money"),
        h5("Press this to get latest tournament prize money from the web..."),
        progressBar(id = "progress", value = 0, total = 100, display_pct = TRUE)
      ),

      #################
      # Main dashboard tab
      #################
      tabItem(
        tabName = "main",
        fluidRow(
          # Collapsible boxes for each player's picks
          box(
            title = tagList(
              img(src = "conor.jfif", height = "60px", class = "circle-image"),
              "Conor's Picks"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("conor_picks_table")
          ),
          box(
            title = tagList(
              img(src = "shane.jfif", height = "60px", class = "circle-image"),
              "Shane's Picks"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("shane_picks_table")
          ),
          box(
            title = tagList(
              img(src = "sean.jfif", height = "60px", class = "circle-image"),
              "Sean's Picks"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("sean_picks_table")
          ),
          box(
            title = tagList(
              img(src = "chris.jfif", height = "60px", class = "circle-image"),
              "Chris's Picks"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("chris_picks_table")
          ),
          box(
            title = tagList(
              img(src = "eddie.jfif", height = "60px", class = "circle-image"),
              "Eddie's Picks"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("eddie_picks_table")
          ),
          box(
            title = tagList(
              img(src = "phil.jfif", height = "60px", class = "circle-image"),
              "Phil's Picks"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("phil_picks_table")
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
          ),
          box(
            title = "Cumulative Earnings",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("cumulative_plot")
          ),
          box(
            title = "Top 3",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("top_3_plot")
          )
        )
      ),
      
      #################
      # Game Rules Tab
      #################
      tabItem(
        tabName = "game_rules",
        h2("Golf Knowledge: Earnings Game 2025"),
        tags$ul(
          h3("Game rules:"),
          tags$li("€50 entry"),
          tags$li("You must pick two players for each tournament"),
          tags$li("You can pick players in advance"),
          tags$li("You can only pick each player once throughout the season"),
          tags$li("Winner will be the player with the most earnings at the end of the season (after Tour Championship)"),
          tags$li("€250 for winner"),
          tags$li("€50 for second place (money back)"),
          h2("Sponsors:"),
          tags$img(src = "baboost.jfif"),
          tags$img(src = "referhop.png"),
          tags$img(src = "pif.png", height="50%", width="50%"),
          tags$img(src = "pga.png", height="50%", width="50%")
        )
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
  }, colnames = FALSE)
  
  output$shane_picks_table <- renderTable({
    data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(player_name == "Shane",
             earnings_g1 == 0,
             earnings_g2 == 0) %>%
      select(event_name, golfer1, golfer2)
  }, colnames = FALSE)
  
  output$sean_picks_table <- renderTable({
    data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(player_name == "Sean",
             earnings_g1 == 0,
             earnings_g2 == 0) %>%
      select(event_name, golfer1, golfer2)
  }, colnames = FALSE)
  
  output$chris_picks_table <- renderTable({
    data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(player_name == "Chris",
             earnings_g1 == 0,
             earnings_g2 == 0) %>%
      select(event_name, golfer1, golfer2)
  }, colnames = FALSE)
  
  output$phil_picks_table <- renderTable({
    data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(player_name == "Phil",
             earnings_g1 == 0,
             earnings_g2 == 0) %>%
      select(event_name, golfer1, golfer2)
  }, colnames = FALSE)
  
  output$eddie_picks_table <- renderTable({
    data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(player_name == "Eddie",
             earnings_g1 == 0,
             earnings_g2 == 0) %>%
      select(event_name, golfer1, golfer2)
  }, colnames = FALSE)
  
  
  
  
  observeEvent(input$submit, { 
    
    
    new_entry <- tibble( Date = Sys.Date(), 
                         event_name = input$event_name, 
                         player_name = input$player_name, 
                         golfer1 = input$golfer1, 
                         golfer2 = input$golfer2,
                         earnings_g1 = NA,
                         earnings_g2 = NA,
                         event_occured = FALSE)
    
    update_google_sheet(new_entry) 
    
    data(read_sheet(sheet_url)) 
    
    # show message once submitted
    thank_you_text("Picks submitted!")
    
    output$thank_you_msg <- 
      renderText({thank_you_text()})
    
    }) 
  
  observeEvent(input$update_data, 
               {
               
               showNotification("Scraping PGA earnings data...Please wait!", type = "message", duration = 25)
                 
                 # start progress bar
                 updateProgressBar(session, id = "progress", value = 0)
                 
                    for (i in seq(1, 100, by = 10)){
                      Sys.sleep(5)
                      
                      updateProgressBar(session, id = "progress", value = i)
                    }
                 
               # Get earnings data
               # loop the web scraping function over all golfers selected for this week
               all_golfers_selected <- data() %>%
                 # filter(is.na(earnings_g1) | is.na(earnings_g2)) %>% # only scrape player data for records that have dont have any earnings figures yet
                 select(golfer1, golfer2) %>%
                 pivot_longer(cols = c("golfer1", "golfer2")) %>%
                 unique() %>%
                 pull()
               
               # get specific url for golfer
               earnings_list <- list() 
               for(g in all_golfers_selected){
                 print(g)
                 result <- scrape_pga_prize_money(g)
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
                 mutate(event_occured = if_else(!is.na(earnings_g1) | !is.na(earnings_g2), TRUE, event_occured)) %>% # if a prize money result was found, the event occured so update this col
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
  ####
  # Leaderboard 
  ####
  output$leaderboard_plot <- renderHighchart({ 
    df <- data() %>% 
      # only use latest pick per player / tournament
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
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
  ####
  # Time series plot 
  ####
  output$time_series_plot <- renderHighchart({ 
    df <- data() %>%
      # only use latest pick per player / tournament
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      # only show data for events that have occurred
      filter(event_occured) %>% 
      left_join(read.csv("data/events_test.csv"), by = "event_name") %>%
      group_by(order, event_name, player_name) %>% 
      summarise(TotalEarnings = sum(earnings_g1 +earnings_g2, na.rm = TRUE)) %>%
      arrange(order) %>%
      mutate(event_name = factor(event_name, levels = unique(event_name[order(order)])))
    
    hchart(df, "line", 
           hcaes(x = event_name, y = TotalEarnings, group = player_name)) %>%
      hc_title(text = "Earnings, by tournament") %>%
      hc_xAxis(title = list(text = "Tournament"), 
               categories = levels(df$event_name)) %>%
      hc_yAxis(title = list(text = "Total Earnings")) %>%
      hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
      hc_tooltip(shared = TRUE, valueSuffix = " $") 
    }) 
  
  ####
  # cumulative earnings time series plot
  ####
  output$cumulative_plot <- renderHighchart({
  df <- data() %>%
    # only use latest pick per player / tournament
    group_by(player_name, event_name) %>%
    slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
    # only show data for events that have occurred
    filter(event_occured) %>% 
    left_join(read.csv("data/events_test.csv"), by = "event_name") %>%
    group_by(order, event_name, player_name) %>% 
    summarise(TotalEarnings = sum(earnings_g1 +earnings_g2, na.rm = TRUE)) %>%
    group_by(player_name) %>%
    arrange(order) %>%
    mutate(TotalEarnings = cumsum(TotalEarnings)) %>%
    ungroup() %>%
    arrange(order) %>%
    mutate(event_name = factor(event_name, levels = unique(event_name[order(order)])))
    
    hchart(df, "line", 
           hcaes(x = event_name, y = TotalEarnings, group = player_name)) %>%
      hc_title(text = "Cumulative Earnings") %>%
      hc_xAxis(title = list(text = "Tournament"), 
               categories = levels(df$event_name)) %>%
      hc_yAxis(title = list(text = "Total Earnings")) %>%
      hc_plotOptions(line = list(marker = list(enabled = TRUE))) %>%
      hc_tooltip(shared = TRUE, valueSuffix = " $")
    })
  
  ####
  # TOP 3
  ####
  output$top_3_plot <- renderHighchart({
  df <- data() %>%
    # only look at events that have been played
    filter(event_occured) %>%
    # only use latest pick per player / tournament
    group_by(player_name, event_name) %>%
    slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
    mutate(event_pick_g1 = paste(golfer1, "@", event_name),
           event_pick_g2 = paste(golfer2, "@", event_name)) %>%
    ungroup() %>%
    select(player_name, event_pick_g1, event_pick_g2, earnings_g1, earnings_g2) %>%
    pivot_longer(
      cols = starts_with("event_pick") | starts_with("earnings"),
      names_to = c(".value", "game"),
      names_sep = "_g"
    ) %>%
    group_by(player_name) %>%
    slice_max(order_by = earnings, n = 3, with_ties = FALSE) %>%
    arrange(desc(earnings), .by_group = TRUE) %>%
    ungroup() %>%
    mutate(event_pick = factor(event_pick, levels = unique(event_pick))) 
  
  
  hchart(df, "bar", hcaes(x = player_name, y = earnings, group = event_pick)) %>%
    hc_chart(inverted = TRUE) %>%  # Make the chart horizontal
    hc_xAxis(title = list(text = "Player Name")) %>%
    hc_yAxis(title = list(text = "Earnings")) %>%
    hc_title(text = "Top 3 picks") %>%
    hc_tooltip(
      pointFormat = "<b>{point.event_pick}</b><br>Earnings: ${point.y:,.0f}"
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE, format = "${point.y:,.0f}")))
  })        
} 

shinyApp(ui = ui, server = server)

# rsconnect::deployApp(appName = "name-of-app", appDir = "directory/where/my/app.R")

# Add dependency file: his tells the cloud which version of R to use
#rsconnect::writeManifest()
