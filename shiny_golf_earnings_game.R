library(shiny) 
library(shinycssloaders)
library(rvest)
library(dplyr) 
library(ggplot2)
library(googlesheets4)
library(DT)
library(lubridate)
library(readr)
library(tidyverse)

# autorize google sheet
# gs4_auth()

# Google sheet URL (this is where the weekly picks are stored)
sheet_url <- "https://docs.google.com/spreadsheets/d/1rdaKGprdxuOKntnZYZrcsvU6Th9HNBumQSD13BhIXlI/edit?pli=1&gid=0#gid=0" 

test <- read_sheet(sheet_url)

  

# Function to scrape prize money data 
scrape_pga_prize_money <- function(golfer_name) { 
  
  # get urls for top 200 players in world
  top_200_page <- read_html("https://www.espn.com/golf/rankings")
  
  player_links <- top_200_page %>%
    html_nodes(".AnchorLink") %>%
    html_attr("href") %>%
    .[grepl("/golf/player/_/id/", .)]
  
  # convert input golfer name to url style
  golfer_name_lower <- gsub(" ", "-", tolower(golfer_name))
  
  # get specific url for golfer
  player_link <- player_links[grepl(golfer_name_lower, player_links)]
  
  player_page <- read_html(player_link)
  
  # get earnings data for this player
  earnings <- player_page %>% 
    html_nodes(".tar.Table__TD") %>%
    html_text() %>%
    first()
  
  data <- data.frame(golfer_name = golfer_name, earnings = earnings) %>%
    mutate(earnings = as.numeric(gsub("[\\$,]", "", earnings)))
  
  return(data)
  }

# Function to update Google Sheet 
update_google_sheet <- function(new_data) { 
  sheet_append(sheet_url, new_data) } 

event_list <- read_csv("data/events.csv")$event_name

# UI 
ui <- fluidPage( 
  titlePanel("Golf Knowledge: 2025 Earnings Game"), 
  sidebarLayout( 
    sidebarPanel( 
      selectInput("event_name", "Select Tournament", choices = event_list), 
      selectInput("player_name", "Your Name", choices = c("Conor", "Shane", "Sean", "Chris")), 
      textInput("golfer1", "Golfer 1", ""), 
      textInput("golfer2", "Golfer 2", ""), 
      actionButton("submit", "Submit Picks"), 
      hr(), 
      hr(),
      actionButton("update_data", "Update Prize Money"),
      h5("Press this to get latest tournament prize money from the web...")),
    mainPanel( 
      tabsetPanel( 
        tabPanel("Leaderboard", 
                 plotOutput("leaderboard_plot")),
        tabPanel("Time Series", 
                 plotOutput("time_series_plot")), 
        tabPanel("Weekly Picks", 
                 DTOutput("picks_table")) 
        ) 
      ) 
    )
  )

# Server 
server <- function(input, output, session) {
  # Load data from Google Sheets 
  data <- reactiveVal(read_sheet(sheet_url)) 
  
  observeEvent(input$submit, { 
    new_entry <- tibble( Date = Sys.Date(), 
                         event_name = input$event_name, 
                         player_name = input$player_name, 
                         golfer1 = input$golfer1, 
                         golfer2 = input$golfer2)
    
    update_google_sheet(new_entry) 
    
    data(read_sheet(sheet_url)) 
    
    }) 
  
  observeEvent(input$update_data, 
               {
               
               # Get earnings data for latest week only
               # loop the web scraping function over all golfers selected for this week
               all_golfers_selected <- data() %>% 
                 filter(event_name == "Genesis Invitational") %>%
                 select(golfer1, golfer2) %>%
                 pivot_longer(cols = c("golfer1", "golfer2")) %>%
                 pull()
                
               earnings_list <- list() 
               for(g in all_golfers_selected){
                 result <- scrape_pga_prize_money(g)
                 earnings_list[[g]] <- result
               }
               earnings <- do.call(rbind, earnings_list) %>%
                 mutate(event_name = "Genesis Invitational")
                 
      
               # Update earnings 
               df <- data() %>% 
                 left_join(earnings, by = c("event_name", "golfer1" = "golfer_name")) %>%
                 mutate(earnings_g1 = earnings) %>%
                 select(-earnings) %>%
                 left_join(earnings, by = c("event_name", "golfer2" = "golfer_name")) %>%
                 mutate(earnings_g2 = earnings) %>%
                 select(-earnings)
                             
                             
               sheet_write(df, ss = sheet_url, sheet = "Sheet1") 
               
               # Save updated data 
               data(df) 
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

shinyApp(ui, server)