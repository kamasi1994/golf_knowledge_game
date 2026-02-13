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
library(fresh)
library(stringi)
library(randomForest)

# get functions
source("golf_app_functions.R")

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

#############
# get list of future events
#############
event_list <- read_csv("data/events.csv", show_col_types = FALSE) %>%
  filter(as.POSIXct(deadline, format = "%d/%m/%Y/%H:%M") > as.POSIXct(Sys.time())) %>% 
  select(event_name) %>%
  pull()

#############
# styling
#############
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
      menuItem("Enter Picks", tabName = "picks", icon = icon("user"), selected = TRUE),
      menuItem("Live Leaderboard", tabName = "leaderboard", icon = icon("chart-line")),
      menuItem("Standings", tabName = "main", icon = icon("dashboard")),
      menuItem("Feeling lucky?", tabName = "coin", icon = icon("dice")),
      menuItem("Game Rules", tabName = "game_rules", icon = icon("info-circle"))
    )
  ),
  
  # Dashboard body
  dashboardBody(
    # apply master green to background
    use_theme(masters_theme),
    
    # Add custom CSS to change sidebar color
    tags$head(
      # Add the viewport meta tag for mobile devices
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      
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
        h1(textOutput("current_event"), style = "text-align: center; font-size: 30px; font-weight: bold; color: #004D40; "),
        selectInput("player_name", "Name", choices = c("Conor", "Shane", "Sean", "Chris", "Phil", "Eddie", "Jive", "Mark", "Ross", "John", "Jack")),
        selectizeInput(inputId = "golfer1",
                       label = "Golfer 1", 
                       selected = NULL,
                       choices = read.csv("data/dg_rankings_jan2026.csv")$name,
                       options = list(placeholder = 'Type to search...', maxOptions = 10)),
        selectizeInput(inputId = "golfer2",
                       label = "Golfer 2", 
                       selected = NULL,
                       choices = read.csv("data/dg_rankings_jan2026.csv")$name,
                       options = list(placeholder = 'Type to search...', maxOptions = 10)),
        uiOutput("submit_button"),
        textOutput("thank_you_msg"),
        uiOutput("have_i_picked"),
        h4("Previously selected golfers", style = "text-align: center; font-size: 30px; font-weight: bold; color: #004D40; "),  
        DTOutput("prev_picked"),
        h4(textOutput("event_odds_name"), style = "text-align: center; font-size: 30px; font-weight: bold; color: #004D40; "),  
        DTOutput("event_odds"),
        tags$div(style = "font-size: 8px; color: grey; text-align: center; margin-top: 20px;",
                 "Terms & Conditions: By using this platform, you agree to your personal data being sold to third parties. Albatross Analytics Ltd has 
                 contracts with biotech artifical organ research labs, advanced cybernetics firms, US government survelliance sub-contractors, far right think tanks, and Kildare County Council. By proceeding,
                 you grant Albatross Analytics Ltd irrevocable rights to distribute your personal data, IP addresses, and geo-locations to these third party partners.")
      ),
      
      #################
      # Live leaderboard tab
      #################
      tabItem(
        tabName = "leaderboard",
        tags$head(
          tags$style(HTML("
                          table.dataTable thead { background-color: #004D40; color: #FFFFFF; }
                          table.dataTable { background-color: #FFFFFF; }
                          table.dataTable tbody tr:nth-child(even) { background-color: #F8F6F0; }
                          table.dataTable tbody tr:nth-child(odd) { background-color: #FFFFFF; }
                          table.dataTable tbody td { font-size: 16px; font-weight: bold; }
                          "))
        ),
        DTOutput("leaderboard"),
        conditionalPanel(
          condition =  "output.live_data_available",
          highchartOutput("live_earnings_prediction")
        ),
        conditionalPanel(
          condition = "!output.live_data_available",
          h3("No active tournament data available. Please check back later!")
        )
      ),

      #################
      # Main dashboard tab
      #################
      tabItem(
        tabName = "main",
        
        fluidRow(
          h3("Selections for next event", style = "text-align: center; font-size: 30px; font-weight: bold; color: #004D40; "),  
          box(
            title = "",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("selections_next_event")
          )
        ),
        fluidRow(
          h3("Standings", style = "text-align: center; font-size: 30px; font-weight: bold; color: #004D40; "),
          box(
            title = "",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("leaderboard_plot")
          ),
          box(
            title = "",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("time_series_plot")
          ),
          box(
            title = "",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("cumulative_plot")
          ),
          box(
            title = "",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("big_dog_sankey")
          ),
          box(
            title = "",
            status = "primary",
            solidHeader = TRUE,
            highchartOutput("top_25_plot")
          )
        )
      ),
      
      #################
      # Feeling lucky Tab
      #################
      tabItem(
        tabName = "coin",
        tags$img(src = "coinflip_new.jpg", width = "80%", height = "auto"),
        h2("Are you feeling lucky?"),
        h4("Toss the coin to double your earnings for the previous week. If you lose, your earnings are set to €0 for that week"),
        h4("Only your first attempt is recorded. Any subsequent tosses are ignored"),
        
        # Custom CSS for the coin animation
        tags$head(
          tags$style(HTML("
        
        @keyframes flip {
          0% { transform: rotateY(0deg); }
          50% { transform: rotateY(1800deg); } /* Spin multiple times */
          100% { transform: rotateY(3600deg); }
        }
        .coin {
          width: 100px;
          height: 100px;
          background-color: gold;
          border-radius: 50%;
          display: inline-block;
          animation: flip 2s ease-in-out; /* Animation duration */
        }
        .result {
          font-size: 24px;
          margin-top: 20px;
          text-align: center;
        }
        
        /* Las Vegas-style button */
        .btn-las-vegas {
          font-size: 32px;               /* Larger font size */
          padding: 20px 40px;            /* More padding to make the button bigger */
          background-color: #FFD700;     /* Gold background */
          color: #000;                  /* Black text */
          border: 3px solid #8B4513;     /* Brown border */
          border-radius: 10px;           /* Rounded corners */
          font-weight: bold;             /* Bold text */
          text-transform: uppercase;     /* Uppercase text */
          box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.3); /* Shadow for depth */
          transition: all 0.3s ease;    /* Smooth hover effect */
        }
        
        /* Hover effect */
        .btn-las-vegas:hover {
          background-color: #FFA500;     /* Orange background on hover */
          color: #FFF;                  /* White text on hover */
          box-shadow: 0px 6px 8px rgba(0, 0, 0, 0.4); /* Larger shadow on hover */
        }
      "))),
        
        # Input for user name
        selectInput("coin_user_name", "Enter Your Name", choices = c("Jive", "Conor", "Shane", "Sean", "Chris", "Phil", "Eddie", "Ross", "Mark", "John", "Jack")),
        
        # Radio buttons for user to choose Heads or Tails
        radioButtons("user_choice", "Choose Heads or Tails:",
                     choices = c("Heads", "Tails"),
                     selected = character(0)),  # No default selection
        
        # Button to flip the coin
        actionButton("flip_coin", "Toss Coin", class = "btn-las-vegas"),
        
        # Placeholder for the game result
        uiOutput("coin_animation"),
        uiOutput("game_result"),
        h4("Degenerate Gamblers (coin toss results)", style = "text-align: center; font-size: 30px; font-weight: bold; color: #004D40; "),  
        DTOutput("degenerate_gambler")
      ),
        
      #################
      # Game Rules Tab
      #################
      tabItem(
        tabName = "game_rules",
        tags$ul(
          h3("Game rules:"),
          tags$li("€50 entry"),
          tags$li("Pick two players for each tournament"),
          tags$li("Picks must be submitted by midday on the Thursday of each tournament (6am for the Open Championship"),
          tags$li("You can only pick each player once throughout the season"),
          tags$li("Winner will be the player with the most earnings at the end of the season (after Tour Championship)"),
          tags$li("Prizes TBD"),
          tags$li("After an official review into alleged abuse of the coin toss feature, it has been decided for the 2026 season that each player will be limited to 5 coin flips"),
          tags$li("These 5 coin flips may be used at any stage throughout the season. Per usual, only the first flip is recorded."),
          tags$li("Coin toss will be disabled for the FedEx Play-Off events (i.e. the last three events)"),
          h3(""),
          h2("Sponsors:"),
          tags$img(src = "baboost.jfif", width = "50%", height = "auto"),
          tags$img(src = "pif.png",  width = "50%", height = "auto"),
          tags$img(src = "pga.png",  width = "50%", height = "auto")
        )
      )
    )
  )
)
#############################################################
# Server 
#############################################################
server <- function(input, output, session) {
  
  #####################################
  # scam popups
  #####################################
  scam_images <- c(
    "worm_bet.jfif",
    "rathmore_scam.jfif",
  )
  
  observe({
    
    # Randomly select one image
    random_image <- sample(scam_images, 1)
    
    showModal(modalDialog(
      tags$div(style = "margin: - 15px - 20px -20px -20px;",
               tags$img(src = random_image,
                        style = "width: 100%; height: auto; display: block;")
      ), size = "s", easyClose = TRUE
    ))
  })
  
  # Load data from Google Sheets 
  data <- reactiveVal(read_sheet(sheet_url, sheet = "2026")) 
  
  # hide thank you text before button is pressed
  thank_you_text <- reactiveVal((""))
  
  # add table of picked golfers
  output$prev_picked <- renderDT({
  data() %>%
    filter(player_name == input$player_name,
           event_occured) %>%
    group_by(player_name, event_name) %>%
    slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>% # get latest pick per player x event
    ungroup() %>%
    select(player_name, event_name, golfer1, earnings_g1, golfer2, earnings_g2) %>%
    mutate(earnings_g1 = scales::label_dollar()(earnings_g1),
           earnings_g2 = scales::label_dollar()(earnings_g2)) %>%
      rename(golfer_g1 = golfer1,
             golfer_g2 = golfer2) %>%
    pivot_longer(cols = starts_with("golfer") | starts_with("earnings"),  # Columns to pivot
                 names_to = c(".value", "golfer_num"),  # Split column names into .value and golfer_num
                 names_sep = "_g") %>%  # Separate names at the underscore
    select(event_name, golfer, earnings) %>%
    datatable(
      colnames = c("Event", "Golfer", "Earnings"),
      escape = FALSE, 
      rownames = FALSE, 
      options = list(
        searching = FALSE, 
        paging = FALSE, 
        ordering = FALSE)
    )
  })
  
  output$current_event <- renderText({
    
    # current event is the next event in the list at a given time
    current_event <- read.csv("data/events.csv") %>%
      filter(Sys.time() <= as.POSIXct(deadline, format = "%d/%m/%Y/%H:%M")) %>%
      first() %>%
      pull(event_name)
    
    paste("Submit tournament picks for: ", current_event)
  })
  
  output$event_odds_name <- renderText({
    paste("Odds for ", get_odds()$event_odds_name)
  })
  
  # add a table of odds for the event
  output$event_odds <- renderDT({
    get_odds()$event_odds_table %>%
      select(-event_name) %>%
      datatable(
        colnames = c("Name", "Odds"),
        escape = FALSE, 
        rownames = FALSE, 
        options = list(
          searching = FALSE, 
          paging = FALSE, 
          ordering = FALSE)
      )
  })
  
  #######################################################
  # Live leaderboard
  #######################################################
  
  # live scores
  live_scores <- reactiveVal(get_live_scores())
  
  # Check if live leaderboard data is available
  output$live_data_available <- reactive({
    !is.null(live_scores())
  })
  
  outputOptions(output, "live_data_available", suspendWhenHidden = FALSE)
  
  output$leaderboard <- renderDT({
    
    req(nrow(data()) > 0)
    
    if(is.null(live_scores())){
      datatable(data.frame(
        Name = NA,
        Golfer = NA,
        Position = NA
      ),
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: center; font-size: 24px; font-weight: bold;",
      "Leaderboard not live"))
    } else{
      data() %>%
        filter(!event_occured) %>%
        left_join(read.csv("data/events.csv"), by = "event_name") %>%
        group_by(player_name, event_name) %>%
        slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>% # get latest pick per player x event
        group_by(player_name) %>%
        slice_min(order_by = order, n = 1, with_ties = FALSE) %>% # then get next un-played tournament
        ungroup() %>%
        select(event_name, player_name, golfer1, golfer2) %>%
        pivot_longer(cols = c("golfer1", "golfer2"),
                     values_to = "golfer") %>%
        select(event_name, player_name, golfer) %>%
        # temporary naming fix for the masters
        mutate(event_name = if_else(event_name == "The Masters", "Masters Tournament", event_name)) %>%
        left_join(live_scores(), by = c("event_name", "golfer")) %>%
        mutate(numeric_position  = as.numeric(sub("^T", "", position))) %>%
        arrange(numeric_position) %>%
        select(-numeric_position, -event_name) %>%
        datatable(caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center; font-size: 24px; font-weight: bold;",
          paste0("Live leaderboard: ", unique(live_scores()$event_name))
          ),
          escape = FALSE, 
                  rownames = FALSE, 
                  colnames = c("Name", "Golfer", "Position"), 
                  options = list(
          searching = FALSE, 
          paging = FALSE, 
          ordering = FALSE)
        )
    }
  })
  
  # # show live predicted earnings using random forest model
  # output$live_earnings_prediction <- renderHighchart({
  #   
  #   req(live_scores()) # Ensure data is available
  #   
  #   live_predicted_earnings <- data() %>%
  #     filter(!event_occured) %>%
  #     left_join(read.csv("data/events.csv"), by = "event_name") %>%
  #     group_by(player_name, event_name) %>%
  #     slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>% # get latest pick per player x event
  #     group_by(player_name) %>%
  #     slice_min(order_by = order, n = 1, with_ties = FALSE) %>% # then get next un-played tournament
  #     ungroup() %>%
  #     select(event_name, player_name, golfer1, golfer2) %>%
  #     pivot_longer(cols = c("golfer1", "golfer2"),
  #                  values_to = "golfer") %>%
  #     select(event_name, player_name, golfer) %>%
  #     # temporary naming fix for the masters
  #     mutate(event_name = if_else(event_name == "The Masters", "Masters Tournament", event_name)) %>%
  #     left_join(live_scores(), by = c("event_name", "golfer")) 
  # 
  #   # Clean the positions column
  #   live_predicted_earnings$adjusted_position <- gsub("T", "", live_predicted_earnings$position)  # Remove "T" from tied positions
  #   live_predicted_earnings$adjusted_position <- as.numeric(live_predicted_earnings$adjusted_position)     # Convert to numeric
  # 
  #   # retain predicive power of tied places
  #   # Adjust positions for ties
  #   live_predicted_earnings <- live_predicted_earnings %>%
  #     group_by(position) %>%
  #     mutate(adjusted_position = ifelse(n() > 1, mean(adjusted_position + 0:(n() - 1)), adjusted_position)) %>%
  #     ungroup() %>%
  #     select(event_name, player_name, position, adjusted_position)
  # 
  # 
  #   # Use the random forest model to predict earnings on live data
  #   predicted_earnings <- predict(earnings_pred_model, live_predicted_earnings) 
  #   
  # 
  #   live_predicted_earnings %>%
  #     mutate(predicted_earnings = if_else(adjusted_position > 65, 0, predicted_earnings)) %>%
  #     select(player_name, position, predicted_earnings) %>%
  #     group_by(player_name) %>%
  #     summarise(predicted_earnings = sum(predicted_earnings, na.rm = T)) %>%
  #     arrange(desc(predicted_earnings)) %>%
  #     hchart("bar", hcaes(x = player_name, y = predicted_earnings, color = player_name)) %>%
  #     hc_title(text = "Live Predicted Earnings") %>%
  #     hc_yAxis(title = list(text = "Total Predicted Earnings")) %>%
  #     hc_tooltip(
  #       pointFormat = "<b>{point.player_name}</b><br>predicted_earnings: ${point.y:,.0f}"
  #     ) 
  #     
  # })
  # 

  #############################
  # limit player selections to the Monday of tournament week
  #############################
  id_date_valid <- reactive({
    
    current_event_deadline <- read.csv("data/events.csv") %>%
      filter(Sys.time() <= as.POSIXct(deadline, format = "%d/%m/%Y/%H:%M")) %>%
      first() %>%
      pull(deadline)
    
    Sys.time() >= as.POSIXct(current_event_deadline, format = "%d/%m/%Y/%H:%M") - as.difftime(3, units = "days") &
      Sys.time() <= as.POSIXct(current_event_deadline, format = "%d/%m/%Y/%H:%M") 
  })
  
  output$submit_button <- renderUI({
    if (id_date_valid()){
      actionButton("submit", "Submit Picks", class = "btn btn-lg")
    } else {
      tagList(
        actionButton("submit", "Submit", class = "btn btn-lg", disabled = TRUE),
        div(class = "text-danger", "Submissions for the next tournament open on the Monday of tournament week")
      )
    }
  })
  
  observeEvent(input$submit, { 
    
    # current event is the next event in the list at a given time
    current_event <- read.csv("data/events.csv") %>%
      filter(Sys.time() <= as.POSIXct(deadline, format = "%d/%m/%Y/%H:%M")) %>%
      first() %>%
      pull(event_name)
    
    new_entry <- tibble( input_date = as.POSIXct(Sys.time()), 
                         event_name = current_event, 
                         player_name = input$player_name, 
                         golfer1 = input$golfer1, 
                         golfer2 = input$golfer2,
                         earnings_g1 = NA,
                         earnings_g2 = NA,
                         event_occured = FALSE,
                         coin_toss = FALSE,
                         scraped = FALSE) 
    
    # add odds
    odds <- get_odds()$event_odds_table %>%
      mutate(player_name = str_squish(player_name))
    
    new_entry <- new_entry %>%
      mutate(
        golfer1 = str_squish(golfer1),
        golfer2 = str_squish(golfer2)) %>%
      left_join(odds, by = c("golfer1" = "player_name", "event_name")) %>%
      mutate(odds_g1 = odds) %>%
      select(-odds) %>%
      left_join(odds, by = c("golfer2" = "player_name", "event_name")) %>%
      mutate(odds_g2 = odds) %>%
      select(-odds)

    # data before submission of new picks
    old <- data()
    
    # have I already picked this player text?
    output$have_i_picked <- renderUI({
      already_picked <- old %>%
        filter(player_name == input$player_name) %>%
        group_by(event_name) %>%
        slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        filter(golfer1 == input$golfer1 | golfer2 == input$golfer1 | golfer1 == input$golfer2 | golfer2 == input$golfer2) %>%
        count() %>%
        pull() > 0
      
      tags$p(if (already_picked) {
        tags$span("")
        tags$span("Warning! You may have already picked one of these players", style = "color: yellow;")
      } else {
        tags$span("Success", style = "color: green;")
      })
    })
    
    # update database
    append_google_sheet(new_entry) 
    
    
    # show message once submitted
    output$thank_you_msg <- renderText({thank_you_text()})
    thank_you_text("Picks submitted!")
    
    
    # reload data
    data(read_sheet(sheet_url, sheet = "2026"))
    
    }) 
  
  ####
  # Picks for next event
  ####
  output$selections_next_event <- renderDataTable({
    
    req(data() %>%
          filter(!event_occured) %>%
          nrow() > 0)
    
    odds <- get_odds()$event_odds_table %>%
      mutate(player_name = str_squish(player_name))
    
    data() %>%
      filter(!event_occured) %>%
      left_join(read.csv("data/events.csv"), by = "event_name") %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>% # get latest pick per player x event
      group_by(player_name) %>%
      slice_min(order_by = order, n = 1, with_ties = FALSE) %>% # then get next un-played tournament
      ungroup() %>%
      # add odds 
      left_join(odds, by = c("golfer1" = "player_name", "event_name")) %>%
      mutate(odds_g1 = odds) %>%
      select(-odds) %>%
      left_join(odds, by = c("golfer2" = "player_name", "event_name")) %>%
      mutate(odds_g2 = odds) %>%
      select(-odds) %>%
      mutate(
        prob_g1 = {
          x <- strsplit(odds_g1, "/")
          as.numeric(sapply(x, `[`, 1)) / as.numeric(sapply(x, `[`, 2))
        },
        prob_g2 = {
          x <- strsplit(odds_g2, "/")
          as.numeric(sapply(x, `[`, 1)) / as.numeric(sapply(x, `[`, 2))
        }
      ) %>%
      mutate(
        g1_red = prob_g1 >= 100,
        g2_red = prob_g2 >= 100
      ) %>%
      select(player_name, golfer1, golfer2, prob_g1, prob_g2, g1_red, g2_red) %>%
      datatable(caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left; color: #555; font-style: italic;',
        "Note: Players in red have odds over 100/1"),
        escape = FALSE, 
        rownames = FALSE, 
        colnames = c("Name", "Golfer 1", "Golfer 2"), 
        options = list(
          searching = FALSE, 
          paging = FALSE, 
          ordering = FALSE,
          columnDefs = list(
            list(visible = FALSE, targets = c("prob_g1", "prob_g2", "g1_red", "g2_red"))
          )
        )
      ) %>%
      formatStyle(
        "golfer1",
        valueColumns = "g1_red",
        color = styleEqual(c(FALSE, TRUE), c("black", "red"))
      ) %>%
      formatStyle(
        "golfer2",
        valueColumns = "g2_red",
        color = styleEqual(c(FALSE, TRUE), c("black", "red"))
      )
    
    
  })
  
  ####
  # Leaderboard 
  ####
  output$leaderboard_plot <- renderHighchart({ 
    
    req(nrow(data()) > 0)
    
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
      )) %>%
      hc_tooltip(shared = TRUE, valueSuffix = " $") 
    }
    ) 
  ####
  # Time series plot 
  ####
  output$time_series_plot <- renderHighchart({ 
    
    req(nrow(data()) > 0)
    
    df <-data() %>%
      # only use latest pick per player / tournament
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      # only show data for events that have occurred
      filter(event_occured) %>% 
      left_join(read.csv("data/events.csv"), by = "event_name") %>%
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
    
    req(nrow(data()) > 0)
    
    df <- data() %>%
      # only use latest pick per player / tournament
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      # only show data for events that have occurred
      filter(event_occured) %>% 
      left_join(read.csv("data/events.csv"), by = "event_name") %>%
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
  # TOP 25 picked players
  ####
  output$top_25_plot <- renderHighchart({#
    
    req(nrow(data()) > 0)
    
    top_25 <- data.frame(read.csv("data/dg_rankings_jan2026.csv")) %>%
      mutate(rank = row_number()) %>%
      slice_min(order_by = rank, n = 25)
    
    
    df <- data() %>%
      # only look at events that have been played
      filter(event_occured) %>%
      # only use latest pick per player / tournament
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(player_name, golfer1, golfer2) %>%
      pivot_longer(
        cols = starts_with("golfer"),
        names_to = "picks") %>%
      select(player_name, value) %>%
      inner_join(top_25, by = c("value" = "name")) %>%
      group_by(player_name) %>%
      summarise(nbr_top_25 = n()) %>%
      arrange(desc(nbr_top_25))
    
    
    
    hchart(df, "bar", hcaes(x = player_name, y = nbr_top_25, color = player_name)) %>%
      hc_chart(inverted = TRUE) %>%  # Make the chart horizontal
      hc_xAxis(title = list(text = "Player Name")) %>%
      hc_yAxis(title = list(text = "Count")) %>%
      hc_title(text = "Number of Top 25 ranked golfers picked") %>%
      hc_caption(text = "Footnote: Data Golf rankings January 2026", style = list(fontSize = "10px", color = "#777777"))
    
  })  
  
  ####
  # Who still has the big dog golfers
  ####
  
  output$big_dog_sankey <- renderHighchart({
    
    req(nrow(data()) > 0)
    
    big_dogs <- c("Rory McIlroy", "Scottie Scheffler", "Jon Rahm", "Tommy Fleetwood")
    
    big_dogs_picked <- data() %>%
      # only look at events that have been played
      filter(event_occured) %>%
      # only use latest pick per player / tournament
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(player_name, golfer1, golfer2) %>%
      pivot_longer(
        cols = starts_with("golfer"),
        names_to = "picks",
        values_to = "already_picked") %>%
      filter(already_picked %in% big_dogs) %>%
      mutate(already_picked_flag = TRUE) %>%
      select(player_name, already_picked, already_picked_flag)
    
    all_players <- c("Conor", "Chris", "Jive", "Eddie", "Phil", "Shane", "Sean", "Mark", "Ross", "John")
    
    
    sankey_data <- expand.grid(player_name = all_players,
               golfers = big_dogs) %>%
      anti_join(big_dogs_picked, by = c("player_name", "golfers" = "already_picked")) %>%
      rename(from = player_name,
             to = golfers) %>%
      mutate(weight = 1)
    
    highchart() %>%
      hc_chart(type = "sankey") %>%
      hc_add_series(
        data = sankey_data,
        type = "sankey",
        name = "Player to Golfer Connections"
      ) %>%
      hc_title(text = "Top Golfers Available") %>%
      hc_subtitle(text = "DataGolf.com top 5") %>%
      hc_plotOptions(
        sankey = list(
          dataLabels = list(
            enabled = TRUE,
            style = list(
              fontWeight = "bold",
              textOutline = "none"
            )
          ),
          nodeWidth = 20,
          nodePadding = 10
        )
      ) %>%
      hc_tooltip(
        headerFormat = "",
        pointFormat = "{point.from} → {point.to}"
      )
  })
    
  ########
  # coin flip processing
  ########
  
  # Reactive value to store the coin flip result
  coin_result <- reactiveVal(NULL)
  
  # determine whether there is an eligible event to flip
  # users can only flip from the Monday after an event ends until the next one starts
  eligible_event_to_flip <- {
    events <- read.csv("data/events.csv") %>%
      arrange(order) %>%
      mutate(
        deadline = as.POSIXct(deadline, format = "%d/%m/%Y/%H:%M"),
        coin_open_time = deadline + 4 * 24 * 60 * 60,
        next_event_start = lead(deadline))

        now <- Sys.time()

        events %>%
          filter(
            coin_open_time <= now,
            is.na(next_event_start) | now < next_event_start
          ) %>%
          slice_tail(n = 1) %>%
          pull(event_name)
  }

  # Observe the button click
  observeEvent(input$flip_coin, {
    # Validate inputs
    if (is.null(input$coin_user_name) || input$coin_user_name == "") {
      showNotification("Please enter your name.", type = "error")
      return()
    }
    if (is.null(input$user_choice) || input$user_choice == "") {
      showNotification("Please choose Heads or Tails.", type = "error")
      return()
    }
    
    # control for when no allowable event exists
    if(length(eligible_event_to_flip) == 0){
      showNotification("You can only flip after a tournament finishes and before the next one starts", type = "error")
      return()
    }
    
    # does a record exist that allows the user to toss coin?
    # Specifically, have they tossed the coin on most recently played event?
    record_exist <- data() %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(
        player_name == input$coin_user_name,
        event_name == eligible_event_to_flip,
        coin_toss == FALSE
      ) %>%
      nrow() > 0
    
    # does the player have at least 1 coin to toss? (each player given 5 attempts/coins at start of season)
    coins_available <- read_sheet(sheet_url, sheet = "coins") %>%
      filter(name == input$coin_user_name) %>%
      pull(coins)
    
    
    # Only proceed if a record exists and user has at least 1 coin
    if (!(record_exist && coins_available >=1)) {
      showNotification("Not available", type = "warning")
      return()
    } else {
      
      # Simulate a coin flip (randomly choose heads or tails)
      result <- sample(c("Heads", "Tails"), 1)
      
      # Update the reactive value
      coin_result(result)
      
      # update earnings for most recent event (TRUE or FALSE)
      user_won <- input$user_choice == result
      
      # update earnings depending on outcome of coin toss
       if(user_won){
         df_new <- data() %>%
           left_join(read.csv("data/events.csv"), by = "event_name") %>%
           group_by(player_name, event_name) %>%
           slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
           filter(player_name == input$coin_user_name & event_name == eligible_event_to_flip & coin_toss == FALSE) %>%
           mutate(earnings_g1 = earnings_g1 * 2, # double the earnings
                  earnings_g2 = earnings_g2 * 2,
                  coin_toss = TRUE, # update coin toss col
                  input_date = as.POSIXct(Sys.time())) %>%
           select(-order, -deadline)
           
           # add new record to table
           append_google_sheet(df_new)
           
           # update data for charts
           data(read_sheet(sheet_url, sheet = "2026")) 
       } 
      
      else {
         
       df_new <- data() %>%
         left_join(read.csv("data/events.csv"), by = "event_name") %>%
         group_by(player_name, event_name) %>%
         slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
         filter(player_name == input$coin_user_name & event_name == eligible_event_to_flip & coin_toss == FALSE) %>%
         mutate(earnings_g1 = 0, # set earnings to €0
              earnings_g2 = 0,
              coin_toss = TRUE, # update coin toss col
              input_date = as.POSIXct(Sys.time())) %>%
         select(-order, - deadline)
       
       # add new record to table
       append_google_sheet(df_new)
       
       # update data for charts
       data(read_sheet(sheet_url, sheet = "2026")) 
      }
      
      # update coins register
      read_sheet(sheet_url, sheet = "coins") %>%
          mutate(coins = if_else(name == input$coin_user_name, coins - 1, coins)) %>%
          write_sheet(sheet_url, sheet = "coins")
    }
  })
  
  # Render the coin animation
  output$coin_animation <- renderUI({
    if (!is.null(coin_result())) {
      tags$div(
        style = "text-align: center; margin-top: 20px;",
        tags$div(class = "coin")  # Animated coin
      )
    }
  })
  
  # Render the game result
  output$game_result <- renderUI({
    if (!is.null(coin_result())) {
      # Determine if the user won or lost
      user_won <- input$user_choice == coin_result()
      
      # Display the result with some styling
      tags$div(
        style = "text-align: center; margin-top: 20px; font-size: 24px;",
        tags$p(paste("You chose:", input$user_choice)),
        tags$p(paste("The coin landed on:", coin_result())),
        tags$p(if (user_won) {
          tags$span("Congrats, you won! 🎉", style = "color: green;")
          tags$span(paste("Your earnings for the ", eligible_event_to_flip, "have been doubled 🤑"), style = "color: green;")
        } else {
          tags$span("You lost. 😢", style = "color: red;")
          tags$span(paste("Your earnings for the", eligible_event_to_flip, "have been set to €0 👿"), style = "color: red;")
        })
      )
    }
  })
  
  # degenerate gamblers table
  output$degenerate_gambler <- renderDT({
    data() %>%
      filter(event_occured & coin_toss) %>%
      mutate(coin_toss_success = if_else(earnings_g1 == 0 & earnings_g2 == 0, "Lost all earnings", "Doubled earnings")) %>%
      select(player_name, event_name, coin_toss_success) %>%
      datatable(colnames = c("Name", "Event", "Coin Toss Result"),
                escape = FALSE, 
                rownames = FALSE, 
                options = list(
                  searching = FALSE, 
                  paging = FALSE, 
                  ordering = FALSE)
      )
  })
} 

shinyApp(ui = ui, server = server)

# rsconnect::deployApp(appName = "name-of-app", appDir = "directory/where/my/app.R")

# Add dependency file: his tells the cloud which version of R to use
#rsconnect::writeManifest()
