#############################################################
# Necessary functions
#############################################################

#############
# scrape tournament odds
#############
get_odds <- function(){
  
  odds <- read_html("http://golfodds.com/weekly-odds.html")
  
  event_odds_name <- odds %>%
    html_element(".Headline-orange") %>%
    html_text()
  
  event_odds_table <- odds %>%
    html_element(".Copy-black") %>%
    html_table() %>%
    filter(row_number() != 1) %>%
    rename(player_name = X1,
           odds = X2) %>%
    slice(1:which(player_name == "")[1] - 1) %>%
    mutate(
      odds_num = {
        x <- strsplit(odds, "/")
        as.numeric(sapply(x, `[`, 1)) / as.numeric(sapply(x, `[`, 2))
      }) %>%
    arrange(odds_num) %>%
    select(-odds_num) %>%
    mutate(event_name = event_odds_name)
  
  return(
    list(
      event_odds_name = event_odds_name,
      event_odds_table = event_odds_table
    )
  )
}
#############
# Function to get live scores
#############
get_live_scores <- function(){
  
  event_name <- read_html("https://www.espn.com/golf/leaderboard") %>%
    html_nodes(".headline.headline__h1.Leaderboard__Event__Title") %>%
    html_text()
  
  tryCatch({
    df <-  read_html("https://www.espn.com/golf/leaderboard") %>%
      html_nodes(".tl.Table__TD") %>%
      html_text() %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      as.data.frame(stringsasFactors = FALSE) %>%
      mutate(V2 = stri_trans_general(V2, "Latin-ASCII"),
             event_name = event_name)
    
    colnames(df) <- c("position", "golfer", "event_name")
    
    # if average length of position string is > 5 then the scraper has picked up
    # data that espn.com only shows if the event hasn't started yet
    if(mean(str_length(df$position)) > 5){
      df <- NULL
    }
    
    return(df)
    
  }, error = function(e){
    return(NULL)
  })
}

#############   
# Function to scrape prize money data 
#############
scrape_pga_prize_money <- function(golfer_name) { 
  
  # find player name in player url file (player_urls.csv)
  # this has the espn.com urls for the top 300 players according to data golf (so includes liv players)
  player_link <- read.csv("data/player_urls.csv") %>%
    filter(name == golfer_name) %>%
    select(url) %>%
    pull()
  
  # read the player page on espn.com
  player_page <- read_html(player_link)
  
  # get earnings data for this player
  earnings <- player_page %>% 
    html_nodes(".tar.Table__TD") %>%
    html_text()
  
  if(length(earnings) == 0){
    return(data.frame(golfer_name = golfer_name, 
                      event_name = character(1), 
                      earnings = numeric(1), 
                      coin_toss = FALSE, 
                      scraped = FALSE))
  } else {
    
    # get corresponding tournament name
    tournaments <- player_page %>%
      html_nodes(".LastTournamentTable__name") %>%
      html_nodes(".AnchorLink") %>%
      html_text() 
    
    # combine
    data <- data.frame(golfer_name = golfer_name, event_name = tournaments, earnings = earnings) %>%
      mutate(earnings = if_else(earnings == "--", "0", earnings), # handle NAs (from missed cuts)
             earnings = as.numeric(gsub("[\\$,]", "", earnings)),
             coin_toss = FALSE, # add a FALSE coin toss column so that only records that havent been gambled are updated
             scraped = FALSE) # add a FALSE scraped column so that only records that havent already been scraped are updated 
    
    return(data)
  }
}

#############
# load earnings prediction model
#############
# earnings_pred_model <- readRDS("data/earnings_model.rds")

#############
# Functions to update Google Sheet 
#############
update_google_sheet <- function(new_data) { 
  sheet_write(new_data, ss = sheet_url, sheet = "2026") } 

append_google_sheet <- function(new_data) { 
  sheet_append(new_data, ss = sheet_url, sheet = "2026") } 
