#####################################################
# update data
#####################################################
update_golf_data <- function(){
  
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
  
  # Google sheet URL 
  sheet_url <- "https://docs.google.com/spreadsheets/d/1rdaKGprdxuOKntnZYZrcsvU6Th9HNBumQSD13BhIXlI/edit?pli=1&gid=0#gid=0" 
  
  # get data from google sheets
  df <- read_sheet(sheet_url, sheet = "2026")
  
  # Get earnings data
  # loop the web scraping function over all golfers selected for this week
  all_golfers_selected <- df %>%
    filter(!scraped) %>% # only scrape player data for records that have dont have any earnings figures yet
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
  already_scraped <- df %>%
    filter(scraped) %>%
    group_by(player_name, event_name) %>%
    slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(input_date, event_name, player_name, golfer1, golfer2, earnings_g1, earnings_g2, event_occured, coin_toss, scraped, odds_g1, odds_g2)
  
  if(nrow(df %>% filter(!scraped)) == 0){
    return("Data is already up to date")
  } else {
      
    df_new <- df  %>%
      filter(!scraped) %>%
      mutate(event_name = case_when(
        event_name == "The Masters" ~ "Masters Tournament", 
        .default = event_name)) %>%
      left_join(earnings, by = c("event_name", "golfer1" = "golfer_name", "coin_toss")) %>%
      # if coin was tossed, preserve existing earnings
      mutate(earnings_g1 = if_else(coin_toss == TRUE, earnings_g1, earnings)) %>% 
      select(-earnings) %>%
      left_join(earnings, by = c("event_name", "golfer2" = "golfer_name", "coin_toss")) %>%
      # if coin was tossed, preserve existing earnings
      mutate(earnings_g2 = if_else(coin_toss == TRUE, earnings_g2, earnings)) %>% 
      select(-earnings) %>%
      # update event_corrected flag if current date is >= 5 days after event deadline
      left_join(read.csv("data/events.csv"), by = "event_name") %>%
      # update scraped flag if earnings were successfully scraped
      mutate(scraped = if_else(is.na(earnings_g1) & is.na(earnings_g2), FALSE, TRUE),
             event_occured = TRUE) %>%
      replace_na(list(earnings_g1 = 0, earnings_g2 = 0)) %>%
      group_by(player_name, event_name) %>%
      slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(input_date, event_name, player_name, golfer1, golfer2, earnings_g1, earnings_g2, event_occured, coin_toss, scraped, odds_g1, odds_g2)
    
    # double earnings if odds >= 100/1
    df_new <- df_new %>%
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
        earnings_g1 = if_else(prob_g1 >= 100, earnings_g1 * 2, earnings_g1),
        earnings_g2 = if_else(prob_g2 >= 100, earnings_g2 * 2, earnings_g2)
      ) %>%
      select(-prob_g1, -prob_g2)
    
    
    # Save updated data
    df <- bind_rows(already_scraped, df_new) %>%
      arrange(event_name, player_name)
    
    update_google_sheet(df)
    
    return("Complete")
  }
}