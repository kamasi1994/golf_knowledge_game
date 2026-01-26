# Function to get the Player ID's of the top 200 golfers (needed to scrape earnings data on espn.com)
# no longer needed as a csv file has been created containinng urls for the data golf rankings top players
# I did this due to the many incorrect spelling names in espn urls, so a function with a player name as the input didnt work all the time
scrape_top_200_payer_ids <- function(){
  
  # get urls for top 200 players in world
  top_200_page <- read_html("https://www.espn.com/golf/rankings")
  
  player_links <- top_200_page %>%
    html_nodes(".AnchorLink") %>%
    html_attr("href") %>%
    .[grepl("/golf/player/_/id/", .)]
  
  return(player_links)
}

scrape_top_200_payer_ids()


# get data golf rankings
# first, manually download a csv file from the data golf website
# Import
dg <- read.csv("data/dg_rankings_jan2026.csv")

# create function to reformat names --> "Scheffler, Scottie" to "Scottie Scheffler
reformat_name <- function(name){
  parts <- strsplit(name, ",\\s*", perl = TRUE)[[1]]
  
  if(length(parts) == 2){
    first <- parts[2]
    last <- parts[1]
    return(paste(first, last))
  } else{
    return(name)
  }
}

dg$name <- sapply(dg$player_name, reformat_name)

dg_urls <- dg %>%
  select(name, dg_rank) %>%
  left_join(read.csv("data/player_urls.csv") %>% select(dg_ranking_name, url), by = c("name" = "dg_ranking_name"))


# manually fill in names
add_url <- function(player_name, new_url){
  
  df <- dg_urls %>%
    mutate(url = if_else(name == player_name, new_url, url))

  return(df)
}

dg_urls <- add_url("Jayden Schaper", "https://www.espn.com/golf/player/_/id/4604053/jayden-schaper")
dg_urls <- add_url("Matt McCarty", "https://www.espn.com/golf/player/_/id/4901368/matt-mccarty")
dg_urls <- add_url("Garrick Higgo", "https://www.espn.com/golf/player/_/id/11393/garrick-higgo")
dg_urls <- add_url("Max McGreevy", "https://www.espn.com/golf/player/_/id/11383/max-mcgreevy")
dg_urls <- add_url(" ", " ")
dg_urls <- add_url(" ", " ")
dg_urls <- add_url(" ", " ")
dg_urls <- add_url(" ", " ")
dg_urls <- add_url(" ", " ")
dg_urls <- add_url(" ", " ")
dg_urls <- add_url(" ", " ")
dg_urls <- add_url(" ", " ")
dg_urls <- add_url(" ", " ")

