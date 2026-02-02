new_entry <- tibble( input_date = as.POSIXct(Sys.time()), 
                     event_name = current_event, 
                     player_name = "Conor", 
                     golfer1 = "Xander Schauffele", 
                     golfer2 = "Jason Day",
                     earnings_g1 = NA,
                     earnings_g2 = NA,
                     event_occured = FALSE,
                     coin_toss = FALSE,
                     scraped = FALSE) 

### troubleshoot missing urls

test <- read_sheet(sheet_url, sheet = "2026")

all_golfers_selected <- test %>%
  filter(!scraped) %>% # only scrape player data for records that have dont have any earnings figures yet
  select(golfer1, golfer2) %>%
  pivot_longer(cols = c("golfer1", "golfer2")) %>%
  unique() %>%
  pull()

earnings_list <- list() 
for(g in all_golfers_selected){
  print(g)
  result <- scrape_pga_prize_money(g)
  earnings_list[[g]] <- result
}
earnings <- do.call(rbind, earnings_list)

scrape_pga_prize_money("Xander Schauffele")



coins <- data.frame(
  name = c("Conor", "Chris", "Jive", "Shane", "Sean", "Eddie", "Phil", "Ross", "Mark", "John"),
  coins = c(4,4,4,4,4,4,4,4,4,4))
)

write.csv(coins, "data/coins.csv")

read.csv("data/events.csv") %>%
  filter(as.Date(deadline, format = "%d/%m/%Y/%H:%M")  + 4 < Sys.time()) %>%
  slice_max(order_by = order) %>%
  pull(event_name)

record_exist <- test %>% 
  filter(player_name == "Conor" & event_name == "The American Express" & coin_toss == FALSE) %>%
  left_join(read.csv("data/coins.csv"), by = c("player_name" = "name")) %>%
  filter(coins >= 1) %>%
  count() %>%
  pull() > 0
