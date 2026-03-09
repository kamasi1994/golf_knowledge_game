##################################### 
# troubleshoot missing urls
##################################### 
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

##################################### 
# add missing odds
##################################### 
odds <- get_odds()$event_odds_table %>%
  mutate(player_name = str_squish(player_name),
         event_name = "Arnold Palmer Invitational pres. by Mastercard")

df <- read_sheet(sheet_url, sheet = "2026") %>%
  mutate(
    golfer1 = str_squish(golfer1),
    golfer2 = str_squish(golfer2))

not_missing_odds <- df %>%
  filter(!(is.na(odds_g1) | is.na(odds_g2))) %>%
  mutate(odds_g1 = as.character(odds_g1),
         odds_g2 = as.character(odds_g2))

missing_odds <- df %>%
  filter(is.na(odds_g1) | is.na(odds_g2)) %>%
  left_join(odds, by = c("golfer1" = "player_name", "event_name")) %>%
  mutate(odds_g1 = odds) %>%
  select(-odds) %>%
  left_join(odds, by = c("golfer2" = "player_name", "event_name")) %>%
  mutate(odds_g2 = odds) %>%
  select(-odds)

new_df <- bind_rows(not_missing_odds, missing_odds) %>%
  arrange(player_name, event_name) 

update_google_sheet(new_df)



