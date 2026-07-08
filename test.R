##################################### 
# troubleshoot missing urls
##################################### 
test <- read_sheet(sheet_url, sheet = "2026")

all_golfers_selected <- test %>%
  filter(!scraped) %>% # only scrape player data for records that have dont have any earnings figures yet
  select(golfer1, golfer2) %>%
  pivot_longer(cols = c("golfer1", "golfer2")) %>%
  filter(!is.na(value)) %>%
  unique() %>%
  pull()

earnings_list <- list() 
for(g in all_golfers_selected){
  print(g)
  result <- scrape_pga_prize_money(g)
  earnings_list[[g]] <- result
}
earnings <- do.call(rbind, earnings_list)

scrape_pga_prize_money("Nico Echavarria")

##################################### 
# add missing odds
##################################### 
odds <- get_odds()$event_odds_table %>%
  mutate(player_name = str_squish(player_name))

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


#################

new_player_urls <- read.csv("data/player_urls.csv") %>%
  mutate(
    id = sub(".*?/id/(\\d+).*", "\\1", url),
    new_url = paste0("https://www.espn.com/golf/player/results/_/id/", id, "/season/2026"))

write.csv(new_player_urls, "data/new_player_urls.csv", row.names = F)


##################
# animated bar chart race
library(tidyverse)
library(highcharter)
library(echarts4r)

df <- test %>% 
  # only use latest pick per player / tournament
  group_by(player_name, event_name) %>%
  slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # only show data for events that have occurred
  filter(event_occured) %>% 
  left_join(read.csv("data/events.csv"), by = "event_name") %>%
  group_by(order, event_name, player_name) %>% 
  summarise(TotalEarnings = sum(earnings_g1 +earnings_g2, na.rm = TRUE), .groups = "drop") %>%
  group_by(player_name) %>%
  arrange(order) %>%
  mutate(TotalEarnings = cumsum(TotalEarnings)) %>%
  ungroup() %>%
  arrange(order) %>%
  mutate(event_name = factor(event_name, levels = unique(event_name[order(order)])))

# =============================================================
# Export data for the D3 bar chart race (barchart_race.html)
# =============================================================
# This just writes your data frame to JSON in the exact shape
# the HTML/JS widget expects. Run this whenever your underlying
# data updates, then the HTML file will pick up the new numbers
# next time it's loaded (or on refresh, if used in Shiny).

# df must have: order, event_name, player_name, TotalEarnings (cumulative)
export_data <- df %>% 
  # only use latest pick per player / tournament
  group_by(player_name, event_name) %>%
  slice_max(order_by = input_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # only show data for events that have occurred
  filter(event_occured) %>% 
  left_join(read.csv("data/events.csv"), by = "event_name") %>%
  group_by(order, event_name, player_name) %>% 
  summarise(TotalEarnings = sum(earnings_g1 +earnings_g2, na.rm = TRUE), .groups = "drop") %>%
  group_by(player_name) %>%
  arrange(order) %>%
  mutate(TotalEarnings = cumsum(TotalEarnings)) %>%
  ungroup() %>%
  arrange(order) %>%
  mutate(event_name = factor(event_name, levels = unique(event_name[order(order)]))) %>%
  select(order, event_name, player_name, TotalEarnings) %>%
  arrange(order, desc(TotalEarnings))

# Write alongside the HTML file (or into your Shiny app's www/ folder)
jsonlite::write_json(export_data, "www/race_data.json", dataframe = "rows", auto_unbox = TRUE)

# ---------------------------------------------------------------
# Embedding in a Shiny dashboard
# ---------------------------------------------------------------
# 1. Put barchart_race.html AND race_data.json into your app's www/ folder
#    (Shiny serves anything in www/ automatically at the app root).
# 2. In your UI, embed it as an iframe:
#
#    ui <- fluidPage(
#      tags$iframe(src = "barchart_race.html", width = "100%", height = "650px",
#                  style = "border: none;")
#    )
#
# That's it — no server-side rendering, no render delay. The JS file
# fetches race_data.json itself and animates in-browser.
