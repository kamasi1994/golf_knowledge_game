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
  arrange(event_name, player_name) 

update_google_sheet(new_df)

# add odds
odds <- get_odds()$event_odds_table %>%
  mutate(player_name = str_squish(player_name))

test %>%
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

