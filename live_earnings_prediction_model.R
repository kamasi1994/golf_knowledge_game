
players_champ <- read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580340") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "THE PLAYERS Championship",
         cut = TRUE) %>%
  select(event_name, position, earnings, cut)



ap_inv <- read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580338") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "Arnold Palmer Invitational pres. by Mastercard",
         cut = TRUE) %>%
  select(event_name, position, earnings, cut)


att_proam <-  read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580333") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 10, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "AT&T Pebble Beach Pro-Am",
         cut = FALSE) %>%
  select(event_name, position, earnings, cut)


genesis <-  read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580335") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "The Genesis Invitational",
         cut = TRUE) %>%
  select(event_name, position, earnings, cut)




masters <-  read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580344") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "The Masters",
         cut = TRUE) %>%
  select(event_name, position, earnings, cut)


pga <-  read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580351") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "PGA Championship",
         cut = TRUE) %>%
  select(event_name, position, earnings, cut)


us_open <-  read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580355") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "US Open",
         cut = TRUE) %>%
  select(event_name, position, earnings, cut)


the_open <-  read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580360") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "The Open Championship",
         cut = TRUE) %>%
  select(event_name, position, earnings, cut)



fedex <-  read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580364") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "FedEx St.Jude Championship",
         cut = FALSE) %>%
  select(event_name, position, earnings, cut)


bmw <-  read_html("https://www.espn.com/golf/leaderboard/_/tournamentId/401580365") %>%
  html_nodes(".Table__TD") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE) %>%
  as.data.frame(stringsasFactors = FALSE) %>%
  rename(position = V2,
         earnings = V10) %>%
  mutate(event_name = "BMW Championship",
         cut = FALSE) %>%
  select(event_name, position, earnings, cut)



data <- bind_rows(players_champ, ap_inv, att_proam, genesis, masters, pga, us_open, the_open, fedex, bmw) %>%
  mutate(earnings = as.numeric(gsub("[\\$,]", "", earnings))) %>%
  filter(!is.na(earnings))


# Clean the positions column
data$position <- gsub("T", "", data$position)  # Remove "T" from tied positions
data$position <- as.numeric(data$position)     # Convert to numeric

# retain predicive power of tied places
# Adjust positions for ties
data <- data %>%
  group_by(position) %>%
  mutate(adjusted_position = ifelse(n() > 1, mean(position + 0:(n() - 1)), position)) %>%
  ungroup() %>%
  select(event_name, cut, earnings, adjusted_position)

# fit data to linear regression
linear_model <- lm(earnings ~ adjusted_position + cut, data = data)

summary(linear_model)

# Fit a quadratic regression model
nonlinear_model_cut <- lm(earnings ~ poly(adjusted_position, 2) + cut, data = data)
nonlinear_model_no_cut <- lm(earnings ~ poly(adjusted_position, 2), data = data)


summary(nonlinear_model)

# Calculate RMSE for both models
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Predictions from linear model
linear_predictions <- predict(linear_model, data)


# Predictions from non-linear model
nonlinear_predictions_no_cut <- predict(nonlinear_model_no_cut, data)

# Calculate RMSE
linear_rmse <- rmse(data$earnings, linear_predictions)
nonlinear_no_cut_rmse <- rmse(data$earnings, nonlinear_predictions_no_cut)

# Print RMSE
cat("Linear Regression RMSE:", linear_rmse, "\n")
cat("Non-Linear Regression without cut RMSE:", nonlinear_no_cut_rmse, "\n")



######### graph
# Add predictions to the data
data$linear_pred <- linear_predictions
data$nonlinear_pred <- nonlinear_predictions
data$nonlinear_pred_nocut <- nonlinear_predictions_no_cut


# Plot the data and models
ggplot(data, aes(x = adjusted_position, y = earnings)) +
  geom_point(size = 3, color = "blue") +  # Actual data points
  geom_line(aes(y = linear_pred), color = "red", size = 1) +  # Linear model
  geom_line(aes(y = nonlinear_pred), color = "green", size = 1) +  # Non-linear model
  geom_line(aes(y = nonlinear_pred_nocut), color = "yellow", size = 1) +  # Non-linear model
  labs(title = "Linear vs Non-Linear Regression",
       x = "Adjusted Position",
       y = "Earnings") +
  theme_minimal()



# Apply log transformation to earnings
data$log_earnings <- log(data$earnings)


# Fit linear regression on log-transformed data
log_model <- lm(log_earnings ~ adjusted_position, data = data)

summary(log_model)


# Predictions from log-transformed model
log_predictions <- exp(predict(log_model, data))  # Convert back to original scale


log_rmse <- rmse(data$earnings, log_predictions)


# Print RMSE
{
cat("Log-Transformed Model RMSE:", log_rmse, "\n")
cat("Linear Regression RMSE:", linear_rmse, "\n")
cat("Non-Linear Regression without cut RMSE:", nonlinear_no_cut_rmse, "\n")
}

# Add predictions to the data
data$log_pred <- log_predictions

# Fit a Random Forest model
rf_model <- randomForest(earnings ~ adjusted_position, data = data, ntree = 500)

data$rf_predicted <- predict(rf_model, data)


# Plot the data and models
ggplot(data, aes(x = adjusted_position, y = earnings)) +
  geom_point(size = 3, color = "blue") +  # Actual data points
  geom_line(aes(y = nonlinear_pred), color = "purple", size = 1) +  # Non-linear model
  geom_line(aes(y = log_pred), color = "red", size = 1) +  # Log-transformed model
  geom_line(aes(y = nonlinear_pred), color = "green", size = 1) +  # Non-linear model
  geom_line(aes(y = nonlinear_pred_nocut), color = "yellow", size = 1) +  # Non-linear model
  labs(title = "Log-Transformed vs 2nd Polynomial Regression vs 3rd Polynomial, vs linear ",
       x = "Adjusted Position",
       y = "Earnings") +
  theme_minimal()



####### 2nd polynomial has greatest predictive power

# Save the model to a file
saveRDS(rf_model, file = "data/earnings_model.rds")
