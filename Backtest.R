# ---- Load Data ----
combined <- read.csv('/Users/shenghangao/Desktop/Applied Stats/Combined_data.csv', stringsAsFactors = FALSE)
dist2018 <- read.csv("/Users/shenghangao/Desktop/season_data421/2018_distribution.csv", stringsAsFactors = FALSE)

# ---- Get actual wins in 2018 ----
# Winner column = +1 win for that team
wins_2018 <- table(combined$Winner[combined$Season == 2018])
wins_2018 <- as.data.frame(wins_2018)
colnames(wins_2018) <- c("Team", "Wins")

# ---- Align with distribution ----
# In dist2018, first column is team name, rest are V1...V18
result_list <- lapply(1:nrow(dist2018), function(i) {
  team <- dist2018[i, 1]
  actual_wins <- wins_2018$Wins[wins_2018$Team == team]
  
  if (length(actual_wins) == 0) {
    return(data.frame(Team = team, ActualWins = NA, LogProb = NA))
  }
  
  col_name <- paste0("V", actual_wins)
  prob <- dist2018[i, col_name]
  
  log_prob <- log(prob)
  data.frame(Team = team, ActualWins = actual_wins, LogProb = log_prob)
})

results <- do.call(rbind, result_list)

# ---- Total Log Likelihood ----
total_log_likelihood <- sum(results$LogProb[is.finite(results$LogProb)], na.rm = TRUE)

# ---- Output ----
print(results)
cat("Total Log Likelihood of the model:", total_log_likelihood, "\n")