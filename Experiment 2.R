# Calculate the variance of the number of runs
n <- players$Hits + players$Misses

players$Variance <-
  (2 * players$Hits * players$Misses *
     (2 * players$Hits * players$Misses - n)) /
  (n^2 * (n - 1))

# Calculate the standard deviation
players$SD <- sqrt(players$Variance)

# Calculate the Z-statistic
players$Z <-
  (players$Observed_Runs - players$Expected_Runs) /
  players$SD

# Calculate the approximate two-sided p-value
players$P_Value <- 2 * pnorm(-abs(players$Z))

# Identify significant results at the 5% level
players$Significant <-
  players$P_Value < 0.05

# Display the results
print(
  players[, c("Player", "Observed_Runs",
              "Expected_Runs", "Z",
              "P_Value", "Significant")],
  digits = 4,
  row.names = FALSE
)

# P-value plot for all nine players

barplot(
  players$P_Value,
  names.arg = players$Player,
  las = 2,
  cex.names = 0.75,
  ylim = c(0, 1),
  ylab = "Two-sided p-value",
  main = "Runs Test: P-values by Player",
  col = "lightblue"
)

# 5% significance threshold
abline(
  h = 0.05,
  col = "red",
  lty = 2,
  lwd = 2
)

# Add legend
legend(
  "topleft",
  legend = "Significance level = 0.05",
  col = "red",
  lty = 2,
  lwd = 2,
  bty = "n"
)
