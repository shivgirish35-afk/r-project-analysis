# Calculate the difference between observed and expected runs
players$Difference <-
  players$Observed_Runs - players$Expected_Runs

# Display the comparison table
print(
  players[, c("Player", "Observed_Runs",
              "Expected_Runs", "Difference")],
  row.names = FALSE
)

# Plot observed and expected runs
barplot(
  rbind(
    players$Observed_Runs,
    players$Expected_Runs
  ),
  beside = TRUE,
  names.arg = players$Player,
  las = 2,
  cex.names = 0.7,
  ylab = "Number of Runs",
  main = "Observed vs Expected Runs",
  legend.text = c("Observed Runs", "Expected Runs"),
  args.legend = list(x = "topright", bty = "n")
)
