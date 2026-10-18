players <- data.frame(
  Player = c(
    "Clint Richardson",
    "Julius Erving",
    "Lionel Hollins",
    "Maurice Cheeks",
    "Caldwell Jones",
    "Andrew Toney",
    "Bobby Jones",
    "Steve Mix",
    "Daryl Dawkins"
  ),

  Hits = c(
    124, 459, 194, 189, 129,
    208, 233, 181, 250
  ),

  Misses = c(
    124, 425, 225, 150, 143,
    243, 200, 170, 153
  ),

  Observed_Runs = c(
    128, 431, 203, 172, 134,
    245, 227, 176, 220
  )
)

players$Expected_Runs <-
  1 + (2 * players$Hits * players$Misses) /
  (players$Hits + players$Misses)

print(players, row.names = FALSE)
