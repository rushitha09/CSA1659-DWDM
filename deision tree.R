expansion_cost <- 2
revenue_no_expansion_good <- 4
revenue_no_expansion_bad <- 1.5
revenue_expansion_good <- 7
revenue_expansion_bad <- 3
prob_good <- 0.45
prob_bad <- 0.55
expected_no_expansion <- (prob_good * revenue_no_expansion_good) + (prob_bad * revenue_no_expansion_bad)
expected_expansion <- (prob_good * revenue_expansion_good) + (prob_bad * revenue_expansion_bad) - expansion_cost
decision_tree <- function() {
  cat("Decision Tree:\n")
  cat("1. Expand the factory\n")
  cat("   a. Good Economy (45%) => Revenue: $", revenue_expansion_good, "M\n")
  cat("   b. Bad Economy (55%) => Revenue: $", revenue_expansion_bad, "M\n")
  cat("   c. Expected Revenue after Expansion (including cost): $", round(expected_expansion, 2), "M\n\n")
  cat("2. Do not expand the factory\n")
  cat("   a. Good Economy (45%) => Revenue: $", revenue_no_expansion_good, "M\n")
  cat("   b. Bad Economy (55%) => Revenue: $", revenue_no_expansion_bad, "M\n")
  cat("   c. Expected Revenue without Expansion: $", round(expected_no_expansion, 2), "M\n\n")
}
decision_tree()
if (expected_expansion > expected_no_expansion) {
  cat("Recommendation: Expand the factory.\n")
} else {
  cat("Recommendation: Do not expand the factory.\n")
}