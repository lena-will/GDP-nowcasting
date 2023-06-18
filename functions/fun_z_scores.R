z_scores <- function(x) {
  (x - mean(x)) / sd(x)
}