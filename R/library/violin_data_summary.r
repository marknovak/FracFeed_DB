# For violin plot
data_summary <- function(x) {
  n <- length(x)
  m <- mean(x, na.rm = T)
  if (n == 1) {
    ymin <- ymax <- m
  } else{
    ymin <- m - sd(x, na.rm = T)
    ymax <- min(m + sd(x, na.rm = T), 100, na.rm = T)
  }
  return(c(y = m, ymin = ymin, ymax = ymax))
}