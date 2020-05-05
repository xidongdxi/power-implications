
# Function to derive the sample size
# Input:
# x: sample size to be solved
# power_target: power target
solver <- function(x, power_target, rand, delta, sd, alpha) {
  z <- delta / sd / sqrt(((rand + 1) / rand + (rand + 1)) / x)
  y <- 1 - pnorm(qnorm(1 - alpha), z, 1)
  return(y - power_target)
}
