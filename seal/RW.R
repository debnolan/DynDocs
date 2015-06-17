 OneDimSimpleRandWalk <- function(num.steps, prob = 0.5) {
  return(cumsum(sample(c(1, -1), num.steps, TRUE)))
}

OneDimBrownianRandWalk <- function(num.steps, mean = 0, sd = 1) {
  return(cumsum(rnorm(num.steps, mean, sd)))
}

TwoDimSimpleRandWalk <- function(num.steps, x.prob = 0.5, y.prob = 0.5) {
  pos = matrix(0, ncol = 2, nrow = num.steps)
  dir = sample(c(1, 2), num.steps, TRUE)
  move = sample(c(1, -1), num.steps, TRUE)
  pos[cbind(1:num.steps, dir)] = move
  pos = apply(pos, 2, cumsum)
  return(pos)
}

TwoDimBrownianRandWalk <- function(num.steps, mean = 0, sd = 1) {
  return(cbind(OneDimBrownianRandWalk(num.steps, mean, sd),
               OneDimBrownianRandWalk(num.steps, mean, sd)))
}

#TDSRW = OneDimSimpleRandWalk(5000)
#plot(TDSRW, type = "l")
