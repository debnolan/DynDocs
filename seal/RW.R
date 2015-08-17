OneDimSimpleRandWalk <- function(num.steps, prob = 0.5) {
  walk = cumsum(sample(c(1, -1), num.steps, TRUE))
  return(cbind(1:num.steps, walk))
}

OneDimBrownianRandWalk <- function(num.steps, mean = 0, sd = 1) {
  walk = cumsum(rnorm(num.steps, mean, sd))
  return(cbind(1:num.steps, walk))
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
  return(cbind(OneDimBrownianRandWalk(num.steps, mean, sd)[, 2],
               OneDimBrownianRandWalk(num.steps, mean, sd)[, 2]))
}

TwoDimRandWalkDrift <- function(x.init = 1, y.init = 1, delta = 0.0065,
                                sigma = 0.005) {
  r.init = sqrt(x.init^2 + y.init^2)
  r = numeric(ceiling(2*r.init/delta))
  r[1] = r.init
  ind = 1
  while (r[ind] > 0) {
    ind = ind + 1
    if (ind > length(r)) {
      r = rbind(r, numeric(ceiling(r.init/delta)))
    }
    r[ind] = r[ind-1] - delta + rnorm(1, sd = sigma)
  }
  r = r[1:(ind+1)]
  theta0 = atan(y.init/x.init) + ifelse(x.init < 0, pi, 0)
  theta = rnorm(ind, sd = sigma)/r[seq(1, ind, 1)]
  theta = c(theta0, theta0 + cumsum(theta))
  
  x = r*cos(theta)
  y = r*sin(theta)
  
  return(cbind(x, y))
}

multiWalk <- function(func, num.steps = 100, num.walks = 1) {
  colors = rgb(runif(num.walks), runif(num.walks), runif(num.walks),
               alpha = 1/log10(10*num.walks))
  walks = lapply(1:num.walks, function(x) func(num.steps))
  min.x = min(sapply(walks, function(x) min(x[, 1])))
  max.x = max(sapply(walks, function(x) max(x[, 1])))
  min.y = min(sapply(walks, function(x) min(x[, 2])))
  max.y = max(sapply(walks, function(x) max(x[, 2])))
  plot(c(), c(), xlim = c(min.x, max.x), ylim = c(min.y, max.y), xlab = "x",
       ylab = "y")
  for (i in 1:num.walks) {
    par(new = TRUE)
    plot(walks[[i]], type = "l", axes = FALSE, col = colors[i], xlab = "",
         ylab = "", xlim = c(min.x, max.x), ylim = c(min.y, max.y))
  }
  rm(walks)
}

