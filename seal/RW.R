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

greatCircleRandWalk <- function(lat.begin, lon.begin, lat.end, lon.end,
    delta = 0.0065, sigma = 0.005) {
  theta.begin = (90 - lat.begin) * pi / 180
  phi.begin = lon.begin * pi / 180
  theta.end = (90 - lat.end) * pi / 180
  phi.end = lon.end * pi / 180
  theta.1 = theta.begin
  phi.1 = phi.begin - phi.end
  theta.2 = acos(sin(theta.1)*sin(phi.1))
  phi.2 = acos(cos(theta.1)/sin(theta.2))
  theta.3 = theta.2
  phi.3 = phi.2 - theta.end
  theta.4 = acos(sin(theta.3)*cos(phi.3))
  phi.4 = asin(cos(theta.3)/sin(theta.4))

  theta.init = theta.4
  theta = numeric(ceiling(2*theta.init/delta))
  theta[1] = theta.init
  ind = 1
  while (theta[ind] > 0) {
    ind = ind + 1
    if (ind > length(theta)) {
      theta = rbind(theta, numeric(ceiling(r.init/delta)))
    }
    theta[ind] = theta[ind-1] - delta + rnorm(1, sd = sigma)
  }
  theta = theta[1:(ind+1)]
  phi0 = phi.4
  phi = rnorm(ind, sd = sigma)/sin(theta[seq(1, ind, 1)])
  phi = c(phi0, phi0 + cumsum(phi))

  theta.5 = acos(sin(theta)*sin(phi))
  phi.5 = acos(cos(theta)*sin(theta.5))
  theta.6 = theta.5
  phi.6 = phi.5 + theta.end
  theta.7 = acos(sin(theta.6)*cos(phi.6))
  phi.7 = asin(cos(theta.6)/sin(theta.7))
  theta.8 = theta.7
  phi.8 = phi.7 + phi.end

  lat = 90 - 180 * theta.8 / pi
  lon = 180 * phi.8 / pi

  return(cbind(lat, lon))
}
