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

TwoDimBrownianRandWalk <- function(num.steps, x.mean = 0, x.sd = 1, y.mean=0, y.sd=1 ) {
  return(cbind(OneDimBrownianRandWalk(num.steps, x.mean, x.sd),
               OneDimBrownianRandWalk(num.steps, y.mean, y.sd)))
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

GCRandomWalk <- function(num.steps = 0, latBegin = 46.8, lonBegin = 145.5,
  latEnd = 34.0, lonEnd = 120.0, delta = 0.0065, sigma = 0.005) {
  theta_begin <- (90 - latBegin) * pi / 180
  phi_begin <- lonBegin * pi / 180
  theta_end <- (90 - latEnd) * pi / 180
  phi_end <- lonEnd * pi / 180
  theta_1 <- theta_begin
  phi_1 <- phi_begin - phi_end
  theta_2 <- acos(sin(theta_1)*sin(phi_1))
  phi_2 <- sign(sin(theta_1)*cos(phi_1)) * acos(cos(theta_1)/sin(theta_2))
  phi_2[is.na(phi_2)] <- 0
  theta_3 <- theta_2
  phi_3 <- phi_2 - theta_end
  theta_4 <- acos(sin(theta_3)*cos(phi_3))
  phi_4 <- ifelse(sin(theta_3)*sin(phi_3) > 0, asin(cos(theta_3)/sin(theta_4)),
    pi - asin(cos(theta_3)/sin(theta_4)))
  phi_4[is.na(phi_4)] <- 0

  theta_init = theta_4
  theta = numeric(ceiling(2*theta_init/delta))
  theta[1] = theta_init
  ind = 1
  while (theta[ind] > 0) {
    ind = ind + 1
    if (ind > length(theta)) {
      theta = rbind(theta, numeric(ceiling(theta_init/delta)))
    }
    theta[ind] = theta[ind-1] - delta + rnorm(1, sd = sigma)
  }
  theta = theta[1:(ind+1)]
  phi_init = phi_4
  phi = rnorm(ind, sd = sigma)/sin(theta[seq(1, ind, 1)])
  phi = c(phi_init, phi_init + cumsum(phi))

  theta_5 <- acos(sin(theta)*sin(phi))
  phi_5 <- sign(sin(theta)*cos(phi)) * acos(cos(theta)/sin(theta_5))
  phi_5[is.na(phi_5)] <- 0
  theta_6 <- theta_5
  phi_6 <- phi_5 + theta_end
  theta_7 <- acos(sin(theta_6)*cos(phi_6))
  phi_7 <- ifelse(sin(theta_6)*sin(phi_6) > 0, asin(cos(theta_6)/sin(theta_7)),
    pi - asin(cos(theta_6)/sin(theta_7)))
  phi_7[is.na(phi_7)] <- 0
  theta_8 <- theta_7
  phi_8 <- phi_7 + phi_end

  lat = 90 - 180 * theta_8 / pi
  lon = 180 * phi_8 / pi

  return(cbind(lon, lat))
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
