len <- 24
x = runif(len)
y = x^3 + runif(len, min = -0.1, max = 0.1)
plot(x, y, pch = 20)

s <- seq(from = 0, to = 1, length = 50)
lines(s, s^3, lty = 2)

df <- data.frame(x, y)
m <- nls(y ~ I(x^power), data = df, start = list(power = 1), trace = T)

lines(s, predict(m, list(x = s)), col = "dark green")
summary(m)

m.exp <- nls(y ~ I(a * exp(b * x)), data = df, start = list(a = 1, b = 0), trace = T)
lines(s, predict(m.exp, list(x = s)), col = "red")

exp.eq <- function(x, a, b) {
  exp(1)^(a + b * sin(x^4))
}
exp.eq(2, 1, 3)  # testing the equation works: 3^2 + 1 = 10

m.sinexp <- nls(y ~ exp.eq(x, a, b), data = df, start = list(a = 1, b = 1), 
                trace = T)
lines(s, predict(m.sinexp, list(x = s)), col = "blue")


curve_fit <- function(type, legend.title, len = 24) {
  
  # generate a basic plot of data points
  len <- 24
  x = runif(len)
  y = x^3 + runif(len, min = -0.1, max = 0.1)
  plot(x, y, pch = 20, main = "Curve Fitting")
  
  # scratch a basic line to fit the data
  s <- seq(from = 0, to = 1, length = 50)
  lines(s, s^3, lty = 2)
  
  # plot the first predict function
  df <- data.frame(x, y)
  m <- nls(y ~ I(x^power), data = df, start = list(power = 1), trace = T)
  
  lines(s, predict(m, list(x = s)), col = "dark green")
  summary(m)
  
  # plot the second predict function (exponential)
  m.exp <- nls(y ~ I(a * exp(b * x)), data = df, start = list(a = 1, b = 0), trace = T)
  lines(s, predict(m.exp, list(x = s)), col = "red")
  
  # plot the third predict function (pre-defined)
  exp.eq <- function(x, a, b) {
    exp(1)^(a + b * sin(x^4))
  }
  exp.eq(2, 1, 3)  # testing the equation works: 3^2 + 1 = 10
  
  m.sinexp <- nls(y ~ exp.eq(x, a, b), data = df, start = list(a = 1, b = 1), 
                  trace = T)
  lines(s, predict(m.sinexp, list(x = s)), col = "blue")
  
  # add a legend
  legend.text <- c(paste0(min, " % or less"),
                   paste0(min + inc, " %"),
                   paste0(min + 2 * inc, " %"),
                   paste0(min + 3 * inc, " %"),
                   paste0(max, " % or more"))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title)
}