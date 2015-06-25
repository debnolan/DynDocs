# Function to compute other cell counts from top-left cell.
tabGen11 = function(cell11) {
  cell21 = 74 - cell11
  cell12 = 257 - cell11
  cell22 = 1310 + cell11
  return(c(cell11, cell21, cell12, cell22))
}

# Function to compute other cell counts from bottom-left cell.
tabGen21 = function(cell21) {
  cell11 = 74 - cell21
  cell12 = 183 + cell21
  cell22 = 1384 - cell21
  return(c(cell11, cell21, cell12, cell22))
}

# Function to compute other cell counts from top-right cell.
tabGen12 = function(cell12) {
  cell11 = 257 - cell12
  cell21 = -183 + cell12
  cell22 = 1567 - cell12
  return(c(cell11, cell21, cell12, cell22))
}

# Function to compute other cell counts from bottom-right cell.
tabGen22 = function(cell22) {
  cell11 = -1310 + cell22
  cell21 = 1384 - cell22
  cell12 = 1567 - cell22
  return(c(cell11, cell21, cell12, cell22))
}

# Static counts of expected counts.
expCounts = c(11.58927, 62.41073, 245.4107, 1321.5893)

# Function to compute chi-square statistic. 
chiSqStat = function(obsCounts) {
  sum(((obsCounts) - expCounts)^2 / expCounts)
}

# Function to make a density plot of the chi-square distribution.
plotGen = function(counts) {
  myChiSq = chiSqStat(counts)
  chiSq = seq(.025, max(8, ceiling(myChiSq)), length.out = 300)
  density = dgamma(chiSq, shape = 1/2, rate = 1/2)
  plot(density ~ chiSq, cex.axis = .8, cex.lab = .8, cex.main = .8,
       col = "#838996", col.axis = "#838996", col.lab = "#838996",
       col.main = "#838996", fg = "#838996", tcl = -.3, type = "l",
       main = expression(paste(chi[1]^2, " Distribution")),
       xlab = expression(chi^2), ylab = "Density", ylim = c(0, 2.5))
  abline(v = myChiSq, col = "#838996", lty = "dashed")
}

# Function to make a chi-square residual plot.
residGen = function(counts) {
  myChiSq = chiSqStat(counts)
  Residuals = (counts - expCounts) / sqrt(expCounts)
  plot(Residuals ~ seq(1, 4), cex.axis = .8, cex.lab = .8, col = "#838996",
       col.axis = "#838996", col.lab = "#838996", fg = "#838996", pch = 20,
       tcl = -.3, xaxt = "n", xlim = c(.5, 4.5), main = "", xlab = "")
  abline(h = 0, col = "#838996")
  text(seq(1, 4), rep(0, 4), cex = .8, col = "#838996",
       labels = c("Gilbert present\nDeath", "Gilbert absent\nDeath",
                  "Gilbert present\nNo death", "Gilbert absent\nNo death"))
}

# Function to generate conclusion statement to be displayed in tutorial.
conclusion = function(counts) {
  myChiSq = chiSqStat(counts)
  #pValue = 1 - pgamma(myChiSq, shape = 1/2, rate = 1/2)
  #if (pValue < .05) {
  #  statement = "reject"
  #} else {
  #  statement = "fail to reject"
  #}
  #paste0("We assessed the null hypothesis that the instance of death on a ",
  #       "shift was independent of Gilbert's presense. We found a chi-square ",
  #       "test statistic of ", myChiSq, ", which corresponds to a P-value of ",
  #       pValue, ". Given this information, we would ", statement, " the ",
  #       "null hypothesis at the standard .05 significance level.")
  return(class(counts))
}