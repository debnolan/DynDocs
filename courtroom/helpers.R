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
expCounts = c(11.59, 62.41, 245.41, 1321.59)

# Function to compute chi-square statistic. 
chiSqStat = function(obsCounts) {
  sum(((obsCounts) - expCounts)^2 / expCounts)
}

# Function to make a density plot of the chi-square distribution.
plotGen = function(counts) {
  myChiSq = chiSqStat(counts)
  chiSq = seq(.05, max(8, ceiling(myChiSq)), length.out = 300)
  density = dchisq(chiSq, df = 1)
  par(cex = .8, col = "#838996", col.axis = "#838996", fg = "#838996",
      mar = c(4, 4, 1, .5) - .25, xpd = FALSE)
  plot(density ~ chiSq, bty = "n", tcl = -.3, type = "l",
       xlab = "", ylab = "", ylim = c(0, 1.75))
  abline(v = myChiSq, col = "#838996", lty = "dashed")
  clip(-420, 420, -1, 2)
  text(median(chiSq), 1.8, expression(paste(chi[1]^2, " Distribution")))
  text(median(chiSq), -.35, expression(chi^2))
  text(-chiSq[300] / 7, .80, "Density", srt = 90)
}

# Function to make a chi-square residual plot.
residGen = function(counts) {
  myChiSq = chiSqStat(counts)
  Residuals = (counts - expCounts) / sqrt(expCounts)
  par(cex = .8, col = "#838996", col.axis = "#838996", fg = "#838996",
      mar = c(.5, 3, 0, 0) + .5, xpd = NA)
  plot(Residuals ~ seq(1, 4), bty = "n", pch = 20, tcl = -.3, xaxt = "n",
       xlab = "", xlim = c(.75, 4.25), ylab = "")
  abline(h = 0, xpd = FALSE)
  text(1:4, 0, cex = .8, col = "#838996",
       labels = c("Gilbert present\nDeath", "Gilbert absent\nDeath",
                  "Gilbert present\nNo death", "Gilbert absent\nNo death"))
  text(.25, (max(Residuals) + min(Residuals)) / 2, "Residuals",
       srt = 90, xpd = TRUE)
}

# Function to generate conclusion statement to be displayed in tutorial.
conclusion = function(counts) {
  myChiSq = chiSqStat(counts)
  pValue = 1 - pchisq(myChiSq, df = 1)
  if (pValue < .05) {
    statement = "reject"
  } else {
    statement = "fail to reject"
  }
  paste0("We assessed the null hypothesis that the instance of death on a ",
         "shift was independent of Gilbert's presense. We found a chi-square ",
         "test statistic of ", sprintf("%.2f", myChiSq), ", which ",
         "corresponds to a P-value of ", sprintf("%.4f", pValue), ". Given ",
         "this information, we would ", statement, " the null hypothesis at ",
         "the standard .05 significance level.")
}