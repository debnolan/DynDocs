# List of table row and column names, for debugging.
dimNames = list(c("Gilbert present", "Gilbert absent", "Total"),
                c("Death", "No death", "Total"))

# Function to compute other cell counts from top-left cell.
tabGen11 = function(cell11) {
  cell21 = 74 - cell11
  cell12 = 257 - cell11
  cell22 = 1310 + cell11
  matrix(c(cell11, cell21, 74, cell12, cell22, 1567, 257, 1384, 1641),
         dimnames = dimNames, nrow = 3)
}

# Function to compute other cell counts from bottom-left cell.
tabGen21 = function(cell21) {
  cell11 = 74 - cell21
  cell12 = 183 + cell21
  cell22 = 1384 - cell21
  matrix(c(cell11, cell21, 74, cell12, cell22, 1567, 257, 1384, 1641),
         dimnames = dimNames, nrow = 3)
}

# Function to compute other cell counts from top-right cell.
tabGen12 = function(cell12) {
  cell11 = 257 - cell12
  cell21 = -183 + cell12
  cell22 = 1567 - cell12
  matrix(c(cell11, cell21, 74, cell12, cell22, 1567, 257, 1384, 1641),
         dimnames = dimNames, nrow = 3)
}

# Function to compute other cell counts from bottom-right cell.
tabGen22 = function(cell22) {
  cell11 = -1310 + cell22
  cell21 = 1384 - cell22
  cell12 = 1567 - cell22
  matrix(c(cell11, cell21, 74, cell12, cell22, 1567, 257, 1384, 1641),
         dimnames = dimNames, nrow = 3)
}

# Static table of expected counts.
expCounts = matrix(c(11.58927,62.41073, 245.4107, 1321.5893), nrow = 2)

# Function to compute chi-square statistic. 
chiSqStat = function(table) {
  sum((table[-3,-3] - expCounts)^2 / expCounts)
}

# Function to make a density plot of the chi-square distribution.
plotGen = function(table) {
  myChiSq = chiSqStat(table)
  chiSq = seq(0, max(8, ceiling(myChiSq)), length.out = 300)
  density = dgamma(chiSq, shape = 1/2, rate = 1/2)
  plot(density ~ chiSq, cex.axis = .8, cex.lab = .8, cex.main = .8,
       col = "#838996", col.axis = "#838996", col.lab = "#838996",
       col.main = "#838996", fg = "#838996", tcl = -.3, type = "l",
       main = expression(paste(chi[1]^2, " Distribution")),
       xlab = expression(chi^2), ylab = "Density")
  abline(v = myChiSq, col = "#838996", lty = "dashed")
}

# Function to make a chi-square residual plot.
residGen = function(table) {
  myChiSq = chiSqStat(table)
  Residuals = as.vector((table[-3,-3] - expCounts) / sqrt(expCounts))
  plot(Residuals ~ seq(1, 4), cex.axis = .8, cex.lab = .8, col = "#838996",
       col.axis = "#838996", col.lab = "#838996", fg = "#838996", pch = 20,
       tcl = -.3, xaxt = "n", xlim = c(.5, 4.5), main = "", xlab = "")
  abline(h = 0)
  text(seq(1, 4), rep(0, 4), cex = .8,
       labels = c("Gilbert present\nDeath", "Gilbert absent\nDeath",
                  "Gilbert present\nNo death", "Gilbert absent\nNo death"))
}

# Function to generate conclusion statement to be displayed in tutorial.
conclusion = function(table) {
  myChiSq = chiSqStat(table)
  pValue = 1 - pgamma(myChiSq, shape = 1/2, rate = 1/2)
  if (pValue < .05) {
    statement = "reject"
  } else {
    statement = "fail to reject"
  }
  paste0("We assessed the null hypothesis that the instance of death on a ",
         "shift was independent of Gilbert's presense. We found a chi-square ",
         "test statistic of ", myChiSq, ", which corresponds to a P-value of ",
         pValue, ". Given this information, we would ", statement, " the ",
         "null hypothesis at the standard .05 significance level.")
}