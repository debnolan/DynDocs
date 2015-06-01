# Just a list of row and column names we want for our matrices.
rcNames = list(c("Gilbert present", "Gilbert absent"),
               c("Death", "No death"))

# The data from the reading as a two-way frequency table.
observed = matrix(c(40, 34, 217, 1350), nrow = 2, dimnames = rcNames)

# Calculation of expected counts given H0: there was no difference in the rate
# of deaths when Gilbert was present versus when Gilbert was absent.
expected = tcrossprod(rowSums(observed), colSums(observed)) / sum(observed)
dimnames(expected) = rcNames

# Calculation of the chi-square statistic. Note that no expected cell frequency
# is below 10, so I have elected not to use the Yates correction.
chiSq = sum((observed - expected)^2 / expected)

# Calculation of the corresponding p-value. Recall the chi-square distribution
# with n degrees of freedom is equivalent to the gamma distribution with shape
# parameter n/2 and rate parameter 1/2.
pValue = 1 - pgamma(chiSq, shape = 1/2, rate = 1/2)