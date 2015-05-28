rcNames = list(c("Gilbert present", "Gilbert absent"),
               c("Death", "No death"))

observed = matrix(c(40, 34, 217, 1350), nrow = 2, dimnames = rcNames)

expected = tcrossprod(rowSums(observed), colSums(observed)) / sum(observed)
dimnames(expected) = rcNames

chiSq = sum((observed - expected)^2 / expected)

pValue = 1 - pgamma(chiSq, shape = 1/2, rate = 1/2)
