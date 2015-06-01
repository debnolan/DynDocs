load(url("http://www.stat.berkeley.edu/users/nolan/data/Projects/election.rda"))
rm(countyUS)
rm(precintPBC)

# The dependent variable is whether the voter cast a vote for 
# Buchanan or voted for another candidate. 
# In addition to the constant, the regressors are two dummy variables
# that respectively indicate whether the ballot records a vote for 
# Nelson (D) or a vote for Deckard (Ref).

summary(glm(ibuchanan ~ inelson + ideckard, family = "binomial",
       subset = isabs==0, data = ballotPBC))

summary(glm(ibuchanan ~ inelson + ideckard, family = "binomial",
        subset = isabs==1, data = ballotPBC))

# The above output should match Table 3 in the paper.

# The confidence intervals of the Nelson coefficients for the two 
# ballot formats do not overlap. Therefore, we reject the hypothesis
# that the effect of voting for Nelson (D) is the same regardless 
# of the ballot format.

xtabs(Freq ~ inelson + ideckard + isabs, data = ballotPBC )
# This gives the counts for table 4

xtabs(Freq ~ inelson + ideckard + isabs, data = ballotPBC , 
      subset = ibuchanan == 1)

xtabs(Freq ~ inelson + ideckard + isabs, data = ballotPBC , 
      subset = ibuchanan == 1) /  
   xtabs(Freq ~ inelson + ideckard + isabs, data = ballotPBC)
# This gives us the percentages in table 4

# A confidence interval for an estimate of the number of votes
# that were miscast might be interesting
# This would need to use the % of those who vote for Gore of
# those who vote for Deckard. 


# Analyze PBC data in precinct-level

precinct = levels(ballotPBC$precinct)
data = as.data.frame(matrix(0, nrow = length(precinct), ncol = 6))

for (i in 1:length(precinct)) {
    data[i, 1] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                  ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 0 &
                                  ballotPBC$ideckard == 0], 0)
  }

for (i in 1:length(precinct)) {
  data[i, 2] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                    ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 0 &
                                    ballotPBC$ideckard == 0], 0)
}

for (i in 1:length(precinct)) {
  data[i, 3] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                    ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 1 &
                                    ballotPBC$ideckard == 0], 0)
}

for (i in 1:length(precinct)) {
  data[i, 4] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                    ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 1 &
                                    ballotPBC$ideckard == 0], 0)
}

for (i in 1:length(precinct)) {
  data[i, 5] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                    ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 0 &
                                    ballotPBC$ideckard == 1], 0)
}

for (i in 1:length(precinct)) {
  data[i, 6] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                    ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 0 &
                                    ballotPBC$ideckard == 1], 0)
}

# Find the precincts with no data

excl.precinct = vector()
for (i in 1:length(precinct)) {
  if (all(data[i, ] == 0)) excl.precinct = c(excl.precinct, i)
}

data1 = data[-excl.precinct,]
