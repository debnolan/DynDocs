load(url("http://www.stat.berkeley.edu/users/nolan/data/Projects/election.rda"))
rm(countyUS)
rm(precintPBC)

# The dependent variable is whether the voter cast a vote for 
# Buchanan or voted for another candidate. 
# In addition to the constant, the regressors are two dummy variables
# that respectively indicate whether the ballot records a vote for 
# Nelson (D) or a vote for Deckard (Ref).

summary(glm(ibuchanan ~ inelson + ideckard, family = "binomial", weights = Freq,
       subset = isabs==0, data = ballotPBC))

summary(glm(ibuchanan ~ inelson + ideckard, family = "binomial", weights = Freq,
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

precinct = unique(as.character(ballotPBC$precinct))
comb = sapply(1:length(precinct), 
           function(i) nrow(ballotPBC[ballotPBC$precinct == precinct[i],]))
table(comb)

data = data.frame()
for (i in 1:length(precinct)) {
  data[i, 1] = ballotPBC$precinct[ballotPBC$precinct == precinct[i]][1]
  
  data[i, 2] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                      ballotPBC$ibuchanan == 0 & 
                                      ballotPBC$inelson == 0 & 
                                      ballotPBC$ideckard == 0], 0)
  data[i, 3] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                      ballotPBC$ibuchanan == 1 & 
                                      ballotPBC$inelson == 0 &
                                      ballotPBC$ideckard == 0], 0)
  data[i, 4] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                      ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 1 &
                                      ballotPBC$ideckard == 0], 0)
  data[i, 5] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                      ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 1 &
                                      ballotPBC$ideckard == 0], 0)
  data[i, 6] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                      ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 0 &
                                      ballotPBC$ideckard == 1], 0)
  data[i, 7] = max(ballotPBC$Freq[ballotPBC$precinct == precinct[i] & 
                                      ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 0 &
                                      ballotPBC$ideckard == 1], 0)
  data[i, 8] = mean(ballotPBC$isabs[ballotPBC$precinct == precinct[i]])
}

colnames(data) = c("precinct", "-B,-N,-D", "B,-N,-D", "-B,N,-D", "B,N,-D", "-B,-N,D", 
                   "B,-N,D", "isabs")

pair.precinct1 = unique(as.character(ballotPBC$precinct[ballotPBC$isabs == 1]))
pair.precinct0 = substr(pair.precinct1, 2, 4)
pair.precinct0 = paste0(0, pair.precinct0)
pair.index = pair.precinct0 %in% precinct
pair.precinct0 = pair.precinct0[pair.index]
pair.precinct1 = pair.precinct1[pair.index]

data.pair0 = sapply(1:length(pair.precinct0), 
                    function(i) data[data$precinct == pair.precinct0[i], 2:7])
data.pair0 = data.frame(pair.precinct0, t(data.pair0))

data.pair1 = sapply(1:length(pair.precinct1), 
                    function(i) data[data$precinct == pair.precinct1[i], 2:7])
data.pair1 = data.frame(pair.precinct1, t(data.pair1))

class(data.pair0[, 2]) = "numeric"; class(data.pair0[, 3]) = "numeric"
class(data.pair0[, 4]) = "numeric"; class(data.pair0[, 5]) = "numeric"
class(data.pair0[, 6]) = "numeric"; class(data.pair0[, 7]) = "numeric"

class(data.pair1[, 2]) = "numeric"; class(data.pair1[, 3]) = "numeric"
class(data.pair1[, 4]) = "numeric"; class(data.pair1[, 5]) = "numeric"
class(data.pair1[, 6]) = "numeric"; class(data.pair1[, 7]) = "numeric"

data.prob0 = data.frame(data.pair0[, 1], data.pair0[, -1] / rowSums(data.pair0[, -1]))
data.prob1 = data.frame(data.pair1[, 1], data.pair1[, -1] / rowSums(data.pair1[, -1]))

# Chi-square Test
chisq.test(data.prob0[, 2], data.prob1[, 2])
chisq.test(data.prob0[, 3], data.prob1[, 3])
chisq.test(data.prob0[, 4], data.prob1[, 4])
chisq.test(data.prob0[, 5], data.prob1[, 5])
chisq.test(data.prob0[, 6], data.prob1[, 6])
chisq.test(data.prob0[, 7], data.prob1[, 7])


# Binomial Regression Fit
summary(glm(ibuchanan ~ inelson + ideckard + isabs, family = "binomial", 
            weights = Freq, data = ballotPBC))

# Simulation
sample = rchisq(n = 1000, df = )

