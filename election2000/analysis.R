# The dependent variable is whether the voter cast a vote for 
# Buchanan or voted for another candidate. 
# In addition to the constant, the regressors are two dummy variables
# that respectively indicate whether the ballot records a vote for 
# Nelson (D) or a vote for Deckard (Ref).

summary(glm(ibuchanan ~ inelson + ideckard, family = "binomial",
       subset = isabs==0, data = ballot data goes here ))

summary(glm(ibuchanan ~ inelson + ideckard, family = "binomial",
        subset = isabs==1, data = ballot data goes here ))

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


