# Load the Data
load(url("http://www.stat.berkeley.edu/users/nolan/data/Projects/election.rda"))
rm(countyUS)
rm(precintPBC)

# Binomial Regression

# We use two ways to do the regression.

# Method #1

# The dependent variable is whether the voter cast a vote for 
# Buchanan(Ref) or voted for another candidate. 
# In addition to the constant, the regressors are two dummy variables
# that respectively indicate whether the ballot records a vote for 
# Nelson (D) or a vote for Deckard (Ref).

fit.elecday = glm(ibuchanan ~ inelson + ideckard, family = "binomial", weights = Freq,
                  subset = isabs == 0, data = ballotPBC)
summary(fit.elecday)

fit.abs = glm(ibuchanan ~ inelson + ideckard, family = "binomial", weights = Freq,
              subset = isabs == 1, data = ballotPBC)
summary(fit.abs)

# The above output should match Table 3 in the paper.

# Method 2

# Do a complete binomial regression instead of doing separately.
# Remember to consider the interaction terms.

fit = glm(ibuchanan ~ inelson + ideckard + isabs + inelson:isabs + ideckard:isabs,
          family = "binomial", weights = Freq, data = ballotPBC)
summary(fit)

# Compare Coefficients Fitted by Two Ways

coef(fit)[1:3]
coef(fit.elecday)

c(coef(fit)["(Intercept)"] + coef(fit)["isabs"], 
  coef(fit)["inelson1"] + coef(fit)["inelson1:isabs"], 
  coef(fit)["ideckard1"] + coef(fit)["ideckard1:isabs"])
coef(fit.abs)

# We can find that the coefficients are equal by these two fitting method.

# In addition, we can tell the difference between the election day and
# absentee fitted formula, which means the ballot format may affect 
# the vote for Buchanan.

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


# Analyze PBC data in ballot-level
data.elecday = c(sum(ballotPBC$Freq[ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 0
                                    & ballotPBC$ideckard == 0 & ballotPBC$isabs == 0]),
                 sum(ballotPBC$Freq[ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 0
                                    & ballotPBC$ideckard == 0 & ballotPBC$isabs == 0]),
                 sum(ballotPBC$Freq[ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 1
                                    & ballotPBC$ideckard == 0 & ballotPBC$isabs == 0]),
                 sum(ballotPBC$Freq[ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 1
                                    & ballotPBC$ideckard == 0 & ballotPBC$isabs == 0]),
                 sum(ballotPBC$Freq[ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 0
                                    & ballotPBC$ideckard == 1 & ballotPBC$isabs == 0]),
                 sum(ballotPBC$Freq[ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 0
                                    & ballotPBC$ideckard == 1 & ballotPBC$isabs == 0]))

data.abs = c(sum(ballotPBC$Freq[ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 0 
                                & ballotPBC$ideckard == 0 & ballotPBC$isabs == 1]),
             sum(ballotPBC$Freq[ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 0
                                & ballotPBC$ideckard == 0 & ballotPBC$isabs == 1]),
             sum(ballotPBC$Freq[ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 1
                                & ballotPBC$ideckard == 0 & ballotPBC$isabs == 1]),
             sum(ballotPBC$Freq[ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 1
                                & ballotPBC$ideckard == 0 & ballotPBC$isabs == 1]),
             sum(ballotPBC$Freq[ballotPBC$ibuchanan == 0 & ballotPBC$inelson == 0
                                & ballotPBC$ideckard == 1 & ballotPBC$isabs == 1]),
             sum(ballotPBC$Freq[ballotPBC$ibuchanan == 1 & ballotPBC$inelson == 0
                                & ballotPBC$ideckard == 1 & ballotPBC$isabs == 1]))

data = rbind(data.elecday, data.abs)
colnames(data) = c("!B, !N, !D", "B, !N, !D", "!B, N, !D", 
                   "B, N, !D", "!B, !N, D", "B, !N, D")

# Now we have the number of votes for each combination of voting in two ballot formats.

# Chi-square Test

chisq.test(data)

# Note that the p-value is smaller than 2.2e-16, hence we will reject the null hypothesis.
# There is difference between the election-day ballots and absentee ballots. 


# Mosaic Plots

# Create Three Factors

senator = factor(levels = c("Nelson", "Deckard", "Others"))
for (i in 1:nrow(ballotPBC)){
  if (ballotPBC$inelson[i] == 1) senator[i] = "Nelson"
  else {
    if (ballotPBC$ideckard[i] == 1) senator[i] = "Deckard"
    else senator[i] = "Others"
  }
}

president = factor(levels = c("Buchanan", "Others"))
for (i in 1:nrow(ballotPBC)){
  if (ballotPBC$ibuchanan[i] == 1) president[i] = "Buchanan"
  else president[i] = "Others"
}

day = factor(levels = c("Election Day", "Absentee"))
for (i in 1:nrow(ballotPBC)){
  if (ballotPBC$isabs[i] == 1) day[i] = "Absentee"
  else day[i] = "Election Day"
}

# Make Two-Variable Tables and Mosaic Plots

mos.table = table(senator, president, day)
mos.table1 = table(senator, president)
mos.table2 = table(senator, day)
mos.table3 = table(president, day)
mosaicplot(mos.table1)
mosaicplot(mos.table2)
mosaicplot(mos.table3)

# Line Plot

# Calculate the percents of vote for Buchanan for each senator's voters
# in different ballot types

vote.pct = rbind(mos.table[, 1, 1] / rowSums(mos.table[, , 1]),
                 mos.table[, 1, 2] / rowSums(mos.table[, , 2]))
rownames(vote.pct) = c("Election Day", "Absentee")

plot(vote.pct[1, ], type = "l", col = "gold", xaxt = "n", ylim = c(0.1, 0.55),
     main = "Percents of Vote for Buchanan for Each Senator's Supporters\n in Different Ballot Types",
     xlab = "Senator", ylab = "Percents of Vote for Buchanan")
lines(vote.pct[2, ], col = "royalblue", xaxt = "n")
axis(side = 1, at = seq_along(vote.pct[1, ]), labels = colnames(vote.pct))
legend(x = "topright", legend = rownames(vote.pct), fill = c("gold", "royalblue"))

# It seems that for absentee ballots the percents of vote for Buchanan 
# for different senator's supporters are similar 
# while for election-day ballots the percents of vote for Buchanan 
# for different senator's supporters are distinctly different.