# satDF = read.csv("http://www.stat.berkeley.edu/users/nolan/data/sat.csv") 
# save(satDF, file = "satDF.rda")
load("satDF.rda")
### Question: which year is the data drawn from?
### variables: teacher salary(annual? average?); ratio(student-teacher ratio?); frac(eligible student?)
# produce a matrix of scatterplot
#### we can make a static matrix of scatterplot, maybe not necessary...
#### then we can make one correlation scatterplot that allow them to see the individual correlation,
#### and it would also show the corresponding numeric correlation coefficient.

#### For each scatterplot, independent variable x could be SAT overall, Math, Vor erbal; dependent variable
#### can be the rest of the variables (ratio, expend, frac, salary)

## # aslo we can make the scatterplot circle proportional to the state population...
## # also include a pointer that tells which state the point is...

pairs( satDF[ , c("expend","ratio","salary","frac", "sat")] )
# find correlations 
cor( satDF[ , c("expend","ratio","salary","frac", "sat")] )
# expenditures are negatively correlated with SAT
# Four states increase the size, New Jersy, New York, Alaska, Connecticut.
order(satDF$expend, decreasing = T) 
# The fraction of eligible students has the strongest correlation with SAT, 
# and they are negatively correlated.
# Fraction of eligible students is highly positively correlated wtih expenditures
# and teacher 
coplot(sat ~ salary | frac, data = satDF, xlab = "Expenditure", ylab = "SAT score")

lm.Ex = lm(sat ~ expend, data = satDF)
lm.frac = lm(sat ~ frac, data = satDF)   
summary(lm.Ex)
summary(lm.frac)

plot(math ~ salary, data = satDF)
coplot(math ~ salary | frac, data = satDF, xlab = "Teacher Salary", ylab = "SAT score")

## Two-variables 
plot(residuals(lm.Ex) ~ satDF$frac,
     xlab = "Fraction of Eligible Students Taking SAT", ylab = "Residuals", main = "Model: SAT scores ~ Expenditures")
lm.ExFrac = lm(sat ~ expend + frac, data = satDF)
lm.SalFrac = lm(sat ~ salary + frac, data = satDF)
lm.RatFrac = lm(sat ~ ratio + frac, data = satDF)
summary(lm.ExFrac)
summary(lm.SalFrac)

## 3-variable model
## Fit a 3-variable model of SAT on teacher salary, student-teacher ratio, and eligible fraction.
plot(rstandard(lm.3) ~ fitted(lm.3),
     xlab = "Fitted Values", ylab = "Standardized Residuals", main = "Model: sat ~ salary + ratio + frac")






