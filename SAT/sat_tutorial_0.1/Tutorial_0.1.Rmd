# Simpson's Paradox 
# What is Simpson's Paradox?
# Simpson's paradox is a paradox in probability and statistics, in which a trend that appears
# in different groups of data disappears or reverses when these groups are combined. 

# simple regression demonstration
x1 <- c(2, 3, 4, 5)
y1 <- x1 + 5

x2 <- x1 + 7
y2 <- x2 - 7

# provide the group data, then ask users to combine the groups and run the regression
# then decompsed the combined back to groups
x <- c(x1, x2)
y <- c(y1, y2)

par(xaxs = "i", yaxs = "i")
plot(x,y, cex = 2, pch = 19,
     col = rep(c("blue", "orange"), each = 4), bg = rep(c("blue", "orange"), each = 4),
     xlim = c(0 , 14), ylim = c(0 , 12))
abline(lm(y1 ~ x1), col="blue", lwd = 3, lty =3)
abline(lm(y2 ~ x2), col="orange", lwd = 3, lty = 3)
abline(lm(y  ~ x), lwd = 2, lty = 1)

# kidney stone treatment example: 
# day 1 treats only small stones
# day 2 treats only large stones
day1total = 87 + 270
day2total = 263 + 80
day1A = 87
day1B = day1total - day1A
day2A = 263
day2B = day2total - day2A
day1Ahit = 81
day1Bhit = 234
day2Ahit = 192
day2Bhit = 55

day1Ahit_per = day1Ahit/day1A
day1Bhit_per = day1Bhit/day1B
day2Ahit_per = day2Ahit/day2A
day2Bhit_per = day2Bhit/day2B
table = matrix(c(day1Ahit_per, day1Bhit_per, day2Ahit_per, day2Bhit_per, Atotal, Btotal), byrow = T, ncol = 2, 
       dimnames = list(c("Day1","Day2","Both Days"), c("Treatment A", "Treatment B")))
Atotal = (day1Ahit + day2Ahit)/350
Btotal = (day1Bhit + day2Bhit)/350


# other examples: 
# low birth weight paradox
# Wisconsin and Texas public school ranking 
# Berkeley gender bias 
# Batting averages

# Reference: 
# Wikipedia
# http://vudlab.com/simpsons/
# 







