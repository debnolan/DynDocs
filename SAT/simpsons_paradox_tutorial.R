# Simpson's Paradox 
x1 <- c(2, 3, 4, 5)
y1 <- x1 + 5

x2 <- x1 + 7
y2 <- x2 - 7

x <- c(x1, x2)
y <- c(y1, y2)

par(xaxs = "i", yaxs = "i")
plot(x,y, cex = 2, pch = 19,
     col=rep(c("blue", "orange"), each=4), bg = rep(c("blue", "orange"), each = 4),
     xlim = c(0 , 14), ylim = c(0 , 12))
abline(lm(y1 ~ x1), col="blue", lwd = 3, lty =3)
abline(lm(y2 ~ x2), col="orange", lwd = 3, lty = 3)
abline(lm(y  ~ x), lwd = 2, lty = 1)

# simple drug example: 
day1total = 100 ## slider value change
day2total = 100
day1A = 90
day1B = day1total - day1A
day2A = 10
day2B = day2total - day2A
day1Ahit = 60
day1Bhit = 8
day2Ahit = 4
day2Bhit = 50

day1Ahit_per = day1Ahit/day1A
day1Bhit_per = day1Bhit/day1B
day2Ahit_per = day2Ahit/day2A
day2Bhit_per = day2Bhit/day2B
matrix(c(day1Ahit_per, day1Bhit_per, day2Ahit_per, day2Bhit_per), byrow = T, ncol = 2, 
       dimnames = list(c("Day1","Day2"), c("A", "B")))
Atotal = (day1Ahit + day2Ahit)
Btotal = (day1Bhit + day2Bhit)









