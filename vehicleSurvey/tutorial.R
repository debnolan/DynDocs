# file htpop.dat is an artificial population of height of 1000 men and 1000 women.
# Each person’s height is measured to the nearest centimeter (cm)
# htsrs.dat is an SRS of size 200 from the population
# htstrat.dat is a stratified sample of 160 women and 40 men 

htpop = read.csv("htpop.dat")
htsrs = read.csv("htsrs.dat")
htstrat = read.csv("htstrat.dat")


plot(density(htpop$HEIGHT))
plot(density(htsrs$HEIGHT))
median(htpop$HEIGHT)
median(htsrs$HEIGHT)

htboot = replicate(2000, median(sample(htsrs$HEIGHT,200, replace = TRUE)))
hist(htboot)
mean(htboot)
sd(htboot)
quantile(htboot, probs = c(0.025, 0.975))


agpop = read.csv("agpop.dat")
agsrs = read.csv("agsrs.dat")
agstrat = read.csv("agstrat.dat")
