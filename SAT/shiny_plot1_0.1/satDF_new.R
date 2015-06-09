setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1_0.1")
load("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/rda_data/satDF.rda")
# data manipulation for easy use of ggplot
# add state.region to satDF
satDF$Region = state.region
# scrape state population data from website
# https://www.census.gov/popest/data/state/totals/1990s/tables/ST-99-03.txt
x = readLines("1995_state_population.txt")
x = x[14:78]
statenames = gsub("[^[:alpha:]]", "", x)
pop = strsplit(x, split = " ")
pop = sapply(pop, function(e) e[length(e)])
# remove extra district Columbia
statenames = statenames[-23]
pop = pop[-23]
pop = as.numeric(as.character(pop))
population_df = data.frame(statenames, pop)
# combine population_df[15:65, ] with satDF
satDF_new = cbind(satDF, StatePopulation = population_df[15:64, 2])
library(plyr)
satDF_new = rename(satDF_new, c("sat" = "SAT Total Score", "math" = "SAT Math Score", "verbal" = "SAT Verbal Score", "expend"="Expenditure", "salary"="Teacher Salary", "ratio" ="Student-Teacher Ratio", "frac" = "Eligible Student Fraction", "StatePopulation" = "State Population"))
save(satDF_new, file = "../rda_data/satDF_new.rda")
load("../rda_data/satDF_new.rda")