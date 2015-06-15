setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/rda_data/")
load("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/rda_data/satDF.rda")

############################ satDF_one
# data manipulation for easy use of ggplot
# add state.region to satDF
satDF$region = state.region
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
satDF_one = cbind(satDF, population = population_df[15:64, 2])
save(satDF_one, file = "satDF_one.rda")

############################ satDF_new
library(plyr)
satDF_new = rename(satDF_one, c("sat" = "SAT_Total_Score",
                                "math" = "SAT Math Score",
                                "verbal" = "SAT Verbal Score",
                                "expend"="Expenditure",
                                "salary"="Teacher Salary",
                                "ratio" ="Student-Teacher Ratio",
                                "frac" = "Eligible Student Fraction",
                                "region" = "Region",
                                "population" = "State Population"))
save(satDF_new, file = "../rda_data/satDF_new.rda")
load("../rda_data/satDF_new.rda")

############################ satDF_pretty
# reformat data for display
load("../rda_data/satDF_new.rda")
satDF_new$State = state.name
satDF_new$Expenditure = sapply(satDF_new$Expenditure, 
                               function(e) paste("$", as.character(e), sep = ""))
satDF_new$`Teacher Salary` = sapply(satDF_new$`Teacher Salary`, 
                                    function(e) paste("$", as.character(e), sep = ""))
satDF_new$`State Population` = sapply(satDF_new$`State Population`, function(e) format(e, big.mark=",", scientific=FALSE))

satDF_pretty = satDF_new
save(satDF_pretty, file = "../rda_data/satDF_pretty.rda")
load("../rda_data/satDF_pretty.rda")