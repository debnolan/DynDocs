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