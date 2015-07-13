#IGNORE

drugRaw = read.csv("drugs.csv", header = FALSE,
         col.names = c("calcium", "placebo1", "alcohol", 
                       "placebo2", "therapy2", "therapy1"))

drugData = list(
  "Calcium" = data.frame("Calcium" = drugRaw$calcium[1:11], 
                         "Placebo" = drugRaw$placebo1[1:11]),
  "Alcohol" = data.frame("Alcohol" = drugRaw$alcohol[1:10], 
                         "Placebo" = drugRaw$placebo2[1:10]),
  "HIV" = data.frame("New_Drug" = drugRaw$therapy2[1:15], 
                     "Standard_Drug" = drugRaw$therapy1[1:15])
)

#using this data so the document will work
save(drugData, file = "../data/drugs.RData")


#actual data
Calcium = data.frame(read.csv("Calcium.csv", header = FALSE, 
                              col.names = c("calcium", "placebo1" )))

Alcohol = data.frame(read.csv("Alcohol.csv", header = FALSE,
                              col.names = c("alcohol", "placebo2" )))

HIV = data.frame(read.csv("HIV.csv", header = FALSE,
                          col.names = c("New_Drug", "Standard_Drug" )))

save(Calcium, file = "../data/Calcium.RData")

save(Alcohol, file = "../data/Alcohol.RData")

save(HIV, file = "../data/HIV.RData")
