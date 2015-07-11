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

save(drugData, file = "drugs.RData")
