naiveBayes = function(handWritten){
  
  prob = list(1:784,1:784,1:784,1:784,1:784,1:784,1:784,1:784,1:784,1:784)
  
  for (i in 1:length(trainMean)){
    for (j in 1:784){
      if (handWritten[j] == 0){
        prob[[i]][j] = 1 - as.numeric(trainMean[[i]][j])
      }else{
        prob[[i]][j] = as.numeric(trainMean[[i]][j])
      }
    }
  }
  
  
  result1 = lapply(prob,function(x) -log(x))
  result2 = unlist(lapply(result1,sum))
  
  finalResult = (-log(as.numeric(trainPrior))) + result2 
  
  theNumber = which(min(finalResult)==finalResult,finalResult) - 1
  
  return(theNumber)
  
}

