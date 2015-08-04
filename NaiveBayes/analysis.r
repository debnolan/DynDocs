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


reverser = function(vector){
  new = rep(list(rep(0,28)), 28) 
  newVector = c()
  for(i in 1:28){
    rowStart = ((i-1)*28) + 1
    rowEnd = i*28
    new[[29-i]] = vector[rowStart:rowEnd]
  }
  for(j in 1:28){
    newVector = c(newVector,new[[j]])
  }  
  return(newVector)
}

jsonParser = function(jsonFormat){
  input = gsub("\"","",jsonFormat)
  coordinates = unlist(strsplit(input,"[]],[[][QL],"))
  coordinates = coordinates[2:length(coordinates)]
  coordinates = coordinates[1:length(coordinates)-1]
  coordinatesList = lapply(strsplit(coordinates,","),as.numeric)
  coordinatesList = lapply(coordinatesList, function(x) c( round(((x[1]+x[3])/2)*(28/300)),round(((x[2]+x[4])/2)*(28/300))))
  coordinatesFinal = unlist(coordinatesList)
  myMat = matrix(rep(0,784),28,28)
  for (i in 1:(length(coordinatesFinal)/2)){
    myMat[coordinatesFinal[2*i-1],coordinatesFinal[2*i]] = 1
  }
  myMat = matrix(reverser(myMat),28,28)
  return(myMat)
}
