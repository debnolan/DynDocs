# This uses global variables trainMean and trainPrior

naiveBayes = function(handWritten)
{

  prob = list(1:784,1:784,1:784,1:784,1:784,1:784,1:784,1:784,1:784,1:784)
  
  for (i in 1:length(trainMean)){
    for (j in 1:784){
      if (handWritten[j] == 0){
        prob[[i]][j] = 1 - as.numeric(trainMean[[i]][j])
      } else {
        prob[[i]][j] = as.numeric(trainMean[[i]][j])
      }
    }
  }

  result1 = lapply(prob, function(x) -log(x))  

  
   # You can replace the initialization of prob and the 2 for loops, and the -log with 
  prob1 = lapply(trainMean, function(x) 
                                as.numeric( - log( ifelse(handWritten == 0,  1 - x, x) )) )

  result2 = unlist(lapply(result1, sum))
# The following is simpler, although we can also just put the sum into the definition of prob1 above and skip this line.
  result2 = sapply(result1, sum)

  
  finalResult = (-log(as.numeric(trainPrior))) + result2 

      # ? what is the second argument to which() doing here, i.e. the last finalResult in the expression?
  theNumber = which(min(finalResult) == finalResult, finalResult) - 1
  
  return(theNumber)
  
}



# From what I can make of this function,
# it is the equivalent of
#    vector[, ncol(vector):1]
# but the column reordering is shorter, clearer, more efficient
# and more general (not being fixed to 28 x 28 images)
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


# This is okay as is, but of course is using knowledge of the JSON content.
# This is now done in the function below jsonParser1, along with removing the need for reverser().

# It is not clear that it has taken into account the first part of the path, i.e. the M(ove)
# Instead,
#  tmp = fromJSON(jsonFormat)
#  tmp$objects[[1]]$path  # is the same as coordinates but with the directive (e.g., "Q", "M", "L")
#  coordinatesList = lapply(tmp$objects[[1]]$path, function(x) unlist(x[-1]))
# So
#   tmp = fromJSON(jsonFormat)
#   coordinatesList = lapply(tmp$objects[[1]]$path, function(x) unlist(x[-1]))
#   coordinatesList = coordinatesList[ 2:(length(coordinatesList) - 1) ]
# replaces the first 5 lines and also leaves us with more information about the steps and the overall canvas contents.
#
#   myMat = matrix(0, 28, 28) avoids rep(0, 784)   and why so many hard-coded numbers!
#
#  The loop is just extracting the i and i+1 elements from coordinatesList to get the row and column.
#  So we can do this with
#   coordinatesList = matrix(coordinatesList, , 2, byrow = TRUE)
#   myMat[ coordinatesList ] = 1
#   myMat[, ncol(myMat):1]

jsonParser = function(jsonFormat){
  input = gsub("\"","",jsonFormat)
  coordinates = unlist(strsplit(input,"[]],[[][QL],"))
  coordinates = coordinates[2:length(coordinates)]
  coordinates = coordinates[1:length(coordinates)-1] #??? Why not do this in the previous line! Also, omitting the first and last coordinates from the array since not split from other text 
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


library(RJSONIO)

jsonParser1 =
function(jsonFormat)
{
    tmp = fromJSON(jsonFormat)
    coordinatesList = lapply(tmp$objects[[1]]$path, function(x) {
                                                        x = unlist(x[-1])
                                                        c( round(((x[1]+x[3])/2)*(28/300)),round(((x[2]+x[4])/2)*(28/300)))
                                                    })
    coordinatesList = matrix(unlist(coordinatesList), , 2, byrow = TRUE)
    myMat = matrix(0, 28, 28)    
    myMat[ coordinatesList ] = 1
    myMat[, ncol(myMat):1]       # no need to define the reverser() function.
}


