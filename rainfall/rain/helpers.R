load("data/FrontRange.rda")
myprint <- function (x) 
{
  tmp <- attributes(x)
    y <- x
    attributes(y) <- NULL
    y <- matrix(y, tmp$dim[1], tmp$dim[2])
    colnames(y) <- tmp$dimnames[[2]]
    rownames(y) <- tmp$dimnames[[1]]
    print(y)
}






