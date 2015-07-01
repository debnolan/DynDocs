
exp_curve <- function(func, color,
                      sig = 0.1,  a = 0, 
                      b = 1,  n = 50) {
  
  # constrain gradient to percents that occur between min and max
  x = runif(n, min = 1, max = 9)
  err = rnorm(n, mean = 0, sd = sig)
  
  if(func == "y1"){
    y = exp(a + b*x) + err
  }else{
    y = exp(a + b*x + err)
  }
  return(list(x = x, y = y))
  
}
