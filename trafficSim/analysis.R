# load C files
dyn.load("BML.so")

# load simulation R functions
source("tutorial.R")

# sets seed for grid generation
set.seed(12345)

# shows intial and final plots for a sample simulation
# sample 10x10 grid with 50 time steps
showSampleSim = function() {
  g = runGrid(c(10, 10), 0.6, 50)
  par(mfrow=c(1, 2))
  plot(g$initial, main="Intial Grid")
  plot(g$final, main="Final Grid")
  print(g$velocity)
  par(mfrow=c(1, 1))
}

# get sum velocities at 50 time steps in a 10x10 grid
getVelocity = function(density) {
  grid = runGrid(c(10, 10), density, 50)
  total_cars = density * 10 * 10
  return(rowSums(grid$velocity) / total_cars * 100)
}

# find the density at which deadlock occurs
analyzeDensity = function() {
  densities = seq(0.15, 0.95, by=0.1)
  velocity = lapply(densities, getVelocity)
  velocityTable = data.frame(matrix(unlist(velocity), ncol=9, byrow=FALSE))
  names(velocityTable) = densities
  return(velocityTable)
}

plotVelocity = function() {
  data = analyzeDensity()
  colors = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", 
             "#e31a1c", "#bd0026", "#800026")
  plot(0, type='n', xlim=c(1, 50), ylim=c(0, 100), main="Percent of Cars Moving by Density", 
       xlab="Time", ylab="Velocity (percent of cars moving)")
  for (i in 1:9) {
    lines(data[, i], col=colors[i])
  }
}

set.seed(12345)
showSampleSim()
set.seed(12345)
plotVelocity()
