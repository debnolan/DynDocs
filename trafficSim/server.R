source("tutorial.R")
dyn.load("~/Documents/projects/DynDocs/trafficSim/BML.so")

shinyServer(
  function(input, output) {
    output$sim_plot = renderPlot({
      getSimPlot(input$sim_size_x, input$sim_size_y, input$sim_time_steps, input$sim_red_cars, 
                 input$sim_blue_cars, input$sim_seed, input$sim_step_slider)
    })
    
    output$density_table = renderTable({
      getVelocityTable(input$density_size_x, input$density_size_y, input$density_time_steps, 
                       input$density_seed)
    })
    
    output$density_plot = renderPlot({
      getVelocityPlot(input$density_size_x, input$density_size_y, input$density_time_steps, 
                      input$density_seed, input$density_select)
    })
  }
)

# get plot of simulation with initial grid and final grid
getSimPlot = function(size_x, size_y, time_steps, red_cars, blue_cars, seed, step) {
  set.seed(seed)
  initial_grid = runGrid(c(size_x, size_y), c(red_cars, blue_cars), step)
  set.seed(seed)
  grid = runGrid(c(size_x, size_y), c(red_cars, blue_cars), time_steps)
  par(mfrow=c(1, 2))
  plot(initial_grid$final, main="Intial Grid")
  plot(grid$final, main="Final Grid")
  par(mfrow=c(1, 1))
}

# get sum velocities of all cars in grid
getVelocity = function(density, size_x, size_y, time_steps) {
  grid = runGrid(c(size_x, size_y), density, time_steps)
  total_cars = density * size_x * size_y
  return(rowSums(grid$velocity) / total_cars * 100)
}

# get velocities of grid at various densities
getVelocityTable = function(size_x, size_y, time_steps, seed) {
  densities = seq(0.15, 0.95, by=0.1)
  set.seed(seed)
  velocity = lapply(densities, getVelocity, size_x, size_y, time_steps)
  velocityTable = data.frame(matrix(unlist(velocity), ncol=9, byrow=FALSE))
  names(velocityTable) = densities
  return(velocityTable)
}

# plots velocities of grid at various densities
getVelocityPlot = function(size_x, size_y, time_steps, seed, densities) {
  data = getVelocityTable(size_x, size_y, time_steps, seed)
  colors = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", 
             "#e31a1c", "#bd0026", "#800026")
  plot(0, type='n', xlim=c(1, time_steps), ylim=c(0, 100), 
       main="Percent of Cars Moving by Density", xlab="Time", 
       ylab="Velocity (percent of cars moving)")
  for (i in 1:9) {
    if (names(data)[i] %in% densities) {
      lines(data[, i], col=colors[i])
    }
  }
}