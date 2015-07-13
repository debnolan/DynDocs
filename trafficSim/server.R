source("tutorial.R")
# either set working directory or change path relative to local root
dyn.load("BML.so")

shinyServer(
  function(input, output) {
    output$sim_plot = renderPlot({
      input$sim_create
      
      isolate({
        sim_size_x = input$sim_size_x
        sim_size_y = input$sim_size_y
        sim_time_steps = input$sim_time_steps
        sim_red_cars = input$sim_red_cars
        sim_blue_cars = input$sim_blue_cars
        sim_seed = input$sim_seed
      })
      
      getSimPlot(sim_size_x, sim_size_y, sim_time_steps, sim_red_cars, sim_blue_cars, 
                 sim_seed, input$sim_step_slider)
    })
    
    output$sim_gridlock = renderText({
      input$sim_create
      
      isolate({
        sim_size_x = input$sim_size_x
        sim_size_y = input$sim_size_y
        sim_time_steps = input$sim_time_steps
        sim_red_cars = input$sim_red_cars
        sim_blue_cars = input$sim_blue_cars
        sim_seed = input$sim_seed
      })
      
      gridlock_time = getGridlock(sim_size_x, sim_size_y, sim_time_steps, sim_red_cars, sim_blue_cars, sim_seed)
      if(is.na(gridlock_time)) {
        "Grid is free flowing at final state"
      } else {
        paste0("Gridlock at time ", as.character(gridlock_time - 1))
      }
    })
    
    output$density_table = renderTable({
      input$density_create
      
      isolate({
        density_size_x = input$density_size_x
        density_size_y = input$density_size_y
        density_time_steps = input$density_time_steps
        density_seed = input$density_seed
      })
      
      getVelocityTable(density_size_x, density_size_y, density_time_steps, density_seed)
    })
    
    output$density_plot = renderPlot({
      input$density_create
      
      isolate({
        density_size_x = input$density_size_x
        density_size_y = input$density_size_y
        density_time_steps = input$density_time_steps
        density_seed = input$density_seed
      })
      
      getVelocityPlot(density_size_x, density_size_y, density_time_steps, density_seed, input$density_select)
    })
  }
)

# get plot of simulation with initial grid and final grid
getSimPlot = function(size_x, size_y, time_steps, red_cars, blue_cars, seed, step) {
  set.seed(seed)
  initial_grid = runGrid(c(size_y, size_x), c(red_cars, blue_cars), step)
  set.seed(seed)
  grid = runGrid(c(size_y, size_x), c(red_cars, blue_cars), time_steps)
  par(mfrow=c(1, 2))
  plot(initial_grid$final, main="Current Grid")
  plot(grid$final, main="Final Grid")
  par(mfrow=c(1, 1))
}

# get time step at which grid reaches gridlock
getGridlock = function(size_x, size_y, time_steps, red_cars, blue_cars, seed) {
  set.seed(seed)
  grid = runGrid(c(size_y, size_x), c(red_cars, blue_cars), time_steps)
  velocity = rowSums(grid$velocity)
  return(match(0, velocity))
}

# get sum velocities of all cars in grid
getVelocity = function(density, size_x, size_y, time_steps) {
  grid = runGrid(c(size_y, size_x), density, time_steps)
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
  colors = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")
  plot(0, type='n', xlim=c(1, time_steps), ylim=c(0, 100),
       main="Percent of Cars Moving by Density", xlab="Time", 
       ylab="Velocity (percent of cars moving)")
  for (i in 1:9) {
    if (names(data)[i] %in% densities) {
      lines(data[, i], col=colors[i], lwd=3)
    }
  }
}