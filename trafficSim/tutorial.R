# create and populate grid of cars
# returns matrix of class BMLGrid
createGrid = function(dims = c(100, 100), numCars = 0.3) {
  if(length(dims) == 1) {
    dims = rep(dims, 2)
  }
  if(length(numCars) == 1 && numCars < 1) {
    numCars = rep(prod(dims) * numCars/2, 2)
  }
  grid = matrix("", dims[1], dims[2])
  pos = sample(1:prod(dims), sum(numCars))
  grid[pos] = sample(rep(c("red", "blue"), ceiling(numCars)))[seq(along = pos)]
  class(grid) = c("BMLGrid", "matrix")
  return(grid)
}

# print grid as strings
print.BMLGrid = function(x, ...) {
  print(structure(noquote(x[nrow(x):1,]), dimnames = list(nrow(x):1, 1:ncol(x))))
}

# plot grid with red and blue boxes
plot.BMLGrid = function(x, xlab = "", ylab = ""...) {
  if(typeof(x) == "character") {
    z = matrix(match(x, c("", "red", "blue")), nrow(x), ncol(x))
  } else {
    z = x
  }
  image(t(z), col = c("white", "red", "blue"), axes = FALSE, xlab = xlab, ylab = ylab, ...)
  box()
}

# get locations of all cars in grid
# returns matrix of x and y coordinates
getCarLocations = function(g) {
  w = (g != "")
  i = row(g)[w]
  j = col(g)[w]
  pos = cbind(i, j)
  return(structure(pos, dimnames = list(g[pos], c("i", "j"))))
}

# calculate next position given a position
# returns vector of (x, y)
getNextPosition = function(curPos, dim, horizontal = TRUE) {
  if(horizontal) {
    if(curPos[2] == dim[2]) {
      y = 1L
    } else {
      y = curPos[2] + 1L
    }
    return(c(curPos[1], y))
  } else {
    if(curPos[1] == dim[1]) {
      x = 1L
    } else {
      x = curPos[1] + 1L
    } 
    return(c(x, curPos[2]))
  }
}

# move all cars of a color in grid in one time step
# returns updated grid
moveCars = function(grid, color = "red") {
  cars = getCarLocations(grid)
  w = which(rownames(cars) == color)
  rows = cars[w, 1]
  cols = cars[w, 2]
  if(color == "red") {
    nextRows = rows
    nextCols = cols + 1L
    nextCols[nextCols > ncol(grid)] = 1L
  } else {
    nextRows = rows + 1L
    nextRows[nextRows > nrow(grid)] = 1L
    nextCols = cols
  }
  nextLocs = cbind(nextRows, nextCols)
  w = grid[nextLocs] == ""
  grid[nextLocs[w, , drop = FALSE]] = color
  grid[cbind(rows, cols)[w, , drop = FALSE]] = ""
  return(grid)
}

# moves all cars numSteps time steps
# returns updated grid
runBML = function(grid = createGrid(...), numSteps = 100) {
  for(i in 1:numSteps) {
    grid = moveCars(grid, "red")
    grid = moveCars(grid, "blue")
  }
  return(grid)
}

# converts grid from characters to integers
# returns matrix with blank as 0, red cars as 1, and blue cars as 2
gridToIntegerGrid = function(g) {
  return(matrix(match(g, c("red", "blue"), 0L), nrow(g), ncol(g)))
}

# call C code to move cars numIter time steps
# returns updated grid
# run dyn.load("...BML.so") to load c files
crunBML = function(grid, numIter = 100L) {
  k = class(grid)
  gi = gridToIntegerGrid(grid)
  velocity = matrix(0L, as.integer(numIter), 2L, dimnames = list(NULL, c("red", "blue")))
  pos = getCarLocations(gi)
  red = pos[rownames(pos) == "1", ]
  blue = pos[rownames(pos) == "2", ]
  ans = .C("R_BML", gi, grid = gi, dim(gi), red = red, nrow(red), blue = blue, nrow(blue), velocity = velocity, as.integer(numIter))
  ans = ans[c("grid", "velocity")]
  class(ans$grid) = k
  # return(ans)
  return(ans$grid)
}

# creates and runs grid simulation
# returns list of initial grid, resulting grid and velocity at each step
runGrid = function(dims, numCars, numIter = 1000, plot = TRUE) {
  grid = createGrid(dims, numCars)
  g.out = crunBML(grid, numIter)
  if(plot) {
    plot(grid)
    plot(g.out)
  }
  return(invisible(list(initial = grid, final = g.out, velocity = g.out)))
}
