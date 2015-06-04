#include <string.h>
#include "BML.h"

// moves cars and updates grids
void moveCars(int *grid, int *newGrid, const int *dims, int *dir, int *numCars, int *carLocations, int *ans) {
  int k, i, j, ni, nj;
  int colorToMove = *dir;
  int numMoved = 0;
  
  memcpy(newGrid, grid, sizeof(int) * dims[0] * dims[1]);
  // iterate through all cars
  for(k = 0; k < *numCars; k++) {
    //find new positions
    i = carLocations[k] - 1;
    j = carLocations[k + *numCars] - 1;
    if(colorToMove == RED_CAR) {
      ni = i;
      nj = j + 1;
      if(nj == dims[1]) {
        nj = 0; 
      }
    } else {
      nj = j;
      ni = i + 1;
      if(ni == dims[0]) {
        ni = 0;
      }
    }
    // move car
    if(grid[ni + nj * dims[0]] == 0) {
      numMoved++;
      newGrid[ni + nj * dims[0]] = colorToMove;
      newGrid[i + j * dims[0]] = 0;
      if(colorToMove == RED_CAR) {
        carLocations[k + *numCars] = nj + 1;
      } else {
        carLocations[k] = ni + 1;
      }
    }
  }
  // return number of cars moved as velocity
  *ans = numMoved;
}
