#include <string.h>
#include "BML.h"

// proxy function callable from R
void R_BML(int *grid, int *newGrid, int *dims, int *red, int *numRed, int *blue, int *numBlue, int *velocity, int *numIters) {
  int dir = RED_CAR, i;
  
  for(i = 0; i < *numIters; i++) {
    // move the red cars
    dir = RED_CAR;
    moveCars(grid, newGrid, dims, &dir, numRed, red, velocity + i);
    memcpy(grid, newGrid, sizeof(int) * dims[0] * dims[1]);
    // move blue cars
    dir = BLUE_CAR;
    moveCars(grid, newGrid, dims, &dir, numBlue, blue, velocity + i + *numIters);
    // if no cars moved skip memory operation
    if(velocity[i] == 0 && velocity[i + *numIters] == 0) {
      break;
    }
    memcpy(grid, newGrid, sizeof(int) * dims[0] * dims[1]);
  }
}