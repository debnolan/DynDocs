// conversion type for car direction constants
typedef enum {RED_CAR = 1, BLUE_CAR = 2} Direction;

// function that updates grid
void moveCars(int *grid, int *newGrid, const int *dims, int *dir, int *numCars, int *carLocations, int *ans);