data = read.table("elephantSealData.txt", header = TRUE,
  col.names = c("day", "lat", "long"))
data = data[36:nrow(data), ]
rownames(data) = NULL

sim = function(data, del = 0.0065, sig = 0.005) {
  phi = data$long[1]*pi/180
  theta = data$lat[1]*pi/180
  Phi = data$long[nrow(data)]*pi/180
  Theta = data$lat[nrow(data)]*pi/180
  journey = matrix(c(phi,theta), ncol = 2)
  
  for (i in 1:65) {
    cosp = sin(theta)*sin(Theta) + cos(theta)*cos(Theta)*cos(Phi-phi)
    cosC = (sin(Theta)-sin(theta)*cosp)/(cos(theta)*sin(acos(cosp)))
    cosc = sin(theta)*cos(del)+cos(theta)*sin(del)*cosC
    cosP = (cos(del)-sin(theta)*cosc)/(cos(theta)*sin(acos(cosc)))
    theta = pi/2-acos(cosc)
    phi = ifelse(Phi<=phi, phi-acos(cosP), phi+acos(cosP))
    phi = phi + rnorm(1, 0, sig/sin(theta))
    theta = theta + rnorm(1, 0, sig)
    journey = rbind(journey, c(phi,theta))
  }
  
  return(journey)
}

tmp = sim(data)*180/pi
plot(-tmp[, 1], tmp[, 2])
