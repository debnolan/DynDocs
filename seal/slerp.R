slerp = function(p1, p2, n) {
# The input p1 is the (latitude, longitude) pair of the first point
# The input p2 is the (latitude, longitude) pair of the second point
# The input n is the number of lat lon pairs on the great circle between the 
# two points (longitude will be evenly spaced). 
# The output is a matrix with two columns (lat and lon) and num rows 

  t = seq(0, 1, length = n)
  lat1 = (90 - p1[1]) * pi/180
  long1 = p1[2] * pi/180  
  
  x1 = sin(lat1)*cos(long1)
  y1 = sin(lat1)*sin(long1)
  z1 = cos(lat1)
  cart1 = c(x1,y1,z1)

  lat2 = (90 - p2[1]) * pi/180
  long2 = p2[2] * pi/180  

  x2 = sin(lat2)*cos(long2)
  y2 = sin(lat2)*sin(long2)
  z2 = cos(lat2)
  cart2 = c(x2,y2,z2)

  om = acos(sum(cart1*cart2))
  
  val=array(dim=c(length(t),2))
  for (i in 1:length(t)) {
    interp = sin((1-t[i])*om)/sin(om)*cart1 + sin(t[i]*om)/sin(om)*cart2
    lat = 90 - acos(interp[3]/sqrt(sum(interp^2))) * 180 / pi
    long = atan2(interp[2],interp[1]) * 180 / pi
    val[i,] = c(lat=lat,long=long)
  }
 colnames=c("lat","long")
 return(val)
}

t = slerp(c(40,-130),c(42,-135), 100)
# t <- slerp(c(30,-60),c(75,-100),0:30/30)
#  plot(t[,2], t[,1])
