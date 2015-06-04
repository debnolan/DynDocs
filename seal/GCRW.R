GCRandomWalk = function(lonBegin, latBegin, lonEnd, latEnd, 
    spe = 0.0065, sig =  0.005) 
{
# lat and lon MUST be in RADIANS 
 pi2<-pi/2

 px = lonBegin; pp = latBegin
 px1 = lonEnd ; pp1 = latEnd
 x = px ; p = pp 

 for(i in 1:65){
   cosp = cos(pi2-pp)*cos(pi2-pp1)+sin(pi2-pp)*sin(pi2-pp1)*cos(px1-px)
   cosC = (cos(pi2-pp1)-cos(pi2-pp)*cosp)/(sin(pi2-pp)*sin(acos(cosp)))
   cosc = cos(pi2-pp)*cos(spe)+sin(pi2-pp)*sin(spe)*cosC
   cosP = (cos(spe)-cos(pi2-pp)*cosc)/(sin(pi2-pp)*sin(acos(cosc)))
   pp = pi2-acos(cosc)
   px = ifelse(px1<=px, px-acos(cosP), px+acos(cosP))
   px = px+rnorm(1,0,sig/sin(pp))
   pp = pp+rnorm(1,0,sig)
   x = c(x,px)
   p = c(p,pp)
  }

 return(matrix(c(x,p), ncol = 2, byrow=FALSE))
}
