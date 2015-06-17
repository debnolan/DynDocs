transform = function(phi, theta, Phi, Theta) {
  phi1 = phi
  theta1 = theta - Theta
  
  phi2 = acos(sin(phi1)*sin(theta1))
  theta2 = acos(cos(phi1)/sin(phi2))
  
  phi3 = phi2
  theta3 = theta2 - Phi
  
  phi4 = acos(sin(phi3)*cos(theta3))
  theta4 = asin(cos(phi3)/sin(phi4))
  return(c(phi4, theta4))
}



















