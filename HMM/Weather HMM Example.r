#Weather HMM Example

#Problem:
#Using the obseravation of if there is an umbrella as an indicator 
#to model the distribution of the weather is rain or not.

#Initial distribution:
#P(+r) = 0.5
#P(-r) = 0.5

#Transition probability table P(Rt+1|Rt):
#P(+r|+r) = 0.7
#P(-r|+r) = 0.3
#P(+r|-r) = 0.3
#P(-r|-r) = 0.7

#Observation prabability table P(Ut|Rt):
#P(+u|+r) = 0.9
#P(-u|+r) = 0.1
#P(+u|-r) = 0.2
#P(-u|-r) = 0.8

#Trasimission Update Function

TUpdateRain <- function(PRain, PNotRain) {
  return(PRain * 0.7 + PNotRain * 0.3)
}

TUpdateNotRain <- function(PRain, PNotRain) {
  return(PNotRain * 0.7 + PRain * 0.3)
}

#Observation Update Function

OUpdateRain <- function(PRain, PNotRain, observation) {
  if (observation == 1) {
    Pr = PRain * 0.9
    Pnr = PNotRain * 0.2
  }
  
  if (observation == 0) {
    Pr = PRain * 0.1
    Pnr = PNotRain * 0.8
  }
  
  return(Pr/ (Pr + Pnr))
}

OUpdateNotRain <- function(PRain, PNotRain, observation) {
  if (observation == 1) {
    Pr = PRain * 0.9
    Pnr = PNotRain * 0.2
  }
  
  if (observation == 0) {
    Pr = PRain * 0.1
    Pnr = PNotRain * 0.8
  }
  
  return(Pnr/ (Pr + Pnr))
}

#Example Observations:
#Umbrella -> Umbrella -> No Umbrella

#First transimission update:
pr1 = TUpdateRain(0.5, 0.5)  #0.5
pnr1 = TUpdateNotRain(0.5, 0.5)  #0.5

#First observation update:
Opr1 = OUpdateRain(pr1, pnr1, 1)  #0.818
Opnr1 = OUpdateNotRain(pr1, pnr1, 1)  #0.182

#Second transimission update:
pr2 = TUpdateRain(Opr1, Opnr1)  #0.627
pnr2 = TUpdateNotRain(Opr1, Opnr1)  #0.373

#Second observation update:
Opr2 = OUpdateRain(pr2, pnr2, 1)  #0.883
Opnr2 = OUpdateNotRain(pr2, pnr2, 1)  #0.117

#Third transimission update:
pr3 = TUpdateRain(Opr2, Opnr2)  #0.653
pnr3 = TUpdateNotRain(Opr2, Opnr2)  #0.347

#Third observation update:
Opr3 = OUpdateRain(pr3, pnr3, 0)  #0.191
Opnr3 = OUpdateNotRain(pr3, pnr3, 0)  #0.809
