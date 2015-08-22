

tt = readLines("Eg.json")
source("../analysis.r")

o = jsonParser(tt)
o1 = jsonParser1(tt)

stopifnot(identical(o, o1))

