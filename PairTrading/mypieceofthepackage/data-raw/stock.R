readData =
  function(fileName, dateFormat = c("%Y-%m-%d", "%Y/%m/%d"), ...)
  {
    data = read.csv(fileName, header = TRUE, 
                    stringsAsFactors = FALSE, ...)
    for(fmt in dateFormat) {
      tmp = as.Date(data$Date, fmt)
      if(all(!is.na(tmp))) {
        data$Date = tmp
        break
      }
    }
    
    data[ order(data$Date), ]
  }

att = readData("../data-raw/ATT.csv")
verizon = readData("../data-raw/VERIZON.csv")
southwest = readData("../data-raw/southwest.csv")
united = readData("../data-raw/united.csv")
hilton = readData("../data-raw/hilton.csv")
hyatt = readData("../data-raw/hyatt.csv")
hershey = readData( "../data-raw/hershey.csv")
kellog = readData("../data-raw/kellog.csv")
toyota = readData( "../data-raw/toyota.csv")
inc = readData( "../data-raw/inc.csv")
ibm = readData( "../data-raw/ibm.csv")
gm = readData( "../data-raw/gm.csv")

stock = list( att, verizon, southwest,united,hilton,hyatt,hershey,kellog,toyota,inc,ibm,gm)

save( stock, file="stock.RData")
