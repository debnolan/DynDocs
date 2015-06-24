
real = read.delim("http://sites.williams.edu/rdeveaux/files/2014/09/Saratoga.txt")
options(scipen=5)
summary(real)

real$Fuel.Type=factor(real$Fuel.Type,labels=c("Gas","Electric","Oil")) ###????why only 3 levels and number not maching the description
real$Sewer.Type=factor(real$Sewer.Type,labels=c("None","Private","Public")) 
real$Heat.Type=factor(real$Heat.Type,labels=c("Hot Air","Hot Water","Electric")) 
real$Waterfront=factor(real$Waterfront,labels=c("No","Yes")) 
real$Central.Air=factor(real$Central.Air,labels=c("No","Yes")) 
real$New.Construct=factor(real$New.Construct,labels=c("No","Yes")) 
real$Fireplace=factor(real$Fireplaces==0,labels=c("Yes","No"))
real$Fireplaces=factor(real$Fireplaces) 
levels(real$Fireplaces)=c("None","1","2 or more","2 or more","2 or more") 
real$Beds=as.factor(real$Bedrooms)
levels(real$Beds)=c("2 or fewer","2 or fewer","3","4 or more","4 or more","4 or more","4 or more")


choosePlot = function(varName = "Fireplace", plotType = "boxplot",
  showing = "NA") {
  
  if (plotType == "boxplot"){
    real$Sewer.Type[real$Sewer.Type == "None"] = NA
    if (varName == "Fireplace") {
      xtitle = "Has Fireplace?"
    }
    if (varName == "Central.Air") {
      xtitle = "Has Central Air?"
    }
    if (varName == "Sewer.Type") {
      xtitle = "Private or public?"
    }
    if (varName == "Beds") {
      xtitle = "How Many Bedrooms?"
    }
    return (bwplot(real[["Price"]] ~ real[[varName]],
                   xlab= xtitle, ylab="Price ($)"))
  }
  
  if (plotType == "densityPlot") {
    if (varName == "Fireplace") {
      if (showing =="All") {
      plot (density (real$Price[real$Fireplace=="No"]),
            xlab = "Price ($)", main = "Density of Price", col = "blue")
      lines (density(real$Price[real$Fireplace=="Yes"]), col = "red")
      legend (x = 400000, y = 0.000006, col = c("red", "blue"),
              legend = c("With Fireplace", "Without Fireplace"),
              pch = 15, pt.cex = 1, cex = 0.6)  
    }
    if (showing =="No") {
      return (plot (density (real$Price[real$Fireplace=="No"]), xlab = "Price ($)", main = "Density of Price", col = "blue"))
    }
    if (showing =="Yes") {
      return(plot (density (real$Price[real$Fireplace=="Yes"]), xlab = "Price ($)", main = "Density of Price", col = "Red"))
    }    
  }
  if (varName == "Central.Air") {
    if (showing =="All") {
      plot (density (real$Price[real$Central.Air=="No"]),
            xlab = "Price ($)", main = "Density of Price", col = "orange")
      lines (density(real$Price[real$Central.Air=="Yes"]), col = "dark blue")
      legend (x = 400000, y = 0.000006, col = c("dark blue", "orange"),
              legend = c("With Cantral Air", "Without Central Air"),
              pch = 15, pt.cex = 1, cex = 0.6)  
    }
    if (showing =="No") {
      return(plot (density (real$Price[real$Central.Air=="No"]),
                   xlab = "Price ($)", main = "Density of Price", col = "orange"))
    }
    if (showing =="Yes") {
      return(plot (density (real$Price[real$Central.Air=="Yes"]), xlab = "Price ($)", main = "Density of Price", col = "dark blue"))
    }    
  }
  if (varName == "Sewer.Type") {
    if (showing =="All") {
      plot (density (real$Price[real$Sewer.Type=="Private"]),
            xlab = "Price ($)", main = "Density of Price", col = "green")
      lines (density(real$Price[real$Sewer.Type=="Public"]), col = "purple")
      legend (x = 400000, y = 0.000006, col = c("purple", "green"),
              legend = c("Public", "Private"), pch = 15, pt.cex = 1, cex = 0.6)  
    }
    if (showing =="Public") {
      return(plot (density (real$Price[real$Sewer.Type=="Public"]),
                   xlab = "Price ($)", main = "Density of Price", col = "purple"))
    }
    if (showing =="Private") {
      return(plot (density (real$Price[real$Sewer.Type=="Private"]),
                   xlab = "Price ($)", main = "Density of Price", col = "green"))
    }    
  }
  if (varName == "Beds") {
    if (showing =="All") {
      plot (density (real$Price[real$Beds=="2 or fewer"]),
            xlab = "Price ($)", main = "Density of Price", col = "#66c2a5")
      lines (density(real$Price[real$Beds=="3"]), col = "#fc8d62")
      lines (density(real$Price[real$Beds=="4 or more"]), col = "#8da0cb")
      legend (x = 400000, y = 0.000006, col = c("#66c2a5", "#fc8d62", "#8da0cb"),
              legend = c("2 or fewer", "3", "4 or more"), pch = 15,
              pt.cex = 1, cex = 0.6)  
    }
    if (showing =="2 or fewer") {
      return(plot (density (real$Price[real$Beds=="2 or fewer"]),
                   xlab = "Price ($)", main = "Density of Price", col = "#66c2a5"))
    }
    if (showing =="3") {
      return(plot (density (real$Price[real$Beds=="3"]),
                   xlab = "Price ($)", main = "Density of Price",
                   col = "#fc8d62"))
    }
    if (showing =="4 or more") {
      return(plot (density (real$Price[real$Beds=="4 or more"]),
                   xlab = "Price ($)", main = "Density of Price", col = "#8da0cb"))
    }    
  }
  }
}
