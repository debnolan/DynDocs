#read in data and clean data
library ("lattice")
library ("mosaic")
library ("ggplot2")
real = read.delim("http://sites.williams.edu/rdeveaux/files/2014/09/Saratoga.txt")
options(scipen=5)
summary(real)

real$Fuel.Type=factor(real$Fuel.Type,labels=c("Gas","Electric","Oil")) 
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

#choosePlot function

#choosePlot

choosePlot = function(variable, plotType, showing = "All") {
  
  if (identical(plotType, "boxplot")) {
    
    if (variable == "Fireplace") {
      xtitle = "Has Fireplace?"
    }
    if (variable == "Central.Air") {
      xtitle = "Has Central Air?"
    }
    
    return (bwplot(real[["Price"]] ~ real[[variable]],
                   xlab= xtitle, ylab="Price ($)"))
  }
  
  #density plot
  if (identical(plotType, "densityPlot")) {
    if (variable == "Fireplace") {
      if (showing =="All") {
        plot (density (real$Price[real$Fireplace=="No"]), xlab = "Price ($)", main = "Density of Price", col = "blue")
        lines (density(real$Price[real$Fireplace=="Yes"]), col = "red")
        legend (x = 500000, y = 0.000006, col = c("red", "blue"), legend = c("With Fireplace", "Without Fireplace"), pch = 15, pt.cex = 1, cex = 0.6)  
      }
      if (showing =="No") {
        return (plot (density (real$Price[real$Fireplace=="No"]), xlab = "Price ($)", main = "Density of Price", col = "blue"))
      }
      if (showing =="Yes") {
        return(plot (density (real$Price[real$Fireplace=="Yes"]), xlab = "Price ($)", main = "Density of Price", col = "Red"))
      }    
    }
    #red for with fireplace, blue for without fireplace
    
    #central air
    if (variable == "Central.Air") {
      if (showing =="All") {
        plot (density (real$Price[real$Central.Air=="No"]), xlab = "Price ($)", main = "Density of Price", col = "orange")
        lines (density(real$Price[real$Central.Air=="Yes"]), col = "dark blue")
        legend (x = 400000, y = 0.000006, col = c("dark blue", "orange"), legend = c("With Cantral Air", "Without Central Air"), pch = 15, pt.cex = 1, cex = 0.6)  
      }
      if (showing =="No") {
        return(plot (density (real$Price[real$Central.Air=="No"]), xlab = "Price ($)", main = "Density of Price", col = "orange"))
      }
      if (showing =="Yes") {
        return(plot (density (real$Price[real$Central.Air=="Yes"]), xlab = "Price ($)", main = "Density of Price", col = "dark blue"))
      }    
    }
    #dark blue for with central air, orange for without central air
  }
  
  #scatterplot
  if (identical(plotType, "scatterplot")) {
    
    if (variable == "Fireplace") {
      if (showing =="All") {
        return (xyplot(real$Price ~ real$Living.Area, pch=20, group=real[[variable]], 
                       xlab="Living Area", ylab="Price ($)", 
                       col = c("#f23320bb", "#000bf97a"), xlim = c(400, 5500), ylim = c(0, 800000),
                       key=list(corner=c(1,1),
                                lines=list(col=c("red","blue"), lty=c(2,2), lwd=6),
                                text=list(c("With Fireplace","Without Fireplace")))))
      }
      if (showing =="No") {
        return (xyplot(real$Price[real$Fireplace == showing] ~ real$Living.Area[real$Fireplace == showing], pch=20, 
                       xlab="Living Area", ylab="Price ($)", 
                       col = c("#000bf97a"), xlim = c(400, 5500), ylim = c(0, 800000)))
      }
      if (showing =="Yes") {
        return (xyplot(real$Price[real$Fireplace == showing] ~ real$Living.Area[real$Fireplace == showing], pch=20, 
                       xlab="Living Area", ylab="Price ($)", 
                       col = c("#f23320bb"), xlim = c(400, 5500), ylim = c(0, 800000)))
      }    
    }
    #red for with fireplace, blue for without fireplace
    
    #central air
    if (variable == "Central.Air") {
      if (showing =="All") {
        return (xyplot(real$Price ~ real$Living.Area, pch=20, group=real[[variable]], 
                       xlab="Living Area", ylab="Price ($)", 
                       col = c("#ff7f009a", "#6e77ff7a"), xlim = c(400, 5500), ylim = c(0, 800000),  
                       key=list(corner=c(1,1),
                                lines=list(col=c("#6e77ff","#ff7f00"), lty=c(2,2), lwd=6),
                                text=list(c("With Central Air","Without Central Air")))))
      }
      
      if (showing =="No") {
        return (xyplot(real$Price[real$Central.Air == showing] ~ real$Living.Area[real$Central.Air== showing], pch=20, 
                       xlab="Living Area", ylab="Price ($)", 
                       col = c("#ff7f009a"), xlim = c(400, 5500), ylim = c(0, 800000)))
      }
      if (showing =="Yes") {
        return (xyplot(real$Price[real$Central.Air == showing] ~ real$Living.Area[real$Central.Air == showing], pch=20, 
                       xlab="Living Area", ylab="Price ($)", 
                       col = c("#6e77ff7a"), xlim = c(400, 5500), ylim = c(0, 800000)))
      }    
    }
    #dark blue for with central air, orange for without central air
  }
}


#linear regression
RegreF = function(varName,showing1){
  if (identical (varName, "Fireplace")) {
  if (showing1 =="All") {
  return (xyplot(real$Price ~ real$Living.Area, pch=20, group=real[[varName]], 
  xlab="Living Area", ylab="Price ($)", col = c("#f23320bb", "#000bf97a"), xlim = c(400, 5500), ylim = c(0, 800000),
  panel = function(x, y, ...) {
  panel.superpose(x, y, ..., panel.groups = function(x,y, col, col.symbol, ...) {
                                         panel.xyplot(x, y, col=col.symbol, ...)
                                         panel.abline(lm(y~x), col.line=col.symbol)
                                              }
                       )
                     },
                     key=list(corner=c(1,1),
                              lines=list(col=c("red","blue"), lty=c(2,2), lwd=6),
                              text=list(c("With Fireplace","Without Fireplace")))))
    }
    if (showing1 =="No") {
      return (xyplot(real$Price[real$Fireplace == showing1] ~ real$Living.Area[real$Fireplace == showing1], pch=20, 
                     xlab="Living Area", ylab="Price ($)", 
                     panel = function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       panel.abline(lm(y~x))},
                     col = c("#000bf97a"), xlim = c(400, 5500), ylim = c(0, 800000)))
    }
    if (showing1 =="Yes") {
      return (xyplot(real$Price[real$Fireplace == showing1] ~ real$Living.Area[real$Fireplace == showing1], pch=20, 
                     xlab="Living Area", ylab="Price ($)", 
                     panel = function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       panel.abline(lm(y~x))},
                     col = c("#f23320bb"), xlim = c(400, 5500), ylim = c(0, 800000)))
    }    
  }
  
  if (identical (varName, "Central.Air")) {
    if (showing1 =="All") {
      return (xyplot(real$Price ~ real$Living.Area, pch=20, group=real[[varName]], 
                     xlab="Living Area", ylab="Price ($)", 
                     col = c("#ff7f009a", "#6e77ff7a"), xlim = c(400, 5500), ylim = c(0, 800000),
                     panel = function(x, y, ...) {
                       panel.superpose(x, y, ...,
                                       panel.groups = function(x,y, col, col.symbol, ...) {
                                         panel.xyplot(x, y, col=col.symbol, ...)
                                         panel.abline(lm(y~x), col.line=col.symbol)
                                       }
                       )
                     },
                     key=list(corner=c(1,1),
                              lines=list(col=c("#6e77ff","#ff7f00"), lty=c(2,2), lwd=6),
                              text=list(c("With Central Air","Without Central Air")))))
    }
    
    if (showing1 =="No") {
      return (xyplot(real$Price[real$Central.Air == showing1] ~ real$Living.Area[real$Central.Air== showing1], pch=20, 
                     xlab="Living Area", ylab="Price ($)",
                     panel = function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       panel.abline(lm(y~x))},
                     col = c("#ff7f009a"), xlim = c(400, 5500), ylim = c(0, 800000)))
    }
    if (showing1 =="Yes") {
      return (xyplot(real$Price[real$Central.Air == showing1] ~ real$Living.Area[real$Central.Air == showing1], pch=20, 
                     xlab="Living Area", ylab="Price ($)",
                     panel = function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       panel.abline(lm(y~x))},
                     col = c("#6e77ff7a"), xlim = c(400, 5500), ylim = c(0, 800000)))
    }    
  }
}







