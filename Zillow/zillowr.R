library ("lattice")
library ("mosaic")
library ("ggplot2")
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


# Density Plot
#if check the "without..." variable
plot (density (real$Price[real$Fireplace=="No"]), xlab = "Price ($)", main = "Density of Price", col = "blue")
#if check "with..." variable
plot(density(real$Price[real$Fireplace == "Yes"]), col="red", main="Density of Price", xlab="Price ($)")
#all
plot(density(real$Price), col="purple", main="Density of Price", xlab="Price ($)")

#if check both the "without..." and "with..." variable
plot (density (real$Price[real$Fireplace=="No"]), xlab = "Price ($)", main = "Density of Price", col = "blue")
lines (density(real$Price[real$Fireplace=="Yes"]), col = "red")
legend (x = 400000, y = 0.000006, col = c("red", "blue"), legend = c("With Fireplace", "Without Fireplace"), pch = 15, pt.cex = 1, cex = 0.6)



#Boxplot
bwplot(Price~Fireplace, xlab="Has Fireplace?", ylab="Price ($)", data=real)


#The fixed scatter plot
#without
xyplot(real$Price[real$Fireplace == "No"]~Living.Area, pch=20, col="blue", auto.key=TRUE, xlab="Living Area", ylab="Price", data=real)
plotFun(mod1fun(Living.Area, Fireplace="No") ~ Living.Area, col="blue", add=TRUE)
#with
xyplot(real$Price[real$Fireplace == "Yes"]~Living.Area, pch=20, col="red", auto.key=TRUE, xlab="Living Area", ylab="Price", data=real)
plotFun(mod1fun(Living.Area, Fireplace= "Yes") ~ Living.Area, col = "red", add=TRUE)


#Scatter plot - check the box
mod1=lm(Price~Living.Area+(Fireplace=="Yes"),data=real)
mod1fun = makeFun(mod1)
xyplot(real$Price ~ real$Living.Area, pch=20, group=real$Fireplace, auto.key=TRUE, xlab="Living Area", ylab="Price ($)")
#with regression line
plotFun(mod1fun(Living.Area, Fireplace= "Yes") ~ Living.Area, col = "red", add=TRUE)

#without
plotFun(mod1fun(Living.Area, Fireplace="No") ~ Living.Area, col="blue", add=TRUE)

#Scatter plot - ALL
mod0=lm(Price~Living.Area,data=real)
xyplot(real$Price ~ real$Living.Area, pch=20, group=real$Fireplace, auto.key=TRUE, xlab="Living Area", ylab="Price ($)")
ladd(panel.abline(mod0))


#choosePlot

#Eg. variable = real$Fireplace
choosePlot = function(variable, plotType) {

  
##boxplot
  if (plotType == "boxplot"){
    real$Sewer.Type[real$Sewer.Type == "None"] = NA
    if (identical (variable, real$Fireplace)) {
      xtitle = "Has Fireplace?"
    }
    if (identical (variable, real$Central.Air)) {
      xtitle = "Has Central Air?"
    }
    if (identical (variable, real$Sewer.Type)) {
      xtitle = "Private or public?"
    }
    if (identical (variable, real$Beds)) {
      xtitle = "How Many Bedrooms?"
    }
    
    bwplot(real$Price~variable, xlab= xtitle, ylab="Price ($)")
  }
  
##density plot
  if (plotType == "densityPlot"){ 
    if (identical (variable, real$Fireplace)) {
      
    plot (density (real$Price[real$Fireplace=="No"]),
          xlab = "Price ($)", main = "Density of Price", col = "blue")
    lines (density(real$Price[real$Fireplace=="Yes"]), col = "red")
    legend (x = 400000, y = 0.000006, col = c("red", "blue"), 
            legend = c("With Fireplace", "Without Fireplace"), 
            pch = 15, pt.cex = 1, cex = 0.6)
    
    #blue for without fireplace, red for with fireplace
    }
    
    
    if (identical (variable, real$Central.Air)) {
      
      plot (density (real$Price[real$Central.Air=="No"]),
            xlab = "Price ($)", main = "Density of Price", col = "orange")
      lines (density(real$Price[real$Central.Air=="Yes"]), col = "purple")
      legend (x = 400000, y = 0.000006, col = c("purple", "orange"), 
              legend = c("With Central Air", "Without Central Air"), 
              pch = 15, pt.cex = 1, cex = 0.6)
      #purple for with central air, orange for without central air
    }
    
    if (identical (variable, real$Sewer.Type)){
      
      plot (density (real$Price[real$Sewer.Type=="Private"]),
            xlab = "Price ($)", main = "Density of Price", col = "cyan")
      lines (density(real$Price[real$Sewer.Type=="Public"]), col = "dark green")
      legend (x = 400000, y = 0.000006, col = c("black", "pink"), 
              legend = c("Private Sewer", "Public Sewer"), 
              pch = 15, pt.cex = 1, cex = 0.6)
      #black for private sewer, pink for public sewer
    }
    
    if (identical (variable, real$Beds)) {
      plot (density (real$Price[real$Beds=="2 or fewer"]),
            xlab = "Price ($)", main = "Density of Price", col = "magenta")
      lines (density(real$Price[real$Beds=="3"]), col = "brown")
      lines (density(real$Price[real$Beds=="4 or more"]), col = "green3")
      legend (x = 400000, y = 0.000006, col = c("magenta", "brown", "green3"), 
              legend = c("2 or fewer bedrooms", "3 bedrooms", "4 or more bedrooms"), 
              pch = 15, pt.cex = 1, cex = 0.6)
      #magneta for 2 or fewer beds, brown for 3 beds, green 3 for 4 or more beds
    }
    
  }
}

choosePlot (real$Sewer.Type, plotType = "densityPlot")









choosePlot = function(variable, plotType, showing = "NA") {
  
  if (plotType == "boxplot"){
    real$Sewer.Type[real$Sewer.Type == "None"] = NA
    if (identical (variable, real$Fireplace)) {
      xtitle = "Has Fireplace?"
    }
    if (identical (variable, real$Central.Air)) {
      xtitle = "Has Central Air?"
    }
    if (identical (variable, real$Sewer.Type2)) {
      xtitle = "Private or public?"
    }
    if (identical (variable, real$Beds)) {
      xtitle = "How Many Bedrooms?"
    }
    return (bwplot(real$Price~variable, xlab= xtitle, ylab="Price ($)"))
  }
  
  if (plotType == "densityPlot" & identical (variable, real$Fireplace)) {
    if (showing =="All") {
      plot (density (real$Price[real$Fireplace=="No"]), xlab = "Price ($)", main = "Density of Price", col = "blue")
      lines (density(real$Price[real$Fireplace=="Yes"]), col = "red")
      legend (x = 400000, y = 0.000006, col = c("red", "blue"), legend = c("With Fireplace", "Without Fireplace"), pch = 15, pt.cex = 1, cex = 0.6)  
    }
    if (showing =="No") {
      return (plot (density (real$Price[real$Fireplace=="No"]), xlab = "Price ($)", main = "Density of Price", col = "blue"))
    }
    if (showing =="Yes") {
      return(plot (density (real$Price[real$Fireplace=="Yes"]), xlab = "Price ($)", main = "Density of Price", col = "Red"))
    }    
  }
  if (plotType == "densityPlot" & identical (variable, real$Central.Air)) {
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
  if (plotType == "densityPlot" & identical (variable, real$Sewer.Type)) {
    if (showing =="All") {
      plot (density (real$Price[real$Sewer.Type=="Private"]), xlab = "Price ($)", main = "Density of Price", col = "green")
      lines (density(real$Price[real$Sewer.Type=="Public"]), col = "purple")
      legend (x = 400000, y = 0.000006, col = c("purple", "green"), legend = c("Public", "Private"), pch = 15, pt.cex = 1, cex = 0.6)  
    }
    if (showing =="Public") {
      return(plot (density (real$Price[real$Sewer.Type=="Public"]), xlab = "Price ($)", main = "Density of Price", col = "purple"))
    }
    if (showing =="Private") {
      return(plot (density (real$Price[real$Sewer.Type=="Private"]), xlab = "Price ($)", main = "Density of Price", col = "green"))
    }    
  }
  if (plotType == "densityPlot" & identical (variable, real$Beds)) {
    if (showing =="All") {
      plot (density (real$Price[real$Beds=="2 or fewer"]), xlab = "Price ($)", main = "Density of Price", col = "#66c2a5")
      lines (density(real$Price[real$Beds=="3"]), col = "#fc8d62")
      lines (density(real$Price[real$Beds=="4 or more"]), col = "#8da0cb")
      legend (x = 400000, y = 0.000006, col = c("#66c2a5", "#fc8d62", "#8da0cb"), legend = c("2 or fewer", "3", "4 or more"), pch = 15, pt.cex = 1, cex = 0.6)  
    }
    if (showing =="2 or fewer") {
      return(plot (density (real$Price[real$Beds=="2 or fewer"]), xlab = "Price ($)", main = "Density of Price", col = "#66c2a5"))
    }
    if (showing =="3") {
      return(plot (density (real$Price[real$Beds=="3"]), xlab = "Price ($)", main = "Density of Price", col = "#fc8d62"))
    }
    if (showing =="4 or more") {
      return(plot (density (real$Price[real$Beds=="4 or more"]), xlab = "Price ($)", main = "Density of Price", col = "#8da0cb"))
    }    
  }
}



