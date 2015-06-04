dataFile="./household_power_consumption.txt"
plotFile="./figure/plot4.png"


##########################################
## readHouseholdPower
## Read in the data from the source file
## 
## Provide some rational defaults for the dates and number
## of rows to read, to allow for quicker debugging and testing
readHouseholdPower <- function(powerFile, numRows=-1, dates=c("1/2/2007","2/2/2007")) {
   powerData <- read.csv (powerFile,
                          colClasses = c("character","character","numeric","numeric","numeric","numeric",
                          "numeric","numeric","numeric") , sep =';' , na.strings='?', nrows = numRows)
   powerData2<- subset(powerData, (Date %in% dates ))
   rm(powerData)  #clean up some memory
   # convert the "Date" column to date objects
   powerData2$Date <- as.Date(powerData2$Date)
   powerData2
   }

##########################################
## makePlot - plot 3 - three value line graph
## Build a histogram of the data 
## 
## Default is to output to the screen, however if
## type = "png" option is passed in then the histogram 
## will be written to a file specifice by outputFile

makePlot <- function (outputFile,plotData,type="default") {
   if (type == "png") { 
     png(filename=outputFile,width = 480, height = 480, units = "px")
   } 
   
   par(mfrow =c(2,2), mar=c(4, 4, 4, 2))
   ## Top Left
   plot (plotData$Global_active_power, type="l", 
         main ="", ylab = "Global Active Power", xlab="",
         xaxt ="n")
   axis(1, at=c(1,nrow(plotData)/2,nrow(plotData)),labels=c("Thu","Fri","Sat"))
   
   ## Top Right
   plot (plotData$Voltage, type="l", 
         main ="", ylab = "Voltage", xlab="datetime",
         xaxt ="n")
   axis(1, at=c(1,nrow(plotData)/2,nrow(plotData)),labels=c("Thu","Fri","Sat"))
   ## Bottom Left
   plot (plotData$Sub_metering_1, type="l", 
         main ="", ylab = "Energy sub metering", xlab="",
         xaxt ="n", col="black")
   lines (plotData$Sub_metering_2, type="l", col="red")
   lines (plotData$Sub_metering_3, type="l", col="blue")
   axis(1, at=c(1,nrow(plotData)/2,nrow(plotData)),labels=c("Thu","Fri","Sat"))
   legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
          col=c("black","red","blue"), lty = 1, cex = .6, bty="n" )

   ## Bottom Right
   plot (plotData$Global_reactive_power, type="l", 
         main ="", ylab="Global_reactive_power", xlab="datetime",
         xaxt ="n")
   axis(1, at=c(1,nrow(plotData)/2,nrow(plotData)),labels=c("Thu","Fri","Sat"))
   
   if (type == "png") { dev.off()}  
}

householdData <- readHouseholdPower(dataFile)
makePlot (plotFile,householdData,"png")