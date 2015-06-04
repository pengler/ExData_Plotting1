dataFile="./household_power_consumption.txt"
plotFile="./figure/plot2.png"
dates=c("1/2/2007","2/2/2007")

##########################################
## readHouseholdPower
## Read in the data from the source file
## 
## Provide some rational defaults for the dates and number
## of rows to read, to allow for quicker debugging and testing
readHouseholdPower <- function(powerFile, dates=c("1/1/1970"), numRows=-1) {
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
## buildXAxis - Build our X axis based on dates
##
## Prototype - pass in the vector of dates as strings
##           - convert to date objects
##           - add additional date at the end
##           - return the vector as string 
##           - use the length of the vector to set at points

##########################################
## makePlot - plot 2 - single value line graph
## Build a histoogram of the data 
## 
## Default is to output to the screen, however if
## type = "png" option is passed in then the histogram 
## will be written to a file specifice by outputFile


## we will call the buildXAxis here

makePlot <- function (outputFile, plotData, dates=c("1/1/1970"), type="default") {
   if (type == "png") { 
     png(filename=outputFile,width = 480, height = 480, units = "px")
   } 
   
   plot (plotData$Global_active_power, type="l", 
         main ="", ylab = "Global Active Power (kilowatts)", xlab="",
         xaxt ="n")
   axis(1, at=c(1,nrow(plotData)/2,nrow(plotData)),labels=c("Thu","Fri","Sat"))
   if (type == "png") { dev.off()}  
}

householdData <- readHouseholdPower(dataFile,dates)
makePlot (plotFile,householdData,dates, "png")
