dataFile="./household_power_consumption.txt"
plotFile="./figure/plot2.png"


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
## makePlot - plot 2 - single value line graph
## Build a histoogram of the data 
## 
## Default is to output to the screen, however if
## type = "png" option is passed in then the histogram 
## will be written to a file specifice by outputFile

makePlot <- function (outputFile,plotData,type="default") {
   if (type == "png") { 
     png(filename=outputFile,width = 480, height = 480, units = "px")
   } 
   
   plot (plotData$Global_active_power, type="l", 
         main ="", ylab = "Global Active Power (kilowatts)", xlab="",
         xaxt ="n")
   axis(1, at=c(1,nrow(plotData)/2,nrow(plotData)),labels=c("Thu","Fri","Sat"))
   if (type == "png") { dev.off()}  
}

householdData <- readHouseholdPower(dataFile)
makePlot (plotFile,householdData,"png")