dataFile="./household_power_consumption.txt"
plotFile="plot4.png"
dates=c("1/2/2007","2/2/2007")

##########################################
## readHouseholdPower
## Read in the data from the source file
## 
## Provide some rational defaults for the dates and number
## of rows to read, to allow for quicker debugging and testing

readHouseholdPower <- function(powerFile, dates=c("1/1/1970"), numRows=-1) { 
  powerData <- read.csv (powerFile,
                         colClasses = c("character","character","numeric",
                                        "numeric","numeric","numeric",
                                        "numeric","numeric","numeric") , 
                         sep =';' , na.strings='?', nrows = numRows)
  powerData2<- subset(powerData, (Date %in% dates ))       # subset based on dates
  rm(powerData)                                            # clean up some memory
  powerData2                                               # return dataframe of household 
                                                           # power data
}

##########################################
## buildXAxis - Build our X axis based on dates
##
## Pass in the vector of dates as strings, return a list of days

buildXaxis <- function (dates) {
  dates <- strptime(dates, "%d/%m/%Y")                     # convert to dates
  newDay <- dates[length(dates)]                           # append an additional date to allow 
                                                           # for fence-posting
  newDay$mday <- newDay$mday+1
  dates <- append (dates,newDay)
  days <- format(dates,"%a")                               # convert to strings
  days                                                     # return list of dates
}

##########################################
## makePlot - plot 4 - Four plots in a grid
## 
## Default is to output to the screen, however if
## type = "png" option is passed in then the histogram 
## will be written to a file specifice by outputFile

makePlot <- function (outputFile, plotData, dates=c("1/1/1970"), type="default") {
   if (type == "png") { 
     png(filename=outputFile,width = 480, height = 480, units = "px")
   } 
   
   par(mfrow =c(2,2), mar=c(4, 4, 4, 2))
   
   ## Top Left - Global Active Power - Line Graph
   plot (plotData$Global_active_power, type="l", 
         main ="", ylab = "Global Active Power", xlab="",
         xaxt ="n")                                        # add Global Active Power series 
   axis(1, at=seq(from=1, to=nrow(plotData), 
                  by=(nrow(plotData)/length(dates))-1), 
        labels=buildXaxis(dates))                          # add computed X-Axis
   
   ## Top Right - Voltage - Line Graph
   plot (plotData$Voltage, type="l", 
         main ="", ylab = "Voltage", xlab="datetime",
         xaxt ="n")                                        # add voltage series
   axis(1, at=seq(from=1, to=nrow(plotData), 
                  by=(nrow(plotData)/length(dates))-1), 
        labels=buildXaxis(dates))                          # add computed X-Axis

   ## Bottom Left - Submetering - multicolor 3 value line graph
   plot (plotData$Sub_metering_1, type="l", 
         main ="", ylab = "Energy sub metering", xlab="",
         xaxt ="n", col="black")                           # add Sub_metering_1 series (black)
   lines (plotData$Sub_metering_2, type="l", col="red")    # add Sub_metering_2 series (red)
   lines (plotData$Sub_metering_3, type="l", col="blue")   # add Sub_metering_3 series (blue)
   axis(1, at=seq(from=1, to=nrow(plotData), 
                  by=(nrow(plotData)/length(dates))-1), 
        labels=buildXaxis(dates))                          # add computed X-Axis
   legend("topright",c("Sub_metering_1","Sub_metering_2",
                       "Sub_metering_3"), 
          col=c("black","red","blue"), lty = 1, cex = .8, 
          bty="n" )                                        # add legend - topleft, scaled %80, no border

   ## Bottom Right - Global Reactive Power - Line Graph
   plot (plotData$Global_reactive_power, type="l", 
         main ="", ylab="Global_reactive_power", xlab="datetime",
         xaxt ="n")                                        # add Global Reactive Power series
   axis(1, at=seq(from=1, to=nrow(plotData), 
                  by=(nrow(plotData)/length(dates))-1), 
        labels=buildXaxis(dates))                          # add computed X-Axis
   
   if (type == "png") { dev.off()}  
}

householdData <- readHouseholdPower(dataFile,dates)
makePlot (plotFile,householdData,dates,"png")