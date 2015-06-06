dataFile="./household_power_consumption.txt"
plotFile="plot2.png"
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
  powerData2<- subset(powerData, (Date %in% dates ))      # subset based on dates
  rm(powerData)                                           # clean up some memory
  powerData2                                              # return dataframe of household 
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
## makePlot - plot 2 - single value line graph
## Build a histogram of the data 
## 
## Default is to output to the screen, however if
## type = "png" option is passed in then the histogram 
## will be written to a file specifice by outputFile

makePlot <- function (outputFile, plotData, dates=c("1/1/1970"), type="default") {
   if (type == "png") { 
     png(filename=outputFile,width = 480, height = 480, units = "px")
   } 
   
   plot (plotData$Global_active_power, type="l", main ="",
         ylab = "Global Active Power (kilowatts)", 
         xlab="", xaxt ="n")                               # add Global Active Power series
   
   axis(1, at=seq(from=1, to=nrow(plotData), 
                  by=(nrow(plotData)/length(dates))-1), 
        labels=buildXaxis(dates))                          # add computed X-Axis
   
   if (type == "png") { dev.off()}  
}

householdData <- readHouseholdPower(dataFile,dates)
makePlot (plotFile,householdData,dates,"png")
