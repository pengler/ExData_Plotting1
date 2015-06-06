dataFile="./household_power_consumption.txt"
plotFile="plot1.png"
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
## makePlot
## Build a histogram of the data 
## 
## Default is to output to the screen, however if
## type = "png" option is passed in then the histogram 
## will be written to a file specifice by outputFile

makePlot <- function (outputFile, plotData, dates=c("1/1/1970"), type="default") {
   if (type == "png") { 
     png(filename=outputFile,width = 480, height = 480, units = "px")
   } 
   
   hist(plotData$Global_active_power,20, col="red", 
        main="Global Active Power", 
        xlab="Global Active Power (kilowatts)")            # add histogram - red fill, 20 breaks
   
   if (type == "png") { dev.off()}  
}

householdData <- readHouseholdPower(dataFile,dates)
makePlot (plotFile,householdData,dates,"png")