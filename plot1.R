dataFile="./household_power_consumption.txt"
plotFile="./figure/plot1.png"

readHouseholdPower <- function(powerFile, numRows=-1, dates=c("1/2/2007","2/2/2007")) {
   powerData <- read.csv (powerFile,
                          colClasses = c("character","character","numeric","numeric","numeric","numeric",
                          "numeric","numeric","numeric") , sep =';' , na.strings='?', nrows = numRows)
   powerData2<- subset(powerData, (Date %in% dates ))
   rm(powerData)
   powerData2
   }

makePlot <- function (outputFile,plotData,type="default") {
   if (type == "png") { 
     png(filename=outputFile,width = 480, height = 480, units = "px")
   } 
   
   hist(plotData$Global_active_power,20, col="red", 
        main="Global Active Power", 
        xlab="Global Active Power (kilowatts)")
   
   if (type == "png") { dev.off()}  
}

householdData <- readHouseholdPower(dataFile)
makePlot (plotFile,householdData,"png")