require (lubridate)
require (dplyr)

powerData <- read.csv ("./household_power_consumption.txt",
   colClasses = c("character","character","numeric","numeric","numeric","numeric",
                   "numeric","numeric","numeric") , sep =';' , na.strings='?')
# powerData <- mutate (powerData, DateTime = dmy_hms(paste(powerData$Date,powerData$Time)) )
powerData2<- subset(powerData, (Date == "01/02/2007" | Date == "01/02/2007") )