rm(list = ls()) 
rootDir <- "./data"
fileName <- paste0(rootDir,"/household_power_consumption.txt")

downloadAndExtractFile <- function(){
    
    if(!file.exists(rootDir)){
        dir.create(rootDir)
    }
    datasetFullPath <- paste0(rootDir,"/exdata-data-household_power_consumption.zip")
    if(!file.exists(datasetFullPath)){
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile = datasetFullPath)
    }

    
    if(!file.exists(fileName)){
        unzip(zipfile = datasetFullPath,exdir= rootDir)
    }
}

plotPng <- function(){
    downloadAndExtractFile()
    filteredData <- loadAndFormatFile()
    par(mfrow = c(2,2))
    plot(filteredData$Timestamp,filteredData$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
    
    plot(filteredData$Timestamp,filteredData$Voltage, type  =  "l", xlab  =  "datetime", ylab  =  "Voltage")    
    
    plot(filteredData$Timestamp, filteredData$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
    lines(filteredData$Timestamp,filteredData$Sub_metering_2,type = "l", col  =  "red")
    lines(filteredData$Timestamp,filteredData$Sub_metering_3,type = "l", col  =  "blue")
    legend("topright", c("Sub_metering_1  ", "Sub_metering_2  ", "Sub_metering_3  "), lty = c(1,1), lwd = 2.5, col = c("black", "red", "blue"), bty = "n")
    
    plot(filteredData$Timestamp,filteredData$Global_reactive_power, type  =  "l", xlab  =  "datetime", ylab  =  "Global_reactive_power")       
    
    dev.copy(png, file = "plot4.png", width = 480, height = 480)
    dev.off()
}


loadAndFormatFile <- function(){
    data <- read.table(file  =  fileName,sep  = ";")
    names(data) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3") 
    filteredData <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]    
    filteredData$Global_active_power <- as.numeric(as.character(filteredData$Global_active_power))
    filteredData$Global_reactive_power <- as.numeric(as.character(filteredData$Global_reactive_power))
    filteredData$Voltage <- as.numeric(as.character(filteredData$Voltage))
    filteredData <- transform(filteredData, Timestamp  =  strptime(paste(filteredData$Date, filteredData$Time, sep = " "), "%d/%m/%Y %H:%M:%S"))
    filteredData$Sub_metering_1 <- as.numeric(as.character(filteredData$Sub_metering_1))
    filteredData$Sub_metering_2 <- as.numeric(as.character(filteredData$Sub_metering_2))
    filteredData$Sub_metering_3 <- as.numeric(as.character(filteredData$Sub_metering_3))
    filteredData
}