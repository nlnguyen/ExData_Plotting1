plot4 <- function(dir="./data", filename="household_power_consumption.txt")
{
      library(data.table)
      options(warn=-1)
      
      # Read Date column to find the rows of interest (ROI) ie. dates from 2007-02-01 to 2007-02-02
      mfile <- paste(dir, filename, sep="/")
      dates <- fread(mfile, na.strings="?", select="Date")
      dates$Date <- as.Date(dates$Date, format="%d/%m/%Y")
      ind <- which(dates$Date>="2007-02-01" & dates$Date<="2007-02-02")
      rm(dates)
      
      
      # Extract the ROI from the data file
      data <- fread(mfile, header="auto", na.strings="?", skip=ind[1], nrows=length(ind))
      names(data) <- as.character(fread(mfile, header=FALSE, nrows=1, data.table=FALSE))
      rm(ind)
      
      
      # Convert Date and Time variables to Date/Time class
      Times <- as.POSIXct(strptime(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S"))
      data[, Date:=NULL]
      data[, Time:=NULL]
      data <- cbind(Times, data)
      
      
      # Plot 4
      png("./plot4.png", width=480, height=480)
      par(mfrow=c(2,2))
      with(data, plot(Times, Global_active_power, xlab="", ylab="Global Active Power", type="l"))
      with(data, plot(Times, Voltage, xlab="datetime", ylab="Voltage", type="l"))
      with(data, plot(Times, Sub_metering_1, xlab="", ylab="Energy sub metering", type="n"))
      with(data, lines(Times, Sub_metering_1, xlab=""))
      with(data, lines(Times, Sub_metering_2, xlab="", col="red"))
      with(data, lines(Times, Sub_metering_3, xlab="", col="blue"))
      legend("topright", legend=names(data)[6:8], bty="n", lty=1, col=c("black", "red", "blue"), 
             x.intersp=0.75, y.intersp=0.75, text.width = strwidth("Sub_metering_1"), xjust=1, yjust=1)
      with(data, plot(Times, Global_reactive_power, xlab="datetime", ylab="Global_reactive_power", 
                      type="l"))
      dev.off()
      options(warn=0)
}