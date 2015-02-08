plot1 <- function(dir="./data", filename="household_power_consumption.txt")
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
      
      
      # Plot 1
      png("./plot1.png", width=480, height=480)
      hist(data$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", 
           main="Global Active Power")
      dev.off()
      options(warn=0)
}