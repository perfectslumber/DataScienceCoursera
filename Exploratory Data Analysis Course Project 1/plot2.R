power <- read.table("household_power_consumption.txt", sep = ";", header = TRUE,
                    na.strings = "?", stringsAsFactors = FALSE)
mydata <- subset(power, Date == "1/2/2007" | Date == "2/2/2007")
mydata$datetime <- paste(mydata$Date, mydata$Time)
mydata$datetime <- strptime(mydata$datetime, "%d/%m/%Y %H:%M:%S")
png("plot2.png", width = 480, height = 480)
plot(mydata$datetime, mydata$Global_active_power,
     type = "l",
     xlab = "",
     ylab = "Global Active Power (kilowatts)")
dev.off()
