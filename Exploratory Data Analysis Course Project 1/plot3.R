power <- read.table("household_power_consumption.txt", sep = ";", header = TRUE,
                    na.strings = "?", stringsAsFactors = FALSE)
mydata <- subset(power, Date == "1/2/2007" | Date == "2/2/2007")
mydata$datetime <- paste(mydata$Date, mydata$Time)
mydata$datetime <- strptime(mydata$datetime, "%d/%m/%Y %H:%M:%S")
png("plot3.png", width = 480, height = 480)
plot(mydata$datetime, mydata$Sub_metering_1,
     type = "l",
     xlab = "",
     ylab = "Energy sub metering")
points(mydata$datetime, mydata$Sub_metering_2,
       type = "l",
       col = "red")
points(mydata$datetime, mydata$Sub_metering_3,
       type = "l",
       col = "blue")
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "blue", "red"), lty = 1, cex = 0.75)
dev.off()
