## This program absorbs the dataset and produce a graph.
## The code does the following:
## 	asks for the location of the dataset, 
## 	reads the dataset,
## 	subsets the data,
## 	modify the time column, 
## 	plots the histogram, 
## 	and save the output to a PNG graphic file in the working directory.

## ask for location of the source file
print("Please enter the position of the data file under the working directory.")
print("If your dataset is placed in the folder named Data under the working directory")
print(" and named as household_power_consumption.txt,")
print(" the input should be ./Data/household_power_consumption.txt.")
cat("Position of the dataset:")
dirSource <- readLines(n=1)

## set data types
library(methods)
setClass("myDate")
setClass("myTime")
setAs("character", "myDate", function(from) as.Date(from, format="%d/%m/%Y") )
setAs("character", "myTime", function(from) as.POSIXct(from, format="%H:%M:%S"))

## read data with data type specifications
rawDataset <- read.table(dirSource,
	header=TRUE,
	na.strings="?",
	colClasses=c("myDate","myTime",rep("numeric",7)),
	sep=";")

## subset the data
ds <- rawDataset[
	rawDataset$Date >= as.Date("2007-02-01") &
	rawDataset$Date <= as.Date("2007-02-02"),]

## modify time column
ds$Time <-ISOdatetime(
	format(ds$Date, "%Y")
	,format(ds$Date, "%m")
	,format(ds$Date, "%d") 
	,format(ds$Time, "%H") 
	,format(ds$Time, "%M") 
	,format(ds$Time, "%S")
	,tz = "")

## Q3 plot the graph
with(ds
	,{
		plot(Sub_metering_1~Time
			,type="l"
			,ylab="Global Active Power (kilowatts)"
			,xlab="")
		lines(Sub_metering_2~Time,col='Red')
		lines(Sub_metering_3~Time,col='Blue')
	}
)
legend("topright"
	,col=c("black", "red", "blue"),
	,lty=1
	,lwd=2
	,legend=c("Sub_metering_1"
		,"Sub_metering_2"
		,"Sub_metering_3"))

## Save the output to PNG
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()
