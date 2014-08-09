## This program absorbs the dataset and produce a histogram.
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

## Q1 plot the histogram
hist(ds$Global_active_power, 
	breaks=seq(0, ceiling(2*max(ds$Global_active_power))/2, by=0.5)
	,col="red",
	,main="Global Actice Power",xlab="Global Active Power (kilowatts)")

## Save the output to PNG
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()
