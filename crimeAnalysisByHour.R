library(lubridate)
library(dplyr)
library(ggplot2)

#' Get the crime data from dallasopendata.com
crime.data.file <- "crime.csv"
if(!file.exists(crime.data.file)){
  download.file("http://www.dallasopendata.com/api/views/ftja-9jxd/rows.csv?accessType=DOWNLOAD",
                destfile=crime.data.file)
}
#' Disclaimer: The data supplied by Dallas Police Department is sampled
#' and should not be used for statistical purposes, but we should be able to 
#' extract enough informaation to get a general idea of where crime is concentrated
#' 
#' The Dallas Police Department implemented a new Records Management System
#' on June 1, 2014. To get crime data for 2014, two datasets are needed.
rms.file <- "rms.csv"
if(!file.exists(rms.file)){
  download.file("http://www.dallasopendata.com/api/views/tbnj-w5hb/rows.csv?accessType=DOWNLOAD",
                destfile=rms.file)
}


#' Read in the crime data into a data.frame
crime.data.part1 <- read.csv(crime.data.file,
                             as.is = TRUE)
crime.data.part2 <- read.csv(rms.file,
                             as.is = TRUE)

#' Get the columns that are needed for this analysis, 
#' change names if necessary, remove records from
#' the rms data if necessary, and then bind the two sets by rows. 
crime.data <- dplyr::select(crime.data.part1, offensedate, offensetimedispatched)
temp <- dplyr::select(crime.data.part2, Date1, Time1)

#' Change the names of the columns to match the first set of data.
colnames(temp) <- c("offensedate", "offensetimedispatched")

#' Remove records before June 1, 2014 and use only 2014 data.
temp <- mutate(temp, tempdate = as.Date(temp$offensedate,
                                        format="%m/%d/%Y"))

temp <- temp[as.Date(temp$tempdate) >= as.Date("2014-06-01") 
             & year(temp$tempdate) == 2014 
             & !is.na(temp$tempdate),]

#' Remove the tempdate column
tempdateindex <- grep("^tempdate$", colnames(temp))
temp <- temp[,-tempdateindex]
head(temp)

#' Bind the two data sets
crime.data <- rbind(crime.data, temp)

#' Check our date range of the data
crime.data$offensedate <- as.Date(crime.data$offensedate,
                                  format="%m/%d/%Y")

paste("Min is ", min(crime.data$offensedate), sep=" ")
paste("Max is ", max(crime.data$offensedate), sep=" ")

#' Check if the data is what is expected
crime.data <- mutate(crime.data, offenseyear = year(crime.data$offensedate))

crime <- group_by(crime.data, offenseyear)
summarize(crime, countsperyear = length(offenseyear))

#' Dates previous to 2014 are not complete, so we will get data for year 2014
crime.data <- crime.data[crime.data$offenseyear == 2014,]

#' Assuming "offensetimedispatched" is the time that the offense
#' happened, we will use that column to determine when the 
#' crime happened.
crime.data$offensetimedispatched <- strptime(crime.data$offensetimedispatched,
                                             format="%H:%M")

#' Add an hour column
merged.data <- mutate(crime.data, offensehour = hour(crime.data$offensetimedispatched))
merged.data$offensetimedispatched <- as.character(merged.data$offensetimedispatched)
merged.data$offensehour <- as.numeric(merged.data$offensehour)
head(merged.data)

#' Group by date and then by hour
crime.data.hour <- group_by(merged.data, offensedate, offensehour)
head(crime.data.hour)

#' Get the crimes per hour
complete.sample <- summarise(crime.data.hour,  
          CrimesPerHour = length(offensehour) )
complete.sample

#' Simple normalization of the crimes per hour throughout time
min.crimes <- min(complete.sample$CrimesPerHour)
max.crimes <- max(complete.sample$CrimesPerHour)
complete.sample$NormCrimesPerHour <- as.numeric(scale(complete.sample$CrimesPerHour, 
                                                      center = min.crimes, 
                                                      scale = max.crimes - min.crimes))
temp <- group_by(complete.sample, offensehour)
temp <- summarise(temp, value = mean(NormCrimesPerHour))
temp
ggplot(temp, aes(x = offensehour, y = value)) + geom_bar(stat = "identity")

#' It looks like the 4th hour is a bit extreme, so let's see if there 
#' is an outlyer. I prefer to use box plots to determine if there is an
#' outlier.
fourth.hour <- complete.sample[complete.sample$offensehour == 4,]
qplot(fourth.hour$offensehour, fourth.hour$CrimesPerHour, data=fourth.hour, geom="boxplot")

#' There are quite a few outliers, so let us take a look at some
fourth.hour[fourth.hour$CrimesPerHour > 50,]

#' It looks like these outliers are grouped together, posibly a sting operation
#' For example, May 19 - 22, there were over 300 offenses in the 4th hour.
#' We will leave that data in for this analysis.

#' The data appears to follow a normal distribution if
#' 0 through 5 were to follow 23, so let's reorder
foo = rep(0, nrow(temp))
foo[with(temp, offensehour == 6)] = 1
foo[with(temp, offensehour == 7)] = 2
foo[with(temp, offensehour == 8)] = 3
foo[with(temp, offensehour == 9)] = 4
foo[with(temp, offensehour == 10)] = 5
foo[with(temp, offensehour == 11)] = 6
foo[with(temp, offensehour == 12)] = 7
foo[with(temp, offensehour == 13)] = 8
foo[with(temp, offensehour == 14)] = 9
foo[with(temp, offensehour == 15)] = 10
foo[with(temp, offensehour == 16)] = 11
foo[with(temp, offensehour == 17)] = 12
foo[with(temp, offensehour == 18)] = 13
foo[with(temp, offensehour == 19)] = 14
foo[with(temp, offensehour == 20)] = 15
foo[with(temp, offensehour == 21)] = 16
foo[with(temp, offensehour == 22)] = 17
foo[with(temp, offensehour == 23)] = 18
foo[with(temp, offensehour == 0)] = 19
foo[with(temp, offensehour == 1)] = 20
foo[with(temp, offensehour == 2)] = 21
foo[with(temp, offensehour == 3)] = 22
foo[with(temp, offensehour == 4)] = 23
foo[with(temp, offensehour == 5)] = 24

temp$offensehour = with(temp, reorder(offensehour, foo))
rm(foo)

ggplot(temp, aes(x = offensehour, y = value)) + geom_bar(stat = "identity")

