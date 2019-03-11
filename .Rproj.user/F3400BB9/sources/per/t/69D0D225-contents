################################################################################### Documentary analysis
# Code extracts anomalies for the wikipedia articles of species featured in Planet # Earth 1 on the dates of Planet Earth 2
##################################################################################
# https://github.com/twitter/AnomalyDetection/blob/master/R/detect_anoms.R
# https://www.r-bloggers.com/anomaly-detection-in-r/
# involves taking the max absolute difference from the detrended sample mean in 

# ddply(newdata, "article", function(x) {length(x$article)})

# Air dates
# episode 1 - 6 November 2016   2016-11-06
#                               2016-11-07

# episode 2 - 13 November 2016	2016-11-13
#                               2016-11-14

# episode 3 - 20 November 2016	2016-11-20
#                               2016-11-21

# episode 4 - 27 November 2016	2016-11-27
#                               2016-11-28

# episode 5 - 4 December 2016 	2016-12-04
#                               2016-12-05

# episode 6 - 11 December 2016	2016-12-11
#                               2016-12-12
##################################################################################
library(data.table)
library(plyr)
library(AnomalyDetection)

# read in the data for the species names 
output<-read.csv("data/output_PE1.csv",header = T,sep = ",")
# set the date class, sometimes we need different formats
# "%d-%m-%Y" "%d/%m/%Y" "%Y-%m-%d"
head(output)
output$date<-as.POSIXct(strptime(output$date,"%Y-%m-%d")) 
head(output)

# select either mobile or desktop or combined access
# we use the combined data

# Mobile
# mobileOutput <- output[output$access=="mobile-web" , ]
# mobileOutput<-droplevels(mobileOutput)
# head(mobileOutput)
# newdata <- mobileOutput[c(3,7:8)]
# head(newdata)
 
# Desktop
# deskOutput <- output[output$access=="desktop" , ]
# deskOutput<-droplevels(deskOutput)
# head(deskOutput)
# newdata <- deskOutput[c(3,7:8)]

# Combined
# can group the mobile and desktop data 
dataTableOutput<-setDT(output)[, .(sumy=sum(views)), by = .(article,date)]
newdata<-data.frame(dataTableOutput)
names(newdata)[names(newdata) == 'sumy'] <- 'views'
newdata<-droplevels(newdata)

##################################################################################
# We test to see how many of the Planet Earth 1 featured species Wikipedia
# articles had anomalies on the same day or day after of Planet Earth 2 air dates
##################################################################################
PEoneAnoms <-ddply(newdata, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.01, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-06"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-07"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-13"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-14"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-20"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-21"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-27"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-28"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-04"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-05"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-11"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-12"), format = "%Y-%m-%d") ,1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})

PEoneAnoms

# how many of them had anomalies on the airdates?
sum(PEoneAnoms$V1>0)
sum(PEoneAnoms$V1>0) / length(PEoneAnoms$article) * 100
# extract the data
write.csv(PEoneAnoms,file="data/anomalies_PE1_combined.csv", row.names=FALSE)


