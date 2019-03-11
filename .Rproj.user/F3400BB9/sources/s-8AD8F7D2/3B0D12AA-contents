###################################################################################
# Documentary analysis
# Code for analysis of anomalies of time series of charity donations  
###################################################################################

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
###################################################################################
library(data.table)
library(plyr)
library(AnomalyDetection)

# read in the data for the charity donations 
bornFree<-read.csv("data/bornfree_donations.csv",header = T,sep = ",")
arkive<-read.csv("data/arkive_donations.csv",header = T,sep = ",")

# change to date format
bornFree$date<-as.POSIXct(strptime(bornFree$date,"%d/%m/%Y"))
arkive$date<-as.POSIXct(strptime(arkive$date,"%d/%m/%Y"))

# test for anomalies 
# BornFree first
resBornFree = AnomalyDetectionTs(data.frame(bornFree[1:2]), max_anoms=0.01, direction='both', plot=TRUE)
resBornFree$plot
resBornFree$anoms
resBornFree$anoms$timestamp

# now Arkive 
resarkive = AnomalyDetectionTs(data.frame(arkive[1:2]), max_anoms=0.01, direction='both', plot=TRUE)
resarkive$plot
resarkive$anoms
resarkive$anoms$timestamp
