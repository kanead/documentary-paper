##################################################################################
# Documentary analysis
# Code collects data from Wikipedia on species that have world species days
##################################################################################

library(dplyr)
library(pageviews)
library(data.table)
library(AnomalyDetection)

# load in the data which is a vector of species names that have a dedicated awareness day
data<-read.csv("data/species_days.csv", header = TRUE, sep = ",")
head(data)
length(data$species)

# remove the duplicated values 
data.new<-data[!duplicated(data), ]
length(data.new$species)
class(data.new)

# use the function from pageviews package to collect data for each species 
get_wiki <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                          , start = as.Date('2016-01-01'), end = as.Date("2016-12-31")
                                          , user_type = "user", platform = c("desktop", "mobile-web"))
}

# then write a function to get the info for the baseline 
get_wiki_baseline <- function(x){article_pageviews(project = "en.wikipedia",                           article = x , start = as.Date('2016-01-01'), end = as.Date                        ("2017-12-31"), user_type = "user", platform = c("desktop",                       "mobile-web"))
}

# loop over each species to collect the data for 2016
output<- data.new$species %>%  get_wiki
length(output)
head(output)
tail(output)

# then loop over each species to collect the data for a baseline 
outputBaseline <- data.new$species %>%  get_wiki_baseline
length(outputBaseline)
head(outputBaseline)
tail(outputBaseline)

# combine output 
dataTableOutput<-setDT(output)[, .(sumy=sum(views)), by = .(article,date)]
combinedOutput<-data.frame(dataTableOutput)
names(combinedOutput)[names(combinedOutput) == 'sumy'] <- 'views'
combinedOutput$date<-as.POSIXct(strptime(combinedOutput$date,"%Y-%m-%d"))
combinedOutput<-droplevels(combinedOutput)
head(combinedOutput)

# do the same combine on the baseline data 
dataTableOutputBaseline<-setDT(outputBaseline)[, .(sumy=sum(views)), by = .(article,date)]
combinedOutputBaseline<-data.frame(dataTableOutputBaseline)
names(combinedOutputBaseline)[names(combinedOutputBaseline) == 'sumy'] <- 'views'
combinedOutputBaseline$date<-as.POSIXct(strptime(combinedOutputBaseline$date,"%Y-%m-%d"))
combinedOutputBaseline<-droplevels(combinedOutputBaseline)
head(combinedOutputBaseline)

# get the median hits over the course of the baseline time period by species (article)
tapply(dataTableOutputBaseline$sumy, dataTableOutputBaseline$article, FUN = median)

# we want to compare the data with a baseline, because the awareness days vary with year we want to apply a date buffer around the awareness day per species 
# and calculate its media value
names(data)[names(data) == 'species'] <- 'article'
dataTableOutputBaseline<-merge(dataTableOutputBaseline, data[,c(1,5,6)], by="article")
head(dataTableOutputBaseline)
dataTableOutputBaseline$start<-as.POSIXct(strptime(dataTableOutputBaseline$start,"%d/%m/%Y"))
dataTableOutputBaseline$end<-as.POSIXct(strptime(dataTableOutputBaseline$end,"%d/%m/%Y"))
head(dataTableOutputBaseline)

length(dataTableOutputBaseline$article)
splitDateDataTableOutputBaseline <- dataTableOutputBaseline %>% group_by(article) %>%
  filter(date >= start & date <= end)  %>% dplyr::summarise(medianHits<-median(sumy))
length(splitDateDataTableOutputBaseline$article)
# these are the baseline values we want to compare with the values on the awareness days 
head(splitDateDataTableOutputBaseline)

##################################################################################
# get the data for the day and day after the species had its dedicated awareness 
# day in 2016, then calculate the max value of hits it had on the day or day after
# the number of anomalies are also calculated but not used in the analysis
##################################################################################
# Dolphin 14/04/16
speciesName <- c("Dolphin")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)
anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-04-14"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-04-15"), format = "%Y-%m-%d"),1,0)
sum(anomalies)
res$anoms
print(c("Dolphin",max(c(
newdata1$views[newdata1$date=="2016-04-14"],
newdata1$views[newdata1$date=="2016-04-15"]
))))

# Giant panda 16/03/16
speciesName <- c("Giant_panda")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-03-16"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-03-17"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Giant panda",max(c(
  newdata1$views[newdata1$date=="2016-03-16"],
  newdata1$views[newdata1$date=="2016-03-17"]
))))

# Giraffe 21/06/16
speciesName <- c("Giraffe")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-06-21"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-06-22"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Giraffe",max(c(
  newdata1$views[newdata1$date=="2016-06-21"],
  newdata1$views[newdata1$date=="2016-03-22"]
))))

# Honey_bee 20/08/16
speciesName <- c("Honey_bee")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-08-20"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-08-21"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Honey_bee",max(c(
  newdata1$views[newdata1$date=="2016-08-20"],
  newdata1$views[newdata1$date=="2016-08-21"]
))))

# Lion 10/08/16
speciesName <- c("Lion")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-08-10"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-08-11"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Lion",max(c(
  newdata1$views[newdata1$date=="2016-08-10"],
  newdata1$views[newdata1$date=="2016-08-11"]
))))

# Orangutan 19/08/16
speciesName <- c("Orangutan")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-08-19"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-08-20"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Orangutan",max(c(
  newdata1$views[newdata1$date=="2016-08-19"],
  newdata1$views[newdata1$date=="2016-08-20"]
))))

# Pangolin 20/02/16
speciesName <- c("Pangolin")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-02-20"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-02-21"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Pangolin",max(c(
  newdata1$views[newdata1$date=="2016-02-20"],
  newdata1$views[newdata1$date=="2016-02-21"]
))))

# Polar_bear 27/02/16
speciesName <- c("Polar_bear")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-02-27"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-02-28"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Polar_bear",max(c(
  newdata1$views[newdata1$date=="2016-02-27"],
  newdata1$views[newdata1$date=="2016-02-28"]
))))

# Rhino 22/09/16
speciesName <- c("Rhinoceros")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-09-22"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-09-23"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Rhinoceros",max(c(
  newdata1$views[newdata1$date=="2016-09-22"],
  newdata1$views[newdata1$date=="2016-09-23"]
))))

# Sea_turtle 23/05/16
speciesName <- c("Sea_turtle")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-05-23"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-05-24"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Sea_turtle",max(c(
  newdata1$views[newdata1$date=="2016-05-23"],
  newdata1$views[newdata1$date=="2016-05-23"]
))))

# Sloth 20/10/16
speciesName <- c("Sloth")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-10-20"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-10-21"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Sloth",max(c(
  newdata1$views[newdata1$date=="2016-10-20"],
  newdata1$views[newdata1$date=="2016-10-21"]
))))

# Snake 16/07/16
speciesName <- c("Snake")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-07-16"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-07-17"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Snake",max(c(
  newdata1$views[newdata1$date=="2016-07-16"],
  newdata1$views[newdata1$date=="2016-07-17"]
))))


# Tiger 29/07/16
speciesName <- c("Tiger")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-07-29"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-07-30"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Tiger",max(c(
  newdata1$views[newdata1$date=="2016-07-29"],
  newdata1$views[newdata1$date=="2016-07-30"]
))))

# Vulture 03/09/16
speciesName <- c("Vulture")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-09-03"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-09-04"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Vulture",max(c(
  newdata1$views[newdata1$date=="2016-09-03"],
  newdata1$views[newdata1$date=="2016-09-04"]
))))

# Whale 13/02/16
speciesName <- c("Whale")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-02-13"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-02-14"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Whale",max(c(
  newdata1$views[newdata1$date=="2016-02-13"],
  newdata1$views[newdata1$date=="2016-02-14"]
))))

# Whale shark 30/08/16
speciesName <- c("Whale_shark")
# keep only the species we're interested in  
newdata1<-combinedOutput[combinedOutput$article %in% speciesName,] 
newdata1<-droplevels(newdata1)

res = AnomalyDetectionTs((data.frame(newdata1[2:3])), max_anoms=0.01, direction='both', plot=TRUE)

anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                  == strptime(as.Date("2016-08-30"), format = "%Y-%m-%d") | 
                    strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                  == strptime(as.Date("2016-08-31"), format = "%Y-%m-%d"),1,0)

sum(anomalies)
res$anoms
print(c("Whale shark",max(c(
  newdata1$views[newdata1$date=="2016-08-30"],
  newdata1$views[newdata1$date=="2016-08-31"]
))))

