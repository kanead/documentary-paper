##################################################################################
# Documentary analysis
# Code extracts anomalies for the wikipedia articles of species featured in Planet # Earth 2
##################################################################################
# https://github.com/twitter/AnomalyDetection/blob/master/R/detect_anoms.R
# https://www.r-bloggers.com/anomaly-detection-in-r/
# involves taking the max absolute difference from the detrended sample mean in 
# terms of standard deviations, remove it and repeat until you have your 
# collection of x outliers.

# deleted some species because of data length: cheetah, darkling beetles, red crab 
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
# library(dplyr) # plyr and dplyr don't play well together 
# detach("package:dplyr", unload=TRUE)

# read in the data for the species names 
output<-read.csv("data/output.csv",header = T,sep = ",")
# set the date class, sometimes we need different formats
# "%d-%m-%Y" "%d/%m/%Y" "%Y-%m-%d"
head(output)
output$date<-as.POSIXct(strptime(output$date,"%Y-%m-%d")) 
head(output)

# select mobile or desktop or combined access
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
head(newdata)
##################################################################################
# collect the species mentioned in episode 1 
episode1 <- c("Pygmy_three-toed_sloth","Komodo_dragon","Lemur","Indri","Ring-tailed_lemur","Bamboo_lemur",
              "Sifaka","Marine_iguana","Crab","Lizard", "Colubridae" ,"Snares_penguin","Shearwater","Buller's_albatross",
              "Fairy_tern","Seychelles_fody","Noddy_(tern)","Yellow_crazy_ant","Chinstrap_penguin",
              "Skua", "Christmas_Island_red_crab")

# keep only the species featured in this episode 
newdata1<-newdata[newdata$article %in% episode1,] 
newdata1<-droplevels(newdata1)

# episode 1 function
airedAnomDataDays1<-ddply(newdata1, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-06"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-07"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
##################################################################################
# collect the species mentioned in episode 2
episode2 <- c("Snow_Leopard","Nubian_ibex","Red_fox","Golden_eagle","Corvidae",
              "Grizzly_bear","Marmot","Bobcat","Mouse","Coyote", "Goldeneye_(duck)","Squirrel","Lagidium",
              "Flamingo")

# keep only the species featured in this episode 
newdata2<-newdata[newdata$article %in% episode2,] 
newdata2<-droplevels(newdata2)

# episode 2 function
airedAnomDataDays2<-ddply(newdata2, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-13"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-14"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
##################################################################################
# collect the species mentioned in episode 3
episode3 <- c("Indri","Spider_monkey","Draco_(genus)","Hummingbird", "Sword-billed_hummingbird","River_dolphin",
              "Capybara","Giant_otter","Caiman","Jaguar","Uroplatus","Click_beetle","Railroad_worm","Glass_frog",
              "Millipede","Red_bird-of-paradise","Wilson's_bird-of-paradise", "Rat", "Spider", "Fire_ant", "Wasp")

# keep only the species featured in this episode 
newdata3<-newdata[newdata$article %in% episode3,] 
newdata3<-droplevels(newdata3)

# episode 3 function
airedAnomDataDays3<-ddply(newdata3, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-20"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-21"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
##################################################################################
# collect the species mentioned in episode 4
episode4 <- c("Lion","Oryx","Giraffe","Harris's_hawk","Ground_squirrel","Butcherbird","Locust","Zebra",
              "Elephant","Sandgrouse","Pale_chanting_goshawk","Golden_mole","Desert_long-eared_bat",
              "Deathstalker", "Darkling_beetle", "Pachydactylus_rangei" ,"Namaqua_chameleon")

# keep only the species featured in this episode 
newdata4<-newdata[newdata$article %in% episode4,] 
newdata4<-droplevels(newdata4)

# episode 4 function
airedAnomDataDays4<-ddply(newdata4, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-27"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-28"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
##################################################################################
# collect the species mentioned in episode 5
episode5 <- c("Saiga_antelope","Lion","African_buffalo","Micromys","Barn_owl","Southern_carmine_bee-eater",
              "Kori_bustard","Ostrich","Elephant","Serval","Southern_African_vlei_rat",
              "Wildebeest","Jackson's_widowbird","Atta_(genus)", "Amitermes_meridionalis", "Termite","Giant_anteater","Bison","Fox",
              "Vole", "Caribou","Arctic_wolf","Rhinoceros","Wild_water_buffalo","Sloth_bear", "Tiger")

# keep only the species featured in this episode 
newdata5<-newdata[newdata$article %in% episode5,] 
newdata5<-droplevels(newdata5)

# episode 5 function
airedAnomDataDays5<-ddply(newdata5, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-12-04"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-05"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
##################################################################################
# collect the species mentioned in episode 6
episode6 <- c("Colobinae","Peregrine_falcon","Leopard","Starling","Great_bowerbird","Raccoon",
              "Rhesus_macaque","Spotted_hyena","Pigeon","Wels_catfish","Hawksbill_sea_turtle",
              "Crab", "Smooth-coated_otter")

# keep only the species featured in this episode 
newdata6<-newdata[newdata$article %in% episode6,] 
newdata6<-droplevels(newdata6)

# episode 6 function
airedAnomDataDays6<-ddply(newdata6, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-12-11"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-12"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
##################################################################################
# combine the data for each of the episodes 
airedAnomDataDays1
airedAnomDataDays2
airedAnomDataDays3
airedAnomDataDays4
airedAnomDataDays5
airedAnomDataDays6

# identify names that appear in more than one episode 
# rename them so we can distinguish 
airedAnomDataDays3$article <- gsub("Indri", "Indri_3", airedAnomDataDays3$article)
airedAnomDataDays5$article <- gsub("Elephant", "Elephant_5", airedAnomDataDays5$article)
airedAnomDataDays5$article <- gsub("Lion", "Lion_5", airedAnomDataDays5$article)
airedAnomDataDays6$article <- gsub("Crab", "Crab_6", airedAnomDataDays6$article)

allDataCombo<-rbind.fill(airedAnomDataDays1,airedAnomDataDays2,airedAnomDataDays3,airedAnomDataDays4,airedAnomDataDays5,airedAnomDataDays6)

# order alphabetically 
allDataCombo <- allDataCombo[order(allDataCombo$article),]
# how many of them had anomalies on the airdates?
sum(allDataCombo$V1>0)
sum(allDataCombo$V1>0) / length(allDataCombo$V1) * 100

# export the data
write.csv(allDataCombo,file="data/anomalies_PE2_combined.csv", row.names=FALSE)

##################################################################################
# summary stats on the article hits 
##################################################################################
library(dplyr)

# turn off exponential notation 
options(scipen = 999)

# mean 
summaryDataMean<-newdata %>%
  group_by(article) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

summaryDataMean<-data.frame(summaryDataMean)
summaryDataMean$views<-round(summaryDataMean$views,0)
names(summaryDataMean)[names(summaryDataMean) == 'views'] <- 'meanViews'

# median 
summaryDataMedian<-newdata %>%
  group_by(article) %>%
  summarise_all(funs(median(., na.rm=TRUE)))

summaryDataMedian<-data.frame(summaryDataMedian)
summaryDataMedian$views<-round(summaryDataMedian$views,0)
names(summaryDataMedian)[names(summaryDataMedian) == 'views'] <- 'medianViews'

# maximum 
summaryDataMax<-newdata %>%
  group_by(article) %>%
  summarise_all(funs(max(., na.rm=TRUE)))

summaryDataMax<-data.frame(summaryDataMax)
names(summaryDataMax)[names(summaryDataMax) == 'views'] <- 'maxViews'

# combine it all into a single data frame 
summaryStatsViews <- data.frame(summaryDataMean$article,summaryDataMean$meanViews,summaryDataMedian$medianViews,summaryDataMax$maxViews)
names(summaryStatsViews) <- c("article", "meanViews","medianViews","maxViews")
head(summaryStatsViews)

# export these summary statistics
write.csv(summaryStatsViews,file="data/summary_stats_views_wikipedia_PE2.csv", row.names=FALSE)