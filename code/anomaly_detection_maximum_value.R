################################################################################### Documentary analysis
# Code to extract absolute maximum value for Wikipedia hits during air dates 
# of Planet Earth 2
##################################################################################

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
output<-read.csv("data/output.csv",header = T,sep = ",")
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
episode1 <- c("Pygmy_three-toed_sloth","Komodo_dragon","Lemur","Indri","Ring-tailed_lemur","Bamboo_lemur",
              "Sifaka","Marine_iguana","Crab","Lizard", "Colubridae" ,"Snares_penguin","Shearwater","Buller's_albatross",
              "Fairy_tern","Seychelles_fody","Noddy_(tern)","Yellow_crazy_ant","Chinstrap_penguin",
              "Skua", "Christmas_Island_red_crab")

# keep only the species featured in this episode 
newdata1<-newdata[newdata$article %in% episode1,] 
newdata1<-droplevels(newdata1)

dateSub<-subset(newdata1, date>="2016-11-06" & date <="2016-11-07")
dateSub<-droplevels(dateSub)

# episode 1 function
airedAnomDataDays1<-ddply(dateSub, "article", function(x) {
  maxDay <- max(x$views)
})

airedAnomDataDays1

##################################################################################
episode2 <- c("Snow_Leopard","Nubian_ibex","Red_fox","Golden_eagle","Corvidae",
              "Grizzly_bear","Marmot","Bobcat","Mouse","Coyote", "Goldeneye_(duck)","Squirrel","Lagidium",
              "Flamingo")

# keep only the species featured in this episode 
newdata2<-newdata[newdata$article %in% episode2,] 
newdata2<-droplevels(newdata2)

dateSub<-subset(newdata2, date>="2016-11-13" & date <="2016-11-14")
dateSub<-droplevels(dateSub)

# episode 2 function
airedAnomDataDays2<-ddply(dateSub, "article", function(x) {
  maxDay <- max(x$views)
})

airedAnomDataDays2

##################################################################################
episode3 <- c("Indri","Spider_monkey","Draco_(genus)","Hummingbird", "Sword-billed_hummingbird","River_dolphin",
              "Capybara","Giant_otter","Caiman","Jaguar","Uroplatus","Click_beetle","Railroad_worm","Glass_frog",
              "Millipede","Red_bird-of-paradise","Wilson's_bird-of-paradise", "Rat", "Spider", "Fire_ant", "Wasp")

# keep only the species featured in this episode 
newdata3<-newdata[newdata$article %in% episode3,] 
newdata3<-droplevels(newdata3)

dateSub<-subset(newdata3, date>="2016-11-20" & date <="2016-11-21")
dateSub<-droplevels(dateSub)

# episode 3 function
airedAnomDataDays3<-ddply(dateSub, "article", function(x) {
  maxDay <- max(x$views)
})

airedAnomDataDays3

##################################################################################
episode4 <- c("Lion","Oryx","Giraffe","Harris's_hawk","Ground_squirrel","Butcherbird","Locust","Zebra",
              "Elephant","Sandgrouse","Pale_chanting_goshawk","Golden_mole","Desert_long-eared_bat",
              "Deathstalker", "Darkling_beetle", "Pachydactylus_rangei" ,"Namaqua_chameleon")

# keep only the species featured in this episode 
newdata4<-newdata[newdata$article %in% episode4,] 
newdata4<-droplevels(newdata4)

dateSub<-subset(newdata4, date>="2016-11-27" & date <="2016-11-28")
dateSub<-droplevels(dateSub)

# episode 4 function
airedAnomDataDays4<-ddply(dateSub, "article", function(x) {
  maxDay <- max(x$views)
})

airedAnomDataDays4

##################################################################################
episode5 <- c("Saiga_antelope","Lion","African_buffalo","Micromys","Barn_owl","Southern_carmine_bee-eater",
              "Kori_bustard","Ostrich","Elephant","Serval","Southern_African_vlei_rat",
              "Wildebeest","Jackson's_widowbird","Atta_(genus)", "Amitermes_meridionalis", "Termite","Giant_anteater","Bison","Fox",
              "Vole", "Caribou","Arctic_wolf","Rhinoceros","Wild_water_buffalo","Sloth_bear", "Tiger")

# keep only the species featured in this episode 
newdata5<-newdata[newdata$article %in% episode5,] 
newdata5<-droplevels(newdata5)

dateSub<-subset(newdata5, date>="2016-12-04" & date <="2016-12-05")
dateSub<-droplevels(dateSub)

# episode 5 function
airedAnomDataDays5<-ddply(dateSub, "article", function(x) {
  maxDay <- max(x$views)
})

airedAnomDataDays5

##################################################################################
episode6 <- c("Colobinae","Peregrine_falcon","Leopard","Starling","Great_bowerbird","Raccoon",
              "Rhesus_macaque","Spotted_hyena","Pigeon","Wels_catfish","Hawksbill_sea_turtle",
              "Crab", "Smooth-coated_otter")

# keep only the species featured in this episode 
newdata6<-newdata[newdata$article %in% episode6,] 
newdata6<-droplevels(newdata6)

dateSub<-subset(newdata6, date>="2016-12-11" & date <="2016-12-12")
dateSub<-droplevels(dateSub)

# episode 6 function
airedAnomDataDays6<-ddply(dateSub, "article", function(x) {
  maxDay <- max(x$views)
})

airedAnomDataDays6

# combine and sort alphabetically 
allData<-rbind(airedAnomDataDays1, airedAnomDataDays2, airedAnomDataDays3,
               airedAnomDataDays4, airedAnomDataDays5, airedAnomDataDays6)
allData$article <- as.character(allData$article)
allData <- allData[order(allData$article),]
allData

# export the data
write.csv(allData,file="data/max_wikipedia_hits_PE2.csv", row.names=FALSE)


