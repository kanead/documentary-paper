################################################################################## Documentary analysis
# Code to calculate causal impact of Planet Earth 2 on the long terms trends of
# the Wikipedia articles of species featured on the show that also had anomalies
#################################################################################
# http://google.github.io/CausalImpact/CausalImpact.html

# libraries 
library(dplyr)
library(CausalImpact)
library(pageviews)
library(data.table)
library(textclean)
library(tidyr)

# load in data
# Use the data for Planet Earth 1 control time series
mydata<-read.csv("data/mentioned_names_both.csv",header = T,sep = ",")
levels(mydata$name)
head(mydata)
# drop the duplicates 
data = mydata %>% distinct(name)

article <- data$name
article <- droplevels(article)

# collect the time series of Wikiepia hits 
wikiFunc <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                        , start = as.Date('2016-05-01'), 
                                        end = as.Date("2017-10-31")
                                        , user_type = "user", platform = c("desktop", "mobile-web"))
}

# collect the wiki hits  
wikiHits <-article %>% wikiFunc

# can group the mobile and desktop data 
dataTableOutput<-setDT(wikiHits)[, .(sumy=sum(views)), by = .(article,date)]
# 66 + 42 for sloth 2016-05-01
newdata<-data.frame(dataTableOutput)
names(newdata)[names(newdata) == 'sumy'] <- 'views'
#newdata$date<-as.POSIXct(strptime(newdata$date,"%Y-%m-%d"))
newdata<-droplevels(newdata)
head(newdata)

# rename the combined dataframe
wikiHits <- newdata

# clean up the data  
# keep the columns we want
wikiHits <- wikiHits[,c("article","date","views")]
wikiHits$article <- as.factor(wikiHits$article)
# match up the species names to the series they featured in 
colnames(mydata)[which(names(mydata) == "name")] <- "article"
wikiHits <- left_join(wikiHits, mydata, by=("article"))
head(wikiHits)
tail(wikiHits)

# remove confusing character strings
wikiHits$article<-strip(wikiHits$article, char.keep = "~~", digit.remove = TRUE,
      apostrophe.remove = TRUE, lower.case = TRUE)
wikiHits$article <- as.factor(wikiHits$article) 
levels(wikiHits$article)
head(wikiHits)

# drop species that don't have the proper time frame of 549 entries
tapply(wikiHits$views,wikiHits$article,length)
wikiHits<-wikiHits[! wikiHits$article %in% c("crab", "southernafricanvleirat","seychellesfody","jacksonswidowbird","sunflowerseastar","indri","yellowgoatfish","elephant","lion"), ]
wikiHits$article <- droplevels(wikiHits$article)
head(wikiHits)
levels(wikiHits$article)

# split the data up into a Planet Earth 2 dataset and a Planet Earth 1 dataset
splitData <- split(wikiHits, wikiHits$series)
wikiHitsPE1 <- splitData$PE1
wikiHitsPE1<-droplevels(wikiHitsPE1)

wikiHitsPE2 <- splitData$PE2
wikiHitsPE2<-droplevels(wikiHitsPE2)

# From long format to wide format 
# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values
data_widePE1 <- spread(wikiHitsPE1, article, views)
head(data_widePE1)

data_widePE2 <- spread(wikiHitsPE2, article, views)
head(data_widePE2)

# plot the basic time series 
plot(data_widePE2$komododragon~data_widePE2$date,pch=16, type="l")
plot(data_widePE1$bactriancamel~data_widePE1$date,pch=16, type="l")

# specify the comparative periods
pre.period <- as.Date(c("2016-05-01", "2016-06-30"))
post.period <- as.Date(c("2017-05-01", "2017-06-30"))

# move date to the end of the dataframe to make the loop easier to work with 
data_widePE1<-data_widePE1[,c(colnames(data_widePE1)[colnames(data_widePE1)!='date'],'date')]
data_widePE1$series <- NULL
head(data_widePE1)

data_widePE2<-data_widePE2[,c(colnames(data_widePE2)[colnames(data_widePE2)!='date'],'date')]
data_widePE2$series <- NULL
head(data_widePE2)

time.points <- data_widePE1$date
class(time.points)
time.points <- as.Date(time.points)
head(time.points)

# function to combine the control time series

combineMedianComp <- function(data1, data2, col, n){
  if(nrow(data1) > nrow(data2)) stop("Rows in 'data2' need to be greater or equal to rows in 'data1'")
  
  medRef <- median(data1[[col]], na.rm = T, ) # median of desired column
  
  medComp <- sapply(data2, function(x){abs(medRef - median(x, na.rm = T))}) # vector with medians for each columns in data2 ('wiki_2')
  
  cols <- names(sort(medComp)[seq_len(n)]) # sort this vector in ascending order, select top n
  
  d2 <- data2[, c(cols)] # select columns in data2 that have medians closest to 'medRef'
  
  d2 <- d2[sample(seq_len(nrow(d2)), size = nrow(data1), replace = F), ] # subset column as to match those in data1
  
  # merge data
  res <- do.call(cbind, list(data1[col], d2)) 
  
  return(res)
}

# run through all of the Planet Earth 2 species' Wikipedia articles and 
# compare them to a control based on the Planet Earth 1 data that is 
# closest in median value 

# amitermesmeridionalis
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "amitermesmeridionalis", n = 5)

df1 <- zoo(cbind(median_data), time.points)
df1

impact1 <- CausalImpact(df1, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact1
#-------------------------

# bamboolemur
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "bamboolemur", n = 5)

df2 <- zoo(cbind(median_data),time.points)
df2

impact2 <- CausalImpact(df2, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact2
#-------------------------

# bobcat
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "bobcat", n = 5)

df3 <- zoo(cbind(median_data),time.points)
df3

impact3 <- CausalImpact(df3, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact3
#-------------------------

# bullersalbatross
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "bullersalbatross", n = 5)

df4 <- zoo(cbind(median_data),time.points)
df4

impact4 <- CausalImpact(df4, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact4
#-------------------------

# butcherbird
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "butcherbird", n = 5)

df5 <- zoo(cbind(median_data),time.points)
df5

impact5 <- CausalImpact(df5, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact5
#-------------------------

# caribou
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "caribou", n = 5)

df6 <- zoo(cbind(median_data),time.points)
df6

impact6 <- CausalImpact(df6, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact6
#-------------------------

# chinstrappenguin
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "chinstrappenguin", n = 5)

df7 <- zoo(cbind(median_data),time.points)
df7

impact7 <- CausalImpact(df7, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact7
#-------------------------

# colubridae
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "colubridae", n = 5)

df8 <- zoo(cbind(median_data),time.points)
df8

impact8 <- CausalImpact(df8, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact8
#-------------------------

# desertlongearedbat
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "desertlongearedbat", n = 5)

df9 <- zoo(cbind(median_data),time.points)
df9

impact9 <- CausalImpact(df9, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact9
#-------------------------

# dracogenus
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "dracogenus", n = 5)

df10<- zoo(cbind(median_data),time.points)
df10

impact10 <- CausalImpact(df10, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact10
#-------------------------

# fairytern
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "fairytern", n = 5)

df11<- zoo(cbind(median_data),time.points)
df11

impact11 <- CausalImpact(df11, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact11
#-------------------------

# flamingo
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "flamingo", n = 5)

df12<- zoo(cbind(median_data),time.points)
df12

impact12 <- CausalImpact(df12, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact12
#-------------------------

# giantanteater
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "giantanteater", n = 5)

df13<- zoo(cbind(median_data),time.points)
df13

impact13 <- CausalImpact(df13, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact13
#-------------------------

# giantotter
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "giantotter", n = 5)

df14<- zoo(cbind(median_data),time.points)
df14

impact14 <- CausalImpact(df14, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact14
#-------------------------

# goldeneagle
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "goldeneagle", n = 5)

df15<- zoo(cbind(median_data),time.points)
df15

impact15 <- CausalImpact(df15, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact15
#-------------------------

# goldenmole
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "goldenmole", n = 5)

df16<- zoo(cbind(median_data),time.points)
df16

impact16 <- CausalImpact(df16, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact16
#-------------------------

# goldeneyeduck
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "goldeneyeduck", n = 5)

df17<- zoo(cbind(median_data),time.points)
df17

impact17 <- CausalImpact(df17, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact17
#-------------------------

# greatbowerbird
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "greatbowerbird", n = 5)

df18<- zoo(cbind(median_data),time.points)
df18

impact18 <- CausalImpact(df18, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact18
#-------------------------

# harrisshawk
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "harrisshawk", n = 5)

df19<- zoo(cbind(median_data),time.points)
df19

impact19 <- CausalImpact(df19, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact19
#-------------------------

# komododragon
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "komododragon", n = 5)

df20<- zoo(cbind(median_data),time.points)
df20

impact20 <- CausalImpact(df20, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact20
#-------------------------

# koribustard
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "koribustard", n = 5)

df21<- zoo(cbind(median_data),time.points)
df21

impact21 <- CausalImpact(df21, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact21
#-------------------------

# lagidium
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "lagidium", n = 5)

df22<- zoo(cbind(median_data),time.points)
df22

impact22 <- CausalImpact(df22, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact22
#-------------------------

# locust
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "locust", n = 5)

df23<- zoo(cbind(median_data),time.points)
df23

impact23 <- CausalImpact(df23, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact23
#-------------------------

# micromys
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "micromys", n = 5)

df24<- zoo(cbind(median_data),time.points)
df24

impact24 <- CausalImpact(df24, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact24
#-------------------------

# nubianibex
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "nubianibex", n = 5)

df25<- zoo(cbind(median_data),time.points)
df25

impact25 <- CausalImpact(df25, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact25
#-------------------------

# pachydactylusrangei
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "pachydactylusrangei", n = 5)

df26<- zoo(cbind(median_data),time.points)
df26

impact26 <- CausalImpact(df26, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact26
#-------------------------

# palechantinggoshawk
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "palechantinggoshawk", n = 5)

df27<- zoo(cbind(median_data),time.points)
df27

impact27 <- CausalImpact(df27, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact27
#-------------------------

# pygmythreetoedsloth
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "pygmythreetoedsloth", n = 5)

df28<- zoo(cbind(median_data),time.points)
df28

impact28 <- CausalImpact(df28, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact28
#-------------------------

# railroadworm
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "railroadworm", n = 5)

df29<- zoo(cbind(median_data),time.points)
df29

impact29 <- CausalImpact(df29, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact29
#-------------------------

# redbirdofparadise
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "redbirdofparadise", n = 5)

df30<- zoo(cbind(median_data),time.points)
df30

impact30 <- CausalImpact(df30, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact30
#-------------------------

# riverdolphin
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "riverdolphin", n = 5)

df31<- zoo(cbind(median_data),time.points)
df31

impact31 <- CausalImpact(df31, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact31
#-------------------------

# saigaantelope
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "saigaantelope", n = 5)

df32<- zoo(cbind(median_data),time.points)
df32

impact32 <- CausalImpact(df32, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact32
#-------------------------

# sandgrouse
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "sandgrouse", n = 5)

df33<- zoo(cbind(median_data),time.points)
df33

impact33 <- CausalImpact(df33, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact33
#-------------------------

# shearwater
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "shearwater", n = 5)

df34<- zoo(cbind(median_data),time.points)
df34

impact34 <- CausalImpact(df34, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact34
#-------------------------

# slothbear
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "slothbear", n = 5)

df35<- zoo(cbind(median_data),time.points)
df35

impact35 <- CausalImpact(df35, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact35
#-------------------------

# snarespenguin
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "snarespenguin", n = 5)

df36<- zoo(cbind(median_data),time.points)
df36

impact36 <- CausalImpact(df36, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact36
#-------------------------

# southerncarminebeeeater
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "southerncarminebeeeater", n = 5)

df37<- zoo(cbind(median_data),time.points)
df37

impact37 <- CausalImpact(df37, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact37
#-------------------------

# spidermonkey
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "spidermonkey", n = 5)

df38<- zoo(cbind(median_data),time.points)
df38

impact38 <- CausalImpact(df38, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact38
#-------------------------

# spottedhyena
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "spottedhyena", n = 5)

df39<- zoo(cbind(median_data),time.points)
df39

impact39 <- CausalImpact(df39, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact39
#-------------------------

# swordbilledhummingbird
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "swordbilledhummingbird", n = 5)

df40<- zoo(cbind(median_data),time.points)
df40

impact40 <- CausalImpact(df40, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact40
#-------------------------

# welscatfish
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "welscatfish", n = 5)

df41<- zoo(cbind(median_data),time.points)
df41

impact41 <- CausalImpact(df41, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact41
#-------------------------

# wilsonsbirdofparadise
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "wilsonsbirdofparadise", n = 5)

df42<- zoo(cbind(median_data),time.points)
df42

impact42 <- CausalImpact(df42, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact42
#-------------------------


# yellowcrazyant
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "yellowcrazyant", n = 5)

df43<- zoo(cbind(median_data),time.points)
df43

impact43 <- CausalImpact(df43, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact43
#-------------------------

# the following species had anomalies only for mobile data
# they get added on top of the species that had anomalies
# for the sum of mobile and desktop 


# arcticwolf
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "arcticwolf", n = 5)

df44<- zoo(cbind(median_data),time.points)
df44

impact44 <- CausalImpact(df44, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact44
#-------------------------

# christmasislandredcrab
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "christmasislandredcrab", n = 5)

df45<- zoo(cbind(median_data),time.points)
df45

impact45 <- CausalImpact(df45, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact45
#-------------------------

# fox
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "fox", n = 5)

df46<- zoo(cbind(median_data),time.points)
df46

impact46 <- CausalImpact(df46, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact46
#-------------------------

# hawksbillseaturtle
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "hawksbillseaturtle", n = 5)

df47<- zoo(cbind(median_data),time.points)
df47

impact47 <- CausalImpact(df47, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact47
#-------------------------

# leopard
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "leopard", n = 5)

df48<- zoo(cbind(median_data),time.points)
df48

impact48 <- CausalImpact(df48, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact48
#-------------------------

# leopard
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "leopard", n = 5)

df48<- zoo(cbind(median_data),time.points)
df48

impact48 <- CausalImpact(df48, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact48
#-------------------------

# namaquachameleon
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "namaquachameleon", n = 5)

df49<- zoo(cbind(median_data),time.points)
df49

impact49 <- CausalImpact(df49, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact49
#-------------------------

# oryx
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "oryx", n = 5)

df50<- zoo(cbind(median_data),time.points)
df50

impact50 <- CausalImpact(df50, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact50
#-------------------------

# raccoon
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "raccoon", n = 5)

df51<- zoo(cbind(median_data),time.points)
df51

impact51 <- CausalImpact(df51, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact51
#-------------------------

# starling
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "starling", n = 5)

df52<- zoo(cbind(median_data),time.points)
df52

impact52 <- CausalImpact(df52, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact52
#-------------------------

# wildebeest
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "wildebeest", n = 5)

df53<- zoo(cbind(median_data),time.points)
df53

impact53 <- CausalImpact(df53, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact53
#-------------------------

# zebra
#-------------------------
median_data <- combineMedianComp(data1 = data_widePE2, data2 = data_widePE1[,-120], col = "zebra", n = 5)

df54<- zoo(cbind(median_data),time.points)
df54

impact54 <- CausalImpact(df54, pre.period, post.period,model.args = list(nseasons = 7, season.duration = 1))
impact54
#-------------------------



# explanation of posterior tail-area probability p 
# https://stats.stackexchange.com/questions/263763/what-does-posterior-tail-area-probability-mean-in-causal-impact?rq=1

# store the posterior probabilities
posterior_prob <- c(
impact1$summary$p[1],
impact2$summary$p[1],
impact3$summary$p[1],
impact4$summary$p[1],
impact5$summary$p[1],
impact6$summary$p[1],
impact7$summary$p[1],
impact8$summary$p[1],
impact9$summary$p[1],
impact10$summary$p[1],
impact11$summary$p[1],
impact12$summary$p[1],
impact13$summary$p[1],
impact14$summary$p[1],
impact15$summary$p[1],
impact16$summary$p[1],
impact17$summary$p[1],
impact18$summary$p[1],
impact19$summary$p[1],
impact20$summary$p[1],
impact21$summary$p[1],
impact22$summary$p[1],
impact23$summary$p[1],
impact24$summary$p[1],
impact25$summary$p[1],
impact26$summary$p[1],
impact27$summary$p[1],
impact28$summary$p[1],
impact29$summary$p[1],
impact30$summary$p[1],
impact31$summary$p[1],
impact32$summary$p[1],
impact33$summary$p[1],
impact34$summary$p[1],
impact35$summary$p[1],
impact36$summary$p[1],
impact37$summary$p[1],
impact38$summary$p[1],
impact39$summary$p[1],
impact40$summary$p[1],
impact41$summary$p[1],
impact42$summary$p[1],
impact43$summary$p[1])
#impact44$summary$p[1],
#impact45$summary$p[1],
#impact46$summary$p[1],
#impact47$summary$p[1],
#impact48$summary$p[1],
#impact49$summary$p[1],
#impact50$summary$p[1],
#impact51$summary$p[1],
#impact52$summary$p[1],
#impact53$summary$p[1],
#impact54$summary$p[1]
#)

# pull out the names for each species from PE2
article_names <-  c(head(names(df1),1),
                   head(names(df2),1),
                   head(names(df3),1),
                   head(names(df4),1),
                   head(names(df5),1),
                   head(names(df6),1),
                   head(names(df7),1),
                   head(names(df8),1),
                   head(names(df9),1),
                   head(names(df10),1),
                   head(names(df11),1),
                   head(names(df12),1),
                   head(names(df13),1),
                   head(names(df14),1),
                   head(names(df15),1),
                   head(names(df16),1),
                   head(names(df17),1),
                   head(names(df18),1),
                   head(names(df19),1),
                   head(names(df20),1),
                   head(names(df21),1),
                   head(names(df22),1),
                   head(names(df23),1),
                   head(names(df24),1),
                   head(names(df25),1),
                   head(names(df26),1),
                   head(names(df27),1),
                   head(names(df28),1),
                   head(names(df29),1),
                   head(names(df30),1),
                   head(names(df31),1),
                   head(names(df32),1),
                   head(names(df33),1),
                   head(names(df34),1),
                   head(names(df35),1),
                   head(names(df36),1),
                   head(names(df37),1),
                   head(names(df38),1),
                   head(names(df39),1),
                   head(names(df40),1),
                   head(names(df41),1),
                   head(names(df42),1),
                   head(names(df43),1))
#                   head(names(df44),1),
#                  head(names(df45),1),
#                   head(names(df46),1),
#                   head(names(df47),1),
#                   head(names(df48),1),
#                   head(names(df49),1),
#                   head(names(df50),1),
#                   head(names(df51),1),
#                   head(names(df52),1),
#                   head(names(df53),1),
#                   head(names(df54),1))

article_names
# stick them together
summary_causal_impact <- cbind(data.frame(posterior_prob,article_names))
# find out which values are less than 0.05
summary_causal_impact$significance <- ifelse(summary_causal_impact$posterior_prob<0.05,1,0)
sum(summary_causal_impact$significance)


# lower values for average response
lower<-c(
impact1$summary$AbsEffect.lower[1],
impact2$summary$AbsEffect.lower[1],
impact3$summary$AbsEffect.lower[1],
impact4$summary$AbsEffect.lower[1],
impact5$summary$AbsEffect.lower[1],
impact6$summary$AbsEffect.lower[1],
impact7$summary$AbsEffect.lower[1],
impact8$summary$AbsEffect.lower[1],
impact9$summary$AbsEffect.lower[1],
impact10$summary$AbsEffect.lower[1],
impact11$summary$AbsEffect.lower[1],
impact12$summary$AbsEffect.lower[1],
impact13$summary$AbsEffect.lower[1],
impact14$summary$AbsEffect.lower[1],
impact15$summary$AbsEffect.lower[1],
impact16$summary$AbsEffect.lower[1],
impact17$summary$AbsEffect.lower[1],
impact18$summary$AbsEffect.lower[1],
impact19$summary$AbsEffect.lower[1],
impact20$summary$AbsEffect.lower[1],
impact21$summary$AbsEffect.lower[1],
impact22$summary$AbsEffect.lower[1],
impact23$summary$AbsEffect.lower[1],
impact24$summary$AbsEffect.lower[1],
impact25$summary$AbsEffect.lower[1],
impact26$summary$AbsEffect.lower[1],
impact27$summary$AbsEffect.lower[1],
impact28$summary$AbsEffect.lower[1],
impact29$summary$AbsEffect.lower[1],
impact30$summary$AbsEffect.lower[1],
impact31$summary$AbsEffect.lower[1],
impact32$summary$AbsEffect.lower[1],
impact33$summary$AbsEffect.lower[1],
impact34$summary$AbsEffect.lower[1],
impact35$summary$AbsEffect.lower[1],
impact36$summary$AbsEffect.lower[1],
impact37$summary$AbsEffect.lower[1],
impact38$summary$AbsEffect.lower[1],
impact39$summary$AbsEffect.lower[1],
impact40$summary$AbsEffect.lower[1],
impact41$summary$AbsEffect.lower[1],
impact42$summary$AbsEffect.lower[1],
impact43$summary$AbsEffect.lower[1]
)

# mean values for average response
averageEffect <- c(
impact1$summary$AbsEffect[1],
impact2$summary$AbsEffect[1],
impact3$summary$AbsEffect[1],
impact4$summary$AbsEffect[1],
impact5$summary$AbsEffect[1],
impact6$summary$AbsEffect[1],
impact7$summary$AbsEffect[1],
impact8$summary$AbsEffect[1],
impact9$summary$AbsEffect[1],
impact10$summary$AbsEffect[1],
impact11$summary$AbsEffect[1],
impact12$summary$AbsEffect[1],
impact13$summary$AbsEffect[1],
impact14$summary$AbsEffect[1],
impact15$summary$AbsEffect[1],
impact16$summary$AbsEffect[1],
impact17$summary$AbsEffect[1],
impact18$summary$AbsEffect[1],
impact19$summary$AbsEffect[1],
impact20$summary$AbsEffect[1],
impact21$summary$AbsEffect[1],
impact22$summary$AbsEffect[1],
impact23$summary$AbsEffect[1],
impact24$summary$AbsEffect[1],
impact25$summary$AbsEffect[1],
impact26$summary$AbsEffect[1],
impact27$summary$AbsEffect[1],
impact28$summary$AbsEffect[1],
impact29$summary$AbsEffect[1],
impact30$summary$AbsEffect[1],
impact31$summary$AbsEffect[1],
impact32$summary$AbsEffect[1],
impact33$summary$AbsEffect[1],
impact34$summary$AbsEffect[1],
impact35$summary$AbsEffect[1],
impact36$summary$AbsEffect[1],
impact37$summary$AbsEffect[1],
impact38$summary$AbsEffect[1],
impact39$summary$AbsEffect[1],
impact40$summary$AbsEffect[1],
impact41$summary$AbsEffect[1],
impact42$summary$AbsEffect[1],
impact43$summary$AbsEffect[1]
)

# upper values for average response
upper <- c(
impact1$summary$AbsEffect.upper[1],
impact2$summary$AbsEffect.upper[1],
impact3$summary$AbsEffect.upper[1],
impact4$summary$AbsEffect.upper[1],
impact5$summary$AbsEffect.upper[1],
impact6$summary$AbsEffect.upper[1],
impact7$summary$AbsEffect.upper[1],
impact8$summary$AbsEffect.upper[1],
impact9$summary$AbsEffect.upper[1],
impact10$summary$AbsEffect.upper[1],
impact11$summary$AbsEffect.upper[1],
impact12$summary$AbsEffect.upper[1],
impact13$summary$AbsEffect.upper[1],
impact14$summary$AbsEffect.upper[1],
impact15$summary$AbsEffect.upper[1],
impact16$summary$AbsEffect.upper[1],
impact17$summary$AbsEffect.upper[1],
impact18$summary$AbsEffect.upper[1],
impact19$summary$AbsEffect.upper[1],
impact20$summary$AbsEffect.upper[1],
impact21$summary$AbsEffect.upper[1],
impact22$summary$AbsEffect.upper[1],
impact23$summary$AbsEffect.upper[1],
impact24$summary$AbsEffect.upper[1],
impact25$summary$AbsEffect.upper[1],
impact26$summary$AbsEffect.upper[1],
impact27$summary$AbsEffect.upper[1],
impact28$summary$AbsEffect.upper[1],
impact29$summary$AbsEffect.upper[1],
impact30$summary$AbsEffect.upper[1],
impact31$summary$AbsEffect.upper[1],
impact32$summary$AbsEffect.upper[1],
impact33$summary$AbsEffect.upper[1],
impact34$summary$AbsEffect.upper[1],
impact35$summary$AbsEffect.upper[1],
impact36$summary$AbsEffect.upper[1],
impact37$summary$AbsEffect.upper[1],
impact38$summary$AbsEffect.upper[1],
impact39$summary$AbsEffect.upper[1],
impact40$summary$AbsEffect.upper[1],
impact41$summary$AbsEffect.upper[1],
impact42$summary$AbsEffect.upper[1],
impact43$summary$AbsEffect.upper[1]
)


# stick it all together to export 

summaryData <- data.frame(cbind(lower,averageEffect,upper,summary_causal_impact[1:43,]
))
head(summaryData)
# are the significant effects positive? 
summaryData$positive <- ifelse(summaryData$averageEffect>0,1,0)
# how many significant and positive results?
sum(summaryData$positive>0 & summaryData$significance>0)

# show one as an example - Nubian Ibex
plot(impact25)

# export
write.csv(summaryData,"data/caual_impact_summary.csv",row.names = F)

