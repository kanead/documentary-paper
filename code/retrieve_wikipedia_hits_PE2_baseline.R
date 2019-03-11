##################################################################################
# Documentary analysis
# Code extracts the timeseries of wikipedia articles of species featured in 
# Planet Earth 2 before the broadcast dates to act as a baseline
##################################################################################

# required packages
library(dplyr)
library(pageviews)

# load in the data which is a vector of species names 
data<-read.csv("data/mentioned_names.csv", header = TRUE, sep = ",")

head(data)
length(data$name)

# remove the duplicated values 
data.new<-data[!duplicated(data), ]
length(data.new)
class(data.new)

# modify the function from pageviews package to collect data for each species 
get_wiki <- function(x){article_pageviews(project = "en.wikipedia", article = x
                       ,start = as.Date('2015-07-01'), end = as.Date("2016-06-30")
                       ,user_type = "user", platform = c("desktop", "mobile-web"))
}

# loop over each species 
output<-data.new %>%  get_wiki
length(output)
head(output)
tail(output)

# save the collected data
write.csv(output, file = "data/output2015.csv",row.names=FALSE)

# mean values of hits for each species 
# for mobile data
mobileOutput <- output[output$access=="mobile-web" , ]
mobileMean<-with(mobileOutput, tapply(views, article, mean))
round(mobileMean,0)

# for desktop data
desktopOutput <- output[output$access=="desktop" , ]
desktopMean<-with(desktopOutput, tapply(views, article, mean))
round(desktopMean,0)

