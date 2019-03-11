##################################################################################
# Documentary analysis
# Code extracts the timeseries of wikipedia articles of species featured in 
# Planet  Earth 2
# UK run 6 November to 11 December 2016
# This encompasses week 44 to week 50
##################################################################################

# required packages
library(dplyr)
library(pageviews)

# load in the data which is a vector of species names mentioned in Planet Earth 2
data<-read.csv("data/mentioned_names.csv", header = TRUE, sep = ",")
head(data)
length(data$name)

# remove the duplicated values 
data.new<-data[!duplicated(data), ]
length(data.new)
class(data.new)

# use the function from pageviews package to collect data for each species - UK air dates
get_wiki <- function(x){article_pageviews(project = "en.wikipedia", article = x
                       , start = as.Date('2016-01-01'), end = as.Date("2016-12-31")
                       , user_type = "user", platform = c("desktop", "mobile-web"))
}

# loop over each species to collect the data
output<-data.new %>%  get_wiki
head(output)
tail(output)

# save the collected data
write.csv(output, file = "data/output.csv",row.names=FALSE)

# mean values of hits for each species 
# for mobile data
mobileOutput <- output[output$access=="mobile-web" , ]
mobileMean<-with(mobileOutput, tapply(views, article, mean))
round(mobileMean,0)

# for desktop data
desktopOutput <- output[output$access=="desktop" , ]
desktopMean<-with(desktopOutput, tapply(views, article, mean))
round(desktopMean,0)


