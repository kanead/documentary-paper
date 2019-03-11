##################################################################################
# Documentary analysis
# Code extracts the timeseries of wikipedia articles of species featured in 
# Planet  Earth 1 as a control for the number of anomalies
##################################################################################

# required packages
library(dplyr)
library(pageviews)

# load in the data which is a vector of species names 
data<-read.csv("data/mentioned_names_PE1.csv", header = TRUE, sep = ",")

head(data)
length(data$name)

# remove the duplicated values 
data.new<-data[!duplicated(data), ]
length(data.new)
class(data.new)

# modify the function from pageviews package to collect data for each species - UK air dates
get_wiki <- function(x){article_pageviews(project = "en.wikipedia", article = x
                      , start = as.Date('2016-01-01'), end = as.Date("2016-12-31")
                      , user_type = "user", platform = c("desktop", "mobile-web"))
}

# loop over each species 
output<-data.new %>%  get_wiki
length(output)
head(output)
tail(output)
output$article<-as.factor(output$article)
levels(output$article)

# save the collected data
write.csv(output, file = "data/output_PE1.csv",row.names=FALSE)

# mean values of hits for each species 
# for mobile data
mobileOutput <- output[output$access=="mobile-web" , ]
mobileMean<-with(mobileOutput, tapply(views, article, mean))
round(mobileMean,0)

# for desktop data
desktopOutput <- output[output$access=="desktop" , ]
desktopMean<-with(desktopOutput, tapply(views, article, mean))
round(desktopMean,0)

