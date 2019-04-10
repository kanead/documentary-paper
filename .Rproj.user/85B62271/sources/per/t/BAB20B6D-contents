##################################################################################
# Documentary analysis
# Code for Regression Models 
##################################################################################
library(ggplot2)
library(fmsb) # VIF
library(MASS) # for negative binomial regression
library(broom) # export regression results 
library(knitr) # export regression results 
library(readxl)
library(modEvA)

mydata<-read_excel("data/PE2_data.xlsx", sheet = "PE2 data")
subData <- mydata[,c("PE2 name", "Baseline","Number tweets","PE2 visits","Time on screen","Taxa","Status","Interaction","Diaries")]

head(subData)
tail(subData)
# PE2 visits represents the difference between the maximum value the page had on the day
# or day after broadcast minus the baseline value from mid 2015 to mid 2016

# rename columns with spaces
names(subData)[names(subData) == 'PE2 name'] <- 'species'
names(subData)[names(subData) == 'Time on screen'] <- 'seconds'
names(subData)[names(subData) == 'Number tweets'] <- 'tweet'
names(subData)[names(subData) == 'PE2 visits'] <- 'diff'
head(subData)

# number of tweets and diff should be numeric
subData$tweet <- as.numeric(subData$tweet)
subData$diff <- as.numeric(subData$diff)

# Interaction and Taxa should be factor variables
subData$Interaction <- as.factor(subData$Interaction)
subData$Taxa <- as.factor(subData$Taxa)

# look at the taxonomic levels 
levels(subData$Taxa)
table(subData$Taxa)

# replace the NAs in conservation Status with 0 
subData$Status[is.na(subData$Status)] <- 0
subData$Status <- as.factor(subData$Status)

# rename prey in the Interaction column as pred to simplify 
levels(subData$Interaction)[levels(subData$Interaction)=="prey"] <- "pred"
levels(subData$Interaction)

head(subData)

##################################################################################
# fit poisson regression for wiki hits on with the negatives removed  
##################################################################################
# drop the differences that are negative i.e. those pages that had values less
# than the median on the episode of Planet Earth 2 covered that species
# also drop any NA values for Wikipedia 
length(subData$Baseline)
wikiData<-subData[subData$diff>0,]
wikiData <- droplevels(wikiData)
wikiData <- wikiData[complete.cases(wikiData$diff), ] # drop the NAs for twitter 
length(wikiData$Baseline)

# multicolinearity test
VIF(lm(wikiData$tweet~wikiData$seconds+wikiData$diff))
VIF(lm(wikiData$seconds~wikiData$tweet+wikiData$diff))
VIF(lm(wikiData$diff~wikiData$tweet+wikiData$seconds))

m.poiss.wiki <- glm(diff~seconds,data = wikiData,family=poisson)
summary(m.poiss.wiki) # overdispersed 

##################################################################################
# fit negative binomial regression for wiki hits on with the negatives removed  
##################################################################################
m.NB.wiki <- glm.nb(diff~seconds,data = wikiData)
summary(m.NB.wiki)
plot(m.NB.wiki)

# check for overdispersion, should be around 1, one suggested cutoff is < 1.5 
summary(m.NB.wiki)$deviance / summary(m.NB.wiki)$df.residual

# get the coefficients back in units we can understand by undoing the link function
exp(coef(m.NB.wiki))

# get pseudo R^2 values 
modEvA::RsqGLM(model=m.NB.wiki)

# export regression output 
tidy_m.NB.wiki <- tidy(m.NB.wiki)
tidy_m.NB.wiki
kable(tidy_m.NB.wiki)

##################################################################################
# Multiple regression wiki GLM
##################################################################################
mult.m.NB.wiki <- glm.nb(diff~seconds+Taxa+Status+Interaction+Diaries,data = wikiData)
summary(mult.m.NB.wiki)

plot(mult.m.NB.wiki)

# check for overdispersion, should be around 1, one suggested cutoff is < 1.5 
summary(mult.m.NB.wiki)$deviance / summary(mult.m.NB.wiki)$df.residual

# get the coefficients back in units we can understand by undoing the link function
exp(coef(mult.m.NB.wiki))

# get pseudo R^2 values 
modEvA::RsqGLM(model=mult.m.NB.wiki)

# export regression output 
tidy_mult.m.NB.wiki <- tidy(mult.m.NB.wiki)
tidy_mult.m.NB.wiki
kable(tidy_mult.m.NB.wiki)

# compare the simple model with the more complex using AIC
AIC(m.NB.wiki,mult.m.NB.wiki)

##################################################################################
# fit negative binomial regression for tweet counts 
##################################################################################
tweetData <- subData[complete.cases(subData$tweet), ] # drop the NAs for twitter 
length(tweetData$Baseline)

m.NB.twitter<-glm.nb(tweet~seconds,data=tweetData)
summary(m.NB.twitter)
plot(m.NB.twitter)

# check for overdispersion, should be around 1, one suggested cutoff is < 1.5 
summary(m.NB.twitter)$deviance / summary(m.NB.twitter)$df.residual

# get the coefficients back in units we can understand by undoing the link function
exp(coef(m.NB.twitter))

# get pseudo R^2 values 
modEvA::RsqGLM(model=m.NB.twitter)

# export regression output 
tidy_m.NB.twitter <- tidy(m.NB.twitter)
tidy_m.NB.twitter
kable(tidy_m.NB.twitter)

##################################################################################
# Multiple regression twitter GLM
##################################################################################
mult.m.NB.twitter<-glm.nb(tweet~seconds+Taxa+Status+Diaries,data=tweetData)
summary(mult.m.NB.twitter)
plot(mult.m.NB.twitter)

# check for overdispersion, should be around 1, one suggested cutoff is < 1.5 
summary(mult.m.NB.twitter)$deviance / summary(mult.m.NB.twitter)$df.residual

# get the coefficients back in units we can understand by undoing the link function
exp(coef(mult.m.NB.twitter))

# get pseudo R^2 values 
modEvA::RsqGLM(model=mult.m.NB.twitter)

# export regression output 
tidy_mult.m.NB.twitter <- tidy(mult.m.NB.twitter)
tidy_mult.m.NB.twitter
kable(tidy_mult.m.NB.twitter)

# compare the simple model with the more complex using AIC
AIC(m.NB.twitter,mult.m.NB.twitter)

