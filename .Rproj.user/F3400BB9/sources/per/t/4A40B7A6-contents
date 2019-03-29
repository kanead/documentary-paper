################################################################################### Documentary analysis
# Code for Regression Models 
##################################################################################
library(ggplot2)
library(fmsb) # VIF
library(MASS) # for negative binomial regression
library(broom) # export regression results 
library(knitr) # export regression results 

mydata<-read.csv("data/master.csv", header = T,sep=",")
head(mydata)
tail(mydata)
subData <- mydata[,c("baseline_2015","tweet","diff","seconds","taxa_gen","status","pred_prey","diaries")]
head(subData)
tail(subData)
# diff represents the difference between the maximum value the page had on the day
# or day after broadcast minus the baseline value from mid 2015 to mid 2016

# rename prey in the pred_prey as pred
levels(subData$pred_prey)[levels(subData$pred_prey)=="prey"] <- "pred"
head(subData)

# recode actinopterygii, amphibia, arachnida, diplopoda, malacostraca 
levels(subData$taxa_gen)[levels(subData$taxa_gen)=="actinopterygii"] <- "reptilia"
levels(subData$taxa_gen)[levels(subData$taxa_gen)=="amphibia"] <- "reptilia"
levels(subData$taxa_gen)[levels(subData$taxa_gen)=="arachnida"] <- "insecta"
levels(subData$taxa_gen)[levels(subData$taxa_gen)=="diplopoda"] <- "insecta"
levels(subData$taxa_gen)[levels(subData$taxa_gen)=="malacostraca"] <- "insecta"
head(subData)
levels(subData$taxa_gen)

##################################################################################
# ANOVA of time on screen as a function of conservation status 
##################################################################################
# replace the NAs in conservation status with 0 
subData$status[is.na(subData$status)] <- 0
subData$status <- as.factor(subData$status)
levels(subData$status)
m.aov<-aov(subData$seconds~subData$status)
summary(m.aov)
p <- ggplot(subData, aes(x=status, y=seconds,fill=status)) + 
  geom_boxplot() +
labs(x="IUCN status", y = "Seconds on screen")
p + scale_fill_manual(labels = c("data deficient", "least concern", "near threatened", "vulnerable","endangered","critically endangered"),values=c("#ffffff", "#009933", "#336600","#ff9900", "#cc0000", "#990000"),name="IUCN status") + theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(subData$status)
round(summary(subData$status)/sum(table(subData$status)),3)
sum(round(summary(subData$status)/sum(table(subData$status)),3))
p2 <- ggplot(subData, aes(x=status, y=tweet,fill=status)) + 
  geom_boxplot() +
  labs(x="IUCN status", y = "number of tweets")
p2 + scale_fill_manual(labels = c("data deficient", "least concern", "near threatened", "vulnerable","endangered","critically endangered"),values=c("#ffffff", "#009933", "#336600","#ff9900", "#cc0000", "#990000"),name="IUCN status") + theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

##################################################################################
# Simple linear model for wikipedia hits 
##################################################################################
# find out which explanatory variables have NAs
apply(subData, 2, function(x){any(is.na(x))})

m.lm.wiki <- lm(diff~seconds,data = subData)
summary(m.lm.wiki)
plot(m.lm.wiki) # poor fit

mult.m.lm.wiki <- lm(diff~seconds+tweet+taxa_gen+status+pred_prey+diaries,data = subData)
summary(mult.m.lm.wiki)
plot(mult.m.lm.wiki) # poor fit
##################################################################################
# fit poisson regression for wiki hits on with the negatives removed  
##################################################################################
# drop the differences that are negative i.e. those pages that had values less
# than the median on the episode of Planet Earth 2 covered that species
length(subData$baseline_2015)
wikiData<-subData[subData$diff>0,]
wikiData <- droplevels(wikiData)
length(wikiData$baseline_2015)

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
mult.m.NB.wiki <- glm.nb(diff~seconds+taxa_gen+status+pred_prey+diaries,data = wikiData)
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
# Simple linear model for twitter hits of the species
##################################################################################
tweetData <- subData[complete.cases(subData), ] # drop the NAs for twitter 

m.lm.twitter<-lm(tweet~seconds,data=tweetData)
summary(m.lm.twitter)
plot(m.lm.twitter) # poor fit 

##################################################################################
# fit negative binomial regression for tweet counts 
##################################################################################
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
mult.m.NB.twitter<-glm.nb(tweet~seconds+taxa_gen+status+diaries,data=tweetData)
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

