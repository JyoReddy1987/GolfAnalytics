###################################################################
# Golf Project (Embarked: 9/23/2015)
# CART analysis of Draft Kings Score + Weather data
# Last Updated: 10/22/2015
# Inki Sul

require(e1071)
require(stringr)
library(reshape2)
require(ggplot2)
require(gridExtra)
library(grid)
library(gtable)
require(UsingR)
require(rjson)
if(!require(coefplot)) { install.packages("coefplot"); require(coefplot)}
if(!require(doBy)) { install.packages("doBy"); require(doBy)}
if(!require(plyr)) { install.packages("plyr"); require(plyr)}
if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)}
require(rpart)
require(rpart.plot)
library(randomForest)
library(tree)
library(party)
# devtools::install_github("hrbrmstr/Rforecastio")
library(Rforecastio)
library(RCurl)
library(RJSONIO)

# Forecast.io API Key: 3dbbe3c97eeaf75fe39c5af5c5b0fc80
# https://api.forecast.io/forecast/APIKEY/LATITUDE,LONGITUDE,TIME
# more_than_one <- data.frame(loc=c("Maine", "Seattle"),
#                            lon=c(43.2672, 47.6097),
#                            lat=c(70.8617, 122.3331),
#                            when=c("2013-05-06T12:00:00-0400",
#                                   "2013-05-06T12:00:00-0400"),
#                            stringsAsFactors=FALSE)

#bigger_list <- mapply(get_forecast_for, 
#                      more_than_one$lon, more_than_one$lat, more_than_one$when,
#                      SIMPLIFY=FALSE)
#names(bigger_list) <- more_than_one$loc
#bigger_list$Seattle[[1]]
#print(sprintf("You have used %s API calls.", then$`x-forecast-api-calls`))
## [1] "You have used 2 API calls."

getwd()
setwd("C:/Users/Inki Sul/Documents/Current Documents/Research/Golf/raw-data")
course <- read.csv('course-level.csv',header=TRUE, sep=",",row.names=NULL)
data <- read.csv('event-level-DK.csv',header=TRUE, sep=",",row.names=NULL)
round <- read.csv('round-level.csv',header=TRUE, sep=",",row.names=NULL)
round_raw <- read.delim('round-level992867.TXT', header=TRUE, sep=";")
subset(course, select = c(1,2,3,10,7,8,9,14,13,29,144,188,190,191,192,193,194,49,50,55,56,94,112,71,72,73,74))
names(data)[1:5] <- c("Tournament.Year", "Tournament.Number", "Player.Number", "Tournament.Name", "ddd")
data$ddd <- NULL
# Number of unique API queries necessary
nrow(unique(data.frame(x=course$Year, y=course$Course..)))
now <- get_current_forecast(43.2672, -70.8617)

# Google Map Geocoder API to retrieve 
url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA,NA))
  }
}

course_subset <- unique(data.frame(x=course$Course.., y=course$Course.Name))
names(course_subset) <- c("Course.Number", "Course.Name")
course_subset <- course_subset[order(course_subset$Course.Number),]
addresses <- course_subset$Course.Name

locations  <- ldply(addresses, function(x) geoCode(x))
names(locations)  <- c("latitude","longitude","location_type", "formatted")
location_dataset <- cbind(course_subset, locations)
write.table(location_dataset,file='location_dataset.csv',sep=',',row.names=F,quote=F)
write.table(location_dataset,file='location_dataset.txt',sep=';',row.names=T,quote=F)
edit(location_dataset)

########

theQuery <- "https://api.forecast.io/forecast/3dbbe3c97eeaf75fe39c5af5c5b0fc80/LATITUDE,LONGITUDE,TIME"
oneResult <- fromJSON(file=theQuery) # please write file=
oneResult
class(oneResult)
length(oneResult)
oneResult$response
oneResult$response$docs
length(oneResult$response$docs)
oneResult$response$docs[[1]]

##############
shot_example <- read.delim('StrokeLevelTOURChamp.txt', header=TRUE, sep=";")

# Importing 
d <- read.csv('event-level-DK.csv',header=TRUE, sep=",",row.names=NULL)
event_raw <- read.delim('all-event987991.TXT', header=TRUE, sep=";")
names(d)[1:5] <- c("Tournament.Year", "Tournament.Number", "Player.Number", "Tournament.Name", "ddd")
d$ddd <- NULL

######################
# Initial CART analysis (Regression Tree method using rpart package) (Formula: Using the pre-selected candidate variables)
formula1 <- DK.Score ~ Driving.Distance.Feet + Driving.Distance.Drives + Driving.Accuracy.Fairway + Driving.Accuracy.Possible.Fairways + GIR.Percentage +
  Scrambling + Approaches.From.150175.Ft + Approaches.From.150175.Attempts + Approaches.From.175200.Ft + Approaches.From.175200.Attempts +
  SG.Putts + SG.T2G
fit <- rpart(DK.Score ~ Driving.Distance.Feet + Driving.Distance.Drives + Driving.Accuracy.Fairway + Driving.Accuracy.Possible.Fairways + GIR.Percentage +
  Scrambling + Approaches.From.150175.Ft + Approaches.From.150175.Attempts + Approaches.From.175200.Ft + Approaches.From.175200.Attempts +
  SG.Putts + SG.T2G, method="anova", data=d)
printcp(fit)
plotcp(fit)
summary(fit)
rsq.rpart(fit)
r <- predict(fit, newdata=d)
d$CART.Prediction <- r
d$SE.CART <- (d$DK.Score - d$CART.Prediction)^2
mean(d$SE.CART)

# plotting tree
plot(fit, uniform=TRUE, main="Regression Tree for DraftKings Score")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
post(fit, file = "RegTree1.ps", title = "Regression Tree for DraftKings Score")

fit$cptable # read lowest xerror CP
pfit <- prune(fit, cp=0.01) # from cptable
# Or you can try this:
# pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(pfit, uniform=TRUE, main="Pruned Regression Tree for DraftKings Score")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "PrunedRegTree1.ps", title = "Pruned Regression Tree for DraftKings Score")
######################

######################
# Additional CART analysis (Using tree package) (Formula: Using the pre-selected candidate variables)
tree <- tree(formula1, data=d)
summary(tree)
plot(tree)
text(tree)
d$tree.Prediction <- predict(tree, newdata=d)
d$SE.tree <- (d$DK.Score - d$tree.Prediction)^2
mean(d$SE.tree)

# Additional analysis - Conditional Inference Tree (Using party package)
### There seems to be something wrong with the way I am running the code, since I end up in a binary tree over the size of >1.6GB
### Takes 5+ minutes to run
ct <- ctree(formula1, data=d)
plot(ct, main="Conditional Inference Tree")
# Table of prediction errors
table(predict(ct), d$DK.Score)
# Estimated DK.score using CTree stored
tr.pred2 <- predict(ct, newdata=d)
d$CTree.Prediction <- tr.pred2[,1]
d$SE.CTree <- (d$DK.Score - d$CTree.Prediction)^2
mean(d$SE.CTree)
# Trying other conditional tree, also takes >5 minutes to run
reduced_ct <- ctree(formula1, data = d, 
               controls = ctree_control(maxsurrogate = 3))
d$CTree2.Prediction <- predict(reduced_ct, newdata=d)[,1]
d$SE.CTree2 <- (d$DK.Score - d$CTree2.Prediction)^2
mean(d$SE.CTree2)

# Additional analysis - Random Forest (randomForest package, pre-selected candidate variables)
# RF does not handle missing values in predictors that well, so I do not recommend this method. 
# However, I ran it with na.omit to see the results, which may not be convincing nor credible.
# Takes longer than 30+ mins
fit.rf <- randomForest(formula1, data=d, na.action=na.omit)
print(fit.rf)
importance(fit.rf)
plot(importance(fit.rf), lty=2, pch=16)
lines(importance(fit.rf))

## Plotting the impotance of each variables
#imp <- importance(fit.rf)
#impvar <- rownames(imp)[order(imp[,1], decreasing=TRUE)]
#op <- par(mfrow=c(1, 3))
#for (i in seq_along(impvar)) {
#  partialPlot(fit.rf, d, impvar[i], xlab=impvar[i],
#              main=paste("Partial Dependence on", impvar[i]),
#              ylim=c(0, 1))
#}

#### Trying CART analysis on raw data!!
# Event raw data
data <- event_raw[with(event_raw, order(Tournament.Year, Tournament.Number, Player.Number)),]
names(data)
data$DK.Score <- d$DK.Score

# Cleansing data for CART analysis
data$Tour <- NULL
data$Money <- as.numeric(gsub(',','',as.character(data$Money),fixed=TRUE))
data$Money[is.na(data$Money)] <- 0

# Split Player first name and last name
data$Player.Name <- as.character(data$Player.Name)
data$Player.Last.Name <- sapply(strsplit(data$Player.Name, ','), function(x) x[1])
data$Player.First.Name <- sapply(strsplit(data$Player.Name, ','), function(x) x[length(x)])
data$Player.First.Name <- gsub(" ", "", data$Player.First.Name, fixed = TRUE)
data$Player.Name <- NULL
#####
### Player age issue (Detailed numeric age to come)
names(data)[7] <- "Player.Age"
data$Player.Age.Year <- as.integer(sapply(strsplit(as.character(data$Player.Age), ' '), function(x) x[1]))
data$Player.Age.Month <- as.integer(sapply(strsplit(as.character(data$Player.Age), ' '), function(x) x[2]))
data$Player.Age.Day <- as.integer(sapply(strsplit(as.character(data$Player.Age), ' '), function(x) x[3]))
# Notice 1243 data points has been lost due to this procedure. Should look into it.
# mean(data$Player.Age.Year, na.rm=TRUE)
data$Player.Age.Numeric <- data$Player.Age.Year + (1/12)*data$Player.Age.Month + (1/365)*data$Player.Age.Day
data$Player.Age <- data$Player.Age.Year
data$Player.Age.Month <- NULL
data$Player.Age.Day <- NULL

### Reordering for prettier data
moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  return(x)
}
data <- moveMe(data, c("Player.First.Name", "Player.Last.Name"), "before","Player.Age")
data <- moveMe(data, c("Player.Age.Numeric"), "after","Player.Age")

lapply(data, class)

# Cleansing data over 1000 which are read in as 1,000
data$Scoring.Avg.Total.Adjustment. <- as.numeric(gsub(" ", "", as.character(data$Scoring.Avg.Total.Adjustment.), fixed = TRUE))
data[is.na(data$Scoring.Avg.Total.Adjustment.),32] <- 0
data$Finish.Position.text. <- NULL
data$X <- NULL
head(data$Approaches..125.150.Yards.ft.,10)
sum(is.na(data$Approaches..125.150.Yards.ft.))
data$Approaches..125.150.Yards.ft. <- as.numeric(gsub(",", "", as.character(data$Approaches..125.150.Yards.ft.),fixed=TRUE))
data[is.na(data$Approaches..125.150.Yards.ft.),69] <- 0
data$Approaches...175.200.Yards.ft. <- as.numeric(gsub(" ", "", as.character(data$Approaches...175.200.Yards.ft.), fixed = TRUE))
data[is.na(data$Approaches...175.200.Yards.ft.),73] <- 0

data$Approaches....200.Yards.ft. <- as.numeric(gsub(" ", "", as.character(data$Approaches....200.Yards.ft.), fixed = TRUE))
data[is.na(data$Approaches....200.Yards.ft.),75] <- 0

head(data$Total.Distance.ft..Prox.to.Hole, 10)
data$Total.Distance.ft..Prox.to.Hole <- as.numeric(gsub(",", "", as.character(data$Total.Distance.ft..Prox.to.Hole),fixed=TRUE))
sum(is.na(data$Total.Distance.ft..Prox.to.Hole))
data[is.na(data$Total.Distance.ft..Prox.to.Hole),96] <- 0

head(data$Fairway.Prox.distance.in.ft.)
data$Fairway.Prox.distance.in.ft. <- as.numeric(gsub(",", "", as.character(data$Fairway.Prox.distance.in.ft.),fixed=TRUE))
sum(is.na(data$Fairway.Prox.distance.in.ft.))
data[is.na(data$Fairway.Prox.distance.in.ft.),100] <- 0

head(data$Rough.Prox.distance.in.ft.)
data$Rough.Prox.distance.in.ft. <- as.numeric(gsub(",", "", as.character(data$Rough.Prox.distance.in.ft.),fixed=TRUE))
sum(is.na(data$Rough.Prox.distance.in.ft.))
data[is.na(data$Rough.Prox.distance.in.ft.),103] <- 0

head(data$Left.Rough.Prox.distance.in.ft.)
data$Left.Rough.Prox.distance.in.ft. <- as.numeric(gsub(",", "", as.character(data$Left.Rough.Prox.distance.in.ft.),fixed=TRUE))
sum(is.na(data$Left.Rough.Prox.distance.in.ft.))
data[is.na(data$Left.Rough.Prox.distance.in.ft.),106] <- 0

head(data$Right.Rough.Prox.distance.in.ft.)
data$Right.Rough.Prox.distance.in.ft. <- as.numeric(gsub(",", "", as.character(data$Right.Rough.Prox.distance.in.ft.),fixed=TRUE))
sum(is.na(data$Right.Rough.Prox.distance.in.ft.))
data[is.na(data$Right.Rough.Prox.distance.in.ft.),108] <- 0

## Function that resolves the "negative sign at the end" issue
sgFix <- function(r)
{
  r <- as.character(r)
  r[grep('-$', r)] <- paste0('-', r[grep('-$', r)])
  r <- sub('-$', '', r)
  r <- as.numeric(gsub(" ", "", r, fixed = TRUE))
  return(r)
}

data$Total.Putts.Gained <- sgFix(data$Total.Putts.Gained)
data$TTL.SG.T2G <- sgFix(data$TTL.SG.T2G)
data$TTL.SG.Total <- sgFix(data$TTL.SG.Total)

lapply(data, class)
names(data)
data$Player.Age.Year <-  NULL
write.table(data,file='event-level-raw.csv',sep=',',row.names=F,quote=F)
data <- moveMe(data, c("Event.Name", "Official.Event.Y.N."), "after","Player.Age")
names(data)
newdata <- subset(data, select = 12:195)

############ CART analysis on Raw Data (Regression Tree method using rpart package)
fit_raw <- rpart(DK.Score ~ ., method="anova", data=newdata)
printcp(fit_raw)
plotcp(fit_raw)
summary(fit_raw)
rsq.rpart(fit_raw)
newdata$CART.Prediction <- predict(fit_raw, newdata=newdata)
newdata$SE.CART <- (newdata$DK.Score - newdata$CART.Prediction)^2
mean(newdata$SE.CART)

# plotting tree
plot(fit_raw, uniform=TRUE, main="Regression Tree (RAW) for DraftKings Score")
text(fit_raw, use.n=TRUE, all=TRUE, cex=.8)
post(fit_raw, file = "Raw_RegTree1.ps", title = "Regression Tree (RAW) for DraftKings Score")

fit_raw$cptable # read lowest xerror CP
pfit_raw <- prune(fit_raw, cp=0.01062268) # from cptable
# Or you can try this:
# pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
plot(pfit_raw, uniform=TRUE, main="Pruned Regression Tree (RAW) for DraftKings Score")
text(pfit_raw, use.n=TRUE, all=TRUE, cex=.8)
post(pfit_raw, file = "Raw_PrunedRegTree1.ps", title = "Pruned Regression Tree (RAW) for DraftKings Score")

