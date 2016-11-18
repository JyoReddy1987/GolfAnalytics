###################################################################
# Golf Project (Embarked: 9/23/2015)
# Tournament Importance Analysis
# Last Updated: 12/17/2015
# Inki Sul

require(e1071)
require(stringr)
library(reshape2)
require(ggplot2)
require(gridExtra)
library(grid)
library(gtable)
require(UsingR)
if(!require(coefplot)) { install.packages("coefplot"); require(coefplot)}
if(!require(doBy)) { install.packages("doBy"); require(doBy)}
if(!require(plyr)) { install.packages("plyr"); require(plyr)}
if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)}
require(rpart)
require(rpart.plot)
library(randomForest)
library(tree)
library(party)
if(!require(ROCR)) { install.packages("ROCR"); require(ROCR)}
if(!require(forecast)) { install.packages("forecast"); require(forecast)}
if(!require(zoo)) { install.packages("zoo"); require(zoo)}
if(!require(neuralnet)) { install.packages("neuralnet"); require(neuralnet)}

### Functions to be used later:
# Reordering function within dataframe for prettier data
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

getwd()
setwd("C:/Users/Inki Sul/Documents/Current Documents/Research/Golf/raw-data")

# Read in event level raw data + tournament importance
data <- read.csv('event-level-raw-comparisonTest-importance.csv',header=TRUE, sep=",",row.names=NULL)
# Subsetting the data for Tiger Woods and its performance variables
tiger <- {
  d <- subset(data[grep("Tiger", data$Player.First.Name, ignore.case = TRUE, fixed = FALSE),], select=c(1,2,3,4,8,9,10,14,195,197,118,78,79))
  d <- d[order(d$Tournament.Year,d$Tournament.Number),]
  d$Tournament.Order <- 1:nrow(unique(data.frame(x=d$Tournament.Year, y=d$Tournament.Number)))
  d
}
tiger$SG_Total <- tiger$SG_T2G + tiger$SG_Putts

# Read in round level data and merging tournament importance
round <- read.csv('round-level.csv',header=TRUE, sep=",",row.names=NULL)
names(round)
tour_imp <- read.csv('tournamentNames_forRankPurposes_filled.csv',header=TRUE, sep=",",row.names=NULL)
class(tour_imp$Tournament.Name)
tournaments <- read.csv('tournament-level.csv',header=TRUE, sep=",",row.names=NULL)
names(tournaments)
tournaments$Tournament.Name <- as.factor(tournaments$Tournament.Name)
aa <- merge(tournaments, tour_imp, by=c("Tournament.Name"), all.x=TRUE)
names(aa)
aa$Tour.Code <- NULL
aa <- moveMe(aa, c("Tournament.Importance"), "after","Tournament.Number")
aa <- aa[,1:4]
aa.frame <- unique(data.frame(x=aa$Tournament.Year, y=aa$Tournament.Number, z=aa$Tournament.Name, w=aa$Tournament.Importance))
names(aa.frame) <- c("Tournament.Year", "Tournament.Number", "Tournament.Name", "Tournament.Importance")
round_merged <- merge(round,aa.frame,by=c("Tournament.Year", "Tournament.Number"), all.x=TRUE)
names(round_merged)[4] <- c("Tournament.Name")
round_merged <- moveMe(round_merged, c("Tournament.Importance"), "after","Tournament.Name")
#write.table(round_merged,file='round-level-abridged-importance.csv',sep=',',row.names=F,quote=F)
#data_Shiny <- subset(data, select=c(1,2,3,4,8,9,10,14,195,197,118,78,79))
#write.table(data_Shiny, file='event-level-abridged-importance.csv',sep=',',row.names=F,quote=F)
#event <- read.csv('event-level-abridged-importance.csv',header=TRUE, sep=",",row.names=NULL)
#round <- read.csv('round-level-abridged-importance.csv',header=TRUE, sep=",",row.names=NULL)
#save.image("~/Current Documents/Research/Golf/Shiny/v4/golf_environment3.RData")
names(event)

# Select Tiger Woods on Round data
tiger_round <- {
  d <- subset(round_merged[grep("Tiger", round_merged$Player.First.Name, ignore.case = TRUE, fixed = FALSE),])
  d <- d[order(d$Tournament.Year,d$Tournament.Number, d$Round.Number),]
  d$Tournament.Order <- 1:nrow(unique(data.frame(x=d$Tournament.Year, y=d$Tournament.Number, z=d$Round.Number)))
  d
}

############ Strokes Gained plots for event level data
# hist <- hist(tiger$SG_Total)
# Draw with black outline, white fill
SGT_general_hist <- ggplot(tiger, aes(x=SG_Total)) 
  + geom_histogram(binwidth=1, colour="black", fill="white")
# Histogram overlaid with kernel density curve
SGT_hist_density <- ggplot(tiger, aes(x=SG_Total)) + 
  geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
                 binwidth=1, colour="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") # Overlay with transparent density plot

# Overlaid histograms by different importance
tiger$Tournament.Importance <- as.factor(tiger$Tournament.Importance)
SGT_hist_byGroup <- ggplot(tiger, aes(x=SG_Total, fill=Tournament.Importance)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity")
# Interleaved histograms by tournament importance
SGT_hist_byGroup <- ggplot(tiger, aes(x=SG_Total, fill=Tournament.Importance)) +
  geom_histogram(binwidth=1, position="dodge")
# Density plots
ggplot(tiger, aes(x=SG_Total, colour=Tournament.Importance)) + geom_density()
# Taking means for each importance
cdat <- ddply(tiger, "Tournament.Importance", summarise, SG_Total_mean=mean(SG_Total, na.rm=TRUE))
# Density plots with means
ggplot(tiger, aes(x=SG_Total, colour=Tournament.Importance)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=SG_Total_mean,  colour=Tournament.Importance),
             linetype="dashed", size=1)

# Box plots
ggplot(tiger, aes(x=Tournament.Importance, y=SG_Total, fill=Tournament.Importance)) + geom_boxplot() + guides(fill=FALSE)

# Using facets
ggplot(tiger, aes(x=SG_Total)) + geom_histogram(binwidth=1, colour="black", fill="white") + facet_grid(Tournament.Importance ~ .)

# With mean lines, using cdat from above
ggplot(tiger, aes(x=SG_Total)) + geom_histogram(binwidth=1, colour="black", fill="white") + 
  facet_grid(Tournament.Importance ~ .) +
  geom_vline(data=cdat, aes(xintercept=SG_Total_mean),
             linetype="dashed", size=1, colour="red")

################################ On ROUND-BY-ROUND DATA
names(tiger_round)
SGT_general_hist <- ggplot(tiger_round, aes(x=Total.SG)) + geom_histogram(binwidth=1, colour="black", fill="white")
# Histogram overlaid with kernel density curve
SGT_hist_density <- ggplot(tiger_round, aes(x=Total.SG)) + 
  geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
                 binwidth=1, colour="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") # Overlay with transparent density plot

# Overlaid histograms by different importance
tiger_round$Tournament.Importance <- as.factor(tiger_round$Tournament.Importance)
SGT_hist_byGroup <- ggplot(tiger_round, aes(x=Total.SG, fill=Tournament.Importance)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity")
# Interleaved histograms by tournament importance
SGT_hist_byGroup <- ggplot(tiger_round, aes(x=Total.SG, fill=Tournament.Importance)) +
  geom_histogram(binwidth=1, position="dodge")
# Density plots
ggplot(tiger_round, aes(x=Total.SG, colour=Tournament.Importance)) + geom_density()
# Taking means for each importance
mdat <- ddply(tiger_round, "Tournament.Importance", summarise, SG_Total_mean=mean(Total.SG, na.rm=TRUE))
# Density plots with means
ggplot(tiger_round, aes(x=Total.SG, colour=Tournament.Importance)) +
  geom_density() +
  geom_vline(data=mdat, aes(xintercept=SG_Total_mean,  colour=Tournament.Importance),
             linetype="dashed", size=1)

# Box plots
ggplot(tiger_round, aes(x=Tournament.Importance, y=Total.SG, fill=Tournament.Importance)) + geom_boxplot() + guides(fill=FALSE)


## Chronologically ordered plots of Strokes Gained data
SGT1 <- ggplot(tiger, aes(x=Tournament.Order,y=SG_Total,group=Tournament.Importance)) + 
  geom_line(aes(colour=Tournament.Importance)) + 
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))
SGT1

SGT2 <- ggplot(tiger, aes(x=Player.Age.Numeric,y=SG_Total,group=Tournament.Importance)) + 
  geom_line(aes(colour=Tournament.Importance)) + 
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))
SGT2

SGPutt1 <- ggplot(tiger, aes(x=Tournament.Order,y=SG_Putts,group=Tournament.Importance)) + 
  geom_line(aes(colour=Tournament.Importance)) + 
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))
SGPutt1

SGT2G1 <- ggplot(tiger, aes(x=Tournament.Order,y=SG_T2G,group=Tournament.Importance)) + 
  geom_line(aes(colour=Tournament.Importance)) + 
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))
SGT2G1

SG <- ggplot(melt(subset(tiger, select=c(14,9,10)), id=1), aes(Tournament.Order,value,fill=variable)) + 
  geom_bar(position="dodge",stat="identity") + 
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))
SG

# Please refer to Tournament Importance Plots.Rmd for more details.