###################################################################
# Golf Project (Embarked: 9/23/2015)
# Structuring Data
# Last Updated: 9/26/2015
# Problem 1: Randomness in week-to-week putting & ball-striking performance
# Problem 2: Identifying stats of importance at each tournament level
# Inki Sul

# One question: Why doesn't the event level data have course name or number in it? Really need this.

require(XML)
require(lubridate)
require(e1071)
require(stringr)
library(ggplot2)
library(reshape2)

getwd()
setwd("C:/Users/Inki Sul/Documents/Current Documents/Research/Golf/raw-data")

# Course-level dataset ranging from 2003-2015
course <- read.delim('course-level987959.TXT', header=TRUE, sep=";")
names(course)

# Event(Per tournament)-level dataset ranging from 2003-2015
# Structuring dataset
raw <- read.delim('all-event987991.TXT', header=TRUE, sep=";")

# Truncating data for variables of interest

# columns I want to salvage = 

# Strokes Gained columns = c(188,190,191,192,193,194)
# Total Putts Gained: 188
# Putts Gained Rank: 190
# Total Strokes Gained Tee to Green: 191
# Strokes Gained: Tee to Green Rank: 192
# Total SG: 193
# Total SG Rank: 194

# Personal info columns = c(1,2,3,10,7,8,9,14,13,29,144)
# Tour code: 1
# Tournament Year: 2
# Tournament Number: 3
# Tournament Name: 10
# Player Number: 7
# Player Name: 8
# Player Age (Yr,Mth,Day format): 9
# Finish Position: 14
# Money Won: 13
# Total Strokes (under the name Scoring Average - actual (total strokes)): 29
# Total Rounds Played: 144

# Other Important Stats = c(49,50,55,56,94,112,71,72,73,74)
# Driving Distance (Total distance): 49
# Driving Distance (Total drives): 50
# Driving Accuracy (# of fairway hits): 55
# Driving Accuracy (possible fairways): 56
# Green in regulation percentage: 94 (GIR %)
# Scrambling: 112
# Approaches from 150-175ft (Total distance): 71
# Approaches from 150-175ft (# of attempts): 72
# Approaches from 175-200ft (Total distance): 73
# Approaches from 175-200ft (# of attempts): 74
## List of missing data
# Clubhead speed (from shot details?)
# Par 5 Performance
# Par 4 Performance
# Par 3 Performance

data <- subset(raw, select = c(1,2,3,10,7,8,9,14,13,29,144,188,190,191,192,193,194,49,50,55,56,94,112,71,72,73,74))
names(data)
# Rename columns
names(data) <- c("Tour.Code", "Tournament.Year", "Tournament.Number", "Tournament.Name",
                 "Player.Number", "Player.Name", "Player.Age",
                 "Finish.Position", "Money.Won", "Total.Strokes", "Total.Rounds.Played",
                 "SG.Putts", "SG.Putts.Rank", "SG.T2G", "SG.T2G.Rank", "Total.SG", "Total.SG.Rank",
                 "Driving.Distance.Feet", "Driving.Distance.Drives", "Driving.Accuracy.Fairway", "Driving.Accuracy.Possible.Fairways",
                 "GIR.Percentage", "Scrambling",
                 "Approaches.From.150175.Ft","Approaches.From.150175.Attempts","Approaches.From.175200.Ft","Approaches.From.175200.Attempts")

# Cleansing data
data$Tour.Code <- as.character(data$Tour.Code)
data$Money.Won <- as.numeric(gsub(',','',as.character(data$Money.Won),fixed=TRUE))
data$Money.Won[is.na(data$Money.Won)] <- 0

# Split Player first name and last name
data$Player.Name <- as.character(data$Player.Name)
data$Player.Last.Name <- sapply(strsplit(data$Player.Name, ','), function(x) x[1])
data$Player.First.Name <- sapply(strsplit(data$Player.Name, ','), function(x) x[length(x)])
data$Player.First.Name <- gsub(" ", "", data$Player.First.Name, fixed = TRUE)
data$Player.Name <- NULL

### Player age issue (Detailed numeric age to come)
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
data$Approaches.From.175200.Ft <- as.numeric(gsub(" ", "", as.character(data$Approaches.From.175200.Ft), fixed = TRUE))
summary(data$Approaches.From.175200.Ft)
## Function that resolves the "negative sign at the end" issue
sgFix <- function(r)
{
  r <- as.character(r)
  r[grep('-$', r)] <- paste0('-', r[grep('-$', r)])
  r <- sub('-$', '', r)
  r <- as.numeric(gsub(" ", "", r, fixed = TRUE))
  return(r)
}

data$SG.Putts <- sgFix(data$SG.Putts)
data$SG.T2G <- sgFix(data$SG.T2G)
data$Total.SG <- sgFix(data$Total.SG)

# Data output
write.table(course,file='course-level.csv',sep=',',row.names=F,quote=F)
write.table(data,file='tournament-level.csv',sep=',',row.names=F,quote=F)

###############################################
############### ** Prepare round-level data
# Round-level dataset ranging from 2003-2015
# Structuring dataset

raw <- read.delim('round-level992867.TXT', header=TRUE, sep=";")
names(raw)

# Truncating data for variables of interest
# columns I want to salvage = 

# Personal info columns = c(1,2,3,11,5,12,13,8,9,10,16,17,19)
# Tour code: 1
# Tournament Year: 2
# Tournament Schedule Number: 3
# Tournament Name: 11
# Course Number: 5
# Course Name: 12
# Course Par: 13
# Player Number: 8
# Player Name: 9
# Round Number: 10
# Round Score: 16
# End of Round Position: 17
# End of Event Position: 19

# Strokes Gained columns = c(173,174,175)
# Total SG:Putt : 173
# Total SG:T2G : 174
# Total SG: 175

# Other Important Stats = c(35,36,42,43,80,98,57,58,59,60)
# Driving Distance (Total distance): 35
# Driving Distance (Total drives): 36
# Driving Accuracy (# of fairway hits): 42
# Driving Accuracy (Possible Fairways): 43
# Green in regulation percentage: 80 (GIR %)
# Scrambling: 98
# Approaches from 150-175ft (Total distance): 57
# Approaches from 150-175ft (# of attempts): 58
# Approaches from 175-200ft (Total distance): 59
# Approaches from 175-200ft (# of attempts): 60
## List of missing data
# Clubhead speed (from shot details?)
# Par 5 Performance
# Par 4 Performance
# Par 3 Performance

data <- subset(raw, select = c(1,2,3,11,5,12,13,8,9,10,16,17,19,173,174,175,35,36,42,43,80,98,57,58,59,60))
names(data)

# Rename columns
names(data) <- c("Tour.Code", "Tournament.Year", "Tournament.Number", "Tournament.Name",
                 "Course.Number", "Course.Name", "Course.Par", "Player.Number", "Player.Name",
                 "Round.Number", "Round.Score", "End.of.Round.Position", "End.of.Tournament.Position",
                 "SG.Putts", "SG.T2G", "Total.SG",
                 "Driving.Distance.Feet", "Driving.Distance.Drives", "Driving.Accuracy.Fairway", "Driving.Accuracy.Possible.Fairways",
                 "GIR.Percentage", "Scrambling",
                 "Approaches.From.150175.Ft","Approaches.From.150175.Attempts","Approaches.From.175200.Ft","Approaches.From.175200.Attempts")

## Cleansing data
lapply(data, class)
data$Tour.Code <- as.character(data$Tour.Code)

# Split Player first name and last name
data$Player.Name <- as.character(data$Player.Name)
data$Player.Last.Name <- sapply(strsplit(data$Player.Name, ','), function(x) x[1])
data$Player.First.Name <- sapply(strsplit(data$Player.Name, ','), function(x) x[length(x)])
data$Player.First.Name <- gsub(" ", "", data$Player.First.Name, fixed = TRUE)
data$Player.Name <- NULL

### Reordering for prettier data
data <- moveMe(data, c("Player.First.Name", "Player.Last.Name"), "after", "Player.Number")

data$Approaches.From.175200.Ft <- as.numeric(gsub(" ", "", as.character(data$Approaches.From.175200.Ft), fixed = TRUE))

## Function that resolves the "negative sign at the end" issue
data$SG.Putts <- sgFix(data$SG.Putts)
data$SG.T2G <- sgFix(data$SG.T2G)
data$Total.SG <- sgFix(data$Total.SG)

# Data output
write.table(data,file='round-level.csv',sep=',',row.names=F,quote=F)

### To-dos
#CHECK  1. get Event-level data for a particular year (Check if 2014 data differs from other years)
#CHECK Variables of interest: putts.gained.rank, total.rounds.played.putts.gained, total.putts.gained
#CHECK  2. check if aggregate rank for putts.gained.rank matches with rank data from the following website
#CHECK  http://www.pgatour.com/stats/stat.02674.2011.html
#CHECK 3. aggregate 2003-2015 putts.gained.rank, total.rounds.played.putts.gained, Total.Putts.Gained
#CHECK 4. Solve the issue of Strokes Gained data with negative signs on the back
#CHECK Prepare round-level data
# 5. Plot correlating finishing position and SG:T2G / SG:Putting
# 6. regression that verifies which of the two statistics predict the better outcome
# 7. regression on which of the important stats mentioned (I believe the series of variables were taken from Top 10 rank in the Stats page) matters the most on the finishing position or DraftKings score
# 8. research on building an application - use Shiny!
