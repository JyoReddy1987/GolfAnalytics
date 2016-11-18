###################################################################
# Golf Project (Embarked: 9/23/2015)
# Plotting Finish Position & Strokes Gained time series
# Last Updated: 9/28/2015
# Inki Sul

require(e1071)
require(stringr)
library(reshape2)
require(ggplot2)
require(gridExtra)
library(grid)
library(gtable)
if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)}

getwd()
# set working directory to wherever the RData file is located
setwd("/home/chaitu1287/Dropbox/golf-project-Inki")
load(file="golf_environment.RData")

# Subsetting for single player, for example: tiger woods
# Get input
firstname_Input <- "tiger"
lastname_Input <- "woods"
input <- paste (firstname_Input,lastname_Input, sep = " ", collapse = NULL)

# Use inputs to subset dataset
event$Player.Name <- paste(event$Player.First.Name, event$Player.Last.Name, set=" ")
indices <- grep(input, event$Player.Name, ignore.case = TRUE, fixed = FALSE)
subset <- subset(event[indices,], select=c(2,3,4,5,6,7,9,10,11,14,16,18))

# Replace cutoff fails with 100
subset_final <- subset
subset_final$Finish.Position[subset_final$Finish.Position==999] <- 100
# This drops cutoff observations
# subset_final <- subset(subset, Finish.Position!=999)

# y-axis: finish position in reversed y-axis OR money won per tournament
FPMW1 <- ggplot(data = subset_final, aes(x = Player.Age.Numeric, y = Finish.Position)) + geom_line()+ylim(100,1)
FPMW2 <- ggplot(data = subset_final, aes(x = Player.Age.Numeric, y = Finish.Position)) + geom_point()+ylim(100,1)
FPMW3 <- ggplot(data = subset_final, aes(x = Player.Age.Numeric, y = Money.Won)) + geom_line()
FPMW4 <- ggplot(data = subset_final, aes(x = Player.Age.Numeric, y = Money.Won)) + geom_point()
pdf(paste(input, "finishPosition_MoneyWon.pdf", sep=" ")) # Creating pdf file to store all four possible graphs
grid.arrange(FPMW1, FPMW2, FPMW3, FPMW4,ncol=2, nrow=2)
dev.off()

# Strokes gained plotted against finish position
p1 <- ggplot(data = subset_final) + geom_point(aes(x = Player.Age.Numeric, y = Finish.Position)) + ylim(100,1)
# Reshaping dataset for bar charts
subset_final_long <- melt(subset(subset_final, select=c(7,10,11)), id=1)
subset_final_long <- subset_final_long[order(subset_final_long$Player.Age.Numeric),]
p2 <- ggplot(subset_final_long, aes(Player.Age.Numeric,value,fill=variable)) + 
  geom_bar(position="dodge",stat="identity") + 
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))

pdf(paste(input, "output.pdf", sep=" ")) # Creating pdf file to store two graphs
grid.arrange(p1,p2,ncol=1, nrow=2)
dev.off()
