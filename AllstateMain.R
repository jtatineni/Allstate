#Allstate Competition
#ver 0.1

#########################
#Init
rm(list=ls(all=TRUE))

#Load/install libraries
require("ggplot2")
require("gbm")
require("Metrics")

#Set Working Directory
workingDirectory <- '/home/wacax/Documents/Wacax/Kaggle Data Analysis/Allstate/Allstate'
setwd(workingDirectory)

dataDirectory <- '/home/wacax/Documents/Wacax/Kaggle Data Analysis/Allstate/Data/'

#Load functions

#############################
#Load Data
#Input Data
train <- read.csv(paste0(dataDirectory, 'train.csv'), header = TRUE, stringsAsFactors = FALSE, na.strings=c("", "NA", "NULL"))
test <- read.csv(paste0(dataDirectory, 'test_v2.csv'), header = TRUE, stringsAsFactors = FALSE, na.strings=c("", "NA", "NULL"))

submissionTemplate <- read.csv(paste0(dataDirectory, 'sampleSubmission.csv'), header = TRUE, stringsAsFactors = FALSE, na.strings=c("", "NA", "NULL"))

###############################
#Data Preprocessing
#toFactors <- c('record_type', 'day', 'state', 'location', 'homeowner', 'car_value',
#               'married_couple', 'C_previous')

#sapply(toFactors, anonfun <- function(feature){
#  column <- paste0('train', '$', feature)
#  column <- as.factor(column)  
#})

train$record_type <- as.factor(train$record_type)
train$day <- as.factor(train$day)
train$state <- as.factor(train$state)
train$location <- as.factor(train$location)
train$homeowner <- as.factor(train$homeowner)
train$car_value <- as.factor(train$car_value)
train$married_couple <- as.factor(train$married_couple)
train$C_previous <- as.factor(train$C_previous)
train$A <- as.factor(train$A)
train$B <- as.factor(train$B)
train$C <- as.factor(train$C)
train$D <- as.factor(train$D)
train$E <- as.factor(train$E)
train$F <- as.factor(train$F)
train$G <- as.factor(train$G)
train$cost <- as.numeric(train$cost)

test$record_type <- as.factor(test$record_type)
test$day <- as.factor(test$day)
test$state <- as.factor(test$state)
test$location <- as.factor(test$location)
test$homeowner <- as.factor(test$homeowner)
test$car_value <- as.factor(test$car_value)
test$married_couple <- as.factor(test$married_couple)
test$C_previous <- as.factor(test$C_previous)
test$A <- as.factor(test$A)
test$B <- as.factor(test$B)
test$C <- as.factor(test$C)
test$D <- as.factor(test$D)
test$E <- as.factor(test$E)
test$F <- as.factor(test$F)
test$G <- as.factor(test$G)
test$cost <- as.numeric(test$cost)

####################################
#EDA
#this needs to be automated as much as possible
set.seed(101)
sampleIndices <- sort(sample(1:nrow(train), 2000)) # these indices are good for the train features and features plots

A.spread <- ggplot(train[sampleIndices, ], aes(x = A)) + geom_density()
print(A.spread, height = 6, width = 8)
B.spread <- ggplot(train[sampleIndices, ], aes(x = B)) + geom_density()
print(B.spread, height = 6, width = 8)
C.spread <- ggplot(train[sampleIndices, ], aes(x = C)) + geom_density()
print(C.spread, height = 6, width = 8)
D.spread <- ggplot(train[sampleIndices, ], aes(x = D)) + geom_density()
print(D.spread, height = 6, width = 8)
E.spread <- ggplot(train[sampleIndices, ], aes(x = E)) + geom_density()
print(E.spread, height = 6, width = 8)
Ff.spread <- ggplot(train[sampleIndices, ], aes(x = F)) + geom_density()
print(Ff.spread, height = 6, width = 8)
G.spread <- ggplot(train[sampleIndices, ], aes(x = G)) + geom_density()
print(G.spread, height = 6, width = 8)