#Allstate Competition
#ver 0.4

#########################
#Init
rm(list=ls(all=TRUE))

#Load/install libraries
require("ggplot2")
require("gbm")
require("Metrics")

#Set Working Directory
workingDirectory <- '/home/wacax/Documents/Wacax/Kaggle Data Analysis/Allstate/Allstate/'
setwd(workingDirectory)

dataDirectory <- '/home/wacax/Documents/Wacax/Kaggle Data Analysis/Allstate/Data/'

#Load functions
source(paste0(workingDirectory, 'gridCrossValidationGBM.R'))

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

train$customer_ID <- as.character(train$customer_ID)
train$day <- as.factor(train$day)
train$time <- strftime(train$time, format = "%I:%M")
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

test$customer_ID <- as.character(test$customer_ID)
test$day <- as.factor(test$day)
test$time <- strftime(test$time, format = "%H:%M")
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

#####################################
#EDA
#this needs to be automated as much as possible
set.seed(101)
sampleIndices <- sort(sample(1:nrow(train), 2000)) # these indices are good for the train features and features plots

#Product options density plots
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

###############################################
#sample data
train$record_type <- as.character(train$record_type) #using characters reduces the search by 50% time
test$record_type <- as.character(test$record_type) #same goes here

nonPurchaseRandSamples <- sapply(unique(train$customer_ID), anonFun <- function(ID){
  randZeroIndex <- sample(which(train$customer_ID == ID & train$record_type == 0), 1)
  return(randZeroIndex)
}
)
#save this!, the process takes about 2.5 hours
save(nonPurchaseRandSamples, file = 'randSamples.RData')

purchasesIndices <- train$record_type == 1
pre_purchaseIndices <- 1:nrow(train) %in% (which(train$record_type == 1) - 1)
pur.prePur.Indices <- purchasesIndices | pre_purchaseIndices
purchaseVector <- train$record_type[purchasesIndices | pre_purchaseIndices]
randPlusPurchase <- purchasesIndices | 1:nrow(train) %in% nonPurchaseRandSamples

#create a "y" matrix and merge it with the train matrix
yMatrix <- train[purchasesIndices, c('customer_ID', 'A', 'B', 'C', 'D', 'E', 'F', 'G')]
names(yMatrix) <- c('customer_ID', 'Ay', 'By', 'Cy', 'Dy', 'Ey', 'Fy', 'Gy')
train <- merge(train, yMatrix, all = TRUE)
rm(yMatrix)

#cost vs. product "A"
ggplot(train, aes(x = A, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[pur.prePur.Indices, ], aes(x = A, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[randPlusPurchase, ], aes(x = A, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
#cost vs. product "B"
ggplot(train, aes(x = B, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[pur.prePur.Indices, ], aes(x = B, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[randPlusPurchase, ], aes(x = B, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
#cost vs. product "C"
ggplot(train, aes(x = C, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[pur.prePur.Indices, ], aes(x = C, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[randPlusPurchase, ], aes(x = C, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
#cost vs. product "D"
ggplot(train, aes(x = D, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[pur.prePur.Indices, ], aes(x = D, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[randPlusPurchase, ], aes(x = D, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
#cost vs. product "E"
ggplot(train, aes(x = E, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[pur.prePur.Indices, ], aes(x = E, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
#cost vs. product "F"
ggplot(train, aes(x = F, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[pur.prePur.Indices, ], aes(x = F, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[randPlusPurchase, ], aes(x = F, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
#cost vs. product "G"
ggplot(train, aes(x = G, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[pur.prePur.Indices, ], aes(x = G, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)
ggplot(train[randPlusPurchase, ], aes(x = G, y = cost, fill = record_type)) +  geom_point() + facet_grid(record_type ~ .)

#back to factor for IDA/modeling
train$record_type <- as.factor(train$record_type)
test$record_type <- as.factor(test$record_type)

####################################
#IDA (initial data analysis)
#Correlations between product options and features
#scatterplots of features vs product A
pairs(Ay ~ shopping_pt + record_type + day + time + state + location
      + group_size + homeowner + car_age + car_value + risk_factor
      + age_oldest + age_youngest + married_couple + C_previous + duration_previous 
      + B + C + D + E + F + G + cost, train) 
#scatterplots of features vs product B
pairs(B ~ shopping_pt + record_type + day + time + state + location
      + group_size + homeowner + car_age + car_value + risk_factor
      + age_oldest + age_youngest + married_couple + C_previous + duration_previous 
      + A + C + D + E + F + G + cost, train) 
#scatterplots of features vs product C
pairs(C ~ shopping_pt + record_type + day + time + state + location
      + group_size + homeowner + car_age + car_value + risk_factor
      + age_oldest + age_youngest + married_couple + C_previous + duration_previous 
      + A + B + D + E + F + G + cost, train) 
#scatterplots of features vs product D
pairs(D ~ shopping_pt + record_type + day + time + state + location
      + group_size + homeowner + car_age + car_value + risk_factor
      + age_oldest + age_youngest + married_couple + C_previous + duration_previous 
      + A + B + C + E + F + G + cost, train) 
#scatterplots of features vs product E
pairs(E ~ shopping_pt + record_type + day + time + state + location
      + group_size + homeowner + car_age + car_value + risk_factor
      + age_oldest + age_youngest + married_couple + C_previous + duration_previous 
      + A + B + C + D + F + G + cost, train) 
#scatterplots of features vs product F
pairs(F ~ shopping_pt + record_type + day + time + state + location
      + group_size + homeowner + car_age + car_value + risk_factor
      + age_oldest + age_youngest + married_couple + C_previous + duration_previous 
      + A + B + C + D + E + G + cost, train) 
#scatterplots of features vs product G
pairs(G ~ shopping_pt + record_type + day + time + state + location
      + group_size + homeowner + car_age + car_value + risk_factor
      + age_oldest + age_youngest + married_couple + C_previous + duration_previous 
      + A + B + C + D + E + G + cost, train) 

#end of awfully long EDA
########################################
#Centrod Generation
#Train
centroidsTrain <- sapply(unique(train$customer_ID), function(ID){
  oneCentroidKMeans <- kmeans(train[train$customer_ID == ID & train$record_type == 0, seq(18, 25)],
                              centers = 1, algorithm = 'Hartigan-Wong')
  return(c(ID, oneCentroidKMeans$centers))
})

#Test
centroidsTest <- sapply(unique(test$customer_ID), function(ID){
  oneCentroidKMeans <- kmeans(test[test$customer_ID == ID, seq(18, 25)],
                              centers = 1, algorithm = 'Hartigan-Wong')
  return(c(ID, oneCentroidKMeans$centers))
})

#save this! as the process takes about 1.6 hours each
centroidsTrain <- as.data.frame(t(centroidsTrain))
centroidsTest <- as.data.frame(t(centroidsTest))
names(centroidsTrain) <- c('customer_ID', 'Ac', 'Bc', 'Cc', 'Dc', 'Ec', 'Fc', 'Gc', 'costCentroid')
names(centroidsTest) <- c('customer_ID', 'Ac', 'Bc', 'Cc', 'Dc', 'Ec', 'Fc', 'Gc', 'costCentroid')
save(centroidsTrain, file = 'centroidsTrain.RData')
save(centroidsTest, file = 'centroidsTest.RData')

#Merge centroids with train and test
#centroidsTrain <- cbind(unique(train$customer_ID), centroidsTrain)
#centroidsTest <- cbind(unique(test$customer_ID), centroidsTest)
train <- merge(train, centroidsTrain, all = TRUE)
test <- merge(test, centroidsTest, all = TRUE)
rm(centroidsTrain, centroidsTest)

#########################################
#Modelling
#Tree Boosting

set.seed(1003)
sampleIndices <- sort(sample(1:nrow(train[trainIndices, ]), floor(nrow(train[trainIndices, ]) * 0.6))) # these indices are useful for validation

#Modeling - Training
amountOfTrees <- 2500
NumberofCVFolds <- 5
cores <- NumberofCVFolds

if (NumberofCVFolds > 3){
  cores <- detectCores() - 1
}

treeDepth <- 7 #interaction.depth X-validation


##grid cross validation
gridCrossValidationGBM <- gridCrossValidationGBM(Weekly_Sales ~ ., cbind(extractedFeatures, train[trainIndices, -3]), sampleIndices, amountOfTrees,
                                                 NumberofCVFolds, cores, seq(1, 6), c(0.001, 0.003))
##
optimalTreeDepth <- gridCrossValidationGBM[1]
optimalShrinkage <- gridCrossValidationGBM[2]

#Use best hiperparameters on full data for package "A". Random non-purchase data 
#subsetting
#numberOfSamples <- 10000
numberOfSamples <- length(nonPurchaseRandSamples) 
set.seed(1001)
gbmAllstateA <- gbm(Ay ~ ., data = train[1:nrow(train) %in% sample(nonPurchaseRandSamples, numberOfSamples), c(-1, -3, -5, -7, seq(-27, -32))], 
                  n.trees = amountOfTrees, n.cores = cores, interaction.depth = optimalTreeDepth,
                  shrinkage = optimalShrinkage, verbose = TRUE, distribution = 'multinomial') #input interaction.depth

summary(gbmAllstateA)

#Use best hiperparameters on full data for package "B". Random non-purchase data 
set.seed(1002)
gbmAllstateB <- gbm(By ~ ., data = train[1:nrow(train) %in% sample(nonPurchaseRandSamples, numberOfSamples), c(-1, -3, -5, -7, -26, seq(-28, -32))], 
                    n.trees = amountOfTrees, n.cores = cores, interaction.depth = optimalTreeDepth,
                    shrinkage = optimalShrinkage, verbose = TRUE, distribution = 'multinomial') #input interaction.depth

summary(gbmAllstateB)

#Use best hiperparameters on full data for package "C". Random non-purchase data 
amountOfTrees <- 2100
set.seed(1003)
gbmAllstateC <- gbm(Cy ~ ., data = train[1:nrow(train) %in% sample(nonPurchaseRandSamples, numberOfSamples), c(-1, -3, -5, -7, -26, -27, seq(-29, -32))], 
                    n.trees = amountOfTrees, n.cores = cores, interaction.depth = optimalTreeDepth,
                    shrinkage = optimalShrinkage, verbose = TRUE, distribution = 'multinomial') #input interaction.depth

summary(gbmAllstateC)

#Use best hiperparameters on full data for package "D". Random non-purchase data 
amountOfTrees <- 2500
set.seed(1004)
gbmAllstateD <- gbm(Dy ~ ., data = train[1:nrow(train) %in% sample(nonPurchaseRandSamples, numberOfSamples), c(-1, -3, -5, -7, seq(-26, -28), seq(-30, -32))], 
                    n.trees = amountOfTrees, n.cores = cores, interaction.depth = optimalTreeDepth,
                    shrinkage = optimalShrinkage, verbose = TRUE, distribution = 'multinomial') #input interaction.depth

summary(gbmAllstateD)

#Use best hiperparameters on full data for package "E". Random non-purchase data 
set.seed(1005)
gbmAllstateE <- gbm(Ey ~ ., data = train[1:nrow(train) %in% sample(nonPurchaseRandSamples, numberOfSamples), c(-1, -3, -5, -7, seq(-26, -29), -31, -32)], 
                    n.trees = amountOfTrees, n.cores = cores, interaction.depth = optimalTreeDepth,
                    shrinkage = optimalShrinkage, verbose = TRUE, distribution = 'multinomial') #input interaction.depth

summary(gbmAllstateE)

#Use best hiperparameters on full data for package "F". Random non-purchase data 
set.seed(1006)
gbmAllstateF <- gbm(Fy ~ ., data = train[1:nrow(train) %in% sample(nonPurchaseRandSamples, numberOfSamples), c(-1, -3, -5, -7, seq(-26, -30), -32)], 
                    n.trees = amountOfTrees, n.cores = cores, interaction.depth = optimalTreeDepth,
                    shrinkage = optimalShrinkage, verbose = TRUE, distribution = 'multinomial') #input interaction.depth

summary(gbmAllstateF)

#Use best hiperparameters on full data for package "G". Random non-purchase data 
set.seed(1007)
gbmAllstateG <- gbm(Gy ~ ., data = train[1:nrow(train) %in% sample(nonPurchaseRandSamples, numberOfSamples), c(-1, -3, -5, -7, seq(-26, -31))], 
                    n.trees = amountOfTrees, n.cores = cores, interaction.depth = optimalTreeDepth,
                    shrinkage = optimalShrinkage, verbose = TRUE, distribution = 'multinomial') #input interaction.depth

summary(gbmAllstateG)

#Predictions
#Package "A"
n.trees <- seq(from = 100, to = amountOfTrees, by = 100)
predictionGBMA <- predict(gbmAllstateA, newdata = test[ , c(-1, -3, -5, -7, seq(-27, -32))], 
                         n.trees = n.trees)
dim(predictionGBMA)

#Package "B"
n.trees <- seq(from = 100, to = amountOfTrees, by = 100)
predictionGBMB <- predict(gbmAllstateB, newdata = test[ , c(-1, -3, -5, -7, -26, seq(-28, -32))], 
                          n.trees = n.trees)
dim(predictionGBMB)

#Package "C"
n.trees <- seq(from = 100, to = amountOfTrees, by = 100)
predictionGBMC <- predict(gbmAllstateC, newdata = test[ ,  c(-1, -3, -5, -7, -26, -27, seq(-29, -32))], 
                          n.trees = n.trees)
dim(predictionGBMC)

#Package "D"
n.trees <- seq(from = 100, to = amountOfTrees, by = 100)
predictionGBMD <- predict(gbmAllstateD, newdata = test[ , c(-1, -3, -5, -7, seq(-26, -28), seq(-30, -32))]], 
                          n.trees = n.trees)
dim(predictionGBMD)

#Package "E"
n.trees <- seq(from = 100, to = amountOfTrees, by = 100)
predictionGBME <- predict(gbmAllstateE, newdata = test[ , c(-1, -3, -5, -7, seq(-26, -29), -31, -32)], 
                          n.trees = n.trees)
dim(predictionGBME)

#Package "F"
n.trees <- seq(from = 100, to = amountOfTrees, by = 100)
predictionGBMF <- predict(gbmAllstateF, newdata = test[ , c(-1, -3, -5, -7, seq(-26, -30), -32)], 
                          n.trees = n.trees)
dim(predictionGBMF)

#Package "G"
n.trees <- seq(from = 100, to = amountOfTrees, by = 100)
predictionGBMG <- predict(gbmAllstateG, newdata = test[ , c(-1, -3, -5, -7, seq(-26, -31))], 
                          n.trees = n.trees)
dim(predictionGBMG)

#Create prediction matrix
predictionMatrix <- sapply(list([gbmAllstateA, predictionGBMA], [gbmAllstateB, predictionGBMB],
                                [gbmAllstateC, predictionGBMC], [gbmAllstateD, predictionGBMD],
                                [gbmAllstateE, predictionGBME], [gbmAllstateF, predictionGBMF],
                                [gbmAllstateG, predictionGBMG]), function(model){
                               
                                  return(model[[2]][ , which.min(abs(n.trees - which.min(model[[1]]$train.error)))])
                               
                             })
#Save .csv file 
submissionTemplate$plan <- predictionMatrix
write.csv(submissionTemplate, file = "predictionI.csv", row.names = FALSE)
