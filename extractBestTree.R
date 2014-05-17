#it extracts the best tree number 
extractBestTree <- function(model, prediction, startsAt = 0){
  predictionOne <- prediction[ , ,which.min(abs(n.trees - which.min(model$train.error)))]
  if(startsAt == 0){
    predictionVector <- apply(predictionOne, 1, which.max) - 1 # the minus one is because the first index refers to zero      
  }else{
    predictionVector <- apply(predictionOne, 1, which.max) # the minus one is because the first index refers to zero      
  }
}
