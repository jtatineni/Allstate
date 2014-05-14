#it extracts the best tree number 
extractBestTree <- function(model, prediction){
  predictionOne <- prediction[ , ,which.min(abs(n.trees - which.min(model$train.error)))]
  derp <- apply(predictionOne, 1, which.max) - 1 # the minus one is because the first index refers to zero
}
