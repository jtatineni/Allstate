#it extracts the best tree number 
extractBestTree <- function(model, prediction, startsAt = 0, y = NULL){
  
  if(model$distribution == 'bernoulli' | model$distribution == 'adaboost' | model$distribution == 'huberized'){
    predictionVector <- prediction[ , which.min(abs(n.trees - which.min(model$train.error)))]
    predictionVector <- round(predictionVector)
    predictionVector[predictionVector < 0] <- 0
    predictionVector[predictionVector > 1] <- 1
    
  }else if(model$distribution == 'multinomial' & length(dim(prediction)) == 2 & !is.null(y)){
    probablities <- table(y) / length(y)
    predictionOne <- apply(prediction, 1, function(vector){
      return(vector *  probablities)
    })
    if(startsAt == 0){
      predictionVector <- apply(predictionOne, 2, which.max) - 1 # the minus one is because the first index refers to zero      
    }else{
      predictionVector <- apply(predictionOne, 2, which.max) # the minus one is because the first index refers to zero      
    }
    
  }else if(model$distribution == 'multinomial' & length(dim(prediction)) == 2 & is.null(y)){
    if(startsAt == 0){
      predictionVector <- apply(prediction, 1, which.max) - 1 # the minus one is because the first index refers to zero      
    }else{
      predictionVector <- apply(prediction, 1, which.max) # the minus one is because the first index refers to zero      
    }
    
  }else{
    predictionOne <- prediction[ , , which.min(abs(n.trees - which.min(model$train.error)))]
    
    if(startsAt == 0){
      predictionVector <- apply(predictionOne, 1, which.max) - 1 # the minus one is because the first index refers to zero      
    }else{
      predictionVector <- apply(predictionOne, 1, which.max) # the minus one is because the first index refers to zero      
    }
  }
  return(predictionVector)  
}
