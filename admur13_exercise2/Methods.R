#This file contains different methods which is need for different algorithms.

#Normalize method - Dataset as a parameter
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Dataset shuffle
datasetShuffle <- function(dataset){
  return(dataset[sample(nrow(dataset)),])
}

#Method for Accuracy
acc <- function(x, y) {
  accu = 0
  for(i in 1:length(x))
  {
    if( x[i] == y[i] )
    {
      accu <- accu + 1;
    }
  }
  return(100*accu/length(y))
}

#Confusion Matrix - The first parameter 'x' is for the predicted model and the second is for the test set.
con_result <- data.frame()
conMatrix <- function(predicted_model,test_data){
  acc <- 0
  pre <- 0
  sensi <- 0
  speci <- 0
  
  conmat <- as.data.frame(table(predict(predicted_model,test_data)))
  
  TN <- conmat[1,3]
  TP <- conmat[4,3]
  FP <- conmat[2,3]
  FN <- conmat[3,3]
  
  #Formula for Accuracy, Precision, Sensitivity and Specificity
  acc <- (TN+TP)/(TN+TP+FP+FN)
  print(acc)
  pre <- (TP)/(TP+FP)
  print(pre)
  sensi <- (TP)/(TP+FN) #Type II error
  print(sensi)
  speci <- (TN)/(TN+FP) #Type I error
  print(speci)
  
  Names <- c("Accuracy", "Precision", "Sensitivity", "Specificity")
  Values <- c(acc, pre, sensi, speci)
  
  return(con_result <- data.frame(Names, Values))
}











