# Source the loadImage.R script in order to load images
source("loadImage.R")
# Source the rExample.R which contains some helper methods, thanks Frederik

# Contains the KNN implementation that we need
library(class)

###########################
###########################
##    Exercise 1.4.1     ##
###########################
###########################

# Load images into a data frame for 100 DPI, group4 and member0
path <- "/home/spider/Documents/software-engineering/sml/exercises/SML-Exercises/admur13_exercise1/group"
data <- loadSinglePersonsData(100, 4, 0, path)
df <- data.frame(data)

# Shuffle the dataset with a specific seed for recreatable reasons
set.seed(123)
df_shuffle <- df[sample(nrow(df)),]

# 50% Training as specified in the exercise, simply take the first 2000 elements and use them for training and add the labels (0-9)
training_set <- df_shuffle[1:2000, -1]
training_labels <- df_shuffle[1:2000,1]
# 50% Test as specified in the exercise, which will be the last 2000 elements 
test_set <- df_shuffle[2001:4000, -1]
test_labels <- df_shuffle[2001:4000, 1]

# Time to test the training data against the test set with a K = 5
prediction <- knn(training_set, test_set, training_labels, 5)

# Function provided by Frederik/N?rbert to get the accuracy of the test.
getAccuracy <- function(testLabels, testPredictions) {
  acc = 0
  for (i in 1:length(testLabels)) {
    if (testLabels[i] == testPredictions[i]) {
      acc <- acc + 1
    }
  }
  acc <- 100 * acc / length(testLabels)
  return(acc)
}

# Shows the accuracy
getAccuracy(test_labels, prediction)

###########################
###########################
##    Exercise 1.4.2     ##
###########################
###########################

# Lets have a collection of accuracies and durations for varying K's
accuracies <- c()
duration.all <- c()
# Lets do it from 1 to a high number of K's
K <- 1:30

# Start time to measure the time it takes to do 1 to a high number of K's
start_time.total <- Sys.time()
# Iterate from 1 to 50
for (i in K) {
  # Start Time
  start_time <- Sys.time()
  # KNN prediction
  prediction <- knn(training_set, test_set, training_labels, i)
  accuracies[i] <- getAccuracy(prediction, test_labels)
  stop_time <- Sys.time()
  # Stop time
  duration <- stop_time - start_time
  # Time taken for each K
  duration.all[i] <- duration
}
# Stop time, which is put into a duration.total, 
# to see the total time it took to iterate through varying K
stop_time.total <- Sys.time()
duration.total <- stop_time.total - start_time.total

# Plot accuracies for each K
plot(K, accuracies, xlab = "Number of K", ylab = "Accuracy")
# Plot durations for each K
plot(K, duration.all, xlab = "Number of K", ylab = "Time (Seconds)")

###########################
###########################
##    Exercise 1.4.3     ##
###########################
###########################

# Create 90% / 10 % split (10 folds in total) using createFolds from caret package
folds <- createFolds(df_shuffle$X1, k = 10)

# K-Nearest Neigbours to look at
K.cross = 5

# k fold, not the same k as K in KNN
k = 1:10

# List for accuracies
accs <- c()

#for (j in K) {
for (i in k) {
  training <- df_shuffle[-folds[[i]],-1]
  training.labels <- df_shuffle[-folds[[i]],1]
  test <- df_shuffle[folds[[i]],-1]
  test.labels <- df_shuffle[folds[[i]],1]
  cross_pred <- knn(training, test, training.labels, k=K.cross)
  accs[i] <- getAccuracy(test.labels, cross_pred)
}
#}
plot(accs, xlab = "k folds", ylab = "Accuracies")

mean(accs)
sd(accs)

###########################
###########################
##    Exercise 1.4.4     ##
###########################
###########################
source("loadImage.R")

smoothImage <- function(grayImg){ # This function is run in loadSinglePersonsData check the code
  kernel <- makeBrush(9, shape='Gaussian', step=TRUE, sigma=0.6) # There exist a number of different functions
  
  print(kernel) # just to show what we have made
  smoothed <- filter2(grayImg, kernel) # filter the image using the kernel
  return(smoothed)
}

# Load images into a data frame for 100 DPI, group4 and member0
dataset.filtering <- loadSinglePersonsData(100, 4, 0, path)
df.filtering <- as.data.frame(dataset.filtering)

# Shuffle the dataset with a specific seed for recreatable reasons
set.seed(123)
df_shuffle.filtering <- df.filtering[sample(nrow(df.filtering)),]

# Smoothing example code, this can be included in your code and will change the code so remember to 
# load the images again, ( A nice feature of R )

# Create 90% / 10 % split (10 folds in total) using createFolds from caret package
folds <- createFolds(df_shuffle.filtering$V1, k = 10)

# K-Nearest Neigbours to look at
K.cross = 5

# k fold, not the same k as K in KNN
k = 1:10

# List for accuracies
accs <- c()

#
#for (j in K) {
for (i in k) {
  training <- df_shuffle.filtering[-folds[[i]],-1]
  training.labels <- df_shuffle.filtering[-folds[[i]],1]
  test <- df_shuffle.filtering[folds[[i]],-1]
  test.labels <- df_shuffle.filtering[folds[[i]],1]
  cross_pred <- knn(training, test, training.labels, k=K.cross)
  accs[i] <- getAccuracy(test.labels, cross_pred)
}
#}
plot(accs, xlab = "k folds", ylab = "Accuracies (With Smoothing)")

mean(accs)
sd(accs)

###########################
###########################
##    Exercise 1.4.5     ##
###########################
###########################

# Example code for reading all images into a list, DPI 100
getAllData <- function(dataList){
  id <- data.frame()
  idList <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        idTemp <- loadSinglePersonsData(100,i - 1,j,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  return(idList)
}

folder <- "/home/spider/Documents/software-engineering/sml/data/trunk/2018/group"


dataList <- list(list(1), list(1,2,3), list(),list(1,2,3), list(), list(), list(), list(1), list(1), list(1,2,3), 
                    list(), list(), list(), list(1,2), list(), list(1,2), list(1), list(), list(1,2,3))

idList <- getAllData(dataList)

# You can now iterate trough the list
for(i in 1:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
  id <- as.data.frame(rbind(id, idTemp))
}

#Dataset shuffle
datasetShuffle <- function(dataset){
  return(dataset[sample(nrow(dataset)),])
}

dataset.all <- as.data.frame(id)

dataset.all <- datasetShuffle(dataset.all)

train <- 1:40000

dataset.all.train <- dataset.all[train,-1]
dataset.all.test <- dataset.all[-train,-1]

dataset.all.train.labels <- factor(dataset.all[train,1])
dataset.all.test.labels <- factor(dataset.all[-train,1])

time.start <- Sys.time()
model <- knn(dataset.all.train, dataset.all.test, dataset.all.train.labels, 5)
time.end <- Sys.time()

duration.forall <- time.end - time.start
accuracy <- getAccuracy(model, dataset.all.test.labels)

sd(accuracy)
mean(accuracy)
var(accuracy)

### CASE 2 ###

data.train <- dataset.all[train,]
data.test <- dataset.all[-train,]

data.train <- datasetShuffle(data.train)
data.test <- datasetShuffle(data.test)

dataset.all.train <- data.train[,-1]
dataset.all.test <- data.test[,-1]

dataset.all.train.labels <- factor(data.train[,1])
dataset.all.test.labels <- factor(data.test[,1])

time.start <- Sys.time()
model <- knn(dataset.all.train, dataset.all.test, dataset.all.train.labels, 5)
time.end <- Sys.time()

duration.forall <- time.end - time.start
accuracy <- getAccuracy(model, dataset.all.test.labels)

sd(accuracy)
mean(accuracy)
var(accuracy)


###########################
###########################
##    Exercise 1.4.6     ##
###########################
###########################
