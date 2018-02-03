# Source the loadImage.R script in order to load images
source("loadImage.R")
# Good old OOP, never disappoints
library(class)

# Load images into a data frame for 100 DPI, group4 and member0
data <- loadSinglePersonsData(100, 4, 0, "C:/Users/spider/Documents/Software Engineering/8th Semester/SML/admur13_exercise1/group")
df <- data.frame(data)

# Shuffle the dataset with a specific seed for recreatable reasons.
set.seed(123)
df_shuffle <- df[sample(nrow(df)),]

# 50% Training as specified in the exercise, simply take the first 2000 elements and use them for training and add the labels (0-9)
training_set <- df_shuffle[1:2000, -1]
training_labels <- df_shuffle[1:2000,1]
# 50% Test as specified in the exercise, which will be the last 2000 elements 
test_set <- df_shuffle[2001:4000, -1]
test_labels <- df_shuffle[2001:4000, 1]

# Time to test the training data against the test set with a K = 5
probability <- knn(training_set, test_set, training_labels, 5)

# Function provided by Frederik/Nörbert to get the accuracy of the test.
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
getAccuracy(test_labels, probability)

# Lets have a collection of accuracies for varying K's
accuracies <- c()

for (i in 1:10) {
  probability <- knn(training_set, test_set, training_labels, i)
  accuracies <- append(accuracies, c(i, getAccuracy(test_labels, probability)))
}

accuracies
