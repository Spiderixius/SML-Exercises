#KNN Assignment
library("gmodels")

#source('C:/Users/Bruger/Desktop/SM-Exercises/loadImage.R', echo=TRUE) #This contains Smoothing override method
source('C:/Users/Bruger/Desktop/Statistical Mashine Learning/BaseFolder/loadImage.R')

directory <- 'C:/Users/Bruger/Desktop/Statistical Mashine Learning/2018/group';

dataset <- loadSinglePersonsData(100,4,0,directory)
dataset <- as.data.frame(dataset)

#normalize(dataset)

dataset <- datasetShuffle(dataset)

#Data split 50-50
train <- 1:2000

data.train <- dataset[train,-1]
data.test <- dataset[-train,-1]

data.train.labels <- factor(dataset[train,1])
data.test.labels <- factor(dataset[-train,1])

####### Exercise 1.4.1 ######
knn.model <- knn(data.train, data.test, data.train.labels,3)

acc(knn.model, data.test.labels)

###### Exercise 1.4.2 ######

k <- 1:20
speed <- list()
accuracy_list <- list() 
for (i in k){
  time.start <- Sys.time()
  model <- knn(data.train, data.test, data.train.labels,i)
  time.end <- Sys.time()
  
  time.taken <- time.end - time.start
  
  speed[i] <- time.taken
  accuracy_list[i] <- acc(model, data.test.labels)
  
}

plot(k,speed, xlab = "Number of K", ylab = "Time (seconds)")
plot(k,accuracy_list, xlab = "Number of K", ylab = "Accuracy")
#plot speed and accuracy

###### Exercise 1.4.3 ######
speed.cross <- list()
speed.cross.overall <- list()

folds <- createFolds(dataset$V1, 10)

accuracy_cross <- list()
accuracy_cross.overall <- list()

a <- list()
s <- list()

for(i in k){
  for(j in 1:length(folds)){
    cross.test <- dataset[folds[[j]],-1]
    cross.train <- dataset[-folds[[j]],-1]
    
    cross.train.labels <- factor(dataset[-folds[[j]],1])
    cross.test.labels <- factor(dataset[folds[[j]],1])
    
    time.start <- Sys.time()
    model <- knn(cross.train, cross.test, cross.train.labels,i)
    time.end <- Sys.time()
    
    time.taken <- time.end - time.start
    
    s[j] <- time.taken
    a[j] <- acc(model, cross.test.labels) #accuracy
    
  }
  speed.cross.overall[i] <- mean(s)
  accuracy_cross.overall[i] <- mean(a)
  
  a <- 0
  s <- 0
}

#plot the speed.cross and accuracy_cross
plot(k, speed.cross.overall, xlab = "Number of K", ylab = "time (seconds)")
plot(k, accuracy_cross.overall, xlab = "Number of K", ylab = "Accuracy")

###### Exercise 1.4.4 ######
#Use a gaussian blur - gblur(image, sigma) -  OVERRIDE FREDERIK'S METHOD IN 'loadImage.R'!!!!!
#Use a new dataset variable for the new blurred images


dataset.gau <- loadSinglePersonsData(100,4,3,directory)
dataset.gau <- data.frame(dataset)


dataset.gau -> datasetShuffle(dataset.gau)


#do a cross validation again
#use a number of k's
speed.cross.gau <- list()
folds <- createFolds(dataset.gau$V1, 10)
accuracy_cross.gau <- list()

for(i in k){
  for(j in folds){
    cross.test.gau <- dataset.gau[folds[[j]],-1]
    cross.train.gau <- dataset.gau[-folds[[j]],-1]
    
    cross.train.gau.labels <- dataset.gau[-folds[[j]],1]
    cross.test.gau.labels <- dataset.gau[folds[[j]],1]
    
    time.start <- Sys.time()
    model <- knn(cross.train.gau, cross.test.gau, cross.train.gau.labels,i)
    time.end <- Sys.time()
    
    time.taken <- time.end - time.start
    
    s[j] <- time.taken
    a[j] <- acc(model, cross.test.gau.labels) #accuracy
    
  }
  speed.cross.gau[i] <- mean(s)
  accuracy_cross.gau[i] <- mean(a)
}

#Plot the results
plot(k, speed.cross.gau, xlab = "Number of K", ylab = "time (seconds)")
plot(k, accuracy_cross.gau, xlab = "Number of K", ylab = "Accuracy")

###### Exercise 1.4.5 ######
#REMEBER TO UPDATE THE FOLDER
#REMEBER TO RUN METHOD - getAllData

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


folder<- "C:/Users/Bruger/Desktop/Statistical Mashine Learning/preProcessed/2018/group"

datalist <- list(  list( 1) ,list( 1, 2 ), list( 1, 2, 3 ),   list( 1, 2, 3 ), list( 1, 0, 4, 2, 3 ), 
                   list( 1, 5, 4, 2, 3 ), list( 0, 2, 3 ), list( 1 ), list( 1, 2, 3 ), list( 1, 2, 3 ), 
                   list( 1, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ), list( 1, 2 ), list( 1, 2, 3 ), 
                   list( 1, 2 ), list( 1, 4, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ))

idList <- getAllData(datalist)

for(i in 1:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
  id <- as.data.frame(rbind(id,idTemp))
}

dataset.all <- as.data.frame(id)

#### IF YOU WANT TO DO THE EXERCISE ACCORDING TO INDIVIDUALS, THEN SPLIT THE DATA BEFORE SHUFFLING ####
#### REMEMBER TO UNCOMMENT THE DATA SPLIT FOR INDIVIDUALS ####
train <- 1:40000

# data.train <- dataset.all[train,]
# data.test <- dataset.all[-train,]
# 
# data.train <- datasetShuffle(data.train)
# data.test <- datasetShuffle(data.test)
# 
# dataset.all.train <- data.train[,-1]
# dataset.all.test <- data.test[,-1]
# 
# dataset.all.train.labels <- factor(data.train[,1])
# dataset.all.test.labels <- factor(data.test[,1])

dataset.all <- datasetShuffle(dataset.all)

dataset.all.train <- dataset.all[train,-1]
dataset.all.test <- dataset.all[-train,-1]

dataset.all.train.labels <- factor(dataset.all[train,1])
dataset.all.test.labels <- factor(dataset.all[-train,1])

time.start <- Sys.time()
model <- knn(dataset.all.train, dataset.all.test, dataset.all.train.labels,5)
time.end <- Sys.time()

print(time.taken <- time.end - time.start)

acc(model, dataset.all.test.labels)








