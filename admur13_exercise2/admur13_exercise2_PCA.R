
#Source image read
source("loadImage.R")
source("Methods.R")
source("loadAll.R")

#Get all data from folder
getAllData <- function(dataList){
  id <- data.frame()
  idList <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        idTemp <- loadSinglePersonsData(100,i - 1, dataList[[i]][j] ,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  return(idList)
}

folder<- "../../../data/trunk/preProcessed/2018/group"

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

###########################
###########################
##      Exercise 2.1     ##
###########################
###########################

##### Exercise 2.1.1 #####
#Split data 50/50

#Disjunct - 57 members in total
# train <- 1:116000 # 29 persons
# dataset.train <- dataset.all[train,]
# dataset.test <- dataset.all[-train,]
# 
# dataset.train <- datasetShuffle(dataset.train)
# dataset.test <- datasetShuffle(dataset.test)
# 
# dataset.train.labels <- factor(dataset.train[,1])
# dataset.test.labels <- factor(dataset.test[,1])
# 
# dataset.train <- dataset.train[,-1]
# dataset.test <- dataset.test[,-1]


# #All persons
dataset.all <- datasetShuffle(dataset.all)

#Split data
train <- 1:(nrow(dataset.all)/2)

dataset.train <- dataset.all[train,-1]
dataset.test <- dataset.all[-train,-1]

dataset.train.labels <- factor(dataset.all[train,1])
dataset.test.labels <- factor(dataset.all[-train,1])

PCA.obj <- prcomp(dataset.train)

##### Exercise 2.1.2 #####
#plot results of PCA object
require(graphics)

PropVariancePCA <- (PCA.obj$sdev^2)/sum(PCA.obj$sdev^2) #Part of variacne as a whole. 
CumuPCA <- cumsum(PCA.obj$sdev^2)/sum(PCA.obj$sdev^2) #Total Variance

#Sdev
plot(main = "Standard Deviation of Principle Components", PCA.obj$sdev[1:20], xlab = "Principle Components", ylab = "Standard Deviations", col = "blue", pch = 4, cex = 2, lwd=3)
lines(PCA.obj$sdev[1:20], col= "red", lwd = 3)

#Proportion of Variance & cumulative proportion of variance
plot(main = "Proportion of Variance", PropVariancePCA[1:20], xlab = "Principle Components", ylab = "Proportion of Variance", col = "blue", pch = 4, cex = 2, lwd=3)
lines(PropVariancePCA[1:20], col= "red", lwd = 3)

plot(main = "Cumulative Proportion of Variance", CumuPCA[1:20], xlab = "Principle Components", ylab = "Cumulative Proportion of Variance", col = "blue", pch = 4, cex = 2, lwd=3)
lines(CumuPCA[1:20], col= "red", lwd = 3)

##### Exercise 2.1.3 and Exercise 2.1.4 #####
#Prepare for knn process - Best k values are 3, 5 and 7 from previous assignment (KNN)
#Code example https://www.kaggle.com/victorzhang/pca-knn-with-r

#PC's representing 80 % of the accumulated variance 
PC.80 <- PCA.obj$x[, CumuPCA < 0.8] # Results in 14 PCs

#PC's representing 90 % of the accumulated variance
PC.90 <- PCA.obj$x[, CumuPCA < 0.9] #Results in 23 PCs

#PC's representing 95 % of the accumulated variance
PC.95 <- PCA.obj$x[, CumuPCA < 0.95] # Results in 34 PCs

#PC's representing 99 % of the accumulated variance
PC.99 <- PCA.obj$x[, CumuPCA < 0.99] # Results in 72 PCs

#The 'train.col.used' must be opdated depending on the number of PCs for specific percentage of variance
numberOfPCs <- 1:14

#Get train and test data from PCA object
train.pca <- PCA.obj$x #x = scores vector
test.pca <- predict(PCA.obj, dataset.test) #Finding Principle Components in test

time.start <- Sys.time()
model <- knn(train.pca[,numberOfPCs], test.pca[,numberOfPCs], dataset.train.labels,5)
time.end <- Sys.time()

#Run time
print(time.end-time.start)

#Performance
acc(model, dataset.test.labels)

###########################
###########################
##      Exercise 2.2     ##
###########################
###########################

##### Exercise 2.2.1 #####
#Normalization After PCA 
#If Normalization has to happen before PCA is run, normalize the dataset before that.
numberOfPCs <- 1:14 #The number of PCs.
PCA.dataset <- normalize(PCA.obj$x) #The best Dataset from Exercise 2.1.3 (95 % of accumulated variance with k-value of 5)

folds <- createFolds(PCA.dataset, 10)

a <- list()
s <- list()

for(i in 1:length(folds)){
  cross.train <- PCA.dataset[-folds[[i]],]
  cross.train <- cross.train[,numberOfPCs]
  #cross.test <- PCA.dataset[folds[[i]],]
  
  cross.train.labels <- dataset.train.labels[-folds[[i]]] ####################  SPØRGER FREDERIK
  
  test <- dataset.test
  cross.test <- predict(PCA.obj,test)
  
  #Run KNN algorithm
  time.start <- Sys.time()
  model <- knn(cross.train, cross.test[,numberOfPCs], cross.train.labels, k=5)
  time.end <- Sys.time()
  
  s[i] <- time.end - time.start
  a[i] <- acc(model, normalize(dataset.test.labels)) #accuracy
  
}

mean(a)
mean(s)

###########################
###########################
##      Exercise 2.3     ##
###########################
###########################

##### Exercise 2.3.1 #####
DPI <- 100
group <- 4
member <- 0
folder <- "../../../data/trunk/preProcessed/2018/group"

cipherNumber <- 1222

rotateSelf <- function(x) t(apply(x, 2, rev))

id <- loadSinglePersonsData(DPI,group,member,folder)
imageSize <- sqrt(ncol(id) - 1)
imageM <- matrix( id[cipherNumber,2:ncol(id)],nrow =
                    imageSize,ncol = imageSize,byrow = FALSE)

imageM <- rotateSelf(imageM) # rotate is a function to rotate the image

image(imageM)

##### Exercise 2.3.2 #####
# Plot the first 10 eigenvectors and plot as images
eigenVector <- 1:10

#PCA.obj$rotation[,eigenVector]
for (e in eigenVector) {
  imageNewM <- matrix(PCA.obj$rotation[,e],nrow = imageSize,ncol = imageSize,byrow = FALSE)
  image(imageNewM)
}

##### Exercise 2.3.3 #####

##### Exercise 2.3.4 #####

##### Exercise 2.3.5 #####

