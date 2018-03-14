
#Source image read
source("loadImage.R")
source("Methods.R")
#source("loadAll.R")

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

#folder<- "../../../data/trunk/preProcessed/2018/group"
#folder <- "C:/Users/spider/Documents/StatisticML/preProcessed/2018/group"
folder <- "c:/Users/Anna/svn/trunk/preProcessed/2018/group"

datalist <- list( list( 1 ) ,list( 1, 2 ), list( 1, 2, 3 ),   list( 1, 2, 3 ), list( 1, 0, 4, 2, 3 ), 
                  list( 1, 5, 4, 2, 3 ), list( 0, 2, 3 ), list( 1 ), list( 1, 2, 3 ), list( 1, 2, 3 ), 
                  list( 1, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ), list( 1, 2 ), list( 1, 2, 3 ), 
                  list( 1, 2 ), list( 1, 4, 2, 3 ), list( 1, 4, 2, 3 ), list( 1, 2, 3 ))

idList <- getAllData(datalist)
id <- c()
for(i in 1:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
  id <- as.data.frame(rbind(id,idTemp))
}

dataset <- as.data.frame(id)

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
dataset.all <- datasetShuffle(dataset)

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
  
  cross.train.labels <- dataset.train.labels[-folds[[i]]] ####################  SPÃRGER FREDERIK
  
  test <- dataset.test
  cross.test <- predict(PCA.obj,test)
  
  #Run KNN algorithm
  time.start <- Sys.time()
  model <- knn(cross.train, cross.test[,numberOfPCs], cross.train.labels, k=5)
  time.end <- Sys.time()
  
  s[i] <- time.end - time.start
  a[i] <- acc(model, normalize(dataset.test)) #accuracy
  
}

mean(a)
mean(s)

###########################
###########################
##      Exercise 2.3     ##
###########################
###########################

PCA.obj <- prcomp(dataset[train,-1])

##### Exercise 2.3.1 #####
DPI <- 100
group <- 4
member <- 0
#folder <- "../../../data/trunk/preProcessed/2018/group"
#folder <- "C:/Users/spider/Documents/StatisticML/preProcessed/2018/group"

rotateSelf <- function(x) t(apply(x, 2, rev))
id <- loadSinglePersonsData(DPI,group,member,folder)
cipherImage <- function(cipherNumber) {
  imageM <- matrix( id[cipherNumber,2:ncol(id)],nrow =
                      imageSize,ncol = imageSize,byrow = FALSE)
  imageM <- rotateSelf(imageM) # rotate is a function to rotate the image
  image(imageM) 
}

# cipher 0
cipherImage(100)
# cipher 1
cipherImage(500)
# cipher 2
cipherImage(1001)
# cipher 3
cipherImage(1201)
# cipher 4
cipherImage(1601)
# cipher 5
cipherImage(2201)
# cipher 6
cipherImage(2601)
# cipher 7
cipherImage(3001)
# cipher 8
cipherImage(3401)
# cipher 9
cipherImage(3800)

##### Exercise 2.3.2 #####
# Plot the first 10 eigenvectors and plot as images
eigenVector <- 1:10

#PCA.obj$rotation[,eigenVector]
for (e in eigenVector) {
  imageNewM <- matrix(PCA.obj$rotation[,e],nrow = imageSize,ncol = imageSize,byrow = FALSE)
  image(imageNewM)
}
#image(matrix(PCA.obj$rotation[,1],nrow = imageSize,ncol = imageSize,byrow = FALSE))

##### Exercise 2.3.3 #####
reconstructed <- function(cipherNumber) {
  trunc <- PCA.obj$x[cipherNumber,1:nrow(PCA.obj$rotation)] %*%
    t(PCA.obj$rotation[,1:nrow(PCA.obj$rotation)])
  trunc <- scale(trunc, center = -1 * PCA.obj$center, scale=FALSE)
  testPic <- matrix(trunc,nrow=imageSize,ncol=imageSize,byrow=FALSE)
  testPic <- rotateSelf(testPic)
  image(testPic) 
}
# cipher 0
reconstruct(100)
# cipher 1
reconstruct(500)
# cipher 2
reconstruct(1001)
# cipher 3
reconstruct(1201)
# cipher 4
reconstruct(1601)
# cipher 5
reconstruct(2201)
# cipher 6
reconstruct(2601)
# cipher 7
reconstruct(3001)
# cipher 8
reconstruct(3401)
# cipher 9
reconstruct(3800)

##### Exercise 2.3.4 #####
cumPic <- function(noPCA, cipherNumber) {
  trunc <- PCA.obj$x[cipherNumber,1:noPCA] %*%
    t(PCA.obj$rotation[,1:noPCA])
  trunc <- scale(trunc, center = -1 * PCA.obj$center, scale=FALSE)
  testPic <- matrix(trunc,nrow=imageSize,ncol=imageSize,byrow=FALSE)
  testPic <- rotateSelf(testPic)
  image(testPic)  
}
cumPic(14, 1001)
cumPic(23, 1001)
cumPic(34, 1001)
cumPic(14, 1601)
cumPic(23, 1601)
cumPic(34, 1601)

##### Exercise 2.3.5 #####
cipherNumber <- 43
cumPic(10)
plot(PCA.obj$x[cipherNumber,1:10], ylim=c(-1,0.5), col="red")
cipherNumber <- 456
cumPic(10)
plot(PCA.obj$x[cipherNumber,1:10])
points(PCA.obj$x[cipherNumber,1:10], col="green")
biplot(PCA.obj[1:10])
?prcomp
summary(PCA.obj)

# cipher 2
cipherNumber <- 1001
PCA.obj$x[cipherNumber, 1:10]
# cipher 0
cipherNumber <- 400
PCA.obj$x[cipherNumber, 1:10]
#Mean
# cipher 2
no <- 1:10
result2 <- c()
result2_2 <- c()
for (a in no) {
  result2[a] <- mean(PCA.obj$x[801:1200, a]) 
  result2_2[a] <- mean(PCA.obj$rotation[, a])
}
result2
#result2_2

# cipher 0
result1 <- c()
for (a in no) {
  result1[a] <- mean(PCA.obj$x[1:400, a])  
}
result1
