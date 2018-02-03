# Accuracy code, set labels and prediction as input
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


# Smoothing example code, this can be included in your code and will change the code so remember to 
# load the images again, ( A nice feature of R )

smoothImage <- function(grayImg){ # This function is run in loadSinglePersonsData check the code
  kernel <- makeBrush(9, shape='Gaussian', step=TRUE, sigma=0.8) # There exist a number of different functions
  print(kernel) # just to show what we have made
  smoothed <- filter2(grayImg, kernel) # filter the image using the kernel
  return(smoothed)
}



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
# Set a "list of list" for each group and member and run
folder <- "/home/frederik/workspace/svn/trunk/preProcessed/2017/group"

dataList <- list( list(1,2,3), list(1,2,3), list(1,2,3), list(1,2), list(1,2,3), list(1,2,3), list(1), list(1), list(1), list(), list(1,2), list(1,2,3), list(), list(1)  )
idList <- getAllData(dataList)

# You can now iterate trough the list
for(i in 1:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
}
# you can combine the data frames using "id <- rbind(id, idTemp)"