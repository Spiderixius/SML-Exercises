getAllData <- function(dataList){
  id <- data.frame()
  idList <- list()
  print(length(dataList))
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in dataList[[i]] ){
        print( i )
        print( j )
        idTemp <- loadSinglePersonsData(100,i - 1, j ,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  return(idList)
}
# Set a "list of list" for each group and member and run
folder <- "/home/frederik/undervisning/2018/script/preprocessed/group"
folder <- "/home/frederik/workspace/svn/trunk/preProcessed/2018/group"

dataList <- list(   list( 1 ),   list( 1, 2 ),   list( 1, 2, 3 ),   list( 1, 2, 3 ),   list( 1, 0, 4, 2, 3 ),   list( 1, 5, 4, 2, 3 ),   list( 0, 2, 3 ),   list( 1 ),   list( 1, 2, 3 ),   list( 1, 2, 3 ),   list( 1, 2, 3 ),   list( 1, 4, 2, 3 ),   list( 1, 2, 3 ),   list( 1, 2 ),   list( 1, 2, 3 ),   list( 1, 2 ),   list( 1, 4, 2, 3 ),   list( 1, 4, 2, 3 ),   list( 1, 2, 3 ),   list(  ),   list( 1, 2 )   )
idList <- getAllData(dataList)

id <- idList[[1]]
# You can now iterate trough the list
for(i in 2:length(idList)){
  idTemp <- idList[[i]]
  id <- rbind(id, idTemp)
}

dim(id)