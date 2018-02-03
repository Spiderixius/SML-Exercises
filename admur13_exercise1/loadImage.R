#-------------------------------------------------------------
#load libraries
#-------------------------------------------------------------
#library("png")
library("jpeg")
library("EBImage")
library("class")
library("gmodels")
library("ggplot2")
library("caret")
#library("lda")
#library("neuralnet")
#library("RSNNS")
#install.packages("neuralnet")


#This file contains 2 functions and some example code in the bottom
#for using the 2 functions.
#The example code use the functions to load and smoothen the image data.
#lastly it converts the data into a table more suitable for R based classification 


#-------------------------------------------------------------
#Smoothing function (you are welcome to use alternative functions from R)
#-------------------------------------------------------------
smoothImage <- function(grayImg){
  #two ways of specifying kernel:
  # kernel <- matrix( 
  #           c(1, 1, 1, 
  #             1, 1, 1, 
  #             1, 1, 1), # the data elements 
  #           3,              # number of rows 
  #           3)
  # kernel <- kernel/9
  # kernel
  kernel <- matrix( 
    1, # the data elements 
    3,# number of rows 
    3)
  kernel <- kernel/9
  #print(kernel)
  
  #using r library for smoothing
  smoothed <- filter2(grayImg, kernel)
  
  #simple implementation of average filter:
  # imgWidth <- length(gray[1,])
  # imgHeight <- length(gray[,1])
  # kernelSize <- 1
  # for(px in 1:imgWidth)
  # {
  #   for(py in 1:imgHeight)
  #   {
  #     baseX <- px - kernelSize
  #     endX <- px + kernelSize
  #     if(baseX < 1){baseX<-1}
  #     if(endX > imgWidth){endX<-imgWidth}
  #     
  #     baseY <- py - kernelSize
  #     endY <- py + kernelSize
  #     if(baseY < 1){baseY<-1}
  #     if(endY > imgHeight){endY<-imgHeight}
  #     
  #     
  #     value <- 0
  #     for(pkx in baseX:endX)
  #     {
  #       for(pky in baseY:endY)
  #       {
  #         value <- value+gray[pky,pkx]
  #       }
  #     }
  #     kernelValues <- (endY-baseY+1)*(endX-baseX+1)    
  #     value <- value/kernelValues
  #     
  #     smoothed[py,px] <- value
  #   }
  # }
  return(smoothed)
}


#-------------------------------------------------------------
#Data loading function.
#This function load a single dataset where the data is
#specified by the DPI of the scanned images, the group number of the person
#and the number of the group member.
#Then the function use the smoothing function above, in order to smoothen the data.
#The function then places a grid around the digit cells, based on the corner file,
#so that it can be verified the data is read in a reasonable maner.
#Lastly a vector of table is returned. Each table in this vector represents
#a single type of digit, so the first table is only 0's, the second is 1's and so on.
#In the tables the rows represent the individual handwritten digits.
#The columns represents the pixel values.
#-------------------------------------------------------------
loadSinglePersonsData <- function(DPI,groupNr,groupMemberNr,folder){
  #load the scaned images

  # ciffers <- list(readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
  #                 readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
  #                 readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
  #                 readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
  #                 readPNG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
  
  ciffers <- list(readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.jpeg"), collapse = "")),
                  readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.jpeg"), collapse = "")),
                  readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.jpeg"), collapse = "")),
                  readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.jpeg"), collapse = "")),
                  readJPEG(paste(c(folder,groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.jpeg"), collapse = "")))
  
  #load the corner values
  corners <- read.csv(paste(c(folder,groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
  corners <- trunc(corners*DPI/300)
  #print(corners)
  
  #define lists to be used
  #  gray <- list(1:5)
  #   smoothed <- list(1:5)
  prepared <- list(1:5)

  #convert the images to gray scale.
  for(i in 1:5)
  {
    if( length(dim(ciffers[[1]]) ) == 3 ) {
      r <-ciffers[[i]][,,1]
      g <-ciffers[[i]][,,2]
      b <-ciffers[[i]][,,3]
      prepared[[i]] <- (r+g+b)/3
    } else {
      prepared[[i]] <- ciffers[[i]]
    }  
  }
  
  #smooth images based on the funtion in the top
  for(i in 1:5)
  {
    prepared[[i]] <- smoothImage(prepared[[i]])
  }  
  
  #extract individual ciffers
  #xStep and yStep is used to ensure the first corner of the
  #individual ciffers are placed fairly accurate
  xStep  <- (corners[1,7]-corners[1,1])/20;
  yStep  <- (corners[1,8]-corners[1,2])/20;
  
  #xStepT and yStepT is used to ensure that the feature vectors
  #from all people have the same size.

  xStepT <- 60*DPI/300
  yStepT <- 60*DPI/300
  
  dataMatrix <- matrix(1:((xStepT-2)*(yStepT-2) + 1)*10*20*20, nrow=10*20*20, ncol=(xStepT-2)*(yStepT-2) + 1)

  for(pages in 1:5)
  {
    for(box in 1:2)
    {
      for(cifX in 1:20)
      {
        aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
        for(cifY in 1:20)
        {
          aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifY-1)
         
          dataMatrix[((pages-1)*2 + box - 1)*20*20 + (cifY-1)*20 + cifX ,1 ] <- (pages-1)*2 + box - 1
          
          for(px in 1:(xStepT-2))
          {
            for(py in 1:(yStepT-2))
            {
              #here things are read in
              dataMatrix[((pages-1)*2 + box - 1)*20*20 + (cifY-1)*20 + cifX ,1 + (px-1)*(yStepT-2) + py ] <- prepared[[pages]][aYbase+py+1,aXbase+px+1]
              
            }
          }
        }
      }
    }
  }
  
  return(dataMatrix)
}

