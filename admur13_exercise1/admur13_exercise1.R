# Source the loadImage.R script in order to load images
source("loadImage.R")
# Good old OOP, never disappoints
library(class)

# Load images into a data frame
data <- loadSinglePersonsData(100, 4, 0, "C:/Users/spider/Documents/Software Engineering/8th Semester/SML/admur13_exercise1/group")
df <- data.frame(data)
