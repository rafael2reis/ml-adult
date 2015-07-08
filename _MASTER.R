rm(list = ls())

setwd('/Users/rafaelreis/workspace/r/MachineLearning')

# Create file system structure
dir.create('data')

# Load the Dataset asStrings
source("dataset.R")
LoadAdultDataset(asStrings = TRUE)

source("code/feature.R")
CreateFeatures(dataset = adult, label = "adult")
CreateFeatures(dataset = adult.test, label = "adult-test")

features <- read.csv("adult-features.csv", header = FALSE, 
                               stringsAsFactors = FALSE,
                               strip.white = TRUE)
features.test <- read.csv("adult-test-features.csv", header = FALSE, 
                                    stringsAsFactors = FALSE,
                                    strip.white = TRUE)

adult <- cbind(adult, features)
adult.test <- cbind(adult.test, features.test)

# Create de .ARFF files to be loaded in Weka
source("create_arff.R")
CreateArff(train = adult, test = adult.test, relation = 'adultf')

# Parse the Weka Ouput:
source("code/parse_output.R")
outputs <- c()
nbtree.output <- ParseOutput("resultados/NBTree_wp.txt")
rforest.output <- ParseOutput("resultados/RotationForest_wp.txt")
rforest1.output <- ParseOutput("resultados/RotationForest1_wp.txt")
outputs <- cbind(outputs, nbtree.output)
outputs <- cbind(outputs, rforest.output)
outputs <- cbind(outputs, rforest1.output)

committee <- c()

for (i in 1:nrow(outputs)) {
  over50K <- 0
  below50K <- 0
  for (j in 1:ncol(outputs)) {
    if ( outputs[[i, j]] == '>50K'  ) {
      over50K <- over50K + 1
    } else {
      below50K <- below50K + 1
    }
  }
  
  if(over50K > below50K) {
    committee[i] <- ">50K"
  } else {
    committee[i] <- "<=50K"
  }
}

actual <- read.table("output/actual.txt", stringsAsFactors = F)

classification <- actual == committee
accuracy <- length(classification[classification == TRUE, ])/length(classification)
