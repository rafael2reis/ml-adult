rm(list = ls())

setwd('/Users/rafaelreis/workspace/r/MachineLearning')

# Create file system structure
dir.create('data')

# Load the Dataset asStrings
source("dataset.R")
LoadAdultDataset(asStrings = TRUE)

# Create de .ARFF files to be loaded in Weka
source("create_arff.R")
CreateArff(train = adult, test = adult.test, relation = 'adult')
