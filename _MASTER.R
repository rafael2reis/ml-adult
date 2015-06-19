rm(list = ls())

source("dataset.R")

# Create de .ARFF files to be loaded in Weka
LoadAdultDataset(asStrings = TRUE)
CreateArff(train = adult, test = adult.test, relation = 'adult')