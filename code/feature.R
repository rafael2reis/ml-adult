#rm(list = ls())
#setwd('/Users/rafaelreis/workspace/r/MachineLearning')

# Load the Dataset asStrings
# source("dataset.R")
# LoadAdultDataset(asStrings = TRUE)
# dataset <- adult
# i <- 2
# label <- "adult"

CreateFeatures <- function(dataset = NULL, label = NULL) {
  features <- c()
  numLinhas <- nrow(dataset)
  for (i in 1:numLinhas) {
    j <- 1
    linha <- c()
    age <- dataset[i, ]$age
    workclass <- dataset[i, ]$workclass
    fnlwgt <- dataset[i, ]$fnlwgt
    education <- dataset[i, ]$education
    educationnum <- dataset[i, ]$educationnum
    maritalstatus <- dataset[i, ]$maritalstatus
    occupation <- dataset[i, ]$occupation
    relationship <- dataset[i, ]$relationship
    race <- dataset[i, ]$race
    sex <- dataset[i, ]$sex
    capitalgain <- dataset[i, ]$capitalgain
    capitalloss <- dataset[i, ]$capitalloss
    hoursperweek <- dataset[i, ]$hoursperweek
    nativecountry <- dataset[i, ]$nativecountry
    
    
    if (capitalgain <= 6849) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum <= 12) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum <= 12 & capitalloss <= 1980) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum <= 12 & capitalloss > 1980) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum <= 12 & capitalloss > 1980 & capitalloss <= 2377) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum <= 12 & capitalloss > 1980 & capitalloss > 2377) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum <= 14) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age <= 32) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Adm-clerical") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Exec-managerial") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Handlers-cleaners") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "State-gov") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Self-emp-not-inc") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Private") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Private" & age <= 52) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Private" & age <= 52 & hoursperweek <= 42) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Private" & age <= 52 & hoursperweek > 42) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Private" & age <= 52 & hoursperweek > 42 & sex == "Male") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Private" & age <= 52 & hoursperweek > 42 & sex == "Female") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Private" & age > 52) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Federal-gov") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Local-gov") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Self-emp-inc") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Prof-specialty" & workclass == "Without-pay") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Other-service") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Sales") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Transport-moving") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Farming-fishing") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Machine-op-inspct") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Tech-support") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Craft-repair") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Protective-serv") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Armed-Forces") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss <= 653 & occupation == "Priv-house-serv") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Never-married" & educationnum > 12 & educationnum > 14 & age > 32 & capitalloss > 653) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum <= 8) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum <= 8 & capitalgain <= 2993) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum <= 8 & capitalgain > 2993) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum <= 8 & capitalgain > 2993 & capitalgain <= 5060) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum <= 8 & capitalgain > 2993 & capitalgain <= 5060 & capitalgain <= 3103) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum <= 8 & capitalgain > 2993 & capitalgain <= 5060 & capitalgain > 3103) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum <= 8 & capitalgain > 2993 & capitalgain > 5060) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age <= 35) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek <= 34) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Bachelors") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "HS-grad") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "HS-grad" & age <= 45) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "HS-grad" & age > 45) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "HS-grad" & age > 45 & age <= 49) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "HS-grad" & age > 45 & age > 49) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "11th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Masters") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "9th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Some-college") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Some-college" & fnlwgt <= 86310) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Some-college" & fnlwgt > 86310) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Assoc-acdm") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "7th-8th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Doctorate") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Assoc-voc") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Prof-school") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "5th-6th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "10th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "Preschool") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "12th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "State-gov" & education == "1st-4th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Self-emp-not-inc") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek <= 43) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "Bachelors") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "HS-grad") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "HS-grad" & fnlwgt <= 163847) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "HS-grad" & fnlwgt > 163847) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "11th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "Masters") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "9th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "Some-college") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "Some-college" & age <= 39) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "Some-college" & age > 39) {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "Assoc-acdm") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    if (capitalgain <= 6849 & maritalstatus == "Married-civ-spouse" & capitalloss <= 1762 & educationnum <= 12 & educationnum > 8 & capitalgain <= 5060 & age > 35 & hoursperweek > 34 & capitalloss <= 1504 & occupation == "Adm-clerical" & workclass == "Private" & sex == "Male" & hoursperweek <= 53 & hoursperweek > 43 & education == "7th-8th") {
      linha[j] <- 1
    } else {
      linha[j] <- 0
    }
    j <- j + 1
    
    
    nova.linha <- paste(linha, collapse = ",")
    write.table( x = nova.linha, file = paste0(label, '-features.csv'), 
                 row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
    
    rm(nova.linha, linha, age, workclass, fnlwgt, education, educationnum, maritalstatus, occupation,
      relationship, race, sex, capitalgain, capitalloss, hoursperweek, nativecountry)
  }
}