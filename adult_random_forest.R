rm(list=ls())
library(randomForest)

source('dataset.R')
LoadAdultDataset()
str(adult)
str(adult.test)

a <- adult[complete.cases(adult), ]
a <- na.roughfix(a)

at <- adult.test[complete.cases(adult.test), ]
at <- na.roughfix(at)

all <- rbind(a, at)

column.test <- at[, "over50K"]

# Bagging
bag.adult = randomForest(over50K ~ age + workclass + fnlwgt + educationnum + maritalstatus + occupation + relationship + race + sex + capitalgain + capitalloss + hoursperweek + nativecountry + education
                         , data = all, subset = 1:nrow(a), mtry = 14, importance =TRUE)

yhat.bag <- predict(bag.adult, newdata = all[(nrow(a)+1):nrow(all), ])
right <- at$over50K == yhat.bag

t <- table(right)
accuracy <- t[names(t) == TRUE] / length(right)
