rm(list=ls())
library(randomForest)

source('dataset.R')
LoadAdultDataset()
str(adult)
str(adult.test)

# Drop NA's from train dataset
a <- adult[complete.cases(adult), ]
a <- na.roughfix(a)

# Drop NA's from test dataset
at <- adult.test[complete.cases(adult.test), ]
at <- na.roughfix(at)

# Group the train and test datasets
# Otherwise, the randomForest function throws an error
all <- rbind(a, at)

train <- 1:nrow(a)
test <- (nrow(a)+1):nrow(all)

# Bagging
bag.adult = randomForest(over50K ~ ., data = all, subset = train, mtry = 14, importance =TRUE)

# Predict using the created bag
# newdata is the test dataset, found in the lower part of all array
yhat.bag <- predict(bag.adult, newdata = all[test, ])
right <- at$over50K == yhat.bag

t <- table(right)
accuracy <- t[names(t) == TRUE] / length(right)
print(paste("Bagging accuracy:", accuracy))

# Random Forest
rf <- randomForest(over50K ~ ., data = all, subset = train, mtry = 4, importance =TRUE)

yhat.rf <- predict(rf, newdata = all[test, ])
right <- at$over50K == yhat.rf

t <- table(right)
accuracy <- t[names(t) == TRUE] / length(right)
print(paste("Random Forest accuracy:", accuracy))

# Boosting
install.packages("gbm")
library(gbm)

# Creat new attribute to use in boosting function
all$over50K2 <- 0
all$over50K2[all$over50K == levels(all$over50K)[2]] <- 1

at$over50K2 <- 0
at$over50K2[at$over50K == levels(at$over50K)[2]] <- 1

boost <- gbm(over50K2~. - over50K, data = all[train, ], distribution = "bernoulli", 
             n.trees = 5000, interaction.depth = 4)
yhat.boost <- predict(boost, newdata = all[test, ], n.trees=5000)
response <- ifelse(yhat.boost>0.5, 1, 0)
right <- at$over50K2 == response

t <- table(right)
accuracy <- t[names(t) == TRUE] / length(right)
print(paste("Boosting accuracy:", accuracy))
