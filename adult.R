rm(list = ls())
require(ggplot2)
library('rpart')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Lê os dados
# De treino
adult <- read.csv("adult.data.csv", header = FALSE, na.strings = "?")
str(adult)
colnames <- c("age",
                "workclass",
                "fnlwgt",
                "education",
                "educationnum",
                "maritalstatus",
                "occupation",
                "relationship",
                "race",
                "sex",
                "capitalgain",
                "capitalloss",
                "hoursperweek",
                "nativecountry",
                "over50K")
names(adult) <- colnames

# De teste
adult.test <- read.csv("adult.test.csv", header = FALSE, na.strings = "?")
names(adult.test) <- colnames

# Análise dos dados
# Tabela de proporções
prop.table(table(adult$over50K))

# Decision Tree
outcome <- 'over50K'
selVars <- setdiff(colnames, c(outcome))

formula <- paste(outcome,' ~ ', paste(selVars, collapse=' + '), sep='')
tmodel <- rpart(formula, data = adult, method="class")
printcp(tmodel)

# Calculando a Matriz de Confusão e a Acurácia
pred <- predict(tmodel, newdata = adult.test)
respostas <- levels(adult.test$over50K)
adult.test$pred <- ifelse(pred[, 2]>0.5, respostas[2], respostas[1])

confusion.matrix <- table(truth=adult.test$over50K, prediction=adult.test$pred)
accuracy <- nrow(adult.test[adult.test$over50K == adult.test$pred, ])/nrow(adult.test)

# Um melhor Plot para a Decision Tree
fancyRpartPlot(tmodel)
