install.packages("rJava")
install.packages("RWekajars")
install.packages("RWeka")

library("rJava")
library("RWekajars")
library("RWeka")

WPM("refresh-cache")
WPM("package-info", "repository", "naiveBayesTree")
WPM("install-package", "naiveBayesTree")

WOW("weka/classifiers/trees/NBTree")

NBTree <- make_Weka_classifier("weka/classifiers/trees/NBTree")

fitted.model <- NBTree(Species ~ ., data=iris)
print(fitted.model)


#
WPM("list-packages", "available")
