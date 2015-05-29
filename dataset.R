adult <- NULL
adult.test <- NULL
colnames <- NULL
outcome <- NULL

LoadAdultDataset <- function() {
  nas <- c(" ?", "?", "? ")
  
  adult <<- read.csv("adult.data.csv", header = FALSE, na.strings = nas)
  colnames <<- c("age",
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
  names(adult) <<- colnames
  
  # De teste
  adult.test <<- read.csv("adult.test.csv", header = FALSE, na.strings = nas)
  names(adult.test) <<- colnames
  
  outcome <<- 'over50K'
}