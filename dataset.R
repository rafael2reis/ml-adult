adult <- NULL
adult.test <- NULL
colnames <- NULL
outcome <- NULL

LoadAdultDataset <- function(asStrings = FALSE) {
  nas <- c(" ?", "?", "? ")
  
  if (asStrings) {
    adult <<- read.csv("adult.data.csv", header = FALSE, 
                       stringsAsFactors = FALSE,
                       strip.white = TRUE)
  } else {
    adult <<- read.csv("adult.data.csv", header = FALSE, na.strings = nas,
                       strip.white = TRUE)
  }
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
  if (asStrings) {
    adult.test <<- read.csv("adult.test.csv", header = FALSE, stringsAsFactors = FALSE, strip.white = TRUE)
  } else {
    adult.test <<- read.csv("adult.test.csv", header = FALSE, na.strings = nas, strip.white = TRUE)
  }
  names(adult.test) <<- colnames
  
  outcome <<- 'over50K'
}