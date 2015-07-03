CreateArff <- function(train = NULL, test = NULL, relation = NULL) {
  # TEST#####
  # source('dataset.R')
  # LoadAdultDataset(asStrings = TRUE)
  # train <- adult
  # test <- adult.test
  # relation <- 'adult'
  ###########
  lines <- c( paste0('@RELATION ', relation) )
  lines[2] <- ''
  
  index <- 3
  
  all <- rbind(train, test)
  for (i in 1:ncol(all)) {
    if ( class(all[, i]) %in% c('numeric', 'integer') ) {
      new.line <- paste0('@ATTRIBUTE ',  names(all)[i],  ' NUMERIC')
    } else if ( class(all[, i]) %in% c('factor', 'character') ) {
      new.line <- paste0('@ATTRIBUTE ',  names(all)[i], ' {',  paste0(unique(all[, i]), collapse = ','), '}')
    }
    
    lines[index] <- new.line
    index <- index + 1
  }
  
  lines[index] <- ''
  index <- index + 1
  lines[index] <- '@DATA'
  index <- index + 1  

  lines.test <- rbind(lines)
  index.test <- index
  
  # Create Train file
  for (i in 1:nrow(train)) {
    lines[index] <- c( paste0(train[i, ], collapse = ',') )
    index <- index + 1
  }
  
  # Create Test file
  for (i in 1:nrow(test)) {
    lines.test[index.test] <- c( paste0(test[i, ], collapse = ',') )
    index.test <- index.test + 1
  }
  
  # Write in file
  train.arff <- file( paste0(relation, ".arff") )
  writeLines(lines, train.arff)
  close(train.arff)
  
  # Write in test file
  test.arff <- file( paste0(relation, ".test.arff") )
  writeLines(lines.test, test.arff)
  close(test.arff)
}