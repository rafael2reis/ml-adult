CreateDataFrame <- function() {
  kids <- c("Jack","Jill","Jonh","Judith")
  ages <- c(12,10,13,14)
  d <- data.frame(kids,ages,stringsAsFactors=FALSE)
  ab <- c(0, 1, 0)
  ac <- c(0, 0, 0)
  ad <- c(1, 0, 0)
  ae <- c(0, 0, 1)
  ab <- rbind(ab, ac)
  ab <- rbind(ab, ad)
  ab <- rbind(ab, ae)
  d <- cbind(d, ab)
}

ParseAdultTree <- function(file = file) {
  # TEST
  rm(list = ls())
  file <- 'adult_tree.txt';
  #####
  
  text <- scan(file, character(0), sep = "\n")
  pat <- "\\|   "
  nos <- list()
  ultimo.nivel <- 0
  for(i in 1:length(text)) {
    # TEST
    #i <- 2
    #
    
    matches <- unlist(gregexpr(pat, text[i]))
    
    valor <- sub("^.*\\|   ", "", text[i])
    valor <- sub(":.+$", "", valor)
    valor <- sub(" = ", " == ", valor)
    valor <- sub(" == (.+)$", ' == "\\1"', valor, fixed = FALSE)
    valor <- valor
    
    novo <- list()
    nivel.atual <- length(matches)
    
    if (matches[1] == -1) {
      novo$pai <- 0
      novo$nivel <- 0
      novo$valor <- valor
    } else {
      if ( nivel.atual > ultimo.nivel  ) {
        novo$pai <- i-1
        novo$nivel <- nivel.atual
      } else if ( nivel.atual == ultimo.nivel ) {
        novo$pai <- nos[[i-1]]$pai
        novo$nivel <- ultimo.nivel
      } else {
        j <- nos[[i-1]]$pai
       
        while ( j > 1 & nos[[j]]$nivel > nivel.atual ) {
          j <- nos[[j]]$pai
        }
        novo$pai <- nos[[j]]$pai
        novo$nivel <- nivel.atual
      }
      
      novo$valor <- paste0(nos[[novo$pai]]$valor, " & ", valor)
    }
    
    lines <- c()
    
    lines[1] <- paste0("if (", novo$valor, ") {")
    lines[2] <- "   linha[j] <- 1"
    lines[3] <- "} else {"
    lines[4] <- "   linha[j] <- 0"
    lines[5] <- "}"
    lines[6] <- "j <- j + 1"
    
    write.table( x = lines, file = 'codigo.R', 
                 row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
    
    ultimo.nivel <- novo$nivel
    nos[[i]] <- novo
  }
}

