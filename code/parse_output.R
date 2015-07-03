ParseOutput <- function(file = file) {
  # TEST
  #file <- 'resultados/NBTree_wp.txt';
  #####
  
  text <- scan(file, character(0), sep = "\n")
  #text2 <- readLines(file,encoding="UTF-8")
  FindLine <- function(x) {
    return( grepl(".*[0-9]+.+(>50K|<=50K).+(>50K|<=50K).+", x) )
  }
  result <- text[sapply(text, FindLine)]
  
  if (length(result) != 16281) {
    stop(paste("O número de registros deve ser 16281! Encontrados:", length(result)))
  }
  
  ExtractInfo <- function(x) {
    xs <- unlist(strsplit(x, "[[:space:]]+"))
    value <- grep(pattern = "(<=50K|>50K)", x = xs, value = T)
    return(sub(pattern = "[[:digit:]]:", replacement = "",  x = value[2]))
  }
  rm(text)
  predicts <- c()
  for (i in 1:length(result)) {
    predicts[i] <- ExtractInfo(result[i])
  }
  
  return(predicts)
}