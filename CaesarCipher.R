# R function that allows us to encrypt and decrypt text-strings
# using the Ceasar Cipher. For more info check: 

CaesarCipher <- function(string, shift = 1, shuffle = FALSE, seed = 123){
  
  # loading dependencies
  if("qdap" %in% installed.packages() == FALSE) {
    install.packages("qdap")
  }
  library(qdap)
  
  # switch of warnings()
  options(warn = -1)
  
  # preventing factorization
  options(stringsAsFactors = FALSE)
  
  # taking care of shifts that are bigger than 26 or smaller than -26
  shift <- shift%%71
  
  # defining a dictionary for mapping characters to numbers
  a <- c(letters,"0","1","2","3","4","5","6","7","8","9",".",",",";",":","-","_","!","\"","#","\'","§","$","%","&","/","(","[",")","]","=","{","}","\\","`","´","+","*","~","^","°","@","€","\n","\r","\r\n")
  
  if(shuffle == TRUE){
    
    set.seed(seed)
    a <- sample(a)
    print(seed)
    print(a)
  }
  
  b <- as.numeric(1:71)
  df <- cbind.data.frame(a,b)
  
  # Deconstructing message into a list of a list of words, split up into their characters
  Lowercase <- tolower(string)
  SplitWords <- strsplit(Lowercase, " ")
  SplitLetters <- lapply(SplitWords, strsplit, "")
  
  # Translating all non-punctuation characters to numerals
  Numeralisation <- function(x){x[is.element(x,df[,1])] <- sapply(x[is.element(x,df[,1])],match,df$a);return(x)}
  NumeraliszedLetters <- lapply(SplitLetters[[1]],Numeralisation)
  
  # Shifting all non-punctuation characters about "shift" units along the alphabet
  Shifter <- function(x){x <- sapply(x,function(y){as.numeric(y)+shift});return(x)}
  ShiftedNumeralizedLetters <- lapply(NumeraliszedLetters,Shifter)
  
  # Through the shift, we might get values larger than 26 or smaller than 0, we need to account for that to map them back to letters
  ImitatingPositiveModularity <- function(x){x[as.numeric(x) > 71] <- (as.numeric(x[as.numeric(x) > 71]) - 71); return(x)}
  ImitatingNegativeModularity <- function(x){x[as.numeric(x) < 1] <- (as.numeric(x[as.numeric(x) < 1]) + 71); return(x)}
  ModularizedShiftedNumeralizedLetters <- lapply(ShiftedNumeralizedLetters,ImitatingPositiveModularity)
  ModularizedShiftedNumeralizedLetters <- lapply(ModularizedShiftedNumeralizedLetters,ImitatingNegativeModularity)
  
  # Translating the non-punctuation numeric values back to letters
  TranslateToLetters <- function(x){x <- df[,1][as.numeric(x)];return(x)}
  EncryptedLetters <- lapply(ModularizedShiftedNumeralizedLetters,TranslateToLetters)
  
  # collapsing the characters back into one string
  CollapsedWords <- lapply(EncryptedLetters, paste, collapse = "")
  CollapsedMessage <- paste(unlist(CollapsedWords),collapse = " ")
  
  # switch warnings on again
  options(warn = 0)
  
  # returning chiffre
  EncryptedMessage <- CollapsedMessage
  return(EncryptedMessage)
}