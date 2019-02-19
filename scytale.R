# R function that allows us to encrypt and decrypt text-strings
# using the scytale method. For more info check: https://en.wikipedia.org/wiki/Scytale
# The method is very similar to the rail-fence method: https://en.wikipedia.org/wiki/Rail_fence_cipher


scytale <- function(Msg,rows = 6, method = "encrypt"){
  
  # checking if the message is a character vector
  if(!is.character(Msg)){
    
    warning("Message must be a character vector")
    stop()
  }
  
  # checking if rows is a numeric value
  if(!is.numeric(rows)){
    
    warning("Message must be a character vector")
    stop()
  }
  
  # to deal with invalid decimal input, round rows up to nearest integer
  rows <- ceiling(rows)
  
  # Splitting Message into single characters
  Split <- unlist(strsplit(tolower(Msg),""))
  
  # checking how many characters there are in the splitted message
  SplitLength <- length(Split)
  
  # checking if number of rows is shorter or equal to number of characters in the message
  if(rows > SplitLength){
    
    warning("Number of rows must be equal or longer to number of characters in message")
    stop()
  }
  
  # computing how many colums we would need given the length of the message and number of rows to arrange characters in a matrix
  MatCols <- ceiling(SplitLength/rows)
  
  # Adding necessary extra elements to the vector so we can arrange characters in a matrix with 'rows' number of rows
  length(Split) <- rows*MatCols
  
  
  # We transpose the matrix if we want to encrypt the message and do not if we want to decrypt
  if(method == "encrypt"){
    
    # Putting Message into matrix
    Mat <- matrix(Split, nrow = rows, ncol = MatCols, byrow = FALSE)
    
    # tranposing matrix
    Mat <- t(Mat)
    
    # Pasting the message back together
    OutMsg <- paste0(as.vector(Mat[!is.na(Mat)]),collapse="")
  }
  
  # We transpose the matrix if we want to encrypt the message and do not if we want to decrypt
  if(method == "decrypt"){
    
    # loading package for convenience when not already in library
    if("R.utils" %in% installed.packages() == FALSE) {
      install.packages("R.utils")
    }
    library(R.utils)
    
    # computing how many NAs were inserted into hte original message
    NumberOfNAs <- (rows*MatCols) - SplitLength
    
    # assigning string to a new vector for inserting NAs in for loop
    Split2 <- Split
    
    if(NumberOfNAs != 0){
      
      # computing the positions of the NAs in the original message
      RowsWithNAs <- tail(c(1:rows),NumberOfNAs)
      NAPositionsInString <- RowsWithNAs * MatCols
      
      # inserting NAs at right positions
      for(i in 1:NumberOfNAs){
        
        Split2 <- insert(Split2,NAPositionsInString[i],NA)
      }
      
    }
    
    # removing the NAs that we have too much
    Split3 <- Split2[1:(length(Split2)-NumberOfNAs)]
    
    # putting the message with reinserted Nas at the right spots into a matrix
    DecMat <- matrix(Split3, nrow = rows, ncol = MatCols, byrow = TRUE)
    
    # pasting everything back together
    DecLet <- as.vector(DecMat[!is.na(DecMat)])
    OutMsg <- paste0(DecLet,collapse="")
    
  }
  
  # return the message
  return(OutMsg)
}


