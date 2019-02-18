## Lets implement some cipher algorithms for ciphering and deciphering messages


## TO DO:
# - We can now guess words based on their length (I,we,can etc.). Crypographers thus usually
# split the message in Chunks of 4 characters seperated by a space character


Text <- c("Contrary to popular belief, Lorem Ipsum is not simply random 
          It has roots in a piece of classical Latin literature from 45 BC,
          making it over 2000 years old. Richard McClintock, a Latin professor
          at Hampden-Sydney College in Virginia, looked up one of the more
          obscure Latin words, consectetur, from a Lorem Ipsum passage,
          and going through the cites of the word in classical literature,
          discovered the undoubtable source. Lorem Ipsum comes from sections
          1.10.32 and 1.10.33 of \"de Finibus Bonorum et Malorum\"  
          (The Extremes of Good and Evil) by Cicero, written in 45 BC. This
          book is a treatise on the theory of ethics, very popular during the Renaissance.
          The first line of Lorem Ipsum, \"Lorem ipsum dolor sit amet..\",
          comes from a line in section 1.10.32. Oh and this last bit is just added for fun.")

# We want to code a transpostion cipher: The scytale and break it.

scytale <- function(Msg,rows = 3,method = "encrypt"){
        
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


# Testing
Enc <- scytale(Text,rows = 2,method = "encrypt");Enc
Dec <- scytale(Enc, rows = 2, method = "decrypt");Dec


#### Breaking the Scytale

# To break the Scytale we first need to know that we are dealing with one just from the encoded text.
# Because the Cipher only rearranges the order of the letters in the text, but doesn´t change the letters
# itself, the distribution of letters just look the same as in plain text. Of course, we´d need to know
# the language of the plaintext to compare it to the right distribution. But usually it´s safe to
# assume that people are commuincating in ther mother tongue. In this case, we will simply assume that the 
# original message was in english.

# First, we need to split the encoded message into its individual characters
Lowercase <- tolower(Enc)
SplitLetters <- strsplit(Lowercase,"")

# We can throw away the structure of the message
Chars <- unlist(SplitLetters)

# we can then get an overview of which characters are contained in the encoded message
unique(Chars)
length(unique(Chars))

# We have 38 unique characters in the encoded message. If every character from the plaintext is mapped
# exactly to one character in the encoded message, that means that we have more characters than the alphabet has letters.
# We also see that the encoded message contains not only letters but also numbers and special characters.

# Lets look at the distribution of the characters
Dist <- table(Chars);Dist

# Creating a Dataframe
DistFrame <-cbind.data.frame(Dist)

# We see that the letters q and z are missing, so we add them along with the numbers 6,7,8,9
FullTable <- table(factor(Chars, levels = c(levels(DistFrame[,1])[1:14],"6","7","8","9",levels(DistFrame[,1])[15:30],"q",levels(DistFrame[,1])[31:37],"y","z")));FullTable


# We can also plot it
barplot(FullTable, main = "Character Distribution in Encoded Message",cex.names = 0.5)
barplot(prop.table(FullTable), main = "Character Distribution in Encoded Message",cex.names = 0.5)

# What we need now is a text in the language that we suspect the plaintext to be in, we we can compare
# the distribution of characters and see if they are reasonably similar. The longer the text is the better.
# We should only keep in mind that it should be a regular text without obvious overrepresentations of specific characters.

# importing english text (King James bible)
KJB <- read.delim("https://raw.githubusercontent.com/mxw/grmr/master/src/finaltests/bible.txt",header = FALSE)

# preprocessing the data
KJBText <- paste(KJB[,1], collapse=" ")

# splitting the bible into characters and looking at character distribution
Lowercase <- tolower(KJBText)
SplitWords <- strsplit(Lowercase, "")
SplitLetters <- lapply(SplitWords, strsplit, "")
SplitLetters <- unlist(SplitLetters)

# having a look
head(SplitLetters,50)

# plotting distribution of characters in the KJB
barplot(prop.table(table(SplitLetters)), main = "Character Distribution in King James Bible",cex.names = 0.5)

# The Distribution of special characters and letters is highly dependent on the nature of the text (e.f. containing
# GPS coordinates). For this reason, we just compare the distirbution of the letters.

# table with only characters from the encoded message
FullTable[c(2,19:44)]

# table with only characters from the KJB
KJBFreq <- table(SplitLetters)
KJBFreq[c(3,28:53)]

# comparing the two distributions graphically
par(mfrow=c(2,1))
barplot(prop.table(FullTable[c(2,19:44)]), main = "Character Distribution in Encoded Message",cex.names = 0.5)
barplot(prop.table(KJBFreq[c(3,28:53)]), main = "Character Distribution in King James Bible",cex.names = 0.5)

# Although the distributions are not exactly identical, we see a lot of characteristic overlaps
# For example, we see clear spikes for the letters: a,e and i -> these are the most common characters in the english language
# Similarly, we see clear dents in the distribution for the letters: j,q,x and z -> These are the least common characters in the english language

# We now have some evidence that the characters from the plaintext were not "translated" to other characters but that only their
# order was changed somehow. Although the scytale is not the only cipher that works in this way, we can simply try the scytale and
# see if it makes sense.

Dec1 <- scytale(Enc, rows = 1, method = "decrypt");Dec1
Dec2 <- scytale(Enc, rows = 2, method = "decrypt");Dec2
Dec3 <- scytale(Enc, rows = 3, method = "decrypt");Dec3
Dec4 <- scytale(Enc, rows = 4, method = "decrypt");Dec4
Dec5 <- scytale(Enc, rows = 5, method = "decrypt");Dec5

# BINGO! We see that when we choose rows = 2, we get a message that makes sense! We successfully broke the scytale!
# But we want a way to do this automatically for the posisble number of rows. The maximum number of rows is limited by
# the amount of characters in the encoded message, so we need to test every possible number of rows up until the number
# of characters in the encoded message

# Finding out how many characters we have in the encoded message
Lowercase <- tolower(Enc)
SplitLetters <- strsplit(Lowercase, "")
LetterVec <- unlist(SplitLetters)
length(LetterVec)

# Creating a helper funciton and testing all possible rownumbers
LapplyHelper <- function(x){return(scytale(Enc,rows = x, method = "decrypt"))}
AllRows <- lapply(1:length(LetterVec),LapplyHelper)

# We now have a list where each element is the attempted decoded message with a different rownumber for the scytale method.
# We can now go trough it manually to see wich rownumber makes sense! If the number of rows was extremely high and too much
# to go trough manually, we could write a function that checks each options automatically if it contains words from the english
# (or any ohter) dictionary and flag the corresponding number of rows for closer manual inspection.


# We want to code a substitution cipher: The caesar cipher
CaesarChiffre <- function(string,shift = 1, shuffle = FALSE, seed = 123){
        
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

# Testing shift
EncA <- CaesarChiffre(Text,shift = 5);EncA
DecA <- CaesarChiffre(EncA,shift = -5);DecA

# Testing default shuffle
EncB <- CaesarChiffre(Text,shift=1, shuffle = TRUE);EncB
DecB <- CaesarChiffre(EncB,shift=-1, shuffle = TRUE);DecB

# Testing custom shuffle
EncC <- CaesarChiffre(Text,shift=7, shuffle = TRUE, seed = 3632);EncC
DecC <- CaesarChiffre(EncC,shift=-7, shuffle = TRUE, seed = 3632);DecC

# Lets import a longer text to cipher parts of it (Sherlock Holmes)
SH <- read.delim("http://norvig.com/big.txt",header = FALSE)

# preprocessing the data
SHText <- paste(SH[,1], collapse=" ")

# Cutting out a part for encryption
Plain <- substr(SHText, 3830, 4889);Plain

# Ciphering the plain text
EncPlain <- CaesarChiffre(Plain, shift = 3);EncPlain

# Finding out how many characters we have in the encoded message
Lowercase <- tolower(EncPlain)
SplitWords <- strsplit(Lowercase, " ")
SplitLetters <- lapply(SplitWords, strsplit, "")
LetterVec <- unlist(SplitLetters)
length(LetterVec)

# finding out how many unique signs there are
unique(LetterVec)
length(unique(LetterVec))

# We can throw away the structure of the message
Chars <- LetterVec

# we can then get an overview of which characters are contained in the encoded message
unique(Chars)
length(unique(Chars))

# We have 33 unique characters in the encoded message. If every character from the plaintext is mapped
# exactly to one character in the encoded message, that means that we have more characters than the alphabet has letters.
# We also see that the encoded message contains not only letters but also numbers and special characters.

# Lets look at the distribution of the characters
Dist <- table(Chars);Dist
DistFrame <- as.data.frame(Dist);DistFrame
















FullTable <- table(factor(Chars, levels = c(levels(DistFrame[,1])[1:13],"6","7","8","9",levels(DistFrame[,1])[14:29],"q",levels(DistFrame[,1])[30:37],"z")));FullTable

###### NEXT UP

# We want to break the simple Ceasar cipher with a brute force attack and trying out all combinations to see which one makes sense

# We want to improve the Caesar Cipher by using a shuffled alphabet instead of a rotated one!

# We want to break the improved cipher using a frequency analysis

# We want to improve the Caesar cipher to make it harder to crack with frequency analysis:

        # We want to include all letters, spaces and punctuation in the plaintext alphabet so it gets substituted aswell
        # We want to make the secret alphabet be a random selction of all kinds of possible signs: Greek, russian, japanese, numbers, letters, punctuation...
        # We want to include a nomenclature of words that we replace by a single sign instead of replacing all their letters
        # We want to throw in some symbols with no meaning at random places into the message


