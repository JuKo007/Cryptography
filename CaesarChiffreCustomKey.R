### Chiffre Algorithm for encrypting and decrypting text strings
### contains some limited handling of numbers and special characters.
### special characters are not encrypted and remain plaintext. Numbers are
### detected and translated to words (e.g. 32 becomes thirtytwo) and then encrypted.

## Instead of shifting the letters across the alphabet, we randomly shuffle the encryption key alphabet

CaesarChiffre <- function(string,key = sample(letters)){
        
        # loading dependencies
        if("qdap" %in% installed.packages() != TRUE) {
                install.packages("qdap")
        }
        library(qdap)
        
        # switch of warnings()
        options(warn = -1)
        
        # defning a dictionary for mapping characters to numbers
        a <- letters
        b <- key
        df <- cbind.data.frame(a,b)
        
        # defining a list of punctuation characters that we do not want to be encrypted
        punct <- c(".",",",";",":","-","_","!","\"","#","\'","§","$","%","&","/","(","[",")","]","=","{","}","\\","`","´","+","*","~","^","°","@","€")
        
        # replacing numbers in message by their text representation
        string <-  replace_number(string,FALSE)
        
        # Deconstructing message into a list of a list of words, split up into their characters
        Lowercase <- tolower(string)
        SplitWords <- strsplit(Lowercase, " ")
        SplitLetters <- lapply(SplitWords, strsplit, "")
  
        # mapping the non-punctuation plaintext characters to the key-alphabet (We?re leaving out the punctuation characters now...)
        Encryption <- function(x){var <- match(x[is.element(x,punct) == FALSE],df$a); return(as.character(df$b[var]))}
        EncryptedLetters <- sapply(SplitLetters[[1]],Encryption)
        
        letters[as.numeric(EncryptedLetters[[1]])]
        SplitLetters[[1]][1]
        
        
        A <-  match(SplitLetters[[1]][[1]],df$a)
        as.character(df$b[A])
        
        
        
        mup(SplitLetters[[1]][[1]])
        
        
        
        
        # Translating all non-punctuation characters to numerals
        Numeralisation <- function(x){x[is.element(x,punct) == FALSE] <- sapply(x[is.element(x,punct) == FALSE],match,df$a);return(x)}
        NumeraliszedLetters <- lapply(SplitLetters[[1]],Numeralisation)
        
        # Shifting all non-punctuation characters about "shift" units along the alphabet
        Shifter <- function(x){x[is.element(x,punct) == FALSE] <- sapply(x[is.element(x,punct) == FALSE],function(y){as.numeric(y)+shift});return(x)}
        ShiftedNumeralizedLetters <- lapply(NumeraliszedLetters,Shifter)
        
        # Through the shift, we might get values larger than 26 or smaller than 0, we need to account for that to map them back to letters
        ImitatingPositiveModularity <- function(x){x[is.element(x,punct) == FALSE & as.numeric(x) > 26] <- (as.numeric(x[is.element(x,punct) == FALSE & as.numeric(x) > 26]) - 26); return(x)}
        ImitatingNegativeModularity <- function(x){x[is.element(x,punct) == FALSE & as.numeric(x) < 1] <- (as.numeric(x[is.element(x,punct) == FALSE & as.numeric(x) < 1]) + 26); return(x)}
        ModularizedShiftedNumeralizedLetters <- lapply(ShiftedNumeralizedLetters,ImitatingPositiveModularity)
        ModularizedShiftedNumeralizedLetters <- lapply(ModularizedShiftedNumeralizedLetters,ImitatingNegativeModularity)
        
        # Translating the non-punctuation numeric values back to letters
        TranslateToLetters <- function(x){x[is.element(x,punct) == FALSE] <- letters[as.numeric(x[is.element(x,punct) == FALSE])];return(x)}
        EncryptedLetters <- lapply(ModularizedShiftedNumeralizedLetters,TranslateToLetters)
        
        # collapsing the characters back into one string
        CollapsedWords <- lapply(EncryptedLetters, paste, collapse = "")
        CollapsedMessage <- paste(unlist(CollapsedWords),collapse = " ")
        
        # switch warnings on again
        options(warn = 0)
        
        # printing key when encrypting
        print(paste("Message was encrypted using the following key: ",key))
        
        # returning chiffre
        EncryptedMessage <- CollapsedMessage
        return(EncryptedMessage)
}

string <- c("Contrary to popular belief, Lorem Ipsum is not simply random 
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
              comes from a line in section 1.10.32.")

## Encrypting and then Decrpyting the test message should result in the original text (in all lowercase and with numbers tranlated into words)
Enc <- CaesarChiffre(string)



Dec <- CaesarChiffre(Enc,shift = -4)
Dec


HannahsText <-c("Heute war ein schoener Tag.")

EncryptedMessage <- CaesarChiffre(HannahsText,shift = 2);EncryptedMessage
Back <- CaesarChiffre(EncryptedMessage, shift = -2);Back

