# This function was created to iteratively run the Expected Moments Algorithm (EMA)
# analysis on a peak-flow file retrieved via the dataRetrieval package.
#
# The function uses Tim Cohns's PeakfqSA program... 
# http://www.timcohn.com/TAC_Software/PeakfqSA/
# to run the EMA analyses iteratively to a specified water year
# using a vector of end years.
#
# In order for the function to work, the working directory must be set to the location 
# of PeakfqSA0998.exe

iterateEMA <- function(pkFile, pkPath, beginYrH = NULL, beginYr, endYr, threshold = NULL, skewOpt, genSkew, skewSD) {
  
  endYrs <- seq(as.numeric(endYr), as.numeric(beginYr) + 10, -1)
  
  newDF <- data.frame(endYear = as.numeric(), Q50 = as.numeric(), upperCI50 = as.numeric(), 
                      lowerCI50 = as.numeric(), Q100 = as.numeric(), upperCI100 = as.numeric(), 
                      lowerCI100 = as.numeric(), Q500 = as.numeric(), upperCI500 = as.numeric(), 
                      lowerCI500 = as.numeric())
  
  topOut <- "                                  EMA FREQUENCY ESTIMATES"
  
  for(i in seq(1, length(endYrs), 1)) {
    
    if(!is.null(beginYrH)) {
      hdr <- c(paste0("I pkInput.spc"),
               "0 something.out",
               "LOTHRESH         0.0",
               paste0("SKEWOPT          ", skewOpt),
               paste0("GENSKEW          ", genSkew),
               paste0("SKEWSD           ", skewSD),
               paste0("BEGYEAR          ", beginYr),
               paste0("ENDYEAR          ", as.character(endYrs[i])),
               "GAGEBASE            0",
               paste0("THRESHOLD        ", as.character(beginYrH), " ",
                      as.character(as.numeric(beginYr) - 1), " ", 
                      as.character(threshold), " 1.d99"),
               paste0("THRESHOLD        ", as.character(beginYr), " ", 
                      as.character(endYrs[i]), " 0.000 1.d99"))
    }
    
    else if(is.null(beginYrH)) {
      hdr <- c(paste0("I pkInput.spc"),
               "0 something.out",
               "LOTHRESH         0.0",
               paste0("SKEWOPT          ", skewOpt),
               paste0("GENSKEW          ", genSkew),
               paste0("SKEWSD           ", skewSD),
               paste0("BEGYEAR          ", beginYr),
               paste0("ENDYEAR          ", as.character(endYrs[i])),
               "GAGEBASE            0",
               paste0("THRESHOLD        ", as.character(beginYr), " ", 
                      as.character(endYrs[i]), " 0.000 1.d99"))
    }
    
    pkFile2 <- pkFile[format(pkFile$peak_dt, "%Y") <= endYrs[i],]
    
    pkVec <- c(paste0("Q ", waterYear(pkFile2[,3]), "       ", 
                      pkFile2[,5], "        ", 
                      pkFile2[,6]))
    
    pkFileTxt <- c(hdr, pkVec)
    
    writeLines(pkFileTxt, paste0(pkPath, "/pkInput.spc"))
    
    command <- "PeakfqSA0998.exe"
    
    args <- paste0("pkInput.spc")
    
    system2(command, args)
    
    newText <- readLines(paste0(pkPath, "/pkInput.out"))
    
    newTbl <- newText[-c(1:grep(pattern = topOut, newText))]
    
    newTbl <- newTbl[6:34]
    
    newTbl <- read.table(text = newTbl)
    
    dfEMA <- data.frame(endYear = endYrs[i], Q50 = newTbl[26,3], upperCI50 = newTbl[26,6], 
                        lowerCI50 = newTbl[26,7], Q100 = newTbl[27,3], upperCI100 = newTbl[27,6], 
                        lowerCI100 = newTbl[27,7], Q500 = newTbl[29,3], upperCI500 = newTbl[29,6], 
                        lowerCI500 = newTbl[29,7], stringsAsFactors = FALSE)
    
    newDF <- dplyr::bind_rows(newDF, dfEMA); rm(dfEMA)
    
    file.remove(paste0(pkPath, "/pkInput.out"))
    
    file.remove(paste0(pkPath, "/pkInput.spc"))
    
  }
  
  return(newDF)
  
}

