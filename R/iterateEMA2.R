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

waterYear <- function (x, numeric = FALSE) {
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  yr <- yr + ifelse(mn < 10L, 0L, 1L)
  if (numeric) 
    return(yr)
  ordered(yr)
}

iterateEMA2 <- function(pkDates, pkFlows, pkCodes, pkPath, beginYrH = NULL, missbeginYr = NULL, missEndYr = NULL, 
                       beginYr, endYr, threshold = NULL, skewOpt, genSkew, skewSE) {
  
  endYrs <- seq(as.numeric(endYr), as.numeric(beginYr) + 10, -1)
  
  newDF <- data.frame(endYear = as.numeric(), Q50 = as.numeric(), lowerQ50 = as.numeric(), 
                      upperQ50 = as.numeric(), Q100 = as.numeric(), lowerQ100 = as.numeric(), 
                      upperQ100 = as.numeric(), Q500 = as.numeric(), lowerQ500 = as.numeric(), 
                      upperQ500 = as.numeric(), Q1000 = as.numeric(), lowerQ1000 = as.numeric(), 
                      upperQ1000 = as.numeric(), Q10000 = as.numeric(), lowerQ10000 = as.numeric(), 
                      upperQ10000 = as.numeric(), Q100000 = as.numeric(), lowerQ100000 = as.numeric(), 
                      upperQ100000 = as.numeric(), Q1000000 = as.numeric(), lowerQ1000000 = as.numeric(), 
                      upperQ1000000 = as.numeric(), Q10000000 = as.numeric(), lowerQ10000000 = as.numeric(), 
                      upperQ10000000 = as.numeric(), stringsAsFactors = FALSE)
  
  topOut <- "                                  EMA FREQUENCY ESTIMATES"
  
  for(i in seq(1, length(endYrs), 1)) {
    
    if (!is.null(beginYrH)) {
      
      for (j in 1:length(beginYrH)) {
        
        hdr <- c(paste0("I pkInput.spc"),
                 "0 something.out",
                 "LOTHRESH         0.0",
                 paste0("SKEWOPT          ", skewOpt),
                 paste0("GENSKEW          ", genSkew),
                 paste0("SKEWSD           ", skewSE),
                 paste0("BEGYEAR          ", beginYr),
                 paste0("ENDYEAR          ", as.character(endYrs[i])),
                 "GAGEBASE            0",
                 paste0("THRESHOLD        ", as.character(beginYrH[j]), " ",
                        as.character(as.numeric(beginYr[j]) - 1), " ", 
                        as.character(threshold[j]), " 1.d99"),
                 paste0("THRESHOLD        ", as.character(beginYr), " ", 
                        as.character(endYrs[i]), " 0.000 1.d99"))
      }
      
    } else if (is.null(beginYrH)) {
      hdr <- c(paste0("I pkInput.spc"),
               "0 something.out",
               "LOTHRESH         0.0",
               paste0("SKEWOPT          ", skewOpt),
               paste0("GENSKEW          ", genSkew),
               paste0("SKEWSD           ", skewSE),
               paste0("BEGYEAR          ", beginYr),
               paste0("ENDYEAR          ", as.character(endYrs[i])),
               "GAGEBASE            0",
               paste0("THRESHOLD        ", as.character(beginYr), " ", 
                      as.character(endYrs[i]), " 0.000 1.d99"))
      
    } else if (!is.null(missingPeriod)) {
      
      for (k in 1:length(missBeginYr)) {
        
        hdr <- c(hdr,
                 paste0("THRESHOLD        ", as.character(missBeginYr[k]), " ", 
                        as.character(missEndYr[k]), " 1.d99 1.d99"))
        
      }
      
    }
    
    pkFile <- data.frame(pkDates, pkFlows = round(pkFlows, 0), pkCodes)
    
    pkVec <- c(paste0("Q ", pkDates, "       ", 
                      pkFile$pkFlows, "        ", 
                      pkFile$pkCodes))
    
    pkFileTxt <- c(hdr, pkVec)
    
    writeLines(pkFileTxt, paste0(pkPath, "/pkInput.spc"))
    
    command <- "PeakfqSA_USACE_win.exe"
    
    args <- paste0("pkInput.spc")
    
    system2(command, args)
    
    newText <- readLines(paste0(pkPath, "/pkInput.out"))
    
    newTbl <- newText[-c(1:grep(pattern = topOut, newText))]
    
    newTbl <- newTbl[6:46]
    
    newTbl <- read.table(text = newTbl)
    
    dfEMA <- data.frame(endYear = endYrs[i], Q50 = newTbl[26, 3], lowerQ50 = newTbl[26, 6], 
                        upperQ50 = newTbl[26, 7], Q100 = newTbl[27, 3], lowerQ100 = newTbl[27, 6], 
                        upperQ100 = newTbl[27, 7], Q500 = newTbl[29, 3], lowerQ500 = newTbl[29, 6], 
                        upperQ500 = newTbl[29, 7], Q1000 = newTbl[30, 3], lowerQ1000 = newTbl[30, 6], 
                        upperQ1000 = newTbl[30, 7], Q10000 = newTbl[32, 3], lowerQ10000 = newTbl[32, 6], 
                        upperQ10000 = newTbl[32, 7], Q100000 = newTbl[35, 3], lowerQ100000 = newTbl[35, 6], 
                        upperQ100000 = newTbl[35, 7], Q1000000 = newTbl[38, 3], lowerQ1000000 = newTbl[38, 6], 
                        upperQ1000000 = newTbl[38, 7], Q10000000 = newTbl[41, 3], lowerQ10000000 = newTbl[41, 6], 
                        upperQ10000000 = newTbl[41, 7], stringsAsFactors = FALSE)
    
    newDF <- dplyr::bind_rows(newDF, dfEMA); rm(dfEMA)
    
    file.remove(paste0(pkPath, "/pkInput.out"))
    
    file.remove(paste0(pkPath, "/pkInput.spc"))
    
  }
  
  return(newDF)
  
}