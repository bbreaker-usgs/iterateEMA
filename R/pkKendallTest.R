# given a USGS site ID, this fucntion;
# 1) retrieves the peak flow file
# 2) changes any -00 days for historic peaks to -01
# 3) adds the water year to the peak flow file
# 4) performs the Kendall test on the peak flow data
# 5) returns the Kendall test results

pkKendallTest <- function(site) {
  
  # get the peak flow data, don't convert dates to class Date
  pk <- readNWISpeak(site, convertType = FALSE)
  
  # replace any -00 vaules for days with -01
  pk$peak_dt <- dplyr::if_else(stringr::str_sub(pk$peak_dt, start = 9, end = 10) == "00", 
                               paste0(stringr::str_sub(pk$peak_dt, 1, 8), "01"), pk$peak_dt)
  
  # change the dates to class Date
  pk$peak_dt <- as.Date(pk$peak_dt, format = "%Y-%m-%d")
  
  # convert the peak-flows to a numeric value
  pk$peak_va <- as.numeric(pk$peak_va)
  
  # get rid of rows with no peak-flow data
  pk <- dplyr::filter(pk, !is.na(peak_va))
  
  # add a numeric column for water year to the data
  pk$wYear <- as.numeric(as.character(smwrBase::waterYear(pk$peak_dt)))
  
  # run the Kendall test
  kenSummary <- Kendall::Kendall(pk$wYear, pk$peak_va)
  
  # convert the results to a data frame
  kenSumDF <- data.frame(t(unlist(kenSummary)))
  
  # add the site number to the beginning of the data frame
  kenSumDF <- cbind(site_no = site, kenSumDF)
  
  # return the summary
  return(kenSumDF)
  
}