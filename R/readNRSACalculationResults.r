# readNRSACalculationResults.r
#
# 12/10/09 cws Created
#  3/11/10 cws separating fileName and path with /.
# 11/09/10 cws removed use of NRSAMetricsLocation, using NRSACalcLocation
#  8/19/11 cws Checking to see if file exists before trying to read it in.
#

readNRSACalculationResults <- function(fileName)
# Reads data validation results from a file in the directory specified by
# NRSAMetricsLocation which contains previously calculated metrics.
#
# Returns the contents of the file as a dataframe if successful, or an
# error message if unsuccessful.
#
# ARGUMENTS:
# filePath   string with full path and filename containing validation results.
#
{
  fullPath <- paste(NRSACalcLocation, fileName, sep='/')
  if(file.access(fullPath, mode=4) == -1) {
      return(sprintf("Error: readNRSACalculationResults() can not read file %s", fullPath))
  }

  if(substr(fileName, nchar(fileName)-3, nchar(fileName)) == '.csv') {
      rr <- read.csv(fullPath
                    ,na=''
                    ,header=TRUE
                    ,stringsAsFactors=FALSE
                    )
#      rr <- rr[!is.na(rr[1]),]
  } else  {
      cat(sprintf("Error: readNRSACalculationResults() currently only reads csv files.\n"))
      rr <- "Error: readNRSACalculationResults() currently only reads csv files.\n"
  }

  return(rr)
}

#end of file