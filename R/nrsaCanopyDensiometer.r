nrsaCanopyDensiometer <- function(bDensiom=NULL, wDensiom=NULL, wChannelBank=c('LF','RT'), wChannelMid=c('CU','CL','CD','CR')) {

################################################################################
# Function: nrsaCanopyDensiometer
# Title: Calculate NRSA Canopy Densiometer Metrics
# Programmers: Randy Hjort
#              Curt Seeliger
#              Suzanne San Romani
#              Marlys Cappert
#              Tom Kincaid
# Date: January 4, 2010
# Description:
#   This function calculates the canopy densiometer portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires a data frame containing the channel cover data file.
# Wadeable Metrics:
#   xcdenmid xcdenbk vcdenmid vcdenbk nbnk nmid.
# Boatable Metrics:
#   xcdenbk vcdenbk nbnk
# Function Revisions:
#   01/04/10 rch: Copied, plagerized and made up this code.
#   02/18/10 cws: Removed source() of NRSAValidation.r and summaryby.r
#   03/23/10 ssr: Moved creation of unit test dataframes to separate functions.
#   03/31/10 cws: Removing extra print() statements and commented-out code.
#   04/06/10 mrc: Modified unit test and metrics code to handle data with just
#            one protocol.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   07/26/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify name of the data file.  Added argument NRSAdir to the
#            function to identify the directory from which the data file is read
#            and to which the output metrics file is written.
#   12/20/12 tmk: Modified data input to use a data frame containing the data
#            file rather than a csv file.  Modified output to be a data frame
#            rather than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frame to
#            character variables.
#   11/16/15 cws Changed calling interface; UID is now SITE, RESULT is now VALUE, 
#            TRANSDIR is now DIRECTION, and split arguments into bDensiom and wDensiom.
#
# TODO: use ddply instead of summaryby. 
#
# Arguments:
#   channelcover = a data frame containing the channel cover data file.  The
#     data frame must include columns that are named as follows:
#       UID - universal ID value
#       SAMPLE_TYPE - sample type
#       TRANSECT - transect label
#       TRANSDIR - transverse location along transect
#       PARAMETER - identifier for each measurement, assessment, score, etc.
#       RESULT - measurement associated with PARAMETER column
#       FLAG - flag
#   Note that possible values for variables in the input data frame are
#   provided in the document named "NRSA Documentation.pdf" included in the help
#   directory for the package.
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The data frame contains the following columns:
#     UID - universal ID value
#     METRIC - metric name
#     RESULT - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
#   metsCanopyDensiometer.1 - calculate metrics
################################################################################

# Print an initial message
  cat('Canopy Densiometer calculations:\n')

# Convert factors to character variables in the channelcover data frame
  intermediateMessage('.1 Convert factors to character variables.', loc='end')
#  channelcover <- convert_to_char(channelcover)

# # Calculate the metrics
#   intermediateMessage('.2 Call function metsCanopyDensiometer.1.', loc='end')
#   mets <- metsCanopyDensiometer.1(channelcover)
#   row.names(mets) <- 1:nrow(mets)
# 
# # Print an exit message
#   intermediateMessage('Done.', loc='end')
# 
# # Return results
#   return(mets)
# }
# 
# 
# 
# metsCanopyDensiometer.1 <- function(indat) {
# # Does all the real work for metsCanopyDensiometer.
# # Returns a dataframe of calculations if successful
# # or a character string describing the problem if
# # one was encountered.
# #
# # ARGUMENTS:
# # indat		dataframe of canopy data.
# # protocols	dataframe relating UID to the
# #			  sampling protocol used at the site.
# #
    intermediateMessage('.1.0Canopy Densiometer mets', loc='end')

    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdf <- function(df, ...) {
        rc <- df %>% select(SITE,DIRECTION,VALUE)
        return(rc)
    }

    bDensiom = absentAsNULL(bDensiom, ifdf)
    wDensiom = absentAsNULL(wDensiom, ifdf)
   
#   intermediateMessage('.1.1 check data and split into wadeable (mid and bank) and boatable (bank)', loc='end')
#   cdData <- subset(indat,PARAMETER == 'DENSIOM')
#   # cdData <- merge(cdData, protocols, by='UID', all.x=TRUE, all.y=FALSE)
 
    mdx <- NULL
    mds <- NULL
    mdc <- NULL
    bkx <- NULL
    bks <- NULL
    bkc <- NULL
    btx <- NULL
    bts <- NULL
    btc <- NULL

    intermediateMessage('.1.2 sent datasets to summaryby function', loc='end')  
    if(!is.null(wDensiom)) {
        mdData <- subset(wDensiom, DIRECTION %in% wChannelMid)
        bkData <- subset(wDensiom, DIRECTION %in% wChannelBank)
        if(nrow(mdData) > 0) {
            mdx <- summaryby(mdData,'mean',"xcdenmid")
            mds <- summaryby(mdData,'sd',"vcdenmid")
            mdc <- summaryby(mdData,'count',"nmid")
        }
        if (nrow(bkData) > 0) { 
            bkx <- summaryby(bkData,'mean',"xcdenbk")
            bks <- summaryby(bkData,'sd',"vcdenbk")
            bkc <- summaryby(bkData,'count',"nbnk")
        }
    }
  
    if (!is.null(bDensiom)) {  
        btx <- summaryby(bDensiom,'mean',"xcdenbk")
        bts <- summaryby(bDensiom,'sd',"vcdenbk")
        btc <- summaryby(bDensiom,'count',"nbnk")
    }

    intermediateMessage('.1.3 put dataset together and finish calculations', loc='end')
    mets <- rbind(mdx,mds,mdc,bkx,bks,bkc,btx,bts,btc)
    mets$VALUE<-ifelse(mets$VALUE=='NaN', NA, mets$VALUE)
  
    # more calcs
    mets$VALUE <- ifelse(mets$METRIC %in% c("xcdenmid", "vcdenmid", "xcdenbk","vcdenbk")
                        ,100 * mets$VALUE/17
                        ,mets$VALUE
                        )
    
    intermediateMessage('.1.4 Done with function metsCanopyDensiometer.1 ', loc='end')
   
    return(mets)
}

# end of file