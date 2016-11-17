#' @export 
#' @title Calculate NRSA Substrate Embeddedness Metrics
#' @description This function calculates the substrate 
#' embeddedness portion of the physical habitat metrics for 
#' National Rivers and Streams Assessment (NRSA) data.  The
#' function requires a data frame containing the channel crosssection 
#' data file.
#' @param percentEmbedded A data frame containing size class data 
#' for the dominant substrate class in the channel bottom, with the 
#' following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item ONBANK logical value specifying whether the values
#'                   were obtained at the bank or somewhere in the
#'                   channel
#'      \item VALUE character values
#' }
#' @return Either a data frame when metric calculation is successful 
#' or a character string containing an error message when metric 
#' calculation is not successful.  The data frame contains the following 
#' columns:
#' \itemize{
#'    \item SITE - universal ID value
#'    \item METRIC - metric name
#'    \item VALUE - metric value
#' } 
#' Metrics calculated include: 
#' n33, n55, vcembed, vembed, xcembed, xembed
#'     
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}

nrsaSubstrateEmbed <- function(percentEmbedded=NULL) {

################################################################################
# Function: metsSubstrateEmbed
# Title: Calculate NRSA Substrate Embeddedness Metrics
# Programmers: Randy Hjort
#              Curt Seeliger
#              Tom Kincaid
# Date: January 27, 2010
# Description:
#   This function calculates the substrate embeddedness portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires a data frame containing the channel crosssection data file.
# Metrics:
#   n33, n55, vcembed, vembed, xcembed, xembed
# Function Revisions:
#   01/27/10 rch: Copied, plagerized and made up this code.
#   02/18/10 cws: Removed source() calls to several files.
#   03/18/10 cws: Unit test merge() with all=TRUE.
#   03/22/10 cws: Moved creation of test dataframes for unit test to separate
#            functions.
#   03/31/10 cws: Removed extra print() statements and commented-out code.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   11/04/10 cws: Modified to handle single-protocol data (in this case, when
#            data has no rows). Unit test modified to check this.
#   07/31/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify names of the data files.  Added argument NRSAdir to the
#            function to identify the directory from which data files are read
#            and to which the output metrics file is written.
#   12/27/12 tmk: Modified data input to use a data frame containing the data
#            file rather than a csv file.  Modified output to be a data frame
#            rather than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frame to
#            character variables.
#   12/11/15 cws created from metsSubstrateEmbed, updated for new calling interface.
#    3/16/16 cws Documenting arguments in comments at top.
#
# Arguments:
# percentEmbedded   dataframe containing size class data for the dominant 
#                   substrate class in the channel bottom, with the following 
#                   columns:
#                   SITE        integer or character specifying the site 
#                               visit
#                   TRANSECT    character value specifying the transect
#                               for which the value was recorded.
#                   ONBANK      logical value specifying whether the values
#                               were obtained at the bank or somewhere in the
#                               channel
#                   VALUE       character values
#
#   Note that possible values for variables in the input data frame are
#   provided in the document named "NRSA Documentation.pdf" included in the help
#   directory for the package.
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The data frame contains the following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     RESULT - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

    intermediateMessage('Substrate Embeddedness mets', loc='start')
    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdf <- function(df, ...) {
#         if(is.null(...)) return(NULL)
#         else if(all(is.na(...))) return(NULL)

        rc <- df %>% select(SITE, TRANSECT, ONBANK, VALUE)
        return(rc)
    }
    
    percentEmbedded <- absentAsNULL(percentEmbedded, ifdf)
    if(is.null(percentEmbedded)) {
    intermediateMessage('.  no data', loc='end')
        return(NULL)
    }

    #set VALUE to numeric
    percentEmbedded$VALUE<-as.numeric( percentEmbedded$VALUE)

#     cdata <- subset(percentEmbedded, DIRECTION %in% c('LC','CT','RC'))
    cdata <- subset(percentEmbedded, !ONBANK)

    intermediateMessage('.1')

    mets <- NULL   # Assemble calculations here
    if(nrow(percentEmbedded)>0) {
        ca <- summaryby(percentEmbedded,'count',"n55")
        xa <- summaryby(percentEmbedded,'mean',"xembed")
        va <- summaryby(percentEmbedded,'sd',"vembed")
        mets <- rbind(ca,xa,va)
        intermediateMessage('.2')
    }
  

    if(nrow(cdata)>0) {
        cc <- summaryby(cdata,'count',"n33")
        xc <- summaryby(cdata,'mean',"xcembed")
        vc <- summaryby(cdata,'sd',"vcembed")
        mets <- rbind(mets, cc,xc,vc)
        intermediateMessage('.3')
    }

    intermediateMessage('.4')

    if(is.null(mets)) {
        mets <- "Data lacks EMBED parameter (may be boatable data)"
    } else {
        mets$VALUE<-ifelse(mets$VALUE=='NaN', NA, mets$VALUE)
    }

    intermediateMessage('.  Done.', loc='end')

    return(mets)
}

# end of file