#' @export
#' @title Calculate NRSA Canopy Densiometer Metrics
#' @description This function calculates the canopy densiometer 
#' portion of the physical habitat metrics for National Rivers 
#' and Streams Assessment (NRSA) data.  The function requires a 
#' data frame containing the channel cover data file.
#' @param bDensiom A data frame containing canopy densiometer 
#' counts for boatable reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item VALUE numeric counts
#' }
#' @param wDensiom A data frame containing canopy densiometer 
#' counts for wadeable reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item DIRECTION character value specifying the direction being
#'  faced.  These values are expected to be the same
#'  as listed in the arguments wChannelBank and wChannelMid
#' \item VALUE numeric counts
#' }
#' @param wChannelBank Character values listing the values of 
#' DIRECTION which indicate that the counts were obtained on the 
#' bank of the channel. Defaults to c('LF','RT').
#' @param wChannelMid Character values listing the values of DIRECTION 
#' which indicate that the counts were obtained midstream in the channel. 
#' Defaults to c('CU','CL','CD','CR').
#' @param isUnitTest Logical argument to determine whether errors should be ignored.
#' Should only be used for running a unit test. Default value is FALSE.
#' @return Either a data frame when metric calculation is successful or a 
#' character string containing an error message when metric calculation 
#' is not successful. The data frame contains the following columns:
#' \itemize{ 
#'     \item SITE - unique site visit identifier
#'     \item METRIC - metric name
#'     \item VALUE - metric value
#'       }
#' The output metrics for boatable sites include: xcdenmid, 
#' vcdenmid, nmid, xcdenbk, vcdenbk, nbnk 
#'  
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#' head(channelcoverEx)
#' # Must subset example dataset to create inputs, keeping only SITE
#'   #  and VALUE
#'   # VALUE must be numeric for this function
#' channelcoverEx <- plyr::mutate(channelcoverEx, VALUE=as.numeric(VALUE))
#' 
#' # Boatable canopy density readings
#' bDen <- subset(channelcoverEx,SAMPLE_TYPE=='PHAB_CHANB' & PARAMETER=='DENSIOM',
#' select = c('SITE','VALUE'))  
#' 
#' # Wadeable canopy density readings
#' wDen <- subset(channelcoverEx,SAMPLE_TYPE=='PHAB_CHANW' & PARAMETER=='DENSIOM',
#' select = c('SITE','BANK','VALUE'))
#' # BANK is renamed DIRECTION for this input
#' wDen <- plyr::rename(wDen,c('BANK'='DIRECTION'))
#' 
#' canDenOut <- nrsaCanopyDensiometer(bDensiom=bDen, wDensiom=wDen)
#' head(canDenOut)

nrsaCanopyDensiometer <- function(bDensiom=NULL, wDensiom=NULL, wChannelBank=c('LF','RT'), wChannelMid=c('CU','CL','CD','CR')                          
                                 ,isUnitTest = FALSE
                                 ,argSavePath = NULL
                                 )
{
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
#    2/23/16 cws Updated argument descriptions, corrected ifdf functions and
#            cleaned up comments a tad.
#   10/05/18 cws Using aquametStandardizeArgument, unit test modified accordingly
#    3/19/19 cws Added isUnitTest argument for consistency with other functions
#    3/20/19 cws Added legal value check of DIRECTION values in wDensiom argument.
#    8/07/23 cws Added argSavePath argument, as newly needed for 
#            aquametStandardizeArgument
#
# TODO: use ddply instead of summaryby. 
#
# ARGUMENTS:
# bDensiom      dataframe containing canopy densiometer counts for boatable 
#               reaches, with the following columns:
#                   SITE        integer or character specifying the site visit
#                   VALUE       numeric counts
#
# wDensiom      dataframe containing canopy densiometer counts for wadeable 
#               reaches, with the following columns:
#                   SITE        integer or character specifying the site visit
#                   DIRECTION   character value specifying the direction being
#                               faced.  These values are expected to be the same
#                               as listed in the arguments wChannelBank and
#                               wChannelMid
#                   VALUE       numeric counts
#
# wChannelBank  character values listing the values of DIRECTION which indicate
#               that the counts were obtained on the bank of the channel.  
#               Defaults to c('LF','RT').
#
# wChannelMid   character values listing the values of DIRECTION which indicate
#               that the counts were obtained midstream in the channel.  
#               Defaults to c('CU','CL','CD','CR').
# argSavePath   character string specifying the path to which data arguments are
#               saved as csv files, or NULL if those files are not to be written.
#
# Output:
#   Either a data frame when metric calculation is successful or a NULL
#   when metric calculation is not successful.  The data frame contains the 
#   following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     VALUE - metric value
#
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

  intermediateMessage('Canopy Densiometer calculations:', loc='start')

    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdfBoatable <- function(df, ...) {
        rc <- df %>% select(SITE,VALUE)
        return(rc)
    }
    ifdfWadeable <- function(df, ...) {
        rc <- df %>% select(SITE,DIRECTION,VALUE)
        return(rc)
    }

    # bDensiom = absentAsNULL(bDensiom, ifdfBoatable) 
    # wDensiom = absentAsNULL(wDensiom, ifdfWadeable)
    bDensiom <- aquametStandardizeArgument(bDensiom, 'xdepth', ifdf=ifdfBoatable
                                          ,struct=list(SITE=c('integer','character'), VALUE='double')
                                          ,rangeLimits = list(VALUE=c(0,20))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )   
    wDensiom <- aquametStandardizeArgument(wDensiom, 'xdepth', ifdf=ifdfWadeable
                                          ,struct = list(SITE=c('integer','character'), DIRECTION='character', VALUE='double')
                                          ,LegalValues = list(DIRECTION = c(wChannelBank, wChannelMid))
                                          ,rangeLimits = list(VALUE=c(0,20))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )   
    
    mdx <- NULL
    mds <- NULL
    mdc <- NULL
    bkx <- NULL
    bks <- NULL
    bkc <- NULL
    btx <- NULL
    bts <- NULL
    btc <- NULL

    intermediateMessage('.1')  
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

    intermediateMessage('.2')  
    mets <- rbind(mdx,mds,mdc,bkx,bks,bkc,btx,bts,btc)
    mets$VALUE<-ifelse(mets$VALUE=='NaN', NA, mets$VALUE)
  
    # more calcs
    mets$VALUE <- ifelse(mets$METRIC %in% c("xcdenmid", "vcdenmid", "xcdenbk","vcdenbk")
                        ,100 * mets$VALUE/17
                        ,mets$VALUE
                        )
    
    intermediateMessage('.Done', loc='end')
   
    return(mets)
}

# end of file