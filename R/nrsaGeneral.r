#' @export
#' @title Calculate NRSA General Metrics
#' @description This function calculates the general portion of 
#' the physical habitat metrics for National Rivers and Streams 
#' Assessment (NRSA) data.  The function requires data frames containing 
#' the thalweg and channel geometry data files.
#' @param sampledTransects  dataframe containing only the list of 
#' transects sampled for each site, one transect per row. Expected to 
#' contain the following columns:
#'  \itemize{
#'        \item SITE integer or character specifying the site 
#'                   visit
#'        \item TRANSECT character value specifying the transect
#'                       for which values were recorded.
#' }                       
#' @param sideChannels dataframe containing side channel presence data at each 
#' station, with the following columns:
#' \itemize{
#'          \item SITE integer or character specifying the site visit
#'          \item VALUE character values, expected to be 'Y', 'N' or NA
#' }
#' @param transectSpacing dataframe containing distances (in meters) between 
#' transects with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric values
#' }
#' @param sideChannelTransects character vector containing values of 
#' TRANSECT that are used to specify whether a transect is located on a 
#' side channel.  Default value is for EPA NARS use (XA, XB, etc.)
#' which occur parallel to main channel transects A, B, etc.
#' @param isUnitTest Logical argument to determine whether errors should be ignored.
#' Should only be used for running a unit test. Default value is FALSE.
#' @param argSavePath character string specifying the path to which data 
#' arguments are saved as csv files, or NULL if those files are not to be 
#' written.
#' @return Either a data frame when metric calculation is successful or a 
#' character string containing an error message when metric calculation is 
#' not successful.  The data frame contains the following columns:
#' \itemize{
#' \item SITE - universal ID value
#' \item METRIC - metric name
#' \item VALUE - metric value
#' }
#' 
#' Metrics calculated include: pct_side, reachlen, xtranspc, sidecnt
#' 
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#' head(thalwegEx)
#' head(changeomEx)
#' 
#' # Must carry out several calculations to provide inputs to function, 
#' # including determining spacing between transects.
#' 
#' # sampledTransects argument
#' sampTr <- unique(thalwegEx[,c('SITE','TRANSECT')])
#' # sideChannels argument - if none present, leave out
#' sideCh <- subset(thalwegEx,PARAMETER %in% c('SIDCHN','OFF_CHAN'),select=c(SITE,VALUE))
#' 
#' # Creating the transect spacing for wadeable streams is more complicated 
#' # than for boatable streams because measurements are only made up to one 
#' # station before the end of the reach. 
#' # Keep increment value for wadeable sites, only present for transect A and
#' #  station 0, then make sure it is numeric.
#' wTr.1 <- subset(thalwegEx, PARAMETER=='INCREMNT' & TRANSECT=='A' & 
#'        STATION==0 & SAMPLE_TYPE=='PHAB_THALW',select=c('SITE','VALUE')) 
#' 
#' wTr.2 <- plyr::mutate(wTr.1, VALUE=as.numeric(VALUE)) 
#' # Identify unique sampled SITE, TRANSECT, STATION combinations, then
#' #   determine the number of stations for each SITE and TRANSECT
#' uniqLoc <- unique(subset(thalwegEx, SAMPLE_TYPE=='PHAB_THALW', 
#'  select = c(SITE,TRANSECT,STATION)))
#' nStations <- stats::aggregate(x = list(nSta = uniqLoc$STATION), 
#'     by = uniqLoc[c('SITE','TRANSECT')], FUN = length)
#' # Merge number of stations back with dataset
#' wTr.3 <- merge(wTr.2, nStations, by = 'SITE') 
#' # Identify maximum TRANSECT sampled, merge back with dataset                                      
#' maxWTr <- stats::aggregate(x = list(lastTran = wTr.3$TRANSECT), by = wTr.3[c('SITE')], 
#'        FUN = function(x){max(as.character(x))})
#' wTr.4 <- merge(wTr.3, maxWTr)
#' # Calculate space between transects for each transect at wadeable sites
#' wTr.5 <- plyr::mutate(wTr.4, VALUE = ifelse(TRANSECT!=lastTran, nSta*VALUE, (nSta-1)*VALUE)) 
#' # Keep only necessary variables
#' wTr.6 <- dplyr::select(wTr.5, -nSta,-lastTran)
#' # For boatable sites, just use actual transect spacing parameter
#' bTr <- subset(changeomEx,PARAMETER=='ACTRANSP',select=c('SITE','TRANSECT','VALUE'))
#' # combine wadeable and boatable transect spacing information
#' trDist <- rbind(wTr.6,bTr) 
#' # Ensure values are numeric
#' trDist$VALUE <- as.numeric(trDist$VALUE)
#' 
#' generalOut <- nrsaGeneral(sampledTransects=sampTr, sideChannels=sideCh,
#' transectSpacing=trDist)
#' 
#' head(generalOut)

nrsaGeneral <- function(sampledTransects = NULL, sideChannels = NULL, transectSpacing = NULL
                       ,sideChannelTransects = c('XA','XB','XC','XD','XE','XF','XG','XH','XI','XJ','XK')
                       ,isUnitTest = FALSE
                       ,argSavePath = NULL
                       ) {

################################################################################
# Function: metsGeneral
# Title: Calculate NRSA General Metrics
# Programmers: Randy Hjort
#              Curt Seeliger
#              Tom Kincaid
# Date: January 4, 2010
# Description:
#   This function calculates the general portion of the physical habitat metrics
#   for National Rivers and Streams Assessment (NRSA) data.  The function
#   requires data frames containing the thalweg and channel geometry data files.
# Function Revisions:
#   01/04/10 rch: Copied, plagerized and made up this code.
#   02/18/10 cws: Removed source() of NRSAValidation.r,NA_filler.r and
#            summaryby.r
#   03/22/10 cws: Added all=TRUE argument to merge() of expected and actual
#            values in unit test.
#   04/02/10 cws: Modified unit test and metrics code to handle data with just
#            one protocol.  Added comment about change in sidecnt value in unit
#            test.
#   06/03/10 cws: Modified reachlen calculation to work with single increment
#            value at A 0 instead of one for each transect.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   02/17/11 cws: Removed calculation of SAMPLED metric, as it was an
#            unneccessary holdover from WEMAP. Changes made to both metrics code
#            and unit test.  This is made somewhat easier by the fact that this
#            metric escaped inclusion in the unit test.
#   04/08/11 cws: Using GPS based DISTANCE parameter (calculated with GIS
#            currently) to base REACHLEN on when ACTRANSP is not available.
#            Unit test updated accordingly -- many changes to function creating
#            test data, and no longer writing REACHLEN if ACTRANSP or DISTANCE
#            are NA.
#   03/08/12 cws: Changed name of output csv from metsGeneralUsingDistance.csv 
#            to metsGeneral.csv.
#   08/03/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify the names of the data files.  Added argument NRSAdir to
#            the function to identify the directory from which the data file is
#            read and to which the output metrics file is written.
#   12/21/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Modified output to be a data frame rather
#            than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   12/29/15 cws Modified calling interface for generalized use.
#    1/19/16 cws Pulled list of side channel transect values out as argument
#            'sideChannelTransects'. No change to unit test at this time.
#    1/20/16 cws No longer requiring TRANSECT column in sideChannels argument.
#    2/01/16 cws Added xtranspc = mean transect spacing to output, mostly for
#            use in nrsaResidualPools
#    2/25/16 cws Documenting arguments in comments at top. Removed old code that
#            had been commented or function(){}d out of use.
#    3/18/19 cws Changed to use aquametStandardizeArgument() instead of 
#            absentAsNull(). Also modified to treat '' values the same as NA
#            for consistency. Unit test modified accordingly.
#    8/07/23 cws Added argSavePath argument, as newly needed for 
#            aquametStandardizeArgument
#
# ARGUMENTS:
# sampledTransects  dataframe containing only the list of transects sampled for
#                   each site, one transect per row. Expected to contain the 
#                   following columns:
#                      SITE         integer or character specifying the site 
#                                   visit
#                      TRANSECT     character value specifying the transect
#                                   for which values were recorded.
#                       
# sideChannels      dataframe containing side channel presence data at each 
#                   station, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       character values, expected to be 'Y', 'N' or
#                                   NA
#
# transectSpacing   dataframe containing distances (in meters) between transects
#                   with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       numeric values
#
# sideChannelTransects character vector containing values of TRANSECT that are
#                      used to specify whether a transect is located on a side
#                      channel.  Default value is for EPA NARS use (XA, XB, etc.)
#                      which occur parallel to main channel transects A, B, etc.
#
# argSavePath   character string specifying the path to which data arguments are
#               saved as csv files, or NULL if those files are not to be written.
#
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The data frame contains the following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     VALUE - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

    intermediateMessage('General mets ', loc='start')

    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdfTransects <- function(df, ...) {
        rc <- df %>% 
              select(SITE, TRANSECT) %>% 
              unique()
        return(rc)
    }
    ifdfValues <- function(df, ...) {
        rc <- df %>% 
              select(SITE, TRANSECT, VALUE) 
        return(rc)
    }
    ifdf <- function(df, ...) {
        rc <- df %>% 
              select(SITE, VALUE) 
        return(rc)
    }
    
    sampledTransects <- aquametStandardizeArgument(sampledTransects, ifdf=ifdfTransects
                                                  ,struct = list(SITE=c('integer','character'), TRANSECT='character')
                                                  ,stopOnError = !isUnitTest
                                                  ,argSavePath = argSavePath
                                                  )
    sideChannels <- aquametStandardizeArgument(sideChannels, ifdf=ifdf
                                              ,struct = list(SITE=c('integer','character'), VALUE=c('character'))
                                              ,legalValues = list(VALUE = c(NA,'','N','Y'))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
    transectSpacing <- aquametStandardizeArgument(transectSpacing, ifdf=ifdfValues
                                                 ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('double'))
                                                 ,rangeLimits = list(VALUE = c(15,1000))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )


    # Calculate count of side channels SIDECNT (only meaningful for wadeables)
    if(is.null(sampledTransects)) {
        sidecnt <- NULL
    } else {
      
        sidecnt <- sampledTransects %>%
                   mutate(inSideChan=TRANSECT %in% sideChannelTransects) %>%
                   ddply('SITE', summarise
                        ,VALUE= protectedSum(inSideChan
                                            ,na.rm=TRUE
                                            )
                        ) %>%
                   mutate(METRIC = 'sidecnt')
    }


    intermediateMessage('.1')

    
    # Calculate percent of reach with side channels PCT_SIDE
    if(is.null(sideChannels)) {
        pct_side <- NULL
    } else {
        pct_side <- sideChannels %>%
                    subset(VALUE %in% c('Y','N','',NA)) %>%
                    mutate(standardizedPresent = VALUE=='Y') %>%
                    ddply('SITE', summarise
                         ,VALUE = 100 * protectedMean(standardizedPresent, na.rm=TRUE)
                         ) %>%
                    mutate(METRIC = 'pct_side')
    }

    intermediateMessage('.2')


    # Calculate reach length REACHLEN and mean transect spacing xtranspc
    if(is.null(transectSpacing)) {
        reachlen <- NULL
        xtranspc <- NULL
    } else {
        reachlen <- transectSpacing %>%
                    ddply('SITE', summarise
                         ,VALUE = protectedSum(as.numeric(VALUE), na.rm=TRUE)
                         ) %>%
                    mutate(METRIC = 'reachlen')
        xtranspc <- transectSpacing %>%
                    ddply('SITE', summarise
                         ,VALUE = protectedMean(as.numeric(VALUE), na.rm=TRUE)
                         ) %>%
                    mutate(METRIC = 'xtranspc')
    }

    intermediateMessage('.3')


    rc <- rbind(sidecnt, pct_side, reachlen, xtranspc)
    intermediateMessage('.Done', loc='end')

    return(rc)
}

# end of file
