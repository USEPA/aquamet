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
#'          \item TRANSECT character value specifying the transect for which 
#'              values were recorded
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
#' sampTr <- unique(thalwegEx[,c('SITE','TRANSECT')])
#' sideCh <- subset(thalwegEx,PARAMETER %in% c('SIDCHN','OFF_CHAN'))
#' 
#' # Creating the transect spacing for wadeable streams is more complicated 
#' # than for boatable streams because measurements are only made up to one 
#' # station before the end of the reach. 
#' wTr.1 <- subset(thalwegEx, PARAMETER=='INCREMNT' & TRANSECT=='A' & 
#'        STATION==0 & SAMPLE_TYPE=='PHAB_THALW',select=c('SITE','VALUE')) 
#' 
#' wTr.2 <- plyr::mutate(wTr.1, VALUE=as.numeric(VALUE)) 
#' 
#' wTr.3 <- merge(wTr.2, plyr::ddply(unique(subset(thalwegEx, SAMPLE_TYPE=='PHAB_THALW', 
#'                              select=c('SITE','TRANSECT','STATION'))),
#'              c('SITE','TRANSECT'), summarise, nSta = length(STATION)),
#'                                      by = 'SITE') 
#'                                      
#' wTr.4 <- plyr::ddply(wTr.3, c('SITE'), mutate, lastTran=max(TRANSECT)) 
#' 
#' wTr.5 <- plyr::mutate(wTr.4, VALUE = ifelse(TRANSECT!=lastTran, nSta*VALUE, (nSta-1)*VALUE)) 
#' 
#' wTr.6 <- dplyr::select(wTr.5, -nSta,-lastTran)
#' 
#' bTr <- subset(changeomEx,PARAMETER=='ACTRANSP',select=c('SITE','TRANSECT','VALUE'))
#' 
#' trDist <- rbind(wTr.6,bTr) %>% plyr::mutate(VALUE=as.numeric(VALUE))
#' 
#' generalOut <- nrsaGeneral(sampledTransects=sampTr, sideChannels=sideCh,
#' transectSpacing=trDist)
#' 
#' head(generalOut)

nrsaGeneral <- function(sampledTransects = NULL, sideChannels = NULL, transectSpacing = NULL
                       ,sideChannelTransects = c('XA','XB','XC','XD','XE','XF','XG','XH','XI','XJ','XK')
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
    
    sampledTransects <- absentAsNULL(sampledTransects, ifdfTransects)
    sideChannels <- absentAsNULL(sideChannels, ifdf)
    transectSpacing <- absentAsNULL(transectSpacing, ifdfValues)


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
                    subset(VALUE %in% c('Y','N',NA)) %>%
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
