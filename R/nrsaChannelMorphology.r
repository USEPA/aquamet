#' @export
#' @title Calculate NRSA Channel Morphology Metrics
#' @description  This function calculates the channel morphology 
#' portion of the physical habitat metrics for National Rivers 
#' and Streams Assessment (NRSA) data.  The function requires data 
#' frames containing the bank geometry, thalweg, and stream 
#' verification form data files.
#' @param bBankHeight A data frame containing bank height at each transect for
#'   boatable reaches, with the following columns:
#'  \itemize{
#'  \item SITE        integer or character specifying the site visit
#'  \item TRANSECT    character value specifying the transect
#'                         for which the value was recorded.
#'  \item VALUE       numeric or character values
#'  \item UNITS       character value specifying the units that
#'                              the value is recorded in, expected to be
#'                              either CM, M or FT.
#' }
#' @param bBankWidth A data frame containing bank width at each transect for
#' boatable reaches, with the following columns:
#' \itemize{
#'  \item SITE        integer or character specifying the site visit
#'  \item TRANSECT    character value specifying the transect
#'             for which the value was recorded.
#'  \item VALUE       numeric or character values
#'  \item UNITS       character value specifying the units that
#'                       the value is recorded in, expected to be
#'                       either CM, M or FT.
#' }
#' @param bDepth A data frame containing thalweg depth at each transect for
#' boatable reaches, with the following columns:
#' \itemize{
#'      \item SITE        integer or character specifying the site visit
#'      \item TRANSECT    character value specifying the transect
#'                       for which the value was recorded.
#'      \item STATION     numeric value specifying the station between
#'                      the transects at which the depth was recorded.
#'      \item VALUE       numeric or character values
#'      \item UNITS       character value specifying the units that
#'                       the value is recorded in, expected to be
#'                       either CM, M or FT.
#' }
#' @param bIncisedHeight A data frame containing incised height at each transect for
#' boatable reaches, with the following columns:
#' \itemize{
#'      \item SITE        integer or character specifying the site visit
#'      \item TRANSECT    character value specifying the transect
#'                       for which the value was recorded.
#'      \item VALUE       numeric or character values
#'      \item UNITS       character value specifying the units that
#'                       the value is recorded in, expected to be
#'                       either CM, M or FT.
#' }
#' @param bWettedWidth A data frame containing wetted width at each transect for
#' boatable reaches, with the following columns:
#' \itemize{
#'  \item SITE integer or character specifying the site visit
#'  \item TRANSECT character value specifying the transect
#'                for which the value was recorded.
#'  \item VALUE numeric or character values
#'  \item UNITS character value specifying the units that
#'              the value is recorded in, expected to be
#'              either CM, M or FT.
#' }
#' @param wBankHeight A data frame containing bank height at each transect for
#'  wadeable reaches, with the following columns:
#'  \itemize{
#'           \item SITE integer or character specifying the site visit
#'           \item TRANSECT character value specifying the transect
#'                          for which the value was recorded.
#'           \item VALUE numeric or character values
#'           \item UNITS character value specifying the units that
#'                       the value is recorded in, expected to be
#'                       either CM, M or FT.
#' }
#' @param wBankWidth A data frame containing bank width at each transect for
#' wadeable reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                       for which the value was recorded.
#'      \item VALUE numeric or character values
#'      \item UNITS character value specifying the units that
#'                  the value is recorded in, expected to be
#'                  either CM, M or FT.
#' }
#' @param wDepth A data frame containing thalweg depth at each transect for
#' wadeable reaches, with the following columns:
#' \itemize{
#'    \item SITE        integer or character specifying the site visit
#'    \item TRANSECT    character value specifying the transect
#'                     for which the value was recorded.
#'    \item STATION     numeric value specifying the station between
#'                     the transects at which the depth was recorded.
#'    \item VALUE       numeric or character values
#'    \item UNITS       character value specifying the units that
#'                     the value is recorded in, expected to be
#'                     either CM, M or FT.
#' }
#' @param wIncisedHeight A data frame containing incised height at each 
#' transect for wadeable reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                      for which the value was recorded.
#'      \item VALUE numeric or character values
#'      \item UNITS character value specifying the units that
#'                  the value is recorded in, expected to be
#'                  either CM, M or FT.
#' }
#' @param wWettedWidth A data frame containing wetted width at each transect 
#' for wadeable reaches, with the following columns:
#' \itemize{
#'          \item SITE integer or character specifying the site visit
#'          \item TRANSECT character value specifying the transect
#'                         for which the value was recorded.
#'          \item STATION numeric value specifying the station between
#'                     the transects at which the depth was recorded.
#'          \item VALUE numeric or character values
#'          \item UNITS character value specifying the units that
#'                      the value is recorded in, expected to be
#'                      either CM, M or FT.
#'
#' }
#' @param isUnitTest Logical argument to determine whether errors should be ignored.
#' Should only be used for running a unit test. Default value is FALSE.
#' @param argSavePath character string specifying the path to which data 
#' arguments are saved as csv files, or NULL if those files are not to be 
#' written.
#' @return Either a data frame when metric calculation is successful or a 
#' character string containing an error message when metric calculation is 
#' not successful.  The data frame contains the following columns:
#' \itemize{
#'     \item SITE - universal ID value
#'     \item METRIC - metric name
#'     \item VALUE - metric value
#' }
#' 
#' Wadeable stream metrics calculated include: bfwd_rat, n_bfrat, n_bh, 
#' n_bw, n_d, n_incis, n_w, n_wd,n_wdr, sdbkf_h, sdbkf_w, sddepth, 
#' sdinc_h, sdwd_rat, sdwidth, sdwxd,xbkf_h, xbkf_w, xdepth, 
#' xinc_h, xwd_rat, xwidth, xwxd
#' 
#' Boatable river metrics calculated include:  bfwd_rat, n_bfrat, n_bh, 
#' n_bw, n_d, n_incis,  n_wd,n_wdr,sdbkf_h, sdbkf_w, sddepth, sdinc_h, 
#' sdwd_rat, sdwidth, sdwxd,xbkf_h, xbkf_w, xdepth, xinc_h, 
#' xwd_rat, xwidth, xwxd

#' Descriptions for all NRSA metrics can be found at:
#' \href{https://github.com/USEPA/aquamet/blob/master/inst/NRSA_physical_habitat_metrics_descriptions.pdf}{NLA_Physical_Habitat_Metric_Descriptions.pdf}.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' 
#' @examples
#' head(bankgeomEx)
#' head(thalwegEx)
#' 
#' # Must subset example dataset to create inputs, keeping only SITE, TRANSECT,
#'   #  VALUE, and UNITS, then make sure VALUE is numeric.
#' # bBankHeight 
#' bBH <- subset(bankgeomEx,PARAMETER=='BANKHT' & SAMPLE_TYPE=='PHAB_CHANBFRONT',
#'         select=c(SITE,TRANSECT,VALUE,UNITS))
#'         bBH$VALUE <- as.numeric(bBH$VALUE)
#' # bBankWidth
#' bBW <- subset(bankgeomEx,PARAMETER=='BANKWID' & SAMPLE_TYPE=='PHAB_CHANBFRONT',
#'         select=c(SITE,TRANSECT,VALUE,UNITS))
#'         bBW$VALUE <- as.numeric(bBW$VALUE)
#' # bDepth
#' bD <- subset(thalwegEx,(PARAMETER=='DEP_SONR'|PARAMETER=='DEP_POLE') & 
#'         SAMPLE_TYPE=='PHAB_THAL',select=c(SITE,STATION,TRANSECT,VALUE,UNITS))
#'         bD$VALUE <- as.numeric(bD$VALUE)
#' # bIncisedHeight
#' bInc <- subset(bankgeomEx,PARAMETER=='INCISED' & SAMPLE_TYPE=='PHAB_CHANBFRONT',
#'         select=c(SITE,TRANSECT,VALUE,UNITS))
#'         bInc$VALUE <- as.numeric(bInc$VALUE)
#' # bWettedWidth
#' bWW <- subset(bankgeomEx,PARAMETER=='WETWID' & SAMPLE_TYPE=='PHAB_CHANBFRONT',
#'         select=c(SITE,TRANSECT,VALUE,UNITS))
#'         bWW$VALUE <- as.numeric(bWW$VALUE)
#' # wBankHeight
#' wBH <- subset(bankgeomEx,PARAMETER=='BANKHGT' & SAMPLE_TYPE=='PHAB_CHANW',
#'         select=c(SITE,TRANSECT,VALUE,UNITS))
#'         wBH$VALUE <- as.numeric(wBH$VALUE)
#' # wBankWidth
#' wBW <- subset(bankgeomEx,PARAMETER=='BANKWID' & SAMPLE_TYPE=='PHAB_CHANW',
#'         select=c(SITE,TRANSECT,VALUE,UNITS))
#'         wBW$VALUE <- as.numeric(wBW$VALUE)
#' # wDepth
#' wD <- subset(thalwegEx,PARAMETER=='DEPTH' & SAMPLE_TYPE=='PHAB_THALW',
#'         select=c(SITE,STATION,TRANSECT,VALUE,UNITS))
#'         wD$VALUE <- as.numeric(wD$VALUE)
#' # wIncisedHeight
#' wInc <- subset(bankgeomEx,PARAMETER=='INCISHGT' & SAMPLE_TYPE=='PHAB_CHANW',
#'         select=c(SITE,TRANSECT,VALUE,UNITS))
#'         wInc$VALUE <- as.numeric(wInc$VALUE)
#' # wWettedWidth
#' wWW <- subset(thalwegEx,PARAMETER=='WETWIDTH', 
#'         select=c(SITE,STATION,TRANSECT,VALUE,UNITS))
#'         wWW$VALUE <- as.numeric(wWW$VALUE)
#' 
#' chanmorphOut <- nrsaChannelMorphology(bBankHeight=bBH, bBankWidth=bBW,
#' bDepth=bD, bIncisedHeight=bInc, bWettedWidth=bWW, wBankHeight=wBH,
#' wBankWidth=wBW, wDepth=wD, wIncisedHeight=wInc, wWettedWidth=wWW)
#' 
#' head(chanmorphOut)

nrsaChannelMorphology <- function(bBankHeight = NULL
                                 ,bBankWidth = NULL
                                 ,bDepth = NULL
                                 ,bIncisedHeight = NULL
                                 ,bWettedWidth = NULL
                                 ,wBankHeight = NULL
                                 ,wBankWidth = NULL
                                 ,wDepth = NULL
                                 ,wIncisedHeight = NULL
                                 ,wWettedWidth = NULL
                                 ,isUnitTest = FALSE
                                 ,argSavePath = NULL
                                 ) {

################################################################################
# Function: nrsaChannelMorphology
# Title: Calculate NRSA Channel Morphology Metrics
# Programmers: Marlys Cappert
#              Curt Seeliger
#              Tom Kincaid
# Date: January 28, 2010
# Description:
#   This function calculates the channel morphology portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires data frames containing the bank geometry, thalweg, and
#   stream verification form data files.
# Stream Metrics:
#   bfwd_rat, n_bfrat, n_bh, n_bw, n_d, n_incis, n_w, n_wd,n_wdr, sdbkf_h,
#   sdbkf_w, sddepth, sdinc_h, sdwd_rat, sdwidth, sdwxd,xbkf_h, xbkf_w, xdepth,
#   xinc_h, xwd_rat, xwidth, xwxd
# River Metrics:
#   bfwd_rat, n_bfrat, n_bh, n_bw, n_d, n_incis,  n_wd,n_wdr,sdbkf_h, sdbkf_w,
#   sddepth, sdinc_h, sdwd_rat, sdwidth, sdwxd,xbkf_h, xbkf_w, xdepth, xinc_h,
#   xwd_rat, xwidth, xwxd
# Function Revisions:
#   01/28/10 mrc: Started.
#   03/17/10 mrc: Recalculate mean and sd mets with appropriate use of side.
#            channels. Replace upData with rename.
#   03/26/10 cws Moved creation of test dataframes to separate functions.
#            Modified test data and metrics code to reflect thalweg
#            station numbering starting at 0 instead of at 1 as in EMAP.
#            Modifying calculation and test code so it works with real data.
#   03/30/10 cws: Cleaned up on.exit() call of metsChannelMorphology.cleanup().
#   04/01/10 cws: Modified unit test and metrics calculations to handle data
#            with only one protocol.
#   04/13/10 cws: Converting xdepth and sddepth calculations from m to cm as
#            requested. Modified unit test accordingly.
#   09/16/10 cws: Removing hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   01/11/11 cws: Changed units in metsChannelMorphology.createBankGeometryData
#            from NONE to M.  Changed thalweg PARAMETER WETWID to WETWIDTH to
#            match data in unit test.
#   10/21/11 cws: Modified unit test to use correct parameters DEP_SONR and 
#            DEP_POLE instead of DEPTH, and have data in FT for for DEP_POLE.  
#            Previously, this resulted in not detecting a bug in the 
#            calculations that resulted in river depths in feet not being 
#            converted correctly to meters.  Values and units are changed in 
#            code rather than while creating the dataframes, just to get this 
#            done, but explicitely changed bfwd_rat at UID 8 to 39.01601245
#            and at UID 9 from 26.5309389 to 35.63173461.
#   10/25/11 cws: Ammended above modification as follows: test thalweg data
#            using SAMPLE_TYPE = PHAB_THAL,PHAB_THALW instead of PHAB_CHANW,
#            PHAB_CHANBFRONT; PARAMETER is DEPTH instead of DEP_SONR for
#            UIDs 1:6.  Ran test thalweg data through valStructThalweg()
#            which shows boatable reaches have unexpected stations 15:19,
#            which is not a problem.  Calculations accidentally modified 
#            earlier to use DEP_SONR instead of DEPTH throughout code;
#            this was also changed.
#   08/03/12 tmk: Removed use of ODBC data connection and replaced with data
#            input from csv files using a call to function read.csv.  Added
#            argument tbl to the function to identify names of the data files.
#            Added argument NRSAdir to the function to identify the directory
#            from which data files are read and to which the output metrics file
#            is written.
#   12/14/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Modified output to be a data frame rather
#            than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   06/03/14 cws: Handling NA UNITS values gracefully by translating NA to ""
#            so that the units conversion of cm and ft to m doesn't set RESULT 
#            to NA.
#   11/20/15 cws Created with new calling interface, and updated unit test. This 
#            code made my eyes bleed.
#    2/25/16 cws Updated argument descriptions, and cleaned up comments a tad.
#    3/17/16 cws Removed old commented out code
#   11/07/16 cws Added metrics xdepth_cm and sddepth_cm. New code at tail end of
#            this scabbed-in mess
#    3/12/19 cws Changed to use aquametStandardizeArgument() instead of 
#            absentAsNull().
#    3/22/19 cws Modified to use dplyr::rename()
#    8/07/23 cws Added argSavePath argument, as newly needed for 
#            aquametStandardizeArgument
#    3/19/25 cws protecting calculation of ratios from division by zero which
#            results in inf as a result. No change was made to the unit test.
#    3/20/25 filtering output of mean throughout this function to change NaN to 
#            NA. Also using protectedSum to combine main and sidechannel values
#            so sum of NA becomes NA rather than 0. Max wetted width at station
#            on wadeable reaches now use protectedMax and na.rm=TRUE
#            
# ARGUMENTS:
# bBankHeight       dataframe containing bank height at each transect for
#                   boatable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
#
# bBankWidth        dataframe containing bank height at each transect for
#                   boatable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
#
# bDepth            dataframe containing thalweg depth at each transect for
#                   boatable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       STATION     numeric value specifying the station between
#                                   the transects at which the depth was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
#
# bIncisedHeight    dataframe containing incised height at each transect for
#                   boatable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
#
# bWettedWidth      dataframe containing wetted width at each transect for
#                   boatable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
#
# wBankHeight       dataframe containing bank height at each transect for
#                   wadeable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
#
# wBankWidth        dataframe containing bank height at each transect for
#                   wadeable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
#
# wDepth            dataframe containing thalweg depth at each transect for
#                   wadeable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       STATION     numeric value specifying the station between
#                                   the transects at which the depth was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
#
# wIncisedHeight    dataframe containing incised height at each transect for
#                   wadeable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
#
# wWettedWidth      dataframe containing wetted width at each transect for
#                   wadeable reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       numeric or character values
#                       UNITS       character value specifying the units that
#                                   the value is recorded in, expected to be
#                                   either CM, M or FT.
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

    intermediateMessage ('metsChannelMorphology started', loc='start')
    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdf <- function(df, ...) {
        args <- list(...)
        pName <- args[[1]]
        rc <- df %>% 
              select(SITE,TRANSECT,VALUE,UNITS) %>% 
              mutate(STATION = 'NONE'
                    ,UNITS = ifelse(is.na(UNITS), '', UNITS)
                    ,VALUE = ifelse(UNITS=='CM', as.numeric(VALUE)/100
                            ,ifelse(UNITS=='FT', as.numeric(VALUE)*0.3048, as.numeric(VALUE) 
                             ))
                    ,PARAMETER = pName
                    )
        return(rc)
    }
    ifDepthDf <- function(df, ...) {
        args <- list(...)
        pName <- args[[1]]
        rc <- df %>% 
              select(SITE,TRANSECT,STATION,VALUE,UNITS) %>% 
              mutate(UNITS = ifelse(is.na(UNITS), '', UNITS)
                    ,VALUE = ifelse(UNITS=='CM', as.numeric(VALUE)/100
                            ,ifelse(UNITS=='FT', as.numeric(VALUE)*0.3048, as.numeric(VALUE) 
                             ))
                    ,PARAMETER = pName
                    )
        return(rc)
    }

    # bBankHeight     <- aquametStandardizeArgument(bBankHeight, ifdf, 'BANKHGT') # adding PARAMETER columns is just temporary until we rewrite what follows...
    # bBankWidth      <- absentAsNULL(bBankWidth, ifdf, 'BANKWID')
    # bDepth          <- absentAsNULL(bDepth, ifDepthDf, 'DEPTH')
    # bIncisedHeight  <- absentAsNULL(bIncisedHeight, ifdf, 'INCISHGT')
    # bWettedWidth    <- absentAsNULL(bWettedWidth, ifdf, 'WETWID')
    # wBankHeight     <- absentAsNULL(wBankHeight, ifdf, 'BANKHGT')
    # wBankWidth      <- absentAsNULL(wBankWidth, ifdf, 'BANKWID')
    # wDepth          <- absentAsNULL(wDepth, ifDepthDf, 'DEPTH')
    # wIncisedHeight  <- absentAsNULL(wIncisedHeight, ifdf, 'INCISHGT')
    # wWettedWidth    <- absentAsNULL(wWettedWidth, ifDepthDf, 'WETWID')
    bBankHeight     <- aquametStandardizeArgument(bBankHeight, ifdf = ifdf, 'BANKHGT'
                                                 ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,15))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )
    bBankWidth      <- aquametStandardizeArgument(bBankWidth, ifdf = ifdf, 'BANKWID'
                                                 ,struct = list(SITE = c('integer','character'), TRANSECT='character', VALUE = c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,200))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )
    bDepth          <- aquametStandardizeArgument(bDepth, ifdf = ifDepthDf, 'DEPTH'
                                                 ,struct = list(SITE = c('integer','character'), TRANSECT='character', STATION=c('integer','character'), VALUE = c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,30))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )
    bIncisedHeight  <- aquametStandardizeArgument(bIncisedHeight, ifdf = ifdf, 'INCISHGT'
                                                 ,struct = list(SITE = c('integer','character'), TRANSECT='character', VALUE = c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,10))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )
    bWettedWidth    <- aquametStandardizeArgument(bWettedWidth, ifdf = ifdf, 'WETWID'
                                                 ,struct = list(SITE = c('integer','character'), TRANSECT='character', VALUE = c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,200))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )
    wBankHeight     <- aquametStandardizeArgument(wBankHeight, ifdf = ifdf, 'BANKHGT'
                                                 ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,5))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )
    wBankWidth      <- aquametStandardizeArgument(wBankWidth, ifdf = ifdf, 'BANKWID'
                                                 ,struct = list(SITE = c('integer','character'), TRANSECT='character', VALUE = c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,80))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )
    wDepth          <- aquametStandardizeArgument(wDepth, ifdf = ifDepthDf, 'DEPTH'
                                                 ,struct = list(SITE = c('integer','character'), TRANSECT='character', STATION=c('integer','character'), VALUE = c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,200))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )
    wIncisedHeight  <- aquametStandardizeArgument(wIncisedHeight, ifdf = ifdf, 'INCISHGT'
                                                 ,struct = list(SITE = c('integer','character'), TRANSECT='character', VALUE = c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,10))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )
    wWettedWidth    <- aquametStandardizeArgument(wWettedWidth, ifdf = ifDepthDf, 'WETWID'
                                                 ,struct = list(SITE = c('integer','character'), TRANSECT='character', STATION=c('integer','character'), VALUE = c('integer','double'), UNITS='character')
                                                 ,rangeLimits = list(VALUE = c(0,60))
                                                 ,stopOnError = !isUnitTest
                                                 ,argSavePath = argSavePath
                                                 )

    local_protectedMax <- function(x, na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE, ...) {
      # Calculates max using max(), but can trap additional incalculable values.
      # Taken from sharedCode outside of aquamet for now
      #
      # Note: The default values for dealing with incalculable values mirrors mean() 
      # while also providing protection against NaN results from a max of zero
      # nonmissing values.  
      #
      # ARGUMENTS:
      # x			vector argument to mean
      # na.rm		logical, calculation ignores NA values if TRUE
      # inf.rm	logical, calculation ignores Inf, -Inf values if TRUE
      # nan.rm	logical, calculation ignores NaN values if TRUE
      # ...		additional arguments passed to or from other methods, e.g. trim
      
      if(inf.rm & length(x) > 0) {
        x <- x[!is.infinite(x)]
      }
      
      if(nan.rm & length(x) > 0) {
        x <- x[!is.nan(x)]
      }
      
      if (length(x) == 0) {
        rc <- as.numeric(NA)
      }
      else if(all(is.na(x)))	{
        rc <- as.numeric(NA)
      }
      else {
        #print(sprintf("max(%s)", paste(x, collapse=',')))
        rc <- max(x, na.rm=na.rm, ...)	# max of all NA is -Inf, should be NA
      }
      
      return(rc)
      
    }
    
    intermediateMessage ('.1')

    # Temporarily reconstuct old data frames and use old code.  THIS WILL STILL
    # BREAK WHEN USERS TO SUPPLY INCOMPLETE DATA, so it needs changing.
    boat <- rbind(bBankHeight 
                 ,bBankWidth 
                 ,bDepth 
                 ,bIncisedHeight 
                 ,bWettedWidth 
                 )
    wade <- rbind(wBankHeight 
                 ,wBankWidth 
                 ,wDepth 
                 ,wIncisedHeight
                 ,wWettedWidth 
                 )

    intermediateMessage ('.2')
      
 
  # these summaries do use the sidechannels, but is odd an different ways.
  # for all of these, keep the side channels and then either pick the mean, max, or sum
  # of the transect pair, depending on the metric
  if(!is.null(wade)>0) {
      # this data (wwidnox1 is used to calculate: xwidth, sdwidth, n_w (max of
      # the sidechannel pair)
      wadenox <- within(wade, TRANSECT<-gsub('X' ,'',TRANSECT))
      wwidnox <- subset (wadenox, wadenox$PARAMETER=='WETWID')
      wwidnox1 <- aggregate(list(VALUE=wwidnox$VALUE)
                       ,list('SITE'=wwidnox$SITE, 'TRANSECT'=wwidnox$TRANSECT, 'STATION'=wwidnox$STATION)
                       ,local_protectedMax, na.rm=TRUE
                       )
#      wwidnox1 <- rename(wwidnox1, 'x', 'VALUE')

      #used later, below for some aggregate metrics, but calculated here for
      # organizational purposes
      #          (sum of the sidechannel pair)
      wwidnox <- subset (wadenox, wadenox$PARAMETER=='WETWID')
      wwidnox1s <- aggregate(list(VALUE=wwidnox$VALUE)
                       ,list('SITE'=wwidnox$SITE
                            ,'TRANSECT'=wwidnox$TRANSECT
                            ,'STATION'=wwidnox$STATION
                            )
                       ,protectedSum, na.rm=TRUE
                       )
#      wwidnox1s <- rename(wwidnox1s, 'x', 'VALUE')

      #this data (bwidnox1 is used to calculate:xbkf_w, sdbkf_w, n_bw (sum of the sidechannel pair))
      bwidnox <- subset (wadenox, wadenox$PARAMETER=='BANKWID')
      bwidnox1 <- aggregate(list(VALUE=bwidnox$VALUE)
                           ,list('SITE'=bwidnox$SITE, 'TRANSECT'=bwidnox$TRANSECT)
                           ,protectedSum , na.rm=TRUE
                           )
#      bwidnox1 <- rename(bwidnox1, 'x', 'VALUE')
 
      #this data (incnox1 is used to calculate: xinc_h, sdinc_h, n_inc (max of
      # the sidechannel pair))
      inchnox <- subset (wadenox, wadenox$PARAMETER=='INCISHGT')
      inchnox1 <- aggregate(list(VALUE=inchnox$VALUE)
                           ,list('SITE'=inchnox$SITE, 'TRANSECT'=inchnox$TRANSECT)
                           ,local_protectedMax, na.rm=TRUE
                           )
#      inchnox1 <- rename(inchnox1, 'x', 'VALUE')

      # this data (bhgtnox1 is used to calculate: xbkf_h, sdbkf_h (max of the
      # sidechannel pair))
      bhgtnox <- subset (wadenox, wadenox$PARAMETER=='BANKHGT')
      bhgtnox1 <- aggregate(list(VALUE=bhgtnox$VALUE)
                       ,list('SITE'=bhgtnox$SITE, 'TRANSECT'=bhgtnox$TRANSECT 
                       )
                       ,local_protectedMax, na.rm=TRUE 
                       )
#      bhgtnox1 <- rename(bhgtnox1, 'x', 'VALUE')
     
      # used later, below for some aggregate metrics, but calculated here for
      # organizational purposes
      #                                      (mean of the sidechannel pair)
      bhgtnox1x <- aggregate(list(VALUE=bhgtnox$VALUE)
                            ,list('SITE'=bhgtnox$SITE, 'TRANSECT'=bhgtnox$TRANSECT)
                            ,protectedMean, na.rm=TRUE
                            )
#      bhgtnox1x <- rename(bhgtnox1x, 'x', 'VALUE')
     
  }

  if(!is.null(boat)>0) {
      # subset the boat data
      boatnox <- within(boat, TRANSECT<-gsub('X' ,'',TRANSECT))

      # this data (wwidnoxb1 is used to calculate: xwidth, sdwidth, n_w (max of
      # the sidechannel pair))
      wwidnoxb <- subset (boatnox, boatnox$PARAMETER=='WETWID')
      wwidnoxb1 <- aggregate(list(VALUE=wwidnoxb$VALUE)
                            ,list('SITE'=wwidnoxb$SITE
                                 ,'TRANSECT'=wwidnoxb$TRANSECT
                                 )
                            ,local_protectedMax, na.rm=TRUE
                            )
#      wwidnoxb1 <- rename(wwidnoxb1, 'x', 'VALUE')

      # used later, below for some aggregate metrics, but calculated here for
      # organizational purposes
      #                                         (sum of the sidechannel pair)
      wwidnoxb1s <- aggregate(list(VALUE=wwidnoxb$VALUE)
                             ,list('SITE'=wwidnoxb$SITE
                                  ,'TRANSECT'=wwidnoxb$TRANSECT
                                  )
                             ,protectedSum, na.rm=TRUE
                             )
#      wwidnoxb1s <- rename(wwidnoxb1s, 'x', 'VALUE')

      # this data (bwidnoxb1 is used to calculate: bkf_w, sdbkf_w, n_bw (sum of
      # the sidechannel pair))
      bwidnoxb <- subset (boatnox, boatnox$PARAMETER=='BANKWID')
      bwidnoxb1 <- aggregate(list(VALUE=bwidnoxb$VALUE)
                            ,list('SITE'=bwidnoxb$SITE
                                 ,'TRANSECT'=bwidnoxb$TRANSECT
                                 )
                            ,protectedSum , na.rm=TRUE
                            )
#      bwidnoxb1 <- rename(bwidnoxb1, 'x', 'VALUE')
    
      # this data (incnoxb1 is used to calculate: xinc_h, sdinc_h, n_inc (max of the sidechannel pair)
 
      inchnoxb <- subset (boatnox, boatnox$PARAMETER=='INCISHGT')
      inchnoxb1 <- aggregate(list(VALUE=inchnoxb$VALUE)
                            ,list('SITE'=inchnoxb$SITE
                                 ,'TRANSECT'=inchnoxb$TRANSECT
                                 )
                            ,local_protectedMax, na.rm=TRUE
                            )
#      inchnoxb1 <- rename(inchnoxb1, 'x', 'VALUE')
      # this data (bhgtnoxb1 is used to calculate:xbkf_h, sdbkf_h, (max of the
      # sidechannel pair))
      bhgtnoxb <- subset (boatnox, boatnox$PARAMETER=='BANKHGT')
      bhgtnoxb1 <- aggregate(list(VALUE=bhgtnoxb$VALUE)
                            ,list('SITE'=bhgtnoxb$SITE
                                 ,'TRANSECT'=bhgtnoxb$TRANSECT
                                 )
                            ,local_protectedMax, na.rm=TRUE
                            )
#      bhgtnoxb1 <- rename(bhgtnoxb1, 'x', 'VALUE')
     
      # used later, below for some aggregate metrics, but calculated here for
      # organizational purposes
      #                                 (mean of the sidechannel pair)
      bhgtnoxb1x <- aggregate(list(VALUE=bhgtnoxb$VALUE)
                             ,list('SITE'=bhgtnoxb$SITE
                                  ,'TRANSECT'=bhgtnoxb$TRANSECT
                                  )
                             ,protectedMean, na.rm=TRUE
                             )
#      bhgtnoxb1x <- rename(bhgtnoxb1x, 'x', 'VALUE')
  }

  # special case to fix an error in the field manual.  If the INCISED
  # height is 0 or NA, the INCISED height is = BANKHGT, assuming BANKHGT > 0)
  if(!is.null(wade)>0) {
      #wade
      incbank <- merge(inchnox1, bhgtnox1
                      ,by=c('SITE','TRANSECT'), all.x=TRUE, all.y=FALSE
                      )

      incbank$VALUE.x <- ifelse (is.na(incbank$VALUE.x), 0, incbank$VALUE.x)
      incbank$VALUE.x <- ifelse (incbank$VALUE.x<=0
                                 ,incbank$VALUE.y
                                 ,incbank$VALUE.x
                                 )
     
      inchnox1 <- incbank
 
      inchnox1$VALUE.y <- NULL

      inchnox1 <- dplyr::rename (inchnox1, VALUE=VALUE.x) #'VALUE.x', 'VALUE')
  }

  if(!is.null(boat)>0) {
       #boat
       incbankb <- merge(inchnoxb1, bhgtnoxb1
                        ,by=c('SITE','TRANSECT'), all.x=TRUE, all.y=FALSE
                        )

       incbankb$VALUE.x <- ifelse (is.na(incbankb$VALUE.x)
                                   ,0
                                   ,incbankb$VALUE.x
                                   )
       incbankb$VALUE.x <- ifelse (incbankb$VALUE.x<=0
                                   ,incbankb$VALUE.y
                                   ,incbankb$VALUE.x)

       ihgtb <- incbankb
 
       ihgtb$VALUE.y <- NULL
 
       ihgtb <- dplyr::rename (ihgtb, VALUE=VALUE.x) # 'VALUE.x', 'VALUE')
  }

  intermediateMessage ('.3') 

#sent these to summaryby, summaries by the transect
  intest <- NULL
  if(!is.null(wade)>0) {
      #wade
      wwidx <- summaryby(wwidnox1,'mean',"xwidth") %>% mutate(VALUE = ifelse(is.nan(VALUE), as.numeric(NA), VALUE))
      wwidsd <- summaryby(wwidnox1,'sd',"sdwidth")
      wwidc <- summaryby(wwidnox1,'count',"n_w")

      bwidx <- summaryby(bwidnox1,'mean',"xbkf_w") %>% mutate(VALUE = ifelse(is.nan(VALUE), as.numeric(NA), VALUE))
      bwidsd <- summaryby(bwidnox1,'sd',"sdbkf_w")
      bwidc <- summaryby(bwidnox1,'count',"n_bw")
#print('debugstuff bwidnox1 2024182'); print(subset(bwidnox1, SITE==2024182)) ; print(subset(bwidc, SITE==2024182))    
           
      bhidx <- summaryby(bhgtnox1,'mean',"xbkf_h") %>% mutate(VALUE = ifelse(is.nan(VALUE), as.numeric(NA), VALUE))
      bhidsd <- summaryby(bhgtnox1,'sd',"sdbkf_h")
      bhidc <- summaryby(bhgtnox1,'count',"n_bh")
            
      ihidx <- summaryby(inchnox1,'mean',"xinc_h") %>% mutate(VALUE = ifelse(is.nan(VALUE), as.numeric(NA), VALUE))
      ihidsd <- summaryby(inchnox1,'sd',"sdinc_h")
      ihidc <- summaryby(inchnox1,'count',"n_incis")

      intest <- rbind (wwidx,wwidsd, wwidc
                      ,bwidx, bwidsd, bwidc
                      ,bhidx, bhidsd, bhidc
                      ,ihidx, ihidsd, ihidc
                      )
  }
  if(!is.null(boat)) {
      #boat
      wwidxb <- summaryby(wwidnoxb1,'mean',"xwidth") %>% mutate(VALUE = ifelse(is.nan(VALUE), as.numeric(NA), VALUE))
      wwidsdb <- summaryby(wwidnoxb1,'sd',"sdwidth")
      wwidcb <- summaryby(wwidnoxb1,'count',"n_w")

      bwidxb <- summaryby(bwidnoxb1,'mean',"xbkf_w") %>% mutate(VALUE = ifelse(is.nan(VALUE), as.numeric(NA), VALUE))
      bwidsdb <- summaryby(bwidnoxb1,'sd',"sdbkf_w")
      bwidcb <- summaryby(bwidnoxb1,'count',"n_bw")

      bhidxb <- summaryby(bhgtnoxb1,'mean',"xbkf_h") %>% mutate(VALUE = ifelse(is.nan(VALUE), as.numeric(NA), VALUE))
      bhidsdb <- summaryby(bhgtnoxb1,'sd',"sdbkf_h")
      bhidcb <- summaryby(bhgtnoxb1,'count',"n_bh")

      ihidxb <- summaryby(inchnoxb1,'mean',"xinc_h") %>% mutate(VALUE = ifelse(is.nan(VALUE), as.numeric(NA), VALUE))
      ihidsdb <- summaryby(inchnoxb1,'sd',"sdinc_h")
      ihidcb <- summaryby(inchnoxb1,'count',"n_incis")

      intest <- rbind (intest,wwidxb, wwidsdb, wwidcb
                      ,bwidxb, bwidsdb, bwidcb
                      ,bhidxb, bhidsdb, bhidcb
                      ,ihidxb, ihidsdb, ihidcb
                      )
  }


  intermediateMessage ( '.4  Complete summaryby summaries.')

  #Do some resahping to calculate the ratios
  #need to use the data WITH the SIDE Channels for this part
  #the side channels are used as above with some slight variations.
  #for BANKHGT use the mean of the sidechannel pair  (bhgtnox1x)
  #for BANKWID use the sum of the sidechannel pair   (bwidnox1)
  #for WETWID use the sum of the sidechannel pair    (wwidnox1s)

  #need to do each protocol separately

  #need to put back together the datasets after the sidechannels have been figured out.
  #need wetwid, bankhgt, bankwid, and depth with the appropriate side channel treatment
  wadeableRatios <- NULL
  if(!is.null(wade)>0) {
      #for wetwid, we use the SUM of the sidechannel pair
      wwidnox1s$PARAMETER <- 'WETWID'
      wwidnox1s$UNITS <- 'M'

      #for bankhgt, we use the MEAN of the sidechannel pair
      bhgtnox1x$PARAMETER <- 'BANKHGT'
      bhgtnox1x$STATION <- 'NONE'
      bhgtnox1x$UNITS <- 'NONE'

      #for bankwid, we ust the SUM of the sidechannel pair.

      bwidnox1$PARAMETER <- 'BANKWID'
      bwidnox1$STATION <- 'NONE'
      bwidnox1$UNITS <- 'NONE'

      wadezero <- subset (wade, wade$STATION %in% c('0', 'NONE'))
  
      wadezerow1 <- subset (wadezero, PARAMETER!='WETWID')
      wadezerow2 <- subset (wwidnox1s, STATION=='0')
      wadezeroww <- rbind (wadezerow1, wadezerow2)

      wadezerobh1 <- subset (wadezeroww, PARAMETER!='BANKHGT')
      wadezerobh <- rbind (wadezerobh1, bhgtnox1x)

      wadezerobw1 <- subset (wadezerobh, PARAMETER!='BANKWID')
      wadezerobw <- rbind (wadezerobw1, bwidnox1)

      wadew1 <- subset (wade, PARAMETER!='WETWID')
      wadeww <- rbind (wadew1, wwidnox1s)

      wadebh1 <- subset (wadeww, PARAMETER!='BANKHGT')
      wadebh <- rbind (wadebh1, bhgtnox1x)

      wadebw1 <- subset (wadebh, PARAMETER!='BANKWID')
      wadebw <- rbind (wadebw1, bwidnox1)

      intermediateMessage ('.5')

      rs1w0 <- reshape(subset (wadezerobw, select= - STATION), idvar=c('SITE','TRANSECT'), direction='wide', timevar='PARAMETER')
      names(wadezerobw) <- gsub('VALUE\\.', '', names(wadezerobw))
  
      rs1w <- reshape(wadebw, idvar=c('SITE','TRANSECT','STATION'), direction='wide', timevar='PARAMETER')
      names(wadebw) <- gsub('VALUE\\.', '', names(wadebw))
 
      #generate seed values to later calculate ratios... see use of sidechannel data as described above.
      rs1w0$bfwd_ratseed <- ifelse(rs1w0$VALUE.BANKHGT %in% 0 & rs1w0$VALUE.DEPTH %in% 0
                                  ,NA
                                  ,(rs1w0$VALUE.BANKWID/(rs1w0$VALUE.BANKHGT+rs1w0$VALUE.DEPTH))
                                  )
  

      rs1w$wdprod <- (rs1w$VALUE.WETWID * rs1w$VALUE.DEPTH)
      rs1w$wdratio <- ifelse(rs1w$VALUE.DEPTH %in% 0
                            ,NA
                            ,(rs1w$VALUE.WETWID / rs1w$VALUE.DEPTH)
                            )

      intermediateMessage ('.6')

      # next summarize (means)the ratios/products
      mn <- aggregate(list(VALUE=rs1w0$bfwd_ratseed)
                     ,list('SITE'=rs1w0$SITE)
                     ,protectedMean , na.rm=TRUE
                     )
    
      mn$METRIC <- 'bfwd_rat'

#      mn <- rename (mn, 'x', 'VALUE')
      mn1 <- aggregate(list(VALUE=rs1w$wdprod)
                      ,list('SITE'=rs1w$SITE)
                      ,protectedMean, na.rm=TRUE
                      )
    
      mn1$METRIC <- 'xwxd'
#      mn1 <- rename(mn1, 'x', 'VALUE')
    
      mn2 <- aggregate(list(VALUE=rs1w$wdratio)
                      ,list('SITE'=rs1w$SITE)
                      ,protectedMean, na.rm=TRUE
                      )

      mn2$METRIC <- 'xwd_rat'
#      mn2 <- rename(mn2, 'x', 'VALUE')
    
      #standard deviations (product and ratio)
    
      sd1 <- aggregate(list(VALUE=rs1w$wdprod)
                      ,list('SITE'=rs1w$SITE)
                      ,sd , na.rm=TRUE
                      )
    
      sd1$METRIC <- 'sdwxd'
#      sd1 <- rename(sd1, 'x', 'VALUE')
    
      sd2 <- aggregate(list(VALUE=rs1w$wdratio)
                      ,list('SITE'=rs1w$SITE)
                      ,sd , na.rm=TRUE
                      )
    
      sd2$METRIC <- 'sdwd_rat'
#      sd2 <- rename(sd2, 'x', 'VALUE')
    
    
      # counts for later calculations
      cc <- aggregate(list(VALUE=rs1w0$bfwd_ratseed)
                     ,list('SITE'=rs1w0$SITE)
                     ,count
                     )
       cc$METRIC <- 'n_bfrat'
#       cc <- rename(cc, 'x', 'VALUE')

      cc1 <- aggregate(list(VALUE=rs1w$wdprod)
                      ,list('SITE'=rs1w$SITE)
                      ,count
                      )
    
      cc1$METRIC <- 'n_wd'
#      cc1 <- rename(cc1, 'x', 'VALUE')
    
      cc2 <- aggregate(list(VALUE=rs1w$wdratio)
                      ,list('SITE'=rs1w$SITE)
                      ,count
                      )
    
      cc2$METRIC <- 'n_wdr'
#      cc2 <- rename(cc2, 'x', 'VALUE')

      wadeableRatios <- rbind(mn, mn1, mn2, sd1, sd2, cc, cc1, cc2)
      intermediateMessage ('.7')
  }

  boatableRatios <- NULL
  if(!is.null(boat) > 0) {
      # do this for the boat sites
      # for wetwid, we use the SUM of the sidechannel pair
      wwidnoxb1s$PARAMETER <- 'WETWID'
      wwidnoxb1s$STATION <- '0'
      wwidnoxb1s$UNITS <- 'M'

      # for bankhgt, we use the MEAN of the sidechannel pair
      bhgtnoxb1x$PARAMETER <- 'BANKHGT'
      bhgtnoxb1x$STATION <- 'NONE'
      bhgtnoxb1x$UNITS <- 'NONE'

      # for bankwid, we ust the SUM of the sidechannel pair.
      bwidnoxb1$PARAMETER <- 'BANKWID'
      bwidnoxb1$STATION <- 'NONE'
      bwidnoxb1$UNITS <- 'NONE'
 
      boatw1 <- subset (boat, PARAMETER!='WETWID')
      boatww <- rbind (boatw1, wwidnoxb1s)

      boatbh1 <- subset (boatww, PARAMETER!='BANKHGT')
      boatbh <- rbind (boatbh1, bhgtnoxb1x)
 
      boatbw1 <- subset (boatbh, PARAMETER!='BANKWID')
      boatbw <- rbind (boatbw1, bwidnoxb1)

      boatbwminus <- subset (boatbw, boatbw$PARAMETER!='DEPTH')
      boatbwminus$STATION <- 0
 
      rs1bminus<- reshape (subset(boatbwminus), idvar=c('SITE','TRANSECT', 'STATION'), direction='wide', timevar='PARAMETER')
      names(boatbwminus) <- gsub('VALUE\\.', '', names(boatbwminus))
 
      # put depth back in
      boatbwdepth <- subset (boatbw, boatbw$PARAMETER=='DEPTH' & STATION=='0', select=-STATION)
  
      intermediateMessage ('.8')
  
      rs1b <-  merge (rs1bminus, boatbwdepth, by= c('SITE', 'TRANSECT'), all=TRUE)

      rs1b <- dplyr::rename(rs1b, VALUE.DEPTH=VALUE) #'VALUE', 'VALUE.DEPTH')
  
      # do these for the boat

      rs1b$bfwd_ratseed <- (rs1b$VALUE.BANKWID/(rs1b$VALUE.BANKHGT+rs1b$VALUE.DEPTH))

      rs1b$wdprod <- (rs1b$VALUE.WETWID * rs1b$VALUE.DEPTH)  #####CHANGED: Use WETWID, not BANKWID #####
      rs1b$wdratio <- (rs1b$VALUE.WETWID / rs1b$VALUE.DEPTH) #####CHANGED: Use WETWID, not BANKWID #####

      intermediateMessage ('.10')

      # next summarize (means)the ratios/products
      mnb <- aggregate(list(VALUE=rs1b$bfwd_ratseed)
                      ,list('SITE'=rs1b$SITE)  #####CHANGED: Use rs1w0, not rs1w #####
                      ,protectedMean , na.rm=TRUE
                      )
    
      mnb$METRIC <- 'bfwd_rat'
#      mnb <- rename(mnb, 'x', 'VALUE')
      mn1b <- aggregate(list(VALUE=rs1b$wdprod)
                       ,list('SITE'=rs1b$SITE)
                       ,protectedMean , na.rm=TRUE
                       )
    
      mn1b$METRIC <- 'xwxd'
#      mn1b <- rename (mn1b, 'x', 'VALUE')
    
      mn2b <- aggregate(list(VALUE=rs1b$wdratio)
                       ,list('SITE'=rs1b$SITE)
                       ,protectedMean , na.rm=TRUE
                       )
    
      mn2b$METRIC <- 'xwd_rat'
#      mn2b <- rename(mn2b, 'x', 'VALUE')
    
      # standard deviations (product and ratio)
      sd1b <- aggregate(list(VALUE=rs1b$wdprod)
                       ,list('SITE'=rs1b$SITE)
                       ,sd , na.rm=TRUE
                       )
    
      sd1b$METRIC <- 'sdwxd'
#      sd1b <- rename(sd1b, 'x', 'VALUE')
    
      sd2b <- aggregate(list(VALUE=rs1b$wdratio)
                       ,list('SITE'=rs1b$SITE)
                       ,sd , na.rm=TRUE
                       )
    
      sd2b$METRIC <- 'sdwd_rat'
#      sd2b <- rename(sd2b, 'x', 'VALUE')

      # counts for later calculations
      ccb <- aggregate(list(VALUE=rs1b$bfwd_ratseed)
                      ,list('SITE'=rs1b$SITE) ##### CHANGED: Use rs1w0, not rs1w #####
                      ,count
                      )
      ccb$METRIC <- 'n_bfrat'   
#      ccb <- rename(ccb,'x', 'VALUE')   

      cc1b <- aggregate(list(VALUE=rs1b$wdprod)
                       ,list('SITE'=rs1b$SITE)
                       ,count
                       )
    
      cc1b$METRIC <- 'n_wd'
#      cc1b <- rename(cc1b, 'x', 'VALUE')
    
      cc2b <- aggregate(list(VALUE=rs1b$wdratio)
                       ,list('SITE'=rs1b$SITE)
                       ,count
                       )
    
      cc2b$METRIC <- 'n_wdr'
#      cc2b <- rename(cc2b, 'x', 'VALUE')
      
      boatableRatios <- rbind(mnb, mn1b, mn2b, sd1b,sd2b,ccb,cc1b,cc2b)
  }
  
  intermediateMessage ( '.11 Complete ratios.')
    
  # whats left are the thalweg depth summaries.... these are computed at
  # ALL stations.  Convert xdepth and sddepth from m to cm, to be
  # backward-compatible with interpretations of EMAP data.
  if(is.null(wade)) {
      wadeThal <- NULL
      wadeThal_cm <- NULL
  } else {
      wadeThal <- subset(wade, PARAMETER=='DEPTH') %>% mutate(VALUE = as.numeric(VALUE)*100) # OH GODS NO! THIS SHOULD HAVE BEEN HANDLED DURING UNIT CONVERSIONS AT THE TOP!!!!
      wadeThal_cm <- wadeThal %>% 
                     mutate(PARAMETER = 'DEPTH_CM')                             # Convert CM to CM, which is gosh darned simple
  }
  if(is.null(boat)) {
      boatThal <- NULL
      boatThal_cm <- NULL
  } else {
      boatThal <- subset(boat, PARAMETER=='DEPTH')
      boatThal_cm <- boatThal %>% 
                     mutate(VALUE = as.numeric(VALUE)*100                       # Convert M to CM
                           ,PARAMETER = 'DEPTH_CM'
                           )
                     
  }
  thal <- rbind(wadeThal, boatThal)
  thal_cm <- rbind(wadeThal_cm, boatThal_cm)

  if(is.null(thal)) {
      depthx <- NULL
      depthsd <- NULL
      depthc <- NULL
  } else {
    depthx <- summaryby(thal,'protectedMean',"xdepth")
    depthsd <- summaryby(thal,'sd',"sddepth")
    depthc <- aggregate( list('VALUE'=thal$VALUE),list(SITE=thal$SITE),count)
    depthc$METRIC <- 'n_d'
    intermediateMessage('.12')
  }  
  
  if(is.null(thal_cm)) {
      depthx_cm <- NULL
      depthsd_cm <- NULL
  } else {
    depthx_cm <- summaryby(thal_cm,'protectedMean',"xdepth_cm")
    depthsd_cm <- summaryby(thal_cm,'sd',"sddepth_cm")
    intermediateMessage('.cm')
  }  

  intermediateMessage('.13')

  # things to put together
  mets <- rbind (intest, wadeableRatios, boatableRatios, depthx, depthsd, depthc, depthx_cm, depthsd_cm)

  intermediateMessage('  Done.', loc='end')

  return(mets)

}



# end of file
