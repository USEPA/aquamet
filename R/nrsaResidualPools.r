#' @export 
#' @title Calculate NRSA Residual Pools Metrics
#' @description This function calculates the residual pools portion 
#' of the physical habitat metrics for National Rivers and Streams 
#' Assessment (NRSA) data.  The function requires data frames containing 
#' the thalweg, channel geometry, and stream verification form data files.
#' @param  bDepth A data frame containing thalweg depths for boatable 
#' reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value of single letters 'A' through 'K'
#'            where 'A' is at the upstream end and 'K' is at the 
#'            downstream end of the reach.
#'      \item STATION integer value ranging from 0 to N indicating 
#'                    location between transects.  The value of 0 occurs
#'                    at the transect location, followed by 1, and runs
#'                    to some value N which is located just before the
#'                    next transect, at which STATION then changes back 
#'                    to 0.
#'      \item VALUE numeric value of individual depths
#'      \item UNITS character value indicating units of depth values;
#'                  is expected to be 'FT' or 'M'.
#' }
#' @param wDepth A data frame containing thalweg depths for wadeable 
#' reaches, with the following columns:
#'  \itemize{
#'        \item SITE integer or character specifying the site visit
#'        \item TRANSECT character value of single letters 'A' through 'K'
#'                        where 'A' is at the downstream end and 'K' is at the 
#'                        upstream end of the reach.
#'        \item STATION integer value ranging from 0 to 9 or 14, depending 
#'                      on stream width, indicating location between 
#'                      transects.  The value of 0 occurs at the transect 
#'                      location, followed by 1, and runs to either 9 or 14 
#'                      which is located just before the next transect, at 
#'                      which STATION then changes back to 0.
#'        \item VALUE numeric value of individual depths, always in 
#'                    centimeters.
#' }
#' @param siteSlopes A data frame containing the mean slope for all reaches, 
#' with the following columns:
#'\itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item VALUE numeric value of individual slopes, always in 
#'                  percent.
#' }
#' @param transectSpacing A data frame containing the transect spacing 
#' for the sites, with the following columns: 
#' \itemize{
#'    \item SITE integer or character specifying the site visit
#'    \item TRANSECT character value of single letters 'A' through 'K'.
#'    \item VALUE numeric value of distance between transects, always 
#'                in meters.
#' }
#' @param writeIntermediateFiles logical value; intermediate results will 
#' be written to the results folder if TRUE.
#'
#' @param oldeMethods logical value; if TRUE, uses deprecated SAS method 
#' of starting initial pool dimensions in metsResidualPools.dimensions(), 
#' otherwise uses current method.  Set to TRUE only for unit testing.  
#' IT IS PROBABLY BEST TO NOT INCLUDE THIS IN THE USER DOCUMENTATION.
#' @return Either a data frame when metric calculation is successful 
#' or a character string containing an error message when metric 
#' calculation is not successful.  The data frame contains the following 
#' columns:
#' \itemize{
#'    \item SITE - universal ID value
#'    \item METRIC - metric name
#'    \item VALUE - metric value
#' } 
#' Pool characteristic metrics: poolID, poolar, poolen, mindep, xdep, rpvdep,
#'  rpmxdep, meddep, dep25, dep75 
#'  
#' Site summary metrics: rpxlen, rpvlen, rpmxlen, totplen, rpxdep, rpvdep, 
#' rpmxdep, rpgt50, rpgt75, rpgt100, rpgt05, rpgt05x, rpgt10, rpgt10x, rpgt20,  
#' rpgt20x, rpxarea, rpvarea, rpmxar, areasum, rp100
#' 
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}


nrsaResidualPools <- function(bDepth=NULL, wDepth=NULL, siteSlopes=NULL, transectSpacing=NULL, writeIntermediateFiles=FALSE, oldeMethods=FALSE) {

################################################################################
# Function: nrsaResidualPools
# Title: Calculate NRSA Residual Pools Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: January 29, 2010
# Description:
#   This function calculates the residual pools portion of the physical habitat
#   metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires data frames containing the thalweg, channel geometry, and
#   stream verification form data files.
# Function Revisions:
#   01/29/10 cws: Created.
#   02/11/10 cws: Using nWadeableStationsPerTransect() to estimate the number of
#            stations expected in each transect at a wadeable reach.  This small
#            function was created to allow use in slope and bearing metrics.
#   03/10/10 cws: Updated as needed for change in
#            nWadeableStationsPerTransect().
#   03/11/10 cws: Using readNRSACalculationResults() to read
#            metsSlopeBearing.csv
#   03/22/10 cws: Moved all unit test dataframes to separate functions
#   03/25/10 cws: Changed diff() calls to dfCompare(), nlaLengthen() to
#            dfLengthen().
#   04/01/10 cws: Removing extra print() statements and commented-out code.
#            Modified unit test to try data with just one protocol instead of
#            both.
#   04/13/10 cws: Converting calculation to use xdepth and sddepth values in cm
#            instead of m, as requested, adding protocols argument to
#            metsResidualPools.siteSummaries(); modified unit test accordingly.
#   09/16/10 cws: Removing hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   01/12/11 cws: Modified metsResidualPools.1 to write intermediate results as
#            csv files, for future debugging.
#   01/27/11 cws: Corrected modification of 12 Jan to write correct dataframes
#            to CSVs. Modified calculation of LOC in
#            metsResidualPools.dataOrganization() to handle sites with
#            nWadeableStationsPerTransect() results that vary among transects,
#            previous calculation caused skips and duplicates of LOC values.
#            Changed unit test to correctly handle the sidechannels in
#            2004 EPA01-0450 1, and modified the expected results at each step
#            accordingly.  Added an integration test of sorts at the end of the
#            unit test.
#   02/09/11 cws: Added two sites to unit test that have varying stations per
#            transect.
#   03/22/11 cws: modified to try working with doSMP pkg.
#   03/08/12 cws: Removing need for doSMP pkg, as it is not available for 2.14. 
#            Using pkg foreach instead.  Cleaning up so unit tests are passed.
#   08/03/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify names of the data files.  Added argument NRSAdir to the
#            function to identify the directory from which data files are read
#            and to which the output metrics file is written.
#   12/21/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Calculated slope and bearing metrics
#            directly rather than reading metric calculation results from a
#            file.  Modified output to be a data frame rather than a csv file.
#            Removed RUnit functions.
#   01/10/13 tmk: Changed the default value for argument writeIntermediateFiles
#            in metsResidualPools.1 to FALSE and replaced calls to function
#            writeNRSACalcResults with calls to write.csv.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   05/21/14 kab: Explicitly refer to aquamet::rename function where called
#   05/27/14 kab: Replaced creation of pp in metsResidualPools.dimensions() with 
#            ddply statement because aggregate was not working correctly.
#   06/03/14 cws: Handling case where second argument (actransp) has no rows.  No
#          change to unit test.
#    5/18/15 cws: Explicit use of aquamet::rename causes development problems 
#            when a) that package is not installed, and b) when the current code
#            base differs from the available package.  Rephrased to use dplyr::rename
#            instead.
#  12/18/15 cws Modified calling interface.  Removed filling in of ACTRANSP when
#           missing with DISTANCE values; that's done by caller.
#   2/09/16 cws Based on local feedback, deciding to expect bDepth argument to
#           have UNITS column and do unit conversion in the code. Removed old
#           comments, added descriptions of arguments
# 11/08/16 cws Added new metrics rpxdep_cm, rpvdep_cm, rpmxdep_cm, rpgt05x_cm, 
#          rpgt10x_cm, rpgt20x_cm which are expressed in the same units for all 
#          protocols.
#
# ARGUMENTS:
# bDepth          dataframe containing thalweg depths for boatable reaches, with
#                 the following columns:
#                   SITE     integer or character specifying the site visit
#                   TRANSECT character value of single letters 'A' through 'K'
#                            where 'A' is at the upstream end and 'K' is at the 
#                            downstream end of the reach.
#                   STATION  integer value ranging from 0 to N indicating 
#                            location between transects.  The value of 0 occurs
#                            at the transect location, followed by 1, and runs
#                            to some value N which is located just before the
#                            next transect, at which STATION then changes back 
#                            to 0.
#                   VALUE    numeric value of individual depths
#                   UNITS    character value indicating units of depth values;
#                            is expected to be 'FT' or 'M'.
#
# wDepth          dataframe containing thalweg depths for wadeable reaches, with
#                 the following columns:
#                   SITE     integer or character specifying the site visit
#                   TRANSECT character value of single letters 'A' through 'K'
#                            where 'A' is at the downstream end and 'K' is at the 
#                            upstream end of the reach.
#                   STATION  integer value ranging from 0 to 9 or 14, depending 
#                            on stream width, indicating location between 
#                            transects.  The value of 0 occurs at the transect 
#                            location, followed by 1, and runs to either 9 or 14 
#                            which is located just before the next transect, at 
#                            which STATION then changes back to 0.
#                   VALUE    numeric value of individual depths, always in 
#                            centimeters.
#
# siteSlopes      dataframe containing the mean slope for all reaches, with the 
#                 following columns:
#                   SITE     integer or character specifying the site visit
#                   VALUE    numeric value of individual slopes, always in 
#                            percent.
#
# transectSpacing dataframe containing the transect spacing for the sites, with 
#                 the following columns: 
#                   SITE     integer or character specifying the site visit
#                   TRANSECT character value of single letters 'A' through 'K'.
#                   VALUE    numeric value of distance between transects, always 
#                            in meters.
#
# writeIntermediateFiles - logical value; intermediate results will be written
#           to the results folder if TRUE.
#
# oldeMethods - logical value; if TRUE, uses deprecated SAS method of starting
#           initial pool dimensions in metsResidualPools.dimensions(), otherwise
#           uses current method.  Set to TRUE only for unit testing.  IT IS
#           PROBABLY BEST TO NOT INCLUDE THIS IN THE USER DOCUMENTATION.
#
    intermediateMessage('Residual Pools calculations', loc='start')

    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdfTransectStation <- function(df, ...) {
        if(is.null(...)) return(NULL)
        else if(all(is.na(...))) return(NULL)

        args <- list(...)
        pName <- args[[1]]
        rc <- df %>% 
              select(SITE, TRANSECT, STATION, VALUE) %>% 
              mutate(PARAMETER=pName)
        return(rc)
    }
    ifdfTransectStationUnits <- function(df, ...) {
        if(is.null(...)) return(NULL)
        else if(all(is.na(...))) return(NULL)

        args <- list(...)
        pName <- args[[1]]
        rc <- df %>% 
              mutate(PARAMETER=pName
                    ,VALUE = ifelse(UNITS=='FT', VALUE*0.3048 # convert to M
                            ,ifelse(UNITS=='M', VALUE, NA
                             ))
                    ) %>%
              select(SITE, TRANSECT, STATION, PARAMETER, VALUE)
        return(rc)
    }
    ifdfTransect <- function(df, ...) {
        if(is.null(...)) return(NULL)
        else if(all(is.na(...))) return(NULL)

        args <- list(...)
        pName <- args[[1]]
        rc <- df %>% 
              select(SITE, TRANSECT, VALUE) %>% 
              mutate(PARAMETER=pName)
        return(rc)
    }
    ifdfMetric <- function(df, ...) {
        if(is.null(...)) return(NULL)
        else if(all(is.na(...))) return(NULL)

        args <- list(...)
        pName <- args[[1]]
        rc <- df %>% 
              select(SITE, VALUE) %>% 
              mutate(METRIC=pName)
        return(rc)
    }

    bDepth <- absentAsNULL(bDepth, ifdfTransectStationUnits, 'DEPTH') # in M
    wDepth <- absentAsNULL(wDepth, ifdfTransectStation, 'DEPTH') # in CM
    siteSlopes <- absentAsNULL(siteSlopes, ifdfMetric, 'xslope')
    transectSpacing <- absentAsNULL(transectSpacing, ifdfTransect, 'ACTRANSP') # in M

    if((is.null(bDepth) & is.null(wDepth)) | is.null(siteSlopes) | is.null(transectSpacing)) return(NULL)


    # Recreate old argument from new ones for now, rip out guts later
    intermediateMessage('.A')
    protocols <- NULL
    thalwegDepths <- NULL 
    if(!is.null(bDepth)) {
        protocols <- data.frame(SITE=unique(bDepth$SITE), PROTOCOL='BOATABLE', stringsAsFactors=FALSE)
        thalwegDepths <- bDepth %>% 
                         mutate(SAMPLE_TYPE = 'PHAB_THAL', UNITS = 'M', VALUE = as.numeric(VALUE), PARAMETER='DEP_POLE')    # could be DEP_SONR, doesn't matter
    }

    if(!is.null(wDepth)) {
        protocols <- rbind(protocols, data.frame(SITE=unique(wDepth$SITE), PROTOCOL='WADEABLE', stringsAsFactors=FALSE))
        thalwegDepths <- rbind(thalwegDepths
                              ,wDepth %>% 
                               mutate(SAMPLE_TYPE = 'PHAB_THALW', UNITS = 'CM', VALUE = as.numeric(VALUE))
                              )
    }
    intermediateMessage('.B')

    actransp <- transectSpacing %>%
                subset(SITE %in% subset(protocols, PROTOCOL=='BOATABLE')$SITE)
    thalweg <- rbind(thalwegDepths
                    ,transectSpacing %>% 
                     merge(nWadeableStationsPerTransect(thalwegDepths)
                          ,by=c('SITE','TRANSECT'), all.x=TRUE
                          ) %>%
                     mutate(STATION = 0
                           ,UNITS = 'M'
                           ,SAMPLE_TYPE = 'PHAB_THALW'
                           ,PARAMETER = 'INCREMNT'
                           ,VALUE = as.numeric(VALUE) / nSta
                           ,nSta = NULL
                           ) %>%
                     subset(SITE %in% subset(protocols, PROTOCOL=='WADEABLE')$SITE)
                    )
    slopes <- siteSlopes %>% mutate(VALUE=as.numeric(VALUE))
    intermediateMessage('.C')
    # That's the end of reconstruction, the rest is unchanged, except for the
    # condensing of DISTANCE and ACTRANSP, which is done by the user of this function.


    # Remove side channel transects until we know how to deal with them
    thal <- subset(thalweg, TRANSECT %in% LETTERS)
  
    # Condense DISTANCE and ACTRANSP into ACTRANSP for calculations.
    # Use ACTRANSP if it exists, otherwise use DISTANCE.  Because of the
    # uncertainty in GPS based calculations, DISTANCE is more susceptible to
    # error than the field-based ACTRANSP value.
#     if(nrow(actransp) > 0) {
#         tt <- dfWiden(actransp, c('SITE','TRANSECT'), 'PARAMETER','VALUE')
#         tt$VALUE <- ifelse(is.na(tt$ACTRANSP), tt$DISTANCE, tt$ACTRANSP)
#         tt$PARAMETER <- 'ACTRANSP'
#         tt$ACTRANSP <- NULL
#         tt$DISTANCE <- NULL
#         actransp<- tt
#     }
    intermediateMessage('.D')
  
    # Organize the data for both protocols in a single and simple manner.
    thalSeries <- nrsaResidualPools.dataOrganization(thal, actransp, slopes)
    if(writeIntermediateFiles==TRUE) {
        write.csv(thalSeries, 'intermediate_thalSeries.csv')
    }
  
    # Determine residual depths and number pools
    residualSeries <- nrsaResidualPools.dimensions(thalSeries, protocols
                                                  ,oldeMethods=oldeMethods
                                                  )
    if(writeIntermediateFiles==TRUE) {
        write.csv(residualSeries, 'intermediate_residualSeries.csv')
    }
  
    # Characterize individual residual pools
    poolSeries <- nrsaResidualPools.poolCharacteristics(residualSeries)
    if(writeIntermediateFiles==TRUE) {
        write.csv(poolSeries, 'intermediate_poolSeries.csv')
    }

    # Summarize pool structure for each SITE
    wide <- nrsaResidualPools.siteSummaries(poolSeries, residualSeries
                                           ,protocols
                                           )

    # transpose to long format
    mets <- dfLengthen(wide, 'SITE', 'METRIC', 'VALUE'
                      ,names(wide)[!(names(wide) %in% 'SITE')]
                      )
    if(writeIntermediateFiles==TRUE) {
        write.csv(mets, 'intermediate_mets.csv')
    }
  return(mets)
  
}



nrsaResidualPools.dataOrganization <- function(thal, actransp, slopes) {
 # Reformats wadeable and boatable reach data into the following format, which
# is returned as a dataframe.  If an error occurs, the return value will be a
# character string describing the problem.
#
# a) columns are SITE, TRANSECT, STATION, LOC, DEPTH, INCREMNT
#    where LOC is the numeric position of the depth sample within the reach
#              starting at 1 and increasing by 1 per expected station.  Thus
#              in a wadeable reach at A 0, LOC will be 1 and at A 2 will be 3
#              regardless of whether we have a depth at A 1.  The nickpoint
#              added later will be at LOC = 0.
#          DEPTH is the thalweg depth in meters.
#          INCREMNT is the distance in meters from this station to the next
#              expected station.  This is recorded directly in the wadeable
#              protocol, and is calculated as actransp/numberOfStations for
#              boatable reaches.
# b) rows are in order from downstream to upstream (wadeable reaches will
#    be unaffected, boatable reaches will be reversed)
# c) A downstream 'nick point' will be established at LOC 0 for each SITE.
#    and have an arbitrary depth of xdepth - sddepth.  This is added to each
#    reach to insure capture of the first pool.
#
# ARGUMENTS:
# thal      dataframe with thalweg parameters
# actransp  dataframe with actual transect space values (ACTRANSP, DISTANCE)
# slopes    dataframe with reach slope mean and stdev values (xslope, vslope)
#
# ASSUMPTIONS:
# The maximum station number found in a wadeable reach is the intended number
#   of stations to sample.  In boatable reaches, the intended number is
#   whatever the actual number is.
# Station numbers for a transect start at 0.
# Wadeable incremnt value for reach is constant and stored at the first row
#   of the reach.
# The siteProtocol function for 2008, 2009 data is incorrect for SITE 12475,
#   12498, 13049, 13941,  so the SAMPLE_TYPE column will be relied upon for
#   this function

    intermediateMessage(' dataOrganization')

  # Work on wadeable reaches: extract DEPTH, create LOC, create INCREMNT.
  wadeable <- subset(thal, SAMPLE_TYPE=='PHAB_THALW')
  if(nrow(wadeable) == 0) {
    intermediateMessage('.w0')
    rawStreams <- NULL
  } else {
        intermediateMessage('.w1')
      # Wadeable DEPTH is converted to meters, from cm.
      rawData <- subset(wadeable, PARAMETER=='DEPTH'
                       ,select=c(SITE,TRANSECT,STATION,VALUE)
                       )
      rawData <- dplyr::rename(rawData, DEPTH = VALUE)
      rawData$DEPTH <- as.numeric(rawData$DEPTH) / 100
  
      # Wadeable LOC is the index of each station (sampled or presumed skipped)
      # within each site.  It is the cumulative sum of expected station counts
      # at the beginning of a transect plus the STATION value plus 1 (we add 1
      # since STATION numbering starts at zero).
        intermediateMessage('.w2')
      nSta <- nWadeableStationsPerTransect(rawData)
      nSta <- first(nSta[order(nSta$SITE,nSta$TRANSECT),], 'SITE','first.SITE')
      nSta$startLOC <- NA
      for (i in 1:nrow(nSta)) {
          nSta[i,]$startLOC <- ifelse(nSta[i,]$first.SITE==TRUE | i==1
                                    ,0
                                    ,nSta[i-1,]$startLOC + nSta[i-1,]$nSta
                                    )
      }
        intermediateMessage('.w3')
      rawData <- merge(rawData, nSta, by=c('SITE','TRANSECT'), all.x=TRUE)
#      rawData$LOC <- (match(rawData$TRANSECT, LETTERS) - 1) * rawData$nSta + rawData$STATION + 1
      rawData$LOC <- rawData$startLOC + rawData$STATION + 1
      rawData$LOC <- as.integer(rawData$LOC)
      rawData <- subset(rawData, select=-c(nSta,first.SITE,startLOC))
        intermediateMessage('.w4')

      # Wadeable INCREMNT is the recorded value of incremnt in meters. It is taken
      # from the first row of each reach, and propagated to all rows in the reach.
      wadeable <- wadeable[order(wadeable$SITE, wadeable$TRANSECT, wadeable$STATION),]
      incremnt <- first(subset(wadeable, PARAMETER=='INCREMNT'), 'SITE', 'firstRow')
      incremnt <- subset(incremnt, firstRow==TRUE, select=c(SITE,VALUE))
      incremnt <- dplyr::rename(incremnt, INCREMNT = VALUE)
      rawStreams <- merge(rawData, incremnt, by='SITE')
      rawStreams$INCREMNT <- as.numeric(rawStreams$INCREMNT)
      rm(rawData, nSta, wadeable, incremnt)
        intermediateMessage('.w5')
  }

  # Work on boatable reaches: calculate DEPTH in m, create LOC, create INCREMNT
  boatable <- subset(thal, SAMPLE_TYPE=='PHAB_THAL')
  
  if(nrow(boatable) == 0) {
      rawRivers <- NULL
        intermediateMessage('.b0')
  } else {
        intermediateMessage('.b1')
      # Boatable DEPTH is converted to meters if it was recorded in feet.
      rawData <- subset(boatable, PARAMETER %in% c('DEP_SONR','DEP_POLE')
                       ,select=c(SITE,TRANSECT,STATION,VALUE,UNITS)
                       )
      rawData$VALUE <- as.numeric(ifelse(rawData$VALUE=='.', NA, rawData$VALUE))
      rawData$DEPTH <- NA
      rawData$DEPTH <- ifelse(rawData$UNITS=='M',  rawData$VALUE
                      ,ifelse(rawData$UNITS=='FT', rawData$VALUE * 0.3048
                             ,NA
                      ))
      rawData <- subset(rawData, select=-c(UNITS,VALUE))
        intermediateMessage('.b2')

      # Determining LOC is different in boatable sites than wadeable sites.
      # Since the emphasis in the field is on accurate transect spacing instead
      # of station spacing, and because crews do not 'skip' stations because of
      # inaccessibility.  LOC consequently does not incorporate gaps based on
      # the number of stations sampled in a transect.  The only complication is
      # that LOC is 'reversed' so that it increases in the direction of upstream
      # travel (like wadeables).  In the code below, revLOC is the number of
      # stations from the beginning, and LOC is the reverse of revLOC -- simpler
      # to express in English than in R.
      rawData <- first(rawData[order(rawData$SITE,rawData$TRANSECT,rawData$STATION),]
                      ,'SITE', 'firstSITE'
                      )
      rawData$revLOC <- NA
      rl <- NA
        intermediateMessage('.b3')
      for (i in 1:nrow(rawData)) {
           rl <- ifelse(rawData$firstSITE[i], 1, rl + 1)
           rawData$revLOC[i] <- rl
      }
        intermediateMessage('.b4')
      rawData <- merge(rawData
                      ,aggregate(list(maxLOC=rawData$revLOC)
                                ,list(SITE=rawData$SITE)
                                ,max, na.rm=TRUE
                                )
                      ,by='SITE'
                      )
      rawData$LOC <- rawData$maxLOC - rawData$revLOC + 1
      rawData$firstSITE <- NULL
      rawData$revLOC <- NULL
      rawData$maxLOC <- NULL
          intermediateMessage('.b5')

      # Boatable INCREMNT is calculated as the actual transect spacing recorded
      # at each transect divided by the number of stations sampled in that transect.
      lastSta <- aggregate(list('lastSta'=rawData$STATION)
                          ,list('SITE'=rawData$SITE, 'TRANSECT'=rawData$TRANSECT)
                          ,max, na.rm=TRUE
                          )
        intermediateMessage('.b6')
      rawRivers <- merge(rawData, lastSta, by=c('SITE','TRANSECT'), all.x=TRUE)
      rawRivers <- merge(rawRivers, actransp[c('SITE','TRANSECT','VALUE')]
                        ,by=c('SITE','TRANSECT'), all.x=TRUE
                        )
      rawRivers$INCREMNT <- as.numeric(rawRivers$VALUE) / (rawRivers$lastSta + 1)
      rawRivers <- subset(rawRivers, select=-c(lastSta, VALUE))
      rm(boatable, rl, lastSta, rawData)
        intermediateMessage('.b7')
  }

  if(is.null(rawStreams) & is.null(rawRivers)) {
      # No boatable or wadeable reaches.  Darn!
      return("There are no wadeable nor boatable reaches in the data")
  } else {
      # Rejoin rivers and streams
      thalSeries <- rbind(rawStreams, rawRivers)
  }

  # Include mean slopes, modified with Stack (1989) equation.
    intermediateMessage('.1')
  slopes <- subset(slopes, METRIC=='xslope')
  slopes$stackSlope <- 0.12 + 0.25 * as.numeric(slopes$VALUE)
  thalSeries <- merge(thalSeries, slopes[c('SITE','stackSlope')], by='SITE', all.x=TRUE)


  # Create and include downstream nickpoints
    intermediateMessage('.2')
  dMean <- aggregate(list('dMean'=thalSeries$DEPTH), list('SITE'=thalSeries$SITE)
                    ,mean, na.rm=TRUE
                    )
  dStdev <- aggregate(list('dStdev'=thalSeries$DEPTH), list('SITE'=thalSeries$SITE)
                     ,sd, na.rm=TRUE
                     )
  thalSeries <- thalSeries[order(thalSeries$SITE, thalSeries$LOC),]
  nicks <- subset(first(thalSeries, 'SITE', 'first'), first=='TRUE')
  nicks <- merge(nicks, dMean, by='SITE', all.x=TRUE)
  nicks <- merge(nicks, dStdev, by='SITE', all.x=TRUE)
  nicks$LOC <- as.integer(0)
  nicks$DEPTH <- nicks$dMean - nicks$dStdev
  nicks$DEPTH <- ifelse(nicks$DEPTH < 0, 0, nicks$DEPTH)

  thalSeries <- rbind(thalSeries, nicks[names(thalSeries)])

  # Final organization details
    intermediateMessage('.3')
  thalSeries <- thalSeries[order(thalSeries$SITE, thalSeries$LOC),]
  rownames(thalSeries) <- NULL
  thalSeries$STATION <- as.integer(thalSeries$STATION)

    intermediateMessage('.4')
  return(thalSeries)
}



nrsaResidualPools.dimensions <- function(thalSeries, thalProtocol,
  minSampPct=85, oldeMethods=FALSE) {

# Detect residual pools by calculating residual depths and numbering pools.
# Return dataframe with residual pools and pool numbers added to input dataframe
# or return a character string describing the error if one occurs.
#
# The dataframe with these initial pool dimensions will have the following
# information: in addition to SITE, TRANSECT, STATION, LOC, DEPTH, INCREMNT
# and stackSlope delivered in the input dataframe thalSeries, the following
# calculations are returned as well:
#   resDepth      residual pool depth at this station.  If zero, there is no
#                   pool.
#   resArea       residual sagittal area between this station and the previous
#                   station.
#   resLength     residual pool length between this station and the previous
#                   station.
#   poolID        pool identification number for the current site, starting
#                   at 1.  If zero, there is no residual pool at this station.
#
# ARGUMENTS:
# thalSeries   dataframe with thalweg depth information in the expected format
#              as created by nrsaResidualPools.dataOrganization().
# thalProtocol dataframe with sampling protocol used at each site.  Used in
#              conjunction with oldeMethods argument.
# minSampPct   numeric value specifying the minimum percentage of depths that
#              must be present to reliably detect residual pools, as stations
#              with missing depths are difficult to associate with a specific
#              pool.
# oldeMethods  logical flag specifying if old (SAS/EMAP) method of calculating
#              residual pools is used.  This affects only wadeable reaches.
#              The unit test for this function is based on SAS calculations, so
#              this flag should be TRUE for testing.
#

    intermediateMessage(' dimensions')

  # Pool detection and numbering starts at beginning of a reach
  thalSeries <- first(thalSeries[order(thalSeries$SITE, thalSeries$LOC),]
                     ,'SITE'
                     ,'siteStart'
                     )

  # Create columns for residual dimensions here, and fill them in later
  thalSeries$resDepth <- as.numeric(NA)
  thalSeries$resArea <- as.numeric(NA)
  thalSeries$resLength <- as.numeric(NA)
  thalSeries$poolID <- as.numeric(NA)

  # Calculate residual dimensions at each station (LOC) along the thalweg
  # Split thalweg series by SITE, and process them in parallel.
  rpSeriesSplit <- split(thalSeries, thalSeries$SITE)
    intermediateMessage('.1')
#  w<-startWorkers(workerCount=7)
#  registerDoSMP(w)
r <- foreach(uid = names(rpSeriesSplit), .combine=rbind) %do% {
  thisSeries <- rpSeriesSplit[[uid]]
  for(i in 1:nrow(thisSeries)) {
      if(thisSeries[i,]$siteStart) {
##          intermediateMessage(paste('.', as.character(i), sep=''))

          # initialize counts and such at site nickpoint
          poolBaseDepth <- thisSeries[i,]$DEPTH
          poolID <- 0
          poolLen <- 0
          inPool <- FALSE
          
          # Determine protocol used for this site.  If this is not determinable
          # then assume it to be wadeable.
          pp <- subset(thalProtocol, SITE==thisSeries[i,]$SITE)
          if(nrow(pp) ==1) {
              isWadeable <- pp$PROTOCOL=='WADEABLE'
          } else {
              isWadeable <- TRUE
          }
          
      } else {
          # Go through site detecting residual depths and related dimensions
          # for each station (LOC) in the site.  The residual depth is the depth
          # of the pool at that point if no water flowed.  Mathematically,
          # residual depth = depth - (poolBaseDepth + poolLen * stackSlope/100)
          #   where poolBaseDepth = depth of downstream lip of pool
          #         poolLen       = distance from the downstream lip of pool to
          #                         current station
          #         stackSlope    = channel slope (in percent), modified using
          #                         Stack (1989) equation.  Dividing by 100
          #                         converts from % slope to a tangent.
          #
          # Identify succeeding pools by counting them in poolID.
          thisSeries[i,]$resLength <- thisSeries[i,]$INCREMNT *
                                     (thisSeries[i,]$LOC - thisSeries[i-1,]$LOC)

          poolLen <- poolLen + thisSeries[i,]$resLength
          if(oldeMethods & isWadeable & thisSeries[i-1,]$siteStart) {
              # In previous rp code, channel length was 0 at start of reach.
              # This results in incorrect calculation in the first position, but
              # is retained here for testing.
              poolLen <- ifelse(is.na(thisSeries[i,]$INCREMNT), NA, 0)
          }
          thisSeries[i,]$resDepth <- thisSeries[i,]$DEPTH -
                                     (poolBaseDepth +
                                      poolLen * thisSeries[i,]$stackSlope/100
                                     )

          if(is.na(thisSeries[i,]$resDepth)) {
              # If residual depth is incalculable, so are the other dimensions.
              thisSeries[i,]$resLength <- NA

          } else if(thisSeries[i,]$resDepth > 0) {
              # This station is part of the current pool.  Increment pool
              # counter if the previous station was not in a pool, and then
              # calculate incremental residual dimensions.  This will also
              # affect the first residual area calculation at each reach.

              if(!inPool) {
                  inPool <- TRUE
                  poolID <- poolID + 1
              }
              thisSeries[i,]$resArea <- thisSeries[i,]$resDepth *
                                       thisSeries[i,]$resLength
              if(oldeMethods & isWadeable & thisSeries[i-1,]$siteStart)
                  thisSeries[i,]$resArea <- 0
                  
              thisSeries[i,]$poolID <- poolID
              
          } else {
              # This station is not in a pool, so reset incremental residual
              # dimensions and get ready to detect next pool
              thisSeries[i,]$resDepth <- 0
              thisSeries[i,]$resArea <- 0
              thisSeries[i,]$resLength <- 0
              thisSeries[i,]$poolID <- 0

              inPool <- FALSE
              poolBaseDepth <- thisSeries[i,]$DEPTH
              poolLen <- 0
          }

      } # end of actions at each station

  } # end of loop through each row
  
  thisSeries    # let rbind() have the results
}
  thalSeries <- r
  intermediateMessage('.2')
  
  # Go through calculations and set values to missing in those sites with
  # sparse sampling, as defined by minSampPct.  The sampled percentage for a
  # site is based on the number of expected depth values and the number that
  # are missing; the number of missing values:
  #    sampPct = 100 * (nPresent - nDepthMissing) / nExpected
  #
  tt <- aggregate(list('nExpected'=thalSeries$LOC), list('SITE'=thalSeries$SITE)
                 ,max, na.rm=TRUE
                 )
  mm <- aggregate(list('nMissing'=thalSeries$DEPTH), list('SITE'=thalSeries$SITE)
                 ,function(x) { sum(is.na(x)) }
                 )
  intermediateMessage('.3')
  pp <- ddply(thalSeries,c('SITE'),summarise,nPresent=length(LOC))
#   pp <- aggregate(list('nPresent'=thalSeries$LOC), list('SITE'=thalSeries$SITE)
#                  ,count
#                  )
  tt <- merge(tt, merge(mm, pp, by='SITE'), by='SITE')
  tt$sampPct <- 100 * (tt$nPresent - tt$nMissing) / tt$nExpected
  tt$keep <- (tt$sampPct >= minSampPct)
  intermediateMessage('.4')
  
  thalSeries <- merge(thalSeries, subset(tt, select=c(SITE,keep)), by='SITE')
  thalSeries$resDepth <- ifelse(thalSeries$keep, thalSeries$resDepth, NA)
  thalSeries$resArea <- ifelse(thalSeries$keep, thalSeries$resArea, NA)
  thalSeries$resLength <- ifelse(thalSeries$keep, thalSeries$resLength, NA)
  thalSeries$poolID <- ifelse(thalSeries$keep, thalSeries$poolID, NA)
  thalSeries$keep <- NULL

  intermediateMessage('.5')
  return(thalSeries)
}



nrsaResidualPools.poolCharacteristics <- function(poolDims) {

# Summarizes individual residual pools, based on the station by station
# dimensions previously calculated.  Returns dataframe of individual pool
# summaries if successful, or a character string describing the error if one
# occurs.
#
# ARGUMENTS:
# poolDims   dataframe with residual pool dimensions as calculated by
#            nrsaResidualPools.dimensions().
#

  intermediateMessage(' poolCharacteristics')
  
  # residual area summaries
  poolar <- aggregate(list('poolar'=poolDims$resArea)
                     ,list('SITE'=poolDims$SITE, 'poolID'=poolDims$poolID)
                     ,sum, na.rm=TRUE
                     )

  intermediateMessage('.1')

  # residual length summaries
  poolen <- aggregate(list('poolen'=poolDims$resLength)
                     ,list('SITE'=poolDims$SITE, 'poolID'=poolDims$poolID)
                     ,sum, na.rm=TRUE
                     )
  intermediateMessage('.2')

  # residual depth summaries
  mindep <- aggregate(list('mindep'=poolDims$resDepth)
                     ,list('SITE'=poolDims$SITE, 'poolID'=poolDims$poolID)
                     ,min, na.rm=TRUE
                     )

  xdep <- aggregate(list('xdep'=poolDims$resDepth)
                   ,list('SITE'=poolDims$SITE, 'poolID'=poolDims$poolID)
                   ,mean, na.rm=TRUE
                   )
  rpvdep <- aggregate(list('rpvdep'=poolDims$resDepth)
                     ,list('SITE'=poolDims$SITE, 'poolID'=poolDims$poolID)
                     ,sd, na.rm=TRUE
                     )
  rpmxdep <- aggregate(list('rpmxdep'=poolDims$resDepth)
                      ,list('SITE'=poolDims$SITE, 'poolID'=poolDims$poolID)
                      ,max, na.rm=TRUE
                      )
  meddep <- aggregate(list('meddep'=poolDims$resDepth)
                     ,list('SITE'=poolDims$SITE, 'poolID'=poolDims$poolID)
                     ,median, na.rm=TRUE
                     )
  dep25 <- aggregate(list('dep25'=poolDims$resDepth)
                    ,list('SITE'=poolDims$SITE, 'poolID'=poolDims$poolID)
                    ,quantile, probs=0.25, na.rm=TRUE, type=2
                    )
  dep75 <- aggregate(list('dep75'=poolDims$resDepth)
                    ,list('SITE'=poolDims$SITE, 'poolID'=poolDims$poolID)
                    ,quantile, probs=0.75, na.rm=TRUE, type=2
                    )
  intermediateMessage('.3')

  # Combine summaries for output
  poolCharacteristics <- merge(poolar, poolen, by=c('SITE','poolID'), all=TRUE)
  poolCharacteristics <- merge(poolCharacteristics, mindep
                              ,by=c('SITE','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, xdep
                              ,by=c('SITE','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, rpvdep
                              ,by=c('SITE','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, rpmxdep
                              ,by=c('SITE','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, meddep
                              ,by=c('SITE','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, dep25
                              ,by=c('SITE','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, dep75
                              ,by=c('SITE','poolID'), all=TRUE
                              )
                              
  poolCharacteristics <- subset(poolCharacteristics, poolID != 0)

  intermediateMessage('.4')
  return(poolCharacteristics)

}



nrsaResidualPools.siteSummaries <- function(poolInfo, stationInfo, protocols) {

# Calculates site summaries from residual pool summaries as calculated by
# nrsaResidualPools.poolCharacteristics().  Returns dataframe of results if
# successful, or a character string describing the error if one occurs.
#
# ARGUMENTS:
# poolInfo      dataframe of residual pool summaries
# stationInfo   dataframe of residual depths and such at each station along a
#                 reach.
# protocols dataframe with sampling protocol used at each site
#

  intermediateMessage(' siteSummaries ', loc='start')
  # pool length summaries
  rpxlen <- aggregate(list('rpxlen'=poolInfo$poolen)
                     ,list('SITE'=poolInfo$SITE)
                     ,mean, na.rm=TRUE
                     )
  rpvlen <- aggregate(list('rpvlen'=poolInfo$poolen)
                     ,list('SITE'=poolInfo$SITE)
                     ,sd, na.rm=TRUE
                     )
  rpmxlen <- aggregate(list('rpmxlen'=poolInfo$poolen)
                      ,list('SITE'=poolInfo$SITE)
                      ,max, na.rm=TRUE
                      )
  totplen <- aggregate(list('totplen'=poolInfo$poolen)
                      ,list('SITE'=poolInfo$SITE)
                      ,sum, na.rm=TRUE
                      )
  intermediateMessage('.1')
  
  # pool depth summaries: summarize individual residual depths, then have a
  # look at the maximum depths of each pool, counting the numbers deeper than
  # certain values, and calculating a mean of those maximum depths for some
  # (intended to assist in discering 'real' pools from artifacts of the
  # calculations).
  # Note that the max() of a vector of length 0 (after NAs removed) returns
  # -Inf as the answer and writes warnings to the screen.  A bit of extra care
  # was thus needed for rpmxdep.
  # Convert depth summaries from m to cm here.
  rpxdep <- aggregate(list('rpxdep'=stationInfo$resDepth)
                     ,list('SITE'=stationInfo$SITE)
                     ,function(x) { mean(ifelse(x==0,NA,x), na.rm=TRUE) }
                     )
  rpxdep$rpxdep <- ifelse(rpxdep$SITE %in% subset(protocols
                                                ,PROTOCOL %in% 'WADEABLE')$SITE
                         ,rpxdep$rpxdep * 100
                         ,rpxdep$rpxdep
                         )

  rpvdep <- aggregate(list('rpvdep'=stationInfo$resDepth)
                     ,list('SITE'=stationInfo$SITE)
                     ,function(x) { sd(ifelse(x==0,NA,x), na.rm=TRUE) }
                     )
  rpvdep$rpvdep <- ifelse(rpvdep$SITE %in% subset(protocols
                                                ,PROTOCOL %in% 'WADEABLE')$SITE
                         ,rpvdep$rpvdep * 100
                         ,rpvdep$rpvdep
                         )

  rpmxdep <- aggregate(list('rpmxdep'=stationInfo$resDepth)
                      ,list('SITE'=stationInfo$SITE)
                      ,function(x) { max(ifelse(is.na(x),-Inf,x), na.rm=TRUE) }
                      )
  rpmxdep$rpmxdep <- ifelse(rpmxdep$rpmxdep==-Inf, NA, rpmxdep$rpmxdep)
  rpmxdep$rpmxdep <- ifelse(rpmxdep$SITE %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$SITE
                           ,rpmxdep$rpmxdep * 100
                           ,rpmxdep$rpmxdep
                           )

  rpgt50 <- aggregate(list('rpgt50'=poolInfo$rpmxdep)
                     ,list('SITE'=poolInfo$SITE)
                     ,function(x) { sum(x > 0.50) }
                     )
  rpgt75 <- aggregate(list('rpgt75'=poolInfo$rpmxdep)
                     ,list('SITE'=poolInfo$SITE)
                     ,function(x) { sum(x > 0.75) }
                     )
  rpgt100 <- aggregate(list('rpgt100'=poolInfo$rpmxdep)
                      ,list('SITE'=poolInfo$SITE)
                      ,function(x) { sum(x > 1.0) }
                      )

  rpgt05 <- aggregate(list('rpgt05'=poolInfo$rpmxdep)
                     ,list('SITE'=poolInfo$SITE)
                     ,function(x) { sum(x > 0.05) }
                     )
  rpgt05x <- aggregate(list('rpgt05x'=poolInfo$rpmxdep)
                      ,list('SITE'=poolInfo$SITE)
                      ,function(x) { mean(ifelse(x > 0.05, x, NA), na.rm=TRUE) }
                      )
  rpgt05x$rpgt05x <- ifelse(rpgt05x$SITE %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$SITE
                           ,rpgt05x$rpgt05x * 100
                           ,rpgt05x$rpgt05x
                           )

  rpgt10 <- aggregate(list('rpgt10'=poolInfo$rpmxdep)
                     ,list('SITE'=poolInfo$SITE)
                     ,function(x) { sum(x > 0.10) }
                     )
  rpgt10x <- aggregate(list('rpgt10x'=poolInfo$rpmxdep)
                      ,list('SITE'=poolInfo$SITE)
                      ,function(x) { mean(ifelse(x > 0.10, x, NA), na.rm=TRUE) }
                      )
  rpgt10x$rpgt10x <- ifelse(rpgt10x$SITE %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$SITE
                           ,rpgt10x$rpgt10x * 100
                           ,rpgt10x$rpgt10x
                           )

  rpgt20 <- aggregate(list('rpgt20'=poolInfo$rpmxdep)
                     ,list('SITE'=poolInfo$SITE)
                     ,function(x) { sum(x > 0.20) }
                     )
  rpgt20x <- aggregate(list('rpgt20x'=poolInfo$rpmxdep)
                      ,list('SITE'=poolInfo$SITE)
                      ,function(x) { mean(ifelse(x > 0.20, x, NA), na.rm=TRUE) }
                      )
  rpgt20x$rpgt20x <- ifelse(rpgt20x$SITE %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$SITE
                           ,rpgt20x$rpgt20x * 100
                           ,rpgt20x$rpgt20x
                           )


  intermediateMessage('.2')
  
  # pool area summaries
  rpxarea <- aggregate(list('rpxarea'=poolInfo$poolar)
                      ,list('SITE'=poolInfo$SITE)
                      ,mean, na.rm=TRUE
                      )
  rpvarea <- aggregate(list('rpvarea'=poolInfo$poolar)
                      ,list('SITE'=poolInfo$SITE)
                      ,sd, na.rm=TRUE
                      )
  rpmxar <- aggregate(list('rpmxar'=poolInfo$poolar)
                     ,list('SITE'=poolInfo$SITE)
                     ,max, na.rm=TRUE
                     )
  areasum <- aggregate(list('areasum'=poolInfo$poolar)
                      ,list('SITE'=poolInfo$SITE)
                      ,sum, na.rm=TRUE
                      )
  intermediateMessage('.3')
  
  # rp100 calculations, finally.  Reachlength should not include the distance
  # from the nickpoint to the start of the reach.  It should also not include
  # the distance from the last station to the nonexistant next station.
  # Consequently the incremnt values associated with the nickpoint and last
  # station are removed.
  tt <- stationInfo[order(stationInfo$SITE,stationInfo$LOC),]
  tt <- last(tt, 'SITE', 'lastStation')
  tt <- subset(tt, LOC>0 & ! lastStation)
  
  
  reachlen <- aggregate(list('reachlen'=tt$INCREMNT)
                       ,list('SITE'=tt$SITE)
                       ,sum, na.rm=TRUE
                       )
  rp100 <- merge(areasum, reachlen, by='SITE')
  rp100$rp100 <- ifelse(is.na(rp100$reachlen), NA
                       ,ifelse(rp100$reachlen==0, NA
                              ,100*rp100$areasum/rp100$reachlen
                              )
                       )
  rp100 <- subset(rp100, select=c(SITE,rp100))
  intermediateMessage('.4')

  # combine summaries, setting missing counts to zero
  lengthMets <- merge(rpxlen, rpvlen, by='SITE', all=TRUE)
  lengthMets <- merge(lengthMets, rpmxlen, by='SITE', all=TRUE)
  lengthMets <- merge(lengthMets, totplen, by='SITE', all=TRUE)
  
  depthMets <- merge(rpxdep, rpvdep, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpmxdep, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpgt50, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpgt75, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpgt100, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpgt05, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpgt05x, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpgt10, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpgt10x, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpgt20, by='SITE', all=TRUE)
  depthMets <- merge(depthMets, rpgt20x, by='SITE', all=TRUE)
  
  wadeableSites <- unique(subset(protocols, PROTOCOL %in% 'WADEABLE')$SITE)
  depthMets <- mutate(depthMets
                     ,rpxdep_cm  = rpxdep  * ifelse(SITE %in% wadeableSites, 1, 100)
                     ,rpvdep_cm  = rpvdep  * ifelse(SITE %in% wadeableSites, 1, 100)
                     ,rpmxdep_cm = rpmxdep * ifelse(SITE %in% wadeableSites, 1, 100)
                     ,rpgt05x_cm = rpgt05x * ifelse(SITE %in% wadeableSites, 1, 100)
                     ,rpgt10x_cm = rpgt10x * ifelse(SITE %in% wadeableSites, 1, 100)
                     ,rpgt20x_cm = rpgt20x * ifelse(SITE %in% wadeableSites, 1, 100)
                     )

  areaMets <- merge(rpxarea, rpvarea, by='SITE', all=TRUE)
  areaMets <- merge(areaMets, rpmxar, by='SITE', all=TRUE)
  areaMets <- merge(areaMets, areasum, by='SITE', all=TRUE)
  
  mets <- merge(lengthMets, depthMets, by='SITE', all=TRUE)
  mets <- merge(mets, areaMets, by='SITE', all=TRUE)
  mets <- merge(mets, rp100, by='SITE', all=TRUE)

  intermediateMessage('.5')
  return(mets)
}

# end of file
