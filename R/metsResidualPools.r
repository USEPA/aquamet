# metsResidualPoolsSMP.r
#
#  1/29/10 cws Created
#  2/11/10 cws Using nWadeableStationsPerTransect() to estimate the number of
#          stations expected in each transect at a wadeable reach.  This small
#          function was created to allow use in slope and bearing metrics.
# 03/10/10 cws Updated as needed for change in nWadeableStationsPerTransect().
# 03/11/10 cws Using readNRSACalculationResults() to read metsSlopeBearing.csv
# 03/22/10 cws Moved all unit test dataframes to separate functions
#  3/25/10 cws Changed diff() calls to dfCompare(), nlaLengthen() to dfLengthen().
# 04/01/10 cws Removing extra print() statements and commented-out code.
#          Modified unit test to try data with just one protocol instead of both.
#  4/13/10 cws Converting calculation to use xdepth and sddepth values in cm
#          instead of m, as requested, adding protocols argument to
#          metsResidualPools.siteSummaries(); modified unit test accordingly.
# 09/16/10 cws Removing hardcoding of NRSA database name, using NRSAdbName
#          instead.
# 01/12/11 cws Modified metsResidualPools.1 to write intermediate results as
#          csv files, for future debugging.
# 01/27/11 cws Corrected modification of 12 Jan to write correct dataframes to
#          CSVs. Modified calculation of LOC in metsResidualPools.dataOrganization()
#          to handle sites with nWadeableStationsPerTransect() results that vary
#          among transects, previous calculation caused skips and duplicates of
#          LOC values. Changed unit test to correctly handle the sidechannels in
#          2004 EPA01-0450 1, and modified the expected results at each step
#          accordingly.  Added an integration test of sorts at the end of the
#          unit test.
# 02/09/11 cws Added two sites to unit test that have varying stations per
#          transect.
# 03/22/11 cws modified to try working with doSMP pkg.
# 03/08/12 cws Removing need for doSMP pkg, as it is not available for 2.14. 
#          Using pkg foreach instead.  Cleaning up so unit tests are passed.
# 06/03/14 cws Handling case where second argument (actransp) has no rows.  No
#          change to unit test.
#
# Note: A value for reachlen is calculated based on the sum of INCREMNT values
#       in stationInfo, which is a dataframe created and returned by
#       metsResidualPools.dimensions().  That function uses the output of
#       metsResidualPools.dataOrganization(), which takes the value of INCREMNT
#       at the first station in the reach and propagates it to all stations,
#       hence the result is independent of whether INCREMNT occurs more than
#       once in the thalweg table.
#
#
require(RODBC)
require(RUnit)
require(foreach)
#require(doSMP)

metsResidualPools <- function()
# Calculate residual pool metrics
#
# These metrics are saved to a csv file inthe directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success, or a character string describing the problem if one
# occurs.
#
# ARGUMENTS:
# none
{
  # Read in thalweg data and retain only required parameters
  chan <- odbcConnect(NRSAdbName)
  thal <- fetchNRSATable(chan, 'tblTHALWEG2')
  if(is.character(thal)) return(thal)
  
  thal <- subset(thal
                ,PARAMETER %in% c('BARWIDTH', 'BAR_PRES', 'CHANUNCD', 'DEPTH'
                                 ,'DEP_POLE', 'DEP_SONR', 'INCREMNT'
                                 ,'POOLFMCD', 'REACHLENGTH', 'SEDIMENT'
                                 ,'OFF_CHAN', 'WETWIDTH'
                                 )
                )

  # Read in channel geometry data and retain only required parameters
  actransp <- fetchNRSATable(chan, 'tblCHANNELGEOMETRY2')
  if(is.character(actransp)) return(actransp)

  actransp <- subset(actransp, PARAMETER %in% c('ACTRANSP','DISTANCE'))

  # Read in slope metrics and retain only required calculations
  slopes <- readNRSACalculationResults('metsSlopeBearing.csv')
  if(is.character(slopes)) return(slopes)
  
  slopes <- subset(slopes, METRIC %in% c('xslope','vslope'))

  # Determine protocols used for each site
  protocols <- siteProtocol(unique(thal$UID))
  
  
  # Calculate rp mets and write them to file.
  mets <- metsResidualPools.1(thal, actransp, slopes, protocols)
  if(is.character(mets)) return(mets)
  
  rc <- writeNRSACalcResults(mets, 'metsResidualPools.csv')
  return(rc)
}

metsResidualPools.1 <- function(thal, actransp, slopes, protocols
                               ,writeIntermediateFiles=TRUE
                               ,oldeMethods=FALSE
                               )
# Does the actual calculations for metsResidualPools()
#
# ARGUMENTS:
# thal      dataframe with thalweg parameters
# actransp  dataframe with actual transect space values (ACTRANSP, DISTANCE)
# slopes    dataframe with reach slope mean and stdev values (xslope, vslope)
# protocols dataframe with sampling protocol used at each site
# writeIntermediateFiles - logical value; intermediate results will be written
#           to the results folder if TRUE.
# oldeMethods - logical value; if TRUE, uses deprecated SAS method of starting
#           initial pool dimensions in metsResidualPools.dimensions(), otherwise
#           uses current method.  Set to TRUE only for unit testing.
{

  # Remove side channel transects until we know how to deal with them
  thal <- subset(thal, TRANSECT %in% LETTERS)
  
  # Condense DISTANCE and ACTRANSP into ACTRANSP for calculations.
  # Use ACTRANSP if it exists, otherwise use DISTANCE.  Because of the
  # uncertainty in GPS based calculations, DISTANCE is more susceptible to
  # error than the field-based ACTRANSP value.
  if(nrow(actransp) > 0) {
      tt <- dfWiden(actransp, c('UID','TRANSECT'), 'PARAMETER','RESULT')
      tt$RESULT <- ifelse(is.na(tt$ACTRANSP), tt$DISTANCE, tt$ACTRANSP)
      tt$PARAMETER <- 'ACTRANSP'
      tt$ACTRANSP <- NULL
      tt$DISTANCE <- NULL
      actransp<- tt
  }
  # Organize the data for both protocols in a single and simple manner.
  thalSeries <- metsResidualPools.dataOrganization(thal, actransp, slopes)
  if(writeIntermediateFiles==TRUE) {
      writeNRSACalcResults(thalSeries, 'intermediate_thalSeries.csv')
  }
  
  # Determine residual depths and number pools
#  thalSeries <- readNRSACalculationResults('intermediate_thalSeriesSMP.csv')
#system.time({
  residualSeries <- metsResidualPools.dimensions(thalSeries, protocols
                                                ,oldeMethods=oldeMethods
                                                )
#})
  if(writeIntermediateFiles==TRUE) {
      writeNRSACalcResults(residualSeries, 'intermediate_residualSeries.csv')
  }
  
  # Characterize individual residual pools
  poolSeries <- metsResidualPools.poolCharacteristics(residualSeries)
  if(writeIntermediateFiles==TRUE) {
      writeNRSACalcResults(poolSeries, 'intermediate_poolSeries.csv')
  }
  # Summarize pool structure for each UID
  wide <- metsResidualPools.siteSummaries(poolSeries, residualSeries
                                         ,protocols
                                         )

  # transpose to long format
  mets <- dfLengthen(wide, 'UID', 'METRIC', 'RESULT'
                    ,names(wide)[!(names(wide) %in% 'UID')]
                    )
  if(writeIntermediateFiles==TRUE) {
      writeNRSACalcResults(mets, 'intermediate_mets.csv')
  }
  return(mets)
  
}

metsResidualPools.dataOrganization <- function(thal, actransp, slopes)
# Reformats wadeable and boatable reach data into the following format, which
# is returned as a dataframe.  If an error occurs, the return value will be a
# character string describing the problem.
#
# a) columns are UID, TRANSECT, STATION, LOC, DEPTH, INCREMNT
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
# c) A downstream 'nick point' will be established at LOC 0 for each UID.
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
# The siteProtocol function for 2008, 2009 data is incorrect for UID 12475,
#   12498, 13049, 13941,  so the SAMPLE_TYPE column will be relied upon for
#   this function
{
  # Work on wadeable reaches: extract DEPTH, create LOC, create INCREMNT.
  wadeable <- subset(thal, SAMPLE_TYPE=='PHAB_THALW')
  if(nrow(wadeable) == 0) {
      rawStreams <- NULL
  } else {
      # Wadeable DEPTH is converted to meters, from cm.
      rawData <- subset(wadeable, PARAMETER=='DEPTH'
                       ,select=c(UID,TRANSECT,STATION,RESULT)
                       )
      rawData <- rename(rawData, 'RESULT', 'DEPTH')
      rawData$DEPTH <- as.numeric(rawData$DEPTH) / 100
  
      # Wadeable LOC is the index of each station (sampled or presumed skipped)
      # within each site.  It is the cumulative sum of expected station counts
      # at the beginning of a transect plus the STATION value plus 1 (we add 1
      # since STATION numbering starts at zero).
      nSta <- nWadeableStationsPerTransect(rawData)
      nSta <- first(nSta[order(nSta$UID,nSta$TRANSECT),], 'UID','first.UID')
      nSta$startLOC <- NA
      for (i in 1:nrow(nSta)) {
          nSta[i,]$startLOC <- ifelse(nSta[i,]$first.UID==TRUE | i==1
                                    ,0
                                    ,nSta[i-1,]$startLOC + nSta[i-1,]$nSta
                                    )
      }
      rawData <- merge(rawData, nSta, by=c('UID','TRANSECT'), all.x=TRUE)
#      rawData$LOC <- (match(rawData$TRANSECT, LETTERS) - 1) * rawData$nSta + rawData$STATION + 1
      rawData$LOC <- rawData$startLOC + rawData$STATION + 1
      rawData$LOC <- as.integer(rawData$LOC)
      rawData <- subset(rawData, select=-c(nSta,first.UID,startLOC))

      # Wadeable INCREMNT is the recorded value of incremnt in meters. It is taken
      # from the first row of each reach, and propagated to all rows in the reach.
      wadeable <- wadeable[order(wadeable$UID, wadeable$TRANSECT, wadeable$STATION),]
      incremnt <- first(subset(wadeable, PARAMETER=='INCREMNT'), 'UID', 'firstRow')
      incremnt <- subset(incremnt, firstRow==TRUE, select=c(UID,RESULT))
      incremnt <- rename(incremnt, 'RESULT','INCREMNT')
      rawStreams <- merge(rawData, incremnt, by='UID')
      rawStreams$INCREMNT <- as.numeric(rawStreams$INCREMNT)
      rm(rawData, nSta, wadeable, incremnt)
  }

  # Work on boatable reaches: calculate DEPTH in m, create LOC, create INCREMNT
  boatable <- subset(thal, SAMPLE_TYPE=='PHAB_THAL')
  
  if(nrow(boatable) == 0) {
      rawRivers <- NULL
  } else {
      # Boatable DEPTH is converted to meters if it was recorded in feet.
      rawData <- subset(boatable, PARAMETER %in% c('DEP_SONR','DEP_POLE')
                       ,select=c(UID,TRANSECT,STATION,RESULT,UNITS)
                       )
      rawData$RESULT <- as.numeric(ifelse(rawData$RESULT=='.', NA, rawData$RESULT))
      rawData$DEPTH <- NA
      rawData$DEPTH <- ifelse(rawData$UNITS=='M',  rawData$RESULT
                      ,ifelse(rawData$UNITS=='FT', rawData$RESULT * 0.3048
                             ,NA
                      ))
      rawData <- subset(rawData, select=-c(UNITS,RESULT))

      # Determining LOC is different in boatable sites than wadeable sites.
      # Since the emphasis in the field is on accurate transect spacing instead
      # of station spacing, and because crews do not 'skip' stations because of
      # inaccessibility.  LOC consequently does not incorporate gaps based on
      # the number of stations sampled in a transect.  The only complication is
      # that LOC is 'reversed' so that it increases in the direction of upstream
      # travel (like wadeables).  In the code below, revLOC is the number of
      # stations from the beginning, and LOC is the reverse of revLOC -- simpler
      # to express in English than in R.
      rawData <- first(rawData[order(rawData$UID,rawData$TRANSECT,rawData$STATION),]
                      ,'UID', 'firstUID'
                      )
      rawData$revLOC <- NA
      rl <- NA
      for (i in 1:nrow(rawData)) {
           rl <- ifelse(rawData$firstUID[i], 1, rl + 1)
           rawData$revLOC[i] <- rl
      }
      rawData <- merge(rawData
                      ,aggregate(list(maxLOC=rawData$revLOC)
                                ,list(UID=rawData$UID)
                                ,max, na.rm=TRUE
                                )
                      ,by='UID'
                      )
      rawData$LOC <- rawData$maxLOC - rawData$revLOC + 1
      rawData$firstUID <- NULL
      rawData$revLOC <- NULL
      rawData$maxLOC <- NULL
      
      # Boatable INCREMNT is calculated as the actual transect spacing recorded
      # at each transect divided by the number of stations sampled in that transect.
      lastSta <- aggregate(list('lastSta'=rawData$STATION)
                          ,list('UID'=rawData$UID, 'TRANSECT'=rawData$TRANSECT)
                          ,max, na.rm=TRUE
                          )
      rawRivers <- merge(rawData, lastSta, by=c('UID','TRANSECT'), all.x=TRUE)
      rawRivers <- merge(rawRivers, actransp[c('UID','TRANSECT','RESULT')]
                        ,by=c('UID','TRANSECT'), all.x=TRUE
                        )
      rawRivers$INCREMNT <- as.numeric(rawRivers$RESULT) / (rawRivers$lastSta + 1)
      rawRivers <- subset(rawRivers, select=-c(lastSta, RESULT))
      rm(boatable, rl, lastSta, rawData)
  }

  if(is.null(rawStreams) & is.null(rawRivers)) {
      # No boatable or wadeable reaches.  Darn!
      return("There are no wadeable nor boatable reaches in the data")
  } else {
      # Rejoin rivers and streams
      thalSeries <- rbind(rawStreams, rawRivers)
  }

  # Include mean slopes, modified with Stack (1989) equation.
  slopes <- subset(slopes, METRIC=='xslope')
  slopes$stackSlope <- 0.12 + 0.25 * as.numeric(slopes$RESULT)
  thalSeries <- merge(thalSeries, slopes[c('UID','stackSlope')], by='UID', all.x=TRUE)


  # Create and include downstream nickpoints
  dMean <- aggregate(list('dMean'=thalSeries$DEPTH), list('UID'=thalSeries$UID)
                    ,mean, na.rm=TRUE
                    )
  dStdev <- aggregate(list('dStdev'=thalSeries$DEPTH), list('UID'=thalSeries$UID)
                     ,sd, na.rm=TRUE
                     )
  thalSeries <- thalSeries[order(thalSeries$UID, thalSeries$LOC),]
  nicks <- subset(first(thalSeries, 'UID', 'first'), first=='TRUE')
  nicks <- merge(nicks, dMean, by='UID', all.x=TRUE)
  nicks <- merge(nicks, dStdev, by='UID', all.x=TRUE)
  nicks$LOC <- as.integer(0)
  nicks$DEPTH <- nicks$dMean - nicks$dStdev
  nicks$DEPTH <- ifelse(nicks$DEPTH < 0, 0, nicks$DEPTH)

  thalSeries <- rbind(thalSeries, nicks[names(thalSeries)])

  # Final organization details
  thalSeries <- thalSeries[order(thalSeries$UID, thalSeries$LOC),]
  rownames(thalSeries) <- NULL
  thalSeries$STATION <- as.integer(thalSeries$STATION)

  return(thalSeries)
}


metsResidualPools.dimensions <- function(thalSeries, thalProtocol
                                        ,minSampPct=85, oldeMethods=FALSE
                                        )
# Detect residual pools by calculating residual depths and numbering pools.
# Return dataframe with residual pools and pool numbers added to input dataframe
# or return a character string describing the error if one occurs.
#
# The dataframe with these initial pool dimensions will have the following
# information: in addition to UID, TRANSECT, STATION, LOC, DEPTH, INCREMNT
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
#              as created by metsResidualPools.dataOrganization().
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
{
  intermediateMessage('starting metsResidualPools.dimensions', loc='start')

  # Pool detection and numbering starts at beginning of a reach
  thalSeries <- first(thalSeries[order(thalSeries$UID, thalSeries$LOC),]
                     ,'UID'
                     ,'siteStart'
                     )

  # Create columns for residual dimensions here, and fill them in later
  thalSeries$resDepth <- as.numeric(NA)
  thalSeries$resArea <- as.numeric(NA)
  thalSeries$resLength <- as.numeric(NA)
  thalSeries$poolID <- as.numeric(NA)

  # Calculate residual dimensions at each station (LOC) along the thalweg
  # Split thalweg series by UID, and process them in parallel.
  rpSeriesSplit <- split(thalSeries, thalSeries$UID)
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
          pp <- subset(thalProtocol, UID==thisSeries[i,]$UID)
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
  intermediateMessage('.cleaning up')
  
  # Go through calculations and set values to missing in those sites with
  # sparse sampling, as defined by minSampPct.  The sampled percentage for a
  # site is based on the number of expected depth values and the number that
  # are missing; the number of missing values:
  #    sampPct = 100 * (nPresent - nDepthMissing) / nExpected
  #
  tt <- aggregate(list('nExpected'=thalSeries$LOC), list('UID'=thalSeries$UID)
                 ,max, na.rm=TRUE
                 )
  mm <- aggregate(list('nMissing'=thalSeries$DEPTH), list('UID'=thalSeries$UID)
                 ,function(x) { sum(is.na(x)) }
                 )
  pp <- aggregate(list('nPresent'=thalSeries$LOC), list('UID'=thalSeries$UID)
                 ,count
                 )
  tt <- merge(tt, merge(mm, pp, by='UID'), by='UID')
  tt$sampPct <- 100 * (tt$nPresent - tt$nMissing) / tt$nExpected
  tt$keep <- (tt$sampPct >= minSampPct)
  
  thalSeries <- merge(thalSeries, subset(tt, select=c(UID,keep)), by='UID')
  thalSeries$resDepth <- ifelse(thalSeries$keep, thalSeries$resDepth, NA)
  thalSeries$resArea <- ifelse(thalSeries$keep, thalSeries$resArea, NA)
  thalSeries$resLength <- ifelse(thalSeries$keep, thalSeries$resLength, NA)
  thalSeries$poolID <- ifelse(thalSeries$keep, thalSeries$poolID, NA)
  thalSeries$keep <- NULL

  intermediateMessage('. Finished.', loc='end')
  return(thalSeries)
}


metsResidualPools.poolCharacteristics <- function(poolDims)
# Summarizes individual residual pools, based on the station by station
# dimensions previously calculated.  Returns dataframe of individual pool
# summaries if successful, or a character string describing the error if one
# occurs.
#
# ARGUMENTS:
# poolDims   dataframe with residual pool dimensions as calculated by
#            metsResidualPools.dimensions().
#
{
  intermediateMessage('starting metsResidualPools.poolCharacteristics', loc='start')
  
  # residual area summaries
  poolar <- aggregate(list('poolar'=poolDims$resArea)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,sum, na.rm=TRUE
                     )

  intermediateMessage('.1')

  # residual length summaries
  poolen <- aggregate(list('poolen'=poolDims$resLength)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,sum, na.rm=TRUE
                     )
  intermediateMessage('.2')

  # residual depth summaries
  mindep <- aggregate(list('mindep'=poolDims$resDepth)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,min, na.rm=TRUE
                     )

  xdep <- aggregate(list('xdep'=poolDims$resDepth)
                   ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                   ,mean, na.rm=TRUE
                   )
  rpvdep <- aggregate(list('rpvdep'=poolDims$resDepth)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,sd, na.rm=TRUE
                     )
  rpmxdep <- aggregate(list('rpmxdep'=poolDims$resDepth)
                      ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                      ,max, na.rm=TRUE
                      )
  meddep <- aggregate(list('meddep'=poolDims$resDepth)
                     ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                     ,median, na.rm=TRUE
                     )
  dep25 <- aggregate(list('dep25'=poolDims$resDepth)
                    ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                    ,quantile, probs=0.25, na.rm=TRUE, type=2
                    )
  dep75 <- aggregate(list('dep75'=poolDims$resDepth)
                    ,list('UID'=poolDims$UID, 'poolID'=poolDims$poolID)
                    ,quantile, probs=0.75, na.rm=TRUE, type=2
                    )
  intermediateMessage('.3')

  # Combine summaries for output
  poolCharacteristics <- merge(poolar, poolen, by=c('UID','poolID'), all=TRUE)
  poolCharacteristics <- merge(poolCharacteristics, mindep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, xdep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, rpvdep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, rpmxdep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, meddep
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, dep25
                              ,by=c('UID','poolID'), all=TRUE
                              )
  poolCharacteristics <- merge(poolCharacteristics, dep75
                              ,by=c('UID','poolID'), all=TRUE
                              )
                              
  poolCharacteristics <- subset(poolCharacteristics, poolID != 0)

  intermediateMessage('. Finished.', loc='end')
  return(poolCharacteristics)

}


metsResidualPools.siteSummaries <- function(poolInfo, stationInfo, protocols)
# Calculates site summaries from residual pool summaries as calculated by
# metsResidualPools.poolCharacteristics().  Returns dataframe of results if
# successful, or a character string describing the error if one occurs.
#
# ARGUMENTS:
# poolInfo      dataframe of residual pool summaries
# stationInfo   dataframe of residual depths and such at each station along a
#                 reach.
# protocols dataframe with sampling protocol used at each site
#
{
  intermediateMessage(' starting metsResidualPools.siteSummaries ', loc='start')
  # pool length summaries
  rpxlen <- aggregate(list('rpxlen'=poolInfo$poolen)
                     ,list('UID'=poolInfo$UID)
                     ,mean, na.rm=TRUE
                     )
  rpvlen <- aggregate(list('rpvlen'=poolInfo$poolen)
                     ,list('UID'=poolInfo$UID)
                     ,sd, na.rm=TRUE
                     )
  rpmxlen <- aggregate(list('rpmxlen'=poolInfo$poolen)
                      ,list('UID'=poolInfo$UID)
                      ,max, na.rm=TRUE
                      )
  totplen <- aggregate(list('totplen'=poolInfo$poolen)
                      ,list('UID'=poolInfo$UID)
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
                     ,list('UID'=stationInfo$UID)
                     ,function(x) { mean(ifelse(x==0,NA,x), na.rm=TRUE) }
                     )
  rpxdep$rpxdep <- ifelse(rpxdep$UID %in% subset(protocols
                                                ,PROTOCOL %in% 'WADEABLE')$UID
                         ,rpxdep$rpxdep * 100
                         ,rpxdep$rpxdep
                         )

  rpvdep <- aggregate(list('rpvdep'=stationInfo$resDepth)
                     ,list('UID'=stationInfo$UID)
                     ,function(x) { sd(ifelse(x==0,NA,x), na.rm=TRUE) }
                     )
  rpvdep$rpvdep <- ifelse(rpvdep$UID %in% subset(protocols
                                                ,PROTOCOL %in% 'WADEABLE')$UID
                         ,rpvdep$rpvdep * 100
                         ,rpvdep$rpvdep
                         )

  rpmxdep <- aggregate(list('rpmxdep'=stationInfo$resDepth)
                      ,list('UID'=stationInfo$UID)
                      ,function(x) { max(ifelse(is.na(x),-Inf,x), na.rm=TRUE) }
                      )
  rpmxdep$rpmxdep <- ifelse(rpmxdep$rpmxdep==-Inf, NA, rpmxdep$rpmxdep)
  rpmxdep$rpmxdep <- ifelse(rpmxdep$UID %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$UID
                           ,rpmxdep$rpmxdep * 100
                           ,rpmxdep$rpmxdep
                           )

  rpgt50 <- aggregate(list('rpgt50'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.50) }
                     )
  rpgt75 <- aggregate(list('rpgt75'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.75) }
                     )
  rpgt100 <- aggregate(list('rpgt100'=poolInfo$rpmxdep)
                      ,list('UID'=poolInfo$UID)
                      ,function(x) { sum(x > 1.0) }
                      )

  rpgt05 <- aggregate(list('rpgt05'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.05) }
                     )
  rpgt05x <- aggregate(list('rpgt05x'=poolInfo$rpmxdep)
                      ,list('UID'=poolInfo$UID)
                      ,function(x) { mean(ifelse(x > 0.05, x, NA), na.rm=TRUE) }
                      )
  rpgt05x$rpgt05x <- ifelse(rpgt05x$UID %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$UID
                           ,rpgt05x$rpgt05x * 100
                           ,rpgt05x$rpgt05x
                           )

  rpgt10 <- aggregate(list('rpgt10'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.10) }
                     )
  rpgt10x <- aggregate(list('rpgt10x'=poolInfo$rpmxdep)
                      ,list('UID'=poolInfo$UID)
                      ,function(x) { mean(ifelse(x > 0.10, x, NA), na.rm=TRUE) }
                      )
  rpgt10x$rpgt10x <- ifelse(rpgt10x$UID %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$UID
                           ,rpgt10x$rpgt10x * 100
                           ,rpgt10x$rpgt10x
                           )

  rpgt20 <- aggregate(list('rpgt20'=poolInfo$rpmxdep)
                     ,list('UID'=poolInfo$UID)
                     ,function(x) { sum(x > 0.20) }
                     )
  rpgt20x <- aggregate(list('rpgt20x'=poolInfo$rpmxdep)
                      ,list('UID'=poolInfo$UID)
                      ,function(x) { mean(ifelse(x > 0.20, x, NA), na.rm=TRUE) }
                      )
  rpgt20x$rpgt20x <- ifelse(rpgt20x$UID %in% subset(protocols
                                                   ,PROTOCOL %in% 'WADEABLE')$UID
                           ,rpgt20x$rpgt20x * 100
                           ,rpgt20x$rpgt20x
                           )


  intermediateMessage('.2')
  
  # pool area summaries
  rpxarea <- aggregate(list('rpxarea'=poolInfo$poolar)
                      ,list('UID'=poolInfo$UID)
                      ,mean, na.rm=TRUE
                      )
  rpvarea <- aggregate(list('rpvarea'=poolInfo$poolar)
                      ,list('UID'=poolInfo$UID)
                      ,sd, na.rm=TRUE
                      )
  rpmxar <- aggregate(list('rpmxar'=poolInfo$poolar)
                     ,list('UID'=poolInfo$UID)
                     ,max, na.rm=TRUE
                     )
  areasum <- aggregate(list('areasum'=poolInfo$poolar)
                      ,list('UID'=poolInfo$UID)
                      ,sum, na.rm=TRUE
                      )
  intermediateMessage('.3')
  
  # rp100 calculations, finally.  Reachlength should not include the distance
  # from the nickpoint to the start of the reach.  It should also not include
  # the distance from the last station to the nonexistant next station.
  # Consequently the incremnt values associated with the nickpoint and last
  # station are removed.
  tt <- stationInfo[order(stationInfo$UID,stationInfo$LOC),]
  tt <- last(tt, 'UID', 'lastStation')
  tt <- subset(tt, LOC>0 & ! lastStation)
  
  
  reachlen <- aggregate(list('reachlen'=tt$INCREMNT)
                       ,list('UID'=tt$UID)
                       ,sum, na.rm=TRUE
                       )
  rp100 <- merge(areasum, reachlen, by='UID')
  rp100$rp100 <- ifelse(is.na(rp100$reachlen), NA
                       ,ifelse(rp100$reachlen==0, NA
                              ,100*rp100$areasum/rp100$reachlen
                              )
                       )
  rp100 <- subset(rp100, select=c(UID,rp100))
  intermediateMessage('.4')

  # combine summaries, setting missing counts to zero
  lengthMets <- merge(rpxlen, rpvlen, by='UID', all=TRUE)
  lengthMets <- merge(lengthMets, rpmxlen, by='UID', all=TRUE)
  lengthMets <- merge(lengthMets, totplen, by='UID', all=TRUE)
  
  depthMets <- merge(rpxdep, rpvdep, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpmxdep, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt50, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt75, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt100, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt05, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt05x, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt10, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt10x, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt20, by='UID', all=TRUE)
  depthMets <- merge(depthMets, rpgt20x, by='UID', all=TRUE)

  areaMets <- merge(rpxarea, rpvarea, by='UID', all=TRUE)
  areaMets <- merge(areaMets, rpmxar, by='UID', all=TRUE)
  areaMets <- merge(areaMets, areasum, by='UID', all=TRUE)
  
  mets <- merge(lengthMets, depthMets, by='UID', all=TRUE)
  mets <- merge(mets, areaMets, by='UID', all=TRUE)
  mets <- merge(mets, rp100, by='UID', all=TRUE)

  intermediateMessage('.  Done.', loc='end')
  return(mets)
}


metsResidualPoolsTest <- function()
# Unit test for metsResidualPools.1()
{
  # Create fake input data
  fakeThal <- metsResidualPools.createThalweg()
  fakeThal <- subset(fakeThal, TRANSECT %in% LETTERS[1:11])
  fakeActransp <- metsResidualPools.createActransp()
  fakeSlopes <- metsResidualPools.createSlopes()
  fakeProtocol <- metsResidualPools.createProtocol()
  
  # Test expected reorganization with both protocols
  intermediateMessage('Testing with both protocols')
  metsResidualPoolsTest.process(fakeThal, fakeActransp, fakeSlopes, fakeProtocol)

  # Create streams-only data and test expected reorganization
  intermediateMessage('Testing with wadeable protocol data')
  fakeThalStreams <- subset(fakeThal
                           ,UID %in% subset(fakeProtocol, PROTOCOL=='WADEABLE')$UID
                           )
  fakeActranspStreams <- subset(fakeActransp
                               ,UID %in% subset(fakeProtocol, PROTOCOL=='WADEABLE')$UID
                               )
  fakeSlopesStreams <- subset(fakeSlopes
                             ,UID %in% subset(fakeProtocol, PROTOCOL=='WADEABLE')$UID
                             )
  fakeProtocolStreams <- subset(fakeProtocol
                               ,UID %in% subset(fakeProtocol, PROTOCOL=='WADEABLE')$UID
                               )

  metsResidualPoolsTest.process(fakeThalStreams, fakeActranspStreams
                               ,fakeSlopesStreams, fakeProtocolStreams
                               )

  # Create rivers-only data and test expected reorganization
  intermediateMessage('Testing with boatable protocol data')
  fakeThalRivers <- subset(fakeThal
                          ,UID %in% subset(fakeProtocol, PROTOCOL=='BOATABLE')$UID
                          )
  fakeActranspRivers <- subset(fakeActransp
                              ,UID %in% subset(fakeProtocol, PROTOCOL=='BOATABLE')$UID
                              )
  fakeSlopesRivers <- subset(fakeSlopes
                            ,UID %in% subset(fakeProtocol, PROTOCOL=='BOATABLE')$UID
                            )
  fakeProtocolRivers <- subset(fakeProtocol
                              ,UID %in% subset(fakeProtocol, PROTOCOL=='BOATABLE')$UID
                              )

  metsResidualPoolsTest.process(fakeThalRivers, fakeActranspRivers
                               ,fakeSlopesRivers, fakeProtocolRivers
                               )

  # Now that individual rp functions are tested, perform an integration test of
  # of the calculation methods
  rr <- metsResidualPools.1(fakeThal, fakeActransp, fakeSlopes, fakeProtocol
                           ,writeIntermediateFiles=FALSE, oldeMethods=TRUE
                           )

  tt <- metsResidualPools.createSiteSummaries(unique(fakeThal$UID))
  ee <- dfLengthen(tt
                  ,'UID','METRIC','RESULT'
                  ,names(tt)[names(tt) %nin% c('UID')]
                  )
  comparisons <- transform(merge(ee,rr, by=c('UID','METRIC')
                                ,suffix= c('.expected','.actual')
                                )
                          ,err = RESULT.actual - RESULT.expected
                          ,relerr = (RESULT.actual - RESULT.expected)/RESULT.expected
                          )
  checkEquals(0, nrow(subset(comparisons, abs(relerr) > 1e-4))
             ,"Error: metsResidualPools.1 failed integration test"
             )
}


metsResidualPoolsTest.process <-
  function(fakeThal, fakeActransp, fakeSlopes, fakeProtocol)
# Does actual testing using provided data.
{
  ######################################################
  # Test data reorganization
  # Compare expected and actual data reorganization.
  # Until we know how to handle side channels, remove them
  seriesResult <- metsResidualPools.dataOrganization(fakeThal, fakeActransp, fakeSlopes)
  seriesResult <- seriesResult[order(seriesResult$UID, seriesResult$LOC),]
  rownames(seriesResult) <- NULL
  expected <- subset(metsResidualPools.createExpectedReorg()
                    ,UID %in% unique(fakeThal$UID)
                    )

  errs <- dfCompare(expected, seriesResult, c('UID','LOC'), zeroFudge=10^-8)
  checkEquals(NULL, errs
             ,"Error: thalweg data not being organized correctly"
             )

  ######################################################
  # Test rp detection and first order calculations
  # Determine expected residual dimensions.
  expected <- metsResidualPools.createInitialCalcs(unique(fakeThal$UID))
  expected <- expected[order(expected$UID, expected$LOC),]

  # Temporarily remove WT_WID for comparison
  expected <- subset(expected, TRANSECT %in% LETTERS, select=-WT_WID)

  rownames(expected) <- NULL

  # Compare expected and actual calculations of residual dimensions
  # Remove siteStart flag prior to comparison
  dimensionsResult <- metsResidualPools.dimensions(seriesResult, fakeProtocol
                                                  ,oldeMethods=TRUE
                                                  )
  dimensionsResult <- dimensionsResult[order(dimensionsResult$UID
                                            ,dimensionsResult$LOC
                                            )
                                      ,]
  dimensionsResult <- subset(dimensionsResult, select=-siteStart)
  rownames(dimensionsResult) <- NULL
  
  # If column names are as expected, order the columns
  checkTrue(all(names(expected) %in% names(dimensionsResult)) &
            all(names(dimensionsResult) %in% names(expected))
           ,"Error: metsResidualPools.dimensions() not using expected names"
           )

  expected <- expected[names(dimensionsResult)]
  diffs <- dfCompare(expected, dimensionsResult, c('UID','LOC'), zeroFudge=1e-2)
  checkEquals(NULL, diffs
             ,"Error: initial rp dimension calculations are incorrect"
             )

  ######################################################
  # Test pool summaries
  # Temporarily get rid of side channel values, which currently only
  # occur in UID= '2004 EPA01-0450 1' in poolID after 25.
  # Note: zeroFudge is set to 1e-2 instead of 1e-5 to allow for large pools
  # in 2002 WMTP99-0601 1: 7, 11, 14, 15, 29, which are off due to earlier
  # truncation errors.
  expected <- metsResidualPools.createPoolCharacteristics(unique(fakeThal$UID))
  expected <- subset(expected, !(UID=='2004 EPA01-0450 1' & poolID > 25))
  expected <- expected[order(expected$UID, expected$poolID),]
  rownames(expected) <- NULL
  poolResults <- metsResidualPools.poolCharacteristics(dimensionsResult)
  poolResults <- poolResults[order(poolResults$UID, poolResults$poolID),]
  rownames(poolResults) <- NULL
  
  diffs <- dfCompare(expected, poolResults, c('UID','poolID'), zeroFudge=1e-2)
  checkEquals(NULL, diffs
             ,"Error: pool characterization calculations are incorrect"
             )

  ######################################################
  # Test site summaries
  expected <- metsResidualPools.createSiteSummaries(unique(fakeThal$UID))
  expected <- expected[order(expected$UID),]
  rownames(expected) <- NULL
  metsResults <- metsResidualPools.siteSummaries(poolResults, dimensionsResult
                                                ,fakeProtocol
                                                )
  metsResults <- metsResults[order(metsResults$UID),]
  rownames(metsResults) <- NULL

  diffs <- dfCompare(expected, metsResults, 'UID', zeroFudge=1e-3)
  checkEquals(NULL, diffs
             ,"Error: site summary calculations are incorrect"
             )

}


metsResidualPools.createThalweg <- function()
# Creates thalweg data used for the metsResidualPools() unit test
# Fake data is taken from these WEMAP sites:
#   2004 ORSE04-R022 1 - normal wadeable reach
#   2004 SHB-0315    1 - wadeable reach with no slope information
#   2004 SHB-0395    1 - wadeable reach with 11 stations per transect.
#   2004 SHB-0395 1 missing incremnt - wadeable reach with no incremnt
#                        information, and also has 11 stations per transect.
#   2004 EPA01-0450  1 - wadeable reach with side channel transects
#   2004 WNVP99-REN1 1 - wadeable reach with too many missing values.
#   2002 WWYP99-0672 1 - normal boatable reach
#   2002 WWYP99-NPR2 1 - boatable reach with no slope information
#   2000 WCAP99-0604 1 - wadeable reach with varying stations per transect
#   2002 WMTP99-0601 1 - boatable reach with varying stations per transect
{
  fakeThal <- rbind(data.frame(
                     UID=rep('2004 ORSE04-R022 1', 100)
                    ,TRANSECT=rep(LETTERS[1:10], each=10)
                    ,STATION=as.integer(rep(0:9, times=10))
                    ,PARAMETER=rep('DEPTH', 100)
                    ,SAMPLE_TYPE=rep('PHAB_THALW', 100)
                    ,RESULT=c(31, 31, 22, 23, 24, 20, 32, 21, 20, 18
                             ,61, 08, 16, 23, 27, 20, 14, 20, 32, 22
                             ,32, 20, 18, 18, 06, 28, 24, 18, 14, 18
                             ,42, 25, 16, 20, 26, 34, 30, 19, 14, 14
                             ,14, 24, 42, 37, 25, 32, 46, 36, 36, 40
                             ,47, 14, 17, 20, 20, 19, 15, 19, 14, 18
                             ,17, 10, 08, 10, 12, 12, 12, 11, 17, 13
                             ,40, 43, 69, 66, 78, 67, 71, 51, 50, 44
                             ,43, 40, 41, 40, 25, 30, 29, 32, 35, 36
                             ,30, 35, 32, 41, 39, 43, 50, 31, 26, 26
                             )
                    ,UNITS=rep('M', 100)
                    )
               ,data.frame(
                     UID=rep('2004 SHB-0315 1', 100)
                    ,TRANSECT=rep(LETTERS[1:10], each=10)
                    ,STATION=rep(0:9, times=10)
                    ,PARAMETER=rep('DEPTH', 100)
                    ,SAMPLE_TYPE=rep('PHAB_THALW', 100)
                    ,RESULT=c(058, 071, 061, 045, 020, 022, 025, 032, 025, 020
                             ,041, 055, 041, 036, 046, 040, 042, 097, 047, 070
                             ,060, 030, 050, 055, 022, 041, 054, 067, 085, 087
                             ,071, 087, 072, 065, 075, 061, 096, 066, 067, 023
                             ,020, 020, 016, 016, 020, 021, 020, 019, 025, 027
                             ,031, 034, 065, 093, 086, 103, 110, 105, 110, 085
                             ,027, 025, 060, 043, 051, 076, 085, 085, 078, 095
                             ,059, 030, 030, 029, 042, 025, 035, 036, 033, 032
                             ,022, 018, 015, 020, 020, 020, 028, 022,  NA, 073
                             ,085, 100, 120, 140, 110, 070 ,057, 035, 061, 025
                             )
                    ,UNITS=rep('M', 100)
                    )
               ,data.frame(
                     UID=rep('2004 SHB-0395 1', 110)
                    ,TRANSECT=rep(LETTERS[1:10], each=11)
                    ,STATION=rep(0:10, times=10)
                    ,PARAMETER=rep('DEPTH', 110)
                    ,SAMPLE_TYPE=rep('PHAB_THALW', 110)
                    ,RESULT=c(017, 025, 025, 030, 039, 040, 050, 055, 056, 024, 049
                             ,035, 035, 044, 042, 034, 043, 036, 036, 038, 038, 045
                             ,036, 034, 046, 080, 100, 150, 140, 085, 072, 044, 046
                             ,044, 048, 051, 041, 048, 060, 040, 036, 030, 034, 036
                             ,034, 039, 042, 039, 054, 043, 042, 030, 034, 035, 039
                             ,037, 046, 054, 060, 059, 056, 056, 040, 046, 034, 083
                             ,048, 048, 062, 036, 038, 065, 100, 054, 035, 080, 036
                             ,049, 056, 084, 035, 015, 036, 050, 040, 042, 045, 090
                             ,069, 055, 038, 076, 034, 050, 046, 049, 032, 035, 042
                             ,050, 052, 045, 039, 034, 046, 056, 045, 048, 033, 044
                             )
                    ,UNITS=rep('M', 110)
                    )
               ,data.frame(
                     UID=rep('2004 SHB-0395 1 missing incremnt', 110)
                    ,TRANSECT=rep(LETTERS[1:10], each=11)
                    ,STATION=rep(0:10, times=10)
                    ,PARAMETER=rep('DEPTH', 110)
                    ,SAMPLE_TYPE=rep('PHAB_THALW', 110)
                    ,RESULT=c(017, 025, 025, 030, 039, 040, 050, 055, 056, 024, 049
                             ,035, 035, 044, 042, 034, 043, 036, 036, 038, 038, 045
                             ,036, 034, 046, 080, 100, 150, 140, 085, 072, 044, 046
                             ,044, 048, 051, 041, 048, 060, 040, 036, 030, 034, 036
                             ,034, 039, 042, 039, 054, 043, 042, 030, 034, 035, 039
                             ,037, 046, 054, 060, 059, 056, 056, 040, 046, 034, 083
                             ,048, 048, 062, 036, 038, 065, 100, 054, 035, 080, 036
                             ,049, 056, 084, 035, 015, 036, 050, 040, 042, 045, 090
                             ,069, 055, 038, 076, 034, 050, 046, 049, 032, 035, 042
                             ,050, 052, 045, 039, 034, 046, 056, 045, 048, 033, 044
                             )
                    ,UNITS=rep('M', 110)
                    )
               ,data.frame(
                     UID=rep('2004 EPA01-0450 1', 125)
                    ,TRANSECT=c(rep(LETTERS[1:10], each=10)
                               ,rep('XA',5), rep('XI', 10), rep('XJ', 10)
                               )
                    ,STATION=c(rep(0:9, times=10)
                              ,rep(0:4), rep(0:9, times=2)
                              )
                    ,PARAMETER=rep('DEPTH', 125)
                    ,SAMPLE_TYPE=rep('PHAB_THALW', 125)
                    ,RESULT=c(55, 38, 33, 52, 56, 60, 50, 51, 43, 41
                             ,42, 56, 50, 49, 55, 64, 60, 75, 50, 49
                             ,68, 37, 77, 95, 80, 51, 45, 42, 36, 40
                             ,40, 41, 38, 66, 46, 45, 35, 41, 44, 74
                             ,55, 62, 65, 43, 64, 67, 54, 32, 34, 46
                             ,76, 58, 50, 64, 66, 49, 28, 35, 41, 66
                             ,70, 62, 58, 30, 28, 26, 82, 40, 49, 57
                             ,63, 76, 39, 50, 44, 42, 36, 94, 47, 49
                             ,87, 63, 45, 52, 49, 85, 35, 47, 39, 36
                             ,55, 60, 62, 66, 63, 74, 54, 54, 52, 64
                             ,30, 67, 60, 26, 29                      # XA
                             ,59, 44, 17, 19, 31, 30, 26, 19, 26, 20  # XI
                             ,24, 25, 50, 35, 56, 25, 29, 50, 21, 21  # XJ
                             )
                    ,UNITS=rep('M', 125)
                    )
               ,data.frame(
                     UID=rep('2004 WNVP99-REN1 1', 150)
                    ,TRANSECT=rep(LETTERS[1:10], each=15)
                    ,STATION=rep(0:14, times=10)
                    ,PARAMETER=rep('DEPTH', 150)
                    ,SAMPLE_TYPE=rep('PHAB_THALW', 150)
                    ,RESULT=c(
                       01, 08, 05, 06, 05, 10, 07, 06, 03, 07, 08, 03, 04, 00, 11
                      ,25, 18, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 05
                      ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                      ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                      ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 01, 03, 03
                      ,02, 03, 03, 03, 02, 02, 11, NA, NA, NA, NA, NA, NA, 02, 05
                      ,02, 03, 03, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                      ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 01
                      ,02, 03, 08, 14, 10, 03, 16, 08, 04, 06, 00, NA, NA, 00, 01
                      ,00, 00, 00, 15, 00, 00, 00, 00, 00, 00, 00, 00, 00, 10, 01
                      )
                    ,UNITS=rep('M', 150)
                    )
               ,data.frame(
                     UID=rep('2002 WWYP99-0672 1', 200)
                    ,TRANSECT=rep(LETTERS[1:10], each=20)
                    ,STATION=rep(0:19, times=10)
                    ,PARAMETER=c(rep('DEP_SONR', 100), rep('DEP_POLE', 100))
                    ,SAMPLE_TYPE=rep('PHAB_THAL', 200)
                    ,RESULT=c(
                       0.90, 1.10, 1.30, 1.30, 1.30, 1.20, 1.50, 1.40, 1.50, 1.60
                      ,1.50, 1.50, 1.50, 1.30, 1.10, 0.90, 0.80, 0.70, 0.80, 0.80
                      ,1.00, 0.50, 0.80, 0.80, 0.50, 0.60, 0.60, 0.50, 0.70, 0.60
                      ,0.90, 1.00, 1.00, 1.00, 2.00, 2.10, 1.90, 1.30, 1.30, 1.20
                      ,1.00, 0.80, 1.00, 1.40, 1.10, 1.20, 1.20, 1.10, 2.10, 2.30
                      ,1.90, 1.30, 1.00, 1.10, 1.00, 1.00, 1.10, 1.60, 1.30, 1.20
                      ,1.20, 1.20, 0.70, 0.40, 0.50, 0.80, 0.80, 0.90, 1.10, 1.20
                      ,1.30, 1.20, 1.30, 1.20, 1.40, 1.40, 1.40, 1.00, 0.80, 0.50
                      ,1.20, 1.40, 0.80, 1.20, 1.00, 1.30, 1.40, 1.20, 1.20, 1.40
                      ,1.00, 1.20, 1.20, 1.50, 1.40, 1.60, 1.30, 1.10, 0.70, 1.00
                      ,2.20, 2.10, 1.80, 1.20, 1.00, 1.30, 0.90, 1.00, 0.90, 0.80
                      ,1.20, 1.30, 1.60, 1.30, 1.20, 1.30, 1.10, 1.10, 1.10, 1.10
                      ,1.50, 1.60, 1.50, 1.10, 1.40, 1.00, 1.20, 1.40, 1.70, 2.00
                      ,2.00, 1.60, 1.20, 1.10, 0.80, 0.80, 1.10, 1.40, 1.60, 1.40
                      ,1.60, 2.00, 2.00, 2.40, 2.10, 1.70, 1.60, 1.70, 1.40, 1.00
                      ,0.80, 1.00, 1.00, 1.00, 1.30, 1.30, 1.00, 0.80, 0.70, 1.00
                      ,0.70, 2.40, 2.00, 0.90, 0.60, 0.70, 0.40, 0.40, 0.50, 0.40
                      ,0.50, 0.40, 0.40, 0.90, 0.80, 0.70, 0.80, 1.40, 0.70, 1.10
                      ,1.00, 1.00, 0.90, 1.30, 1.10, 0.70, 1.40, 2.00, 2.00, 2.00
                      ,1.90, 1.60, 1.30, 1.20, 0.70, 0.60, 0.80, 0.70, 0.90, 1.00
                      )
                    ,UNITS=rep('M', 200)
                    )
               ,data.frame(
                     UID=rep('2002 WWYP99-NPR2 1', 200)
                    ,TRANSECT=rep(LETTERS[1:10], each=20)
                    ,STATION=rep(0:19, times=10)
                    ,PARAMETER=c(rep('DEP_SONR', 100), rep('DEP_POLE', 100))
                    ,SAMPLE_TYPE=rep('PHAB_THAL', 200)
                    ,RESULT=c(
                       1.00, 1.80, 1.90, 1.30, 1.30, 1.30, 1.30, 1.40, 1.70, 1.50
                      ,1.30, 1.40, 1.20, 0.80, 0.40, 0.70, 0.60, 0.60, 0.80, 0.70
                      ,0.60, 0.70, 0.40, 0.70, 0.40, 0.50, 0.60, 0.40, 0.60, 0.70
                      ,0.70, 0.80, 0.60, 0.60, 0.50, 0.50, 1.10, 1.30, 1.10, 0.60
                      ,0.70, 1.10, 0.90, 1.40, 1.10, 1.00, 0.80, 0.90, 0.80, 0.80
                      ,0.60, 1.00, 1.00, 1.10, 0.80, 0.80, 0.60, 0.60, 1.50, 0.90
                      ,0.50, 0.60, 0.50, 0.80, 1.20, 0.90, 0.80, 0.80, 1.20, 1.90
                      ,2.30, 2.10, 1.20, 0.60, 0.20, 1.30, 1.10, 1.20, 0.80, 0.60
                      ,0.40, 0.40, 0.40, 0.60, 0.70, 1.50, 0.70, 0.70, 0.90, 0.90
                      ,0.80, 0.80, 0.80, 0.60, 0.30, 0.30, 0.80, 1.30, 1.50, 1.20
                      ,0.70, 0.70, 1.50, 1.20, 1.00, 0.70, 0.60, 0.80, 0.70, 0.30
                      ,0.90, 1.00, 1.00, 1.10, 1.10, 0.90, 0.80, 0.70, 0.70, 0.50
                      ,0.70, 0.70, 1.20, 0.90, 0.70, 0.80, 1.00, 1.00, 0.80, 0.70
                      ,0.60, 0.70, 0.80, 0.80, 1.00, 1.00, 0.60, 0.80, 0.90, 1.00
                      ,1.00, 0.70, 0.70, 0.60, 0.50, 0.60, 3.00, 0.60, 1.40, 1.60
                      ,1.90, 2.30, 2.30, 1.20, 0.90, 0.90, 1.00, 1.10, 1.80, 1.70
                      ,1.30, 1.00, 1.00, 0.90, 0.60, 1.00, 0.80, 1.00, 1.10, 1.20
                      ,1.00, 0.80, 0.70, 0.90, 0.70, 0.60, 0.90, 0.70, 0.80, 0.90
                      ,1.00, 0.90, 1.50, 2.40, 1.00, 1.30, 0.60, 1.10, 0.90, 0.50
                      ,0.50, 0.80, 1.00, 1.00, 0.60, 0.50, 0.80, 0.70, 0.60, 1.50
                      )
                    ,UNITS=rep('M', 200)
                    )
               ,data.frame(
                     UID=rep('2000 WCAP99-0604 1', 102)
                    ,TRANSECT=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                               ,'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                               ,'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                               ,'C', 'C', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                               ,'D', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                               ,'E', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                               ,'F', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                               ,'G', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                               ,'H', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                               ,'I', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                               ,'J', 'J'
                               )
                    ,STATION=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                              ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                              ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                              ,10, 11, 0, 1, 2, 3, 4, 5, 6, 7
                              ,8, 0, 1, 2, 3, 4, 5, 6, 7, 8
                              ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                              ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                              ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                              ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                              ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                              ,9, 10
                              )
                    ,PARAMETER=rep('DEPTH', 102)
                    ,SAMPLE_TYPE=rep('PHAB_THALW', 102)
                    ,RESULT=c(15, 88, 30, 25, 20, 5, 20, 30, 25, 31
                             ,20, 15, 23, 16, 20, 6, 23, 12, 21, 19
                             ,20, 36, 25, 10, 25, 7, 37, 5, 40, 29
                             ,30, 8, 10, 8, 38, 10, 20, 80, 37, 3
                             ,20, 20, 8, 20, 40, 28, 12, 30, 8, 16
                             ,23, 10, 11, 9, 35, 40, 21, 31, 32, 41
                             ,45, 23, 20, 40, 35, 20, 35, 20, 20, 9
                             ,38, 45, 15, 20, 39, 30, 37, 40, 35, 20
                             ,55, 25, 33, 15, 33, 25, 31, 27, 16, 15
                             ,31, 40, 20, 28, 12, 8, 11, 30, 40, 62
                             ,28, 30
                             )
                    ,UNITS=rep('CM', 102)
                    )
               ,data.frame(
                     UID=rep('2002 WMTP99-0601 1', 193)
                    ,TRANSECT=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                               ,'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'B', 'B'
                               ,'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                               ,'B', 'B', 'B', 'B', 'B', 'C', 'C', 'C', 'C', 'C'
                               ,'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                               ,'C', 'C', 'C', 'C', 'D', 'D', 'D', 'D', 'D', 'D'
                               ,'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                               ,'D', 'D', 'D', 'D', 'E', 'E', 'E', 'E', 'E', 'E'
                               ,'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                               ,'E', 'E', 'E', 'E', 'F', 'F', 'F', 'F', 'F', 'F'
                               ,'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                               ,'F', 'F', 'F', 'F', 'G', 'G', 'G', 'G', 'G', 'G'
                               ,'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                               ,'G', 'G', 'G', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                               ,'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                               ,'H', 'H', 'H', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                               ,'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                               ,'I', 'I', 'I', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                               ,'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                               ,'J', 'J', 'J'
                               )
                    ,STATION=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                              ,10, 11, 12, 13, 14, 15, 16, 17, 0, 1
                              ,2, 3, 4, 5, 6, 7, 8, 9, 10, 11
                              ,12, 13, 14, 15, 16, 0, 1, 2, 3, 4
                              ,5, 6, 7, 8, 9, 10, 11, 12, 13, 14
                              ,15, 16, 17, 18, 0, 1, 2, 3, 4, 5
                              ,6, 7, 8, 9, 10, 11, 12, 13, 14, 15
                              ,16, 17, 18, 19, 0, 1, 2, 3, 4, 5
                              ,6, 7, 8, 9, 10, 11, 12, 13, 14, 15
                              ,16, 17, 18, 19, 0, 1, 2, 3, 4, 5
                              ,6, 7, 8, 9, 10, 11, 12, 13, 14, 15
                              ,16, 17, 18, 19, 0, 1, 2, 3, 4, 5
                              ,6, 7, 8, 9, 10, 11, 12, 13, 14
                              ,15, 16, 17, 18, 0, 1, 2, 3, 4, 5
                              ,6, 7, 8, 9, 10, 11, 12, 13, 14, 15
                              ,16, 17, 18, 19, 0, 1, 2, 3, 4, 5
                              ,6, 7, 8, 9, 10, 11, 12, 13, 14
                              ,15, 16, 17, 18, 19, 0, 1, 2, 3, 4
                              ,5, 6, 7, 8, 9, 10, 11, 12, 13, 14
                              ,15, 16, 17, 18, 19
                              )
                    ,PARAMETER=rep('DEP_POLE',193)
                    ,SAMPLE_TYPE=rep('PHAB_THAL', 193)
                    ,RESULT=c(1.3, 1.2, 1.1, 1, 0.9, 0.8, 0.9, 0.9, 1, 1
                             ,1, 1.1, 1.4, 1.8, 1.8, 1.5, 1.4, 1.1, 0.9, 0.8
                             ,0.8, 0.9, 0.9, 0.9, 1, 0.9, 1.2, 1, 1.1, 1.1
                             ,1, 1.1, 1.4, 1.2, 1.3, 1.1, 1.5, 1.3, 1.8, 2.2
                             ,2, 1.4, 0.8, 0.9, 1.2, 1.3, 1.5, 0.9, 1.1, 1.7
                             ,2, 2, 2.1, 1.7, 1.9, 1.5, 1.5, 1.3, 1.2, 1
                             ,1.2, 1.4, 1.2, 1.2, 1.2, 1.2, 1.5, 1.5, 1.3, 1.4
                             ,1.3, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.1, 1.2, 1.1
                             ,1.1, 1.1, 1.1, 1.1, 1.2, 1.3, 1.3, 1.2, 1.3, 1.3
                             ,1.2, 1.3, 1.3, 1.1, 1, 0.9, 0.7, 0.8, 1.3, 1.9
                             ,1.6, 1.6, 1.5, 1.3, 1.3, 1.3, 1.3, 1.2, 1, 1
                             ,1.1, 1.3, 2, 1.2, 1.2, 1.2, 1.2, 1.1, 1.1, 1.4
                             ,1.8, 1.8, 2.2, 2.5, 1.7, 1.3, 1.2, 1, 1.1, 1
                             ,1, 1.1, 1.1, 0.7, 1, 1.2, 1.3, 1.3, 1.6, 1.6
                             ,1.4, 1.8, 1.6, 1.3, 1.2, 1.2, 1, 1.1, 1.1, 1
                             ,1.8, 1.4, 1.1, 1.8, 2, 2.1, 2.3, 1.6, 1.9, 1.7
                             ,1.4, 1.3, 1.1, 1.3, 1.3, 1.2, 1.1, 1.1, 1.2, 1.5
                             ,1.3, 1.3, 1, 0.8, 1, 1, 1.3, 1.6, 1.3, 1.5
                             ,1.4, 1.4, 1.5, 1.3, 1.6, 1.6, 1.6, 1.5, 1.4, 1.3
                             ,1.4, 1.5, 1.4
                             )
                    ,UNITS=rep('M', 193)
                    )
              )
  fakeThal$UID <- as.character(fakeThal$UID)
  fakeThal$TRANSECT <- as.character(fakeThal$TRANSECT)
  fakeThal$PARAMETER <- as.character(fakeThal$PARAMETER)
  fakeThal$RESULT <- as.character(fakeThal$RESULT)

  # Add in incremnt values to wadeable reaches:
  incr <- fakeThal
  incr$PARAMETER <- 'INCREMNT'
  incr$RESULT <- ifelse(incr$UID=='2004 ORSE04-R022 1', 1.5
                ,ifelse(incr$UID=='2004 SHB-0315 1',    3.2
                ,ifelse(incr$UID=='2004 SHB-0395 1',    4.7272727
                ,ifelse(incr$UID=='2004 SHB-0395 1 missing incremnt', NA
                ,ifelse(incr$UID=='2004 EPA01-0450 1',  3.2
                ,ifelse(incr$UID=='2004 WNVP99-REN1 1', 1.5
                ,ifelse(incr$UID=='2000 WCAP99-0604 1', 1.5
                ,ifelse(incr$UID=='2002 WWYP99-0672 1'
                       ,ifelse(incr$TRANSECT=='A', 21.2369
                       ,ifelse(incr$TRANSECT=='B', 35.35065
                       ,ifelse(incr$TRANSECT=='C', 26.69925
                       ,ifelse(incr$TRANSECT=='D', 30.92955
                       ,ifelse(incr$TRANSECT=='E', 27.29605
                       ,ifelse(incr$TRANSECT=='F', 30.9275
                       ,ifelse(incr$TRANSECT=='G', 29.45835
                       ,ifelse(incr$TRANSECT=='H', 31.2091
                       ,ifelse(incr$TRANSECT=='I', 35.83085
                       ,ifelse(incr$TRANSECT=='J', 27.97765
                       ,NA
                       ))))))))))
                ,ifelse(incr$UID=='2002 WWYP99-NPR2 1'
                       ,ifelse(incr$TRANSECT=='A', 22.99615
                       ,ifelse(incr$TRANSECT=='B', 36.17845
                       ,ifelse(incr$TRANSECT=='C', 36.7343
                       ,ifelse(incr$TRANSECT=='D', 32.6829
                       ,ifelse(incr$TRANSECT=='E', 39.75945
                       ,ifelse(incr$TRANSECT=='F', 39.36215
                       ,ifelse(incr$TRANSECT=='G', 39.2826
                       ,ifelse(incr$TRANSECT=='H', 34.9146
                       ,ifelse(incr$TRANSECT=='I', 41.7576
                       ,ifelse(incr$TRANSECT=='J', 36.56745
                       ,NA
                       ))))))))))
                ,ifelse(incr$UID=='2002 WMTP99-0601 1'
                       ,ifelse(incr$TRANSECT=='A', 16.287222222
                       ,ifelse(incr$TRANSECT=='B', 35.914705882
                       ,ifelse(incr$TRANSECT=='C', 30.194210526
                       ,ifelse(incr$TRANSECT=='D', 66.048
                       ,ifelse(incr$TRANSECT=='E', 23.749
                       ,ifelse(incr$TRANSECT=='F', 23.5635
                       ,ifelse(incr$TRANSECT=='G', 19.04947368
                       ,ifelse(incr$TRANSECT=='H', 31.049
                       ,ifelse(incr$TRANSECT=='I', 30.739
                       ,ifelse(incr$TRANSECT=='J', 23.0875
                       ,NA
                       ))))))))))
                ,NA
                ))))))))))
  incr$RESULT <- as.character(incr$RESULT)
  fakeThal <- rbind(fakeThal, incr)

  return(fakeThal)
}

metsResidualPools.createExpectedReorg <- function()
# Create expected results, converting stream depths in cm to m.
# The fakeThal argument should not contain any side-channel information.
{
  expected <- rbind(data.frame(UID = rep('2000 WCAP99-0604 1', times=103)
                              ,TRANSECT = c(rep('A', times=11)
                                           ,rep('B', times=10)
                                           ,rep('C', times=12)
                                           ,rep('D', times=9)
                                           ,rep('E', times=10)
                                           ,rep('F', times=10)
                                           ,rep('G', times=10)
                                           ,rep('H', times=10)
                                           ,rep('I', times=10)
                                           ,rep('J', times=11)
                                           )
                              ,STATION = c(0, 0:9, 0:9, 0:11, 0:8, 0:9
                                          ,0:9, 0:9, 0:9, 0:9, 0:10
                                          )
                              ,DEPTH = c(0.11101544307011, 0.15, 0.88, 0.3, 0.25, 0.2, 0.05, 0.2, 0.3, 0.25
                                        ,0.31, 0.2, 0.15, 0.23, 0.16, 0.2, 0.06, 0.23, 0.12, 0.21
                                        ,0.19, 0.2, 0.36, 0.25, 0.1, 0.25, 0.07, 0.37, 0.05, 0.4
                                        ,0.29, 0.3, 0.08, 0.1, 0.08, 0.38, 0.1, 0.2, 0.8, 0.37
                                        ,0.03, 0.2, 0.2, 0.08, 0.2, 0.4, 0.28, 0.12, 0.3, 0.08
                                        ,0.16, 0.23, 0.1, 0.11, 0.09, 0.35, 0.4, 0.21, 0.31, 0.32
                                        ,0.41, 0.45, 0.23, 0.2, 0.4, 0.35, 0.2, 0.35, 0.2, 0.2
                                        ,0.09, 0.38, 0.45, 0.15, 0.2, 0.39, 0.3, 0.37, 0.4, 0.35
                                        ,0.2, 0.55, 0.25, 0.33, 0.15, 0.33, 0.25, 0.31, 0.27, 0.16
                                        ,0.15, 0.31, 0.4, 0.2, 0.28, 0.12, 0.08, 0.11, 0.3, 0.4
                                        ,0.62, 0.28, 0.3
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 43, 44, 45, 46, 47, 48, 49, 50
                                                 ,51, 52, 53, 54, 55, 56, 57, 58, 59, 60
                                                 ,61, 62, 63, 64, 65, 66, 67, 68, 69, 70
                                                 ,71, 72, 73, 74, 75, 76, 77, 78, 79, 80
                                                 ,81, 82, 83, 84, 85, 86, 87, 88, 89, 90
                                                 ,91, 92, 93, 94, 95, 96, 97, 98, 99, 100
                                                 ,101, 102, 103
                                                 )
                                               )
                              ,INCREMNT = rep(1.5, length=103)
                              ,stackSlope = rep(3.53, length=103)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID = rep('2002 WMTP99-0601 1', times=194)
                              ,TRANSECT = c(rep('J', times=21)
                                           ,rep('I', times=20)
                                           ,rep('H', times=20)
                                           ,rep('G', times=19)
                                           ,rep('F', times=20)
                                           ,rep('E', times=20)
                                           ,rep('D', times=20)
                                           ,rep('C', times=19)
                                           ,rep('B', times=17)
                                           ,rep('A', times=18)
                                           )
                              ,STATION = c(19, 19:0, 19:0, 19:0, 18:0, 19:0
                                          ,19:0, 19:0, 18:0, 16:0, 17:0
                                          )
                              ,DEPTH = c(0.977841534308785, 1.4, 1.5, 1.4, 1.3, 1.4, 1.5, 1.6, 1.6, 1.6
                                        ,1.3, 1.5, 1.4, 1.4, 1.5, 1.3, 1.6, 1.3, 1, 1
                                        ,0.8, 1, 1.3, 1.3, 1.5, 1.2, 1.1, 1.1, 1.2, 1.3
                                        ,1.3, 1.1, 1.3, 1.4, 1.7, 1.9, 1.6, 2.3, 2.1, 2
                                        ,1.8, 1.1, 1.4, 1.8, 1, 1.1, 1.1, 1, 1.2, 1.2
                                        ,1.3, 1.6, 1.8, 1.4, 1.6, 1.6, 1.3, 1.3, 1.2, 1
                                        ,0.7, 1.1, 1.1, 1, 1, 1.1, 1, 1.2, 1.3, 1.7
                                        ,2.5, 2.2, 1.8, 1.8, 1.4, 1.1, 1.1, 1.2, 1.2, 1.2
                                        ,1.2, 2, 1.3, 1.1, 1, 1, 1.2, 1.3, 1.3, 1.3
                                        ,1.3, 1.5, 1.6, 1.6, 1.9, 1.3, 0.8, 0.7, 0.9, 1
                                        ,1.1, 1.3, 1.3, 1.2, 1.3, 1.3, 1.2, 1.3, 1.3, 1.2
                                        ,1.1, 1.1, 1.1, 1.1, 1.1, 1.2, 1.1, 1.2, 1.2, 1.2
                                        ,1.2, 1.2, 1.2, 1.3, 1.4, 1.3, 1.5, 1.5, 1.2, 1.2
                                        ,1.2, 1.2, 1.4, 1.2, 1, 1.2, 1.3, 1.5, 1.5, 1.9
                                        ,1.7, 2.1, 2, 2, 1.7, 1.1, 0.9, 1.5, 1.3, 1.2
                                        ,0.9, 0.8, 1.4, 2, 2.2, 1.8, 1.3, 1.5, 1.1, 1.3
                                        ,1.2, 1.4, 1.1, 1, 1.1, 1.1, 1, 1.2, 0.9, 1
                                        ,0.9, 0.9, 0.9, 0.8, 0.8, 0.9, 1.1, 1.4, 1.5, 1.8
                                        ,1.8, 1.4, 1.1, 1, 1, 1, 0.9, 0.9, 0.8, 0.9
                                        ,1, 1.1, 1.2, 1.3
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                                 ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                                 ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                                 ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                                 ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                                 ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                                 ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                                                 ,110, 111, 112, 113, 114, 115, 116, 117, 118, 119
                                                 ,120, 121, 122, 123, 124, 125, 126, 127, 128, 129
                                                 ,130, 131, 132, 133, 134, 135, 136, 137, 138, 139
                                                 ,140, 141, 142, 143, 144, 145, 146, 147, 148, 149
                                                 ,150, 151, 152, 153, 154, 155, 156, 157, 158, 159
                                                 ,160, 161, 162, 163, 164, 165, 166, 167, 168, 169
                                                 ,170, 171, 172, 173, 174, 175, 176, 177, 178, 179
                                                 ,180, 181, 182, 183, 184, 185, 186, 187, 188, 189
                                                 ,190, 191, 192, 193
                                                 )
                                               )
                              ,INCREMNT = c(rep(23.0875, times=21)
                                           ,rep(30.739, times=20)
                                           ,rep(31.049, times=20)
                                           ,rep(19.04947368, times=19)
                                           ,rep(23.5635, times=20)
                                           ,rep(23.749, times=20)
                                           ,rep(66.048, times=20)
                                           ,rep(30.194210526, times=19)
                                           ,rep(35.914705882, times=17)
                                           ,rep(16.287222222, times=18)
                                           )
                              ,stackSlope = rep(0.19775, times=194)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID = rep('2002 WWYP99-0672 1', times=201)
                              ,TRANSECT = c(rep('J', times=21)
                                           ,rep('I', times=20)
                                           ,rep('H', times=20)
                                           ,rep('G', times=20)
                                           ,rep('F', times=20)
                                           ,rep('E', times=20)
                                           ,rep('D', times=20)
                                           ,rep('C', times=20)
                                           ,rep('B', times=20)
                                           ,rep('A', times=20)
                                           )
                              ,STATION = c(19, 19:0, 19:0, 19:0, 19:0, 19:0
                                          ,19:0, 19:0, 19:0, 19:0, 19:0
                                          )
                              ,DEPTH = c(0.747770916242412, 1, 0.9, 0.7, 0.8, 0.6, 0.7, 1.2, 1.3, 1.6
                                        ,1.9, 2, 2, 2, 1.4, 0.7, 1.1, 1.3, 0.9, 1
                                        ,1, 1.1, 0.7, 1.4, 0.8, 0.7, 0.8, 0.9, 0.4, 0.4
                                        ,0.5, 0.4, 0.5, 0.4, 0.4, 0.7, 0.6, 0.9, 2, 2.4
                                        ,0.7, 1, 0.7, 0.8, 1, 1.3, 1.3, 1, 1, 1
                                        ,0.8, 1, 1.4, 1.7, 1.6, 1.7, 2.1, 2.4, 2, 2
                                        ,1.6, 1.4, 1.6, 1.4, 1.1, 0.8, 0.8, 1.1, 1.2, 1.6
                                        ,2, 2, 1.7, 1.4, 1.2, 1, 1.4, 1.1, 1.5, 1.6
                                        ,1.5, 1.1, 1.1, 1.1, 1.1, 1.3, 1.2, 1.3, 1.6, 1.3
                                        ,1.2, 0.8, 0.9, 1, 0.9, 1.3, 1, 1.2, 1.8, 2.1
                                        ,2.2, 1, 0.7, 1.1, 1.3, 1.6, 1.4, 1.5, 1.2, 1.2
                                        ,1, 1.4, 1.2, 1.2, 1.4, 1.3, 1, 1.2, 0.8, 1.4
                                        ,1.2, 0.5, 0.8, 1, 1.4, 1.4, 1.4, 1.2, 1.3, 1.2
                                        ,1.3, 1.2, 1.1, 0.9, 0.8, 0.8, 0.5, 0.4, 0.7, 1.2
                                        ,1.2, 1.2, 1.3, 1.6, 1.1, 1, 1, 1.1, 1, 1.3
                                        ,1.9, 2.3, 2.1, 1.1, 1.2, 1.2, 1.1, 1.4, 1, 0.8
                                        ,1, 1.2, 1.3, 1.3, 1.9, 2.1, 2, 1, 1, 1
                                        ,0.9, 0.6, 0.7, 0.5, 0.6, 0.6, 0.5, 0.8, 0.8, 0.5
                                        ,1, 0.8, 0.8, 0.7, 0.8, 0.9, 1.1, 1.3, 1.5, 1.5
                                        ,1.5, 1.6, 1.5, 1.4, 1.5, 1.2, 1.3, 1.3, 1.3, 1.1
                                        ,0.9
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                                 ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                                 ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                                 ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                                 ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                                 ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                                 ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                                                 ,110, 111, 112, 113, 114, 115, 116, 117, 118, 119
                                                 ,120, 121, 122, 123, 124, 125, 126, 127, 128, 129
                                                 ,130, 131, 132, 133, 134, 135, 136, 137, 138, 139
                                                 ,140, 141, 142, 143, 144, 145, 146, 147, 148, 149
                                                 ,150, 151, 152, 153, 154, 155, 156, 157, 158, 159
                                                 ,160, 161, 162, 163, 164, 165, 166, 167, 168, 169
                                                 ,170, 171, 172, 173, 174, 175, 176, 177, 178, 179
                                                 ,180, 181, 182, 183, 184, 185, 186, 187, 188, 189
                                                 ,190, 191, 192, 193, 194, 195, 196, 197, 198, 199
                                                 ,200
                                                 ))
                              ,INCREMNT = c(rep(27.97765, times=21)
                                           ,rep(35.83085, times=20)
                                           ,rep(31.2091, times=20)
                                           ,rep(29.45835, times=20)
                                           ,rep(30.9275, times=20)
                                           ,rep(27.29605, times=20)
                                           ,rep(30.92955, times=20)
                                           ,rep(26.69925, times=20)
                                           ,rep(35.35065, times=20)
                                           ,rep(21.2369, times=20)
                                           )
                              ,stackSlope = rep(0.218, times=201)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID = rep('2002 WWYP99-NPR2 1', times=201)
                              ,TRANSECT = c(rep('J', times=21)
                                           ,rep('I', times=20)
                                           ,rep('H', times=20)
                                           ,rep('G', times=20)
                                           ,rep('F', times=20)
                                           ,rep('E', times=20)
                                           ,rep('D', times=20)
                                           ,rep('C', times=20)
                                           ,rep('B', times=20)
                                           ,rep('A', times=20)
                                           )
                              ,STATION = c(19, 19:0, 19:0, 19:0, 19:0, 19:0
                                          ,19:0, 19:0, 19:0, 19:0, 19:0
                                          )
                              ,DEPTH = c(0.515202205623001, 1.5, 0.6, 0.7, 0.8, 0.5, 0.6, 1, 1, 0.8
                                        ,0.5, 0.5, 0.9, 1.1, 0.6, 1.3, 1, 2.4, 1.5, 0.9
                                        ,1, 0.9, 0.8, 0.7, 0.9, 0.6, 0.7, 0.9, 0.7, 0.8
                                        ,1, 1.2, 1.1, 1, 0.8, 1, 0.6, 0.9, 1, 1
                                        ,1.3, 1.7, 1.8, 1.1, 1, 0.9, 0.9, 1.2, 2.3, 2.3
                                        ,1.9, 1.6, 1.4, 0.6, 3, 0.6, 0.5, 0.6, 0.7, 0.7
                                        ,1, 1, 0.9, 0.8, 0.6, 1, 1, 0.8, 0.8, 0.7
                                        ,0.6, 0.7, 0.8, 1, 1, 0.8, 0.7, 0.9, 1.2, 0.7
                                        ,0.7, 0.5, 0.7, 0.7, 0.8, 0.9, 1.1, 1.1, 1, 1
                                        ,0.9, 0.3, 0.7, 0.8, 0.6, 0.7, 1, 1.2, 1.5, 0.7
                                        ,0.7, 1.2, 1.5, 1.3, 0.8, 0.3, 0.3, 0.6, 0.8, 0.8
                                        ,0.8, 0.9, 0.9, 0.7, 0.7, 1.5, 0.7, 0.6, 0.4, 0.4
                                        ,0.4, 0.6, 0.8, 1.2, 1.1, 1.3, 0.2, 0.6, 1.2, 2.1
                                        ,2.3, 1.9, 1.2, 0.8, 0.8, 0.9, 1.2, 0.8, 0.5, 0.6
                                        ,0.5, 0.9, 1.5, 0.6, 0.6, 0.8, 0.8, 1.1, 1, 1
                                        ,0.6, 0.8, 0.8, 0.9, 0.8, 1, 1.1, 1.4, 0.9, 1.1
                                        ,0.7, 0.6, 1.1, 1.3, 1.1, 0.5, 0.5, 0.6, 0.6, 0.8
                                        ,0.7, 0.7, 0.6, 0.4, 0.6, 0.5, 0.4, 0.7, 0.4, 0.7
                                        ,0.6, 0.7, 0.8, 0.6, 0.6, 0.7, 0.4, 0.8, 1.2, 1.4
                                        ,1.3, 1.5, 1.7, 1.4, 1.3, 1.3, 1.3, 1.3, 1.9, 1.8
                                        ,1
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                                 ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                                 ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                                 ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                                 ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                                 ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                                 ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                                                 ,110, 111, 112, 113, 114, 115, 116, 117, 118, 119
                                                 ,120, 121, 122, 123, 124, 125, 126, 127, 128, 129
                                                 ,130, 131, 132, 133, 134, 135, 136, 137, 138, 139
                                                 ,140, 141, 142, 143, 144, 145, 146, 147, 148, 149
                                                 ,150, 151, 152, 153, 154, 155, 156, 157, 158, 159
                                                 ,160, 161, 162, 163, 164, 165, 166, 167, 168, 169
                                                 ,170, 171, 172, 173, 174, 175, 176, 177, 178, 179
                                                 ,180, 181, 182, 183, 184, 185, 186, 187, 188, 189
                                                 ,190, 191, 192, 193, 194, 195, 196, 197, 198, 199
                                                 ,200
                                                 ))
                              ,INCREMNT = c(rep(36.56745, times=21)
                                           ,rep(41.7576, times=20)
                                           ,rep(34.9146, times=20)
                                           ,rep(39.2826, times=20)
                                           ,rep(39.36215, times=20)
                                           ,rep(39.75945, times=20)
                                           ,rep(32.6829, times=20)
                                           ,rep(36.7343, times=20)
                                           ,rep(36.17845, times=20)
                                           ,rep(22.99615, times=20)
                                           )
                              ,stackSlope = rep(NA, times=201)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID = rep('2004 EPA01-0450 1', times=101)
                              ,TRANSECT = c(rep('A', times=11)
                                           ,rep('B', times=10)
                                           ,rep('C', times=10)
                                           ,rep('D', times=10)
                                           ,rep('E', times=10)
                                           ,rep('F', times=10)
                                           ,rep('G', times=10)
                                           ,rep('H', times=10)
                                           ,rep('I', times=10)
                                           ,rep('J', times=10)
                                           )
                              ,STATION = c(0, 0:9, 0:9, 0:9, 0:9, 0:9
                                          ,0:9, 0:9, 0:9, 0:9, 0:9
                                          )
                              ,DEPTH = c(0.381929831472502, 0.55, 0.38, 0.33, 0.52, 0.56, 0.6, 0.5, 0.51, 0.43
                                        ,0.41, 0.42, 0.56, 0.5, 0.49, 0.55, 0.64, 0.6, 0.75, 0.5
                                        ,0.49, 0.68, 0.37, 0.77, 0.95, 0.8, 0.51, 0.45, 0.42, 0.36
                                        ,0.4, 0.4, 0.41, 0.38, 0.66, 0.46, 0.45, 0.35, 0.41, 0.44
                                        ,0.74, 0.55, 0.62, 0.65, 0.43, 0.64, 0.67, 0.54, 0.32, 0.34
                                        ,0.46, 0.76, 0.58, 0.5, 0.64, 0.66, 0.49, 0.28, 0.35, 0.41
                                        ,0.66, 0.7, 0.62, 0.58, 0.3, 0.28, 0.26, 0.82, 0.4, 0.49
                                        ,0.57, 0.63, 0.76, 0.39, 0.5, 0.44, 0.42, 0.36, 0.94, 0.47
                                        ,0.49, 0.87, 0.63, 0.45, 0.52, 0.49, 0.85, 0.35, 0.47, 0.39
                                        ,0.36, 0.55, 0.6, 0.62, 0.66, 0.63, 0.74, 0.54, 0.54, 0.52
                                        ,0.64
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                                 ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                                 ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                                 ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                                 ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                                 ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                                 ,100
                                                 ))
                              ,INCREMNT = rep(3.2, times=101)
                              ,stackSlope = rep(1.739, times=101)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID = rep('2004 ORSE04-R022 1', times=101)
                              ,TRANSECT = c(rep('A', times=11)
                                           ,rep('B', times=10)
                                           ,rep('C', times=10)
                                           ,rep('D', times=10)
                                           ,rep('E', times=10)
                                           ,rep('F', times=10)
                                           ,rep('G', times=10)
                                           ,rep('H', times=10)
                                           ,rep('I', times=10)
                                           ,rep('J', times=10)
                                           )
                              ,STATION = c(0, 0:9, 0:9, 0:9, 0:9, 0:9
                                          ,0:9, 0:9, 0:9, 0:9, 0:9
                                          )
                              ,DEPTH = c(0.13652217660186, 0.31, 0.31, 0.22, 0.23, 0.24, 0.2, 0.32, 0.21, 0.2
                                        ,0.18, 0.61, 0.08, 0.16, 0.23, 0.27, 0.2, 0.14, 0.2, 0.32
                                        ,0.22, 0.32, 0.2, 0.18, 0.18, 0.06, 0.28, 0.24, 0.18, 0.14
                                        ,0.18, 0.42, 0.25, 0.16, 0.2, 0.26, 0.34, 0.3, 0.19, 0.14
                                        ,0.14, 0.14, 0.24, 0.42, 0.37, 0.25, 0.32, 0.46, 0.36, 0.36
                                        ,0.4, 0.47, 0.14, 0.17, 0.2, 0.2, 0.19, 0.15, 0.19, 0.14
                                        ,0.18, 0.17, 0.1, 0.08, 0.1, 0.12, 0.12, 0.12, 0.11, 0.17
                                        ,0.13, 0.4, 0.43, 0.69, 0.66, 0.78, 0.67, 0.71, 0.51, 0.5
                                        ,0.44, 0.43, 0.4, 0.41, 0.4, 0.25, 0.3, 0.29, 0.32, 0.35
                                        ,0.36, 0.3, 0.35, 0.32, 0.41, 0.39, 0.43, 0.5, 0.31, 0.26
                                        ,0.26
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                                 ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                                 ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                                 ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                                 ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                                 ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                                 ,100
                                                 ))
                              ,INCREMNT = rep(1.5, times=101)
                              ,stackSlope = rep(1.095, times=101)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID = rep('2004 SHB-0315 1', times=101)
                              ,TRANSECT = c(rep('A', times=11)
                                           ,rep('B', times=10)
                                           ,rep('C', times=10)
                                           ,rep('D', times=10)
                                           ,rep('E', times=10)
                                           ,rep('F', times=10)
                                           ,rep('G', times=10)
                                           ,rep('H', times=10)
                                           ,rep('I', times=10)
                                           ,rep('J', times=10)
                                           )
                              ,STATION = c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                          )
                              ,DEPTH = c(0.227618855082317, 0.58, 0.71, 0.61, 0.45, 0.2, 0.22, 0.25, 0.32, 0.25
                                        ,0.2, 0.41, 0.55, 0.41, 0.36, 0.46, 0.4, 0.42, 0.97, 0.47
                                        ,0.7, 0.6, 0.3, 0.5, 0.55, 0.22, 0.41, 0.54, 0.67, 0.85
                                        ,0.87, 0.71, 0.87, 0.72, 0.65, 0.75, 0.61, 0.96, 0.66, 0.67
                                        ,0.23, 0.2, 0.2, 0.16, 0.16, 0.2, 0.21, 0.2, 0.19, 0.25
                                        ,0.27, 0.31, 0.34, 0.65, 0.93, 0.86, 1.03, 1.1, 1.05, 1.1
                                        ,0.85, 0.27, 0.25, 0.6, 0.43, 0.51, 0.76, 0.85, 0.85, 0.78
                                        ,0.95, 0.59, 0.3, 0.3, 0.29, 0.42, 0.25, 0.35, 0.36, 0.33
                                        ,0.32, 0.22, 0.18, 0.15, 0.2, 0.2, 0.2, 0.28, 0.22, NA
                                        ,0.73, 0.85, 1, 1.2, 1.4, 1.1, 0.7, 0.57, 0.35, 0.61
                                        ,0.25
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                                 ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                                 ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                                 ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                                 ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                                 ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                                 ,100
                                                 ))
                              ,INCREMNT = rep(3.2, times=101)
                              ,stackSlope = rep(NA, times=101)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID = rep('2004 SHB-0395 1', times=111)
                              ,TRANSECT = c(rep('A', times=12)
                                           ,rep('B', times=11)
                                           ,rep('C', times=11)
                                           ,rep('D', times=11)
                                           ,rep('E', times=11)
                                           ,rep('F', times=11)
                                           ,rep('G', times=11)
                                           ,rep('H', times=11)
                                           ,rep('I', times=11)
                                           ,rep('J', times=11)
                                           )
                              ,STATION = c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                          )
                              ,DEPTH = c(0.277160440099518, 0.17, 0.25, 0.25, 0.3, 0.39, 0.4, 0.5, 0.55, 0.56
                                        ,0.24, 0.49, 0.35, 0.35, 0.44, 0.42, 0.34, 0.43, 0.36, 0.36
                                        ,0.38, 0.38, 0.45, 0.36, 0.34, 0.46, 0.8, 1, 1.5, 1.4, 0.85     ## Has 11 on a row
                                        ,0.72, 0.44, 0.46, 0.44, 0.48, 0.51, 0.41, 0.48, 0.6, 0.4
                                        ,0.36, 0.3, 0.34, 0.36, 0.34, 0.39, 0.42, 0.39, 0.54, 0.43
                                        ,0.42, 0.3, 0.34, 0.35, 0.39, 0.37, 0.46, 0.54, 0.6, 0.59
                                        ,0.56, 0.56, 0.4, 0.46, 0.34, 0.83, 0.48, 0.48, 0.62, 0.36
                                        ,0.38, 0.65, 1, 0.54, 0.35, 0.8, 0.36, 0.49, 0.56, 0.84
                                        ,0.35, 0.15, 0.36, 0.5, 0.4, 0.42, 0.45, 0.9, 0.69, 0.55
                                        ,0.38, 0.76, 0.34, 0.5, 0.46, 0.49, 0.32, 0.35, 0.42, 0.5
                                        ,0.52, 0.45, 0.39, 0.34, 0.46, 0.56, 0.45, 0.48, 0.33, 0.44
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                                 ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                                 ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                                 ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                                 ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                                 ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                                 ,100, 101, 102, 103, 104, 105, 106, 107
                                                 ,108, 109, 110
                                                 ))
                              ,INCREMNT = rep(4.7272727, times=111)
                              ,stackSlope = rep(0.78875, times=111)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID = rep('2004 SHB-0395 1 missing incremnt', times=111)
                              ,TRANSECT = c(rep('A', times=12)
                                           ,rep('B', times=11)
                                           ,rep('C', times=11)
                                           ,rep('D', times=11)
                                           ,rep('E', times=11)
                                           ,rep('F', times=11)
                                           ,rep('G', times=11)
                                           ,rep('H', times=11)
                                           ,rep('I', times=11)
                                           ,rep('J', times=11)
                                           )
                              ,STATION = c(0, 0:10, 0:10, 0:10, 0:10, 0:10
                                          ,0:10, 0:10, 0:10, 0:10, 0:10
                                          )
                              ,DEPTH = c(0.277160440099518, 0.17, 0.25, 0.25, 0.3, 0.39, 0.4, 0.5, 0.55, 0.56
                                        ,0.24, 0.49, 0.35, 0.35, 0.44, 0.42, 0.34, 0.43, 0.36, 0.36
                                        ,0.38, 0.38, 0.45, 0.36, 0.34, 0.46, 0.8, 1, 1.5, 1.4, 0.85     ## Has 11 on a row
                                        ,0.72, 0.44, 0.46, 0.44, 0.48, 0.51, 0.41, 0.48, 0.6, 0.4
                                        ,0.36, 0.3, 0.34, 0.36, 0.34, 0.39, 0.42, 0.39, 0.54, 0.43
                                        ,0.42, 0.3, 0.34, 0.35, 0.39, 0.37, 0.46, 0.54, 0.6, 0.59
                                        ,0.56, 0.56, 0.4, 0.46, 0.34, 0.83, 0.48, 0.48, 0.62, 0.36
                                        ,0.38, 0.65, 1, 0.54, 0.35, 0.8, 0.36, 0.49, 0.56, 0.84
                                        ,0.35, 0.15, 0.36, 0.5, 0.4, 0.42, 0.45, 0.9, 0.69, 0.55
                                        ,0.38, 0.76, 0.34, 0.5, 0.46, 0.49, 0.32, 0.35, 0.42, 0.5
                                        ,0.52, 0.45, 0.39, 0.34, 0.46, 0.56, 0.45, 0.48, 0.33, 0.44
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                                 ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                                 ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                                 ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                                 ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                                 ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                                 ,100, 101, 102, 103, 104, 105, 106, 107
                                                 ,108, 109, 110
                                                 ))
                              ,INCREMNT = rep(NA, times=111)
                              ,stackSlope = rep(0.78875, times=111)
                              ,stringsAsFactors=FALSE
                              )
                   ,data.frame(UID = rep('2004 WNVP99-REN1 1', times=151)
                              ,TRANSECT = c(rep('A', times=16)
                                           ,rep('B', times=15)
                                           ,rep('C', times=15)
                                           ,rep('D', times=15)
                                           ,rep('E', times=15)
                                           ,rep('F', times=15)
                                           ,rep('G', times=15)
                                           ,rep('H', times=15)
                                           ,rep('I', times=15)
                                           ,rep('J', times=15)
                                           )
                              ,STATION = c(0, 0:14, 0:14, 0:14, 0:14, 0:14
                                          ,0:14, 0:14, 0:14, 0:14, 0:14
                                          )
                              ,DEPTH = c(0, 0.01, 0.08, 0.05, 0.06, 0.05, 0.1, 0.07, 0.06, 0.03
                                        ,0.07, 0.08, 0.03, 0.04, 0, 0.11, 0.25, 0.18, NA, NA
                                        ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                        ,0.05, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                        ,NA, NA, NA, 0.01, 0.03, 0.03, 0.02, 0.03, 0.03, 0.03
                                        ,0.02, 0.02, 0.11, NA, NA, NA, NA, NA, NA, 0.02
                                        ,0.05, 0.02, 0.03, 0.03, NA, NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                        ,0.01, 0.02, 0.03, 0.08, 0.14, 0.1, 0.03, 0.16, 0.08, 0.04
                                        ,0.06, 0, NA, NA, 0, 0.01, 0, 0, 0, 0.15
                                        ,0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1
                                        ,0.01
                                        )
                              ,LOC = as.integer(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                                 ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                                 ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                                 ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                                 ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                                 ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                                 ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                                 ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                                 ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                                 ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                                 ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                                                 ,110, 111, 112, 113, 114, 115, 116, 117, 118, 119
                                                 ,120, 121, 122, 123, 124, 125, 126, 127, 128, 129
                                                 ,130, 131, 132, 133, 134, 135, 136, 137, 138, 139
                                                 ,140, 141, 142, 143, 144, 145, 146, 147, 148, 149
                                                 ,150
                                                 ))
                              ,INCREMNT = rep(1.5, times=151)
                              ,stackSlope = rep(1.30715, times=151)
                              ,stringsAsFactors=FALSE
                              )
                   )
  return(expected)
}

metsResidualPools.createExpectedReorgOLD <- function(fakeThal, fakeSlopes)
# Create expected results, converting stream depths in cm to m.
# The fakeThal argument should not contain any side-channel information.
{
  expected <- subset(fakeThal, PARAMETER %in% c('DEPTH','DEP_SONR','DEP_POLE'))
  expected <- expected[order(expected$UID, expected$TRANSECT, expected$STATION),]
  expected$DEPTH <- ifelse(expected$UID %in% c('2002 WWYP99-0672 1'
                                              ,'2002 WWYP99-NPR2 1'
                                              ,'2002 WMTP99-0601 1'
                                              )
                          ,as.numeric(expected$RESULT)
                          ,as.numeric(expected$RESULT) / 100
                          )

  expected$LOC <- c(c(1:41,43:103) #  "2000 WCAP99-0604 1"
                   ,193:1          #  "2002 WMTP99-0601 1"
                   ,200:1          #  "2002 WWYP99-0672 1"
                   ,200:1          #  "2002 WWYP99-NPR2 1"
                   ,1:100          #  "2004 EPA01-0450 1" (had side channels)
                   ,1:100          #  "2004 ORSE04-R022 1"
                   ,1:100          #  "2004 SHB-0315 1"
                   ,1:110          #  "2004 SHB-0395 1 missing incremnt"
                   ,1:110          #  "2004 SHB-0395 1"
                   ,1:150          #  "2004 WNVP99-REN1 1"
                   )
#  expected$INCREMNT <- as.numeric(subset(fakeThal, PARAMETER=='INCREMNT')$RESULT)
#  expected <- subset(expected, select=-c(PARAMETER,RESULT))
  expected <- merge(subset(expected, select=-c(PARAMETER,RESULT))
                   ,unique(rename(subset(fakeThal
                                        ,PARAMETER=='INCREMNT'
                                        ,select=c('UID','TRANSECT','RESULT')
                                        )
                                 ,'RESULT'
                                 ,'INCREMNT'
                                 )
                          )
                   ,c('UID','TRANSECT')
                   ,all=TRUE
                   )
  expected$INCREMNT <- as.numeric(expected$INCREMNT)
  
  expected <- merge(expected, fakeSlopes, by='UID', all.x=TRUE)
  expected$stackSlope <- 0.12 + 0.25 * as.numeric(expected$RESULT)
  expected <- subset(expected, select=-c(METRIC,RESULT))

  nicks <- subset(expected, LOC==1)
  nicks$LOC <- 0
  nicks <- merge(nicks
                ,merge(aggregate(list('xdep'=expected$DEPTH)
                                ,list('UID'=expected$UID)
                                ,mean, na.rm=TRUE
                                )
                      ,aggregate(list('vdep'=expected$DEPTH)
                                ,list('UID'=expected$UID)
                                ,sd, na.rm=TRUE
                                )
                      ,by='UID'
                      )
                ,by='UID', all.x=TRUE
                )
  nicks$DEPTH <- ifelse(nicks$xdep < nicks$vdep, 0, nicks$xdep - nicks$vdep)

  nicks <- subset(nicks, select=-c(xdep,vdep))
  expected <- rbind(expected, nicks)
  expected$LOC <- as.integer(expected$LOC)

#  expected <- subset(expected, TRANSECT %in% LETTERS) # Until we know how to handle side channels...
  expected <- expected[order(expected$UID, expected$LOC),]
  rownames(expected) <- NULL
  expected <- subset(expected, select=-c(SAMPLE_TYPE, UNITS))

  return(expected)
}


metsResidualPools.createInitialCalcs <- function(uids)
# Creates the expected residual dimension calculations for the
# metsResidualPools unit test.
# Fake data is taken from these WEMAP sites:
#   2004 ORSE04-R022 1 - normal wadeable reach
#   2004 SHB-0315 1    - wadeable reach with no slope information
#   2004 SHB-0395 1    - wadeable reach with 11 stations per transect.
#   2004 SHB-0395 1 missing incremnt - wadeable reach with no incremnt
#                        information, and also has 11 stations per transect.
#   2004 EPA01-0450  1 - wadeable reach with side channel transects
#   2004 WNVP99-REN1 1 - wadeable reach with too many missing values.
#   2002 WWYP99-0672 1 - normal boatable reach
#   2002 WWYP99-NPR2 1 - boatable reach with no slope information
#   2000 WCAP99-0604 1 - wadeable reach with varying stations per transect
#   2002 WMTP99-0601 1 - boatable reach with varying stations per transect
#
# The data for these dataframes is based on the SAS output file rparrays.sas7bdat,
# as written by rparraysR.sas.  The *boatable* reaches require additional changes
# as follows:
#   Transects need to be un-reversed so they run J-A as LOC increases.
#   Stations need to be un-reversed so they run 19-0 as LOC increases.
#   There is an extra row inserted at the second position (LOC 0) which is
#     spurious; the last value of LOC (prior to un-reversal) should be removed
#     ,as well as the last TRANSECT value (K) and STATION (0), and the second
#     value of all other columns must be removed (NA, except for WT_WID which
#     is 51 & 59 for the two boatable reaches).
#
# SAS code for formatting the contents of rparrays for a site is:
# %let sitelen=195;
# data _null_; set rparrays;
# *  array ary[*] trans1-trans&sitelen;
# *  array ary[*] sta1-sta&sitelen;
# *  array ary[*] dind1-dind&sitelen;
# *  array ary[*] incr1-incr&sitelen;
# *  array ary[*] dep1-dep&sitelen;
# *  array ary[*] wwid1-wwid&sitelen;
# *  array ary[*] wwid1-wwid&sitelen;
# *  array ary[*] resd1-resd&sitelen;
#   array ary[*] rlen1-rlen&sitelen;
# *  array ary[*] resa1-resa&sitelen;
# *  array ary[*] chlen1-chlen&sitelen;
# *  array ary[*] chlen1-chlen&sitelen;
# *  array ary[*] pid1-pid&sitelen;
#   do i=1 to &sitelen;
#       lineOffset = &sitelen - i + 1;
#       if int((i-1)/10) = (i-1)/10 and i>1  then do;
#           put ; put '                                  ,' @;
#       end;
#       else do;
#           put ', ' @;
#       end;
#       put ary[i] +(-1)  @;
#    end;
# run;
#
# Because of the presence of side channels in 2004 EPA01-0450 1, the nickpoint
# for this reach is changed from 32.407776425567 to 38.1929831472502.  This
# changed the resDepth at LOC=1,2 from (22.592223574433, 0.0274235744328)
# to (16.8070168527498, 0), the resArea at LOC 2 from 0.0008775543818 to 0, the
# resLength at LOC 2 from 3.2 to 0, and the poolID at LOC 2 from 1 to 0.
#
# ARGUMENTS:
# uids      character vector of UIDs.  The returned dataframe will contain
#           values for the intersection of the specified UIDs and the available
#           UIDs.
{
  expected <- rbind(data.frame('UID'='2004 ORSE04-R022 1'
                        ,'TRANSECT'=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                     ,'A', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                     ,'B', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                     ,'C', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                     ,'D', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                     ,'E', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                     ,'F', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                     ,'G', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                     ,'H', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                     ,'I', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                     ,'J'
                                     )
                        ,'STATION'=c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                    ,9
                                    )
                        ,'LOC'=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                                ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                                ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                                ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                                ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                                ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                                ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                                ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                                ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                                ,100
                                )
                        ,'INCREMNT'=c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                     ,1.5
                                     )
                        ,'DEPTH'=c(13.652217660186, 31, 31, 22, 23, 24, 20, 32, 21, 20
                                  ,18, 61, 8, 16, 23, 27, 20, 14, 20, 32
                                  ,22, 32, 20, 18, 18, 6, 28, 24, 18, 14
                                  ,18, 42, 25, 16, 20, 26, 34, 30, 19, 14
                                  ,14, 14, 24, 42, 37, 25, 32, 46, 36, 36
                                  ,40, 47, 14, 17, 20, 20, 19, 15, 19, 14
                                  ,18, 17, 10, 8, 10, 12, 12, 12, 11, 17
                                  ,13, 40, 43, 69, 66, 78, 67, 71, 51, 50
                                  ,44, 43, 40, 41, 40, 25, 30, 29, 32, 35
                                  ,36, 30, 35, 32, 41, 39, 43, 50, 31, 26
                                  ,26
                                  )
                        ,'WT_WID'=c(NA, 1.7, 1.96, 2.22, 2.48, 2.74, 3, 3.04, 3.08, 3.12
                                   ,3.16, 3.2, 3.16, 3.12, 3.08, 3.04, 3, 3.16, 3.32, 3.48
                                   ,3.64, 3.8, 4.24, 4.68, 5.12, 5.56, 6, 5.44, 4.88, 4.32
                                   ,3.76, 3.2, 2.76, 2.32, 1.88, 1.44, 1, 1.6, 2.2, 2.8
                                   ,3.4, 4, 3.6, 3.2, 2.8, 2.4, 2, 2.32, 2.64, 2.96
                                   ,3.28, 3.6, 3.68, 3.76, 3.84, 3.92, 4, 4, 4, 4
                                   ,4, 4, 3.8, 3.6, 3.4, 3.2, 3, 3.68, 4.36, 5.04
                                   ,5.72, 6.4, 9.72, 13.04, 16.36, 19.68, 23, 20.8, 18.6, 16.4
                                   ,14.2, 12, 10.4, 8.8, 7.2, 5.6, 4, 4, 4, 4
                                   ,4, 4, 4.2, 4.4, 4.6, 4.8, 5, NA, NA, NA
                                   ,NA
                                   )
                        ,'stackSlope'= 0.12 + 0.25*3.9
                        ,'resDepth'=c(NA, 17.347782339814, 15.705282339814, 5.062782339814, 4.420282339814, 3.777782339814, 0, 10.3575, 0, 0
                                     ,0, 41.3575, 0, 6.3575, 11.715, 14.0725, 5.43, 0, 4.3575, 14.715
                                     ,3.0725, 11.43, 0, 0, 0, 0, 20.3575, 14.715, 7.0725, 1.43
                                     ,3.7875, 26.145, 7.5025, 0, 2.3575, 6.715, 13.0725, 7.43, 0, 0
                                     ,0, 0, 8.3575, 24.715, 18.0725, 4.43, 9.7875, 22.145, 10.5025, 8.86
                                     ,11.2175, 16.575, 0, 1.3575, 2.715, 1.0725, 0, 0, 2.3575, 0
                                     ,2.3575, 0, 0, 0, 0.3575, 0.715, 0, 0, 0, 4.3575
                                     ,0, 25.3575, 26.715, 51.0725, 46.43, 56.7875, 44.145, 46.5025, 24.86, 22.2175
                                     ,14.575, 11.9325, 7.29, 6.6475, 4.005, 0, 3.3575, 0.715, 2.0725, 3.43
                                     ,2.7875, 0, 3.3575, 0, 7.3575, 3.715, 6.0725, 11.43, 0, 0
                                     ,0
                                     )
                        ,'resArea'=c(NA, 0, 0.2355792350972, 0.0759417350972, 0.0663042350972, 0.0566667350972, 0, 0.1553625, 0, 0
                                    ,0, 0.6203625, 0, 0.0953625, 0.175725, 0.2110875, 0.08145, 0, 0.0653625, 0.220725
                                    ,0.0460875, 0.17145, 0, 0, 0, 0, 0.3053625, 0.220725, 0.1060875, 0.02145
                                    ,0.0568125, 0.392175, 0.1125375, 0, 0.0353625, 0.100725, 0.1960875, 0.11145, 0, 0
                                    ,0, 0, 0.1253625, 0.370725, 0.2710875, 0.06645, 0.1468125, 0.332175, 0.1575375, 0.1329
                                    ,0.1682625, 0.248625, 0, 0.0203625, 0.040725, 0.0160875, 0, 0, 0.0353625, 0
                                    ,0.0353625, 0, 0, 0, 0.0053625, 0.010725, 0, 0, 0, 0.0653625
                                    ,0, 0.3803625, 0.400725, 0.7660875, 0.69645, 0.8518125, 0.662175, 0.6975375, 0.3729, 0.3332625
                                    ,0.218625, 0.1789875, 0.10935, 0.0997125, 0.060075, 0, 0.0503625, 0.010725, 0.0310875, 0.05145
                                    ,0.0418125, 0, 0.0503625, 0, 0.1103625, 0.055725, 0.0910875, 0.17145, 0, 0
                                    ,0
                                    )
                        ,'resLength'=c(NA, 1.5, 1.5, 1.5, 1.5, 1.5, 0, 1.5, 0, 0
                                      ,0, 1.5, 0, 1.5, 1.5, 1.5, 1.5, 0, 1.5, 1.5
                                      ,1.5, 1.5, 0, 0, 0, 0, 1.5, 1.5, 1.5, 1.5
                                      ,1.5, 1.5, 1.5, 0, 1.5, 1.5, 1.5, 1.5, 0, 0
                                      ,0, 0, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                      ,1.5, 1.5, 0, 1.5, 1.5, 1.5, 0, 0, 1.5, 0
                                      ,1.5, 0, 0, 0, 1.5, 1.5, 0, 0, 0, 1.5
                                      ,0, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                      ,1.5, 1.5, 1.5, 1.5, 1.5, 0, 1.5, 1.5, 1.5, 1.5
                                      ,1.5, 0, 1.5, 0, 1.5, 1.5, 1.5, 1.5, 0, 0
                                      ,0
                                      )
                        ,'poolID'=c(NA, 1, 1, 1, 1, 1, 0, 2, 0, 0
                                   ,0, 3, 0, 4, 4, 4, 4, 0, 5, 5
                                   ,5, 5, 0, 0, 0, 0, 6, 6, 6, 6
                                   ,6, 6, 6, 0, 7, 7, 7, 7, 0, 0
                                   ,0, 0, 8, 8, 8, 8, 8, 8, 8, 8
                                   ,8, 8, 0, 9, 9, 9, 0, 0, 10, 0
                                   ,11, 0, 0, 0, 12, 12, 0, 0, 0, 13
                                   ,0, 14, 14, 14, 14, 14, 14, 14, 14, 14
                                   ,14, 14, 14, 14, 14, 0, 15, 15, 15, 15
                                   ,15, 0, 16, 0, 17, 17, 17, 17, 0, 0
                                   ,0
                                   )
                        ,stringsAsFactors=FALSE
                        )
                   ,data.frame('UID'='2004 SHB-0315 1'
                        ,'TRANSECT'=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                     ,'A', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                     ,'B', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                     ,'C', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                     ,'D', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                     ,'E', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                     ,'F', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                     ,'G', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                     ,'H', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                     ,'I', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                     ,'J'
                                     )
                       ,'STATION'=c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9
                                   )
                       ,'LOC'=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                               ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                               ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                               ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                               ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                               ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                               ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                               ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                               ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                               ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                               ,100
                               )
                       ,'INCREMNT'=c(3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2
                                    )
                       ,'DEPTH'=c(22.761885508232, 58, 71, 61, 45, 20, 22, 25, 32, 25
                                 ,20, 41, 55, 41, 36, 46, 40, 42, 97, 47
                                 ,70, 60, 30, 50, 55, 22, 41, 54, 67, 85
                                 ,87, 71, 87, 72, 65, 75, 61, 96, 66, 67
                                 ,23, 20, 20, 16, 16, 20, 21, 20, 19, 25
                                 ,27, 31, 34, 65, 93, 86, 103, 110, 105, 110
                                 ,85, 27, 25, 60, 43, 51, 76, 85, 85, 78
                                 ,95, 59, 30, 30, 29, 42, 25, 35, 36, 33
                                 ,32, 22, 18, 15, 20, 20, 20, 28, 22, NA
                                 ,73, 85, 100, 120, 140, 110, 70, 57, 35, 61
                                 ,25
                                 )
                       ,'WT_WID'=c(NA, 6, NA, NA, NA, NA, 14.5, NA, NA, NA
                                  ,NA, 15.6, NA, NA, NA, NA, 20.1, NA, NA, NA
                                  ,NA, 15.7, NA, NA, NA, NA, 9.2, NA, NA, NA
                                  ,NA, 8.3, NA, NA, NA, NA, 7.5, NA, NA, NA
                                  ,NA, 4.3, NA, NA, NA, NA, 10.7, NA, NA, NA
                                  ,NA, 9.1, NA, NA, NA, NA, 5.2, NA, NA, NA
                                  ,NA, 9.9, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 12.1, NA, NA, NA, NA, 8.3, NA, NA, NA
                                  ,NA, 13.2, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 6.9, NA, NA, NA, NA, 6.1, NA, NA, NA
                                  ,NA
                                  )
                       ,'stackSlope'=NA
                       ,'resDepth'=NA
                       ,'resArea'=NA
                       ,'resLength'=NA
                       ,'poolID'=NA
                       ,stringsAsFactors=FALSE
                       )
                   ,data.frame('UID'='2004 SHB-0395 1'
                       ,'TRANSECT'=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                    ,'A', 'A', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                    ,'B', 'B', 'B', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                    ,'C', 'C', 'C', 'C', 'D', 'D', 'D', 'D', 'D', 'D'
                                    ,'D', 'D', 'D', 'D', 'D', 'E', 'E', 'E', 'E', 'E'
                                    ,'E', 'E', 'E', 'E', 'E', 'E', 'F', 'F', 'F', 'F'
                                    ,'F', 'F', 'F', 'F', 'F', 'F', 'F', 'G', 'G', 'G'
                                    ,'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'H', 'H'
                                    ,'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'I'
                                    ,'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                    ,'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                    ,'J'
                                    )
                       ,'STATION'=c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 10, 0, 1, 2, 3, 4, 5, 6, 7
                                   ,8, 9, 10, 0, 1, 2, 3, 4, 5, 6
                                   ,7, 8, 9, 10, 0, 1, 2, 3, 4, 5
                                   ,6, 7, 8, 9, 10, 0, 1, 2, 3, 4
                                   ,5, 6, 7, 8, 9, 10, 0, 1, 2, 3
                                   ,4, 5, 6, 7, 8, 9, 10, 0, 1, 2
                                   ,3, 4, 5, 6, 7, 8, 9, 10, 0, 1
                                   ,2, 3, 4, 5, 6, 7, 8, 9, 10, 0
                                   ,1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                   ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                   ,10
                                   )
                       ,'LOC'=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                               ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                               ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                               ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                               ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                               ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                               ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                               ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                               ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                               ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                               ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                               ,110
                               )
                       ,'INCREMNT'=c(4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                    ,4.7272727272727
                                    )
                       ,'DEPTH'=c(27.716044009952, 17, 25, 25, 30, 39, 40, 50, 55, 56
                                 ,24, 49, 35, 35, 44, 42, 34, 43, 36, 36
                                 ,38, 38, 45, 36, 34, 46, 80, 100, 150, 140
                                 ,85, 72, 44, 46, 44, 48, 51, 41, 48, 60
                                 ,40, 36, 30, 34, 36, 34, 39, 42, 39, 54
                                 ,43, 42, 30, 34, 35, 39, 37, 46, 54, 60
                                 ,59, 56, 56, 40, 46, 34, 83, 48, 48, 62
                                 ,36, 38, 65, 100, 54, 35, 80, 36, 49, 56
                                 ,84, 35, 15, 36, 50, 40, 42, 45, 90, 69
                                 ,55, 38, 76, 34, 50, 46, 49, 32, 35, 42
                                 ,50, 52, 45, 39, 34, 46, 56, 45, 48, 33
                                 ,44
                                 )
                       ,'WT_WID'=c(NA, 19.4, 17.72, 16.04, 14.36, 12.68, 11, 11.266666666667, 11.533333333333, 11.8
                                  ,12.066666666667, 12.333333333333, 12.6, 11.76, 10.92, 10.08, 9.24, 8.4, 9.6, 10.8
                                  ,12, 13.2, 14.4, 15.6, 14.06, 12.52, 10.98, 9.44, 7.9, 8.6833333333333
                                  ,9.4666666666667, 10.25, 11.033333333333, 11.816666666667, 12.6, 12.08, 11.56, 11.04, 10.52, 10
                                  ,10.4, 10.8, 11.2, 11.6, 12, 12.4, 11.62, 10.84, 10.06, 9.28
                                  ,8.5, 8.7166666666667, 8.9333333333333, 9.15, 9.3666666666667, 9.5833333333333, 9.8, 9.08, 8.36, 7.64
                                  ,6.92, 6.2, 6.35, 6.5, 6.65, 6.8, 6.95, 7.1, 8.5, 9.9
                                  ,11.3, 12.7, 14.1, 12.816666666667, 11.533333333333, 10.25, 8.9666666666667, 7.6833333333333, 6.4, 7.12
                                  ,7.84, 8.56, 9.28, 10, 9.85, 9.7, 9.55, 9.4, 9.25, 9.1
                                  ,9.06, 9.02, 8.98, 8.94, 8.9, 8.9333333333333, 8.9666666666667, 9, 9.0333333333333, 9.0666666666667
                                  ,9.1, 9.1, 9.1, 9.1, 9.1, 9.1, NA, NA, NA, NA
                                  ,NA
                                  )
                       ,'stackSlope'=0.12 + 0.25*2.675
                       ,'resDepth'=c(NA, 0, 4.2713636363636, 0.5427272727273, 1.8140909090909, 7.0854545454545, 4.3568181818182, 10.628181818182, 11.899545454545, 9.1709090909091
                                    ,0, 21.271363636364, 3.5427272727273, 0, 5.2713636363636, 0, 0, 5.2713636363636, 0, 0
                                    ,0, 0, 3.2713636363636, 0, 0, 8.2713636363636, 38.542727272727, 54.814090909091, 101.08545454545, 87.356818181818
                                    ,28.628181818182, 11.899545454545, 0, 0, 0, 0.2713636363636, 0, 0, 3.2713636363636, 11.542727272727
                                    ,0, 0, 0, 0.2713636363636, 0, 0, 1.2713636363636, 0.5427272727273, 0, 11.271363636364
                                    ,0, 0, 0, 0.2713636363636, 0, 0.2713636363636, 0, 5.2713636363636, 9.5427272727273, 11.814090909091
                                    ,7.0854545454545, 0.3568181818182, 0, 0, 2.2713636363636, 0, 45.271363636364, 6.5427272727273, 2.8140909090909, 13.085454545455
                                    ,0, 0, 23.271363636364, 54.542727272727, 4.8140909090909, 0, 41.271363636364, 0, 9.2713636363636, 12.542727272727
                                    ,36.814090909091, 0, 0, 17.271363636364, 27.542727272727, 13.814090909091, 12.085454545455, 11.356818181818, 52.628181818182, 27.899545454545
                                    ,10.170909090909, 0, 34.271363636364, 0, 12.271363636364, 4.5427272727273, 3.8140909090909, 0, 0, 3.2713636363636
                                    ,7.5427272727273, 5.8140909090909, 0, 0, 0, 8.2713636363636, 14.542727272727, 0, 0, 0
                                    ,7.2713636363636
                                    )
                       ,'resArea'=c(NA, 0, 0.2019190082645, 0.0256561983471, 0.0857570247934, 0.3349487603306, 0.205958677686, 0.5024231404959, 0.5625239669421, 0.4335338842975
                                   ,0, 1.0055553719008, 0.1674743801653, 0, 0.2491917355372, 0, 0, 0.2491917355372, 0, 0
                                   ,0, 0, 0.1546462809917, 0, 0, 0.3910099173554, 1.8220198347107, 2.5912115702479, 4.7785851239669, 4.1295950413223
                                   ,1.353332231405, 0.5625239669421, 0, 0, 0, 0.0128280991736, 0, 0, 0.1546462809917, 0.5456561983471
                                   ,0, 0, 0, 0.0128280991736, 0, 0, 0.0601008264463, 0.0256561983471, 0, 0.5328280991736
                                   ,0, 0, 0, 0.0128280991736, 0, 0.0128280991736, 0, 0.2491917355372, 0.4511107438017, 0.5584842975207
                                   ,0.3349487603306, 0.016867768595, 0, 0, 0.107373553719, 0, 2.1401008264463, 0.3092925619835, 0.1330297520661, 0.6185851239669
                                   ,0, 0, 1.1001008264463, 2.5783834710744, 0.2275752066116, 0, 1.9510099173554, 0, 0.4382826446281, 0.5929289256198
                                   ,1.7403024793388, 0, 0, 0.8164644628099, 1.3020198347107, 0.6530297520661, 0.5713123966942, 0.536867768595, 2.4878776859504, 1.3188876033058
                                   ,0.4808066115702, 0, 1.6201008264463, 0, 0.5801008264463, 0.214747107438, 0.1803024793388, 0, 0, 0.1546462809917
                                   ,0.3565652892562, 0.2748479338843, 0, 0, 0, 0.3910099173554, 0.6874743801653, 0, 0, 0
                                   ,0.3437371900826
                                   )
                       ,'resLength'=c(NA, 0, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                     ,0, 4.7272727272727, 4.7272727272727, 0, 4.7272727272727, 0, 0, 4.7272727272727, 0, 0
                                     ,0, 0, 4.7272727272727, 0, 0, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                     ,4.7272727272727, 4.7272727272727, 0, 0, 0, 4.7272727272727, 0, 0, 4.7272727272727, 4.7272727272727
                                     ,0, 0, 0, 4.7272727272727, 0, 0, 4.7272727272727, 4.7272727272727, 0, 4.7272727272727
                                     ,0, 0, 0, 4.7272727272727, 0, 4.7272727272727, 0, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                     ,4.7272727272727, 4.7272727272727, 0, 0, 4.7272727272727, 0, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                     ,0, 0, 4.7272727272727, 4.7272727272727, 4.7272727272727, 0, 4.7272727272727, 0, 4.7272727272727, 4.7272727272727
                                     ,4.7272727272727, 0, 0, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727, 4.7272727272727
                                     ,4.7272727272727, 0, 4.7272727272727, 0, 4.7272727272727, 4.7272727272727, 4.7272727272727, 0, 0, 4.7272727272727
                                     ,4.7272727272727, 4.7272727272727, 0, 0, 0, 4.7272727272727, 4.7272727272727, 0, 0, 0
                                     ,4.7272727272727
                                     )
                       ,'poolID'=c(NA, 0, 1, 1, 1, 1, 1, 1, 1, 1
                                  ,0, 2, 2, 0, 3, 0, 0, 4, 0, 0
                                  ,0, 0, 5, 0, 0, 6, 6, 6, 6, 6
                                  ,6, 6, 0, 0, 0, 7, 0, 0, 8, 8
                                  ,0, 0, 0, 9, 0, 0, 10, 10, 0, 11
                                  ,0, 0, 0, 12, 0, 13, 0, 14, 14, 14
                                  ,14, 14, 0, 0, 15, 0, 16, 16, 16, 16
                                  ,0, 0, 17, 17, 17, 0, 18, 0, 19, 19
                                  ,19, 0, 0, 20, 20, 20, 20, 20, 20, 20
                                  ,20, 0, 21, 0, 22, 22, 22, 0, 0, 23
                                  ,23, 23, 0, 0, 0, 24, 24, 0, 0, 0
                                  ,25
                                  )
                       ,stringsAsFactors=FALSE
                       )
                   ,data.frame('UID'='2004 SHB-0395 1 missing incremnt'
                       ,'TRANSECT'=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                    ,'A', 'A', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                    ,'B', 'B', 'B', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                    ,'C', 'C', 'C', 'C', 'D', 'D', 'D', 'D', 'D', 'D'
                                    ,'D', 'D', 'D', 'D', 'D', 'E', 'E', 'E', 'E', 'E'
                                    ,'E', 'E', 'E', 'E', 'E', 'E', 'F', 'F', 'F', 'F'
                                    ,'F', 'F', 'F', 'F', 'F', 'F', 'F', 'G', 'G', 'G'
                                    ,'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'H', 'H'
                                    ,'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'I'
                                    ,'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                    ,'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                    ,'J'
                                    )
                       ,'STATION'=c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 10, 0, 1, 2, 3, 4, 5, 6, 7
                                   ,8, 9, 10, 0, 1, 2, 3, 4, 5, 6
                                   ,7, 8, 9, 10, 0, 1, 2, 3, 4, 5
                                   ,6, 7, 8, 9, 10, 0, 1, 2, 3, 4
                                   ,5, 6, 7, 8, 9, 10, 0, 1, 2, 3
                                   ,4, 5, 6, 7, 8, 9, 10, 0, 1, 2
                                   ,3, 4, 5, 6, 7, 8, 9, 10, 0, 1
                                   ,2, 3, 4, 5, 6, 7, 8, 9, 10, 0
                                   ,1, 2, 3, 4, 5, 6, 7, 8, 9, 10
                                   ,0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                                   ,10
                                   )
                       ,'LOC'=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                               ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                               ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                               ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                               ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                               ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                               ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                               ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                               ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                               ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                               ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                               ,110
                               )
                       ,'INCREMNT'=NA
                       ,'DEPTH'=c(27.716044009952, 17, 25, 25, 30, 39, 40, 50, 55, 56
                                 ,24, 49, 35, 35, 44, 42, 34, 43, 36, 36
                                 ,38, 38, 45, 36, 34, 46, 80, 100, 150, 140
                                 ,85, 72, 44, 46, 44, 48, 51, 41, 48, 60
                                 ,40, 36, 30, 34, 36, 34, 39, 42, 39, 54
                                 ,43, 42, 30, 34, 35, 39, 37, 46, 54, 60
                                 ,59, 56, 56, 40, 46, 34, 83, 48, 48, 62
                                 ,36, 38, 65, 100, 54, 35, 80, 36, 49, 56
                                 ,84, 35, 15, 36, 50, 40, 42, 45, 90, 69
                                 ,55, 38, 76, 34, 50, 46, 49, 32, 35, 42
                                 ,50, 52, 45, 39, 34, 46, 56, 45, 48, 33
                                 ,44
                                 )
                       ,'WT_WID'=NA
                       ,'stackSlope'=0.12 + 0.25*2.675
                       ,'resDepth'=NA
                       ,'resArea'=NA
                       ,'resLength'=NA
                       ,'poolID'=NA
                       ,stringsAsFactors=FALSE
                       )
                   ,data.frame('UID'='2004 EPA01-0450 1'
                       ,'TRANSECT'=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                    ,'A', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                    ,'B', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                    ,'C', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                    ,'D', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                    ,'E', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                    ,'F', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                    ,'G', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                    ,'H', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                    ,'I', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                    ,'J', 'XA', 'XA', 'XA', 'XA', 'XA', 'XI', 'XI', 'XI', 'XI'
                                    ,'XI', 'XI', 'XI', 'XI', 'XI', 'XI', 'XJ', 'XJ', 'XJ', 'XJ'
                                    ,'XJ', 'XJ', 'XJ', 'XJ', 'XJ', 'XJ'
                                    )
                       ,'STATION'=c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 0, 1, 2, 3, 4, 0, 1, 2, 3
                                   ,4, 5, 6, 7, 8, 9, 0, 1, 2, 3
                                   ,4, 5, 6, 7, 8, 9
                                   )
                       ,'LOC'=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                               ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                               ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                               ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                               ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                               ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                               ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                               ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                               ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                               ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                               ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                               ,110, 111, 112, 113, 114, 115, 116, 117, 118, 119
                               ,120, 121, 122, 123, 124, 125
                              )
                       ,'INCREMNT'=c(3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    ,3.2, 3.2, 3.2, 3.2, 3.2, 3.2
                                    )
                       ,'DEPTH'=c(38.1929831472502, 55, 38, 33, 52, 56, 60, 50, 51, 43
                                 ,41, 42, 56, 50, 49, 55, 64, 60, 75, 50
                                 ,49, 68, 37, 77, 95, 80, 51, 45, 42, 36
                                 ,40, 40, 41, 38, 66, 46, 45, 35, 41, 44
                                 ,74, 55, 62, 65, 43, 64, 67, 54, 32, 34
                                 ,46, 76, 58, 50, 64, 66, 49, 28, 35, 41
                                 ,66, 70, 62, 58, 30, 28, 26, 82, 40, 49
                                 ,57, 63, 76, 39, 50, 44, 42, 36, 94, 47
                                 ,49, 87, 63, 45, 52, 49, 85, 35, 47, 39
                                 ,36, 55, 60, 62, 66, 63, 74, 54, 54, 52
                                 ,64, 30, 67, 60, 26, 29, 59, 44, 17, 19
                                 ,31, 30, 26, 19, 26, 20, 24, 25, 50, 35
                                 ,56, 25, 29, 50, 21, 21
                                 )
                       ,'WT_WID'=c(NA, 7.4, 7.68, 7.96, 8.24, 8.52, 8.8, 9.06, 9.32, 9.58
                                  ,9.84, 10.1, 10.16, 10.22, 10.28, 10.34, 10.4, 12.58, 14.76, 16.94
                                  ,19.12, 21.3, 18.28, 15.26, 12.24, 9.22, 6.2, 6.32, 6.44, 6.56
                                  ,6.68, 6.8, 6.66, 6.52, 6.38, 6.24, 6.1, 6.52, 6.94, 7.36
                                  ,7.78, 8.2, 8.42, 8.64, 8.86, 9.08, 9.3, 9.12, 8.94, 8.76
                                  ,8.58, 8.4, 9.74, 11.08, 12.42, 13.76, 15.1, 15.42, 15.74, 16.06
                                  ,16.38, 16.7, 17.6, 18.5, 19.4, 20.3, 21.2, 19.96, 18.72, 17.48
                                  ,16.24, 15, 13.96, 12.92, 11.88, 10.84, 9.8, 10.1, 10.4, 10.7
                                  ,11, 11.3, 10.22, 9.14, 8.06, 6.98, 5.9, 6.28, 6.66, 7.04
                                  ,7.42, 7.8, 8.32, 8.84, 9.36, 9.88, 10.4, NA, NA, NA
                                  ,NA, 4.4, NA, NA, NA, NA, 3, NA, NA, NA
                                  ,NA, 5.3, NA, NA, NA, NA, 4.2, NA, NA, NA
                                  ,NA, 2.2, NA, NA, NA, NA
                                  )
                       ,'stackSlope'= 0.12 + 0.25*6.476
                       ,'resDepth'=c(NA, 16.8070168527498, 0, 0, 13.4352, 11.8704, 10.3056, 0, 0, 0
                                    ,0, 0, 8.4352, 0, 0, 0.4352, 3.8704, 0, 9.4352, 0
                                    ,0, 13.4352, 0, 34.4352, 46.8704, 26.3056, 0, 0, 0, 0
                                    ,0, 0, 0, 0, 22.4352, 0, 0, 0, 0.4352, 0
                                    ,24.4352, 0, 1.4352, 0, 0, 15.4352, 12.8704, 0, 0, 0
                                    ,6.4352, 30.8704, 7.3056, 0, 8.4352, 4.8704, 0, 0, 1.4352, 1.8704
                                    ,21.3056, 19.7408, 6.176, 0, 0, 0, 0, 50.4352, 2.8704, 6.3056
                                    ,8.7408, 9.176, 16.6112, 0, 5.4352, 0, 0, 0, 52.4352, 0
                                    ,0, 32.4352, 2.8704, 0, 1.4352, 0, 30.4352, 0, 6.4352, 0
                                    ,0, 13.4352, 12.8704, 9.3056, 7.7408, 0, 5.4352, 0, 0, 0
                                    ,6.4352, NA, NA, NA, NA, NA, 21.027423574433, 0.4626235744328, 0, 0
                                    ,6.4352, 0, 0, 0, 1.4352, 0, 0, 0, 19.4352, 0
                                    ,15.4352, 0, 0, 15.4352, 0, 0
                                    )
                       ,'resArea'=c(NA, 0, 0, 0, 0.4299264, 0.3798528, 0.3297792, 0, 0, 0
                                   ,0, 0, 0.2699264, 0, 0, 0.0139264, 0.1238528, 0, 0.3019264, 0
                                   ,0, 0.4299264, 0, 1.1019264, 1.4998528, 0.8417792, 0, 0, 0, 0
                                   ,0, 0, 0, 0, 0.7179264, 0, 0, 0, 0.0139264, 0
                                   ,0.7819264, 0, 0.0459264, 0, 0, 0.4939264, 0.4118528, 0, 0, 0
                                   ,0.2059264, 0.9878528, 0.2337792, 0, 0.2699264, 0.1558528, 0, 0, 0.0459264, 0.0598528
                                   ,0.6817792, 0.6317056, 0.197632, 0, 0, 0, 0, 1.6139264, 0.0918528, 0.2017792
                                   ,0.2797056, 0.293632, 0.5315584, 0, 0.1739264, 0, 0, 0, 1.6779264, 0
                                   ,0, 1.0379264, 0.0918528, 0, 0.0459264, 0, 0.9739264, 0, 0.2059264, 0
                                   ,0, 0.4299264, 0.4118528, 0.2977792, 0.2477056, 0, 0.1739264, 0, 0, 0
                                   ,0.2059264, NA, NA, NA, NA, NA, 0.6728775543818, 0.0148039543818, 0, 0
                                   ,0.2059264, 0, 0, 0, 0.0459264, 0, 0, 0, 0.6219264, 0
                                   ,0.4939264, 0, 0, 0.4939264, 0, 0
                                  )
                       ,'resLength'=c(NA, 3.2, 0, 0, 3.2, 3.2, 3.2, 0, 0, 0
                                     ,0, 0, 3.2, 0, 0, 3.2, 3.2, 0, 3.2, 0
                                     ,0, 3.2, 0, 3.2, 3.2, 3.2, 0, 0, 0, 0
                                     ,0, 0, 0, 0, 3.2, 0, 0, 0, 3.2, 0
                                     ,3.2, 0, 3.2, 0, 0, 3.2, 3.2, 0, 0, 0
                                     ,3.2, 3.2, 3.2, 0, 3.2, 3.2, 0, 0, 3.2, 3.2
                                     ,3.2, 3.2, 3.2, 0, 0, 0, 0, 3.2, 3.2, 3.2
                                     ,3.2, 3.2, 3.2, 0, 3.2, 0, 0, 0, 3.2, 0
                                     ,0, 3.2, 3.2, 0, 3.2, 0, 3.2, 0, 3.2, 0
                                     ,0, 3.2, 3.2, 3.2, 3.2, 0, 3.2, 0, 0, 0
                                     ,3.2, NA, 3.2, 3.2, 3.2, 3.2, 3.2, 3.2, 0, 0
                                     ,3.2, 0, 0, 0, 3.2, 0, 0, 0, 3.2, 0
                                     ,3.2, 0, 0, 3.2, 0, 0
                                     )
                       ,'poolID'=c(NA, 1, 0, 0, 2, 2, 2, 0, 0, 0
                                  ,0, 0, 3, 0, 0, 4, 4, 0, 5, 0
                                  ,0, 6, 0, 7, 7, 7, 0, 0, 0, 0
                                  ,0, 0, 0, 0, 8, 0, 0, 0, 9, 0
                                  ,10, 0, 11, 0, 0, 12, 12, 0, 0, 0
                                  ,13, 13, 13, 0, 14, 14, 0, 0, 15, 15
                                  ,15, 15, 15, 0, 0, 0, 0, 16, 16, 16
                                  ,16, 16, 16, 0, 17, 0, 0, 0, 18, 0
                                  ,0, 19, 19, 0, 20, 0, 21, 0, 22, 0
                                  ,0, 23, 23, 23, 23, 0, 24, 0, 0, 0
                                  ,25, 26, 26, 26, 26, 26, 27, 27, 0, 0
                                  ,28, 0, 0, 0, 29, 0, 0, 0, 30, 0
                                  ,31, 0, 0, 32, 0, 0
                                  )
                       ,stringsAsFactors=FALSE
                       )
                   ,data.frame('UID'='2004 WNVP99-REN1 1'
                       ,'TRANSECT'=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                    ,'A', 'A', 'A', 'A', 'A', 'A', 'B', 'B', 'B', 'B'
                                    ,'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                    ,'B', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                    ,'C', 'C', 'C', 'C', 'C', 'C', 'D', 'D', 'D', 'D'
                                    ,'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                    ,'D', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                    ,'E', 'E', 'E', 'E', 'E', 'E', 'F', 'F', 'F', 'F'
                                    ,'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                    ,'F', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                    ,'G', 'G', 'G', 'G', 'G', 'G', 'H', 'H', 'H', 'H'
                                    ,'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                    ,'H', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                    ,'I', 'I', 'I', 'I', 'I', 'I', 'J', 'J', 'J', 'J'
                                    ,'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                    ,'J'
                                    )
                       ,'STATION'=c(0, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 10, 11, 12, 13, 14, 0, 1, 2, 3
                                   ,4, 5, 6, 7, 8, 9, 10, 11, 12, 13
                                   ,14, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 10, 11, 12, 13, 14, 0, 1, 2, 3
                                   ,4, 5, 6, 7, 8, 9, 10, 11, 12, 13
                                   ,14, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 10, 11, 12, 13, 14, 0, 1, 2, 3
                                   ,4, 5, 6, 7, 8, 9, 10, 11, 12, 13
                                   ,14, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 10, 11, 12, 13, 14, 0, 1, 2, 3
                                   ,4, 5, 6, 7, 8, 9, 10, 11, 12, 13
                                   ,14, 0, 1, 2, 3, 4, 5, 6, 7, 8
                                   ,9, 10, 11, 12, 13, 14, 0, 1, 2, 3
                                   ,4, 5, 6, 7, 8, 9, 10, 11, 12, 13
                                   ,14
                                   )
                       ,'LOC'=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                               ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                               ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                               ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                               ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                               ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                               ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                               ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                               ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                               ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                               ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                               ,110, 111, 112, 113, 114, 115, 116, 117, 118, 119
                               ,120, 121, 122, 123, 124, 125, 126, 127, 128, 129
                               ,130, 131, 132, 133, 134, 135, 136, 137, 138, 139
                               ,140, 141, 142, 143, 144, 145, 146, 147, 148, 149
                               ,150
                               )
                       ,'INCREMNT'=c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                    ,1.5
                                    )
                       ,'DEPTH'=c(0, 1, 8, 5, 6, 5, 10, 7, 6, 3
                                 ,7, 8, 3, 4, 0, 11, 25, 18, NA, NA
                                 ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                 ,5, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                 ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                 ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                 ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                 ,NA, NA, NA, 1, 3, 3, 2, 3, 3, 3
                                 ,2, 2, 11, NA, NA, NA, NA, NA, NA, 2
                                 ,5, 2, 3, 3, NA, NA, NA, NA, NA, NA
                                 ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                 ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                 ,1, 2, 3, 8, 14, 10, 3, 16, 8, 4
                                 ,6, 0, NA, NA, 0, 1, 0, 0, 0, 15
                                 ,0, 0, 0, 0, 0, 0, 0, 0, 0, 10
                                 ,1
                                 )
                       ,'WT_WID'=c(NA, 0.6, NA, NA, NA, NA, NA, NA, 0.8, NA
                                  ,NA, NA, NA, NA, NA, NA, 1.2, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, 0.8, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 0.75, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 0.7, NA, NA, NA, NA, NA, NA, 0.4, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA
                                  )
                       ,'stackSlope'= 0.12 + 0.25*4.7486
                       ,'resDepth'=NA
                       ,'resArea'=NA
                       ,'resLength'=NA
                       ,'poolID'=NA
                       ,stringsAsFactors=FALSE
                       )
                   ,data.frame('UID'='2002 WWYP99-0672 1'
                       ,'TRANSECT'=c('J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                    ,'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                    ,'J', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                    ,'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                    ,'I', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                    ,'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                    ,'H', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                    ,'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                    ,'G', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                    ,'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                    ,'F', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                    ,'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                    ,'E', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                    ,'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                    ,'D', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                    ,'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                    ,'C', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                    ,'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                    ,'B', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                    ,'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                    ,'A'
                                    )
                       ,'STATION'=c(19, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0
                                   )
                       ,'LOC'=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                               ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                               ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                               ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                               ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                               ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                               ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                               ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                               ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                               ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                               ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                               ,110, 111, 112, 113, 114, 115, 116, 117, 118, 119
                               ,120, 121, 122, 123, 124, 125, 126, 127, 128, 129
                               ,130, 131, 132, 133, 134, 135, 136, 137, 138, 139
                               ,140, 141, 142, 143, 144, 145, 146, 147, 148, 149
                               ,150, 151, 152, 153, 154, 155, 156, 157, 158, 159
                               ,160, 161, 162, 163, 164, 165, 166, 167, 168, 169
                               ,170, 171, 172, 173, 174, 175, 176, 177, 178, 179
                               ,180, 181, 182, 183, 184, 185, 186, 187, 188, 189
                               ,190, 191, 192, 193, 194, 195, 196, 197, 198, 199
                               ,200
                               )
                       ,'INCREMNT'=c(27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765
                                    ,27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765
                                    ,27.97765, 27.97765, 35.83085, 35.83085, 35.83085, 35.83085, 35.83085, 35.83085, 35.83085, 35.83085
                                    ,35.83085, 35.83085, 35.83085, 35.83085, 35.83085, 35.83085, 35.83085, 35.83085, 35.83085, 35.83085
                                    ,35.83085, 35.83085, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091
                                    ,31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091
                                    ,31.2091, 31.2091, 29.45835, 29.45835, 29.45835, 29.45835, 29.45835, 29.45835, 29.45835, 29.45835
                                    ,29.45835, 29.45835, 29.45835, 29.45835, 29.45835, 29.45835, 29.45835, 29.45835, 29.45835, 29.45835
                                    ,29.45835, 29.45835, 30.9275, 30.9275, 30.9275, 30.9275, 30.9275, 30.9275, 30.9275, 30.9275
                                    ,30.9275, 30.9275, 30.9275, 30.9275, 30.9275, 30.9275, 30.9275, 30.9275, 30.9275, 30.9275
                                    ,30.9275, 30.9275, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605
                                    ,27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605
                                    ,27.29605, 27.29605, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955
                                    ,30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955
                                    ,30.92955, 30.92955, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925
                                    ,26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925
                                    ,26.69925, 26.69925, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065
                                    ,35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065
                                    ,35.35065, 35.35065, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369
                                    ,21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369
                                    ,21.2369, 21.2369
                                    )
                       ,'DEPTH'=c(0.7477709162424, 1, 0.9, 0.7, 0.8, 0.6, 0.7, 1.2, 1.3
                                 ,1.6, 1.9, 2, 2, 2, 1.4, 0.7, 1.1, 1.3, 0.9
                                 ,1, 1, 1.1, 0.7, 1.4, 0.8, 0.7, 0.8, 0.9, 0.4
                                 ,0.4, 0.5, 0.4, 0.5, 0.4, 0.4, 0.7, 0.6, 0.9, 2
                                 ,2.4, 0.7, 1, 0.7, 0.8, 1, 1.3, 1.3, 1, 1
                                 ,1, 0.8, 1, 1.4, 1.7, 1.6, 1.7, 2.1, 2.4, 2
                                 ,2, 1.6, 1.4, 1.6, 1.4, 1.1, 0.8, 0.8, 1.1, 1.2
                                 ,1.6, 2, 2, 1.7, 1.4, 1.2, 1, 1.4, 1.1, 1.5
                                 ,1.6, 1.5, 1.1, 1.1, 1.1, 1.1, 1.3, 1.2, 1.3, 1.6
                                 ,1.3, 1.2, 0.8, 0.9, 1, 0.9, 1.3, 1, 1.2, 1.8
                                 ,2.1, 2.2, 1, 0.7, 1.1, 1.3, 1.6, 1.4, 1.5, 1.2
                                 ,1.2, 1, 1.4, 1.2, 1.2, 1.4, 1.3, 1, 1.2, 0.8
                                 ,1.4, 1.2, 0.5, 0.8, 1, 1.4, 1.4, 1.4, 1.2, 1.3
                                 ,1.2, 1.3, 1.2, 1.1, 0.9, 0.8, 0.8, 0.5, 0.4, 0.7
                                 ,1.2, 1.2, 1.2, 1.3, 1.6, 1.1, 1, 1, 1.1, 1
                                 ,1.3, 1.9, 2.3, 2.1, 1.1, 1.2, 1.2, 1.1, 1.4, 1
                                 ,0.8, 1, 1.2, 1.3, 1.3, 1.9, 2.1, 2, 1, 1
                                 ,1, 0.9, 0.6, 0.7, 0.5, 0.6, 0.6, 0.5, 0.8, 0.8
                                 ,0.5, 1, 0.8, 0.8, 0.7, 0.8, 0.9, 1.1, 1.3, 1.5
                                 ,1.5, 1.5, 1.6, 1.5, 1.4, 1.5, 1.2, 1.3, 1.3, 1.3
                                 ,1.1, 0.9
                                 )
                       ,'WT_WID'=c(NA, 50.8, 50.6, 50.4, 50.2, 50, 49.8, 49.6, 49.4
                                  ,49.2, 49, 48.8, 48.6, 48.4, 48.2, 48, 47.8, 47.6, 47.4
                                  ,47.2, 47, 46.9, 46.8, 46.7, 46.6, 46.5, 46.4, 46.3, 46.2
                                  ,46.1, 46, 45.9, 45.8, 45.7, 45.6, 45.5, 45.4, 45.3, 45.2
                                  ,45.1, 45, 43.75, 42.5, 41.25, 40, 38.75, 37.5, 36.25, 35
                                  ,33.75, 32.5, 31.25, 30, 28.75, 27.5, 26.25, 25, 23.75, 22.5
                                  ,21.25, 20, 20.8, 21.6, 22.4, 23.2, 24, 24.8, 25.6, 26.4
                                  ,27.2, 28, 28.8, 29.6, 30.4, 31.2, 32, 32.8, 33.6, 34.4
                                  ,35.2, 36, 35.8, 35.6, 35.4, 35.2, 35, 34.8, 34.6, 34.4
                                  ,34.2, 34, 33.8, 33.6, 33.4, 33.2, 33, 32.8, 32.6, 32.4
                                  ,32.2, 32, 32.9, 33.8, 34.7, 35.6, 36.5, 37.4, 38.3, 39.2
                                  ,40.1, 41, 41.9, 42.8, 43.7, 44.6, 45.5, 46.4, 47.3, 48.2
                                  ,49.1, 50, 50.75, 51.5, 52.25, 53, 53.75, 54.5, 55.25, 56
                                  ,56.75, 57.5, 58.25, 59, 59.75, 60.5, 61.25, 62, 62.75, 63.5
                                  ,64.25, 65, 64.85, 64.7, 64.55, 64.4, 64.25, 64.1, 63.95, 63.8
                                  ,63.65, 63.5, 63.35, 63.2, 63.05, 62.9, 62.75, 62.6, 62.45, 62.3
                                  ,62.15, 62, 63.85, 65.7, 67.55, 69.4, 71.25, 73.1, 74.95, 76.8
                                  ,78.65, 80.5, 82.35, 84.2, 86.05, 87.9, 89.75, 91.6, 93.45, 95.3
                                  ,97.15, 99, 96.65, 94.3, 91.95, 89.6, 87.25, 84.9, 82.55, 80.2
                                  ,77.85, 75.5, 73.15, 70.8, 68.45, 66.1, 63.75, 61.4, 59.05, 56.7
                                  ,54.35, 52
                                  )
                       ,'stackSlope'= 0.12 + 0.25*0.392
                       ,'resDepth'=c(NA, 0.1912378067576, 0.0302465297576, 0, 0.039008723, 0, 0.039008723, 0.478017446, 0.517026169
                                    ,0.756034892, 0.995043615, 1.034052338, 0.973061061, 0.912069784, 0.251078507, 0, 0.339008723, 0.478017446, 0.017026169
                                    ,0.056034892, 0, 0.021888747, 0, 0.621888747, 0, 0, 0.021888747, 0.043777494, 0
                                    ,0, 0.021888747, 0, 0.021888747, 0, 0, 0.221888747, 0.043777494, 0.265666241, 1.287554988
                                    ,1.609443735, 0, 0.231964162, 0, 0.031964162, 0.163928324, 0.395892486, 0.327856648, 0, 0
                                    ,0, 0, 0.131964162, 0.463928324, 0.695892486, 0.527856648, 0.55982081, 0.891784972, 1.123749134, 0.655713296
                                    ,0.587677458, 0.11964162, 0, 0.135780797, 0, 0, 0, 0, 0.235780797, 0.271561594
                                    ,0.607342391, 0.943123188, 0.878903985, 0.514684782, 0.150465579, 0, 0, 0.335780797, 0, 0.335780797
                                    ,0.371561594, 0.207342391, 0, 0, 0, 0, 0.13257805, 0, 0.03257805, 0.2651561
                                    ,0, 0, 0, 0.03257805, 0.0651561, 0, 0.33257805, 0, 0.13257805, 0.6651561
                                    ,0.89773415, 0.9303122, 0, 0, 0.340494611, 0.480989222, 0.721483833, 0.461978444, 0.502473055, 0.142967666
                                    ,0.083462277, 0, 0.340494611, 0.080989222, 0.021483833, 0.161978444, 0.002473055, 0, 0.140494611, 0
                                    ,0.540494611, 0.280989222, 0, 0.232573581, 0.365147162, 0.697720743, 0.630294324, 0.562867905, 0.295441486, 0.328015067
                                    ,0.160588648, 0.193162229, 0.02573581, 0, 0, 0, 0, 0, 0, 0.232573581
                                    ,0.665147162, 0.597720743, 0.539516378, 0.581312013, 0.823107648, 0.264903283, 0.106698918, 0.048494553, 0.090290188, 0
                                    ,0.241795635, 0.78359127, 1.125386905, 0.86718254, 0, 0.041795635, 0, 0, 0.241795635, 0
                                    ,0, 0.141795635, 0.264731218, 0.287666801, 0.210602384, 0.733537967, 0.85647355, 0.679409133, 0, 0
                                    ,0, 0, 0, 0.022935583, 0, 0.022935583, 0, 0, 0.222935583, 0.145871166
                                    ,0, 0.422935583, 0.176639141, 0.130342699, 0, 0.053703558, 0.107407116, 0.261110674, 0.414814232, 0.56851779
                                    ,0.522221348, 0.475924906, 0.529628464, 0.383332022, 0.23703558, 0.290739138, 0, 0.053703558, 0.007407116, 0
                                    ,0, 0
                                    )
                       ,'resArea'=c(NA, 5.3503844242314, 0.8462268232724, 0, 1.091372399041, 0, 1.091372399041, 13.373804798082, 14.465177197123
                                   ,21.152079596164, 27.838981995205, 28.930354394246, 27.223961793287, 25.517569192328, 7.0245865913685, 0, 9.484667399041, 13.373804798082, 0.4763521971229
                                   ,1.5677245961638, 0, 0.7842924104449, 0, 22.282802410445, 0, 0, 0.784292410445, 1.5685848208899, 0
                                   ,0, 0.7842924104449, 0, 0.7842924104449, 0, 0, 7.950462410445, 1.5685848208899, 9.5190472313349, 46.13418964178
                                   ,57.667737052225, 0, 7.2393927282742, 0, 0.9975727282742, 5.1160554565484, 12.355448184823, 10.232110913097, 0, 0
                                   ,0, 0, 4.1184827282742, 14.478785456548, 21.718178184823, 16.473930913097, 17.471503641371, 27.831806369645, 35.071199097919, 20.464221826194
                                   ,18.340884554468, 3.733907282742, 0, 3.999878241305, 0, 0, 0, 0, 6.945713241305, 7.9997564826099
                                   ,17.891304723915, 27.78285296522, 25.891061206525, 15.16176444783, 4.4324676891346, 0, 0, 9.891548241305, 0, 9.891548241305
                                   ,10.94559148261, 6.1079647239148, 0, 0, 0, 0, 4.100307641375, 0, 1.007557641375, 8.20061528275
                                   ,0, 0, 0, 1.007557641375, 2.01511528275, 0, 10.285807641375, 0, 4.100307641375, 20.57161528275
                                   ,27.764672924125, 28.7722305655, 0, 0, 9.2941579265866, 13.129105853173, 19.69365877976, 12.610186706346, 13.715529632933, 3.9024525595193
                                   ,2.2781904861058, 0, 9.2941579265866, 2.2106858531731, 0.5864237797597, 4.4213717063462, 0.0675046329328, 0, 3.8349479265866, 0
                                   ,14.753367926587, 7.6698958531731, 0, 7.1933962022186, 11.293837404437, 21.580188606656, 19.494719808874, 17.409251011093, 9.1378722133113, 10.14535841553
                                   ,4.9669346177484, 5.974420819967, 0.7959970221855, 0, 0, 0, 0, 0, 0, 7.1933962022185
                                   ,20.572702404437, 18.487233606656, 14.404682655317, 15.52059476309, 21.976356870864, 7.0727189786378, 2.8487810864115, 1.2947681941852, 2.410680301959, 0
                                   ,6.4557621077738, 20.921299215548, 30.046986323321, 23.153123431095, 0, 1.1159121077737, 0, 0, 6.4557621077737, 0
                                   ,0, 3.7858371077737, 9.3584206315917, 10.169208398771, 7.4449311659496, 25.931043933129, 30.276896700308, 24.017554467486, 0, 0
                                   ,0, 0, 0, 0.8107877671789, 0, 0.8107877671789, 0, 0, 7.880917767179, 5.1566405343579
                                   ,0, 14.951047767179, 3.7512677735029, 2.7680748643931, 0, 1.1404970908902, 2.2809941817804, 5.5451812726706, 8.8093683635608, 12.073555454451
                                   ,11.090362545341, 10.107169636231, 11.247666727122, 8.1407838180118, 5.033900908902, 6.1743979997922, 0, 1.1404970908902, 0.1573041817804, 0
                                   ,0, 0
                                   )
                       ,'resLength'=c(NA, 27.97765, 27.97765, 0, 27.97765, 0, 27.97765, 27.97765, 27.97765
                                     ,27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 27.97765, 0, 27.97765, 27.97765, 27.97765
                                     ,27.97765, 0, 35.83085, 0, 35.83085, 0, 0, 35.83085, 35.83085, 0
                                     ,0, 35.83085, 0, 35.83085, 0, 0, 35.83085, 35.83085, 35.83085, 35.83085
                                     ,35.83085, 0, 31.2091, 0, 31.2091, 31.2091, 31.2091, 31.2091, 0, 0
                                     ,0, 0, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091, 31.2091
                                     ,31.2091, 31.2091, 0, 29.45835, 0, 0, 0, 0, 29.45835, 29.45835
                                     ,29.45835, 29.45835, 29.45835, 29.45835, 29.45835, 0, 0, 29.45835, 0, 29.45835
                                     ,29.45835, 29.45835, 0, 0, 0, 0, 30.9275, 0, 30.9275, 30.9275
                                     ,0, 0, 0, 30.9275, 30.9275, 0, 30.9275, 0, 30.9275, 30.9275
                                     ,30.9275, 30.9275, 0, 0, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605
                                     ,27.29605, 0, 27.29605, 27.29605, 27.29605, 27.29605, 27.29605, 0, 27.29605, 0
                                     ,27.29605, 27.29605, 0, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955, 30.92955
                                     ,30.92955, 30.92955, 30.92955, 0, 0, 0, 0, 0, 0, 30.92955
                                     ,30.92955, 30.92955, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 26.69925, 0
                                     ,26.69925, 26.69925, 26.69925, 26.69925, 0, 26.69925, 0, 0, 26.69925, 0
                                     ,0, 26.69925, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 35.35065, 0, 0
                                     ,0, 0, 0, 35.35065, 0, 35.35065, 0, 0, 35.35065, 35.35065
                                     ,0, 35.35065, 21.2369, 21.2369, 0, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369
                                     ,21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 21.2369, 0, 21.2369, 21.2369, 0
                                     ,0, 0
                                     )
                       ,'poolID'=c(NA, 1, 1, 0, 2, 0, 3, 3, 3
                                  ,3, 3, 3, 3, 3, 3, 0, 4, 4, 4
                                  ,4, 0, 5, 0, 6, 0, 0, 7, 7, 0
                                  ,0, 8, 0, 9, 0, 0, 10, 10, 10, 10
                                  ,10, 0, 11, 0, 12, 12, 12, 12, 0, 0
                                  ,0, 0, 13, 13, 13, 13, 13, 13, 13, 13
                                  ,13, 13, 0, 14, 0, 0, 0, 0, 15, 15
                                  ,15, 15, 15, 15, 15, 0, 0, 16, 0, 17
                                  ,17, 17, 0, 0, 0, 0, 18, 0, 19, 19
                                  ,0, 0, 0, 20, 20, 0, 21, 0, 22, 22
                                  ,22, 22, 0, 0, 23, 23, 23, 23, 23, 23
                                  ,23, 0, 24, 24, 24, 24, 24, 0, 25, 0
                                  ,26, 26, 0, 27, 27, 27, 27, 27, 27, 27
                                  ,27, 27, 27, 0, 0, 0, 0, 0, 0, 28
                                  ,28, 28, 28, 28, 28, 28, 28, 28, 28, 0
                                  ,29, 29, 29, 29, 0, 30, 0, 0, 31, 0
                                  ,0, 32, 32, 32, 32, 32, 32, 32, 0, 0
                                  ,0, 0, 0, 33, 0, 34, 0, 0, 35, 35
                                  ,0, 36, 36, 36, 0, 37, 37, 37, 37, 37
                                  ,37, 37, 37, 37, 37, 37, 0, 38, 38, 0
                                  ,0, 0
                                  )
                       ,stringsAsFactors=FALSE
                       )
                   ,data.frame('UID'='2002 WWYP99-NPR2 1'
                       ,'TRANSECT'=c('J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                    ,'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                    ,'J', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                    ,'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                    ,'I', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                    ,'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                    ,'H', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                    ,'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                    ,'G', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                    ,'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                    ,'F', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                    ,'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                    ,'E', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                    ,'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                    ,'D', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                    ,'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                    ,'C', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                    ,'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                    ,'B', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                    ,'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                    ,'A'
                                    )
                       ,'STATION'=c(19, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                   ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                   ,0
                                   )
                       ,'LOC'=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9
                               ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                               ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                               ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39
                               ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49
                               ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59
                               ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69
                               ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79
                               ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89
                               ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99
                               ,100, 101, 102, 103, 104, 105, 106, 107, 108, 109
                               ,110, 111, 112, 113, 114, 115, 116, 117, 118, 119
                               ,120, 121, 122, 123, 124, 125, 126, 127, 128, 129
                               ,130, 131, 132, 133, 134, 135, 136, 137, 138, 139
                               ,140, 141, 142, 143, 144, 145, 146, 147, 148, 149
                               ,150, 151, 152, 153, 154, 155, 156, 157, 158, 159
                               ,160, 161, 162, 163, 164, 165, 166, 167, 168, 169
                               ,170, 171, 172, 173, 174, 175, 176, 177, 178, 179
                               ,180, 181, 182, 183, 184, 185, 186, 187, 188, 189
                               ,190, 191, 192, 193, 194, 195, 196, 197, 198, 199
                               ,200
                               )
                       ,'INCREMNT'=c(36.56745, 36.56745, 36.56745, 36.56745, 36.56745, 36.56745, 36.56745, 36.56745, 36.56745
                                    ,36.56745, 36.56745, 36.56745, 36.56745, 36.56745, 36.56745, 36.56745, 36.56745, 36.56745, 36.56745
                                    ,36.56745, 36.56745, 41.7576, 41.7576, 41.7576, 41.7576, 41.7576, 41.7576, 41.7576, 41.7576
                                    ,41.7576, 41.7576, 41.7576, 41.7576, 41.7576, 41.7576, 41.7576, 41.7576, 41.7576, 41.7576
                                    ,41.7576, 41.7576, 34.9146, 34.9146, 34.9146, 34.9146, 34.9146, 34.9146, 34.9146, 34.9146
                                    ,34.9146, 34.9146, 34.9146, 34.9146, 34.9146, 34.9146, 34.9146, 34.9146, 34.9146, 34.9146
                                    ,34.9146, 34.9146, 39.2826, 39.2826, 39.2826, 39.2826, 39.2826, 39.2826, 39.2826, 39.2826
                                    ,39.2826, 39.2826, 39.2826, 39.2826, 39.2826, 39.2826, 39.2826, 39.2826, 39.2826, 39.2826
                                    ,39.2826, 39.2826, 39.36215, 39.36215, 39.36215, 39.36215, 39.36215, 39.36215, 39.36215, 39.36215
                                    ,39.36215, 39.36215, 39.36215, 39.36215, 39.36215, 39.36215, 39.36215, 39.36215, 39.36215, 39.36215
                                    ,39.36215, 39.36215, 39.75945, 39.75945, 39.75945, 39.75945, 39.75945, 39.75945, 39.75945, 39.75945
                                    ,39.75945, 39.75945, 39.75945, 39.75945, 39.75945, 39.75945, 39.75945, 39.75945, 39.75945, 39.75945
                                    ,39.75945, 39.75945, 32.6829, 32.6829, 32.6829, 32.6829, 32.6829, 32.6829, 32.6829, 32.6829
                                    ,32.6829, 32.6829, 32.6829, 32.6829, 32.6829, 32.6829, 32.6829, 32.6829, 32.6829, 32.6829
                                    ,32.6829, 32.6829, 36.7343, 36.7343, 36.7343, 36.7343, 36.7343, 36.7343, 36.7343, 36.7343
                                    ,36.7343, 36.7343, 36.7343, 36.7343, 36.7343, 36.7343, 36.7343, 36.7343, 36.7343, 36.7343
                                    ,36.7343, 36.7343, 36.17845, 36.17845, 36.17845, 36.17845, 36.17845, 36.17845, 36.17845, 36.17845
                                    ,36.17845, 36.17845, 36.17845, 36.17845, 36.17845, 36.17845, 36.17845, 36.17845, 36.17845, 36.17845
                                    ,36.17845, 36.17845, 22.99615, 22.99615, 22.99615, 22.99615, 22.99615, 22.99615, 22.99615, 22.99615
                                    ,22.99615, 22.99615, 22.99615, 22.99615, 22.99615, 22.99615, 22.99615, 22.99615, 22.99615, 22.99615
                                    ,22.99615, 22.99615
                                    )
                       ,'DEPTH'=c(0.515202205623, 1.5, 0.6, 0.7, 0.8, 0.5, 0.6, 1, 1
                                 ,0.8, 0.5, 0.5, 0.9, 1.1, 0.6, 1.3, 1, 2.4, 1.5
                                 ,0.9, 1, 0.9, 0.8, 0.7, 0.9, 0.6, 0.7, 0.9, 0.7
                                 ,0.8, 1, 1.2, 1.1, 1, 0.8, 1, 0.6, 0.9, 1
                                 ,1, 1.3, 1.7, 1.8, 1.1, 1, 0.9, 0.9, 1.2, 2.3
                                 ,2.3, 1.9, 1.6, 1.4, 0.6, 3, 0.6, 0.5, 0.6, 0.7
                                 ,0.7, 1, 1, 0.9, 0.8, 0.6, 1, 1, 0.8, 0.8
                                 ,0.7, 0.6, 0.7, 0.8, 1, 1, 0.8, 0.7, 0.9, 1.2
                                 ,0.7, 0.7, 0.5, 0.7, 0.7, 0.8, 0.9, 1.1, 1.1, 1
                                 ,1, 0.9, 0.3, 0.7, 0.8, 0.6, 0.7, 1, 1.2, 1.5
                                 ,0.7, 0.7, 1.2, 1.5, 1.3, 0.8, 0.3, 0.3, 0.6, 0.8
                                 ,0.8, 0.8, 0.9, 0.9, 0.7, 0.7, 1.5, 0.7, 0.6, 0.4
                                 ,0.4, 0.4, 0.6, 0.8, 1.2, 1.1, 1.3, 0.2, 0.6, 1.2
                                 ,2.1, 2.3, 1.9, 1.2, 0.8, 0.8, 0.9, 1.2, 0.8, 0.5
                                 ,0.6, 0.5, 0.9, 1.5, 0.6, 0.6, 0.8, 0.8, 1.1, 1
                                 ,1, 0.6, 0.8, 0.8, 0.9, 0.8, 1, 1.1, 1.4, 0.9
                                 ,1.1, 0.7, 0.6, 1.1, 1.3, 1.1, 0.5, 0.5, 0.6, 0.6
                                 ,0.8, 0.7, 0.7, 0.6, 0.4, 0.6, 0.5, 0.4, 0.7, 0.4
                                 ,0.7, 0.6, 0.7, 0.8, 0.6, 0.6, 0.7, 0.4, 0.8, 1.2
                                 ,1.4, 1.3, 1.5, 1.7, 1.4, 1.3, 1.3, 1.3, 1.3, 1.9
                                 ,1.8, 1
                                 )
                       ,'WT_WID'=c(NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 83, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 57, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 60, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 67, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 48, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 60, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 50, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 41, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 68, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                  ,NA, 78
                                  )
                       ,'stackSlope'=NA
                       ,'resDepth'=NA
                       ,'resArea'=NA
                       ,'resLength'=NA
                       ,'poolID'=NA
                       ,stringsAsFactors=FALSE
                       )
                   ,data.frame(UID='2000 WCAP99-0604 1'
                        ,TRANSECT=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                   ,'A', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                   ,'B', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                   ,'C', 'C', 'C', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                   ,'D', 'D', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                   ,'E', 'E', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                   ,'F', 'F', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                   ,'G', 'G', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                   ,'H', 'H', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                   ,'I', 'I', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                   ,'J', 'J', 'J'
                                   )
                        ,STATION=c('0', '0', '1', '2', '3', '4', '5', '6', '7', '8'
                                  ,'9', '0', '1', '2', '3', '4', '5', '6', '7', '8'
                                  ,'9', '0', '1', '2', '3', '4', '5', '6', '7', '8'
                                  ,'9', '10', '11', '0', '1', '2', '3', '4', '5', '6'
                                  ,'7', '8', '0', '1', '2', '3', '4', '5', '6', '7'
                                  ,'8', '9', '0', '1', '2', '3', '4', '5', '6', '7'
                                  ,'8', '9', '0', '1', '2', '3', '4', '5', '6', '7'
                                  ,'8', '9', '0', '1', '2', '3', '4', '5', '6', '7'
                                  ,'8', '9', '0', '1', '2', '3', '4', '5', '6', '7'
                                  ,'8', '9', '0', '1', '2', '3', '4', '5', '6', '7'
                                  ,'8', '9', '10'
                                  )
                        ,LOC=c(0,1:41,43:103) # 0:102
                        ,INCREMNT=c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5
                                   ,1.5, 1.5, 1.5
                                   )
                        ,DEPTH=c(11.101544307, 15, 88, 30, 25, 20, 5, 20, 30, 25
                                ,31, 20, 15, 23, 16, 20, 6, 23, 12, 21
                                ,19, 20, 36, 25, 10, 25, 7, 37, 5, 40
                                ,29, 30, 8, 10, 8, 38, 10, 20, 80, 37
                                ,3, 20, 20, 8, 20, 40, 28, 12, 30, 8
                                ,16, 23, 10, 11, 9, 35, 40, 21, 31, 32
                                ,41, 45, 23, 20, 40, 35, 20, 35, 20, 20
                                ,9, 38, 45, 15, 20, 39, 30, 37, 40, 35
                                ,20, 55, 25, 33, 15, 33, 25, 31, 27, 16
                                ,15, 31, 40, 20, 28, 12, 8, 11, 30, 40
                                ,62, 28, 30
                                )
                        ,WT_WID=c(NA, 0.6, 0.88, 1.16, 1.44, 1.72, 2, 2.36, 2.72, 3.08
                                 ,3.44, 3.8, 3.84, 3.88, 3.92, 3.96, 4, 3.76, 3.52, 3.28
                                 ,3.04, 2.8, 2.64, 2.48, 2.32, 2.16, 2, 2.1, 2.2, 2.3
                                 ,2.4, 2.5, 2.6, 2.7, 2.9, 3.1, 3.3, 3.5, 3.7, 3.28
                                 ,2.86, 2.44, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.28, 2.46
                                 ,2.64, 2.82, 3, 3.8, 4.6, 5.4, 6.2, 7, 6.44, 5.88
                                 ,5.32, 4.76, 4.2, 3.6, 3, 2.4, 1.8, 1.2, 2.48, 3.76
                                 ,5.04, 6.32, 7.6, 8.04, 8.48, 8.92, 9.36, 9.8, 8.74, 7.68
                                 ,6.62, 5.56, 4.5, 4.36, 4.22, 4.08, 3.94, 3.8, 4.1, 4.4
                                 ,4.7, 5, 5.3, 4.84, 4.38, 3.92, 3.46, 3, NA, NA
                                 ,NA, NA, NA
                                 )
                        ,stackSlope=13.64/4 + 0.12
                        ,resDepth=c(NA, 3.898455693, 71.603455693, 8.308455693, 0, 0, 0, 9.705, 14.41, 4.115
                                   ,4.82, 0, 0, 2.705, 0, 0, 0, 11.705, 0, 3.705
                                   ,0, 0, 10.705, 0, 0, 9.705, 0, 24.705, 0, 29.705
                                   ,13.41, 9.115, 0, 0, 0, 24.705, 0, 4.705, 59.41, 11.115
                                   ,0, 11.705, 1.115, 0, 6.705, 21.41, 4.115, 0, 12.705, 0
                                   ,2.705, 4.41, 0, 0, 0, 20.705, 20.41, 0, 4.705, 0.41
                                   ,4.115, 2.82, 0, 0, 14.705, 4.41, 0, 9.705, 0, 0
                                   ,0, 23.705, 25.41, 0, 0, 13.705, 0, 1.705, 0, 0
                                   ,0, 29.705, 0, 2.705, 0, 12.705, 0, 0.705, 0, 0
                                   ,0, 10.705, 14.41, 0, 2.705, 0, 0, 0, 13.705, 18.41
                                   ,35.115, 0, 0
                                   )
                        ,resArea=c(NA, 0, 1.0740518354, 0.1246268354, 0, 0, 0, 0.145575, 0.21615, 0.061725
                                  ,0.0723, 0, 0, 0.040575, 0, 0, 0, 0.175575, 0, 0.055575
                                  ,0, 0, 0.160575, 0, 0, 0.145575, 0, 0.370575, 0, 0.445575
                                  ,0.20115, 0.136725, 0, 0, 0, 0.370575, 0, 0.070575, 0.89115, 0.166725
                                  ,0, 0.175575, 0.03345, 0, 0.100575, 0.32115, 0.061725, 0, 0.190575, 0
                                  ,0.040575, 0.06615, 0, 0, 0, 0.310575, 0.30615, 0, 0.070575, 0.00615
                                  ,0.061725, 0.0423, 0, 0, 0.220575, 0.06615, 0, 0.145575, 0, 0
                                  ,0, 0.355575, 0.38115, 0, 0, 0.205575, 0, 0.025575, 0, 0
                                  ,0, 0.445575, 0, 0.040575, 0, 0.190575, 0, 0.010575, 0, 0
                                  ,0, 0.160575, 0.21615, 0, 0.040575, 0, 0, 0, 0.205575, 0.27615
                                  ,0.526725, 0, 0
                                  )
                        ,resLength=c(NA, 1.5, 1.5, 1.5, 0, 0, 0, 1.5, 1.5, 1.5
                                    ,1.5, 0, 0, 1.5, 0, 0, 0, 1.5, 0, 1.5
                                    ,0, 0, 1.5, 0, 0, 1.5, 0, 1.5, 0, 1.5
                                    ,1.5, 1.5, 0, 0, 0, 1.5, 0, 1.5, 1.5, 1.5
                                    ,0, 1.5, 3, 0, 1.5, 1.5, 1.5, 0, 1.5, 0
                                    ,1.5, 1.5, 0, 0, 0, 1.5, 1.5, 0, 1.5, 1.5
                                    ,1.5, 1.5, 0, 0, 1.5, 1.5, 0, 1.5, 0, 0
                                    ,0, 1.5, 1.5, 0, 0, 1.5, 0, 1.5, 0, 0
                                    ,0, 1.5, 0, 1.5, 0, 1.5, 0, 1.5, 0, 0
                                    ,0, 1.5, 1.5, 0, 1.5, 0, 0, 0, 1.5, 1.5
                                    ,1.5, 0, 0
                                    )
                        ,poolID=c(NA, 1, 1, 1, 0, 0, 0, 2, 2, 2
                                 ,2, 0, 0, 3, 0, 0, 0, 4, 0, 5
                                 ,0, 0, 6, 0, 0, 7, 0, 8, 0, 9
                                 ,9, 9, 0, 0, 0, 10, 0, 11, 11, 11
                                 ,0, 12, 12, 0, 13, 13, 13, 0, 14, 0
                                 ,15, 15, 0, 0, 0, 16, 16, 0, 17, 17
                                 ,17, 17, 0, 0, 18, 18, 0, 19, 0, 0
                                 ,0, 20, 20, 0, 0, 21, 0, 22, 0, 0
                                 ,0, 23, 0, 24, 0, 25, 0, 26, 0, 0
                                 ,0, 27, 27, 0, 28, 0, 0, 0, 29, 29
                                 ,29, 0, 0
                                 )
                        ,stringsAsFactors=FALSE
                        )
                   ,data.frame(UID='2002 WMTP99-0601 1'
                              ,TRANSECT=c('J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                         ,'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J', 'J'
                                         ,'J', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                         ,'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I'
                                         ,'I', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                         ,'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
                                         ,'H', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                         ,'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
                                         ,'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                         ,'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F', 'F'
                                         ,'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                         ,'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E'
                                         ,'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                         ,'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
                                         ,'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
                                         ,'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'B'
                                         ,'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'
                                         ,'B', 'B', 'B', 'B', 'B', 'B', 'A', 'A', 'A', 'A'
                                         ,'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'
                                         ,'A', 'A', 'A', 'A'
                                         )
                              ,STATION=c(19, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                        ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                        ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                        ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                        ,0, 19, 18, 17, 16, 15, 14, 13, 12, 11
                                        ,10, 9, 8, 7, 6, 5, 4, 3, 2, 1
                                        ,0, 18, 17, 16, 15, 14, 13, 12, 11, 10
                                        ,9, 8, 7, 6, 5, 4, 3, 2, 1, 0
                                        ,19, 18, 17, 16, 15, 14, 13, 12, 11, 10
                                        ,9, 8, 7, 6, 5, 4, 3, 2, 1, 0
                                        ,19, 18, 17, 16, 15, 14, 13, 12, 11, 10
                                        ,9, 8, 7, 6, 5, 4, 3, 2, 1, 0
                                        ,19, 18, 17, 16, 15, 14, 13, 12, 11, 10
                                        ,9, 8, 7, 6, 5, 4, 3, 2, 1, 0
                                        ,18, 17, 16, 15, 14, 13, 12, 11, 10, 9
                                        ,8, 7, 6, 5, 4, 3, 2, 1, 0, 16
                                        ,15, 14, 13, 12, 11, 10, 9, 8, 7, 6
                                        ,5, 4, 3, 2, 1, 0, 17, 16, 15, 14
                                        ,13, 12, 11, 10, 9, 8, 7, 6, 5, 4
                                        ,3, 2, 1, 0
                                        )
                              ,LOC=0:193
                              ,INCREMNT=c(rep(23.0875, times=21)
                                         ,rep(30.73915, times=20)
                                         ,rep(31.04895, times=20)
                                         ,rep(19.049210526, times=19)
                                         ,rep(23.56325, times=20)
                                         ,rep(23.7489, times=20)
                                         ,rep(66.04805, times=20)
                                         ,rep(30.194263158, times=19)
                                         ,rep(35.914823529, times=17)
                                         ,rep(16.287055556, times=18)
                                         )
                              ,DEPTH=c(0.9778415343, 1.4, 1.5, 1.4, 1.3, 1.4, 1.5, 1.6, 1.6
                                      ,1.6, 1.3, 1.5, 1.4, 1.4, 1.5, 1.3, 1.6, 1.3, 1
                                      ,1, 0.8, 1, 1.3, 1.3, 1.5, 1.2, 1.1, 1.1, 1.2
                                      ,1.3, 1.3, 1.1, 1.3, 1.4, 1.7, 1.9, 1.6, 2.3, 2.1
                                      ,2, 1.8, 1.1, 1.4, 1.8, 1, 1.1, 1.1, 1, 1.2
                                      ,1.2, 1.3, 1.6, 1.8, 1.4, 1.6, 1.6, 1.3, 1.3, 1.2
                                      ,1, 0.7, 1.1, 1.1, 1, 1, 1.1, 1, 1.2, 1.3
                                      ,1.7, 2.5, 2.2, 1.8, 1.8, 1.4, 1.1, 1.1, 1.2, 1.2
                                      ,1.2, 1.2, 2, 1.3, 1.1, 1, 1, 1.2, 1.3, 1.3
                                      ,1.3, 1.3, 1.5, 1.6, 1.6, 1.9, 1.3, 0.8, 0.7, 0.9
                                      ,1, 1.1, 1.3, 1.3, 1.2, 1.3, 1.3, 1.2, 1.3, 1.3
                                      ,1.2, 1.1, 1.1, 1.1, 1.1, 1.1, 1.2, 1.1, 1.2, 1.2
                                      ,1.2, 1.2, 1.2, 1.2, 1.3, 1.4, 1.3, 1.5, 1.5, 1.2
                                      ,1.2, 1.2, 1.2, 1.4, 1.2, 1, 1.2, 1.3, 1.5, 1.5
                                      ,1.9, 1.7, 2.1, 2, 2, 1.7, 1.1, 0.9, 1.5, 1.3
                                      ,1.2, 0.9, 0.8, 1.4, 2, 2.2, 1.8, 1.3, 1.5, 1.1
                                      ,1.3, 1.2, 1.4, 1.1, 1, 1.1, 1.1, 1, 1.2, 0.9
                                      ,1, 0.9, 0.9, 0.9, 0.8, 0.8, 0.9, 1.1, 1.4, 1.5
                                      ,1.8, 1.8, 1.4, 1.1, 1, 1, 1, 0.9, 0.9, 0.8
                                      ,0.9, 1, 1.1, 1.2, 1.3
                                      )
                              ,WT_WID=c(47, 47.722222222, 48.444444444, 49.166666667, 49.888888889, 50.611111111, 51.333333333, 52.055555556, 52.777777778, 53.5
                                       ,54.222222222, 54.944444444, 55.666666667, 56.388888889, 57.111111111, 57.833333333, 58.555555556, 59.277777778, 60, 59
                                       ,58, 57, 56, 55, 54, 53, 52, 51, 50, 49
                                       ,48, 47, 46, 45, 44, 43, 42.684210526, 42.368421053, 42.052631579, 41.736842105
                                       ,41.421052632, 41.105263158, 40.789473684, 40.473684211, 40.157894737, 39.842105263, 39.526315789, 39.210526316, 38.894736842, 38.578947368
                                       ,38.263157895, 37.947368421, 37.631578947, 37.315789474, 37, 37.3, 37.6, 37.9, 38.2, 38.5
                                       ,38.8, 39.1, 39.4, 39.7, 40, 40.3, 40.6, 40.9, 41.2, 41.5
                                       ,41.8, 42.1, 42.4, 42.7, 43, 43.75, 44.5, 45.25, 46, 46.75
                                       ,47.5, 48.25, 49, 49.75, 50.5, 51.25, 52, 52.75, 53.5, 54.25
                                       ,55, 55.75, 56.5, 57.25, 58, 57.3, 56.6, 55.9, 55.2, 54.5
                                       ,53.8, 53.1, 52.4, 51.7, 51, 50.3, 49.6, 48.9, 48.2, 47.5
                                       ,46.8, 46.1, 45.4, 44.7, 44, 45.578947368, 47.157894737, 48.736842105, 50.315789474, 51.894736842
                                       ,53.473684211, 55.052631579, 56.631578947, 58.210526316, 59.789473684, 61.368421053, 62.947368421, 64.526315789, 66.105263158, 67.684210526
                                       ,69.263157895, 70.842105263, 72.421052632, 74, 72.85, 71.7, 70.55, 69.4, 68.25, 67.1
                                       ,65.95, 64.8, 63.65, 62.5, 61.35, 60.2, 59.05, 57.9, 56.75, 55.6
                                       ,54.45, 53.3, 52.15, 51, 51.6, 52.2, 52.8, 53.4, 54, 54.6
                                       ,55.2, 55.8, 56.4, 57, 57.6, 58.2, 58.8, 59.4, 60, 60.6
                                       ,61.2, 61.8, 62.4, 63, 62.5, 62, 61.5, 61, 60.5, 60
                                       ,59.5, 59, 58.5, 58, 57.5, 57, 56.5, 56, 55.5, 55
                                       ,54.5, 54, 53.5, 53
                                       )
                              ,stackSlope= 0.12 + 0.25*0.311
                              ,resDepth=rev(c(0.3389617382, 0.2711693906, 0.2033770429, 0.1355846953, 0.0677923476, 0, 0, 0, 0, 0
                                         ,0, 0.0035248699, 0.3357325223, 0.7679401747, 0.800147827,0.5323554794, 0.4645631317, 0.1967707841, 0.0289784365, 0
                                         ,0, 0, 0, 0, 0.0289784365, 0, 0.1289784365, 0, 0, 0.0289784365
                                         ,0, 0, 0.1289784365, 0, 0.1289784365, 0, 0.3417450676, 0.201454223, 0.7611633784, 1.2208725338
                                         ,1.0805816892, 0.5402908446, 0, 0, 0.1208725338, 0.2805816892, 0.5402908446, 0, 0, 0
                                         ,0.1305533595, 0.1902625149, 0.3499716703, 0.0096808257, 0.2693899811, 0, 0.1081699434, 0.0387799623, 0.0693899811, 0
                                         ,0, 0.0693899811, 0, 0, 0, 0, 0, 0.0693899811, 0, 0
                                         ,0, 0, 0, 0, 0, 0.0060731005, 0.0530365502, 0, 0.0530365502, 0
                                         ,0, 0, 0, 0, 0, 0.0841362985, 0.1310997483, 0.078063198, 0.2250266478, 0.2719900975
                                         ,0.2189535473, 0.365916997, 0.4128804468, 0.2598438965, 0.2068073463, 0.1534036731, 0, 0, 0, 0.4806330581
                                         ,0.227229385, 0.2738257119, 0.2204220388, 0.0670183656, 0.1136146925, 0.1602110194, 0.2068073463, 0.1534036731, 0, 0
                                         ,0, 0.0068073463, 0.7534036731, 0, 0, 0.0246603724, 0.0623301862 , 0, 0, 0.1726226066
                                         ,0.6102924204, 0.6479622342, 1.085632048, 1.4233018618, 0.6609716757, 0.2986414895, 0.2363113033, 0.0739811171, 0.2116509309, 0.1493207447
                                         ,0.1869905586, 0.3246603724, 0.3623301862, 0, 0, 0, 0, 0, 0.108805611, 0.1702049096
                                         ,0.0316042082, 0.4930035069, 0.3544028055, 0.1158021041, 0.0772014027, 0.1386007014, 0, 0, 0.0386007014, 0
                                         ,0.5772014027, 0.2386007014, 0, 0.1529199779, 0.413706647, 0.5744933161, 0.8352799852, 0.1960666544, 0.5568533235, 0.4176399926
                                         ,0.1784266617, 0.1392133309, 0, 0.0176399926, 0.0784266618, 0.0392133309, 0, 0, 0.0960666544, 0.4568533235
                                         ,0.3176399926, 0.3784266618, 0.1392133309, 0, 0, 0, 0, 0.2543446665, 0, 0.0543446665
                                         ,0, 0.008689333, 0.1543446665, 0, 0.2112604642, 0.2569157977, 0.3025711312, 0.2482264647, 0.1938817982, 0.1395371317
                                         ,0.2851924652, 0.4308477987, 0.3765031322, NA
                                         ))
                              ,resArea=rev(c(5.5206886612, 4.4165509289, 3.3124131967, 2.2082754645, 1.1041377322, 0, 0, 0, 0, 0
                                        ,0, 0.0574097526, 5.4680942426, 12.507484288, 13.032052111, 8.6705032681, 7.5663655358, 3.2048166925, 1.040755432, 0
                                        ,0, 0, 0, 0, 1.040755432, 0, 4.6322377849, 0, 0, 1.040755432
                                        ,0, 0, 4.6322377849, 0, 4.6322377849, 0, 10.318740505, 6.0827618243, 22.982767354, 36.863346568
                                        ,32.627367888, 16.313683944, 0, 0, 3.6496570946, 8.4719573613, 16.313683944, 0, 0, 0
                                        ,3.9419624943, 5.7448364452, 10.567136712, 0.2923053997, 17.792682943, 0, 7.1444138285, 2.5613408857, 4.5830729428, 0
                                        ,0, 4.5830729428, 0, 0, 0, 0, 0, 4.5830729428, 0, 0
                                        ,0, 0, 0, 0, 0, 0.1442294565, 1.2595597282, 0, 1.2595597282, 0
                                        ,0, 0, 0, 0, 0, 1.9981445394, 3.1134748112, 1.853915083, 5.3441353547, 6.4594656265
                                        ,5.1999058983, 8.6901261701, 9.8054564418, 6.1710067136, 4.8730532015, 3.6146891008, 0, 0, 0, 11.325276907
                                        ,5.3542628061, 6.4522237053, 5.1938596046, 1.5791705038, 2.6771314031, 3.7750923023, 4.8730532015, 3.6146891008, 0, 0
                                        ,0, 0.1604032015, 17.752639101, 0, 0, 0.4697606249, 1.1873408388, 0, 0, 3.2883243743
                                        ,11.625588799, 12.343169013, 20.680433437, 27.112776809, 12.590988602, 5.6888846049, 4.5015437661, 1.4092818747, 4.0317831412, 2.8444423024
                                        ,3.5620225163, 6.1845237828, 6.9021039967, 0, 0, 0, 0, 0, 3.3782999757, 5.2846837287
                                        ,0.9812774817, 15.307241235, 11.003834988, 3.5955337409, 2.3970224939, 4.303406247, 0, 0, 1.198511247, 0
                                        ,17.921497494, 7.408301247, 0, 4.7006301379, 12.716990678, 17.659436218, 25.675796759, 6.0269222988, 17.117197839, 12.837898379
                                        ,5.4846839195, 4.2792994598, 0, 0.5422383793, 2.4107689195, 1.2053844598, 0, 0, 2.9530072988, 14.043282839
                                        ,9.7639833793, 11.63251392, 4.2792994598, 0, 0, 0, 0, 5.8721570534, 0, 1.2546770534
                                        ,0, 0.2006141067, 3.5634170534, 0, 4.877454841, 5.9315177876, 6.9855807343, 5.7309036809, 4.4762266276, 3.2215495742
                                        ,6.5843525209, 9.9471554675, 8.6924784142, NA
                                        ))
                              ,resLength=rev(c(16.287055556, 16.287055556, 16.287055556, 16.287055556, 16.287055556, 0, 0, 0, 0, 0
                                          ,0, 16.287055556, 16.287055556, 16.287055556, 16.287055556, 16.287055556, 16.287055556, 16.287055556, 35.914823529, 0
                                          ,0, 0, 0, 0, 35.914823529, 0, 35.914823529, 0, 0, 35.914823529
                                          ,0, 0, 35.914823529, 0, 35.914823529, 0, 30.194263158, 30.194263158, 30.194263158, 30.194263158
                                          ,30.194263158, 30.194263158, 0, 0, 30.194263158, 30.194263158, 30.194263158, 0, 0, 0
                                          ,30.194263158, 30.194263158, 30.194263158, 30.194263158, 66.04805, 0, 66.04805, 66.04805, 66.04805, 0
                                          ,0, 66.04805, 0, 0, 0, 0, 0, 66.04805, 0, 0
                                          ,0, 0, 0, 0, 0, 23.7489, 23.7489, 0, 23.7489, 0
                                          ,0, 0, 0, 0, 0, 23.7489, 23.7489, 23.7489, 23.7489, 23.7489
                                          ,23.7489, 23.7489, 23.7489, 23.7489, 23.56325, 23.56325, 0, 0, 0, 23.56325
                                          ,23.56325, 23.56325, 23.56325, 23.56325, 23.56325, 23.56325, 23.56325, 23.56325, 0, 0
                                          ,0, 23.56325, 23.56325, 0, 0, 19.049210526, 19.049210526, 0, 0, 19.049210526
                                          ,19.049210526, 19.049210526, 19.049210526, 19.049210526, 19.049210526, 19.049210526, 19.049210526, 19.049210526, 19.049210526, 19.049210526
                                          ,19.049210526, 19.049210526, 19.049210526, 0, 0, 0, 0, 0, 31.04895, 31.04895
                                          ,31.04895, 31.04895, 31.04895, 31.04895, 31.04895, 31.04895, 0, 0, 31.04895, 0
                                          ,31.04895, 31.04895, 0, 30.73915, 30.73915, 30.73915, 30.73915, 30.73915, 30.73915, 30.73915
                                          ,30.73915, 30.73915, 0, 30.73915, 30.73915, 30.73915, 0, 0, 30.73915, 30.73915
                                          ,30.73915, 30.73915, 30.73915, 0, 0, 0, 0, 23.0874, 0, 23.0874
                                          ,0, 23.0874, 23.0874, 0, 23.0874, 23.0874, 23.0874,23.0874, 23.0874, 23.0874
                                          ,23.0874, 23.0874, 23.0874, NA
                                          ))
                              ,poolID=rev(c(30, 30, 30, 30, 30, 0, 0, 0, 0, 0
                                       ,0, 29, 29, 29, 29, 29, 29, 29, 29, 0
                                       ,0, 0, 0, 0, 28, 0, 27, 0, 0, 26
                                       ,0, 0, 25, 0, 24, 0, 23, 23, 23, 23
                                       ,23, 23, 0, 0, 22, 22, 22, 0, 0, 0
                                       ,21, 21, 21, 21, 21, 0, 20, 20, 20, 0
                                       ,0, 19, 0, 0, 0, 0, 0, 18, 0, 0
                                       ,0, 0, 0, 0, 0, 17, 17, 0, 16, 0
                                       ,0, 0, 0, 0, 0, 15, 15, 15, 15, 15
                                       ,15, 15, 15, 15, 15, 15, 0, 0, 0, 14
                                       ,14, 14, 14, 14, 14, 14, 14, 14, 0, 0
                                       ,0, 13, 13, 0, 0, 12, 12, 0, 0, 11
                                       ,11, 11, 11, 11, 11, 11, 11, 11, 11, 11
                                       ,11, 11, 11, 0, 0, 0, 0, 0, 10, 10
                                       ,10, 10, 10, 10, 10, 10, 0, 0, 9, 0
                                       ,8, 8, 0, 7, 7, 7, 7, 7, 7, 7
                                       ,7, 7, 0, 6, 6, 6, 0, 0, 5, 5
                                       ,5, 5, 5, 0, 0, 0, 0, 4, 0, 3
                                       ,0, 2, 2, 0, 1, 1, 1, 1, 1, 1
                                       ,1, 1, 1, NA
                                       ))
                              ,stringsAsFactors=FALSE
                              )
                   ) # end of rbind of expected residual dimension calculations

  expected$STATION <- as.integer(expected$STATION)
  expected$LOC <- as.integer(expected$LOC)

  # Wadeable depths and residual depths in EMAP/SAS code were in cm; NRSA
  # works in m.  Convert the above data, culled from WEMAP, to the expected
  # formats.
  expected$DEPTH <- ifelse(expected$UID %in%
                           c('2002 WWYP99-0672 1','2002 WWYP99-NPR2 1','2002 WMTP99-0601 1')
                          ,expected$DEPTH
                          ,expected$DEPTH/100      # convert cm to m in streams

                          )
  expected$resDepth <- ifelse(expected$UID %in%
                           c('2002 WWYP99-0672 1','2002 WWYP99-NPR2 1','2002 WMTP99-0601 1')
                          ,expected$resDepth
                          ,expected$resDepth/100    # convert cm to m in streams

                          )

  # return only requested UIDs
  if(!is.null(uids)) {
      expected <- subset(expected, UID %in% uids)
  }
  
  return(expected)
}

metsResidualPools.createActransp <- function()
# Create fake actual transect spacing data for boatable reaches from the
# station distance specified in the fake thalweg data.
#
# Note: The value of ACTRANSP listed here are given in the WEMAP rparrays output
# but apparently reversed.  This reversal is because the transect and station
# values in rparrays is reversed (A->J, B->I, etc) in the code.
#
# Arguments:
# fakeThal  dataframe of thalweg data from which ACTRANSP values are
# calculated.
{
  rc<-data.frame(UID = c(rep('2002 WMTP99-0601 1', times=10)
  
                        ,rep('2002 WWYP99-0672 1', times=10)
                        ,rep('2002 WWYP99-NPR2 1', times=10)
                        )
                ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
                             ,'A','B','C','D','E','F','G','H','I','J'
                             ,'A','B','C','D','E','F','G','H','I','J'
                             )
                ,PARAMETER = rep('ACTRANSP', times=30)
                ,RESULT = c(rev(c(461.75, 614.78, 620.98, 361.94, 471.27
                                 ,474.98, 1320.96, 573.69, 610.55, 293.17
                                 )
                               )
                           ,424.738, 707.013, 533.985, 618.591, 545.921
                           ,618.55, 589.167, 624.182, 716.617, 559.553
                           ,459.923, 723.569, 734.686, 653.658, 795.189
                           ,787.243, 785.652, 698.292, 835.152, 731.349
                           )
                ,stringsAsFactors=FALSE
                )

  return(rc)
}

metsResidualPools.createActranspOLD <- function(fakeThal)
# Create fake actual transect spacing data for boatable reaches from the
# station distance specified in the fake thalweg data.
#
# Arguments:
# fakeThal  dataframe of thalweg data from which ACTRANSP values are
# calculated.
{
  boatableReaches <- c('2002 WWYP99-0672 1','2002 WWYP99-NPR2 1','2002 WMTP99-0601 1')

  fakeActransp <-data.frame('UID'=rep(boatableReaches, each=10)
                           ,'TRANSECT'=rep(LETTERS[1:10], times=3)
                           ,'PARAMETER'='ACTRANSP'
                           ,stringsAsFactors=FALSE
                           )
  fakeActransp <- merge(fakeActransp
                       ,subset(fakeThal
                              ,PARAMETER=='INCREMNT' &
                               UID %in% boatableReaches &
                               STATION==0
                              ,select=c(UID, TRANSECT, RESULT)
                              )
                       ,by=c('UID','TRANSECT')
                       )
  fakeActransp$RESULT <- 20 * as.numeric(fakeActransp$RESULT)

  return(fakeActransp)
}


metsResidualPools.createSlopes <- function()
# Create fake slopes data for unit test.
{
  fakeSlopes <- data.frame('UID'=c('2004 ORSE04-R022 1','2004SHB-0315 1'
                                  ,'2004 SHB-0395 1', '2004 SHB-0395 1 missing incremnt'
                                  ,'2004 EPA01-0450 1', '2004 WNVP99-REN1 1'
                                  ,'2002 WWYP99-0672 1', '2002 WWYP99-NPR2 1'
                                  ,'2000 WCAP99-0604 1', '2002 WMTP99-0601 1'
                                  )
                          ,'METRIC'='xslope'
                          ,'RESULT'=c('3.9000', NA
                                     ,'2.6750', '2.6750'
                                     ,'6.4760', '4.7486'
                                     ,'0.39200', NA
                                     ,'13.64', '0.31100'
                                     )
                          ,stringsAsFactors=FALSE
                          )
  return(fakeSlopes)
}

metsResidualPools.createProtocol <- function()
# Create fake protocol data.
{
  fakeProtocol <- data.frame('UID'=c('2004 ORSE04-R022 1','2004SHB-0315 1'
                                    ,'2004 SHB-0395 1', '2004 SHB-0395 1 missing incremnt'
                                    ,'2004 EPA01-0450 1', '2004 WNVP99-REN1 1'
                                    ,'2002 WWYP99-0672 1', '2002 WWYP99-NPR2 1'
                                    ,'2000 WCAP99-0604 1', '2002 WMTP99-0601 1'
                                    )
                            ,'PROTOCOL'=c(rep('WADEABLE', 6), rep('BOATABLE', 2)
                                         ,'WADEABLE', 'BOATABLE'
                                         )
                            ,stringsAsFactors=FALSE
                            )
  return(fakeProtocol)
}


metsResidualPools.createPoolCharacteristics <- function(uids)
# Creates pool summary values for the metsResidualPools() unit test.
# Pool summaries in mhrpin.sas7bdat are taken from these WEMAP sites:
#   2004 ORSE04-R022 1 - normal wadeable reach
#   2004 SHB-0315    1 - wadeable reach with no slope information
#   2004 SHB-0395    1 - wadeable reach with 11 stations per transect.
#   2004 SHB-0395 1 missing incremnt - wadeable reach with no incremnt
#                        information, and also has 11 stations per transect.
#   2004 EPA01-0450  1 - wadeable reach with side channel transects
#   2004 WNVP99-REN1 1 - wadeable reach with too many missing values.
#   2002 WWYP99-0672 1 - normal boatable reach
#   2002 WWYP99-NPR2 1 - boatable reach with no slope information
#
# Note: Changes to previous calculations for EPA01-0450 due to elimination of
# side channels continues to propagate to the summary for poolID 1; poolar is
# changed from 0.00087755 to 0; poolen from 6.4 to 3.2; mindep from 0.0002742357
# to 0.168070168527498; xdep from 11.3098236 to 16.8070168527498; rpvdep from
# 15.9557231 to NA; rpmxdep from 22.5922236 to 16.8070168527498; medep from
# 11.3098236 to 16.8070168527498; dep25 from 0.02742357 to 16.8070168527498;
# and dep75 from 22.5922236 to 16.8070168527498.
#
# Note: SAS code to list WEMAP values use here is:
#   data rpin; set rtotal.mrrpin(where=(site_id='WMTP99-0601' and year=2002 and visit_no=1)); run;
#   proc print; run;
#   data _NULL_; set rpin;
#     format _numeric_ 20.8;
#     if (_N_-1)/5 = int((_N_-1)/5) then put ' ';
#     put poolen +(-1) ', ' @;
#   run;
#
# ARGUMENTS:
# uids      character vector of UIDs.  The returned dataframe will contain
#           values for the intersection of the specified UIDs and the available
#           UIDs.
{

  pchar <- rbind(data.frame('UID'='2004 ORSE04-R022 1', 'poolID'=c(1:17)
                              ,'poolar'= c(0.4344919404, 0.1553625, 0.6203625, 0.563625, 0.503625
                                          ,1.21515, 0.443625, 2.0199375, 0.077175, 0.0353625
                                          ,0.0353625, 0.0160875, 0.0653625, 5.8280625, 0.1854375
                                          ,0.0503625, 0.428625
                                          )
                              ,'poolen'= c(7.5, 1.5, 1.5, 6, 6
                                          ,10.5, 6, 15, 4.5, 1.5
                                          ,1.5, 3, 1.5, 21, 7.5
                                          ,1.5, 6
                                          )
                              ,'mindep'= c(3.7777823398, 10.3575, 41.3575, 5.43, 3.0725
                                          ,1.43, 2.3575, 4.43, 1.0725, 2.3575
                                          ,2.3575, 0.3575, 4.3575, 4.005, 0.715
                                          ,3.3575, 3.715
                                          )
                              ,'xdep'= c(9.2627823398, 10.3575, 41.3575, 9.39375, 8.39375
                                        ,11.572857143, 7.39375, 13.46625, 1.715, 2.3575
                                        ,2.3575, 0.53625, 4.3575, 27.752678571, 2.4725
                                        ,3.3575, 7.14375
                                        )
                              ,'rpvdep'= c(6.6717325804, NA, NA, 4.1716915534, 5.5911844675
                                          ,9.1259768231, 4.3997739052, 6.5750497727, 0.8776709235, NA
                                          ,NA, 0.2527906743, NA, 18.089078714, 1.123394688
                                          ,NA, 3.2311778683
                                          )
                              ,'rpmxdep'= c(17.34778234, 10.3575, 41.3575, 14.0725, 14.715
                                           ,26.145, 13.0725, 24.715, 2.715, 2.3575
                                           ,2.3575, 0.715, 4.3575, 56.7875, 3.43
                                           ,3.3575, 11.43
                                           )
                              ,'meddep'= c(5.0627823398, 10.3575, 41.3575, 9.03625, 7.89375
                                          ,7.5025, 7.0725, 10.86, 1.3575, 2.3575
                                          ,2.3575, 0.53625, 4.3575, 25.10875, 2.7875
                                          ,3.3575, 6.715
                                          )
                              ,'dep25'= c(4.4202823398, 10.3575, 41.3575, 5.89375, 3.715
                                         ,3.7875, 4.53625, 8.86, 1.0725, 2.3575
                                         ,2.3575, 0.3575, 4.3575, 11.9325, 2.0725
                                         ,3.3575, 4.89375
                                         )
                              ,'dep75'= c(15.70528234, 10.3575, 41.3575, 12.89375, 13.0725
                                         ,20.3575, 10.25125, 18.0725, 2.715, 2.3575
                                         ,2.3575, 0.715, 4.3575, 46.43, 3.3575
                                         ,3.3575, 9.39375
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ,data.frame('UID'='2004 SHB-0315 1', 'poolID'=c(1:45)
                              ,'poolar'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'poolen'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'mindep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'xdep'= c(NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        )
                              ,'rpvdep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'rpmxdep'= c(NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           )
                              ,'meddep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'dep25'= c(NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         )
                              ,'dep75'= c(NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ,data.frame('UID'='2004 SHB-0395 1', 'poolID'=c(1:25)
                              ,'poolar'= c(2.35272066, 1.17302975, 0.24919174, 0.24919174, 0.15464628
                                          ,15.6282777, 0.0128281, 0.70030248, 0.0128281, 0.08575702
                                          ,0.5328281, 0.0128281, 0.0128281, 1.61060331, 0.10737355
                                          ,3.20100826, 3.9060595, 1.95100992, 2.77151405, 8.16726612
                                          ,1.62010083, 0.97515041, 0.7860595, 1.0784843, 0.34373719
                                          )
                              ,'poolen'= c(37.8181818, 9.45454545, 4.72727273, 4.72727273, 4.72727273
                                          ,33.0909091, 4.72727273, 9.45454545, 4.72727273, 9.45454545
                                          ,4.72727273, 4.72727273, 4.72727273, 23.6363636, 4.72727273
                                          ,18.9090909, 14.1818182, 4.72727273, 14.1818182, 37.8181818
                                          ,4.72727273, 14.1818182, 14.1818182, 9.45454545, 4.72727273
                                          )
                              ,'mindep'= c(0.54272727, 3.54272727, 5.27136364, 5.27136364, 3.27136364
                                          ,8.27136364, 0.27136364, 3.27136364, 0.27136364, 0.54272727
                                          ,11.2713636, 0.27136364, 0.27136364, 0.35681818, 2.27136364
                                          ,2.81409091, 4.81409091, 41.2713636, 9.27136364, 10.1709091
                                          ,34.2713636, 3.81409091, 3.27136364, 8.27136364, 7.27136364
                                          )
                              ,'xdep'= c(6.22113636, 12.4070455, 5.27136364, 5.27136364, 3.27136364
                                        ,47.2283117, 0.27136364, 7.40704545, 0.27136364, 0.90704545
                                        ,11.2713636, 0.27136364, 0.27136364, 6.81409091, 2.27136364
                                        ,16.9284091, 27.5427273, 41.2713636, 19.5427273, 21.5961364
                                        ,34.2713636, 6.87606061, 5.54272727, 11.4070455, 7.27136364
                                        )
                              ,'rpvdep'= c(4.14139831, 12.536039, NA, NA, NA
                                          ,35.9527529, NA, 5.84873732, NA, 0.51522371
                                          ,NA, NA, NA, 4.37594199, NA
                                          ,19.3663603, 25.1379738, NA, 15.0466095, 14.3585672
                                          ,NA, 4.68665113, 2.1485729, 4.43452375, NA
                                          )
                              ,'rpmxdep'= c(11.8995455, 21.2713636, 5.27136364, 5.27136364, 3.27136364
                                           ,101.085455, 0.27136364, 11.5427273, 0.27136364, 1.27136364
                                           ,11.2713636, 0.27136364, 0.27136364, 11.8140909, 2.27136364
                                           ,45.2713636, 54.5427273, 41.2713636, 36.8140909, 52.6281818
                                           ,34.2713636, 12.2713636, 7.54272727, 14.5427273, 7.27136364
                                           )
                              ,'meddep'= c(5.72113636, 12.4070455, 5.27136364, 5.27136364, 3.27136364
                                          ,38.5427273, 0.27136364, 7.40704545, 0.27136364, 0.90704545
                                          ,11.2713636, 0.27136364, 0.27136364, 7.08545455, 2.27136364
                                          ,9.81409091, 23.2713636, 41.2713636, 12.5427273, 15.5427273
                                          ,34.2713636, 4.54272727, 5.81409091, 11.4070455, 7.27136364
                                          )
                              ,'dep25'= c(3.04272727, 3.54272727, 5.27136364, 5.27136364, 3.27136364
                                         ,11.8995455, 0.27136364, 3.27136364, 0.27136364, 0.54272727
                                         ,11.2713636, 0.27136364, 0.27136364, 5.27136364, 2.27136364
                                         ,4.67840909, 4.81409091, 41.2713636, 9.27136364, 11.7211364
                                         ,34.2713636, 3.81409091, 3.27136364, 8.27136364, 7.27136364
                                         )
                              ,'dep75'= c(9.89954545, 21.2713636, 5.27136364, 5.27136364, 3.27136364
                                         ,87.3568182, 0.27136364, 11.5427273, 0.27136364, 1.27136364
                                         ,11.2713636, 0.27136364, 0.27136364, 9.54272727, 2.27136364
                                         ,29.1784091, 54.5427273, 41.2713636, 36.8140909, 27.7211364
                                         ,34.2713636, 12.2713636, 7.54272727, 14.5427273, 7.27136364
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ,data.frame('UID'='2004 SHB-0395 1 missing incremnt'
                              ,'poolID'=c(1:45)
                              ,'poolar'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'poolen'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'mindep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'xdep'= c(NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        )
                              ,'rpvdep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'rpmxdep'= c(NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           )
                              ,'meddep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'dep25'= c(NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         )
                              ,'dep75'= c(NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ,data.frame('UID'='2004 EPA01-0450 1', 'poolID'=c(1:32)
                              ,'poolar'= c(0, 1.1395584, 0.2699264, 0.1377792, 0.3019264
                                          ,0.4299264, 3.4435584, 0.7179264, 0.0139264, 0.7819264
                                          ,0.0459264, 0.9057792, 1.4275584, 0.4257792, 1.616896
                                          ,3.0124544, 0.1739264, 1.6779264, 1.1297792, 0.0459264
                                          ,0.9739264, 0.2059264, 1.387264, 0.1739264, 0.2059264
                                          ,NA, 0.68768151, 0.2059264, 0.0459264, 0.6219264
                                          ,0.4939264, 0.4939264
                                          )
                              ,'poolen'= c(3.2, 9.6, 3.2, 6.4, 3.2
                                          ,3.2, 9.6, 3.2, 3.2, 3.2
                                          ,3.2, 6.4, 9.6, 6.4, 16
                                          ,19.2, 3.2, 3.2, 6.4, 3.2
                                          ,3.2, 3.2, 12.8, 3.2, 3.2
                                          ,12.8, 6.4, 3.2, 3.2, 3.2
                                          ,3.2, 3.2
                                          )
                              ,'mindep'= c(16.8070168527498, 10.3056, 8.4352, 0.4352, 9.4352
                                          ,13.4352, 26.3056, 22.4352, 0.4352, 24.4352
                                          ,1.4352, 12.8704, 6.4352, 4.8704, 1.4352
                                          ,2.8704, 5.4352, 52.4352, 2.8704, 1.4352
                                          ,30.4352, 6.4352, 7.7408, 5.4352, 6.4352
                                          ,NA, 0.46262357, 6.4352, 1.4352, 19.4352
                                          ,15.4352, 15.4352
                                          )
                              ,'xdep'= c(16.8070168527498, 11.8704, 8.4352, 2.1528, 9.4352
                                        ,13.4352, 35.8704, 22.4352, 0.4352, 24.4352
                                        ,1.4352, 14.1528, 14.8704, 6.6528, 10.1056
                                        ,15.6898667, 5.4352, 52.4352, 17.6528, 1.4352
                                        ,30.4352, 6.4352, 10.838, 5.4352, 6.4352
                                        ,NA, 10.7450236, 6.4352, 1.4352, 19.4352
                                        ,15.4352, 15.4352
                                        )
                              ,'rpvdep'= c(NA, 1.5648, NA, 2.42905321, NA
                                          ,NA, 10.3572486, NA, NA, NA
                                          ,NA, 1.81358747, 13.8632391, 2.52069425, 9.70455036
                                          ,17.6136891, NA, NA, 20.9054706, NA
                                          ,NA, NA, 2.75783703, NA, NA
                                          ,NA, 14.5415095, NA, NA, NA
                                          ,NA, NA
                                          )
                              ,'rpmxdep'= c(16.8070168527498, 13.4352, 8.4352, 3.8704, 9.4352
                                           ,13.4352, 46.8704, 22.4352, 0.4352, 24.4352
                                           ,1.4352, 15.4352, 30.8704, 8.4352, 21.3056
                                           ,50.4352, 5.4352, 52.4352, 32.4352, 1.4352
                                           ,30.4352, 6.4352, 13.4352, 5.4352, 6.4352
                                           ,NA, 21.0274236, 6.4352, 1.4352, 19.4352
                                           ,15.4352, 15.4352
                                           )
                              ,'meddep'= c(16.8070168527498, 11.8704, 8.4352, 2.1528, 9.4352
                                          ,13.4352, 34.4352, 22.4352, 0.4352, 24.4352
                                          ,1.4352, 14.1528, 7.3056, 6.6528, 6.176
                                          ,8.9584, 5.4352, 52.4352, 17.6528, 1.4352
                                          ,30.4352, 6.4352, 11.088, 5.4352, 6.4352
                                          ,NA, 10.7450236, 6.4352, 1.4352, 19.4352
                                          ,15.4352, 15.4352
                                          )
                              ,'dep25'= c(16.8070168527498, 10.3056, 8.4352, 0.4352, 9.4352
                                         ,13.4352, 26.3056, 22.4352, 0.4352, 24.4352
                                         ,1.4352, 12.8704, 6.4352, 4.8704, 1.8704
                                         ,6.3056, 5.4352, 52.4352, 2.8704, 1.4352
                                         ,30.4352, 6.4352, 8.5232, 5.4352, 6.4352
                                         ,NA, 0.46262357, 6.4352, 1.4352, 19.4352
                                         ,15.4352, 15.4352
                                         )
                              ,'dep75'= c(16.8070168527498, 13.4352, 8.4352, 3.8704, 9.4352
                                         ,13.4352, 46.8704, 22.4352, 0.4352, 24.4352
                                         ,1.4352, 15.4352, 30.8704, 8.4352, 19.7408
                                         ,16.6112, 5.4352, 52.4352, 32.4352, 1.4352
                                         ,30.4352, 6.4352, 13.1528, 5.4352, 6.4352
                                         ,NA, 21.0274236, 6.4352, 1.4352, 19.4352
                                         ,15.4352, 15.4352
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ,data.frame('UID'='2004 WNVP99-REN1 1', 'poolID'=c(1:45)
                              ,'poolar'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'poolen'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'mindep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'xdep'= c(NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        )
                              ,'rpvdep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'rpmxdep'= c(NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           )
                              ,'meddep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          )
                              ,'dep25'= c(NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         )
                              ,'dep75'= c(NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ,data.frame('UID'='2002 WWYP99-0672 1', 'poolID'=c(1:38)
                              ,'poolar'= c(6.19661125, 1.0913724, 166.617888, 24.902549, 0.78429241
                                          ,22.2828024, 2.35287723, 0.78429241, 0.78429241, 122.840021
                                          ,7.23939273, 28.7011873, 179.7029, 3.99987824, 106.104921
                                          ,9.89154824, 26.9451044, 4.10030764, 9.20817292, 3.02267292
                                          ,10.2858076, 81.2088264, 74.6232819, 16.5801439, 3.83494793
                                          ,22.4232638, 107.991976, 111.781915, 80.5771711, 1.11591211
                                          ,6.45576211, 110.983892, 0.81078777, 0.81078777, 13.0375583
                                          ,21.4703904, 81.643878, 1.29780127
                                          )
                              ,'poolen'= c(55.9553, 27.97765, 251.79885, 111.9106, 35.83085
                                          ,35.83085, 71.6617, 35.83085, 35.83085, 179.15425
                                          ,31.2091, 124.8364, 312.091, 29.45835, 206.20845
                                          ,29.45835, 88.37505, 30.9275, 61.855, 61.855
                                          ,30.9275, 123.71, 191.07235, 136.48025, 27.29605
                                          ,54.5921, 309.2955, 279.6834, 106.797, 26.69925
                                          ,26.69925, 238.80315, 35.35065, 35.35065, 70.7013
                                          ,77.82445, 233.6059, 42.4738
                                          )
                              ,'mindep'= c(0.03024653, 0.03900872, 0.03900872, 0.01702617, 0.02188875
                                          ,0.62188875, 0.02188875, 0.02188875, 0.02188875, 0.04377749
                                          ,0.23196416, 0.03196416, 0.11964162, 0.1357808, 0.15046558
                                          ,0.3357808, 0.20734239, 0.13257805, 0.03257805, 0.03257805
                                          ,0.33257805, 0.13257805, 0.08346228, 0.00247306, 0.14049461
                                          ,0.28098922, 0.02573581, 0.04849455, 0.24179564, 0.04179563
                                          ,0.24179563, 0.14179563, 0.02293558, 0.02293558, 0.14587117
                                          ,0.1303427, 0.05370356, 0.00740712
                                          )
                              ,'xdep'= c(0.11074217, 0.03900872, 0.66171028, 0.22252181, 0.02188875
                                        ,0.62188875, 0.03283312, 0.02188875, 0.02188875, 0.68566624
                                        ,0.23196416, 0.22991041, 0.57580289, 0.1357808, 0.51455176
                                        ,0.3357808, 0.30489493, 0.13257805, 0.14886708, 0.04886707
                                        ,0.33257805, 0.65644513, 0.39054987, 0.12148383, 0.14049461
                                        ,0.41074192, 0.3491547, 0.39497645, 0.75448909, 0.04179563
                                        ,0.24179563, 0.45345953, 0.02293558, 0.02293558, 0.18440337
                                        ,0.24330581, 0.34949408, 0.03055534
                                        )
                              ,'rpvdep'= c(0.11383802, NA, 0.35869874, 0.22270576, NA
                                          ,NA, 0.01547768, NA, NA, 0.71048322
                                          ,NA, 0.16399174, 0.30572029, NA, 0.31473596
                                          ,NA, 0.08635647, NA, 0.16445752, 0.02303616
                                          ,NA, 0.36866252, 0.221145, 0.13730342, NA
                                          ,0.18349802, 0.21804194, 0.27734607, 0.37146849, NA
                                          ,NA, 0.29187241, NA, NA, 0.05449277
                                          ,0.15727677, 0.17368356, 0.03273653
                                          )
                              ,'rpmxdep'= c(0.19123781, 0.03900872, 1.03405234, 0.47801745, 0.02188875
                                           ,0.62188875, 0.04377749, 0.02188875, 0.02188875, 1.60944374
                                           ,0.23196416, 0.39589249, 1.12374913, 0.1357808, 0.94312319
                                           ,0.3357808, 0.37156159, 0.13257805, 0.2651561, 0.0651561
                                           ,0.33257805, 0.9303122, 0.72148383, 0.34049461, 0.14049461
                                           ,0.54049461, 0.69772074, 0.82310765, 1.12538691, 0.04179563
                                           ,0.24179563, 0.85647355, 0.02293558, 0.02293558, 0.22293558
                                           ,0.42293558, 0.56851779, 0.05370356
                                           )
                              ,'meddep'= c(0.11074217, 0.03900872, 0.75603489, 0.19752181, 0.02188875
                                          ,0.62188875, 0.03283312, 0.02188875, 0.02188875, 0.26566624
                                          ,0.23196416, 0.24589249, 0.57374913, 0.1357808, 0.51468478
                                          ,0.3357808, 0.3357808, 0.13257805, 0.14886708, 0.04886707
                                          ,0.33257805, 0.78144513, 0.46197844, 0.08098922, 0.14049461
                                          ,0.41074192, 0.31172828, 0.40220983, 0.82538691, 0.04179563
                                          ,0.24179563, 0.2876668, 0.02293558, 0.02293558, 0.18440337
                                          ,0.17663914, 0.38333202, 0.03055534
                                          )
                              ,'dep25'= c(0.03024653, 0.03900872, 0.47801745, 0.03653053, 0.02188875
                                         ,0.62188875, 0.02188875, 0.02188875, 0.02188875, 0.22188875
                                         ,0.23196416, 0.09794624, 0.46392832, 0.1357808, 0.2357808
                                         ,0.3357808, 0.20734239, 0.13257805, 0.03257805, 0.03257805
                                         ,0.33257805, 0.39886708, 0.14296767, 0.02148383, 0.14049461
                                         ,0.28098922, 0.19316223, 0.10669892, 0.51269345, 0.04179563
                                         ,0.24179563, 0.21060238, 0.02293558, 0.02293558, 0.14587117
                                         ,0.1303427, 0.23703558, 0.00740712
                                         )
                              ,'dep75'= c(0.19123781, 0.03900872, 0.97306106, 0.40851308, 0.02188875
                                         ,0.62188875, 0.04377749, 0.02188875, 0.02188875, 1.28755499
                                         ,0.23196416, 0.36187457, 0.69589249, 0.1357808, 0.87890399
                                         ,0.3357808, 0.37156159, 0.13257805, 0.2651561, 0.0651561
                                         ,0.33257805, 0.91402318, 0.50247306, 0.16197844, 0.14049461
                                         ,0.54049461, 0.56286791, 0.59772074, 0.99628472, 0.04179563
                                         ,0.24179563, 0.73353797, 0.02293558, 0.02293558, 0.22293558
                                         ,0.42293558, 0.52222135, 0.05370356
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ,data.frame('UID'='2002 WWYP99-NPR2 1', 'poolID'=c(1:49)
                              ,'poolar'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA
                                          )
                              ,'poolen'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA
                                          )
                              ,'mindep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA
                                          )
                              ,'xdep'= c(NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA, NA
                                        ,NA, NA, NA, NA
                                        )
                              ,'rpvdep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA
                                          )
                              ,'rpmxdep'= c(NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA, NA
                                           ,NA, NA, NA, NA
                                           )
                              ,'meddep'= c(NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA, NA
                                          ,NA, NA, NA, NA
                                          )
                              ,'dep25'= c(NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA
                                         )
                              ,'dep75'= c(NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA, NA
                                         ,NA, NA, NA, NA
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ,data.frame(UID='2000 WCAP99-0604 1',poolID=c(1:29)
                              ,'poolar'= c(1.1986786708, 0.49575, 0.040575, 0.175575, 0.055575
                                          ,0.160575, 0.145575, 0.370575, 0.78345, 0.370575
                                          ,1.12845, 0.209025, 0.48345, 0.190575, 0.106725
                                          ,0.616725, 0.18075, 0.286725, 0.145575, 0.736725
                                          ,0.205575, 0.025575, 0.445575, 0.040575, 0.190575
                                          ,0.010575, 0.376725, 0.040575, 1.00845
                                          )
                              ,'poolen'= c(4.5, 6, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 4.5, 1.5
                                          ,4.5, 4.5, 4.5, 1.5, 3, 3, 6, 3, 1.5, 3
                                          ,1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 3, 1.5, 4.5
                                          )
                              ,'mindep'= c(3.898455693, 4.115, 2.705, 11.705, 3.705
                                          ,10.705, 9.705, 24.705, 9.115, 24.705
                                          ,4.705, 1.115, 4.115, 12.705, 2.705
                                          ,20.41, 0.41, 4.41, 9.705, 23.705
                                          ,13.705, 1.705, 29.705, 2.705, 12.705
                                          ,0.705, 10.705, 2.705, 13.705
                                          )
                              ,'xdep'= c(27.936789026, 8.2625, 2.705, 11.705, 3.705
                                        ,10.705, 9.705, 24.705, 17.41, 24.705
                                        ,25.076666667, 6.41, 10.743333333, 12.705, 3.5575
                                        ,20.5575, 3.0125, 9.5575, 9.705, 24.5575
                                        ,13.705, 1.705, 29.705, 2.705, 12.705
                                        ,0.705, 12.5575, 2.705, 22.41
                                        )
                              ,'rpvdep'= c(37.880672623, 4.7932287309, NA, NA, NA
                                          , NA, NA, NA, 10.862183252, NA
                                          ,29.905774665, 7.4882608128, 9.3279343015, NA, 1.2056170619
                                          ,0.2085965005, 1.905266823, 7.2796643123, NA, 1.2056170619
                                          ,NA, NA, NA, NA, NA
                                          ,NA, 2.6198306243, NA, 11.25153434
                                          )
                              ,'rpmxdep'= c(71.603455693, 14.41, 2.705, 11.705, 3.705
                                           ,10.705, 9.705, 24.705, 29.705, 24.705
                                           ,59.41, 11.705, 21.41, 12.705, 4.41
                                           ,20.705, 4.705, 14.705, 9.705, 25.41
                                           ,13.705, 1.705, 29.705, 2.705, 12.705
                                           ,0.705, 14.41, 2.705, 35.115
                                           )
                              ,'meddep'= c(8.308455693, 7.2625, 2.705, 11.705, 3.705
                                          ,10.705, 9.705, 24.705, 13.41, 24.705
                                          ,11.115, 6.41, 6.705, 12.705, 3.5575
                                          ,20.5575, 3.4675, 9.5575, 9.705, 24.5575
                                          ,13.705, 1.705, 29.705, 2.705, 12.705
                                          ,0.705, 12.5575, 2.705, 18.41
                                          )
                              ,'dep25'= c(3.898455693, 4.4675, 2.705, 11.705, 3.705
                                         ,10.705, 9.705, 24.705, 9.115, 24.705
                                         ,4.705, 1.115, 4.115, 12.705, 2.705
                                         ,20.41, 1.615, 4.41, 9.705, 23.705
                                         ,13.705, 1.705, 29.705, 2.705, 12.705
                                         ,0.705, 10.705, 2.705, 13.705
                                         )
                              ,'dep75'= c(71.603455693, 12.0575, 2.705, 11.705, 3.705
                                         ,10.705, 9.705, 24.705, 29.705, 24.705
                                         ,59.41, 11.705, 21.41, 12.705, 4.41
                                         ,20.705, 4.41, 14.705, 9.705, 25.41
                                         ,13.705, 1.705, 29.705, 2.705, 12.705
                                         ,0.705, 14.41, 2.705, 35.115
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ,data.frame(UID='2002 WMTP99-0601 1',poolID=c(1:30)
                              ,'poolar'= c(56.447219648, 3.7640311601, 1.2546770534, 5.8721570534, 42.672086896
                                          ,4.1583917586, 106.49885569, 25.329798741, 1.198511247, 46.25129989
                                          ,122.76586702, 1.6571014637, 17.913042302, 44.844759534, 57.123372941
                                          ,1.2595597282, 1.4037891847, 4.5830729428, 4.5830729428, 14.288827657
                                          ,38.338923994, 28.4352984, 125.18866808, 4.6322377849, 4.6322377849
                                          ,1.040755432, 4.6322377849, 1.040755432, 51.547481323, 16.562065984
                                          )
                              ,'poolen'= c(207.78660000, 46.17480000, 23.08740000, 23.08740000, 153.69575000
                                          ,92.21745000, 276.65235000, 62.09790000, 31.04895000, 248.39160000
                                          ,266.68894737, 38.09842105, 47.12650000, 212.06925000, 260.86660000
                                          ,23.74890000, 47.49780000, 66.04805000, 66.04805000, 198.14415000
                                          ,186.82510263, 90.58278947, 181.16557895, 35.91482353, 35.91482353
                                          ,35.91482353, 35.91482353, 35.91482353, 149.92421242, 81.43527778
                                          )
                              ,'mindep'= c(0.1395371317, 0.008689333, 0.0543446665, 0.2543446665, 0.0960666544
                                          ,0.0176399926, 0.1392133309, 0.2386007014, 0.0386007014, 0.0316042082
                                          ,0.0739811171, 0.0246603724, 0.0068073463, 0.0670183656, 0.078063198
                                          ,0.0530365502, 0.0060731005, 0.0693899811, 0.0693899811, 0.0387799623
                                          ,0.0096808257, 0.1208725338, 0.201454223, 0.1289784365, 0.1289784365
                                          ,0.0289784365, 0.1289784365, 0.0289784365, 0.0035248699, 0.0677923476
                                          )
                              ,'xdep'= c(0.271659576, 0.0815169997, 0.0543446665, 0.2543446665, 0.2776399926
                                        ,0.0450933284, 0.3849555433, 0.4079010521, 0.0386007014, 0.1862031562
                                        ,0.4603335392, 0.0434952793, 0.3801055097, 0.2114628101, 0.2189201724
                                        ,0.0530365502, 0.0295548254, 0.0693899811, 0.0693899811, 0.0721132956
                                        ,0.1899716703, 0.3139150225, 0.6910179561, 0.1289784365, 0.1289784365
                                        ,0.0289784365, 0.1289784365, 0.0289784365, 0.3912516532, 0.2033770429
                                        )
                              ,'rpvdep'= c(0.0904101697, 0.102993874, NA, NA, 0.1549243271
                                          ,0.0308169688, 0.2405003499, 0.2394268521, NA, 0.1566015964
                                          ,0.3908501951, 0.0266365808, 0.5279233255, 0.1189161159, 0.1067314629
                                          ,NA, 0.0332081738, NA, NA, 0.0347750586
                                          ,0.1303290806, 0.21168671, 0.4054155935, NA, NA
                                          ,NA, NA, NA, 0.3063271617, 0.1071891132
                                          )
                              ,'rpmxdep'= c(0.4308477987, 0.1543446665, 0.0543446665, 0.2543446665, 0.4568533235
                                           ,0.0784266618, 0.8352799852, 0.5772014027, 0.0386007014, 0.4930035069
                                           ,1.4233018618, 0.0623301862, 0.7534036731, 0.4806330581, 0.4128804468
                                           ,0.0530365502, 0.0530365502, 0.0693899811, 0.0693899811, 0.1081699434
                                           ,0.3499716703, 0.5402908446, 1.2208725338, 0.1289784365, 0.1289784365
                                           ,0.0289784365, 0.1289784365, 0.0289784365, 0.800147827, 0.3389617382
                                           )
                              ,'meddep'= c(0.2569157977, 0.0815169997, 0.0543446665, 0.2543446665, 0.3176399926
                                          ,0.0392133309, 0.413706647, 0.4079010521, 0.0386007014, 0.1272014028
                                          ,0.3116509309, 0.0434952793, 0.3801055097, 0.2068073463, 0.2189535473
                                          ,0.0530365502, 0.0295548254, 0.0693899811, 0.0693899811, 0.0693899811
                                          ,0.1902625149, 0.2805816892, 0.6507271115, 0.1289784365, 0.1289784365
                                          ,0.0289784365, 0.1289784365, 0.0289784365, 0.400147827, 0.2033770429
                                          )
                              ,'dep25'= c(0.2112604642, 0.008689333, 0.0543446665, 0.2543446665, 0.1392133309
                                         ,0.0176399926, 0.1784266617, 0.2386007014, 0.0386007014, 0.0930035069
                                         ,0.1869905586, 0.0246603724, 0.0068073463, 0.1534036731, 0.1310997483
                                         ,0.0530365502, 0.0060731005, 0.0693899811, 0.0693899811, 0.0387799623
                                         ,0.1305533595, 0.1208725338, 0.3417450676, 0.1289784365, 0.1289784365
                                         ,0.0289784365, 0.1289784365, 0.0289784365, 0.1128746103, 0.1355846953
                                         )
                              ,'dep75'= c(0.3025711312, 0.1543446665, 0.0543446665, 0.2543446665, 0.3784266618
                                         ,0.0784266618, 0.5568533235, 0.5772014027, 0.0386007014, 0.2623038576
                                         ,0.6479622342, 0.0623301862, 0.7534036731, 0.227229385, 0.2719900975
                                         ,0.0530365502, 0.0530365502, 0.0693899811, 0.0693899811, 0.1081699434
                                         ,0.2693899811, 0.5402908446, 1.0805816892, 0.1289784365, 0.1289784365
                                         ,0.0289784365, 0.1289784365, 0.0289784365, 0.650147827, 0.2711693906
                                         )
                              ,stringsAsFactors=FALSE
                              )
                ) # end of rbind()

  # Convert intermediate depth mets from cm to m in streams
  boatableReaches <- c('2002 WWYP99-0672 1', '2002 WWYP99-NPR2 1', '2002 WMTP99-0601 1')
  pchar$mindep <- ifelse(pchar$UID %in% boatableReaches
                        ,pchar$mindep
                        ,pchar$mindep / 100
                        )
  pchar$xdep <- ifelse(pchar$UID %in% boatableReaches
                      ,pchar$xdep
                      ,pchar$xdep / 100
                      )
  pchar$rpvdep <- ifelse(pchar$UID %in% boatableReaches
                        ,pchar$rpvdep
                        ,pchar$rpvdep / 100
                        )
  pchar$rpmxdep <- ifelse(pchar$UID %in% boatableReaches
                         ,pchar$rpmxdep
                         ,pchar$rpmxdep / 100
                         )
  pchar$meddep <- ifelse(pchar$UID %in% boatableReaches
                        ,pchar$meddep
                        ,pchar$meddep / 100
                        )
  pchar$dep25 <- ifelse(pchar$UID %in% boatableReaches
                       ,pchar$dep25
                       ,pchar$dep25 / 100
                       )
  pchar$dep75 <- ifelse(pchar$UID %in% boatableReaches
                       ,pchar$dep75
                       ,pchar$dep75 / 100
                       )

  # Drop information for reaches with no residual pools:
  pchar <- subset(pchar, !(UID %in% c('2004 SHB-0315 1'
                                     ,'2004 SHB-0395 1 missing incremnt'
                                     ,'2004 WNVP99-REN1 1'
                                     ,'2002 WWYP99-NPR2 1'
                                     )
                          )
                 )

  # Change column types
  pchar$poolID <- as.numeric(pchar$poolID)

  # return only requested UIDs
  if(!is.null(uids)) {
      pchar <- subset(pchar, UID %in% uids)
  }

  return(pchar)
}

metsResidualPools.createSiteSummaries <- function(uids)
# Creates dataframe of expected site summarys for metsResidualPoolsTest()
# Note: mets taken from WEMAP phabmet and rphabmet on server on 20-Jan-2010,
#   except for SHB-0395 was taken from m:/emap/data/wemap/streams/total where
#   the incremnt is not missing.
#
# Note: some values for UID='2004 EPA01-0450 1' have been changed from their
#   values calculated with SAS because those values include the side channel
#   pools, whereas NRSA does not (currently).  These values and the changes are
#   as follows:
#                 SAS/WEMAP value     value without side channels
#       rpgt05    26                  21
#       rpgt05x   0.199155864         0.209541439797349
#       rpgt10    18                  14
#       rpgt10x   0.256290915         0.278564731124595
#       rpgt20    11                  10
#       rpgt20x   0.322979316         0.334249823574433
#
# Note: Because side channels are not used in the calculations, the nickpoint
# was changed, and that change continues to propagate to the summary metrics:
# rpxlen changed from 6.144 to 6.016; rpvlen changed from 4.42248799 to
# 4.46091171548298; totplen changed from 153.6 to 150.4; rpxdep changed from
# 13.9122426 to 14.0845748266543; rpvdep changed from 13.0591835 to
# 12.9835518984015; rpgt05x changed from 20.9541439797349 to 20.678657945369;
# rpgt10x changed from 27.8564731124595 to 27.4432440609107; rpgt20 changed
# from 10 to 9; rpgt20x changed from 33.4249823574433 to 34.6286222222222.
#
# Note: Due to an apparent bug in the residual pool calculations in SAS, the
#   reachlengths calculated for rivers are one station incremnt longer than
#   they should be.  This is a minor problem, but it interferes with the
#   unit test and thus is fixed here for UID=2002 WWYP99-0672 1: rp100 is
#   changed from 24.8300518 to 24.9191689642238.
# Note: Because of truncation during calculation, some expected values in this
#   test were modified: in '2002 WMTP99-0601 1', rpmxlen was changed from
#   276.652 to 276.651; totplen was changed from 3260.08 to 3260.09221775026;
#   rp100 changed from 14.4713 to 14.511964488024.  Also, '2000 WCAP99-0604 1'
#   value rp100 was changed from 6.68355 to 6.74972189491067.  This allows us
#   to keep the zeroFudge factor at 1e-3 instead of 1e-1.
#
# ARGUMENTS:
# uids      character vector of UIDs.  The returned dataframe will contain
#           values for the intersection of the specified UIDs and the available
#           UIDs.
#
{
  schar <- data.frame('UID'=c('2004 EPA01-0450 1','2004 ORSE04-R022 1'
                             ,'2004 SHB-0315 1','2004 SHB-0395 1'
                             ,'2004 SHB-0395 1 missing incremnt','2004 WNVP99-REN1 1'
                             ,'2002 WWYP99-0672 1','2002 WWYP99-NPR2 1'
                             ,'2000 WCAP99-0604 1', '2002 WMTP99-0601 1'
                             )
                     ,'rpxlen'=c(6.016, 6, NA, 12.1018
                                ,NA, NA, 101.721539, NA
                                ,2.68966, 108.669
                                )
                     ,'rpvlen'=c(4.46091171548298, 5.38226254, NA, 10.4891
                                ,NA, NA, 88.9036817, NA
                                ,1.52018, 86.5821
                                )
                     ,'rpmxlen'=c(19.2, 21, NA, 37.8182
                                 ,NA, NA, 312.091, NA
                                 ,6, 276.651
                                 )
                     ,'totplen'=c(150.4, 102, NA, 302.545
                                 ,NA, NA, 3865.4185, NA
                                 ,78, 3260.09221775026
                                 )
                     ,'rpxdep'=c(14.0845748266543, 12.6845428, NA, 15.7648
                                ,NA, NA, 0.37834952, NA
                                ,13.4217, 0.27688
                                )
                     ,'rpvdep'=c(12.9835518984015, 13.1246311, NA, 19.9576
                                ,NA, NA, 0.31961106, NA
                                ,13.6985, 0.26112
                                )
                     ,'rpmxdep'=c(52.4352, 56.7875, NA, 101.085
                                 ,NA, NA, 1.60944374, NA
                                 ,71.6035, 1.42330
                                 )
                     ,'rpgt50'=c(2, 1, NA, 3, NA, NA, 13, NA, 2, 7)
                     ,'rpgt75'=c(0, 0, NA, 1, NA, NA, 8, NA, 0, 5)
                     ,'rpgt100'=c(0, 0, NA, 1, NA, NA, 4, NA, 0, 2)
                     ,'rpgt05'=c(21, 10, NA, 18, NA, NA, 30, NA, 21, 27)
                     ,'rpgt05x'=c(20.678657945369, 23.0000282, NA, 26.9919
                                 ,NA, NA, 0.53179391, NA, 22.7923, 0.38731
                                 )
                     ,'rpgt10'=c(14, 10, NA, 14, NA, NA, 28, NA, 19, 20)
                     ,'rpgt10x'=c(27.4432440609107, 23.0000282, NA, 32.8927
                                 ,NA, NA, 0.5655342, NA, 24.1699, 0.50087
                                 )
                     ,'rpgt20'=c(9, 4, NA, 8, NA, NA, 24, NA,10, 15)
                     ,'rpgt20x'=c(34.6286222222222, 37.25125, NA, 48.3945
                                 ,NA, NA, 0.6347861, NA, 34.2473, 0.62453
                                 )
                     ,'rpxarea'=c(0.82585014, 0.7457657, NA, 1.90782
                                 ,NA, NA, 38.8022944, NA, 0.35261, 27.9973
                                 )
                     ,'rpvarea'=c(0.9000954, 1.40685232, NA, 3.37186
                                 ,NA, NA, 50.8180096, NA, 0.33517, 35.9842
                                 )
                     ,'rpmxar'=c(3.4435584, 5.8280625, NA, 15.6283
                                ,NA, NA, 179.7029, NA, 1.19868, 125.189
                                )
                     ,'areasum'=c(20.6462536, 12.6780169, NA, 47.6956
                                 ,NA, NA, 1474.48719, NA, 10.2258, 839.920
                                 )
                     ,'rp100'=c(6.51712549, 8.53738515, NA, 9.25638449
                               ,NA, NA, 24.9191689642238, NA, 6.74972189491067, 14.511964488024
                               )
                     ,stringsAsFactors=FALSE
                     )

  if(FALSE) {
  # Convert depth summaries from cm to m in streams
  schar$rpxdep <- ifelse(schar$UID %in% c('2002 WWYP99-0672 1'
                                          ,'2002 WWYP99-NPR2 1')
                         ,schar$rpxdep
                         ,schar$rpxdep / 100
                         )
  schar$rpvdep <- ifelse(schar$UID %in% c('2002 WWYP99-0672 1'
                                         ,'2002 WWYP99-NPR2 1')
                        ,schar$rpvdep
                        ,schar$rpvdep / 100
                        )
  schar$rpmxdep <- ifelse(schar$UID %in% c('2002 WWYP99-0672 1'
                                          ,'2002 WWYP99-NPR2 1')
                         ,schar$rpmxdep
                         ,schar$rpmxdep / 100
                         )
  schar$rpgt05x <- ifelse(schar$UID %in% c('2002 WWYP99-0672 1'
                                        ,'2002 WWYP99-NPR2 1')
                       ,schar$rpgt05x
                       ,schar$rpgt05x / 100
                       )
  schar$rpgt10x <- ifelse(schar$UID %in% c('2002 WWYP99-0672 1'
                                        ,'2002 WWYP99-NPR2 1')
                       ,schar$rpgt10x
                       ,schar$rpgt10x / 100
                       )
  schar$rpgt20x <- ifelse(schar$UID %in% c('2002 WWYP99-0672 1'
                                        ,'2002 WWYP99-NPR2 1')
                       ,schar$rpgt20x
                       ,schar$rpgt20x / 100
                       )
  }
  
  # Change column types
  schar$rpgt50 <- as.integer(schar$rpgt50)
  schar$rpgt75 <- as.integer(schar$rpgt75)
  schar$rpgt100 <- as.integer(schar$rpgt100)
  schar$rpgt05 <- as.integer(schar$rpgt05)
  schar$rpgt10 <- as.integer(schar$rpgt10)
  schar$rpgt20 <- as.integer(schar$rpgt20)

  if(!is.null(uids)) {
      schar <- subset(schar, UID %in% uids)
  }

  return(schar)

}

# end of file