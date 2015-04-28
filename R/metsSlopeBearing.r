# metsSlopeBearing.r
#
# 02/01/10 cws created.
# 03/10/10 cws Updated as needed for change in nWadeableStationsPerTransect().
# 03/11/10 cws call to writeNRSACalcResults() corrected.
# 03/22/10 cws Moved unit test dataframes to separate functions
#  3/25/10 cws Changed diff() calls to dfCompare(), nlaLengthen() to dfLengthen().
# 04/09/10 ssr Modified unit test to include stream only and boatable only options.
#          Modified metsSlopeBearing.1 to allow stream or river only options.
#  4/22/10 cws cleaned up unit test and added case for site with slopes measured
#          as changes in elevation.  Updated calculations to handle these sites.
#  5/27/10 cws Updated code to use INCREMNT occuring only at A 0, reflecting
#          the current data organization.  Modified unit test accordingly.
# 09/16/10 cws Removing hardcoding of NRSA database name, using NRSAdbName
#          instead.
# 10/08/10 cws Correcting use of LINE column in boatable data. Changed test
#          data accordingly (LINE=1,2,3 became LINE=999,1,2)
# 11/09/10 cws Removed check for small sample sizes (N<=2) for slopes which
#          caused xslope and vslope to be set to NA in these cases.  This was
#          done to allow use of GPS-based xslope estimations, which have only
#          one value per site.  Unit test changed accordingly.
# 03/18/11 cws Changing default slope units from PERCENT to CM when slope units
#          are NONE in wadeable protocol sites; boatable sites still assume
#          PERCENT slopes unless specified in CM.  In cases where the protocol
#          is unknown (NONE), they are handled as if wadeable.  Elevation
#          changes (SLOPE values when units are CM) are adjusted to be
#          split proportionately over the transect when they are not recorded
#          for subsightings.  This change was made to work with a modification
#          to the field practice when recording elevations when using a water
#          tube (crews recorded the elevation for the entire transect rather
#          than for each subsighting).  Unit test updated to test elevation
#          adjustment.  Functions providing test data to the unit test were
#          rewritten to make modifications to the data easier.
# 08/17/11 cws Incorporating GIS based calculations of sinuosity and slope.
#          Updated unit test accordingly.
# 11/29/11 cws Incorporation of GIS based calculations assumed all metrics 
#          would be calculated for all UIDs in the calculations which isn't 
#          true, and unit test used same method of incorporating GIS 
#          calculation results, occluding the error.  Modified code and 
#          unit test accordingly.
# 12/19/11 cws Restandardized names of individual metrics files, removing 
#          WITHGPSSLOPES and the like.
#  3/08/12 cws Reading GPS based calculations from gpsBasedCalculations_asOf201203008.csv
#  3/27/12 cws Adding metrics xslope_field, xslope_map and pctClinometer as requested
#          Now xslope is the prefered choice of field and map values (if both exist);
#          vslope is unchanged.  The value of sinu still takes on the map value
#          over the field value if both values exist.  Added test data to test ability to 
#          correctly select which xslope value to use.
#  6/02/14 cws Modified creation of slopeMethods dataframe (used for determining
#          pctClinometer) to allow for case when slope data is present for ALL sites 
#          in a study (e.g. Calapooia). No change to unit test.
#

require(RODBC)
require(RUnit)

metsSlopeBearing <- function()
# Calculates NRSA channel slope and bearing metrics:
#
# These metrics are saved to a csv file in the directory specified by
# NRSAMetricsLocation.
#
# Returns NULL on success or a character string describing the problem if one
# occurs.
#
# ARGUMENTS:
# none
{
  chan <- odbcConnect(NRSAdbName)

  thal <- fetchNRSATable(chan, 'tblTHALWEG2')
  if(is.character(thal)) return(thal)

  chanGeom <- fetchNRSATable(chan, 'tblCHANNELGEOMETRY2')
  if(is.character(chanGeom)) return(chanGeom)
  
  gisCalcs <- readNRSACalculationResults('gpsBasedCalculations_asOf201203008.csv')
  if(is.character(gisCalcs)) {
      gisCalcs <- NULL
  } else if(!setequal(names(gisCalcs), c('UID','METRIC','RESULT'))) {
      return("Error: GPS calculation file not in expected format.")
  } else if(any(unique(gisCalcs$METRIC) != tolower(unique(gisCalcs$METRIC)))) {
      return("Error: GPS calculation file metrics names should be in lower case.")
  }
  
  protocols <- siteProtocol(c(unique(chanGeom$UID), unique(thal$UID), unique(gisCalcs$UID)))

  mets <- metsSlopeBearing.1(thal, chanGeom, gisCalcs, protocols)
  if(is.character(mets)) return(mets)

  rc <- writeNRSACalcResults(mets, 'metsSlopeBearing.csv')
  return(rc)
}


metsSlopeBearing.1 <- function(thal, chanGeom, gisCalcs, protocols)
# Does the work for for metsSlopeBearing
#
# ARGUMENTS:
# thal        dataframe with thalweg data table
# chanGeom    dataframe with channel geometry table
# gisCalcs    dataframe with GIS based calculations of slope and sinuosity (in long
#               UID,PARAMETER,RESULT format, or NULL if these calculations do not exist.
# protocols   dataframe with protocol (WADEABLE, BOATABLE) used for each UID
#
# rm(dists,sbWadeable,sbBoatable,sb,nsta,tt,sumDists,tsb,transpc,tranEast, tranNorth, tranSlope, tran,totEast, totNorth, fishDist, nEast, nNorth, reach, xslope, vslope, nslp, reach, xbearing, sinu, mets)
{
  intermediateMessage('Slope and bearing calculations', loc='start')

  # Get expected parameters from each dataframe for main channel only.  Calculate
  # transect spacing in wadeables (like ACTRANSP in boatable reaches) as the
  # expected number of stations in the transect times INCREMNT (the distance
  # between adjacent stations) in that transect.  Calculate backsighting
  # percentages for the boatable reaches based on the backsighted distances.
  # The boatable parameter ACTRANSP is ignored in favour of the distances over
  # which backsightings are made.
  dists <- subset(thal
                 ,PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0
                  & UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID
                 ,select=c(UID,PARAMETER,RESULT)
                 )

  sbWadeable <- subset(chanGeom
                      ,PARAMETER %in% c('BEARING','BEARING2','BEARING3'
                                       ,'PROP','PROP2','PROP3'
                                       ,'SLOPE','SLOPE2','SLOPE3'
                                       )
                       & TRANSECT %in% LETTERS[1:11]
                       & UID %in% subset(protocols, PROTOCOL=='WADEABLE')$UID
                      ,select=c(UID,TRANSECT,PARAMETER,RESULT,UNITS)
                      )

  sbBoatable <- subset(chanGeom
                      ,PARAMETER %in% c('BEAR','SLOPE','DISTANCE')
                       & TRANSECT %in% LETTERS[1:11]
                       & UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID
                      ,select=c(UID,TRANSECT,LINE,PARAMETER,RESULT,UNITS)
                      )
  sbBoatable$LINE <- as.numeric(as.character(sbBoatable$LINE))
  sbBoatable$RESULT <- as.numeric(as.character(sbBoatable$RESULT))

  intermediateMessage('.1')

  #########################################################################
  # Organize for wadeable and boatable reaches into a single structure with the
  # following columns:
  # UID         Unique Identifier
  # TRANSECT    A-K
  # LINE        a numeric value = 0 for the main reading, 1, 2 or 3 thereafter.
  #               This value will be NA for TRANSPC, which is not associated
  #               with a specific line on the form.
  # PARAMETER   with values BEARING, SLOPE, PROPORTION, TRANSPC
  # RESULT      The numeric value of the measured parameter.
  # UNITS       The measurement units, CM or PERCENT for slopes, NONE for others.
  
  # Calculate transect spacing TRANSPC in wadeable reaches.  Include TRANSPC
  # as a parameter in the wadeable data. Fill in LINE and standardize PARAMETER
  # values.
  dists$RESULT <- as.numeric(as.character(dists$RESULT))
  newDists <- NULL
  if (nrow(dists)>0){
      # Calculate transect spacing for wadeable sites
      nsta <- nWadeableStationsPerTransect(thal)
      newDists <- merge(dists, nsta, by='UID', all.x=TRUE)

      newDists$TRANSPC <- as.character(newDists$nSta * as.numeric(newDists$RESULT))
      newDists <- newDists[c('UID','TRANSECT','TRANSPC')]
  }
  
  # Create LINE value based on PARAMETER, then standardize PARAMETER values
  sbWadeable$LINE <- ifelse(sbWadeable$PARAMETER %in% c('PROP','SLOPE','BEARING'), 0
                    ,ifelse(sbWadeable$PARAMETER %in% c('PROP2','SLOPE2','BEARING2'), 1
                    ,ifelse(sbWadeable$PARAMETER %in% c('PROP3','SLOPE3','BEARING3'), 2
                    ,NA
                    )))
  sbWadeable$PARAMETER <- ifelse(substr(sbWadeable$PARAMETER,1,4) == 'PROP', 'PROPORTION'
                         ,ifelse(substr(sbWadeable$PARAMETER,1,4) == 'SLOP', 'SLOPE'
                         ,ifelse(substr(sbWadeable$PARAMETER,1,4) == 'BEAR', 'BEARING'
                         ,NA
                         )))
  intermediateMessage('.2')
  

  # Calculate transect spacing TRANSPC from incremental DISTANCE values.
  # Calculate incremental proportion values from DISTANCE and TRANSPC.
  # Handle TRANSPC as a parameter in a separate dataframe.  Boatable reaches
  # use LINE==999 at the transect, and 1,2... for partial bearings, so change
  # at-transect LINE values from 999 to 1 and increment other values.
  tt <- subset(sbBoatable, PARAMETER=='DISTANCE')
  sumDists <- NULL
  if (nrow(tt)>0){
      sumDists <- aggregate(list('transpc'=tt$RESULT)
                           ,list('UID'=tt$UID, 'TRANSECT'=tt$TRANSECT)
                           ,sum, na.rm=TRUE
                           )

      sbBoatable <- merge(sbBoatable, sumDists, c('UID', 'TRANSECT'))

      sbBoatable$RESULT <- ifelse(sbBoatable$PARAMETER=='DISTANCE'
                                 ,100 * sbBoatable$RESULT/sbBoatable$transpc
                                 ,sbBoatable$RESULT
                                 )
      sbBoatable$transpc <- NULL

      sumDists$TRANSPC <- as.character(sumDists$transpc)
      sumDists$transpc <- NULL

      sbBoatable$PARAMETER <- ifelse(sbBoatable$PARAMETER=='DISTANCE'
                                    ,'PROPORTION'
                                    ,sbBoatable$PARAMETER
                                    )
      sbBoatable$PARAMETER <- ifelse(sbBoatable$PARAMETER=='BEAR'
                                    ,'BEARING'
                                    ,sbBoatable$PARAMETER
                                    )
      sbBoatable$LINE <- ifelse(sbBoatable$LINE==999, 0, sbBoatable$LINE + 1)
      
      sumDists <- sumDists[c('UID','TRANSECT','TRANSPC')]
  }

  tsb <- rbind(sbWadeable[c('UID','TRANSECT','LINE','PARAMETER','RESULT','UNITS')]
              ,sbBoatable[c('UID','TRANSECT','LINE','PARAMETER','RESULT','UNITS')]
              )

  transpc <- rbind(newDists, sumDists)

  intermediateMessage('.3')

  # Transpose to wide format for intermediate calculations at each transect
  # Put TRANSPC values on every row, regardless of LINE value.
  # NOTE: Boatable slopes (SLOPE_ND at least) have units = 'NONE', but are
  # expressed in percent, while wadeable slopes are either CM, PERCENT or NONE
  # if not marked; the default units for NONE are different for the two
  # protocols. In the case where we do not know what protocol was set to NONE,
  # assume it is a wadeable reach for now.
  sb <- reshape(tsb
               ,idvar=c('UID','TRANSECT','LINE')
               ,direction='wide'
               ,timevar='PARAMETER'
               )
  sb <- rename(sb, names(sb), sub('RESULT\\.(\\1)', '\\1', names(sb)))
  sb <- merge(sb, transpc, by=c('UID','TRANSECT'), all.x=TRUE)
  sb$BEARING <- as.numeric(sb$BEARING)
  sb$PROPORTION <- as.numeric(sb$PROPORTION)
  sb$TRANSPC <- as.numeric(sb$TRANSPC)

   # Handle changes to elevation data recording in the wadeable protocol.
   # Separate out wadeables, adjust elevations as needed, and recombine with
   # boatables.
   sb.w <- subset(sb, UID %in% subset(protocols
                                     ,PROTOCOL %in% c('WADEABLE','NONE')
                                     )$UID
                 )
   if(nrow(sb.w) > 0) {
       sb.w <- metsSlopeBearing.adjustElevations(sb.w)
       sb <- rbind(sb.w
                  ,subset(sb, UID %nin% subset(protocols
                                     ,PROTOCOL %in% c('WADEABLE','NONE')
                                     )$UID
                         )
                  )
       rm(sb.w)
   }

  # Convert mix of slopes in cm and percent to all percent.
  sb$SLOPE <-  ifelse(sb$UID %in% subset(protocols, PROTOCOL %in% c('WADEABLE','NONE'))$UID
                     ,ifelse(sb$UNITS.SLOPE %in% c('PERCENT')
                            ,as.numeric(sb$SLOPE)
                            ,ifelse(sb$UNITS.SLOPE %in% c('CM','NONE')
                                   ,(360/(2*pi))*tan(as.numeric(sb$SLOPE)/(sb$TRANSPC*100 * sb$PROPORTION/100))
                                   ,NA
                                   )
                            )
              ,ifelse(sb$UID %in% subset(protocols, PROTOCOL=='BOATABLE')$UID
                     ,ifelse(sb$UNITS.SLOPE %in% c('PERCENT','NONE')
                            ,as.numeric(sb$SLOPE)
                            ,ifelse(sb$UNITS.SLOPE %in% c('CM')
                                   ,(360/(2*pi))*tan(as.numeric(sb$SLOPE)/(sb$TRANSPC*100 * sb$PROPORTION/100))
                                   ,NA
                                   )
                            )
                     ,as.numeric(sb$SLOPE) # no change when protocol is unknown
                     )
              )
  sb$UNITS.SLOPE <- ifelse(sb$UNITS.SLOPE == 'CM', 'PERCENT', sb$UNITS.SLOPE)
  intermediateMessage('.4')

  #########################################################################
  # Intermediate calculations over single transect
  # tranEast   distance East traveled from this transect to the next
  #              = sum( sin(BEARINGInRadians) * DISTANCE )
  # tranNorth  distance North traveled from this transect to the next
  #              = sum( cos(BEARINGInRadians) * DISTANCE )
  # transpc    distance along channel from this transect to the next one
  # tranSlope  mean slope between this transect and the next one, weighted by
  #              the proportions of the total distance between adjacent
  #              transects over which each slope measurement was taken.
  #              Transects which have no slope data have tranSlope set to NA;
  #              otherwise the NAs would sum to 0, dangitall.
  sb$lineEast <- sin(sb$BEARING * 2*pi/360) * sb$TRANSPC * sb$PROPORTION/100
  sb$lineNorth <- cos(sb$BEARING * 2*pi/360) * sb$TRANSPC * sb$PROPORTION/100
  sb$lineSlope <- sb$SLOPE * sb$PROPORTION/100

  tranEast <- aggregate(list('tranEast'=sb$lineEast)
                       ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                       ,sum, na.rm=TRUE
                       )
  tranNorth <- aggregate(list('tranNorth'=sb$lineNorth)
                        ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                        ,sum, na.rm=TRUE
                        )
  tranSlope <- merge(aggregate(list('tranSlope'=sb$lineSlope)
                              ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                              ,sum, na.rm=TRUE
                              )
                    ,aggregate(list('n'=sb$lineSlope)
                              ,list('UID'=sb$UID, 'TRANSECT'=sb$TRANSECT)
                              ,count
                              )
                    ,c('UID','TRANSECT')
                    )
  tranSlope$tranSlope <- ifelse(tranSlope$n==0, NA, tranSlope$tranSlope)
  tranSlope$n <- NULL

  transpc <- rename(subset(sb, LINE==0, select=c(UID, TRANSECT, TRANSPC))
                   ,'TRANSPC', 'transpc'
                   )
  intermediateMessage('.5')

  tran <- merge(merge(tranEast, tranNorth, c('UID','TRANSECT'), all=TRUE)
               ,merge(tranSlope, transpc, c('UID','TRANSECT'), all=TRUE)
               ,c('UID','TRANSECT'), all=TRUE
               )
   intermediateMessage('.6')

   
  #########################################################################
  # Intermediate calculations over entire reach
  # totEast   distance East travelled from start to end of reach
  #
  # totNorth  distance North travelled from start to end of reach
  # fishDist  distance along channel from start to end of reach
  #             = sum(transect spacing)
  #             = sum(actransp)
  # crowDist  straight line distance from start to end of reach
  #             = sqrt(totEast^2 + totNorth^2)
  # nEast     count of tranEast values used to remove meaningless values of
  #             fishDist and crowDist
  # nNorth    count of tranNorth values used to remove meaningless values of
  #             fishDist and crowDist
  totEast <- aggregate(list('totEast'=tran$tranEast)
                      ,list('UID'=tran$UID)
                      ,sum, na.rm=TRUE
                      )
  totNorth <- aggregate(list('totNorth'=tran$tranNorth)
                       ,list('UID'=tran$UID)
                       ,sum, na.rm=TRUE
                       )
  fishDist <- aggregate(list('fishDist'=tran$transpc)
                       ,list('UID'=tran$UID)
                       ,sum, na.rm=TRUE
                       )
  nEast <- aggregate(list('nEast'=tran$tranEast)
                    ,list('UID'=tran$UID)
                    ,count
                    )
  nNorth <- aggregate(list('nNorth'=tran$tranNorth)
                     ,list('UID'=tran$UID)
                     ,count
                     )
  intermediateMessage('.7')

  reach <- merge(totEast
                ,merge(totNorth, fishDist, 'UID', all=TRUE)
                ,'UID', all=TRUE
                )
  reach <- merge(reach
                ,merge(nEast, nNorth, 'UID', all=TRUE)
                ,'UID', all=TRUE
                )
  reach$crowDist <- ifelse(reach$nEast > 2 & reach$nNorth > 2
                          ,sqrt(reach$totEast^2 + reach$totNorth^2)
                          ,NA
                          )
  reach$fishDist <- ifelse(reach$nEast > 2 & reach$nNorth > 2
                          ,reach$fishDist
                          ,NA
                          )
  intermediateMessage('.8')

  #########################################################################
  # Metrics calculations
  # xslope_field    mean of tranSlope based on field values
  # xslope_map      mean slope calculated from a map (either by hand or by GIS)
  # xslope          Most reliable value of site slope at sites we have the choice.
  #                   Measurements made in the field by non-clinometer methods are most
  #                   prefered, next are clinometer-based measurements with means > 1.5%.
  #                   Sites which do not have either of these choices are assigned 
  #                   map-based values.
  # pctClinometer   Percent of field based slope values which were taken by clinometer.
  # vslope          std deviation of tranSlope
  # nslp            count of tranSlope
  # transpc         mean distance between transects
  # xbearing        overall bearing of reach based the inverse cosine of the ratio
  #                   totNorth/crowDist
  # sinu            sinuosity as the ratio fishDist/crowDist.  
  nslp <- aggregate(list('RESULT'=tran$tranSlope)
                   ,list('UID'=tran$UID)
                   ,count
                   )
  nslp$METRIC <- 'nslp'

  xslope_field <- aggregate(list('RESULT'=tran$tranSlope)
                     ,list('UID'=tran$UID)
                     ,mean, na.rm=TRUE
                     )
  xslope_field$METRIC <- 'xslope_field'

  tt <- subset(chanGeom
              ,PARAMETER %in% c('SLOPE','SLOPE2','SLOPE3')
               & TRANSECT %in% LETTERS[1:11]
              ,select=c(UID,TRANSECT,PARAMETER,METHOD)
              )
  
  noSlopeUIDs <- unique(subset(chanGeom,UID %nin% tt$UID)$UID)
  if(length(noSlopeUIDs) > 0) {
      noSlopeData <- data.frame(UID = noSlopeUIDs
                               ,TRANSECT = ''
                               ,PARAMETER = ''
                               ,METHOD = ''
                               ,stringsAsFactors = FALSE
                               )
  } else {
      noSlopeData <- NULL
  }
  
  slopeMethods <- rbind(tt          # Methods for slopes recorded in field
                       ,noSlopeData # Method for absent slopes, if any, so pctClinometer will have a value.
                       )
  pctClinometer <- aggregate(list(RESULT=slopeMethods$METHOD)
                            ,list(UID=slopeMethods$UID)
                            ,function(m) { 
                                # Assume everything not explicitely noted as CL is not CL
                                # including missing and NONE methods.  Thus if a site is
                                # always missing (and thus has a mean of NA), it is 0 % CL.
                                p <- 100*mean(toupper(m) == 'CL', na.rm=TRUE)
                                p <- ifelse(is.na(p), 0, p)
                                return(p)
                             }
                            )
  pctClinometer$METRIC='pctClinometer'

  if(is.null(gisCalcs)) {
      xslope_map <- subset(data.frame(UID=1, METRIC='two', RESULT=3), FALSE)
  } else {
      xslope_map <- within(subset(gisCalcs
                                 ,tolower(METRIC) == 'xslope' & 
                                  UID %in% unique(c(thal$UID, chanGeom$UID))
                                 )
                          ,METRIC <- paste(METRIC, '_map', sep='')
                          )
  }
  
  usingMapSlopes <- subset(xslope_map
                          ,(!is.na(RESULT) &                                   # replace unreliable field value
                            UID %in% subset(pctClinometer, RESULT > 20)$UID & 
                            UID %in% subset(xslope_field, RESULT < 1.5)$UID
                           ) | 
                           UID %nin% subset(xslope_field, !is.na(RESULT))$UID  # fill in missing field value
                          )$UID
  xslope <- within(rbind(subset(xslope_field, UID %nin% usingMapSlopes)
                        ,subset(xslope_map, UID %in% usingMapSlopes)
                        )
                  ,METRIC <- 'xslope'
                  )
  
  vslope <- aggregate(list('RESULT'=tran$tranSlope)
                     ,list('UID'=tran$UID)
                     ,sd, na.rm=TRUE
                     )
  vslope$METRIC <- 'vslope'

  transpc <- aggregate(list('RESULT'=tran$transpc)
                      ,list('UID'=tran$UID)
                      ,mean, na.rm=TRUE
                      )
  transpc$METRIC <- 'transpc'

  reach$RESULT <- ifelse(reach$totEast > 0
                          ,(360/(2*pi))*acos(reach$totNorth/reach$crowDist)
                          ,360 - (360/(2*pi))*acos(reach$totNorth/reach$crowDist)
                          )
  xbearing <- reach[c('UID','RESULT')]
  xbearing$METRIC <- 'xbearing'

  reach$RESULT <- ifelse(reach$crowDist == 0
                        ,NA
                        ,reach$fishDist / reach$crowDist
                        )
  field_sinu <- reach[c('UID','RESULT')]
  field_sinu$METRIC <- 'sinu'
  if(is.null(gisCalcs)) {
      sinu <- field_sinu
  } else {
      sinu <- rbind(subset(field_sinu, UID %nin% subset(gisCalcs, tolower(METRIC) == 'sinu')$UID )
                   ,subset(gisCalcs, tolower(METRIC) == 'sinu' & 
                                     UID %in% unique(c(thal$UID, chanGeom$UID))
                          )
                   )
  }
  
  mets <- rbind(xslope, xslope_map, xslope_field, pctClinometer, vslope, nslp
               ,transpc, xbearing, sinu
               )
  intermediateMessage('.9')

  
  #########################################################################
  # Clean up and convert NaN values to NA
  mets <- mets[c('UID','METRIC','RESULT')]
  mets$RESULT <- as.character(mets$RESULT)

  mets$RESULT <- ifelse(is.nan(as.numeric(mets$RESULT)), NA, mets$RESULT)

  # Normally, filter out metrics with too small of a sample size to be reliable,
  # but instead skip this step because they decided to estimate slope using GPS
  # coordinates so we only have one slope per site.
#  badSlopeUIDs <- unique(nslp[as.numeric(nslp$RESULT)<=2,]$UID)
#  if(length(badSlopeUIDs)>0) {
#      mets$RESULT <- ifelse(mets$METRIC=='xslope' & mets$UID %in% badSlopeUIDs
#                           ,NA
#                           ,mets$RESULT
#                           )
#      mets$RESULT <- ifelse(mets$METRIC=='vslope' & mets$UID %in% badSlopeUIDs
#                           ,NA
#                           ,mets$RESULT
#                           )
#  }

  intermediateMessage('.  Done.', loc='end')
  return(mets)
}

metsSlopeBearing.adjustElevations <- function(df)
# Adjusts slopes recorded as elevations to account for a change in the field
# protocol that didn't quite get mentioned to us until last week.  Crews
# recording slopes using the elevation method were told they could record
# the change in elevation for more than one supplemental sighting since the
# water tube goes around bends that would occlude our vision.  This is the
# reason for large numbers of transects in which we have missing supplemental
# slopes.
# This function spreads the recorded slope value along subsequent supplemental
# readings within a transect.  The return value is the input dataframe with
# the adjusted slope values in the SLOPE column.
#
# ARGUMENTS:
# df        dataframe with slope & bearing information, one row per subsighting,
#           with columns UID, TRANSECT, LINE, SLOPE, UNITS.SLOPE, PROPORTION,
#           UNITS.PROPORTION, BEARING, UNITS.BEARING, TRANSPC.
#
# ASSUMPTIONS:
# Only wadeable reach data are included in the dataframe.
#
{
  # Assign group membership for each 'sighting'.  A 'group' is the set of
  # rows/sightings for which an elevation was recorded.  It is defined as
  # follows:
  #   group[1] = 1 at the start of the data
  #   group[n] = 1 + group[n-1] when the transect changes OR
  #                                  the slope value is not missing
  #            = group[n-1]     when the slope is missing AND the transect
  #                                  has not changed.
  #
  #   (this assumes that the slope value is recorded at the start of each group)
  #
  # As an example (leaving out the UID column and a few others):
  #
  #   Tran  Line  Slope  Units  Prop 'Group' groupSlope groupUnits
  #   A     0     2      CM      100  1      2          CM
  #   B     0     3      CM      100  2      3          CM
  #   C     0     4      CM      30   3      4          CM
  #   C     1     NA     NA      70   3      4          CM
  #   D     0     1      CM      100  4      1          CM
  #   ...
  #
  # Group membership will be used to distribute the recorded elevation change
  # at the first line of the group through out all lines/sightings in the group.
  
  df <- df[order(df$UID, df$TRANSECT, df$LINE),]
  df$group <- as.integer(NA)
  for(i in 1: nrow(df)) {
      if(i==1) {
          df$group[i] <- 1
      } else {
          if(df$TRANSECT[i] != df$TRANSECT[i-1] | !is.na(df$SLOPE[i])) {
              df$group[i] <- 1 + df$group[i-1]
          } else {
              df$group[i] <- df$group[i-1]
          }
      }
  }

  # Put first SLOPE and UNITS.SLOPE of each group on every row in the group.
  df <- merge(df
             ,transform(subset(first(df, 'group', 'first.group'), first.group
                              ,select=c(group, SLOPE, UNITS.SLOPE)
                              )
                       ,groupSlope=as.numeric(SLOPE)
                       ,groupUnits=UNITS.SLOPE
                       ,SLOPE=NULL
                       ,UNITS.SLOPE=NULL
                       ,stringsAsFactors=FALSE
                       )
             ,by='group'
             )

  # Flag groups that require slope adjustments (groups of 1 do not, others may).
  df <- merge(df
             ,aggregate(list(groupAdjust=df$group)
                       ,list(group=df$group)
                       ,function(x) { ifelse(length(x)==1, FALSE, TRUE) }
                       )
             ,by='group'
             )
  
  # Make adjustments to the recorded slope when it's an elevation change, ignoring
  # slopes recorded directly in percent.  Using the example from the above
  # section:
  #               OLD    OLD                                       NEW    NEW
  #   Tran  Line  Slope  Units  Prop 'Group' groupSlope groupUnits Slope  Units
  #   A     0     2      CM      100  1      2          CM         2      CM
  #   B     0     3      CM      100  2      3          CM         3      CM
  #   C     0     4      CM      30   3      4          CM         1.2    CM
  #   C     1     NA     NA      70   3      4          CM         2.8    CM
  #   D     0     1      CM      100  4      1          CM         1      CM
  #   ...
  df$SLOPE <- ifelse(df$groupUnits=='PERCENT'
                    ,df$SLOPE
                    ,ifelse(df$groupAdjust
                           ,df$groupSlope * df$PROPORTION/100
                           ,df$SLOPE
                           )
                    )
  df$UNITS.SLOPE <- df$groupUnits
  
  df$groupSlope <- NULL
  df$groupUnits <- NULL
  df$groupAdjust <- NULL
  df$group <- NULL

  return(df)
}


metsSlopeBearingTest <- function()
# Unit test for metsSlopeBearing. Test data taken from WEMAP data on 2-Feb-2010,
# and is then transformed into the expected organization for NRSA data.
# 2000 WAZP99-0505 1                 Stream with no supplemental readings
# 2000 WAZP99-0569 1                 Stream with many supplemental readings
# 2003 WWYP99-0659 1                 Stream with slopes in cm
# 2003 WWYP99-0659 1 missing CM subsightings   Stream with slopes in cm
# 2003 WWYP99-0659 1 slope unit NONE Stream with slopes in cm, but UNITS=NONE
# 2000 WAZP99-0569 1 no incremnt     Stream with no incremnt information
# 2000 WAZP99-0569 1 no slopes       Stream with no slope information
# 2000 WAZP99-0569 1 only 2 slopes   Stream with insufficient slope information
# 2000 WIDP99-0556 1                 River with some supplemental readings
# 2000 WIDP99-0556 1 with NA slopes  River with all slope values missing
# 2000 WIDP99-0556 1 with absent slopes  River with no slope values at all
# 2000 WIDP99-0556 1 slope unit NONE River with some supplemental readings and
#                                      slope units are NONE
# 2000 WSDP99-0531 1                 River with lots of supplemental readings
#
# The expected metrics are obtained for these reaches as well, and modified as
# follows:
#   '2000 WAZP99-0569 1' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 only 2 slopes' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 no slopes' xbearing changed from 42.435 to 42.43476
#   '2000 WAZP99-0569 1 no incremnt' xbearing changed from 42.435 to NA and
#       sinu changed from 1.1251 to NA since lack of distance between stations
#       means the distances on which these metrics rely on are unavailable.
#   '2003 WWYP99-0659 1' xslope changed from 0.86733 to 0.588307961 and vslope
#       changed from 0.73286 to 0.371371587 to account for error in previous
#       calculations of slopes at transects with supplemental slopes measured as
#       elevation change.  These should be calculated as elev/(transpc * prop)
#       but instead were calculated as (elev/(transpc * prop))*prop, which were
#       then multiplied by the proportion again and summed to determine the
#       slope of the transect.  This lead to diminished mean slopes at each
#       transect.
#
{
  # Create fake input data.  Thalweg data initially has 1 incremnt at
  # each station, instead of once per site as expected by the calculation code,
  # for testing purposes.
  fakeThal_IncremntsAtEachUID <- metsSlopeBearing.makeThalweg()
  fakeThal <- subset(fakeThal_IncremntsAtEachUID
                    ,!(PARAMETER=='INCREMNT' & TRANSECT !='A' & STATION !=0)
                    )

  fakeChanGeom <- metsSlopeBearing.makeChannelGeometry()

  fakeGisCalcs <- metsSlopeBearing.makeGisCalcs()
  
  fakeProtocol <- metsSlopeBearing.makeProtocols()

  # Create expected results.
  expected <- metsSlopeBearing.makeExpectedResults()
  
                           
  # Test calculations with both wadeable and boatable data AND with extra incremnt values.
  results <- metsSlopeBearing.1(fakeThal_IncremntsAtEachUID, fakeChanGeom, NULL, fakeProtocol)
  results <- results[order(results$UID, results$METRIC),]
  results$RESULT <- as.numeric(results$RESULT)

  # Compare expected and actual results
  expected <- expected[order(expected$UID, expected$METRIC),]
  expected$RESULT <- as.numeric(expected$RESULT)
  errs <- dfCompare(expected, results, c('UID','METRIC'), zeroFudge=1e-4)
#  return(errs)
  checkEquals(NULL, errs
             ,"Error: Slope and bearing metrics are broken with both protocols and extra incremnt values in thalweg table"
             )

             
  # Test calculations with both wadeable and boatable data AND GIS values
  # Fold GIS results into expected values, removing unexpected site and overwriting
  # sinuosity values based on field values with those based on map calculations.
#  expectedWithGIS <- rbind(subset(expected, !(METRIC=='sinu' & 
#                                              UID %in% unique(subset(fakeGisCalcs, METRIC=='sinu')$UID)
#                                             )
#                                 )
#                          ,subset(fakeGisCalcs
#                                 ,UID %nin% c('An extra reach not in the main expected results','2000 WAZP99-0505 1') &
#                                  METRIC=='xslope'
#                                 )
#                          ,within(subset(fakeGisCalcs
#                                        ,UID != 'An extra reach not in the main expected results'
#                                        )
#                                 ,METRIC <- ifelse(METRIC == 'xslope', 'xslope_map', METRIC)
#                                 )
#                          )
  expectedWithGIS <- metsSlopeBearing.makeExpectedResultsWithGIS()
  
  results <- metsSlopeBearing.1(fakeThal, fakeChanGeom, fakeGisCalcs, fakeProtocol)
  results <- results[order(results$UID, results$METRIC),]
  results$RESULT <- as.numeric(results$RESULT)
    
  expectedWithGIS <- expectedWithGIS[order(expectedWithGIS$UID, expectedWithGIS$METRIC),]
  expectedWithGIS$RESULT <- as.numeric(expectedWithGIS$RESULT)
  errs <- dfCompare(expectedWithGIS, results, c('UID','METRIC'), zeroFudge=1e-4)
#    return(errs)
  checkEquals(NULL, errs
             ,"Error: Slope and bearing metrics are broken with GIS calculations, both protocols"
             )


  # Test calculations with both wadeable and boatable data AND with extra incremnt 
  # values AND GIS values
  results <- metsSlopeBearing.1(fakeThal_IncremntsAtEachUID, fakeChanGeom, fakeGisCalcs, fakeProtocol)
  results <- results[order(results$UID, results$METRIC),]
  results$RESULT <- as.numeric(results$RESULT)
    
  expectedWithGIS <- expectedWithGIS[order(expectedWithGIS$UID, expectedWithGIS$METRIC),]
  expectedWithGIS$RESULT <- as.numeric(expectedWithGIS$RESULT)
  errs <- dfCompare(expectedWithGIS, results, c('UID','METRIC'), zeroFudge=1e-4)
#    return(errs)
  checkEquals(NULL, errs
             ,"Error: Slope and bearing metrics are broken with GIS calculations, both protocols and extra incremnt values in thalweg table"
             )


  # Test calculation with both wadeable and boatable reach data and no GIS 
  # based calculations or extra incremnt values.
  results <- metsSlopeBearing.1(fakeThal, fakeChanGeom, NULL, fakeProtocol)

  expected <- expected[order(expected$UID, expected$METRIC),]
  results <- results[order(results$UID, results$METRIC),]
  expected$RESULT <- as.numeric(expected$RESULT)
  results$RESULT <- as.numeric(results$RESULT)
  errs <- dfCompare(expected, results, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs)
  checkEquals(NULL, errs
             ,"Error: Slope and bearing metrics are broken with both protocols using normal thalweg data"
             )
             
             
  # Test calculation restricted to wadeable reaches
  fakeProtocol.s <- subset(fakeProtocol, PROTOCOL=='WADEABLE')
  fakeThal.s <- subset(fakeThal, UID %in% fakeProtocol.s$UID)
  fakeChanGeom.s <- subset(fakeChanGeom, UID %in% fakeProtocol.s$UID)
  expected.s <- subset(expected, UID %in% fakeProtocol.s$UID)

  results.s <- metsSlopeBearing.1(fakeThal.s, fakeChanGeom.s, NULL, fakeProtocol.s)
  results.s$RESULT <- as.numeric(results.s$RESULT)
  errs.s <- dfCompare(expected.s, results.s, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs.s)
  checkEquals(NULL, errs.s
             ,"Error: Slope and bearing metrics are broken with wadeable data"
             )

             
  # Test calculation restricted to boatable reaches
  fakeProtocol.r <- subset(fakeProtocol, PROTOCOL=='BOATABLE')
  fakeThal.r <- subset(fakeThal, UID %in% fakeProtocol.r$UID)
  fakeChanGeom.r <- subset(fakeChanGeom, UID %in% fakeProtocol.r$UID)
  expected.r <- subset(expected, UID %in% fakeProtocol.r$UID)
  
  results.r <- metsSlopeBearing.1(fakeThal.r, fakeChanGeom.r, NULL, fakeProtocol.r)
  results.r$RESULT <- as.numeric(results.r$RESULT)
  errs.r <- dfCompare(expected.r, results.r, c('UID','METRIC'), zeroFudge=1e-3)
#  return(errs.r)
  checkEquals(NULL, errs.r
             ,"Error: Slope and bearing metrics are broken with boatable data"
             )


}

metsSlopeBearing.makeTestDataDEPRECATED <- function()
# Creates thalweg and channel geometry data used in testing metsSlopeBearing().
# Returns a list of dataframes, element 1 is thalweg, element 2 is chanGeom.
#
# Test data taken from WEMAP data on 2-Feb-2010, and is then transformed into
# the expected organization for NRSA data.
# 2000 WAZP99-0505 1               Stream with no supplemental readings
# 2000 WAZP99-0569 1               Stream with many supplemental readings
# 2003 WWYP99-0659 1               Stream with slopes taken in cm
# 2000 WAZP99-0569 1 no incremnt   Stream with no incremnt information
# 2000 WAZP99-0569 1 no slopes     Stream with no slope information
# 2000 WAZP99-0569 1 only 2 slopes Stream with insufficient slope information
# 2000 WIDP99-0556 1               River with some supplemental readings
# 2000 WSDP99-0531 1               River with lots of supplemental readings
#
{
#234567890123456789012345678901234567890
  wemapThal <- data.frame(matrix(
                  c('2000 WAZP99-0505 1', 'A', 1.5, 7, 354, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'B', 1.5, 6, 357, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'C', 1.5, 4, 11, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'D', 1.5, 7.5, 3, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'E', 1.5, 12.5, 9, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'F', 1.5, 5, 17, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'G', 1.5, 3.5, 5, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'H', 1.5, 1.5, 57, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'I', 1.5, 4, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0505 1', 'J', 1.5, 6.5, 53, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1', 'A', 1.5, 6, 50, 33
                   ,10, 58, 34, 12, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'B', 1.5, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'C', 1.5, 12, 4, 25
                   ,12, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'D', 1.5, 11, 25, 80
                   ,13, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'E', 1.5, 22, 65, 75
                   ,8, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'F', 1.5, 8, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'G', 1.5, 12, 10, 80
                   ,10, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'H', 1.5, 6, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'I', 1.5, 12.2, 76, 30
                   ,14, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1', 'J', 1.5, 8, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2003 WWYP99-0659 1', 'A', 1.5, 36, 161, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'B', 1.5, 12, 110, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'C', 1.5, 12, 193,  20
                   ,4,  75, 30,  0, 124, 50, 'CM'
                   ,'2003 WWYP99-0659 1', 'D', 1.5, 26, 230, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'E', 1.5,  8, 193, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'F', 1.5, 18, 120, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'G', 1.5,  9, 210,  50
                   ,2, 108, 50, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'H', 1.5, 14, 246, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'I', 1.5,  2, 157,  50
                   ,10, 238, 50, NA,  NA, NA, 'CM'
                   ,'2003 WWYP99-0659 1', 'J', 1.5,  1, 100, 100
                   ,NA,  NA, NA, NA,  NA, NA, 'CM'

                   ,'2000 WAZP99-0569 1 no incremnt', 'A', NA, 6, 50, 33
                   ,10, 58, 34, 12, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'B', NA, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'C', NA, 12, 4, 25
                   ,12, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'D', NA, 11, 25, 80
                   ,13, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'E', NA, 22, 65, 75
                   ,8, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'F', NA, 8, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'G', NA, 12, 10, 80
                   ,10, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'H', NA, 6, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'I', NA, 12.2, 76, 30
                   ,14, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no incremnt', 'J', NA, 8, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1 no slopes', 'A', 1.5, NA, 50, 33
                   ,NA, 58, 34, NA, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'B', 1.5, NA, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'C', 1.5, NA, 4, 25
                   ,NA, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'D', 1.5, NA, 25, 80
                   ,NA, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'E', 1.5, NA, 65, 75
                   ,NA, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'F', 1.5, NA, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'G', 1.5, NA, 10, 80
                   ,NA, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'H', 1.5, NA, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'I', 1.5, NA, 76, 30
                   ,NA, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 no slopes', 'J', 1.5, NA, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WAZP99-0569 1 only 2 slopes', 'A', 1.5, 6, 50, 33
                   ,NA, 58, 34, NA, 0, 33, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'B', 1.5, 10, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'C', 1.5, NA, 4, 25
                   ,NA, 39, 75, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'D', 1.5, NA, 25, 80
                   ,NA, 53, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'E', 1.5, NA, 65, 75
                   ,NA, 19, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'F', 1.5, NA, 37, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'G', 1.5, NA, 10, 80
                   ,NA, 73, 20, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'H', 1.5, NA, 107, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'I', 1.5, NA, 76, 30
                   ,NA, 45, 70, NA, NA, NA, 'PERCENT'
                   ,'2000 WAZP99-0569 1 only 2 slopes', 'J', 1.5, NA, 23, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WIDP99-0556 1', 'A', 30, 0.2, 150, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'B', 30, 0.2, 50, 40
                   ,0.2, 140, 60, NA, NA, NA , 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'C', 33.3333333, 0.2, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'D', 30, 0.2, 50, 28.5714286
                   ,0.2, 40, 28.5714286, 0.2, 20, 42.8571429, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'E', 30, 0.2, 70, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'F', 30, 0.2, 60, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'G', 30, 0.1, 60, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'H', 30, 0.2, 50, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'I', 30, 0.1, 40, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'
                   ,'2000 WIDP99-0556 1', 'J', 30, 0.2, 30, 100
                   ,NA, NA, NA, NA, NA, NA, 'PERCENT'

                   ,'2000 WSDP99-0531 1', 'A', 20, 0.1, 80, 45.7142857
                   ,0.1, 340, 40, 0.1, 20, 14.2857143, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'B', 20, 0.1, 50, 50
                   ,0.1, 10, 25, 0.1, 100, 25, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'C', 20, 0.1, 360, 22.5
                   ,0.1, 350, 42.5, 0.1, 60, 35, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'D', 20, 0.1, 40, 25
                   ,0.1, 40, 12.5, 0.1, 80, 62.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'E', 20, 0.1, 330, 12.5
                   ,0.1, 20, 87.5, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'F', 20, 0.1, 120, 15
                   ,0.1, 330, 22.5, 0.1, 80, 62.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'G', 20, 0.1, 50, 75
                   ,0.1, 140, 25, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'H', 20, 0.1, 90, 40.7407407
                   ,0.1, 340, 59.2592593, NA, NA, NA, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'I', 20, 0.1, 200, 30
                   ,0.1, 200, 32.5, 0.1, 220, 37.5, 'PERCENT'
                   ,'2000 WSDP99-0531 1', 'J', 20, 0.1, 180, 12.5
                   ,0.1, 160, 52.5, 0.1, 240, 35, 'PERCENT'
                   )
                   ,ncol=13, byrow=TRUE
                 ) # end of matrix() call
                 ,stringsAsFactors=FALSE
             ) # end of data.frame() call

  names(wemapThal) <- c('UID','TRANSECT','incremnt', 'slopet','beart','proportt'
                        ,'slope1','bear1','proport1','slope2','bear2','proport2'
                        ,'units'
                        )

  wemapRiverStations <- data.frame('UID'=c(rep('2000 WIDP99-0556 1', 10)
                                          ,rep('2000 WSDP99-0531 1', 10)
                                          )
                                  ,'TRANSECT'=rep(LETTERS[1:10], 2)
                                  ,'nsta'=c(20,20,18,20,20, 20,20,20,20,20
                                           ,20,20,20,20,20, 20,20,20,20,20
                                           )
                                  )

  # Create fakeThal from transposed wemap thalweg data.  This holds the wadeable
  # reach incremnt values; everything else is in the channel geometry.  Transect
  # C of WAZP99-0505 ended at station 8, so that is removed explicitly.  The
  # UNITS information is handled separately.
  # To allow proper counting of stations per transect during metrics calculation,
  # DEPTH is added at each station, though these values will not be used.
  units <- rename(wemapThal[c('UID','TRANSECT','units')], 'units', 'UNITS')
  
  ss <- subset(wemapThal, select=-units)
  tt <- dfLengthen(ss
                  ,c('UID','TRANSECT')
                  ,'PARAMETER'
                  ,'RESULT'
                  ,names(ss)[!(names(ss) %in%
                                       c('UID','TRANSECT')
                                     )]
                        )

  fakeThal <- subset(tt
                    ,UID %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                               ,'2003 WWYP99-0659 1'
                               ,'2000 WAZP99-0569 1 no incremnt'
                               ,'2000 WAZP99-0569 1 no slopes'
                               ,'2000 WAZP99-0569 1 only 2 slopes'
                               ) &
                     PARAMETER == 'incremnt'
                    )
  fakeThal <- merge(fakeThal, list('STATION'=0:9), all=TRUE)
  fakeThal <- subset(fakeThal
                    ,!(UID!='2000 WAZP99-0505 1' & TRANSECT=='C' & STATION=='9')
                    )
  fakeThal$PARAMETER <- 'INCREMNT'
  fakeThal$SAMPLE_TYPE <- 'PHAB_THALW'
  fakeThal$FLAG <- NA
  fakeThal$UNITS <- 'M'

  addDepths<-fakeThal
  addDepths$PARAMETER='DEPTH'
  addDepths$UNITS<-'CM'
  fakeThal<-rbind(fakeThal,addDepths)

  # Create fakeChanGeom from transposed wemap thalweg data.  River and stream
  # data are slightly different, and thus are assembled separately.  Rows with
  # missing values are removed.
  wadeable <- subset(tt
                    ,UID %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                               ,'2003 WWYP99-0659 1'
                               ,'2000 WAZP99-0569 1 no incremnt'
                               ,'2000 WAZP99-0569 1 no slopes'
                               ,'2000 WAZP99-0569 1 only 2 slopes'
                               )
                     & PARAMETER != 'incremnt'
                    )
  wadeable$LINE <- NA
  wadeable$PARAMETER <- ifelse(wadeable$PARAMETER == 'slopet', 'SLOPE'
                       ,ifelse(wadeable$PARAMETER == 'beart', 'BEARING'
                       ,ifelse(wadeable$PARAMETER == 'proportt', 'PROP'
                       ,ifelse(wadeable$PARAMETER == 'slope1', 'SLOPE2'
                       ,ifelse(wadeable$PARAMETER == 'bear1', 'BEARING2'
                       ,ifelse(wadeable$PARAMETER == 'proport1', 'PROP2'
                       ,ifelse(wadeable$PARAMETER == 'slope2', 'SLOPE3'
                       ,ifelse(wadeable$PARAMETER == 'bear2', 'BEARING3'
                       ,ifelse(wadeable$PARAMETER == 'proport2', 'PROP3', NA
                       )))))))))
  wadeable <- merge(wadeable, units, by=c('UID','TRANSECT'))
  wadeable$UNITS <- ifelse(wadeable$PARAMETER %in% c('SLOPE','SLOPE2','SLOPE3')
                          ,wadeable$UNITS
                          ,'NONE'
                          )

  bb <- subset(wemapThal
              ,!(UID %in% c('2000 WAZP99-0505 1','2000 WAZP99-0569 1'
                           ,'2003 WWYP99-0659 1'
                           ,'2000 WAZP99-0569 1 no incremnt'
                           ,'2000 WAZP99-0569 1 no slopes'
                           ,'2000 WAZP99-0569 1 only 2 slopes'
                           )
                )
              )
  bb <- merge(bb, wemapRiverStations, by=c('UID','TRANSECT'), all.x=TRUE)
  bb$distancet <- (as.numeric(bb$proportt)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$distance1 <- (as.numeric(bb$proport1)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$distance2 <- (as.numeric(bb$proport2)/100) * as.numeric(bb$incremnt) * bb$nsta
  bb$incremnt <- NULL
  bb$proportt <- NULL
  bb$proport1 <- NULL
  bb$proport2 <- NULL
  bb$nsta <- NULL

  boatable <- dfLengthen(bb, c('UID','TRANSECT'), 'PARAMETER', 'RESULT'
                        ,names(bb)[!(names(bb) %in% c('UID','TRANSECT'))]
                        )

  boatable$LINE <- ifelse(boatable$PARAMETER %in% c('slopet','beart','distancet'), 999
                  ,ifelse(boatable$PARAMETER %in% c('slope1','bear1','distance1'), 1
                  ,ifelse(boatable$PARAMETER %in% c('slope2','bear2','distance2'), 2, NA
                  )))

  boatable$PARAMETER <-
        ifelse(boatable$PARAMETER %in% c('slopet','slope1','slope2'), 'SLOPE'
       ,ifelse(boatable$PARAMETER %in% c('beart','bear1','bear2'), 'BEAR'
       ,ifelse(boatable$PARAMETER %in% c('distancet','distance1','distance2'), 'DISTANCE', NA
       )))
  boatable$UNITS <- 'NONE'

  fakeChanGeom <- subset(rbind(boatable, wadeable), !is.na(RESULT))
  fakeChanGeom$TRANLINE <- 'NONE'
  fakeChanGeom$BANK <- 'NONE'
  fakeChanGeom$SAMPLE_TYPE <- ifelse(fakeChanGeom$UID %in%
                                       c('2000 WAZP99-0505 1'
                                        ,'2000 WAZP99-0569 1'
                                        ,'2003 WWYP99-0659 1'
                                        ,'2000 WAZP99-0569 1 no incremnt'
                                        ,'2000 WAZP99-0569 1 no slopes'
                                        ,'2000 WAZP99-0569 1 only 2 slopes'
                                        )
                                    ,'PHAB_SLOPE'
                                    ,'PHAB_CHANBFRONT'
                                    )
  fakeChanGeom$FLAG <- as.character(NA)

  return(list(fakeThal, fakeChanGeom))
}


metsSlopeBearing.makeThalweg <- function()
# Create dataframe of thalweg data for unit test
{
  tc <-textConnection("
          UID TRANSECT RESULT PARAMETER STATION SAMPLE_TYPE FLAG UNITS
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with low slope' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 0 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 1 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 2 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 3 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing PERCENT subsightings' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 4 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 5 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 6 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 7 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' C NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 8 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' C 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' A NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' B NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' D NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' E NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' F NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' G NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' H NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' I NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no incremnt' J NA INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 no slopes' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 INCREMNT 9 PHAB_THALW NA M
        '2000 WAZP99-0505 1' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' C 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' C 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with low slope' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' C 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1 with clinometer and low slope' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 0 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 1 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 2 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 3 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 4 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 5 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 6 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 7 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' C NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' C 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 8 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' C 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0505 1' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 missing CM subsightings' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2003 WWYP99-0659 1 slope unit NONE' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' A NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' B NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' D NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' E NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' F NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' G NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' H NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' I NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no incremnt' J NA DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 no slopes' J 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' A 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' B 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' D 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' E 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' F 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' G 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' H 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' I 1.5 DEPTH 9 PHAB_THALW NA CM
        '2000 WAZP99-0569 1 only 2 slopes' J 1.5 DEPTH 9 PHAB_THALW NA CM
       ")
  tt <- read.table(tc, header=TRUE,stringsAsFactors=FALSE)
  close(tc)
  tt$RESULT <- as.character(tt$RESULT)

  return(tt)
}

metsSlopeBearing.makeChannelGeometry <- function()
# Create dataframe of channel geometry data for unit test
{
    cg <- rbind(data.frame(UID = '2000 WAZP99-0505 1'
                          ,TRANSECT = rep(LETTERS[1:10], each=3)
                          ,LINE = as.numeric(NA)
                          ,PARAMETER = c('SLOPE','PROP','BEARING','PROP','SLOPE'
                                        ,'BEARING','PROP','BEARING','SLOPE','SLOPE'
                                        ,'BEARING','PROP','PROP','BEARING','SLOPE'
                                        ,'PROP','BEARING','SLOPE','SLOPE','BEARING'
                                        ,'PROP','BEARING','PROP','SLOPE','PROP'
                                        ,'BEARING','SLOPE','PROP','BEARING','SLOPE')
                          ,RESULT = as.character(c('7','100','354','100','6'
                                                  ,'357','100','11','4','7.5'
                                                  ,'3','100','100','9','12.5'
                                                  ,'100','17','5','3.5','5'
                                                  ,'100','57','100','1.5','100'
                                                  ,'23','4','100','53','6.5')
                                                )
                          ,UNITS = c('PERCENT','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','NONE','PERCENT','PERCENT'
                                    ,'NONE','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','PERCENT','PERCENT','NONE'
                                    ,'NONE','NONE','NONE','PERCENT','NONE'
                                    ,'NONE','PERCENT','NONE','NONE','PERCENT'
                                    )
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_SLOPE'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'TR'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(UID = '2000 WAZP99-0505 1 with clinometer'
                          ,TRANSECT = rep(LETTERS[1:10], each=3)
                          ,LINE = as.numeric(NA)
                          ,PARAMETER = c('SLOPE','PROP','BEARING','PROP','SLOPE'
                                        ,'BEARING','PROP','BEARING','SLOPE','SLOPE'
                                        ,'BEARING','PROP','PROP','BEARING','SLOPE'
                                        ,'PROP','BEARING','SLOPE','SLOPE','BEARING'
                                        ,'PROP','BEARING','PROP','SLOPE','PROP'
                                        ,'BEARING','SLOPE','PROP','BEARING','SLOPE')
                          ,RESULT = as.character(c('7','100','354','100','6'
                                                  ,'357','100','11','4','7.5'
                                                  ,'3','100','100','9','12.5'
                                                  ,'100','17','5','3.5','5'
                                                  ,'100','57','100','1.5','100'
                                                  ,'23','4','100','53','6.5')
                                                )
                          ,UNITS = c('PERCENT','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','NONE','PERCENT','PERCENT'
                                    ,'NONE','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','PERCENT','PERCENT','NONE'
                                    ,'NONE','NONE','NONE','PERCENT','NONE'
                                    ,'NONE','PERCENT','NONE','NONE','PERCENT'
                                    )
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_SLOPE'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'CL'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(UID = '2000 WAZP99-0505 1 with low slope'
                          ,TRANSECT = rep(LETTERS[1:10], each=3)
                          ,LINE = as.numeric(NA)
                          ,PARAMETER = c('SLOPE','PROP','BEARING','PROP','SLOPE'
                                        ,'BEARING','PROP','BEARING','SLOPE','SLOPE'
                                        ,'BEARING','PROP','PROP','BEARING','SLOPE'
                                        ,'PROP','BEARING','SLOPE','SLOPE','BEARING'
                                        ,'PROP','BEARING','PROP','SLOPE','PROP'
                                        ,'BEARING','SLOPE','PROP','BEARING','SLOPE')
                          ,RESULT = as.character(c('0.7','100','354','100','0.6'
                                                  ,'357','100','11','0.4','0.75'
                                                  ,'3','100','100','9','1.25'
                                                  ,'100','17','0.5','0.35','5'
                                                  ,'100','57','100','0.15','100'
                                                  ,'23','0.4','100','53','0.65')
                                                )
                          ,UNITS = c('PERCENT','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','NONE','PERCENT','PERCENT'
                                    ,'NONE','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','PERCENT','PERCENT','NONE'
                                    ,'NONE','NONE','NONE','PERCENT','NONE'
                                    ,'NONE','PERCENT','NONE','NONE','PERCENT'
                                    )
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_SLOPE'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'TR'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(UID = '2000 WAZP99-0505 1 with clinometer and low slope'
                          ,TRANSECT = rep(LETTERS[1:10], each=3)
                          ,LINE = as.numeric(NA)
                          ,PARAMETER = c('SLOPE','PROP','BEARING','PROP','SLOPE'
                                        ,'BEARING','PROP','BEARING','SLOPE','SLOPE'
                                        ,'BEARING','PROP','PROP','BEARING','SLOPE'
                                        ,'PROP','BEARING','SLOPE','SLOPE','BEARING'
                                        ,'PROP','BEARING','PROP','SLOPE','PROP'
                                        ,'BEARING','SLOPE','PROP','BEARING','SLOPE')
                          ,RESULT = as.character(c('0.7','100','354','100','0.6'
                                                  ,'357','100','11','0.4','0.75'
                                                  ,'3','100','100','9','1.25'
                                                  ,'100','17','0.5','0.35','5'
                                                  ,'100','57','100','0.15','100'
                                                  ,'23','0.4','100','53','0.65')
                                                )
                          ,UNITS = c('PERCENT','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','NONE','PERCENT','PERCENT'
                                    ,'NONE','NONE','NONE','NONE','PERCENT'
                                    ,'NONE','NONE','PERCENT','PERCENT','NONE'
                                    ,'NONE','NONE','NONE','PERCENT','NONE'
                                    ,'NONE','PERCENT','NONE','NONE','PERCENT'
                                    )
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_SLOPE'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'CL'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(UID = '2000 WAZP99-0569 1'
			              ,TRANSECT = c('A','A','A','A','A','A','A','A','A'
                                       ,'B','B','B'
                                       ,'C','C','C','C','C','C'
                				       ,'D','D','D','D','D','D'
			                           ,'E','E','E','E','E','E'
            			               ,'F','F','F'
			                	       ,'G','G','G','G','G','G'
            			               ,'H','H','H','I','I','I','I','I','I'
			            	           ,'J','J','J'
  				                       )
			              ,RESULT = c('34','58','33','6','0'
                				     ,'12','10','33','50','100'
			                         ,'40','10','4','75','25'
            			             ,'39','12','12','20','53'
            			             ,'11','80','13','25','19'
			                         ,'25','8','75','22','65'
            			             ,'100','37','8','20','12'
            			             ,'73','10','80','10','100'
                                     ,'107','6','12.2','45','70'
			                         ,'30','76','14','8','100'
            			             ,'23')
			              ,PARAMETER = c('PROP2','BEARING2','PROP3','SLOPE','BEARING3'
			                            ,'SLOPE3','SLOPE2','PROP','BEARING','PROP'
            			                ,'BEARING','SLOPE','BEARING','PROP2','PROP'
			                            ,'BEARING2','SLOPE','SLOPE2','PROP2','BEARING2'
			                            ,'SLOPE','PROP','SLOPE2','BEARING','BEARING2'
            			                ,'PROP2','SLOPE2','PROP','SLOPE','BEARING'
			                            ,'PROP','BEARING','SLOPE','PROP2','SLOPE'
			                            ,'BEARING2','BEARING','PROP','SLOPE2','PROP'
            			                ,'BEARING','SLOPE','SLOPE','BEARING2','PROP2'
			                            ,'PROP','BEARING','SLOPE2','SLOPE','PROP'
			                            ,'BEARING'
                    					)
            			  ,LINE = as.numeric(NA)
			              ,UNITS = c('NONE','NONE','NONE','PERCENT','NONE'
                                    ,'PERCENT','PERCENT','NONE','NONE','NONE'
			                        ,'NONE','PERCENT','NONE','NONE','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'PERCENT','NONE','PERCENT','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','PERCENT','NONE'
            			            ,'NONE','NONE','PERCENT','NONE','PERCENT'
            			            ,'NONE','NONE','NONE','PERCENT','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'NONE','NONE','PERCENT','PERCENT','NONE'
            			            ,'NONE'
                				    )
            			  ,TRANLINE = 'NONE'
			              ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
            			  ,METHOD = 'TR'
            			  ,stringsAsFactors=FALSE
                          )
    	       ,data.frame(UID = '2000 WAZP99-0569 1 missing PERCENT subsightings'
	            		  ,TRANSECT = c('A','A','A','A','A','A','A','A','A'
            			               ,'B','B','B'
			                           ,'C','C','C','C','C','C'
            			               ,'D','D','D','D','D','D'
            			               ,'E','E','E','E','E','E'
			                           ,'F','F','F'
            			               ,'G','G','G','G','G','G'
            			               ,'H','H','H'
            			               ,'I','I','I','I','I','I'
            			               ,'J','J','J'
            			               )
            			  ,RESULT = c('34','58','33','6','0'
            			             ,NA,NA,'33','50','100'
            			             ,'40','10','4','75','25'
            			             ,'39','12',NA,'20','53'
            			             ,'11','80',NA,'25','19'
            			             ,'25',NA,'75','22','65'
            			             ,'100','37','8','20','12'
            			             ,'73','10','80',NA,'100'
            			             ,'107','6','12.2','45','70'
            			             ,'30','76',NA,'8','100'
            			             ,'23'
                				     )
            			  ,PARAMETER = c('PROP2','BEARING2','PROP3','SLOPE','BEARING3'
            			                ,'SLOPE3','SLOPE2','PROP','BEARING','PROP'
            			                ,'BEARING','SLOPE','BEARING','PROP2','PROP'
            			                ,'BEARING2','SLOPE','SLOPE2','PROP2','BEARING2'
            			                ,'SLOPE','PROP','SLOPE2','BEARING','BEARING2'
            			                ,'PROP2','SLOPE2','PROP','SLOPE','BEARING'
            			                ,'PROP','BEARING','SLOPE','PROP2','SLOPE'
            			                ,'BEARING2','BEARING','PROP','SLOPE2','PROP'
            			                ,'BEARING','SLOPE','SLOPE','BEARING2','PROP2'
            			                ,'PROP','BEARING','SLOPE2','SLOPE','PROP'
            			                ,'BEARING'
                    					)
            			  ,LINE = as.numeric(NA)
            			  ,UNITS = c('NONE','NONE','NONE','PERCENT','NONE'
            			            ,'PERCENT','PERCENT','NONE','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','NONE','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
			                        ,'PERCENT','NONE','PERCENT','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','PERCENT','NONE'
			                        ,'NONE','NONE','PERCENT','NONE','PERCENT'
            			            ,'NONE','NONE','NONE','PERCENT','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'NONE','NONE','PERCENT','PERCENT','NONE'
            			            ,'NONE'
                				    )
            			  ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
            			  ,METHOD = 'TR'
            			  ,stringsAsFactors=FALSE
            			  )
               ,data.frame(UID ='2000 WAZP99-0569 1 no incremnt'
            		   	  ,TRANSECT = c('A','A','A','A','A','A','A','A','A'
                				       ,'B','B','B'
	                			       ,'C','C','C','C','C','C'
                				       ,'D','D','D','D','D','D'
                				       ,'E','E','E','E','E','E'
                				       ,'F','F','F'
                				       ,'G','G','G','G','G','G'
                				       ,'H','H','H'
                				       ,'I','I','I','I','I','I'
                				       ,'J','J','J'
                				       )
            			  ,RESULT = c('34','58','33','6','0'
            			             ,'12','10','33','50','100'
            			             ,'40','10','4','75','25'
            			             ,'39','12','12','20','53'
            			             ,'11','80','13','25','19'
            			             ,'25','8','75','22','65'
            			             ,'100','37','8','20','12'
            			             ,'73','10','80','10','100'
            			             ,'107','6','12.2','45','70'
            			             ,'30','76','14','8','100'
            			             ,'23'
                				     )
             			  ,PARAMETER = c('PROP2','BEARING2','PROP3','SLOPE','BEARING3'
            			                ,'SLOPE3','SLOPE2','PROP','BEARING','PROP'
            			                ,'BEARING','SLOPE','BEARING','PROP2','PROP'
            			                ,'BEARING2','SLOPE','SLOPE2','PROP2','BEARING2'
            			                ,'SLOPE','PROP','SLOPE2','BEARING','BEARING2'
            			                ,'PROP2','SLOPE2','PROP','SLOPE','BEARING'
            			                ,'PROP','BEARING','SLOPE','PROP2','SLOPE'
            			                ,'BEARING2','BEARING','PROP','SLOPE2','PROP'
            			                ,'BEARING','SLOPE','SLOPE','BEARING2','PROP2'
            			                ,'PROP','BEARING','SLOPE2','SLOPE','PROP'
            			                ,'BEARING')
            			  ,LINE = as.numeric(NA)
            			  ,UNITS = c('NONE','NONE','NONE','PERCENT','NONE'
            			            ,'PERCENT','PERCENT','NONE','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','NONE','NONE'
			                        ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'PERCENT','NONE','PERCENT','NONE','NONE'
            			            ,'NONE','PERCENT','NONE','PERCENT','NONE'
            			            ,'NONE','NONE','PERCENT','NONE','PERCENT'
            			            ,'NONE','NONE','NONE','PERCENT','NONE'
            			            ,'NONE','PERCENT','PERCENT','NONE','NONE'
            			            ,'NONE','NONE','PERCENT','PERCENT','NONE'
            			            ,'NONE')
            			  ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
            			  ,METHOD = 'TR'
            			  ,stringsAsFactors=FALSE
            			  )
     	       ,data.frame(UID = '2000 WAZP99-0569 1 no slopes'
            			  ,TRANSECT = c('A','A','A','A','A','A'
                				       ,'B','B'
	                			       ,'C','C','C','C'
                				       ,'D','D','D','D'
                				       ,'E','E','E','E'
                     	               ,'F','F'
            			               ,'G','G','G','G'
                                       ,'H','H'
                                       ,'I','I','I','I'
                                       ,'J','J'
                				       )
            			  ,RESULT = c('0','33','34','58','50'
            			             ,'33','100','40','75','39'
            			             ,'4','25','20','53','25'
			                         ,'80','19','25','75','65'
            			             ,'100','37','20','73','10'
            			             ,'80','107','100','30','45'
            			             ,'70','76','100','23')
            			  ,PARAMETER = c('BEARING3','PROP3','PROP2','BEARING2','BEARING'
            			                ,'PROP','PROP','BEARING','PROP2','BEARING2'
            			                ,'BEARING','PROP','PROP2','BEARING2','BEARING'
            			                ,'PROP','BEARING2','PROP2','PROP','BEARING'
            			                ,'PROP','BEARING','PROP2','BEARING2','BEARING'
            			                ,'PROP','BEARING','PROP','PROP','BEARING2'
            			                ,'PROP2','BEARING','PROP','BEARING'
                    					)
            			  ,LINE = as.numeric(NA)
			              ,UNITS = 'NONE'
            			  ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
            			  ,METHOD = 'TR'
            			  ,stringsAsFactors=FALSE
            			  )
    	       ,data.frame(UID='2000 WAZP99-0569 1 only 2 slopes'
	            		  ,TRANSECT = c('A','A','A','A','A','A','A'
                				       ,'B','B','B'
                 				       ,'C','C','C','C'
                                       ,'D','D','D','D'
	                                   ,'E','E','E','E'
                                       ,'F','F'
                                       ,'G','G','G','G'
                                       ,'H','H'
                                       ,'I','I','I','I'
                                       ,'J','J'
                                       )
            			  ,RESULT = c('34','58','33','6','0'
			                         ,'33','50','100','40','10'
            			             ,'4','75','25','39','20'
			                         ,'53','80','25','19','25'
			                         ,'75','65','100','37','20'
            			             ,'73','10','80','107','100'
			                         ,'76','30','70','45','100'
			                         ,'23')
            			  ,PARAMETER = c('PROP2','BEARING2','PROP3','SLOPE','BEARING3'
			                            ,'PROP','BEARING','PROP','BEARING','SLOPE'
            			                ,'BEARING','PROP2','PROP','BEARING2','PROP2'
			                            ,'BEARING2','PROP','BEARING','BEARING2','PROP2'
			                            ,'PROP','BEARING','PROP','BEARING','PROP2'
            				            ,'BEARING2','BEARING','PROP','BEARING','PROP'
			                            ,'BEARING','PROP','PROP2','BEARING2','PROP'
			                            ,'BEARING')
            			  ,LINE = as.numeric(NA)
			              ,UNITS = c('NONE','NONE','NONE','PERCENT','NONE'
                				    ,'NONE','NONE','NONE','NONE','PERCENT'
            			            ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE'
				                    )
            			  ,TRANLINE = 'NONE'
			              ,BANK = 'NONE'
			              ,SAMPLE_TYPE = 'PHAB_SLOPE'
            			  ,FLAG = as.character(NA)
			              ,METHOD = 'TR'
			              ,stringsAsFactors=FALSE
            			  ) 
	           ,data.frame(UID = '2000 WIDP99-0556 1'
            			  ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
			                           ,'A','B','C','D','E','F','G','H','I','J'
            			               ,'B','D','B','D','D','D','A','B','C','D'
			                           ,'E','F','G','H','I','J','A','B','C','D'
			                           ,'E','F','G','H','I','J','B','D','D'
                				       )        
			              ,RESULT = c('0.2','0.2','0.2','0.2','0.2'
			                         ,'0.2','0.1','0.2','0.1','0.2'
             			             ,'150','50','40','50','70'
			                         ,'60','60','50','40','30'
                     	             ,'0.2','0.2','140','40','0.2'
             			             ,'20','PERCENT','PERCENT','PERCENT','PERCENT'
             			             ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
			                         ,'PERCENT','600','240','599.9999994','171.4285716'
			                         ,'600','600','600','600','600'
			                         ,'600','360','171.4285716','257.1428574'
				                     )
			              ,PARAMETER = c('SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                            ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
            			                ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'SLOPE','SLOPE','BEAR','BEAR','SLOPE'
            			                ,'BEAR','NA','NA','NA','NA'
            			                ,'NA','NA','NA','NA','NA'
			                            ,'NA','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                            ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
            			                ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE')
            			  ,LINE = c(999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,1, 1, 1, 1, 2
			                       ,2, NA, NA, NA, NA
            			           ,NA, NA, NA, NA, NA
			                       ,NA, 999, 999, 999, 999
            			           ,999, 999, 999, 999, 999
			                       ,999, 1, 1, 2)
            			  ,UNITS = 'NONE'
			              ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
			              ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
            			  ,FLAG = as.character(NA)
			              ,METHOD = 'NONE'
            			  ,stringsAsFactors=FALSE
			              )
               ,data.frame(UID = '2000 WIDP99-0556 1 with NA slopes'
                          ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
                                       ,'A','B','C','D','E','F','G','H','I','J'
                                       ,'B','D','B','D','D','D','A','B','C','D'
                                       ,'E','F','G','H','I','J','A','B','C','D'
                                       ,'E','F','G','H','I','J','B','D','D'
                                       )        
                          ,RESULT = c(NA, NA, NA, NA, NA
                                     ,NA, NA, NA, NA, NA
                                     ,'150','50','40','50','70'
                                     ,'60','60','50','40','30'
                                     ,NA, NA, '140','40', NA
                                     ,'20','PERCENT','PERCENT','PERCENT','PERCENT'
                                     ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
                                     ,'PERCENT','600','240','599.9999994','171.4285716'
                                     ,'600','600','600','600','600'
                                     ,'600','360','171.4285716','257.1428574'
                                     )
                          ,PARAMETER = c('SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
                                        ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
                                        ,'BEAR','BEAR','BEAR','BEAR','BEAR'
                                        ,'BEAR','BEAR','BEAR','BEAR','BEAR'
                                        ,'SLOPE','SLOPE','BEAR','BEAR','SLOPE'
                                        ,'BEAR','NA','NA','NA','NA'
                                        ,'NA','NA','NA','NA','NA'
                                        ,'NA','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
                                        ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
                                        ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE')
                          ,LINE = c(999, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,1, 1, 1, 1, 2
                                   ,2, NA, NA, NA, NA
                                   ,NA, NA, NA, NA, NA
                                   ,NA, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,999, 1, 1, 2)
                          ,UNITS = 'NONE'
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'NONE'
                          ,stringsAsFactors=FALSE
                          )
               ,data.frame(UID = '2000 WIDP99-0556 1 with absent slopes'
                          ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
                                       ,'B','D','D','A','B','C','D'
                                       ,'E','F','G','H','I','J','A','B','C','D'
                                       ,'E','F','G','H','I','J','B','D','D'
                                       )        
                          ,RESULT = c('150','50','40','50','70'
                                     ,'60','60','50','40','30'
                                     ,'140','40'
                                     ,'20','PERCENT','PERCENT','PERCENT','PERCENT'
                                     ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
                                     ,'PERCENT','600','240','599.9999994','171.4285716'
                                     ,'600','600','600','600','600'
                                     ,'600','360','171.4285716','257.1428574'
                                     )
                          ,PARAMETER = c('BEAR','BEAR','BEAR','BEAR','BEAR'
                                        ,'BEAR','BEAR','BEAR','BEAR','BEAR'
                                        ,'BEAR','BEAR'
                                        ,'BEAR','NA','NA','NA','NA'
                                        ,'NA','NA','NA','NA','NA'
                                        ,'NA','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
                                        ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
                                        ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE')
                          ,LINE = c(999, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,1, 1
                                   ,2, NA, NA, NA, NA
                                   ,NA, NA, NA, NA, NA
                                   ,NA, 999, 999, 999, 999
                                   ,999, 999, 999, 999, 999
                                   ,999, 1, 1, 2)
                          ,UNITS = 'NONE'
                          ,TRANLINE = 'NONE'
                          ,BANK = 'NONE'
                          ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
                          ,FLAG = as.character(NA)
                          ,METHOD = 'NONE'
                          ,stringsAsFactors=FALSE
                          )
        	   ,data.frame(UID = '2000 WIDP99-0556 1 slope unit PCT'
            			  ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
			                           ,'A','B','C','D','E','F','G','H','I','J'
			                           ,'B','D','B','D','D','D','A','B','C','D'
            			               ,'E','F','G','H','I','J','A','B','C','D'
			                           ,'E','F','G','H','I','J','B','D','D'
                 				       )        
            			  ,RESULT = c('0.2','0.2','0.2','0.2','0.2'
		                             ,'0.2','0.1','0.2','0.1','0.2'
			                         ,'150','50','40','50','70'
            			             ,'60','60','50','40','30'
			                         ,'0.2','0.2','140','40','0.2'
			                         ,'20','PERCENT','PERCENT','PERCENT','PERCENT'
            			             ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
			                         ,'PERCENT','600','240','599.9999994','171.4285716'
			                         ,'600','600','600','600','600'
            			             ,'600','360','171.4285716','257.1428574'
			                	     )
            			  ,PARAMETER = c('SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                            ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                            ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			                ,'SLOPE','SLOPE','BEAR','BEAR','SLOPE'
			                            ,'BEAR','NA','NA','NA','NA'
			                            ,'NA','NA','NA','NA','NA'
            			                ,'NA','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                            ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                            ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE')
            			  ,LINE = c(999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,999, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,1, 1, 1, 1, 2
            			           ,2, NA, NA, NA, NA
			                       ,NA, NA, NA, NA, NA
            			           ,NA, 999, 999, 999, 999
			                       ,999, 999, 999, 999, 999
            			           ,999, 1, 1, 2
			                	   )
            			  ,UNITS = c('PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
			                        ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
			                        ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
			                        ,'NONE','NONE','NONE','NONE','NONE'
            			            ,'NONE','NONE','NONE','NONE')
			              ,TRANLINE = 'NONE'
            			  ,BANK = 'NONE'
            			  ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
			              ,FLAG = as.character(NA)
			              ,METHOD = 'NONE'
            			  ,stringsAsFactors=FALSE
			              )
	           ,data.frame(UID = '2000 WSDP99-0531 1'
			              ,TRANSECT = c('A','B','C','D','E','F','G','H','I','J'
            			               ,'A','B','C','D','E','F','G','H','I','J'
            			               ,'A','B','C','D','E','F','G','H','I','J'
			                           ,'A','B','C','D','E','F','G','H','I','J'
            			               ,'A','B','C','D','F','I','J'
                				       ,'A','B','C','D','F','I','J'
                				       ,'A','B','C','D','E','F','G','H','I','J'
                				       ,'A','B','C','D','E','F','G','H','I','J'
                				       ,'A','B','C','D','E','F','G','H','I','J'
                				       ,'A','B','C','D','F','I','J')
            			  ,RESULT = c('0.1','0.1','0.1','0.1','0.1'
			                         ,'0.1','0.1','0.1','0.1','0.1'
			                         ,'80','50','360','40','330'
            			             ,'120','50','90','200','180'
			                         ,'0.1','0.1','0.1','0.1','0.1'
			                         ,'0.1','0.1','0.1','0.1','0.1'
            			             ,'340','10','350','40','20'
			                         ,'330','140','340','200','160'
			                         ,'0.1','0.1','0.1','0.1','0.1'
            			             ,'0.1','0.1','20','100','60'
			                         ,'80','80','220','240','PERCENT'
			                         ,'PERCENT','PERCENT','PERCENT','PERCENT','PERCENT'
            			             ,'PERCENT','PERCENT','PERCENT','PERCENT','182.8571428'
			                         ,'200','90','100','50','60'
			                         ,'300','162.9629628','120','50','160'
            			             ,'100','170','50','350','90'
			                         ,'100','237.0370372','130','210','57.1428572'
			                         ,'100','140','250','250','150'
            			             ,'140')
            			,PARAMETER = c('SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                          ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
            			              ,'BEAR','BEAR','BEAR','BEAR','BEAR'
			                          ,'BEAR','BEAR','BEAR','BEAR','BEAR'
			                          ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
            			              ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                          ,'BEAR','BEAR','BEAR','BEAR','BEAR'
			                          ,'BEAR','BEAR','BEAR','BEAR','BEAR'
            			              ,'SLOPE','SLOPE','SLOPE','SLOPE','SLOPE'
			                          ,'SLOPE','SLOPE','BEAR','BEAR','BEAR'
			                          ,'BEAR','BEAR','BEAR','BEAR','NA'
            			              ,'NA','NA','NA','NA','NA'
			                          ,'NA','NA','NA','NA','DISTANCE'
			                          ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
            			              ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                          ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                          ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
            			              ,'DISTANCE','DISTANCE','DISTANCE','DISTANCE','DISTANCE'
			                          ,'DISTANCE'
                				      )
            			,LINE = c(999, 999, 999, 999, 999
			                     ,999, 999, 999, 999, 999
            			         ,999, 999, 999, 999, 999
			                     ,999, 999, 999, 999, 999
         	                     ,1, 1, 1, 1, 1
            			         ,1, 1, 1, 1, 1
			                     ,1, 1, 1, 1, 1
            			         ,1, 1, 1, 1, 1
			                     ,2, 2, 2, 2, 2
            			         ,2, 2, 2, 2, 2
			                     ,2, 2, 2, 2, NA
            			         ,NA, NA, NA, NA, NA
			                     ,NA, NA, NA, NA, 999
            			         ,999, 999, 999, 999, 999
			                     ,999, 999, 999, 999, 1
            			         ,1, 1, 1, 1, 1
			                     ,1, 1, 1, 1, 2
            			         ,2, 2, 2, 2, 2
            			         ,2)
            			,UNITS = 'NONE'
			            ,TRANLINE = 'NONE'
            			,BANK = 'NONE'
			            ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
            			,FLAG = as.character(NA)
			            ,METHOD = 'NONE'
            			,stringsAsFactors=FALSE
			)
	       ,data.frame(UID = '2003 WWYP99-0659 1'
        			  ,TRANSECT = c('A','A','A','B','B'
		        	               ,'B','C','C','C','C'
			                       ,'C','C','C','C','C'
			                       ,'D','D','D','E','E'
        			               ,'E','F','F','F','G'
		        	               ,'G','G','G','G','G'
			                       ,'H','H','H','I','I'
			                       ,'I','I','I','I','J'
        			               ,'J','J'
		        		       )
        			  ,RESULT = c('161','36','100','12','100'
		        	             ,'110','50','30','75','193'
			                     ,'4','12','124','0','20'
			                     ,'26','230','100','100','193'
        			             ,'8','100','120','18','50'
		        	             ,'9','108','210','2','50'
			                     ,'246','100','14','10','50'
			                     ,'238','50','157','2','100'
        			             ,'100','1'
		        		     )
        			  ,PARAMETER = c('BEARING','SLOPE','PROP','SLOPE','PROP'
		        	                ,'BEARING','PROP3','PROP2','BEARING2','BEARING'
			                        ,'SLOPE2','SLOPE','BEARING3','SLOPE3','PROP'
			                        ,'SLOPE','BEARING','PROP','PROP','BEARING'
        			                ,'SLOPE','PROP','BEARING','SLOPE','PROP2'
		        	                ,'SLOPE','BEARING2','BEARING','SLOPE2','PROP'
			                        ,'BEARING','PROP','SLOPE','SLOPE2','PROP'
			                        ,'BEARING2','PROP2','BEARING','SLOPE','PROP'
        			                ,'BEARING','SLOPE'
		        		 	)
        			  ,LINE = as.numeric(NA)
		        	  ,UNITS = c('NONE','CM','NONE','CM','NONE'
			                    ,'NONE','NONE','NONE','NONE','NONE'
			                    ,'CM','CM','NONE','CM','NONE'
        			            ,'CM','NONE','NONE','NONE','NONE'
		        	            ,'CM','NONE','NONE','CM','NONE'
			                    ,'CM','NONE','NONE','CM','NONE'
			                    ,'NONE','NONE','CM','CM','NONE'
        			            ,'NONE','NONE','NONE','CM','NONE'
		        	            ,'NONE','CM'
				                )
        			  ,TRANLINE = 'NONE'
		        	  ,BANK = 'NONE'
        			  ,SAMPLE_TYPE = 'PHAB_SLOPE'
		        	  ,FLAG = as.character(NA)
        			  ,METHOD = 'TR'
		        	  ,stringsAsFactors=FALSE
			  )
	          ,data.frame(UID ='2003 WWYP99-0659 1 missing CM subsightings'
			             ,TRANSECT = c('A','A','A','B','B'
            			              ,'B','C','C','C','C'
                                      ,'C','C','C','C','C'
			                          ,'D','D','D','E','E'
			                          ,'E','F','F','F','G'
			                          ,'G','G','G','G','G'
			                          ,'H','H','H','I','I'
			                          ,'I','I','I','I','J'
			                          ,'J','J'
				                      )
			             ,RESULT = c('161','36','100','12','100'
			                        ,'110','50','30','75','193'
                                    ,NA,'16','124',NA,'20'
			                        ,'26','230','100','100','193'
			                        ,'8','100','120','18','50'
			                        ,'11','108','210',NA,'50'
			                        ,'246','100','14',NA,'50'
			                        ,'238','50','157','12','100'
			                        ,'100','1')
			             ,PARAMETER = c('BEARING','SLOPE','PROP','SLOPE','PROP'
			                           ,'BEARING','PROP3','PROP2','BEARING2','BEARING'
			                           ,'SLOPE2','SLOPE','BEARING3','SLOPE3','PROP'
			                           ,'SLOPE','BEARING','PROP','PROP','BEARING'
			                           ,'SLOPE','PROP','BEARING','SLOPE','PROP2'
			                           ,'SLOPE','BEARING2','BEARING','SLOPE2','PROP'
			                           ,'BEARING','PROP','SLOPE','SLOPE2','PROP'
			                           ,'BEARING2','PROP2','BEARING','SLOPE','PROP'
			                           ,'BEARING','SLOPE')
            			 ,LINE = as.numeric(NA)
                         ,UNITS = c('NONE','CM','NONE','CM','NONE'
            			           ,'NONE','NONE','NONE','NONE','NONE'
			                       ,'CM','CM','NONE','CM','NONE'
                                   ,'CM','NONE','NONE','NONE','NONE'
                                   ,'CM','NONE','NONE','CM','NONE'
			                       ,'CM','NONE','NONE','CM','NONE'
			                       ,'NONE','NONE','CM','CM','NONE'
			                       ,'NONE','NONE','NONE','CM','NONE'
			                       ,'NONE','CM')
			            ,TRANLINE = 'NONE'
			            ,BANK = 'NONE'
			            ,SAMPLE_TYPE = 'PHAB_SLOPE'
			            ,FLAG = as.character(NA)
			            ,METHOD = 'TR'
			            ,stringsAsFactors=FALSE
			            )
	         ,data.frame(UID = '2003 WWYP99-0659 1 slope unit NONE'
			            ,TRANSECT = c('A','A','A','B','B'
                                      ,'B','C','C','C','C'
			                          ,'C','C','C','C','C'
			                          ,'D','D','D','E','E'
			                          ,'E','F','F','F','G'
			                          ,'G','G','G','G','G'
			                          ,'H','H','H','I','I'
			                          ,'I','I','I','I','J'
			                          ,'J','J')
			            ,RESULT = c('161','36','100','12','100'
			                       ,'110','50','30','75','193'
			                       ,'4','12','124','0','20'
                                   ,'26','230','100','100','193'
			                       ,'8','100','120','18','50'
			                       ,'9','108','210','2','50'
			                       ,'246','100','14','10','50'
			                       ,'238','50','157','2','100'
			                       ,'100','1')
			            ,PARAMETER = c('BEARING','SLOPE','PROP','SLOPE','PROP'
                                      ,'BEARING','PROP3','PROP2','BEARING2','BEARING'
			                          ,'SLOPE2','SLOPE','BEARING3','SLOPE3','PROP'
			                          ,'SLOPE','BEARING','PROP','PROP','BEARING'
			                          ,'SLOPE','PROP','BEARING','SLOPE','PROP2'
			                          ,'SLOPE','BEARING2','BEARING','SLOPE2','PROP'
			                          ,'BEARING','PROP','SLOPE','SLOPE2','PROP'
			                          ,'BEARING2','PROP2','BEARING','SLOPE','PROP'
			                          ,'BEARING','SLOPE'
					                  )
			            ,LINE = as.numeric(NA)
			            ,UNITS = 'NONE'
			            ,TRANLINE = 'NONE'
			            ,BANK = 'NONE'
			            ,SAMPLE_TYPE = 'PHAB_SLOPE'
			            ,FLAG = as.character(NA)
			            ,METHOD = 'TR'
			            ,stringsAsFactors=FALSE
			            ) 
              )

  return(cg)
}


metsSlopeBearing.makeChannelGeometryOLD <- function()
# Create dataframe of channel geometry data for unit test
{
  tc <-textConnection("
         UID TRANSECT RESULT PARAMETER LINE UNITS TRANLINE BANK SAMPLE_TYPE FLAG
        '2000 WIDP99-0556 1' A 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' C 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' E 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' F 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' G 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' H 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' I 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' J 0.2 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' A 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' C 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' E 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' F 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' G 0.1 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' H 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' I 0.1 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' J 0.2 SLOPE 999 PERCENT NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 0.1 SLOPE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' A 150 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' C 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' E 70 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' F 60 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' G 60 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' H 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' I 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' J 30 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' A 150 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' C 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' E 70 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' F 60 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' G 60 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' H 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' I 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' J 30 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 80 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 360 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 40 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 330 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 120 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 50 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 90 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 200 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 180 BEAR 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 0.2 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 0.2 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 0.2 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 0.2 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 0.1 SLOPE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 140 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 40 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 140 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 40 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 340 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 10 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 350 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 40 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 20 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 330 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 140 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 340 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 200 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 160 BEAR 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 0.2 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 0.2 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 0.1 SLOPE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 20 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 20 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 20 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 100 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 60 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 80 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 80 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 220 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 240 BEAR 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' A PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' C PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' E PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' F PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' G PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' H PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' I PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' J PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' A PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' C PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' E PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' F PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' G PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' H PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' I PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' J PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J PERCENT NA NA NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' A 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 240 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' C 599.9999994 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 171.4285716 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' E 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' F 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' G 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' H 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' I 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' J 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' A 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 240 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' C 599.9999994 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 171.4285716 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' E 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' F 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' G 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' H 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' I 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' J 600 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 182.8571428 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 200 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 90 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 100 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 50 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 60 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 300 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 162.9629628 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 120 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 50 DISTANCE 999 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' B 360 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 171.4285716 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' B 360 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 171.4285716 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 160 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 100 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 170 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 50 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' E 350 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 90 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' G 100 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' H 237.0370372 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 130 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 210 DISTANCE 1 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1' D 257.1428574 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WIDP99-0556 1 slope unit PCT' D 257.1428574 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' A 57.1428572 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' B 100 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' C 140 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' D 250 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' F 250 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' I 150 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WSDP99-0531 1' J 140 DISTANCE 2 NONE NONE NONE PHAB_CHANBFRONT NA
        '2000 WAZP99-0505 1' A 7 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' A 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' A 354 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' B 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' B 357 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' C 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' C 11 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' C 4 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' D 7.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' D 3 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' D 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' E 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' E 9 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' E 12.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' F 17 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' F 5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' G 3.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' G 5 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' G 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' H 57 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' H 1.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' I 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' I 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' I 4 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' J 53 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0505 1' J 6.5 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 12 SLOPE3 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 10 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' B 10 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' C 12 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 11 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 13 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 8 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 22 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' F 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' G 10 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' H 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 12.2 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' I 14 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' J 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no incremnt' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 no slopes' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' B 10 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 only 2 slopes' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 12 SLOPE3 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 10 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' B 10 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' C 12 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 11 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 13 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 8 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 22 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' F 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' G 10 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' H 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 12.2 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' I 14 SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' J 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 34 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 58 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 33 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 0 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A NA SLOPE3 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 33 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' A 50 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 40 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' B 10 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 4 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 75 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 25 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 39 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' C NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 53 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 11 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' D 25 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 19 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 25 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 75 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 22 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' E 65 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 37 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' F 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 20 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 12 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 73 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 10 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G 80 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' G NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 107 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' H 6 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 12.2 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 45 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 70 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 30 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I 76 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' I NA SLOPE2 NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 8 SLOPE NA PERCENT NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2000 WAZP99-0569 1 missing PERCENT subsightings' J 23 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' A 161 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' A 36 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' A 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' B 12 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' B 110 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 50 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 30 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 75 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 4 SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 12 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 124 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 0 SLOPE3 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' C 20 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' D 26 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' D 230 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' D 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' E 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' E 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' E 8 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' F 120 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' F 18 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 9 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 108 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 210 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 2 SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' G 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' H 246 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' H 14 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 10 SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 238 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 157 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' I 2 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' J 100 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1' J 1 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' A 161 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' A 36 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' A 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' B 12 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' B 110 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 50 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 30 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 75 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 4 SLOPE2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 12 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 124 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 0 SLOPE3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' C 20 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' D 26 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' D 230 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' D 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' E 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' E 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' E 8 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' F 120 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' F 18 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 9 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 108 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 210 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 2 SLOPE2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' G 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' H 246 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' H 14 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 10 SLOPE2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 238 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 157 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' I 2 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' J 100 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 slope unit NONE' J 1 SLOPE NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' A 161 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' A 36 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' A 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' B 12 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' B 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' B 110 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 50 PROP3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 30 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 75 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C NA SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 16 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 124 BEARING3 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C NA SLOPE3 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' C 20 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' D 26 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' D 230 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' D 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' E 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' E 193 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' E 8 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' F 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' F 120 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' F 18 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 11 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 108 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 210 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G NA SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' G 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' H 246 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' H 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' H 14 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I NA SLOPE2 NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 50 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 238 BEARING2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 50 PROP2 NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 157 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' I 12 SLOPE NA CM NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' J 100 PROP NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' J 100 BEARING NA NONE NONE NONE PHAB_SLOPE NA
        '2003 WWYP99-0659 1 missing CM subsightings' J 1 SLOPE NA CM NONE NONE PHAB_SLOPE NA
       ")
  tt <- read.table(tc, header=TRUE,stringsAsFactors=FALSE)
  close(tc)
  tt$LINE <- as.numeric(tt$LINE)
  tt$FLAG <- as.logical(tt$FLAG)
  tt$METHOD <- 'TR'

  return(tt)
}

metsSlopeBearing.makeProtocols <- function()
# Create dataframe of protocol information for unit test
{
  return(data.frame('UID'=c('2000 WAZP99-0505 1'
                           ,'2000 WAZP99-0505 1 with clinometer'
                           ,'2000 WAZP99-0505 1 with low slope'
                           ,'2000 WAZP99-0505 1 with clinometer and low slope'
                           ,'2000 WAZP99-0569 1'
                           ,'2003 WWYP99-0659 1'
                           ,'2003 WWYP99-0659 1 slope unit NONE'
                           ,'2003 WWYP99-0659 1 missing CM subsightings'
                           ,'2000 WAZP99-0569 1 no incremnt'
                           ,'2000 WAZP99-0569 1 no slopes'
                           ,'2000 WAZP99-0569 1 only 2 slopes'
                           ,'2000 WAZP99-0569 1 missing PERCENT subsightings'

                           ,'2000 WIDP99-0556 1'
                           ,'2000 WIDP99-0556 1 with NA slopes'
                           ,'2000 WIDP99-0556 1 with absent slopes'
                           ,'2000 WIDP99-0556 1 slope unit PCT'
                           ,'2000 WSDP99-0531 1'
                           )
                   ,'PROTOCOL'=c('WADEABLE','WADEABLE','WADEABLE','WADEABLE'
                                ,'WADEABLE','WADEABLE','WADEABLE','WADEABLE'
                                ,'WADEABLE','WADEABLE','WADEABLE','WADEABLE'
                                ,'BOATABLE','BOATABLE','BOATABLE','BOATABLE'
                                ,'BOATABLE'
                                )
                   ,stringsAsFactors=FALSE
                   )
        )
}


metsSlopeBearing.makeExpectedResults <- function()
# Create dataframe of calculation results for unit test using field values.
#
# NOTE: '2000 WAZP99-0569 1 only 2 slopes' xslope and vslope values should be
#       NA when metric calculations remove calculations based on small sample
#       sizes, but are 5.99, 5.67099638511611 (respectively) when small sample
#       sizes are not taken into account (as when the study uses GPS based
#       slope estimations).
# NOTE: Sites with no slope infomration will not have xslope, xslope_field 
#       nor pctClinometer values -- not even NA.  These are not included in the
#       expected results.  Also boatable UIDs 2000 WIDP99-0556 1, 
#       2000 WIDP99-0556 1 slope unit PCT, and 2000 WSDP99-0531 1 will not 
#       have pct_Clinometer nor xslope_field values.  Additionally, xslope values
#       are removed (0.18, 0.18 and 0.10, respectively) as any field data for
#       boatable reaches is (currently) ignored (only 2 sites recorded any in 
#       2008-2009 so this is ok).
{
  expected <- rbind(data.frame('UID'='2000 WAZP99-0505 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('5.75','5.75','2.9930','10','15.0'
                                         ,'16.442','1.0680','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0505 1 with clinometer'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('5.75','5.75','2.9930','10','15.0'
                                         ,'16.442','1.0680','100'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0505 1 with low slope'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.575','0.575','0.29930','10','15.0'
                                         ,'16.442','1.0680','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0505 1 with clinometer and low slope'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.575','0.575','0.29930','10','15.0'
                                         ,'16.442','1.0680','100'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('10.8300','10.8300','3.5006','10','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('UID'='2003 WWYP99-0659 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.588307961','0.588307961','0.371371587','10','15.000'
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('UID'='2003 WWYP99-0659 1 slope unit NONE'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.588307961','0.588307961','0.371371587','10','15.000'
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('UID'='2003 WWYP99-0659 1 missing CM subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.588283195','0.588283195','0.371371003','10','15.000'
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 no incremnt'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('10.8300','10.8300','3.5006','10',NA
                                         ,NA,NA,'0'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 no slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c(NA, NA, NA,'0','15.000'
                                         ,'42.43476','1.1251',0
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 only 2 slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('5.99','5.99','5.67099638511611','2','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 missing PERCENT subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('7.554','7.554','4.236571465','10','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1 with NA slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c(NA,NA,NA,'0','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1 with absent slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c(NA,NA,NA,'0','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1 slope unit PCT'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WSDP99-0531 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.10000','0.10000','0.00000','10','400.00'
                                         ,'51.499','2.33470','0'
                                         )
                              )
                   )
  expected$UID <- as.character(expected$UID)
  expected$METRIC <- as.character(expected$METRIC)
  expected$RESULT <- as.character(expected$RESULT)

  return(expected)
}


metsSlopeBearing.makeExpectedResultsWithGIS <- function()
# Create dataframe of calculation results for unit test using field values 
# AND map based values
#
# NOTE: '2000 WAZP99-0569 1 only 2 slopes' xslope and vslope values should be
#       NA when metric calculations remove calculations based on small sample
#       sizes, but are 5.99, 5.67099638511611 (respectively) when small sample
#       sizes are not taken into account (as when the study uses GPS based
#       slope estimations).
# NOTE: Sites with no slope infomration will not have xslope, xslope_field 
#       nor pctClinometer values -- not even NA.  These are not included in the
#       expected results.  Also boatable UIDs 2000 WIDP99-0556 1, 
#       2000 WIDP99-0556 1 slope unit PCT, and 2000 WSDP99-0531 1 will not 
#       have pct_Clinometer nor xslope_field values.  Additionally, xslope values
#       are removed (0.18, 0.18 and 0.10, respectively) as any field data for
#       boatable reaches is (currently) ignored (only 2 sites recorded any in 
#       2008-2009 so this is ok).
{
  expected <- rbind(data.frame('UID'='2000 WAZP99-0505 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer','xslope_map'
                                         )
                              ,'RESULT'=c('5.75','5.75','2.9930','10','15.0'
                                         ,'16.442','1.067600000001','0','12.3456'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0505 1 with clinometer'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('5.75','5.75','2.9930','10','15.0'
                                         ,'16.442','1.0680','100'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0505 1 with low slope'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.575','0.575','0.29930','10','15.0'
                                         ,'16.442','1.0680','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0505 1 with clinometer and low slope'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.575','0.575','0.29930','10','15.0'
                                         ,'16.442','1.0680','100'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer','xslope_map'
                                         )
                              ,'RESULT'=c('10.8300','10.8300','3.5006','10','15.000'
                                         ,'42.43476','1.1251','0','10.82999999999999'
                                         )
                              )
                   ,data.frame('UID'='2003 WWYP99-0659 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.588307961','0.588307961','0.371371587','10','15.000'
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('UID'='2003 WWYP99-0659 1 slope unit NONE'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.588307961','0.588307961','0.371371587','10','15.000'
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('UID'='2003 WWYP99-0659 1 missing CM subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.588283195','0.588283195','0.371371003','10','15.000'
                                         ,'161.8415','1.66858','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 no incremnt'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('10.8300','10.8300','3.5006','10',NA
                                         ,NA,NA,'0'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 no slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c(NA, NA, NA,'0','15.000'
                                         ,'42.43476','1.1251',0
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 only 2 slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('5.99','5.99','5.67099638511611','2','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WAZP99-0569 1 missing PERCENT subsightings'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('7.554','7.554','4.236571465','10','15.000'
                                         ,'42.43476','1.1251','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1 with NA slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer','xslope_map'
                                         )
                              ,'RESULT'=c(NA,'0.01234',NA,'0','600.00'
                                         ,'59.395','1.23583','0','0.01234'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1 with absent slopes'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer','xslope_map'
                                         )
                              ,'RESULT'=c(NA,'0.567890',NA,'0','600.00'
                                         ,'59.395','1.23583','0','0.567890'
                                         )
                              )
                   ,data.frame('UID'='2000 WIDP99-0556 1 slope unit PCT'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.18000','0.18000','0.04216','10','600.00'
                                         ,'59.395','1.23583','0'
                                         )
                              )
                   ,data.frame('UID'='2000 WSDP99-0531 1'
                              ,'METRIC'=c('xslope_field','xslope','vslope','nslp','transpc'
                                         ,'xbearing','sinu','pctClinometer'
                                         )
                              ,'RESULT'=c('0.10000','0.10000','0.00000','10','400.00'
                                         ,'51.499','2.33470','0'
                                         )
                              )
                   )
  expected$UID <- as.character(expected$UID)
  expected$METRIC <- as.character(expected$METRIC)
  expected$RESULT <- as.character(expected$RESULT)

  return(expected)
}


metsSlopeBearing.makeGisCalcs <- function()
# Creates a dataframe of fake GIS calculations of XSLOPE and SINU.
{
    gisCalcs <- rbind(data.frame('UID'='2000 WAZP99-0505 1'
                                ,'METRIC'=c('xslope','sinu')
                                ,'RESULT'=c('12.3456','1.067600000001')
                                ,stringsAsFactors=FALSE
                                )
                     ,data.frame('UID'='2000 WAZP99-0569 1'
                                ,'METRIC'=c('xslope')
                                ,'RESULT'=c('10.82999999999999')
                                ,stringsAsFactors=FALSE
                                )
                     ,data.frame('UID'='2000 WIDP99-0556 1 with NA slopes'
                                ,'METRIC'='xslope'
                                ,RESULT='0.01234'
                                ,stringsAsFactors=FALSE
                                )
                     ,data.frame('UID'='2000 WIDP99-0556 1 with absent slopes'
                                ,'METRIC'='xslope'
                                ,RESULT='0.567890'
                                ,stringsAsFactors=FALSE
                                )
                     ,data.frame('UID'='An extra reach not in the main expected results'
                                ,'METRIC'=c('xslope','xsinu')
                                ,'RESULT'=c('1.5999999999999','1.101010101')
                                ,stringsAsFactors=FALSE
                                )
                     )

  return(gisCalcs)
}

# end of file
