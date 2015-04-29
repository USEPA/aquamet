metsSlopeBearing <- function(thalweg, channelgeometry, visits, gisCalcs=NULL) {
  
################################################################################
# Function: metsSlopeBearing
# Title: Calculate NRSA Slope and Bearing Metrics
# Programmers: Curt Seeliger
#              Suzanne San Romani
#              Tom Kincaid
# Date: February 1, 2010
# Description:
#   This function calculates the slope and bearing portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires data frames containing the thalweg, channel geometry, and
#   stream verification form data files.
# Function Revisions:
#   02/01/10 cws: Created.
#   03/10/10 cws: Updated as needed for change in
#            nWadeableStationsPerTransect().
#   03/11/10 cws: Call to writeNRSACalcResults() corrected.
#   03/22/10 cws: Moved unit test dataframes to separate functions
#   03/25/10 cws: Changed diff() calls to dfCompare(), nlaLengthen() to
#            dfLengthen().
#   04/09/10 ssr: Modified unit test to include stream only and boatable only
#            options.
#            Modified metsSlopeBearing.1 to allow stream or river only options.
#   04/22/10 cws: Cleaned up unit test and added case for site with slopes
#            measured as changes in elevation.  Updated calculations to handle
#            these sites.
#   05/27/10 cws: Updated code to use INCREMNT occuring only at A 0, reflecting
#            the current data organization.  Modified unit test accordingly.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   10/08/10 cws: Corrected use of LINE column in boatable data. Changed test
#            data accordingly (LINE=1,2,3 became LINE=999,1,2)
#   11/09/10 cws: Removed check for small sample sizes (N<=2) for slopes which
#            caused xslope and vslope to be set to NA in these cases.  This was
#            done to allow use of GPS-based xslope estimations, which have only
#            one value per site.  Unit test changed accordingly.
#   03/18/11 cws: Changing default slope units from PERCENT to CM when slope
#            units are NONE in wadeable protocol sites; boatable sites still
#            assume PERCENT slopes unless specified in CM.  In cases where the
#            protocol is unknown (NONE), they are handled as if wadeable.
#            Elevation changes (SLOPE values when units are CM) are adjusted to
#            be split proportionately over the transect when they are not
#            recorded for subsightings.  This change was made to work with a
#            modification to the field practice when recording elevations when
#            using a water tube (crews recorded the elevation for the entire
#            transect rather than for each subsighting).  Unit test updated to
#            test elevation adjustment.  Functions providing test data to the
#            unit test were rewritten to make modifications to the data easier.
#   08/17/11 cws: Incorporated GIS based calculations of sinuosity and slope.
#            Updated unit test accordingly.
#   11/29/11 cws: Incorporated GIS based calculations assumed all metrics 
#            would be calculated for all UIDs in the calculations which isn't 
#            true, and unit test used same method of incorporating GIS 
#            calculation results, occluding the error.  Modified code and 
#            unit test accordingly.
#   12/19/11 cws: Restandardized names of individual metrics files, removing 
#            WITHGPSSLOPES and the like.
#   03/08/12 cws: Reading GPS based calculations from
#            gpsBasedCalculations_asOf201203008.csv
#   03/27/12 cws: Added metrics xslope_field, xslope_map and pctClinometer as
#            requested.  Now xslope is the prefered choice of field and map
#            values (if both exist); vslope is unchanged.  The value of sinu
#            still takes on the map value over the field value if both values
#            exist.  Added test data to test ability to correctly select which
#            xslope value to use.
#   08/03/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify names of the data files.  Added argument NRSAdir to the
#            function to identify the directory from which data files are read
#            and to which the output metrics file is written.  Set the GPS-based
#            calculations data frame (gisCalcs) equal to NULL.
#   12/21/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Modified output to be a data frame rather
#            than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   06/02/14 cws: Modified creation of slopeMethods dataframe (used for determining
#            pctClinometer) to allow for case when slope data is present for ALL sites 
#            in a study (e.g. Calapooia). No change to unit test.
# Arguments:
#   thalweg = a data frame containing the thalweg data file.  The data frame
#     must include columns that are named as follows:
#       UID - universal ID value
#       SAMPLE_TYPE - sample type
#       TRANSECT - transect label
#       STATION - station number along thalweg between transects
#       PARAMETER - identifier for each measurement, assessment, score, etc.
#       RESULT - measurement associated with PARAMETER column
#       UNITS - units of the RESULT measurement
#       FLAG - flag
#   channelgeometry = a data frame containing the channel geometry data file.
#     The data frame must include columns that are named as follows:
#       UID - universal ID value
#       SAMPLE_TYPE - sample type
#       TRANSECT - transect label
#       TRANLINE - location (mid-channel or bank)along transect
#       BANK - bank (left or right) along transect
#       LINE - way point (1,2, etc.) between transects
#       METHOD - method used to measure slope
#       PARAMETER - identifier for each measurement, assessment, score, etc.
#       RESULT - measurement associated with PARAMETER column
#       UNITS - units of the RESULT measurement
#       FLAG - flag
#   visits = a data frame containing the stream verification form data file.
#     The data frame contains a protocol value for each UID value.  It also
#     should include columns that relate UID to values meaningfull to the user,
#     e.g., site ID, date collected, and visit number.  The data frame must
#     include columns that are named as follows:
#       UID - universal ID value
#       VALXSITE - protocol used during a site visit (BOATABLE, PARBYBOAT,
#         ALTERED, INTWADE, PARBYWADE, WADEABLE)
#   gisCalcs = a data frame containing metric values that were determined using
#     GPS based calculations.  The default value for this argument is NULL.  The
#     data frame must include columns that are named as follows:
#       UID - universal ID value
#       METRIC - metric name
#       RESULT - metric value
#   Note that possible values for variables in the input data frames are
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
#   siteProtocol determine sampling protocol values
#   metsSlopeBearing.1 - calculate metrics
################################################################################

# Print an initial message
  cat('Slope and Bearing calculations:\n')

# Convert factors to character variables in the input data frames
  intermediateMessage('.1 Convert factors to character variables.', loc='end')
  thalweg <- convert_to_char(thalweg)
  channelgeometry <- convert_to_char(channelgeometry)
  visits <- convert_to_char(visits)
  if(!is.null(gisCalcs))
    gisCalcs <- convert_to_char(gisCalcs)

# Determine protocol used for each site
  intermediateMessage('.2 Set protocols.', loc='end')
  if(is.null(gisCalcs)) {
    protocols <- siteProtocol(c(unique(channelgeometry$UID), unique(thalweg$UID)),
      visits)
  } else {
    protocols <- siteProtocol(c(unique(channelgeometry$UID), unique(thalweg$UID),
      unique(gisCalcs$UID)), visits)
  }

# Calculate the metrics
  intermediateMessage('.3 Call function metsSlopeBearing.1.', loc='end')
  mets <- metsSlopeBearing.1(thalweg, channelgeometry, gisCalcs, protocols)
  row.names(mets) <- 1:nrow(mets)

# Print an exit message
  intermediateMessage('Done.', loc='end')

# Return results
  return(mets)
}



metsSlopeBearing.1 <- function(thal, chanGeom, gisCalcs, protocols) {

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



metsSlopeBearing.adjustElevations <- function(df) {

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



# end of file
