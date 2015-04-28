# valGISCalculations.r
#
# 08/26/10 cws created
# 09/16/10 cws Removing hardcoding of NRSA database name, using NRSAdbName
#          instead. Also greatly modified selection of errant distances to check.
# 11/04/10 cws made LOC and TRANLINE matching case-insensitive to allow for that
#          variation in results from GIS calculations

#require(plyr)
require(RODBC)

valGISCalculations <- function()
# Imports calculated backbearing and distance values based on recorded lat/lon
# data for each transect and subsighting from a csv file, and creates a
# file of rows for validation.  Backsighting angle is (0-360) and distance in
# meters between the points.
#
# ASSUMPTIONS:
# GIS calculations are in path specified by NRSACalcLocation and file is named
#   backBearings.csv.
# NRSAvalidation.r has been sourced.
{
  chan <- odbcConnect(NRSAdbName)

  metadata <- fetchNRSATable(chan, 'tblPARAMETERDESCRIPTIONS2')
  if(!is.data.frame(metadata)) return(metadata)

  siteInfo <- fetchNRSATable(chan, 'tblVISITS2', filterSideChannels=FALSE)
  if(!is.data.frame(siteInfo)) return(siteInfo)

  cg <- fetchNRSATable(chan, 'tblCHANNELGEOMETRY2')
  if (!is.data.frame(cg)) {
      return(sprintf("Could not open tblCHANNELGEOMETRY2: %s", cg))
  }

  gisCalcs <- read.csv(paste(NRSACalcLocation,'backBearings.csv',sep=''), stringsAsFactors=FALSE)

  results <- valGISCalculations.1(cg, gisCalcs, metadata, siteInfo)
  writeNRSAValidationResults(rr
                            ,paste(NRSAvalidationLocation
                                  ,'valGISCalculations.csv'
                                  ,sep=''
                                  )
                            )

  return(NULL)
}

valGISCalculations.1 <- function(cg, gisCalcs, metadata, siteInfo)
# Check dataframe of gis calculations of distance and backbearing for
# results indicative of incorrect lat/lon values.
#
# ARGUMENTS:
# cg        dataframe of channel geometry
# gisCalcs  dataframe of calculations of distance and backbearing.  Expected to
#             have columns UID, TRANSECT, LATITUDE, LONGITUDE, DISTANCE, BEARING
#             but may have others as well.                                              n
# metadata  dataframe of metadata for parameters, used for form information.
# siteInfo  dataframe of site information, relating internal UID values to
#             site_id and
#
{
  # Remove unnecessary parameters from the channel geometry data.  Convert
  # NA LINE values to 999 to match what we exported.
  # Make sure parameter names are standardized.  Use the returned values of
  # gisCalcs::loc to determine which set of coordinates was used (mid, bank or
  # sup).
  cg$PARAMETER <- ifelse(cg$PARAMETER=='LATSS2', 'LATSS'
                 ,ifelse(cg$PARAMETER=='LATDD2', 'LATDD'
                 ,ifelse(cg$PARAMETER=='LATMM2', 'LATMM'
                 ,ifelse(cg$PARAMETER=='LATSS2', 'LATSS'
                 ,ifelse(cg$PARAMETER=='LONGDD2', 'LONGDD'
                 ,ifelse(cg$PARAMETER=='LONGMM2', 'LONGMM'
                 ,ifelse(cg$PARAMETER=='LONGSS2', 'LONGSS', cg$PARAMETER
                 )))))))
  tt <- subset(cg, PARAMETER %in% c('LATDD','LATDD_TOP','LATMM','LATMM_TOP'
                                   ,'LATSS','LATSS_TOP','LONGDD','LONGDD_TOP'
                                   ,'LONGMM','LONGMM_TOP','LONGSS','LONGSS_TOP'
                                   )
                  )
  tt$LINE[is.na(tt$LINE)] <- 999
  gisCalcs$LOC <- toupper(gisCalcs$LOC)
  latlon <- subset(merge(tt, gisCalcs[c('UID','TRANSECT','LOC')]
                        ,by=c('UID','TRANSECT'), all.x=TRUE
                        )
                  ,LOC=='BANK' & TRANLINE=='BANK' |
                   LOC=='MID' & TRANLINE=='MID' |
                   LOC=='SUP' & TRANLINE=='NONE'
                  )
  

  # Look for outlier distances, flag-ing possibly incorrect lat/lon values and
  # removing rows with NA or FALSE flag values.  A distance is an outlier if
  # it is more than 2 standard deviations away from the site mean.
  distanceStats <- merge(gisCalcs
                        ,merge(aggregate(list(mean=gisCalcs$DISTANCE)
                                        ,list(UID=gisCalcs$UID)
                                        ,mean, na.rm=TRUE
                                        )
                              ,aggregate(list(sd=gisCalcs$DISTANCE)
                                        ,list(UID=gisCalcs$UID)
                                        ,sd, na.rm=TRUE
                                        )
                              ,by='UID'
                              )
                        ,by='UID'
                        )
  flagged <- subset(transform(distanceStats, sdFromMean=abs(DISTANCE-mean)/sd)
                   ,sdFromMean > 2
                   )

  # Flag rows for the two transects involved in the flagged distance values. The
  # distance at a transect is between it and the location of the next lat/lon
  # (e.g. distance at transect J-main is between J-main and K-main; distance
  # at E-1 is between E-1 and E-main or E-2, whatever comes next).
  keys <- latlon[c('UID','TRANSECT','LINE')]
  rownames(keys) <- NULL
  flaggedRows <- rownames(subset(keys
                                , paste(UID, TRANSECT, LINE) %in%
                                  paste(flagged$UID, flagged$TRANSECT, flagged$LINE)
                                )
                         )
  involvedKeys <- keys[rownames(keys) %in%
                       c(flaggedRows, as.character(1+as.integer(flaggedRows)))
                      ,]
  involvedLatLons <- subset(latlon
                           ,paste(UID, TRANSECT) %in%
                            paste(involvedKeys$UID, involvedKeys$TRANSECT)
                           )
  involvedLatLons <- involvedLatLons[order(involvedLatLons$UID,involvedLatLons$TRANSECT),]

  # Create useful text in testDescription.  Organize lat/lon values for simplified
  # review.
  tt <- dfWiden(involvedLatLons, c('UID','TRANSECT','LINE','TRANLINE','BANK'),'PARAMETER','RESULT', makeNumeric=FALSE)
  tt$TESTDESCRIPTION <- ifelse(tt$TRANLINE=='NONE'
                              ,sprintf("Possibly incorrect supplemental Lat/Lon %s:%s:%s, %s:%s:%s"
                                      ,tt$LATDD, tt$LATMM, tt$LATSS
                                      ,tt$LONGDD, tt$LONGMM, tt$LONGSS
                                      )
                              ,sprintf("Possibly incorrect main Lat/Lon %s:%s:%s, %s:%s:%s"
                                      ,tt$LATDD_TOP, tt$LATMM_TOP, tt$LATSS_TOP
                                      ,tt$LONGDD_TOP, tt$LONGMM_TOP, tt$LONGSS_TOP
                                      )
                              )
 reviewThese <- merge(involvedLatLons
                     ,tt[c('UID','TRANSECT','LINE','TESTDESCRIPTION')]
                     ,by=c('UID','TRANSECT','LINE')
                     )
  
  # Create dataframe for output to validation
  rr <- constructNRSAValidationResults(reviewThese, metadata, siteInfo, ssFmt = "EXCEL")

  return(rr)
}


valGISCalculationsTest <- function()
#
{

}
# end of file