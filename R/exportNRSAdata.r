# exportNRSAdata.r
#
# Exports recorded lat/lon data for each transect and subsighting to csv file
# for processing by GIS to get backsighting angle (0-360) and distance in meters
# between the points.
#
# Assumes NRSAvalidation.r has been sourced.
#
# 06/30/10 cws created
# 09/13/10 cws turned into a function definition.
# 09/15/10 cws rewrote to use determineBestLatLon().
# 11/03/10 cws modified a bit for New Mexico.
#

require(RODBC)

exportNRSAdata <- function()
# Read in relevant data and subset it appropriately.  For now, get GPS_DATUM from
# nrsa:VALIDATION.          \
{
  nrsa2 <- odbcConnect(NRSAdbName)
  nrsa <- odbcConnect(NRSAdbName)   # or wherever the VERIFICATION table is kept

  cg <- fetchNRSATable(nrsa2, 'tblCHANNELGEOMETRY2')
  if (!is.data.frame(cg)) {
      stop(sprintf("Could not open tblCHANNELGEOMETRY2: %s", cg))
  }

  visits <- fetchNRSATable(nrsa2, 'tblVISITS2')
  if (!is.data.frame(visits)) {
      stop(sprintf("Could not open tblVISITS2: %s", visits))
  }

  ver <- sqlFetch(nrsa, 'VERIFICATION', stringsAsFactors=FALSE)
  if (!is.data.frame(ver)) {
      stop(sprintf("Could not open nrsa:VERIFICATION: %s", ver))
  }
  datum <- ver[c('SITE_ID','DATE_COL','VISIT_NO','GPS_DATUM')]

  visits <- merge(visits, datum, by=c('SITE_ID','DATE_COL','VISIT_NO'), all.x=TRUE)

  tt <- merge(visits[c('SITE_ID','DATE_COL','VISIT_NO','UID','GPS_DATUM')]
             ,cg
             ,by='UID'
             ,all.y=TRUE
             )
  tt <- subset(tt, grepl('^(LAT|LON).*', PARAMETER))

  # Make sure parameter names are standardized
  tt$PARAMETER <- ifelse(tt$PARAMETER=='LATSS2', 'LATSS'
                 ,ifelse(tt$PARAMETER=='LATDD2', 'LATDD'
                 ,ifelse(tt$PARAMETER=='LATMM2', 'LATMM'
                 ,ifelse(tt$PARAMETER=='LATSS2', 'LATSS'
                 ,ifelse(tt$PARAMETER=='LONGDD2', 'LONGDD'
                 ,ifelse(tt$PARAMETER=='LONGMM2', 'LONGMM'
                 ,ifelse(tt$PARAMETER=='LONGSS2', 'LONGSS', tt$PARAMETER
                 )))))))

  # Figure out which set of coordinates to use in calculating transect distances.
  selected <- determineBestLatLon(tt)
  
  # Use best lat/lon determination to select rows for output:
#  latlon <- subset(tt
#                  ,paste(UID,TRANSECT,LINE,TRANLINE) %in%
#                   paste(selected$UID,selected$TRANSECT,selected$LINE,selected$TRANLINE)
#                  )
  latlon <- merge(visits, selected, by=c('UID'))#,'TRANSECT','LINE','TRANLINE'))
  

  # The main lat/lon is recorded at the top of the form, and any supplemental
  # sightings on the way to the transect are recorded at the bottom of the form.
  # Order rows with supplemental readings by line, and put rows with main (top)
  # values after the supplemental
  latlon$LINE[is.na(latlon$LINE)] <- 999

  latlon <- latlon[order(latlon$UID,latlon$TRANSECT,latlon$LINE),]

  # write data to csv after getting rid of unnecessary columns
  write.csv(latlon[c('UID','SITE_ID','DATE_COL','VISIT_NO','TRANSECT','LINE','lat','lon','TRANLINE','GPS_DATUM')]
           ,paste(NRSACalcLocation, 'nrsaLatLon.csv', sep='/')
           ,row.names=FALSE
           )

}

# end of file