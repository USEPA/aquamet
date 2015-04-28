# determineBestLatLon.r
#
# 09/15/10 cws Created
#

determineBestLatLon<- function(df)
# Determines which set of coordinates at a transect to use when calculating
# transect spacing.
# Returns a dataframe of UID, TRANSECT, LINE, TRANLINE of the best coordinates
# to use, along with the best coordinates (lat and lon) in decimal degrees and
# latdms and londms as character strings formated as degrees:minutes:seconds
#
# ARGUMENTS:
# df       dataframe with lat/lon values from tblCHANNELGEOMETRY2.  This may
#            include all lat/lon rows or a subset of 'interesting' rows.
#
{
  # Convert separate DMS values to single decimal degrees value.  Gotta transpose
  # to wide first.  It's OK if the resulting value is NA due to incomplete recording,
  # those rows will be removed in the next step.
  ww <- dfWiden(df[c('UID','TRANSECT','LINE','TRANLINE','PARAMETER','RESULT')]
               ,c('UID','TRANSECT','LINE','TRANLINE')
               ,'PARAMETER','RESULT'
               )

  ww$topLat <- ww$LATDD_TOP  + ww$LATMM_TOP/60  + ww$LATSS_TOP/3600
  ww$topLon <- ww$LONGDD_TOP + ww$LONGMM_TOP/60 + ww$LONGSS_TOP/3600
  ww$botLat <- ww$LATDD      + ww$LATMM/60      + ww$LATSS/3600
  ww$botLon <- ww$LONGDD     + ww$LONGMM/60     + ww$LONGSS/3600

  ww$topLatdms <- sprintf("%02d:%02d:%04.1f",as.integer(ww$LATDD_TOP),as.integer(ww$LATMM_TOP),ww$LATSS_TOP)
  ww$topLondms <- sprintf("%02d:%02d:%04.1f",as.integer(ww$LONGDD_TOP),as.integer(ww$LONGMM_TOP),ww$LONGSS_TOP)
  ww$botLatdms <- sprintf("%02d:%02d:%04.1f",as.integer(ww$LATDD),as.integer(ww$LATMM),ww$LATSS)
  ww$botLondms <- sprintf("%02d:%02d:%04.1f",as.integer(ww$LONGDD),as.integer(ww$LONGMM),ww$LONGSS)

  # Get rid of rows where either lat or long are missing, i.e. keep rows that have
  # both lat & lon (check values from top & bottom of form separately).
  # On 02Aug2010 this is 39 rows.
  ww <- subset(ww, (!is.na(topLat) & !is.na(topLon)) |
                   (!is.na(botLat) & !is.na(botLon))
              )


  # Try to use location values taken midstream.  Where those aren't available at
  # a transect, use the values taken at the bank.  If neither are available, then
  # the row has supplemental lat/lon values, so use those.  This is easiest to do
  # when all the pertainent values are on the same row.  Getting them there is easy
  # in sql or proc transpose, but reshape() somehow only returns the rows with
  # supplemental data plus the first row in the dataframe.  Thus we split them
  # by TRANLINE and remerge the parts.
  # NOTE: On 02Aug2010, 9602 locations use midstream values, 81 locations use
  # the at-bank values, and there are another 24 supplemental values for a total
  # of 9707 rows.
  bank <- rename(subset(ww[c('UID','TRANSECT','TRANLINE','LINE','topLat','topLon'
                            ,'topLatdms','topLondms'
                            )]
                       ,TRANLINE=='BANK'
                       ,select=-TRANLINE
                       )
                ,c('topLat','topLon','topLatdms','topLondms')
                ,c('bankLat','bankLon','bankLatdms','bankLondms')
                )
  mid <-  rename(subset(ww[c('UID','TRANSECT','TRANLINE','LINE','topLat','topLon'
                            ,'topLatdms','topLondms'
                            )]
                       ,TRANLINE=='MID'
                       ,select=-TRANLINE
                       )
                ,c('topLat','topLon','topLatdms','topLondms')
                ,c('midLat','midLon','midLatdms','midLondms')
                )
  sup <-  rename(subset(ww[c('UID','TRANSECT','TRANLINE','LINE','botLat','botLon'
                            ,'botLatdms','botLondms'
                            )]
                       ,TRANLINE=='NONE'
                       ,select=-TRANLINE
                       )
                ,c('botLat','botLon','botLatdms','botLondms')
                ,c('supLat','supLon','supLatdms','supLondms')
                )

  latlon <- merge(merge(bank, mid
                       ,by=c('UID','TRANSECT','LINE')
                       ,all=TRUE
                       )
                 ,sup
                 ,by=c('UID','TRANSECT','LINE')
                 ,all=TRUE
                 )

  latlon$lat <- ifelse(!is.na(latlon$midLat) & !is.na(latlon$midLon), latlon$midLat
               ,ifelse(!is.na(latlon$bankLat) & !is.na(latlon$bankLon), latlon$bankLat
                      ,latlon$supLat
               ))
  latlon$lon <- ifelse(!is.na(latlon$midLat) & !is.na(latlon$midLon), latlon$midLon
               ,ifelse(!is.na(latlon$bankLat) & !is.na(latlon$bankLon), latlon$bankLon
                      ,latlon$supLon
               ))
  latlon$TRANLINE <- ifelse(!is.na(latlon$midLat) & !is.na(latlon$midLon), 'MID'
                    ,ifelse(!is.na(latlon$bankLat) & !is.na(latlon$bankLon), 'BANK'
                           ,'NONE'
                    ))
  latlon$latdms <- ifelse(latlon$TRANLINE=='MID', latlon$midLatdms
                  ,ifelse(latlon$TRANLINE=='BANK',latlon$bankLatdms
                         ,latlon$supLondms
                    ))
  latlon$londms <- ifelse(latlon$TRANLINE=='MID', latlon$midLondms
                  ,ifelse(latlon$TRANLINE=='BANK',latlon$bankLondms
                         ,latlon$supLondms
                   ))

  latlon <- latlon[c('UID','TRANSECT','LINE','TRANLINE','lat','lon','latdms','londms')]
  return(latlon)
}


# end of file