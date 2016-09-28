#  nrsaLittoralDepthTest.r
# 
#  2/22/16 cws rewritten for new calling interface.
#

nrsaLittoralDepthTest <- function()
# Unit test for nrsaLittoralDepth
# IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
# has only wadable sites.  The  metsLittoralDepth.1 function needs data for
# both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
# were set to zero.
{
  # Create correctly formated test data, and run data through metsLittoralDepth.1
  testData <- nrsaLittoralDepthTest.inputdata()

  actual <- nrsaLittoralDepth(bLittoralDepth = testData)

  metsExpected <- nrsaLittoralDepthTest.inputmetrics()

  # compare results from baseData (actual) with expectedResults  (metsExpected)

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  tt <- merge(actual, metsExpected, by=c('SITE','METRIC'),all=TRUE)
  tt$diff <- tt$VALUE - tt$EXPECTED

  errs <- subset(tt, abs(diff) > 10^-7 | is.na(VALUE) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Littoral Depth metrics are broken"
             )

  actual <- nrsaLittoralDepth(bLittoralDepth = subset(testData, PARAMETER %nin% c('POLE','SONAR')))
  checkEquals(NULL, actual
             ,"Error: Littoral Depth metrics do not handle stream-only studies"
             )
}


nrsaLittoralDepthTest.inputdata <-function()
{
    newdat <- rbind(expand.grid(LINE=1:5
                               ,TRANSECT = LETTERS[1:11]
                               ,SITE = c('OSM04461-0004','WCAP99-0585','WWYP99-0555')
                               ,SAMPLE_TYPE = 'PHAB_CHANBFRONT'
                               ,PARAMETER='SONAR'
                               )
                   )

    newdat$VALUE <- c(0.03048,0,0,0.03048,0.06096,0.1524,0.1524,0.12192,0.06096,
                      0.06096,0.12192,0.18288,0.06096,0.06096,0.03048,0.1524,0.12192,0.03048,
                      0.03048,0.06096,0.09144,0.06096,0.18288,0.1524,0.18288,0.27432,0.42672,
                      0.4572,0.51816,0.48768,0.06096,0.06096,0.12192,0.1524,0.18288,0.03048,
                      0.09144,0.06096,0.09144,0.1524,NA,NA,NA,NA,NA,0.06096,0.03048,0.06096,
                      0.06096,0.24384,0.03048,0.09144,0.09144,0.09144,0.09144,0.4,0.5,0.3,0.2,0.2,
                      0.2,0.2,0.3,0.4,0.4,0.2,0.3,0.3,0.2,0.3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,0.3,0.4,0.7,0.8,1.1,0.3,0.5,0.6,0.9,1.2,0.2,0.3,0.4,0.4,0.5,0.6,0.7,0.8,0.5,
                      0.3,0.3,0.4,0.5,0.4,0.4,0.2,0.3,0.6,0.5,0.5,0.5,0.4,0.3,0.2,0.2,1,0.8,0.3,0.6,0.4,0.3,
                      0.5,0.4,0.5,0.3,0.1,0.1,0.3,0.3,0.2,0.8,0.5,0.4,0.4,0.2
                     )
 
 
    newdat$SITE <- as.character(newdat$SITE)
    newdat$TRANSECT <- as.character(newdat$TRANSECT)
    newdat$SAMPLE_TYPE <- as.character(newdat$SAMPLE_TYPE)
    newdat$PARAMETER <- as.character(newdat$PARAMETER)

    return(newdat)
 
}


nrsaLittoralDepthTest.inputmetrics <-function()
{
    newdat<- rbind(data.frame(SITE = c('OSM04461-0004','WCAP99-0585','WWYP99-0555')
                             ,METRIC='xlit'
                             ,VALUE=c(0.1243584,0.2933333333,0.4654545455)
                             ) 
                  ,data.frame(SITE = c('OSM04461-0004','WCAP99-0585','WWYP99-0555')
                             ,METRIC='vlit'
                             ,VALUE=c(0.120014099,0.0961150105,0.2405381173)
                             ) 
                  ,data.frame(SITE = c('OSM04461-0004','WCAP99-0585','WWYP99-0555')
                             ,METRIC='mxlit'
                             ,VALUE=c(0.51816,0.5,1.2)
                             ) 
                  ,data.frame(SITE = c('OSM04461-0004','WCAP99-0585','WWYP99-0555')
                             ,METRIC='mnlit'
                             ,VALUE=c(0,0.2,0.1)
                             )       
                  )
    newdat<- rename(newdat, 'VALUE','EXPECTED')

    return(newdat)
}

# end of file