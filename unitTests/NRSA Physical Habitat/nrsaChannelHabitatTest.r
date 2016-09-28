#  nrsaChannelHabitatTest.r
# RUnit tests


nrsaChannelHabitatTest <- function()
# Unit test for nrsaChannelHabitat
# IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
# has only wadable sites.  The nrsaChannelHabitat function needs data for
# both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
# were set to zero.
{
  intermediateMessage('Channel Habitat test of data', loc='end')

  # Create test datasets; first mixed protocol, then stream only and river only
  testData <- nrsaChannelHabitatTest.inputData()
  metsExpected <- nrsaChannelHabitatTest.testResults()
  metsExpected <- rename(metsExpected, 'VALUE','EXPECTED')
  
  riverData <- subset(testData, SAMPLE_TYPE=='PHAB_THAL')
  riverMets <- subset(metsExpected, SITE %in% unique(riverData$SITE))

  streamData <- subset(testData, SAMPLE_TYPE=='PHAB_THALW')
  streamMets <- subset(metsExpected, SITE %in% unique(streamData$SITE))


  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.

  # Check mixed protocol calculations
  testDataResult<- nrsaChannelHabitat(bChannelUnit=riverData
                                     ,wChannelUnit=streamData
                                     )
  tt <- merge(testDataResult, metsExpected, by=c('SITE','METRIC'), all=T)
  tt$diff <- tt$VALUE - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(VALUE) != is.na(EXPECTED))

  checkEquals(0, nrow(errs)
             ,"Error: Channel Habitat calculations with mixed protocol data are broken"
             )

  # Check boatable protocol calculations
  testDataResult<- nrsaChannelHabitat(bChannelUnit=riverData)
  tt <- merge(testDataResult, riverMets, by=c('SITE','METRIC'), all=T)
  tt$diff <- tt$VALUE - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(VALUE) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Channel Habitat calculations with boatable protocol data are broken"
             )

  # Check mixed protocol calculations
  testDataResult<- nrsaChannelHabitat(wChannelUnit=streamData)
  tt <- merge(testDataResult, streamMets, by=c('SITE','METRIC'), all=T)
  tt$diff <- tt$VALUE - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(VALUE) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Channel Habitat calculations with wadeable protocol data are broken"
             )

}



nrsaChannelHabitatTest.cleanup <- function(indb)
# Clean up when nrsaChannelHabitat() terminates
{
  odbcClose(indb)
}



nrsaChannelHabitatTest.inputData <- function()
# creates dataframe of channel habitat data for unit test
{
          # Create correctly formated test data, and run data through nrsaChannelHabitat
 testData<-    rbind(data.frame(SITE ='WAZP99-0591',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THAL',
                TRANSECT='A',
                STA_NUM=c("1"),
                VALUE=c("GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"RI",
 NA,"RA",NA,"GL",NA,"PO",NA,"GL",NA,"PO",NA,"PO",NA,"GL",NA,"GL",NA,"RI",NA,"RI",NA,
 "RA",NA,"RA",NA,"RA",NA,"RA",NA,"PO",NA,"GL",NA,"GL",NA,"RI",NA,"GL",NA,"RI",NA,"GL",
 NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"GL",NA,"PO",NA,"GL",NA,"GL",NA,"GL",
 NA,"RI",NA,"RI",NA,"RA",NA,"RA",NA,"RI",NA,"PO",NA,"PO",NA,"GL",NA,"RI",NA,"RI",NA,
 "RI",NA,"GL",NA,"GL",NA,"GL",NA,"PO",NA,"GL",NA,NA,NA,"GL",NA,"GL",NA,"GL",NA,"GL",
 NA,"PO",NA,"GL",NA,"GL",NA,"GL",NA,"RI",NA,"RI",NA,"RI",NA,"RA",NA,"GL",NA,"GL",NA,
 "GL",NA,"PO",NA,"PO",NA,"GL",NA,"RI",NA,"RI",NA,"GL",NA,"RI",NA,"RI",NA,"RI",NA,"RA",
 NA,"CA",NA,"RA",NA,"RI",NA,"RI",NA,"RA",NA,"GL",NA,"PO",NA,"PO",NA,"RI",NA,"RI",NA,
 "GL",NA,"RI",NA,"RI",NA,"RA",NA,"RA",NA,NA)
 ),
     data.frame(SITE ='WCAP99-0587',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THALW',
                TRANSECT='A',
                STA_NUM=c("1"),
                VALUE=c("PP","RA","RA","GL","GL","GL","RI","RI","RI","PP","PP","RA","RA","PP","RI","PB","PP","PP","CA",
"RA","RA","PB","PP","RA","RI","GL","GL","PP","RA","RI","PB","PP","PB","RA","RI","RA","GL","PB",
"PB","PB","PB","RI","RI","RI","RI","RA","PB","PB","RI","RI","RA","PB","RI","GL","RI","RA","CA",
"PB","GL","PP","RI","RI","RI","RI","RA","CA","PB","CA","RI","RI","PP","RI","GL","PP","PP","RA",
"CA","RA","CA","PB","PP","PB","PP","CA","PB","CA","PB","PB","GL","PB","CA","CA","PB","PB","FA",
"PB","RA","PB","PB","GL","CA","PB","PB")
 ),
     data.frame(SITE ='WCAP99-0905',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THALW',
                TRANSECT='A',
                STA_NUM=c("1"),
                VALUE=c("GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","PT","PT","PT","PT",
"PT","PT","GL","GL","GL","GL","PT","PT","PT","PT","PT","PT","PT","PT","PT","PT","PT","PT","PT",
"PT","PT","PT","GL","GL","GL","GL","GL","RI","GL","GL","GL","GL","PL","PL","PL","PL","RI","RI",
"GL","GL","GL","GL","GL","GL","GL","RI","RI","RI","RI","RI","RI","RI","GL","GL","GL","PT","PT",
"RI","RI","GL","GL","GL","GL","RI","RI","RI","GL","GL","GL","GL","GL","PT","PT","PT","RI","PT",
"PT","GL","RI","PL","PL")
 ),
     data.frame(SITE ='WCOP99-0563',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THALW',
                TRANSECT=c('A'),
                STA_NUM=c("1"),
                VALUE=c("GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","PD","PD","PD","PD","PD","PD","PD",
"PD","PD","PD","PD","PD","PD","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL",
"GL","GL","GL","PD","PD","PD","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL","GL")
 ),
     data.frame(SITE ='WCAP99-0585',
                PARAMETER='CHANUNCD',
                SAMPLE_TYPE='PHAB_THALW',
                TRANSECT=c("A","A","A","A","B","B","B","B","C","C","C","C"),
                STA_NUM=c("1","2","3","4","1","2","3","4","1","2","3","4"),
                VALUE=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
 )
 )
 
     testData$SITE <- as.character(testData$SITE)
     testData$TRANSECT <- as.character(testData$TRANSECT)
     testData$STA_NUM <- as.character(testData$STA_NUM)
     testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
     testData$PARAMETER <- as.character(testData$PARAMETER)
     testData$VALUE <- as.character(testData$VALUE)

return(testData)
}



nrsaChannelHabitatTest.testResults <- function()
# creates dataframe of channel habitat metrics calculation results for unit test
{
  metsExpected <- rbind(data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_fa',
                        VALUE=c( 0,0.9708737864,0,0,NA )
               ) 
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_ca',
                        VALUE=c( 1.0101010101,10.67961165,0,0, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_ra',
                        VALUE=c( 13.131313131,16.504854369,0,0, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_ri',
                        VALUE=c( 25.252525253,21.359223301,17,0, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_gl',
                        VALUE=c( 47.474747475,10.67961165,48,89.333333333,NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_fast',
                        VALUE=c( 39.393939394,49.514563107,17,0, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_slow',
                        VALUE=c( 60.6060606,50.485436893,83,100, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pool',
                        VALUE=c( 13.131313131,39.805825243,35,10.666666667, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pp',
                        VALUE=c( NA,14.563106796,0,0, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pd',
                        VALUE=c( NA,0,0,10.666666667, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pb',
                        VALUE=c( NA,25.242718447,0,0, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pt',
                        VALUE=c( NA,0,29,0, NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_pl',
                        VALUE=c(NA,0,6,0,NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_p',
                        VALUE=c(NA,0,0,0,NA) 
               )
                        , data.frame(SITE = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
                        METRIC='pct_dr',
                        VALUE=c(0,0,0,0,NA) 
               )
#                        , data.frame(UID = c('WAZP99-0591','WCAP99-0587','WCAP99-0905','WCOP99-0563','WCAP99-0585'),
#                        METRIC='pct_sb',
#                        RESULT=c( NA,0,0,0, NA)
#               )
           )
return(metsExpected)
}



# end of file
