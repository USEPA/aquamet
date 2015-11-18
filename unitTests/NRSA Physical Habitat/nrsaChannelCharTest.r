# nrsaChannelCharTest.r
# RUnit tests


nrsaChannelCharTest <- function()
# Unit test for nrsaChannelChar.1
# IGNORE THE RESULTS for Boatable sites.  The test data is from WEMAP data and
# has only wadable sites.  The  nrsaChannelChar.1 function needs data for
# both SAMPLE_TYPES, so the data was duplicated and RESULTS for Boatable obs.
# were set to zero.
{
    # Create correctly formated test data as NRSA0809 format, and then split 
    # into current arguments
    testBGData <- nrsaChannelCharTest.createBankGeom()
    testCCData <- nrsaChannelCharTest.createChanChar()
    metsExpected <- nrsaChannelCharTest.testResults()

    # Test with mixed protocol data
    nrsaChannelCharTest.1(bankfullWidth =         subset(testCCData, PARAMETER == 'BANKFULL')
                         ,channelPattern =        subset(testCCData, PARAMETER == 'PATTERN')
                         ,constraintFeatures =    subset(testCCData, PARAMETER == 'FEATURES')
                         ,constraintMultiple =    subset(testBGData, PARAMETER == 'CONSTRT')
                         ,constraintSingle =      subset(testCCData, PARAMETER == 'CONSTRNT')
                         ,constraintPercent =     subset(testCCData, PARAMETER == 'PERCENT')
                         ,seeOverBank =           subset(testBGData, PARAMETER == 'SEEOVRBK')
                         ,shoreToVegDistance =    subset(testBGData, PARAMETER == 'SHOR2RIP')
                         ,valleyConstraintUnseen =subset(testCCData, PARAMETER == 'VALLYBOX')
                         ,valleyWidth =           subset(testCCData, PARAMETER == 'VALLEY')
                         ,metsExpected
                         ,"Error: Channel Characteristic metrics using data with both protocols are broken"
                         )

    # Test with wadeable-only data
    nrsaChannelCharTest.1(bankfullWidth =         subset(testCCData, PARAMETER == 'BANKFULL' & SITE %in% c('stream1','stream2'))
                         ,channelPattern =        subset(testCCData, PARAMETER == 'PATTERN' & SITE %in% c('stream1','stream2'))
                         ,constraintFeatures =    subset(testCCData, PARAMETER == 'FEATURES' & SITE %in% c('stream1','stream2'))
                         ,constraintMultiple =    subset(testBGData, PARAMETER == 'CONSTRT' & SITE %in% c('stream1','stream2'))
                         ,constraintSingle =      subset(testCCData, PARAMETER == 'CONSTRNT' & SITE %in% c('stream1','stream2'))
                         ,constraintPercent =     subset(testCCData, PARAMETER == 'PERCENT' & SITE %in% c('stream1','stream2'))
                         ,seeOverBank =           subset(testBGData, PARAMETER == 'SEEOVRBK' & SITE %in% c('stream1','stream2'))
                         ,shoreToVegDistance =    subset(testBGData, PARAMETER == 'SHOR2RIP' & SITE %in% c('stream1','stream2'))
                         ,valleyConstraintUnseen =subset(testCCData, PARAMETER == 'VALLYBOX' & SITE %in% c('stream1','stream2'))
                         ,valleyWidth =           subset(testCCData, PARAMETER == 'VALLEY' & SITE %in% c('stream1','stream2'))
                         ,subset(metsExpected, SITE %in% c('stream1','stream2'))
                         ,"Error: Channel Characteristic metrics with only wadeable data are broken"
                         )
    
    # Test with only boatable-only data
    nrsaChannelCharTest.1(bankfullWidth =         subset(testCCData, PARAMETER == 'BANKFULL' & SITE %nin% c('stream1','stream2'))
                         ,channelPattern =        subset(testCCData, PARAMETER == 'PATTERN' & SITE %nin% c('stream1','stream2'))
                         ,constraintFeatures =    subset(testCCData, PARAMETER == 'FEATURES' & SITE %nin% c('stream1','stream2'))
                         ,constraintMultiple =    subset(testBGData, PARAMETER == 'CONSTRT' & SITE %nin% c('stream1','stream2'))
                         ,constraintSingle =      subset(testCCData, PARAMETER == 'CONSTRNT' & SITE %nin% c('stream1','stream2'))
                         ,constraintPercent =     subset(testCCData, PARAMETER == 'PERCENT' & SITE %nin% c('stream1','stream2'))
                         ,seeOverBank =           subset(testBGData, PARAMETER == 'SEEOVRBK' & SITE %nin% c('stream1','stream2'))
                         ,shoreToVegDistance =    subset(testBGData, PARAMETER == 'SHOR2RIP' & SITE %nin% c('stream1','stream2'))
                         ,valleyConstraintUnseen =subset(testCCData, PARAMETER == 'VALLYBOX' & SITE %nin% c('stream1','stream2'))
                         ,valleyWidth =           subset(testCCData, PARAMETER == 'VALLEY' & SITE %nin% c('stream1','stream2'))
                         ,subset(metsExpected, SITE %nin% c('stream1','stream2'))
                         ,"Error: Channel Characteristic metrics with only boatable data are broken"
                         )

    # Test with only constraint form data
    nrsaChannelCharTest.1(bankfullWidth =         subset(testCCData, PARAMETER == 'BANKFULL')
                         ,channelPattern =        subset(testCCData, PARAMETER == 'PATTERN')
                         ,constraintFeatures =    subset(testCCData, PARAMETER == 'FEATURES')
                         ,constraintMultiple =    NULL
                         ,constraintSingle =      subset(testCCData, PARAMETER == 'CONSTRNT')
                         ,constraintPercent =     subset(testCCData, PARAMETER == 'PERCENT')
                         ,seeOverBank =           NULL
                         ,shoreToVegDistance =    NULL
                         ,valleyConstraintUnseen =subset(testCCData, PARAMETER == 'VALLYBOX')
                         ,valleyWidth =           subset(testCCData, PARAMETER == 'VALLEY')
                         ,subset(metsExpected, METRIC %nin% c('pctch_b','pctch_c','pctch_n','pctch_u','pct_ovrb','xshor2vg','mxshor','mnshor'))
                         ,"Error: Channel Characteristic metrics with only contraint form data are broken"
                         )

    # Test with only boatable thalweg form data
    nrsaChannelCharTest.1(bankfullWidth =         NULL
                         ,channelPattern =        NULL
                         ,constraintFeatures =    NULL
                         ,constraintMultiple =    subset(testBGData, PARAMETER == 'CONSTRT')
                         ,constraintSingle =      NULL
                         ,constraintPercent =     NULL
                         ,seeOverBank =           subset(testBGData, PARAMETER == 'SEEOVRBK')
                         ,shoreToVegDistance =    subset(testBGData, PARAMETER == 'SHOR2RIP')
                         ,valleyConstraintUnseen =NULL
                         ,valleyWidth =           NULL
                         ,subset(metsExpected, METRIC %in% c('pctch_b','pctch_c','pctch_n','pctch_u','pct_ovrb','xshor2vg','mxshor','mnshor'))
                         ,"Error: Channel Characteristic metrics with only boatable thalweg form data are broken"
                         )
    
    # Test with no data
    nrsaChannelCharTest.1(bankfullWidth =         subset(testCCData, FALSE)
                         ,channelPattern =        subset(testCCData, FALSE)
                         ,constraintFeatures =    subset(testCCData, FALSE)
                         ,constraintMultiple =    subset(testBGData, FALSE)
                         ,constraintSingle =      subset(testCCData, FALSE)
                         ,constraintPercent =     subset(testCCData, FALSE)
                         ,seeOverBank =           subset(testBGData, FALSE)
                         ,shoreToVegDistance =    subset(testBGData, FALSE)
                         ,valleyConstraintUnseen =subset(testCCData, FALSE)
                         ,valleyWidth =           subset(testCCData, FALSE)
                         ,subset(metsExpected, FALSE)
                         ,"Error: Channel Characteristic metrics with no data at all are broken"
                         )
}



nrsaChannelCharTest.1 <- function(bankfullWidth             # both, on channel constraint form
                                 ,channelPattern            # both, on channel constraint form
                                 ,constraintFeatures        # both, on channel constraint form
                                 ,constraintMultiple        # boatable only 
                                 ,constraintSingle          # both, on channel constraint form 
                                 ,constraintPercent         # both, on channel constraint form
                                 ,seeOverBank               # boatable only
                                 ,shoreToVegDistance        # boatable only
                                 ,valleyConstraintUnseen    # both, on channel constraint form
                                 ,valleyWidth               # both, on channel constraint form
                                 ,metsExpected
                                 ,errmsg
                                 )
# Check character mets separately from numeric mets to allow zeriFudge to
# have an effect.
# Calculated values should be within 10E-7 of expected values, should
# only be missing where they are supposed to be missing and nonmissing where
# they are supposed to be nonmissing.
# Note: the errs dataframe can be printed to show where the errors occur when
# debugging.
{
    metsResult <- nrsaChannelChar(bankfullWidth = bankfullWidth
                                 ,channelPattern = channelPattern
                                 ,constraintFeatures = constraintFeatures
                                 ,constraintMultiple = constraintMultiple
                                 ,constraintSingle = constraintSingle
                                 ,constraintPercent = constraintPercent
                                 ,seeOverBank = seeOverBank
                                 ,shoreToVegDistance = shoreToVegDistance
                                 ,valleyConstraintUnseen = valleyConstraintUnseen
                                 ,valleyWidth = valleyWidth
                                 )

    textMets <- c('constraint','confeatures','conpattern','convalleybox')
    
    expected <- metsExpected %>% subset(METRIC %in% textMets)
    actual <- metsResult %>% subset(METRIC %in% textMets)
    if(nrow(expected) == 0 & nrow(actual) == 0) {
        errC <- NULL
    } else {
        errC <- dfCompare(expected, actual, c('SITE','METRIC'), zeroFudge=1e-7)
    }
        
    expected <- metsExpected %>% subset(METRIC %nin% textMets)
    actual <- metsResult %>% subset(METRIC %nin% textMets)
    if(nrow(expected) == 0 & nrow(actual) == 0) {
        errN <- NULL
    } else {
        errN <- dfCompare(expected %>% mutate(VALUE=as.numeric(VALUE))
                         ,actual %>% mutate(VALUE=as.numeric(VALUE))
                         ,c('SITE','METRIC'), zeroFudge=1e-7
                         )
    }

    errs <- rbind(errC, errN)

    checkEquals(NULL, errs, errmsg)
}



nrsaChannelCharTest.cleanup <- function(indb)
# Clean up when nrsaChannelChar() terminates
{
  odbcClose(indb)
}



nrsaChannelCharTest.createBankGeom <- function()
# creates dataframe of channel characteristics data for unit test, as found in
# tblBANKGEOMETRY2
{
  # Create correctly formated test data, and run data through nrsaChannelChar.1
  testData <- rbind(expand.grid(PARAMETER=c('CONSTRT','SEEOVRBK','SHOR2RIP')
                   ,TRANSECT = LETTERS[1:11]
                   ,SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576')
                   ,SAMPLE_TYPE = 'PHAB_CHANB'
                   )
     )

  testData$VALUE <- c("N","YES",0,"N","YES",0,"N","YES",0,"C",NA,0,"C","YES",0,"N","YES",
              0,"C","YES",0,"N","YES",0,"N",NA,0,"N","YES",0,"C","YES",0,"B","N",0.3,
              "U","N",30,"B","N",2,"U","N",1,"U","N",2,"U","N",10,"B","N",15,"B","N",3,
              "B","N",1,"C","N",5,"B","N",2,"C","YES",0,"B","N",0,"B","N",0,"B","YES",
              0,"B","YES",0,"B","N",0,"B","N",0,"B",NA,0,"U","YES",0,"U","YES",1,NA,
              NA,NA,"C","YES",2.7,"C","YES",1.5,NA,NA,NA,"C","YES",1.3,"C","YES",
              1.1,"C","N",0.5,"C","N",0,"C","N",1.5,"C","YES",22,"C","N",3,"C","N",30,
              "B","YES",15,"U","YES",5,"B","YES",2,"C","N",10,"B","N",1,"U","N",1,
              "B","N",2,"B","N",1,"B","N",1,"B","N",2,"B","N",0.3)

  testData$SITE <- as.character(testData$SITE)
  testData$TRANSECT <- as.character(testData$TRANSECT)
  testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
  testData$PARAMETER <- as.character(testData$PARAMETER)

  return(testData)
}



nrsaChannelCharTest.createChanChar <- function()
# Create and return a dataframe of simulated channel constraint data from
# tblCHANNELCHAR2. Unlike WEMAP, this data is collected for both wadeable and
# boatable reaches.
{
  testData <- expand.grid(PARAMETER=c('BANKFULL','CONSTRNT','FEATURES'
                                     ,'PATTERN','PERCENT','VALLEY','VALLYBOX'
                                     )
                         ,SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528'
                                  ,'WUTP99-0553','WWYP99-0576','stream1','stream2'
                                  )
                         )
  testData$PARAMETER <- as.character(testData$PARAMETER)
  testData$SITE <- as.character(testData$SITE)
  testData$VALUE <- rep(c('5.5','CON_BROAD','HILLSLOPE','SINGLE','100','1500','Y'),7)
  testData$SAMPLE_TYPE <- 'PHAB_CHCON'
  testData$TRANSECT <- 'NONE'
  testData$FLAG <- ''

  return(testData)
}



nrsaChannelCharTest.testResults <- function()
# creates dataframe of channel characteristics metrics calculation results for unit test
{
  metsExpected <- rbind(data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='xshor2vg',
                                   VALUE=c(0,6.4818181818,0.1,6.36,3.6636363636)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='mxshor',
                                   VALUE=c(0,30,1,30,15)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='mnshor',
                                   VALUE=c(0,0.3,0,0,0.3)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pct_ovrb',
                                   VALUE=c(100,0,55.555555556,50,27.272727273)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pctch_b',
                                   VALUE=c( 0,54.545454545,70,0,72.727272727)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pctch_c',
                                   VALUE=c( 36.363636364,9.0909090909,10,100,9.0909090909)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                               METRIC='pctch_n',
                               VALUE=c( 63.636363636,0,0,0,0)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576'),
                                   METRIC='pctch_u',
                                   VALUE=c( 0,36.363636364,20,0,18.181818182)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576','stream1','stream2'),
                                   METRIC='conbankfull',
                                   VALUE=rep('5.5', 7)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576','stream1','stream2'),
                                   METRIC='constraint',
                                   VALUE=rep('CON_BROAD', 7)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576','stream1','stream2'),
                                   METRIC='confeatures',
                                   VALUE=rep('HILLSLOPE', 7)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576','stream1','stream2'),
                                   METRIC='conpattern',
                                   VALUE=rep('SINGLE', 7)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576','stream1','stream2'),
                                   METRIC='conpercent',
                                   VALUE=rep('100', 7)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576','stream1','stream2'),
                                   METRIC='convalley',
                                   VALUE=rep('1500', 7)
                                  )
                       ,data.frame(SITE = c('WAZP99-0591','WMTP99-0587','WSDP99-0528','WUTP99-0553','WWYP99-0576','stream1','stream2'),
                                   METRIC='convalleybox',
                                   VALUE=rep('Y', 7)
                                  )
                       )

  return(metsExpected %>% mutate(SITE=as.character(SITE), METRIC=as.character(METRIC)))
}



# end of file
