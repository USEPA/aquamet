# metsFishCover.r
# RUnit tests


metsFishCoverTest <- function ()
{
  # Create correctly formated test data, and run data through metsFishCover.1
  testData <- metsFishCover.createData()
  testData.River <- subset(testData, UID %in% c('1','2','3','4','5'))
  testData.Stream <- subset(testData, UID %in% c('6','7','8','9','10'))

  testDataResult <- metsFishCover.1(testData)
  testDataResult.River <- metsFishCover.1(testData.River)
  testDataResult.Stream <- metsFishCover.1(testData.Stream)
  
  #create the expected results (mets) for the test data using outside calculations
  metsExpected <- metsFishCover.createResults()
  
  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)
  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  metsExpected <- rename(metsExpected, 'RESULT','EXPECTED')
  rr <- testDataResult

  tt <- merge(rr, metsExpected, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (stream and river) are broken"
             )
             
  metsExpected.River <- subset(metsExpected, UID %in% c('1','2','3','4','5'))
  rr <- testDataResult.River

  tt <- merge(rr, metsExpected.River, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (river) are broken"
             )

  metsExpected.Stream <- subset(metsExpected, UID %in% c('6','7','8','9','10'))
  rr <- testDataResult.Stream

  tt <- merge(rr, metsExpected.Stream, by=c('UID','METRIC'),all=T)
  tt$diff <- tt$RESULT - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(RESULT) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (stream) are broken"
             )
}



metsFishCover.createData <- function()
#
{
  boatData <- rbind(expand.grid(UID = 1:5
                               ,TRANSECT = LETTERS[1:11]
                               ,SAMPLE_TYPE = 'PHAB_CHANB'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDERCUT', 'WOODY'
                                             )
                               )
                    )
  boatData$UID <- as.character(boatData$UID)
  boatData$TRANSECT <- as.character(boatData$TRANSECT)
  boatData$SAMPLE_TYPE <- as.character(boatData$SAMPLE_TYPE)
  boatData$PARAMETER <- as.character(boatData$PARAMETER)
  boatData$RESULT <- rep(as.character(0:4), length.out=nrow(boatData))
  boatData$FLAG <- as.character(NA)

    strmData <- rbind(expand.grid(UID = 6:10
                               ,TRANSECT = LETTERS[1:11]
                               ,SAMPLE_TYPE = 'PHAB_CHANB'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDCUT', 'WOODY'
                                             )
                               )
                    )
  strmData$UID <- as.character(strmData$UID)
  strmData$TRANSECT <- as.character(strmData$TRANSECT)
  strmData$SAMPLE_TYPE <- as.character(strmData$SAMPLE_TYPE)
  strmData$PARAMETER <- as.character(strmData$PARAMETER)
  strmData$RESULT <- rep(as.character(0:4), length.out=nrow(strmData))
  strmData$FLAG <- as.character(NA)

  testData <- rbind(boatData,strmData)

  return(testData)
}



metsFishCover.createResults <- function()
#
{
  mets<- rbind(data.frame(UID=rep('1',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('10',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.875,     0.875,      0.875,     0.875
                                  ,0.875,     0.875,      0.875,     0.875,      0.875
                                  ,9,         4,          6
                                  ,7.875,     3.5,        5.25
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('2',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.05,      0.05,       0.05,      0.05
                                  ,0.05,      0.05,       0.05,      0.05,      0.05
                                  ,9,         4,          6
                                  ,0.45,      0.2,        0.3
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('3',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.25,      0.25,       0.25,      0.25
                                  ,0.25,      0.25,       0.25,      0.25,      0.25
                                  ,9,         4,          6
                                  ,2.25,      1,          1.5
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('4',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.575,     0.575,      0.575,     0.575
                                  ,0.575,     0.575,      0.575,     0.575,      0.575
                                  ,9,         4,          6
                                  ,5.175,     2.3,        3.45
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )


              ,data.frame(UID=rep('5',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.875,     0.875,      0.875,     0.875
                                  ,0.875,     0.875,      0.875,     0.875,      0.875
                                  ,9,         4,          6
                                  ,7.875,     3.5,        5.25
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )

              ,data.frame(UID=rep('6',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )

              ,data.frame(UID=rep('7',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.05,      0.05,       0.05,      0.05
                                  ,0.05,      0.05,       0.05,      0.05,      0.05
                                  ,9,         4,          6
                                  ,0.45,      0.2,        0.3
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('8',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.25,      0.25,       0.25,      0.25
                                  ,0.25,      0.25,       0.25,      0.25,      0.25
                                  ,9,         4,          6
                                  ,2.25,      1,          1.5
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(UID=rep('9',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,RESULT=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.575,     0.575,      0.575,     0.575
                                  ,0.575,     0.575,      0.575,     0.575,      0.575
                                  ,9,         4,          6
                                  ,5.175,     2.3,        3.45
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              )

  return(mets)
}



# end of file


                                  
