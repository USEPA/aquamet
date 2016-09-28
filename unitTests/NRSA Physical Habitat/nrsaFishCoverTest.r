# metsFishCoverTest.r
#
#   12/07/15 cws Modified calling interface. Still need to refactor interior.
#            to handle NULL argument values.
#

nrsaFishCoverTest <- function ()
{
  # Create correctly formated test data, and run data through metsFishCover.1
  testData <- nrsaFishCover.createData()
  testData.River <- subset(testData, SITE %in% c('1','2','3','4','5'))
  testData.Stream <- subset(testData, SITE %in% c('6','7','8','9','10'))

  #create the expected results (mets) for the test data using outside calculations
  metsExpected <- nrsaFishCover.createResults()
  metsExpected <- rename(metsExpected, 'VALUE','EXPECTED')

  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)
  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  testDataResult <- nrsaFishCover(algae = testData %>% subset(PARAMETER=='ALGAE')
                                 ,boulder = testData %>% subset(PARAMETER=='BOULDR')
                                 ,brush = testData %>% subset(PARAMETER=='BRUSH')
                                 ,liveTree = testData %>% subset(PARAMETER=='LVTREE')
                                 ,macrophytes = testData %>% subset(PARAMETER=='MACPHY')
                                 ,overhang = testData %>% subset(PARAMETER=='OVRHNG')
                                 ,structures = testData %>% subset(PARAMETER=='STRUCT')
                                 ,undercut = testData %>% subset(PARAMETER %in% c('UNDCUT','UNDERCUT'))
                                 ,woodyDebris = testData %>% subset(PARAMETER=='WOODY')
                                 )
  
  tt <- merge(testDataResult, metsExpected, by=c('SITE','METRIC'),all=T)
  tt$diff <- tt$VALUE - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(VALUE) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (stream and river) are broken"
             )
  
  
  testDataResult.River <- nrsaFishCover(algae = testData %>% subset(SITE %in% c('1','2','3','4','5') & PARAMETER=='ALGAE')
                                        ,boulder = testData %>% subset(SITE %in% c('1','2','3','4','5') & PARAMETER=='BOULDR')
                                        ,brush = testData %>% subset(SITE %in% c('1','2','3','4','5') & PARAMETER=='BRUSH')
                                        ,liveTree = testData %>% subset(SITE %in% c('1','2','3','4','5') & PARAMETER=='LVTREE')
                                        ,macrophytes = testData %>% subset(SITE %in% c('1','2','3','4','5') & PARAMETER=='MACPHY')
                                        ,overhang = testData %>% subset(SITE %in% c('1','2','3','4','5') & PARAMETER=='OVRHNG')
                                        ,structures = testData %>% subset(SITE %in% c('1','2','3','4','5') & PARAMETER=='STRUCT')
                                        ,undercut = testData %>% subset(SITE %in% c('1','2','3','4','5') & PARAMETER %in% c('UNDCUT','UNDERCUT'))
                                        ,woodyDebris = testData %>% subset(SITE %in% c('1','2','3','4','5') & PARAMETER=='WOODY')
                                        )
  metsExpected.River <- subset(metsExpected, SITE %in% c('1','2','3','4','5'))
  tt <- merge(testDataResult.River, metsExpected.River, by=c('SITE','METRIC'),all=T)
  tt$diff <- tt$VALUE - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(VALUE) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (river) are broken"
             )


  testDataResult.Stream <- nrsaFishCover(algae = testData %>% subset(SITE %in% c('6','7','8','9','10') & PARAMETER=='ALGAE')
                                       ,boulder = testData %>% subset(SITE %in% c('6','7','8','9','10') & PARAMETER=='BOULDR')
                                       ,brush = testData %>% subset(SITE %in% c('6','7','8','9','10') & PARAMETER=='BRUSH')
                                       ,liveTree = testData %>% subset(SITE %in% c('6','7','8','9','10') & PARAMETER=='LVTREE')
                                       ,macrophytes = testData %>% subset(SITE %in% c('6','7','8','9','10') & PARAMETER=='MACPHY')
                                       ,overhang = testData %>% subset(SITE %in% c('6','7','8','9','10') & PARAMETER=='OVRHNG')
                                       ,structures = testData %>% subset(SITE %in% c('6','7','8','9','10') & PARAMETER=='STRUCT')
                                       ,undercut = testData %>% subset(SITE %in% c('6','7','8','9','10') & PARAMETER %in% c('UNDCUT','UNDERCUT'))
                                       ,woodyDebris = testData %>% subset(SITE %in% c('6','7','8','9','10') & PARAMETER=='WOODY')
                                       )
  metsExpected.Stream <- subset(metsExpected, SITE %in% c('6','7','8','9','10'))
  tt <- merge(testDataResult.Stream, metsExpected.Stream, by=c('SITE','METRIC'),all=T)
  tt$diff <- tt$VALUE - tt$EXPECTED
  errs <- subset(tt, abs(diff) > 10^-7 | is.na(VALUE) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: Fish Cover metrics (stream) are broken"
             )
  
  
  # test with missing 'natural component'. NOTE: Expected values for this test are wrong below
  testDataResult <- nrsaFishCover(algae = NULL
                                 ,boulder = testData %>% subset(PARAMETER=='BOULDR')
                                 ,brush = testData %>% subset(PARAMETER=='BRUSH')
                                 ,liveTree = testData %>% subset(PARAMETER=='LVTREE')
                                 ,macrophytes = testData %>% subset(PARAMETER=='MACPHY')
                                 ,overhang = testData %>% subset(PARAMETER=='OVRHNG')
                                 ,structures = testData %>% subset(PARAMETER=='STRUCT')
                                 ,undercut = testData %>% subset(PARAMETER %in% c('UNDCUT','UNDERCUT'))
                                 ,woodyDebris = testData %>% subset(PARAMETER=='WOODY')
                                 )
#   metsExpected.noAlgae <- mutate(metsExpected
#                                 ,EXPECTED = ifelse(METRIC=='pfc_alg', NA
#                                            ,ifelse(METRIC=='xfc_alg', NA
#                                            ,ifelse(METRIC=='pfc_all' & EXPECTED > 0, EXPECTED - 1
#                                            ,ifelse(METRIC=='xfc_all'
#                                                   ,ifelse(SITE=='1', 0
#                                                   ,ifelse(SITE=='2', 0.4
#                                                   ,ifelse(SITE=='3', 2.0
#                                                   ,ifelse(SITE=='4', 4.6
#                                                   ,ifelse(SITE=='5', 7.0
#                                                   ,ifelse(SITE=='6', 0
#                                                   ,ifelse(SITE=='7', 0.4
#                                                   ,ifelse(SITE=='8', 2.0
#                                                   ,ifelse(SITE=='9', 4.6
#                                                   ,ifelse(SITE=='10', 7.0
#                                                          ,-9999
#                                                    ))))))))))
#                                                   ,EXPECTED
#                                             ))))
#                                 )
#   tt <- merge(testDataResult, metsExpected.noAlgae, by=c('SITE','METRIC'), all=T)
#   tt$diff <- tt$VALUE - tt$EXPECTED
#   errs <- subset(tt, abs(diff) > 10^-7 | is.na(VALUE) != is.na(EXPECTED))
#   checkEquals(0, nrow(errs)
#              ,"Error: Fish Cover metrics (with missing algae data) are broken"
#              )

  # Test with missing 'big' component
  
  # Test with missing overhang, which has iqr value calculated

  # Test with missing undercut, which has iqr value calculated
  
}



nrsaFishCover.createData <- function()
#
{
  boatData <- rbind(expand.grid(SITE = 1:5
                               ,TRANSECT = LETTERS[1:11]
                               ,SAMPLE_TYPE = 'PHAB_CHANB'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDERCUT', 'WOODY'
                                             )
                               )
                    )
  boatData$SITE <- as.character(boatData$SITE)
  boatData$TRANSECT <- as.character(boatData$TRANSECT)
  boatData$SAMPLE_TYPE <- as.character(boatData$SAMPLE_TYPE)
  boatData$PARAMETER <- as.character(boatData$PARAMETER)
  boatData$VALUE <- rep(as.character(0:4), length.out=nrow(boatData))
  boatData$FLAG <- as.character(NA)

    strmData <- rbind(expand.grid(SITE = 6:10
                               ,TRANSECT = LETTERS[1:11]
                               ,SAMPLE_TYPE = 'PHAB_CHANB'
                               ,PARAMETER = c('ALGAE', 'BOULDR', 'BRUSH'
                                             ,'LVTREE', 'MACPHY', 'OVRHNG'
                                             ,'STRUCT', 'UNDCUT', 'WOODY'
                                             )
                               )
                    )
  strmData$SITE <- as.character(strmData$SITE)
  strmData$TRANSECT <- as.character(strmData$TRANSECT)
  strmData$SAMPLE_TYPE <- as.character(strmData$SAMPLE_TYPE)
  strmData$PARAMETER <- as.character(strmData$PARAMETER)
  strmData$VALUE <- rep(as.character(0:4), length.out=nrow(strmData))
  strmData$FLAG <- as.character(NA)

  testData <- rbind(boatData,strmData)

  return(testData)
}



nrsaFishCover.createResults <- function()
#
{
  mets<- rbind(data.frame(SITE=rep('1',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(SITE=rep('10',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.875,     0.875,      0.875,     0.875
                                  ,0.875,     0.875,      0.875,     0.875,      0.875
                                  ,9,         4,          6
                                  ,7.875,     3.5,        5.25
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(SITE=rep('2',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.05,      0.05,       0.05,      0.05
                                  ,0.05,      0.05,       0.05,      0.05,      0.05
                                  ,9,         4,          6
                                  ,0.45,      0.2,        0.3
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(SITE=rep('3',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.25,      0.25,       0.25,      0.25
                                  ,0.25,      0.25,       0.25,      0.25,      0.25
                                  ,9,         4,          6
                                  ,2.25,      1,          1.5
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(SITE=rep('4',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.575,     0.575,      0.575,     0.575
                                  ,0.575,     0.575,      0.575,     0.575,      0.575
                                  ,9,         4,          6
                                  ,5.175,     2.3,        3.45
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )


              ,data.frame(SITE=rep('5',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.875,     0.875,      0.875,     0.875
                                  ,0.875,     0.875,      0.875,     0.875,      0.875
                                  ,9,         4,          6
                                  ,7.875,     3.5,        5.25
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )

              ,data.frame(SITE=rep('6',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0,         0
                                  ,0,         0,          0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0,          0
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )

              ,data.frame(SITE=rep('7',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.05,      0.05,       0.05,      0.05
                                  ,0.05,      0.05,       0.05,      0.05,      0.05
                                  ,9,         4,          6
                                  ,0.45,      0.2,        0.3
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(SITE=rep('8',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(1,         1,          1,         1
                                  ,1,         1,          1,         1,          1
                                  ,0.25,      0.25,       0.25,      0.25
                                  ,0.25,      0.25,       0.25,      0.25,      0.25
                                  ,9,         4,          6
                                  ,2.25,      1,          1.5
                                  ,0,         0
                                  ,0,         0,          0,         0
                                  )
                         )
              ,data.frame(SITE=rep('9',30)
                         ,METRIC=c('pfc_alg', 'pfc_rck', 'pfc_brs', 'pfc_lvt'
                                  ,'pfc_aqm', 'pfc_ohv', 'pfc_hum', 'pfc_ucb', 'pfc_lwd'
                                  ,'xfc_alg', 'xfc_rck', 'xfc_brs', 'xfc_lvt'
                                  ,'xfc_aqm','xfc_ohv', 'xfc_hum', 'xfc_ucb', 'xfc_lwd'
                                  ,'pfc_all', 'pfc_big', 'pfc_nat'
                                  ,'xfc_all', 'xfc_big', 'xfc_nat'
                                  ,'sdfc_ucb', 'sdfc_ohv'
                                  ,'idrucb', 'idrohv', 'iqrucb', 'iqrohv'
                                   )
                         ,VALUE=c(1,         1,          1,         1
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
  
  mets <- mutate(mets, SITE=as.character(SITE), METRIC=as.character(METRIC))
  
  return(mets)
}



# end of file


                                  
