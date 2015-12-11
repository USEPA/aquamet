# nrsaRiparianVegetation.r
# RUnit tests


nrsaRiparianVegetationTest <- function()
# Unit test for metsRiparianVegetation().
{
    # Riparian veg data in 0809 organization
    ripData <- nrsaRiparianVegetationTest.makeData()

    # Create dataframe with expected results
    expected <- nrsaRiparianVegetationTest.makeExpected()

    # Compare expected and calculated results
    results <- nrsaRiparianVegetation(canopyCoverLargeDiameter = subset(ripData, PARAMETER=='CANBTRE') %>% dplyr::rename(BANK=TRANSDIR)
                                     ,canopyCoverSmallDiameter = subset(ripData, PARAMETER=='CANSTRE') %>% dplyr::rename(BANK=TRANSDIR)
                                     ,canopyVegetationType = subset(ripData, PARAMETER=='CANVEG') %>% dplyr::rename(BANK=TRANSDIR)
                                     ,groundCoverBare = subset(ripData, PARAMETER=='BARE') %>% dplyr::rename(BANK=TRANSDIR)
                                     ,groundCoverNonwoody = subset(ripData, PARAMETER=='GCNWDY') %>% dplyr::rename(BANK=TRANSDIR)
                                     ,groundCoverWoody = subset(ripData, PARAMETER=='GCWDY') %>% dplyr::rename(BANK=TRANSDIR)
                                     ,understoryCoverNonwoody = subset(ripData, PARAMETER=='UNDNWDY') %>% dplyr::rename(BANK=TRANSDIR)
                                     ,understoryCoverWoody = subset(ripData, PARAMETER=='UNDWDY') %>% dplyr::rename(BANK=TRANSDIR)
                                     ,understoryVegetationType = subset(ripData, PARAMETER=='UNDERVEG') %>% dplyr::rename(BANK=TRANSDIR)
                                     ,coverCalculationValues = data.frame(field=c(NA,'0','1','2','3','4')
                                                                         ,calc=c(NA,0,0.05,0.25,0.575,0.875)
                                                                         ,stringsAsFactors=FALSE
                                                                         )
                                     )
  
    expected$VALUE <- as.numeric(expected$VALUE)
    results$VALUE <- as.numeric(results$VALUE)
    errs <- dfCompare(expected, results, c('SITE','METRIC'), zeroFudge=1e-4)
#  return(errs)
    checkEquals(NULL, errs, "Error: metsRiparianVegetation is broken.")
}



nrsaRiparianVegetationTest.makeData <- function()
# Create dataframe of fake riparian vegetation data. Uses data from the
# following WEMAP sites:
# 2000 WAZP99-0512 1 - site with no missing values
# 2000 WAZP99-0545 1 - site with many missing values, and all missing values
#                      for CANVEG, UNDERVEG
# 2000 WCAP99-0592 1 - site with side channels, one of which is entirely missing
#                      data.
# 2002 WUTP02-R003 1   site with all missing values for BARE
#
# Expected values for metrics were obtained from the wemap calculations and
# were changed only as follows:
#   2000 WAZP99-0545 1 pcan_c, pcan_d, pcan_e, pcan_m, pcan_n, pmid_c, pmid_d,
#                      pmid_e, pmid_m, pmid_n were changed from 0 to NA since
#                      all the values are missing, and NA is thus more correct.
#   2000 WCAP99-0592 1 pcan_c, pcan_d, pcan_e, pcan_m, pcan_n , pmid_c, pmid_d,
#                      pmid_e, pmid_m, pmid_n were not handling cases with some
#                      missing values correctly (the missing values were
#                      included in the sample size).  pcan_c changed from
#                      0.76923077 to 0.83333333333, pcan_m changed from
#                      0.11538462 to 0.125, pcan_n from 0.03846154 to
#                      0.0416666666666667, and pmind_m from 0.92307692 to 1.
{
  #4567890123456789012345678901234567890
  wemapRip <- data.frame(matrix(
                  c('2000 WAZP99-0512 1', 'A', 'LF', '0', '0', 'N', '0', '1', 'D', '0', '3', 0
                   ,'2000 WAZP99-0512 1', 'A', 'RT', '1', '0', 'D', '1', '0', 'D', '1', '1', 1
                   ,'2000 WAZP99-0512 1', 'B', 'LF', '0', '0', 'N', '0', '0', 'N', '1', '2', 1
                   ,'2000 WAZP99-0512 1', 'B', 'RT', '1', '1', 'D', '1', '0', 'D', '1', '2', 1
                   ,'2000 WAZP99-0512 1', 'C', 'LF', '1', '0', 'D', '1', '0', 'D', '0', '2', 1
                   ,'2000 WAZP99-0512 1', 'C', 'RT', '1', '1', 'D', '1', '2', 'D', '0', '2', 0
                   ,'2000 WAZP99-0512 1', 'D', 'LF', '0', '2', 'D', '1', '0', 'D', '1', '2', 0
                   ,'2000 WAZP99-0512 1', 'D', 'RT', '1', '1', 'D', '1', '2', 'D', '1', '2', 0
                   ,'2000 WAZP99-0512 1', 'E', 'LF', '1', '1', 'D', '1', '0', 'D', '0', '1', 0
                   ,'2000 WAZP99-0512 1', 'E', 'RT', '1', '0', 'D', '2', '0', 'D', '0', '1', 0
                   ,'2000 WAZP99-0512 1', 'F', 'LF', '0', '0', 'N', '1', '0', 'D', '1', '1', 1
                   ,'2000 WAZP99-0512 1', 'F', 'RT', '0', '0', 'N', '1', '0', 'D', '1', '1', 0
                   ,'2000 WAZP99-0512 1', 'G', 'LF', '0', '0', 'N', '0', '0', 'N', '1', '1', 2
                   ,'2000 WAZP99-0512 1', 'G', 'RT', '0', '0', 'N', '1', '0', 'D', '1', '2', 0
                   ,'2000 WAZP99-0512 1', 'H', 'LF', '0', '0', 'N', '1', '0', 'D', '0', '1', 2
                   ,'2000 WAZP99-0512 1', 'H', 'RT', '1', '0', 'D', '1', '0', 'D', '0', '1', 0
                   ,'2000 WAZP99-0512 1', 'I', 'LF', '0', '0', 'N', '0', '0', 'N', '1', '1', 3
                   ,'2000 WAZP99-0512 1', 'I', 'RT', '0', '0', 'N', '1', '0', 'D', '2', '1', 0
                   ,'2000 WAZP99-0512 1', 'J', 'LF', '0', '0', 'N', '1', '0', 'D', '1', '1', 1
                   ,'2000 WAZP99-0512 1', 'J', 'RT', '1', '1', 'D', '1', '0', 'D', '2', '1', 1
                   ,'2000 WAZP99-0512 1', 'K', 'LF', '0', '0', 'N', '1', '0', 'D', '1', '1', 3
                   ,'2000 WAZP99-0512 1', 'K', 'RT', '0', '0', 'N', '1', '0', 'D', '1', '2', 1
                   ,'2000 WAZP99-0545 1', 'A', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'A', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'B', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'B', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'C', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'C', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'D', 'LF', NA, NA, NA, NA, NA, NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'D', 'RT', NA, NA, NA, NA, NA, NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'E', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'E', 'RT', NA, NA, NA, NA, NA, NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'F', 'LF', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'F', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'G', 'LF', NA, NA, NA, NA, NA, NA, '0', '2', 0
                   ,'2000 WAZP99-0545 1', 'G', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'H', 'LF', NA, NA, NA, NA, NA, NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'H', 'RT', NA, NA, NA, NA, NA, NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'I', 'LF', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'I', 'RT', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'J', 'LF', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'J', 'RT', '0', '0', NA, '0', '0', NA, '0', '4', 0
                   ,'2000 WAZP99-0545 1', 'K', 'LF', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WAZP99-0545 1', 'K', 'RT', '0', '0', NA, '0', '0', NA, '0', '3', 0
                   ,'2000 WCAP99-0592 1', 'A', 'LF', '0', '0', 'N', '1', '4', 'M', '0', '4', 0
                   ,'2000 WCAP99-0592 1', 'A', 'RT', '1', '2', 'M', '2', '2', 'M', '1', '3', 1
                   ,'2000 WCAP99-0592 1', 'B', 'LF', '3', '1', 'M', '2', '2', 'M', '2', '3', 1
                   ,'2000 WCAP99-0592 1', 'B', 'RT', '1', '3', 'M', '2', '2', 'M', '2', '3', 2
                   ,'2000 WCAP99-0592 1', 'C', 'LF', '1', '3', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'C', 'RT', '1', '3', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'D', 'LF', '2', '2', 'C', '3', '1', 'M', '2', '1', 2
                   ,'2000 WCAP99-0592 1', 'D', 'RT', '2', '2', 'C', '2', '1', 'M', '1', '1', 3
                   ,'2000 WCAP99-0592 1', 'E', 'LF', '1', '2', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'E', 'RT', '0', '1', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'F', 'LF', '2', '1', 'C', '2', '2', 'M', '2', '2', 2
                   ,'2000 WCAP99-0592 1', 'F', 'RT', '2', '0', 'C', '2', '2', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'G', 'LF', '2', '2', 'C', '2', '2', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'G', 'RT', '2', '2', 'C', '2', '1', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'H', 'LF', '1', '1', 'C', '3', '2', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'H', 'RT', '2', '1', 'C', '2', '2', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'I', 'LF', '1', '2', 'C', '1', '2', 'M', '1', '4', 0
                   ,'2000 WCAP99-0592 1', 'I', 'RT', '1', '2', 'C', '3', '2', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'J', 'LF', '1', '0', 'C', '1', '1', 'M', '1', '4', 0
                   ,'2000 WCAP99-0592 1', 'J', 'RT', '0', '2', 'C', '3', '1', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'K', 'LF', '3', '1', 'C', '3', '1', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'K', 'RT', '2', '1', 'C', '2', '1', 'M', '2', '3', 0
                   ,'2000 WCAP99-0592 1', 'XF', 'LF', NA, NA, NA, NA, NA, NA, NA, NA, NA
                   ,'2000 WCAP99-0592 1', 'XF', 'RT', NA, NA, NA, NA, NA, NA, NA, NA, NA
                   ,'2000 WCAP99-0592 1', 'XG', 'LF', '2', '2', 'C', '1', '2', 'M', '3', '2', 0
                   ,'2000 WCAP99-0592 1', 'XG', 'RT', '2', '2', 'C', '2', '2', 'M', '1', '3', 0
                   ,'2002 WUTP02-R003 1', 'A', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'A', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'B', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'B', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'C', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'C', 'RT', NA, NA, 'C', '3', '3', 'D', NA, NA, NA
                   ,'2002 WUTP02-R003 1', 'D', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'D', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'E', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'E', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'F', 'LF', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'F', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'G', 'LF', '3', '3', 'C', '2', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'G', 'RT', '3', '3', 'C', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'H', 'LF', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'H', 'RT', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'I', 'LF', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'I', 'RT', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'J', 'LF', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'J', 'RT', '2', '2', 'D', '3', '3', 'D', '3', '3', NA
                   ,'2002 WUTP02-R003 1', 'K', 'LF', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   ,'2002 WUTP02-R003 1', 'K', 'RT', '2', '2', 'C', '3', '3', 'D', '3', '2', NA
                   )
                   ,ncol=12, byrow=TRUE
                 ) # end of matrix() call
                 ,stringsAsFactors=FALSE
             ) # end of data.frame() call

  names(wemapRip) <- c('SITE','TRANSECT','TRANSDIR','CANBTRE','CANSTRE','CANVEG'
                      ,'UNDWDY','UNDNWDY','UNDERVEG','GCWDY','GCNWDY','BARE'
                      )

  fakeVisRip <- dfLengthen(wemapRip
                          ,c('SITE','TRANSECT','TRANSDIR')
                          ,'PARAMETER'
                          ,'VALUE'
                          ,names(wemapRip)[!(names(wemapRip) %in%
                                               c('SITE','TRANSECT','TRANSDIR')
                                            )]
                          )

  return(fakeVisRip)
}


nrsaRiparianVegetationTest.makeExpected <- function()
# Create dataframe of expected metrics calculations for unit test
{
  texpected <- data.frame(matrix(
                  c('2000 WAZP99-0512 1', 0, 0.45454545, 0, 0, 0.54545455
                                        , 0, 0.86363636, 0, 0, 0.13636364
                                        , 0.02045455, 0.02272727, 0.05, 0.025, 0.05227273, 0.14659091, 0.09318182
                                        , 0.04318182, 0.075, 0.09318182, 0.11818182, 0.19886364, 0.14545455, 0.31704545
                                        , 0.45454545, 0.86363636, 1, 0.54545455, 0.45454545, 0.86363636, 0.45454545
                   ,'2000 WAZP99-0545 1', NA, NA, NA, NA, NA
                                        , NA, NA, NA, NA, NA
                                        , 0, 0, 0, 0, 0, 0.62840909, 0
                                        , 0, 0, 0, 0, 0.62840909, 0, 0.62840909
                                        , 0, 0, 1, 0, 0, 0, 0
                   ,'2000 WCAP99-0592 1', 0.83333333, 0, 0, 0.125, 0.041666667
                                        , 0, 0, 0, 1, 0
                                        , 0.17083333, 0.20104167, 0.284375, 0.21770833, 0.265625, 0.43333333, 0.10104167
                                        , 0.371875, 0.50208333, 0.65625, 0.87395833, 0.69895833, 0.921875, 1.57291667
                                        , 0.95833333, 1, 1, 0.95833333, 0.95833333, 1, 0.95833333
                   ,'2002 WUTP02-R003 1', 0.68181818, 0.31818182, 0, 0, 0
                                        , 0, 1, 0, 0, 0
                                        , 0.28095238, 0.28095238, 0.56022727, 0.575, 0.575, 0.43571429, NA
                                        , 0.56190476, 1.13522727, 1.09659091, 1.67159091, 1.01071429, 1.64545455,2.63636364
                                        , 1, 1, 1, 1, 1, 1, 1
                   )
                ,ncol=32, byrow=TRUE
                ) #end of matrix() call
              ,stringsAsFactors=FALSE
              ) # end of data.frame() call
  names(texpected) <-c('SITE','pcan_c', 'pcan_d', 'pcan_e', 'pcan_m', 'pcan_n'
                      ,'pmid_c', 'pmid_d', 'pmid_e', 'pmid_m', 'pmid_n'
                      ,'xcl', 'xcs', 'xmw', 'xmh', 'xgw', 'xgh', 'xgb'
                      ,'xc', 'xm', 'xcmw', 'xcm', 'xg', 'xcmgw', 'xcmg'
                      ,'xpcan', 'xpmid', 'xpgveg', 'xpmgw', 'xpcm', 'xpmg', 'xpcmg'
                      )

  expected <- dfLengthen(texpected, 'SITE', 'METRIC', 'VALUE'
                        ,names(texpected)[!(names(texpected)=='SITE')]
                        )
  return(expected)
}

# end of file