# metsGeneral.r
# RUnit tests


nrsaGeneralTest <- function()
# Unit test for metsGeneral.1
# IGNORE THE VALUES for Boatable sites.  The test data is from WEMAP data and
#has only wadable sites.  The  metsGeneral.1 function needs data for
#both SAMPLE_TYPES, so the data was duplicated and VALUES for Boatable obs.
#were set to zero.
#
#  1/19/16 cws Removed debug print statements
#  2/01/16 cws Modified to include xtranspc metric calculation. Corrected
#          naming of 'helper' functions by adding 'Test'.
#
{
  testData <- nrsaGeneralTest.testData()

  metsExpected <- nrsaGeneralTest.expectedMets()

  nrsaGeneralTest.process(testData, metsExpected)

  wd <- subset(testData, SITE %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905'))
  wm <- subset(metsExpected, SITE %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905'))
  nrsaGeneralTest.process(wd, wm)

  bd <- subset(testData, !(SITE %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905')))
  bm <- subset(metsExpected, !(SITE %in% c('WCAP99-0587','WCAP99-0592','WCAP99-0905')))
  nrsaGeneralTest.process(bd, bm)

}


nrsaGeneralTest.process <- function(testData, metsExpected)
# performs the bulk of the unit test on the given data and expected VALUEs
# testData<- bd; metsExpected<-bm
{
    lastTransect <- function(df) {
        # returns dataframe with SITE and lastTransect containing value of TRANSECT
        # at which the provided has a row.
        # ASSUMPTIONS
        #   TRANSECT column exists and never has NA value
        rc <- ddply(df, 'SITE', summarise, lastTransect=max(TRANSECT))
        return(rc)
    }
  testDataResult<- nrsaGeneral(sampledTransects = subset(testData, PARAMETER == 'INCREMNT') # this subset done for historical reasons, results in sidecnt=NA instead of 0 for boatable reaches.
                              ,sideChannels = subset(testData, PARAMETER %in% c('SIDCHN','OFF_CHAN') & TRANSECT %in% LETTERS)
                              ,transectSpacing = rbind(merge(testData %>%                               # values for wadeable sites
                                                             subset(PARAMETER == 'DISTANCE') %>%        # sum DISTANCE between waypoints, if calculated
                                                             ddply(.(SITE,TRANSECT), summarise
                                                                  ,DISTANCE=protectedSum(as.numeric(VALUE), na.rm=TRUE) 
                                                                  )
                                                            ,testData %>%                               # ACTRANSP value is recorded distance between transects
                                                             subset(PARAMETER == 'ACTRANSP') %>%
                                                             dplyr::rename(ACTRANSP = VALUE) %>%
                                                             select(SITE, TRANSECT, ACTRANSP)
                                                            ,by=c('SITE','TRANSECT')
                                                            ,all=TRUE                                   # include all sites with ACTRANSP or DISTANCE, not just sites with both, dammit
                                                            ) %>% 
                                                       mutate(VALUE = ifelse(is.na(ACTRANSP), DISTANCE, ACTRANSP)) %>%
                                                       select(SITE,TRANSECT,VALUE)   
                                                      ,testData %>%                                     # values for wadeable sites in the standard fussy way
                                                       subset(PARAMETER == 'INCREMNT' & TRANSECT=='A' & STATION==0) %>%
                                                       select(SITE,VALUE) %>%
                                                       merge(merge(nWadeableStationsPerTransect(testData) # decrement station count of last transect by 1
                                                                  ,lastTransect(subset(testData, TRANSECT %in% LETTERS[1:11] & VALUE %nin% c('',NA)))
                                                                  ,by='SITE', all.x=TRUE
                                                                  ) %>%
                                                             mutate(nSta = ifelse(TRANSECT == lastTransect, nSta - 1, nSta)
                                                                   ,lastTransect = NULL
                                                                   )
                                                            ,by='SITE', all.x=TRUE
                                                            ) %>%
                                                       mutate(VALUE = as.numeric(VALUE) * nSta
                                                             ,nSta = NULL
                                                             )
#                                                       ,testData %>%                                     # values for wadeable sites, the simple unfussy way
#                                                        subset(PARAMETER == 'INCREMNT' & TRANSECT=='A' & STATION==0) %>%
#                                                        select(SITE,VALUE) %>%
#                                                        merge(nWadeableStationsPerTransect(testData) 
#                                                             ,by='SITE', all.x=TRUE
#                                                             ) %>%
#                                                        mutate(VALUE = as.numeric(VALUE) * nSta
#                                                              ,nSta = NULL
#                                                              )
                                                      )
                              )

  #compare results from baseData (testDataResult) with expectedResults  (metsExpected)

  metsExpected <- rename(metsExpected, 'VALUE','EXPECTED')
  testDataResult$VALUE<-as.numeric(testDataResult$VALUE)

  # Calculated values should be within 10E-7 of expected values, should
  # only be missing where they are supposed to be missing and nonmissing where
  # they are supposed to be nonmissing.
  # Note: the errs dataframe can be printed to show where the errors occur when
  # debugging.
  tt <- merge(testDataResult, metsExpected, by=c('SITE','METRIC'), all=TRUE)
  tt$diff <- tt$VALUE - tt$EXPECTED

  errs <- subset(tt, abs(diff) > 10^-7 | is.na(VALUE) != is.na(EXPECTED))
  checkEquals(0, nrow(errs)
             ,"Error: General  metrics are broken"
             )

}


nrsaGeneralTest.testData <- function()
# Creates test data for metsGeneral unit test
# Wadeable SITEs: WCAP99-0587, WCAP99-0592, WCAP99-0905
# Boatable SITEs: WCAP99-0585, WCAP99-0591
{
  # Create correctly formated test data, and run data through metsGeneral.1
  testData <- rbind(data.frame(SITE ='WCAP99-0585',
                               PARAMETER='ACTRANSP',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10]),
                               STATION='0',
                               VALUE=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
                              ),
                    data.frame(SITE ='WCAP99-0585 w/DISTANCE',
                               PARAMETER='DISTANCE',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10]),
                               STATION='0',
                               VALUE=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
                              ),
                    data.frame(SITE ='WCAP99-0587',
                               PARAMETER='INCREMNT',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G",
     "G","G","G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I",
     "I","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","10","11","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5",
     "6","7","8","9","0","1","2","3","4","5","6","7","8","9","10","0","1","2","3","4","5","6","7","8","9",
     "0","1","2","3","4","5","6","7","8","9"),
                               VALUE=c("1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,
     NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,
     NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,
     NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,
     NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0591',
                               PARAMETER='ACTRANSP',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10]),
                               STATION=rep('0',10),
                               VALUE="600",
                               stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0591 w/DISTANCE',
                               PARAMETER='DISTANCE',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10]),
                               STATION=rep('0',10),
                               VALUE=rep("800",10),
                               stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0591 w/ACTRANSP+DISTANCE',
                               PARAMETER=rep(c('ACTRANSP','DISTANCE'), each=10),
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10],LETTERS[1:10]),
                               STATION=rep("0",10),
                               VALUE=rep(c('600','800'), each=10),
                               stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0591 w/ACTRANSP+DISTANCE and some NA',
                               PARAMETER=rep(c('ACTRANSP','DISTANCE'), each=10),
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c(LETTERS[1:10],LETTERS[1:10]),
                               STATION=rep("0",10),
                               VALUE=c(NA,"600","600","600","600","600","600","600","600",NA
                                       ,"800",NA,"800","800","800","800","800","800","800",NA
                                       ),
                               stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0592',
                               PARAMETER='INCREMNT',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XF","XF","XF","XG","XG","XG","XG","XG","XG","XG","XG","XG",
     "XG"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","7","8","9","0","1","2","3","4","5","6","7","8","9"),
                               VALUE=c("1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,
     "1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,
     NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,
     NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"1.5",
     NA,NA,NA,NA,NA,NA,NA,NA,NA),stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0905',
                               PARAMETER='INCREMNT',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XI","XJ"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","0"),
                               VALUE=c("1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6","1.6",
     "1.6","1.6","1.6","1.6","1.6"),stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0585',
                               PARAMETER='OFF_CHAN',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=LETTERS[1:10],
                               STATION='0',
                               VALUE=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
                               ),
                    data.frame(SITE ='WCAP99-0587',
                               PARAMETER='SIDCHN',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G",
     "G","G","G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I",
     "I","I","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","10","11","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5",
     "6","7","8","9","0","1","2","3","4","5","6","7","8","9","10","0","1","2","3","4","5","6","7","8","9",
     "0","1","2","3","4","5","6","7","8","9"),
                               VALUE=c("N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N"),stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0591',
                               PARAMETER='OFF_CHAN',
                               SAMPLE_TYPE='PHAB_THAL',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
                   "A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B",
        "B","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","D","D","D",
     "D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","E","E","E","E","E","E","E",
     "E","E","E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","F","F",
     "F","F","F","F","F","F","F","F","G","G","G","G","G","G","G","G","G","G","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H","H",
     "H","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","I","J","J","J","J","J","J","J","J",
     "J","J","J","J","J","J","J","J","J","J","J","J"),
                               STATION=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
     "16","17","18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16",
     "17","18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17",
     "18","19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18",
     "19","20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19",
     "20","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20",
     "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1",
     "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1","2",
     "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","1","2","3",
     "4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"),
                               VALUE=c("N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","N","N","N","N","N","Y","Y",
     "N","N","N","Y","Y","N","N","N","N","N","N","N","N","N","N","N","Y","Y","Y","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","Y","Y","N","N","N","Y","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y"," ","N","N","N",
     "N","N","N","N","N","N","N","N","Y","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","Y","Y",
     "Y","Y","Y","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","Y","Y"),stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0592',
                               PARAMETER='SIDCHN',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XF","XF","XF","XG","XG","XG","XG","XG","XG","XG","XG","XG",
     "XG"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","7","8","9","0","1","2","3","4","5","6","7","8","9"),
                               VALUE=c("Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y"," ","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y"),stringsAsFactors=FALSE
                               ),
                    data.frame(SITE ='WCAP99-0905',
                               PARAMETER='SIDCHN',
                               SAMPLE_TYPE='PHAB_THALW',
                               TRANSECT=c("A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B",
     "B","B","B","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","E",
     "E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","G","G","G","G","G","G",
     "G","G","G","G","H","H","H","H","H","H","H","H","H","H","I","I","I","I","I","I","I","I","I","I","J","J",
     "J","J","J","J","J","J","J","J","XI","XJ"),
                               STATION=c("0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7",
     "8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2",
     "3","4","5","6","7","8","9","0","0"),
                               VALUE=c("N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N",
     "N","N","N","N","N","N","N","N","N","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
     "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","N","Y"),stringsAsFactors=FALSE
                              )
  )



      testData$SITE <- as.character(testData$SITE)
      testData$TRANSECT <- as.character(testData$TRANSECT)
      testData$STATION <- as.numeric(testData$STATION)
      testData$SAMPLE_TYPE <- as.character(testData$SAMPLE_TYPE)
      testData$PARAMETER <- as.character(testData$PARAMETER)
      testData$VALUE <- as.character(testData$VALUE)

  return(testData)
}



nrsaGeneralTest.expectedMets <- function()
# Create dataframe of expected metrics calculations
# The sidecnt value for WCAP99-0905 was changed from 3 to 2 due to the
# difference in how the value was calculated -- EMAP defines it as the
# number of side channel transects in the sub_bank file, while NRSA defines
# it as the number of side channel transects recorded in the tblTHALWEG2 table.
{
  metsExpected <- rbind(data.frame(SITE = c('WCAP99-0585','WCAP99-0587','WCAP99-0591','WCAP99-0592','WCAP99-0905'),
                                   METRIC='pct_side',
                                   VALUE=c( NA,0,10.552763819,100,28 )
                                   ,stringsAsFactors=FALSE
                                  )
                       ,data.frame(SITE = c(#'WCAP99-0585','WCAP99-0585 w/DISTANCE',
                                           'WCAP99-0587','WCAP99-0591','WCAP99-0591 w/DISTANCE','WCAP99-0591 w/ACTRANSP+DISTANCE','WCAP99-0591 w/ACTRANSP+DISTANCE and some NA','WCAP99-0592','WCAP99-0905'
                                          ),
                                   METRIC='reachlen',
                                   VALUE=c(#NA,NA,
                                            153,6000,8000,6000,5600,148.5,158.4
                                           )
                                   ,stringsAsFactors=FALSE
                                  )
                       ,data.frame(SITE = c('WCAP99-0585','WCAP99-0585 w/DISTANCE',
                                           'WCAP99-0587','WCAP99-0591','WCAP99-0591 w/DISTANCE','WCAP99-0591 w/ACTRANSP+DISTANCE','WCAP99-0591 w/ACTRANSP+DISTANCE and some NA','WCAP99-0592','WCAP99-0905'
                                          ),
                                   METRIC='xtranspc',
                                   VALUE=c(NA,NA,15.3,600,800,600,622.2222222,14.85,15.84)
                                   ,stringsAsFactors=FALSE
                                  )
                       ,data.frame(SITE = c('WCAP99-0585','WCAP99-0587','WCAP99-0591','WCAP99-0592','WCAP99-0905'),
                                   METRIC='sidecnt',
                                   VALUE=c(NA,0,NA,2,2 )
                                   ,stringsAsFactors=FALSE
                                  )
                       )
  return(metsExpected)
}

# end of file