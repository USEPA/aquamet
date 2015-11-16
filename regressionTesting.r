# regressionTesting.r
#
# Compare current metrics values with values from new generalized metrics 
# functions.  Should be no difference.

source('l:/Priv/CORFiles/IM/Rwork/sharedCode/db.r')

# Get all SAMPLE_TYPE+PARAMETER combinations from all tables in the 1314 database:
# TODO: build dbMassQuery with args like dbMassGet
nrsa0809 <- odbcConnect('NRSA0809')
allTables <- subset(sqlTables(nrsa0809), TABLE_SCHEM=='dbo' & grepl('^tbl.+$', TABLE_NAME))$TABLE_NAME %>%
             setdiff(c('tblMETADATA','tblPARAMETERDESCRIPTIONS','tblPARAMETERDESCRIPTIONS2', 'tblFORMVALUEMAP'))
tableTriplets <- do.call(rbind
                        ,lapply(allTables
                               ,function(tname) {
                                    tCols <- sqlColumns(nrsa0809, tname)$COLUMN_NAME
                                    if('SAMPLE_TYPE' %in% tCols & 'PARAMETER' %in% tCols) {
                                        rc <- sqlQuery(nrsa0809
                                                      ,sprintf("SELECT DISTINCT SAMPLE_TYPE, PARAMETER FROM %s"
                                                              ,tname
                                                              )
                                                      ,stringsAsFactors=FALSE
                                                      )
                                        rc$TABLE_NAME <- tname
                                    } else {
                                        rc <- NULL
                                    }
                                    return(rc)
                                }
                               )
                        ) %>%
                 mutate(SAMPLE_TYPE = trimws(SAMPLE_TYPE)
                       ,PARAMETER = trimws(PARAMETER)
                       ) %>%
                unique()




currentMets <- dbGet('NRSA0809', 'tblPHABMET') # %>% dplyr::rename(SITE=BATCHNO,VALUE=RESULT)
visits <- dbGet('NRSA0809', 'tblVISITS2') %>% dplyr::rename(SITE=BATCHNO)

# bank morphology ##################################################################
# subset(tableTriplets, PARAMETER %in% c('ANGLE','UNDERCUT'))
bankgeom <- dbGet('NRSA0809', 'tblBANKGEOMETRY2', where="PARAMETER in ('ANGLE','UNDERCUT')") %>% 
            dplyr::rename(SITE=BATCHNO, VALUE=RESULT) %>%
            mutate(PARAMETER=trimws(PARAMETER)
                  ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
                  ,TRANSECT = trimws(TRANSECT)
                  ,VALUE=trimws(VALUE)
                  )

bm <- nrsaBankMorphology(bAngle = subset(bankgeom, PARAMETER=='ANGLE' & SAMPLE_TYPE=='PHAB_CHANBFRONT' & trimws(TRANSECT) %nin% paste0('X', LETTERS))
                        ,wAngle =  subset(bankgeom, PARAMETER=='ANGLE' & SAMPLE_TYPE=='PHAB_CHANW')
                        ,wUndercut = subset(bankgeom, PARAMETER=='UNDERCUT' & SAMPLE_TYPE=='PHAB_CHANW')
                        )

dd <- dfCompare(currentMets %>% dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(METRIC=trimws(METRIC), VALUE=trimws(VALUE)) %>% subset(METRIC %in% toupper(unique(bm$METRIC)))
               ,bm %>% mutate(METRIC=toupper(METRIC))
               ,c('SITE','METRIC')
               )

# shows 260 differences:
#  Lots of differences because current 'NA' does not match new NA.
#  1417 is PARBYBOAT and was calculated as boatable, but it has wadeable bank geometry data so different mets are calculated now
#  15508 is OTHER_NSP, but has wadeable bank geometry data so mets were not calculated at all, but are calculated now
#  15639 has changes in BAP_MED, BAP_STP, BAP_VST values and N_BA changes from 10 to 9.  It looks like we dropped a '75-100' somewhere, no change in history after 2010-04-05...


# substrate characterisation ##################################################################
# subset(tableTriplets, grepl('BOTTOM|SHORE', PARAMETER))
# NOTE: Some rows have TRANSECT values of 'NOT MARKED', which were used in 0809 calculations,
#       so can't just subset as TRANSECT %in% LETTERS.  Dang.
littoral <- dbGet('NRSA0809', 'tblLITTORAL2') %>% 
            subset(trimws(PARAMETER) %in% c('BOTTOMSEC','BOTTOMDOM','SHORESEC','SHOREDOM')) %>%
            dplyr::rename(SITE=BATCHNO, VALUE=RESULT) %>%
            mutate(PARAMETER=trimws(PARAMETER)
                  ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
                  ,TRANSECT = trimws(TRANSECT)
                  ,VALUE=trimws(VALUE)
                  )
thalweg <-  dbGet('NRSA0809', 'tblTHALWEG2') %>%
            subset(trimws(PARAMETER) %in% c('SIZE_CLS')) %>%
            dplyr::rename(SITE=BATCHNO, VALUE=RESULT) %>%
            mutate(PARAMETER=trimws(PARAMETER)
                  ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
                  ,TRANSECT = trimws(TRANSECT)
                  ,VALUE=trimws(VALUE)
                  )
chanxc <-   dbGet('NRSA0809', 'tblCHANNELCROSSSECTION2') %>%
            subset(trimws(PARAMETER) %in% c('SIZE_CLS','XSIZE_CLS')) %>% 
            dplyr::rename(SITE=BATCHNO, VALUE=RESULT) %>%
            mutate(PARAMETER=trimws(PARAMETER)
                  ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
                  ,TRANSECT = trimws(TRANSECT)
                  ,VALUE=trimws(VALUE)
                  )

sc <- nrsaSubstrateCharacterization(bBottomDom = subset(littoral, PARAMETER == 'BOTTOMDOM' & trimws(TRANSECT) %nin% paste0('X', LETTERS))
                                   ,bBottomSec = subset(littoral, PARAMETER == 'BOTTOMSEC' & trimws(TRANSECT) %nin% paste0('X', LETTERS))
                                   ,bShoreDom = subset(littoral, PARAMETER == 'SHOREDOM' & trimws(TRANSECT) %nin% paste0('X', LETTERS))
                                   ,bShoreSec = subset(littoral, PARAMETER == 'SHORESEC' & trimws(TRANSECT) %nin% paste0('X', LETTERS))
                                   ,bSizeClass = subset(thalweg, PARAMETER == 'SIZE_CLS' & trimws(TRANSECT) %nin% paste0('X', LETTERS))
                                   ,wSizeClass = subset(chanxc, PARAMETER == 'SIZE_CLS')
                                   ,wMezzoSizeClass = subset(chanxc, PARAMETER == 'XSIZE_CLS')
                                   )

dd <- dfCompare(currentMets %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% 
                mutate(METRIC=trimws(METRIC), VALUE=as.numeric(trimws(VALUE))) %>% 
                subset(METRIC %in% toupper(unique(sc$METRIC)))
               ,sc %>% 
                mutate(METRIC=toupper(METRIC)
                      ,VALUE=as.numeric(VALUE)
                      )
               ,c('SITE','METRIC')
               ,zeroFudge=1e-6
               )

# Zero differences!

#################################################################
#
# Slope & Bearing
base_cg <- dbGet('NRSA0809', 'tblCHANNELGEOMETRY2')
cg <- base_cg %>%
      subset(grepl('SLOPE|DISTANCE|BEAR|PROP',PARAMETER)) %>% 
      dplyr::rename(SITE=BATCHNO, VALUE=RESULT) %>%
      mutate(PARAMETER=trimws(PARAMETER)
            ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
            ,TRANSECT = trimws(TRANSECT)
            ,VALUE=trimws(VALUE)
            ,UNITS=toupper(trimws(UNITS))
            ,METHOD=toupper(trimws(METHOD))
            )
base_thal <- dbGet('NRSA0809', 'tblTHALWEG2')
thal <- base_thal %>%
        #subset(trimws(PARAMETER) %in% c('INCREMNT')) %>% 
        dplyr::rename(SITE=BATCHNO, VALUE=RESULT) %>%
        mutate(PARAMETER=trimws(PARAMETER)
              ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
              ,TRANSECT = trimws(TRANSECT)
              ,VALUE=trimws(VALUE)
              ,STATION=as.integer(STATION)
              )
gisCalcs <- read.csv('l:/Priv/CORFiles/IM/Rwork/nrsa/results/gpsBasedCalculations_asOf201203008.csv'
                    ,stringsAsFactors=FALSE, na='', header=TRUE
                    ) %>%
            dplyr::rename(SITE=UID, VALUE=RESULT) # %>%
#             mutate(PARAMETER=trimws(PARAMETER)
#                   ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
#                   ,TRANSECT = trimws(TRANSECT)
#                   ,VALUE=trimws(VALUE)
#                   )

protocols <- siteProtocol(c(unique(base_cg$BATCHNO), unique(base_thal$BATCHNO), unique(gisCalcs$SITE)), mutate(visits, UID=SITE)[c('UID','VALXSITE')])
oldrecalc <- metsSlopeBearing.1(base_thal %>% dplyr::rename(UID=BATCHNO) %>% mutate(PARAMETER=trimws(PARAMETER)
                                                                                  ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
                                                                                  ,TRANSECT = trimws(TRANSECT)
                                                                                  ,RESULT=trimws(RESULT)
                                                                                  ,STATION=as.integer(STATION)
                                                                                  )
                               ,base_cg   %>% dplyr::rename(UID=BATCHNO) %>% mutate(PARAMETER=trimws(PARAMETER)
                                                                                   ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
                                                                                   ,TRANSECT = trimws(TRANSECT)
                                                                                   ,RESULT=trimws(RESULT)
                                                                                   ,UNITS=toupper(trimws(UNITS))
                                                                                   ,METHOD=toupper(trimws(METHOD))
                                                                                   )
                               ,gisCalcs  %>% dplyr::rename(UID=SITE,RESULT=VALUE)
                               ,protocols)
oldrecalc2 <- metsSlopeBearing.1(thal %>% dplyr::rename(UID=SITE, RESULT=VALUE) 
                                ,cg   %>% dplyr::rename(UID=SITE, RESULT=VALUE) 
                                ,gisCalcs  %>% dplyr::rename(UID=SITE,RESULT=VALUE)
                                ,protocols)
currentSB <- currentMets %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% 
                mutate(METRIC=trimws(METRIC), VALUE=as.numeric(trimws(VALUE))) %>% 
                subset(METRIC %in% toupper(unique(oldrecalc$METRIC)))
ddOld <- dfCompare(currentSB
               ,oldrecalc %>% 
                mutate(SITE=UID
                      ,METRIC=toupper(METRIC)
                      ,VALUE=as.numeric(RESULT)
                      ,UID=NULL
                      ,RESULT=NULL
                      )
               ,c('SITE','METRIC')
               ,zeroFudge=1e-6
               )
# > ddOld
#      [,1]                                                                                                  
# [1,] "Difference at SITE=12195, METRIC=TRANSPC; VALUE: First=<75.5291943090909> Second=<84.148043>"        
# [2,] "Difference at SITE=12195, METRIC=XBEARING; VALUE: First=<289.252393606211> Second=<298.003867517293>"
# [3,] "Difference at SITE=12200, METRIC=TRANSPC; VALUE: First=<313671.231655836> Second=<40>"               
# [4,] "Difference at SITE=12200, METRIC=XBEARING; VALUE: First=<99.9549350001598> Second=<273.732299601608>"
expandValueRows <- function(df, targetRowCombos, keys=c('SITE','SAMPLE_TYPE','TRANSECT','REP')) {
    # Used to make sure that a slope value exists in all rows that PROPORTION does 
    # within a site's transect.
#    rc <- merge(df, targetRowCombos[keys], by=keys, all=TRUE)
    grps <- split(df, with(df, paste(SITE,SAMPLE_TYPE,TRANSECT))) %>%
                 lapply(function(rowGroup) {
                            expandedGroup <- merge(rowGroup[setdiff(names(rowGroup), 'REP')]
                                                  ,targetRowCombos %>%
                                                   subset(paste(SITE, SAMPLE_TYPE, TRANSECT) %in% with(rowGroup, paste(SITE, SAMPLE_TYPE, TRANSECT))
                                                         )[c('SITE','SAMPLE_TYPE','TRANSECT','REP')] %>% 
                                                   unique()
                                                  ,c('SITE','SAMPLE_TYPE','TRANSECT')
                                                  )
                            return(expandedGroup)
                        }
                       )
    rc <- do.call(rbind, grps)
}

expandValueRowsTest <- function() {
    # Unit test for expandValueRows
    fullData <- expand.grid(SITE=1:3, SAMPLE_TYPE=1:2, TRANSECT=LETTERS[1:5], REP=1:3, stringsAsFactors=FALSE) %>%
                as.data.frame() %>%
                mutate(VALUE = paste(SITE, SAMPLE_TYPE, TRANSECT, REP)) 
    
    # Test case where no changes should be made to data that has full range of all keys
    actual <- expandValueRows(fullData, fullData[c('SITE','SAMPLE_TYPE','TRANSECT','REP')])
    dd <- dfCompare(fullData, actual, c('SITE','SAMPLE_TYPE','TRANSECT','REP'))
    checkTrue(is.null(dd), "Incorrectly changed full data that did not need expansion")
    
    # Test case where no changes should be made to data that has partial range of keys
    partialData <- fullData %>%
                   subset(SITE==1 & SAMPLE_TYPE==1 |                                           # site 1 1 has all reps in all transects
                          SITE==1 & SAMPLE_TYPE==2 & REP==1 |                                  # site 1 2 has 1 rep in all transects
                          SITE==2 & SAMPLE_TYPE==1 & (REP==1 | TRANSECT %in% LETTERS[3:5]) |   # site 2 1 has rep 1 in C,D,E
                          SITE==2 & SAMPLE_TYPE==2 & (REP==3 | TRANSECT %in% LETTERS[4:9]) |   # site 2 2 has rep 3 in C,D,E
                          SITE==3 & SAMPLE_TYPE==1 & (REP==1 | REP==2 & TRANSECT %in% LETTERS[2:3] | REP==3 & TRANSECT %in% LETTERS[4:5]) |
                          SITE==3 & SAMPLE_TYPE==1 & (REP==3 | REP==2 & TRANSECT %in% LETTERS[2:3] | REP==1 & TRANSECT %in% LETTERS[4:5]) 
                         )
    actual <- expandValueRows(partialData, partialData[c('SITE','SAMPLE_TYPE','TRANSECT','REP')])
    dd <- dfCompare(fullData, actual, c('SITE','SAMPLE_TYPE','TRANSECT','REP'))
    checkTrue(is.null(dd), "Incorrectly changed partial data that did not need expansion")
    
    # Test case where changes should be made to data that has partial range of keys
    # where data is expected to be full
    actual <- expandValueRows(partialData, fullData[c('SITE','SAMPLE_TYPE','TRANSECT','REP')])
    dd <- dfCompare(fullData, actual, c('SITE','SAMPLE_TYPE','TRANSECT','REP'))
    checkTrue(is.null(dd), "Incorrectly changed partial data that did not need expansion")
    
    # Test case where changes should be made to data that has partial range of keys
    # where data is NOT expected to be full.  This mirrors the case when crews
    # have recorded one slope for a transect but several subsightings.
    actual <- expandValueRows(subset(partialData, SITE %in% 1:2 | REP==1)
                             ,partialData[c('SITE','SAMPLE_TYPE','TRANSECT','REP')]
                             )
    dd <- dfCompare(fullData, actual, c('SITE','SAMPLE_TYPE','TRANSECT','REP'))
    checkTrue(is.null(dd), "Incorrectly changed partial data that did not need expansion")
    
}

sb0 <- sb
sb <- nrsaSlopeBearing(bBearing = subset(cg, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='BEAR') %>% select(SITE, TRANSECT, LINE, VALUE)
                      ,bDistance = subset(cg, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DISTANCE')   %>% select(SITE, TRANSECT, LINE, VALUE)
                      ,bSlope = subset(cg, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='SLOPE')  %>% select(SITE, TRANSECT, LINE, VALUE, METHOD, UNITS) # excludes SLOPE_ND rows, and hence causes PCTCLINOMETER =0 rows to be missing
                      ,wBearing = subset(cg, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('BEARING', PARAMETER))
                      ,wIncrement = subset(thal, PARAMETER=='INCREMNT' & TRANSECT=='A' & STATION==0)[c('SITE','VALUE')]
                      ,wProportion = subset(cg, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('PROP', PARAMETER))
                      ,wSlope = subset(cg, SAMPLE_TYPE=='PHAB_SLOPE' & grepl('SLOPE', PARAMETER))
                      ,wStations = thal[c('SITE','TRANSECT','STATION')]
                      ,gisCalcs = gisCalcs
                      )
identical(sb,sb0)
dd0 <- dd
dd <- dfCompare(currentMets %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% 
                mutate(METRIC=trimws(METRIC), VALUE=as.numeric(trimws(VALUE))) %>% 
                subset(METRIC %in% toupper(unique(sb$METRIC)))
               ,sb %>% 
                mutate(METRIC=toupper(METRIC)
                      ,VALUE=as.numeric(VALUE)
                      )
               ,c('SITE','METRIC')
               ,zeroFudge=1e-6
               )

##########################################################
#
# Human Disturbance
nrsa0809<-odbcConnect('NRSA0809')
currentMets0 <- sqlFetch(nrsa0809, 'tblPHABMET', stringsAsFactors=FALSE)
raw0809 <- sqlFetch(nrsa0809, 'tblvisrip2', stringsAsFactors=FALSE)
rip0809 <- dbGet('NRSA0809','tblvisrip2') %>% mutate(TRANSECT=trimws(TRANSECT), TRANSDIR=trimws(TRANSDIR), PARAMETER=trimws(PARAMETER), RESULT=trimws(RESULT)) %>% dplyr::rename(UID=BATCHNO)
#hd1 <- metsHumanInfluence.1(rip0809)
hd <- nrsaHumanInfluence(buildings =          rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='BUILD', select=-PARAMETER)
                        ,landfillTrash =      rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='LANDFL', select=-PARAMETER)
                        ,logging =            rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='LOG', select=-PARAMETER)
                        ,mining =             rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='MINE', select=-PARAMETER)
                        ,parkLawn =           rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='PARK', select=-PARAMETER)
                        ,pastureRangeHay =    rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='PAST', select=-PARAMETER)
                        ,pavementClearedlot = rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='PAVE', select=-PARAMETER)
                        ,pipesInOut =         rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='PIPES', select=-PARAMETER)
                        ,roadsRailroads =     rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='ROAD', select=-PARAMETER)
                        ,rowcrops =           rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='ROW', select=-PARAMETER)
                        ,wallRevetment =      rip0809 %>% dplyr::rename(SITE=UID, VALUE=RESULT) %>% subset(PARAMETER=='WALL', select=-PARAMETER)
                        ,influenceWeights =   data.frame() # NOT IMPLEMENTED YET
                        )
oldhd <- currentMets %>% subset(PARAMETER %in% toupper(hd$METRIC)) %>% dplyr::rename(UID=BATCHNO,METRIC=PARAMETER)
dd<-dfCompare(hd1 %>% dplyr::rename(SITE=UID,VALUE=RESULT), hd, c('SITE','METRIC')) # no diffs with recalculations.
dd<-dfCompare(oldhd, hd %>% mutate(METRIC=toupper(METRIC)), c('UID','METRIC'))



# end of file