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

#######################################################
#
# Canopy densiometer
#
chancovbase <- dbGet('NRSA0809','tblCHANCOV2')
chancov <- chancovbase %>% dplyr::rename(SITE=BATCHNO, VALUE=RESULT) %>% 
           mutate(TRANSECT=trimws(TRANSECT), TRANSDIR=trimws(TRANSDIR), SAMPLE_TYPE=trimws(SAMPLE_TYPE), VALUE=as.numeric(trimws(VALUE)), DIRECTION=trimws(TRANSDIR)
                 )
chancovMain <- chancov %>% subset(TRANSECT %in% LETTERS) %>% 
               mutate(TRANSECT=trimws(TRANSECT), TRANSDIR=trimws(TRANSDIR), SAMPLE_TYPE=trimws(SAMPLE_TYPE))

cd <- nrsaCanopyDensiometer(bDensiom=subset(chancov, SAMPLE_TYPE=='PHAB_CHANB'), wDensiom=subset(chancov, SAMPLE_TYPE=='PHAB_CHANW'))
chancovForOldCode <- chancovbase %>% 
                     dplyr::rename(UID=BATCHNO) %>% 
                     mutate(TRANSECT=trimws(TRANSECT), TRANSDIR=trimws(TRANSDIR), SAMPLE_TYPE=trimws(SAMPLE_TYPE), PARAMETER=trimws(PARAMETER), RESULT=as.numeric(trimws(RESULT)))
oldrecalc <- metsCanopyDensiometer.1(chancovForOldCode)

cd0809 <- subset(currentMets, PARAMETER %in% c('XCDENBK', 'VCDENBK','NBNK', 'XCDENMID','VCDENMID','NMID')) %>% dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT)
dd <- dfCompare(cd0809, cd %>% mutate(METRIC=toupper(METRIC)), c('SITE','METRIC'))
ddold <- dfCompare(subset(currentMets, PARAMETER %in% c('XCDENBK', 'VCDENBK','NBNK', 'XCDENMID','VCDENMID','NMID')) %>% dplyr::rename(UID=BATCHNO, METRIC=PARAMETER)
                  ,oldrecalc %>% mutate(METRIC=toupper(METRIC))
                  ,c('UID','METRIC')
                  )
recalcDiffs <- dfCompare(oldrecalc %>% dplyr::rename(SITE=UID, VALUE=RESULT), cd, c('SITE','METRIC'))
# recalcs using unchanged mets code are identical to recalcs using new code.
# total of 33 differences at 11 sites when including side channels, all boatables and now with larger N values: 11727, 12353, 13626, 14217, 14622, 14968, 14969, 14970, 14971, 15297, 15408
# total of 271 differences at many sites when excluding side channels


##################################################
#
# Channel Characteristics

bankgeometryBase <- dbGet('NRSA0809','tblBANKGEOMETRY2')
channelcharBase <- dbGet('NRSA0809','tblCHANNELCHAR2')

bankgeometry <- bankgeometryBase %>% 
                dplyr::rename(SITE=BATCHNO, VALUE=RESULT) %>% 
                mutate(PARAMETER=trimws(PARAMETER), SAMPLE_TYPE=trimws(SAMPLE_TYPE), TRANSECT=trimws(TRANSECT), VALUE=trimws(VALUE)) %>% 
                subset(TRANSECT %in% LETTERS)
channelchar <- channelcharBase %>% 
                dplyr::rename(SITE=BATCHNO, VALUE=RESULT) %>% 
                mutate(PARAMETER=trimws(PARAMETER), SAMPLE_TYPE=trimws(SAMPLE_TYPE), TRANSECT=trimws(TRANSECT), VALUE=trimws(VALUE))
ccRecalc <- nrsaChannelChar(bankgeometry, channelchar)

dd0 <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(ccRecalc$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT)
               ,ccRecalc %>% mutate(METRIC=toupper(METRIC))
               ,c('SITE','METRIC')
               )
# There are 9 differences, all because 'NA' is not equal to NA.

cc <- nrsaChannelChar(bankfullWidth =         subset(channelchar, PARAMETER == 'BANKFULL')
                     ,channelPattern =        subset(channelchar, PARAMETER == 'PATTERN')
                     ,constraintFeatures =    subset(channelchar, PARAMETER == 'FEATURES')
                     ,constraintMultiple =    subset(bankgeometry, PARAMETER == 'CONSTRT')
                     ,constraintSingle =      subset(channelchar, PARAMETER == 'CONSTRNT')
                     ,constraintPercent =     subset(channelchar, PARAMETER == 'PERCENT')
                     ,seeOverBank =           subset(bankgeometry, PARAMETER == 'SEEOVRBK')
                     ,shoreToVegDistance =    subset(bankgeometry, PARAMETER == 'SHOR2RIP')
                     ,valleyConstraintUnseen =subset(channelchar, PARAMETER == 'VALLYBOX')
                     ,valleyWidth =           subset(channelchar, PARAMETER == 'VALLEY')
                     )
dd <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(ccRecalc$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT)
               ,cc %>% mutate(METRIC=toupper(METRIC))
               ,c('SITE','METRIC')
               )
# There are the same 9 differences, all because 'NA' is not equal to NA.


##################################################
#
# Channel Habitat
thalwegBase <-  dbGet('NRSA0809', 'tblTHALWEG2')
thalweg <- thalwegBase %>% mutate(PARAMETER=trimws(PARAMETER), SAMPLE_TYPE=trimws(SAMPLE_TYPE), TRANSECT=trimws(TRANSECT), STATION=trimws(STATION), RESULT=trimws(RESULT)) %>% 
           dplyr::rename(SITE=BATCHNO,VALUE=RESULT) 
chRecalc <- metsChannelHabitat(thalweg)
dd0 <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(chRecalc$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT)
               ,chRecalc %>% mutate(METRIC=toupper(METRIC))
               ,c('SITE','METRIC')
               )
# Zero differences!
ch <- nrsaChannelHabitat(bChannelUnit=thalweg %>% subset(PARAMETER=='CHANUNCD' & SAMPLE_TYPE=='PHAB_THAL')
                        ,wChannelUnit=thalweg %>% subset(PARAMETER=='CHANUNCD' & SAMPLE_TYPE=='PHAB_THALW')
                        )
dd <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(chRecalc$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT)
               ,chRecalc %>% mutate(METRIC=toupper(METRIC))
               ,c('SITE','METRIC')
               )
# Zero differences still!


##################################################
#
# Channel Morphology
# NOTE: This was hard to get right -- need to capitalize UNITS, trimws() everything+, subset TRANSECT but keep 'NOT MARKED', or results will be quietly wrong.
bankgeometryBase <- dbGet('NRSA0809', 'tblBANKGEOMETRY2')
thalwegBase <- dbGet('NRSA0809', 'tblThalweg2')
visitsBase <- dbGet('NRSA0809', 'tblVisits2')
bankgeometry <- bankgeometryBase %>% 
                dplyr::rename(SITE=BATCHNO,VALUE=RESULT) %>% 
                mutate(TRANSECT=trimws(TRANSECT)
                      ,TRANSDIR=trimws(TRANSDIR)
                      ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
                      ,PARAMETER=trimws(PARAMETER)
                      ,VALUE=trimws(VALUE)
                      ,UNITS=trimws(UNITS)
                      ) %>% 
                subset(TRANSECT %in% LETTERS | SITE %in% wadedSites)
thalweg <- thalwegBase %>% 
            dplyr::rename(SITE=BATCHNO,VALUE=RESULT) %>% 
            mutate(TRANSECT=trimws(TRANSECT)
                  ,STATION=trimws(STATION)
                  ,SAMPLE_TYPE=trimws(SAMPLE_TYPE)
                  ,PARAMETER=trimws(PARAMETER)
                  ,VALUE=trimws(VALUE)
                  ,UNITS=toupper(trimws(UNITS))
                  ) %>% 
            subset(TRANSECT %in% c(LETTERS,'NOT MARKED') | SITE %in% wadedSites)
visits <- visitsBase %>% dplyr::rename(SITE=BATCHNO) %>% mutate(VALXSITE=trimws(VALXSITE))

wadedSites <- subset(siteProtocol(visits$SITE, visits), PROTOCOL=='WADEABLE' | SITE %in% c(1417,15508))$SITE # Two sites are OTHER_NSP and PARBYBOAT but were waded
#wadedSites <- subset(siteProtocol(visits$SITE, visits), PROTOCOL=='WADEABLE')$SITE 
cmRecalc <- nrsaChannelMorphology(bankgeometry, thalweg, visits)
dd0 <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(cmRecalc$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE))
               ,cmRecalc %>% mutate(METRIC=toupper(METRIC)) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE))
               ,c('SITE','METRIC')
               )
# zero differences! after some finagling..

cm <- nrsaChannelMorphology(bBankHeight =     bankgeometry %>% subset(SITE %nin% wadedSites & PARAMETER %in% c('BANKHT','BANKHGT'))
                           ,bBankWidth =      bankgeometry %>% subset(SITE %nin% wadedSites & PARAMETER=='BANKWID')
                           ,bDepth =          thalweg %>% subset(SITE %nin% wadedSites & PARAMETER %in% c('DEP_POLE','DEP_SONR'))
                           ,bIncisedHeight =  bankgeometry %>% subset(SITE %nin% wadedSites & PARAMETER %in% c('INCISED','INCISHGT'))
                           ,bWettedWidth =    bankgeometry %>% subset(SITE %nin% wadedSites & PARAMETER=='WETWID')
                           ,wBankHeight =     bankgeometry %>% subset(SITE %in% wadedSites & PARAMETER=='BANKHGT')
                           ,wBankWidth =      bankgeometry %>% subset(SITE %in% wadedSites & PARAMETER=='BANKWID')
                           ,wDepth =          thalweg %>% subset(SITE %in% wadedSites & PARAMETER=='DEPTH')
                           ,wIncisedHeight =  bankgeometry %>% subset(SITE %in% wadedSites & PARAMETER=='INCISHGT')
                           ,wWettedWidth =    thalweg %>% subset(SITE %in% wadedSites & PARAMETER=='WETWIDTH')
                           )
dd <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(cm$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE))
               ,cm %>% mutate(METRIC=toupper(METRIC)) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE))
               ,c('SITE','METRIC')
               )
# 33 differences all in 1417 (OTHER_NSP) and 15508 (PARBYBOAT), both recorded as wadeable.
# In currentMets, this means that 15508 is missing 20 metrics -- all but DEPTH related 
# values which are present but expressed in meters; should have been converted back to cm
# as they are now. It also resulted in 1417 an incorrect reduction in values used
# for metrics calculation, as well as unit mishandling noted for 15508.


##################################################
# Fish Cover
base_fishCover <- dbGet('NRSA0809','tblFISHCOVER2')
fishCover <- base_fishCover %>% 
             dplyr::rename(SITE=BATCHNO,VALUE=RESULT) %>% 
             mutate(TRANSECT=trimws(TRANSECT)
                   ,PARAMETER=trimws(PARAMETER)
                   ,VALUE=trimws(VALUE)
                   ) %>% 
            subset(TRANSECT %in% LETTERS | SAMPLE_TYPE == 'PHAB_CHANW') # get rid of boatable side channels, as we did before for some reason

fc <- nrsaFishCover(algae = fishCover %>% subset(PARAMETER=='ALGAE')
                   ,boulder = fishCover %>% subset(PARAMETER=='BOULDR')
                   ,brush = fishCover %>% subset(PARAMETER=='BRUSH')
                   ,liveTree = fishCover %>% subset(PARAMETER=='LVTREE')
                   ,macrophytes = fishCover %>% subset(PARAMETER=='MACPHY')
                   ,overhang = fishCover %>% subset(PARAMETER=='OVRHNG')
                   ,structures = fishCover %>% subset(PARAMETER=='STRUCT')
                   ,undercut = fishCover %>% subset(PARAMETER %in% c('UNDCUT','UNDERCUT'))
                   ,woodyDebris = fishCover %>% subset(PARAMETER=='WOODY')
                   )


dd <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(fc$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE))
               ,fc %>% mutate(METRIC=toupper(METRIC)) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE))
               ,c('SITE','METRIC')
               )
# No diffreences once data for boatable side channels are removed.


##################################################
# Invasive Species
base_invasiveLegacy <- dbGet('NRSA0809','tblINVASIVELEGACY2')
invasiveLegacy <- base_invasiveLegacy %>% 
                  dplyr::rename(SITE=BATCHNO,VALUE=RESULT) %>% 
                  mutate(TRANSECT=trimws(TRANSECT)
                        ,PARAMETER=trimws(PARAMETER)
                        ,VALUE=trimws(VALUE)
                        ) %>% 
                  subset(PARAMETER %in%  c('E_WTRMILF','HYDRILLA','W_HYACINTH','YLW_FLHEAR','P_LSTRIFE','G_REED','FLWR_RUSH','SALT_CED','MF_ROSE','SPURGE','NO_INVASIVES'))

oneSpecies <- function(spName) {
    if(spName %nin% invasiveLegacy$PARAMETER) return(NULL)
    spData <- invasiveLegacy %>%
              dcast(SITE+TRANSECT~PARAMETER, value.var='VALUE') %>%
              within(VALUE <- ifelse(is.na(get(spName)), '', get(spName))) %>%
              select(SITE, TRANSECT, VALUE)
    return(spData)
}
is <- nrsaInvasiveSpecies(arudon = oneSpecies('G_REED')
                         ,butumb = oneSpecies('FLWR_RUSH')
                         ,eiccra = oneSpecies('W_HYACINTH')
                         ,eupesu = oneSpecies('SPURGE')
                         ,hydver = oneSpecies('HYDRILLA')
                         ,lytsal = oneSpecies('P_LSTRIFE')
                         ,myrspi = oneSpecies('E_WTRMILF')
                         ,none   = oneSpecies('NO_INVASIVES')
                         ,nympel = oneSpecies('YLW_FLHEAR')
                         ,rosmul = oneSpecies('MF_ROSE')
                         ,tamspp = oneSpecies('SALT_CED')
                         )
isold <- nrsaInvasiveSpecies(arudon = oneSpecies('G_REED')
                         ,butumb = oneSpecies('FLWR_RUSH')
                         ,eiccra = oneSpecies('W_HYACINTH')
                         ,eupesu = oneSpecies('SPURGE')
                         ,hydver = oneSpecies('HYDRILLA')
                         ,lytsal = oneSpecies('P_LSTRIFE')
                         ,myrspi = oneSpecies('E_WTRMILF')
                         ,none   = oneSpecies('NO_INVASIVES')
                         ,nympel = oneSpecies('YLW_FLTHEAR')    # 0809 calculations were done with this PARAMETER mis-spelling!!
                         ,rosmul = oneSpecies('MF_ROSE')
                         ,tamspp = oneSpecies('SALT_CED')
                         )

# make comparisons after removing new rows with f_* = 0, which weren't included in the old mets
dd <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(is$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE))
               ,is %>% 
                mutate(METRIC=toupper(METRIC)) %>% 
                mutate(VALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE)) %>% 
                subset(METRIC=='IP_SCORE' | as.numeric(VALUE) != 0)
               ,c('SITE','METRIC')
               )
dd2 <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(is$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE))
               ,isold %>% 
                mutate(METRIC=toupper(METRIC)) %>% 
                mutate(VALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE)) %>% 
                subset(METRIC=='IP_SCORE' | as.numeric(VALUE) != 0)
               ,c('SITE','METRIC')
               )

# 5 differences, all in 15652 and due to difference in how yellow floating heart data was included or not.  Damn.

source('c:/Users/cseelige/local/aquamet/R/metsInvasiveSpecies.r')
isRecalc <- metsInvasiveSpecies(base_invasiveLegacy %>% dplyr::rename(UID=BATCHNO) %>% mutate(TRANSECT=trimws(TRANSECT),PARAMETER=trimws(PARAMETER),RESULT=trimws(RESULT)) )
ddRecalc <- dfCompare(is %>% subset(METRIC=='IP_SCORE' | as.numeric(VALUE) != 0)
                     ,isRecalc %>% dplyr::rename(SITE=UID, VALUE=RESULT)
                     ,c('SITE','METRIC')
                     )

##################################################
# Large woody debris
base_largewoody <- dbGet('NRSA0809','tblWOOD2')
largewoody <- base_largewoody %>% 
              dplyr::rename(SITE=BATCHNO,VALUE=RESULT) %>% 
              mutate(TRANSECT=trimws(TRANSECT)
                    ,PARAMETER=trimws(PARAMETER)
                    ,VALUE=trimws(VALUE)
                    ) %>%
              subset(TRANSECT %in% LETTERS | SAMPLE_TYPE == 'PHAB_THALW') # get rid of boatable side channels, as we did before for some reason

lw <- nrsaLargeWoody(bDryExtralargeDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DXDLL')
                    ,bDryLargeDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DLDLL')
                    ,bDryMediumDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DMDLL')
                    ,bDrySmallDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DSDLL')
                    ,bDryExtralargeDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DXDML')
                    ,bDryLargeDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DLDML')
                    ,bDryMediumDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DMDML')
                    ,bDrySmallDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DSDML')
                    ,bDryExtralargeDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DXDSL')
                    ,bDryLargeDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DLDSL')
                    ,bDryMediumDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DMDSL')
                    ,bDrySmallDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='DSDSL')
                    ,bWetExtralargeDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WXDLL')
                    ,bWetLargeDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WLDLL')
                    ,bWetMediumDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WMDLL')
                    ,bWetSmallDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WSDLL')
                    ,bWetExtralargeDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WXDML')
                    ,bWetLargeDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WLDML')
                    ,bWetMediumDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WMDML')
                    ,bWetSmallDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WSDML')
                    ,bWetExtralargeDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WXDSL')
                    ,bWetLargeDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WLDSL')
                    ,bWetMediumDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WMDSL')
                    ,bWetSmallDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_CHANBFRONT' & PARAMETER=='WSDSL')
                    ,wDryExtralargeDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DXDLL')
                    ,wDryLargeDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DLDLL')
                    ,wDryMediumDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DMDLL')
                    ,wDrySmallDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DSDLL')
                    ,wDryExtralargeDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DXDML')
                    ,wDryLargeDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DLDML')
                    ,wDryMediumDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DMDML')
                    ,wDrySmallDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DSDML')
                    ,wDryExtralargeDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DXDSL')
                    ,wDryLargeDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DLDSL')
                    ,wDryMediumDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DMDSL')
                    ,wDrySmallDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='DSDSL')
                    ,wWetExtralargeDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WXDLL')
                    ,wWetLargeDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WLDLL')
                    ,wWetMediumDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WMDLL')
                    ,wWetSmallDiamLongLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WSDLL')
                    ,wWetExtralargeDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WXDML')
                    ,wWetLargeDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WLDML')
                    ,wWetMediumDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WMDML')
                    ,wWetSmallDiamMediumLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WSDML')
                    ,wWetExtralargeDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WXDSL')
                    ,wWetLargeDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WLDSL')
                    ,wWetMediumDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WMDSL')
                    ,wWetSmallDiamShortLength = subset(largewoody, SAMPLE_TYPE=='PHAB_THALW' & PARAMETER=='WSDSL')
                    ,reachlength = subset(currentMets, PARAMETER=='REACHLEN') %>% dplyr::rename(SITE=BATCHNO,VALUE=RESULT)
                    ,meanBankfullWidth = subset(currentMets, PARAMETER=='XBKF_W') %>% dplyr::rename(SITE=BATCHNO,VALUE=RESULT)
                    )
dd <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(lw$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE))
               ,lw %>% 
                mutate(METRIC=toupper(METRIC)) %>% 
                mutate(VALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE)) 
               ,c('SITE','METRIC')
               )
ss <- merge(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(lw$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% 
                mutate(oldVALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE)) %>%
                select(SITE, METRIC, oldVALUE) %>% mutate(inOld = TRUE)
               ,lw %>% 
                mutate(METRIC=toupper(METRIC)) %>% 
                mutate(newVALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE))  %>%
                select(SITE, METRIC, newVALUE) %>% mutate(inNew = TRUE)
               ,c('SITE','METRIC'), all=TRUE
               )
# 276 new rows, all for sites 13785 & 15508, plus 45 differences in site 1417. 
# Reasons:
# 1417 was calculated as boatable, but here is calculated as wadeable.
# 15508 is OTHER_NSP (neither boatable nor wadeable)
# 13785 is not in tblVISITS2


##################################################
# Legacy Tree metrics
base_invasiveLegacy <- dbGet('NRSA0809','tblINVASIVELEGACY2')
invasiveLegacy <- base_invasiveLegacy %>% 
                  dplyr::rename(SITE=BATCHNO,VALUE=RESULT) %>% 
                  mutate(TRANSECT=trimws(TRANSECT)
                        ,PARAMETER=trimws(PARAMETER)
                        ,VALUE=trimws(VALUE)
                        ) %>% subset(PARAMETER %in% c('DBH','DISTANCE','HEIGHT','SPECIES','TREE_TYP','NOT_VIS'))

lt <- nrsaLegacyTree(dbhClass = subset(invasiveLegacy, PARAMETER=='DBH')
                    ,distance = subset(invasiveLegacy, PARAMETER=='DISTANCE')
                    ,heightClass = subset(invasiveLegacy, PARAMETER=='HEIGHT')
                    ,species = subset(invasiveLegacy, PARAMETER=='SPECIES')
                    ,type = subset(invasiveLegacy, PARAMETER=='TREE_TYP')
                    )
dd <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(lt$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE))
               ,lt %>% 
                mutate(METRIC=toupper(METRIC)) %>% 
                mutate(VALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE)) 
               ,c('SITE','METRIC')
               )

# 468 differences
# Reasons: 
#  287 in LTMDDOM & LTMDSUB and thus (likely) due to ordering of species.
#  8 in LTMDDIST that changed from NaN to NA
#  173 in LTMXSPP, which is row-order dependent.


##################################################
# Riparian vegetation metrics
base_riparianveg <- dbGet('NRSA0809','tblVISRIP2')
riparianveg <- base_riparianveg %>% 
               dplyr::rename(SITE=BATCHNO, VALUE=RESULT, BANK=TRANSDIR) %>% 
               mutate(TRANSECT=trimws(TRANSECT)
                     ,PARAMETER=trimws(PARAMETER)
                     ,BANK=trimws(BANK)
                     ,VALUE=trimws(VALUE)
                     ) %>%
               subset(TRANSECT %in% LETTERS | SAMPLE_TYPE == 'PHAB_CHANW') # get rid of boatable side channels, as we did before for some reason


rv <- nrsaRiparianVegetation(canopyCoverLargeDiameter = subset(riparianveg, PARAMETER=='CANBTRE')
                            ,canopyCoverSmallDiameter = subset(riparianveg, PARAMETER=='CANSTRE')
                            ,canopyVegetationType = subset(riparianveg, PARAMETER=='CANVEG')
                            ,groundCoverBare = subset(riparianveg, PARAMETER=='BARE')
                            ,groundCoverNonwoody = subset(riparianveg, PARAMETER=='GCNWDY')
                            ,groundCoverWoody = subset(riparianveg, PARAMETER=='GCWDY')
                            ,understoryCoverNonwoody = subset(riparianveg, PARAMETER=='UNDNWDY')
                            ,understoryCoverWoody = subset(riparianveg, PARAMETER=='UNDWDY')
                            ,understoryVegetationType = subset(riparianveg, PARAMETER=='UNDERVEG')
                            )
dd <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(rv$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE))
               ,rv %>% 
                mutate(METRIC=toupper(METRIC)) %>% 
                mutate(VALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE)) 
               ,c('SITE','METRIC')
               )
# No differences!


##################################################
# Substrate embededness
base_ccs <- dbGet('NRSA0809','tblCHANNELCROSSSECTION2')
ccs <- base_ccs %>% 
                  dplyr::rename(SITE=BATCHNO,VALUE=RESULT) %>% 
                  mutate(TRANSECT=trimws(TRANSECT)
                        ,PARAMETER=trimws(PARAMETER)
                        ,VALUE=trimws(VALUE)
                        ) #%>% subset(PARAMETER %in% c('DBH','DISTANCE','HEIGHT','SPECIES','TREE_TYP','NOT_VIS'))
se <- nrsaSubstrateEmbed(percentEmbedded = subset(ccs, PARAMETER=='EMBED') %>% 
                                           mutate(ONBANK=ifelse(TRANSDIR %in% c('LC','CT','RC'), FALSE, TRUE))
                        )
dd <- dfCompare(currentMets %>% 
                subset(PARAMETER %in% toupper(unique(se$METRIC))) %>% 
                dplyr::rename(SITE=BATCHNO, METRIC=PARAMETER, VALUE=RESULT) %>% mutate(VALUE = ifelse(VALUE %in% c(NA, 'NA'), NA, VALUE))
               ,se %>% 
                mutate(METRIC=toupper(METRIC)) %>% 
                mutate(VALUE = ifelse(VALUE %in% c(NA, NaN), NA, VALUE)) 
               ,c('SITE','METRIC')
               )
# No differences!

##################################################
#


##################################################
#


##################################################
#
# Bed Stability

bankgeometryBase <- dbGet('NRSA0809','tblBANKGEOMETRY2')
thalwegBase <- dbGet('NRSA0809','tblTHALWEG2')
visitsBase <- dbGet('NRSA0809','tblVISITS2')
channelgeometryBase <- dbGet('NRSA0809','tblCHANNELGEOMETRY2')
channelcrosssectionBase <- dbGet('NRSA0809','tblCHANNELGEOMETRY2')
littoralBase <- dbGet('NRSA0809','tblLITTORAL2')
woodBase <- dbGet('NRSA0809','tblWOOD2')
fishcoverBase <- dbGet('NRSA0809','tblFISHCOVER2')
giscalcs0809 <- read.csv('l:/Priv/CORFiles/IM/Rwork/nrsa/results/gpsBasedCalculations_asOf201203008.csv', stringsAsFactors=FALSE)

oldRecalcFull <- nrsaBedStability(bankgeometryBase, thalwegBase, visitsBase, channelgeometryBase, channelcrosssectionBase, littoralBase, woodBase, fishcoverBase, gisCalcs=giscalcs0809)


# end of file