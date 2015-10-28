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




currentMets <- dbGet('NRSA0809', 'tblPHABMET') %>% dplyr::rename(SITE=UID,METRIC=RESULT)
visits <- dbGet('NRSA0809', 'tblVISITS2') %>% dplyr::rename(SITE=UID)

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