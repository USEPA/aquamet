library(testthat)
library(aquametAlt)

context("Test benthic metric and MMI calculations")

# test_that("Benthic Metrics",
#   {
#    testOut <- invertMet(bentCts_test,bentTaxa,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT'),dist='IS_DISTINCT300',ct='TOTAL300'
#                         ,taxlevel='WSA')
#    testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT','METRIC_TAXA_ROOT')
#                                   ,variable.name='PARAMETER',value.name='RESULT') %>%
#      plyr::mutate(PARAMETER=as.character(PARAMETER),RESULT=as.numeric(RESULT))
#    compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
#    expect_true(nrow(compOut)==1270)
#    expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
#   }
# )


  # Must first create input dataset using WSA taxonomy and traits
  bentCts <- merge(bentCts_test,select(bentTaxa, -SAMPLE_TYPE),by=c('TARGET_TAXON','TAXA_ID'))
  ## Roll mites, oligochaetes, and polychaetes up to family level
  fixTaxa <- with(bentCts,which(CLASS %in% c('ARACHNIDA','POLYCHAETA','OLIGOCHAETA') & !is.na(FAMILY) & FAMILY!=''))
  bentCts$TARGET_TAXON[fixTaxa] <- bentCts$FAMILY[fixTaxa]
  bentCts.1 <- merge(bentCts,subset(bentTaxa,select=c('TAXA_ID','TARGET_TAXON')),by='TARGET_TAXON',all.x=TRUE) %>%
      plyr::rename(c('TAXA_ID.y'='TAXA_ID')) %>% 
      select(-TAXA_ID.x, -PUBLICATION_DATE, -FFG, -HABIT, -PTV) %>%
      mutate(TAXA_ID=ifelse(TARGET_TAXON %in% c('CRICOTOPUS/ORTHOCLADIUS','THIENEMANNIMYIA GENUS GR.'),3581
                            ,ifelse(TARGET_TAXON %in% c('CERATOPOGONINAE'),3566,TAXA_ID))
             ,TARGET_TAXON=ifelse(TAXA_ID==3581,'CHIRONOMIDAE',ifelse(TAXA_ID==3566,'CERATOPOGONIDAE',TARGET_TAXON)))   

  bentCts.2 <- plyr::ddply(bentCts.1,c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID'),summarise,TOTAL=sum(TOTAL300)) %>%
      merge(dplyr::select(bentTaxa,-SAMPLE_TYPE),by='TAXA_ID')

  bentCts.3 <- assignDistinct(bentCts.2,c('UID','SAMPLE_TYPE','SAMPLE_CAT')) %>% 
  mutate(IS_DISTINCT=ifelse(is.na(IS_DISTINCT),0,IS_DISTINCT))
  
  inTest <- subset(bentCts.3,select=c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID','TOTAL','IS_DISTINCT'))
  

test_that("Benthic Taxonomy metric values correct",
          {
            testOut <- calcTaxonomyMets(inTest,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                        ,dist='IS_DISTINCT',ct='TOTAL')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                           ,variable.name='PARAMETER',value.name='RESULT') %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
            expect_true(nrow(compOut)==550)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
            
          })

test_that("Benthic FFG metric values correct",
{
  testOut <- calcFFGmets(inTest,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                              ,dist='IS_DISTINCT',ct='TOTAL',ffg='FFG_WSA')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
  expect_true(nrow(compOut)==180)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})


test_that("Benthic Habit metric values correct",
{
  testOut <- calcHabitMets(inTest,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                         ,dist='IS_DISTINCT',ct='TOTAL',habit='HABIT_WSA')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
  expect_true(nrow(compOut)==150)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})

test_that("Benthic tolerance metric values correct",
{
  testOut <- calcTolMets(inTest,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                           ,dist='IS_DISTINCT',ct='TOTAL',ptv='PTV_WSA')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
  expect_true(nrow(compOut)==280)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  
})


test_that("Benthic dominance and diversity metric values correct",
{
  testOut <- calcDominMets(inTest,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                         ,dist='IS_DISTINCT',ct='TOTAL')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
  expect_true(nrow(compOut)==70)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  })