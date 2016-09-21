library(testthat)
library(aquametAlt)

context("Test benthic metric and MMI calculations")

#   # Must first create input dataset using WSA taxonomy and traits
#   bentCts <- merge(bentCts_test,dplyr::select(bentTaxa, -SAMPLE_TYPE),by=c('TARGET_TAXON','TAXA_ID'))
#   ## Roll mites, oligochaetes, and polychaetes up to family level
#   fixTaxa <- with(bentCts,which(CLASS %in% c('ARACHNIDA','POLYCHAETA','OLIGOCHAETA') & !is.na(FAMILY) & FAMILY!=''))
#   bentCts$TARGET_TAXON[fixTaxa] <- bentCts$FAMILY[fixTaxa]
#   bentCts.1 <- merge(bentCts,subset(bentTaxa,select=c('TAXA_ID','TARGET_TAXON')),by='TARGET_TAXON',all.x=TRUE) %>%
#       plyr::rename(c('TAXA_ID.y'='TAXA_ID')) %>% 
#       dplyr::select(-TAXA_ID.x, -PUBLICATION_DATE, -FFG, -HABIT, -PTV) %>%
#       plyr::mutate(TAXA_ID=ifelse(TARGET_TAXON %in% c('CRICOTOPUS/ORTHOCLADIUS','THIENEMANNIMYIA GENUS GR.'),3581
#                             ,ifelse(TARGET_TAXON %in% c('CERATOPOGONINAE'),3566,TAXA_ID))
#              ,TARGET_TAXON=ifelse(TAXA_ID==3581,'CHIRONOMIDAE',ifelse(TAXA_ID==3566,'CERATOPOGONIDAE',TARGET_TAXON)))   
# 
#   bentCts.2 <- plyr::ddply(bentCts.1,c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID'),summarise,TOTAL=sum(TOTAL300)) %>%
#       merge(dplyr::select(bentTaxa,-SAMPLE_TYPE),by='TAXA_ID')
# 
#   bentCts.3 <- assignDistinct(bentCts.2,c('UID','SAMPLE_TYPE','SAMPLE_CAT')) %>% 
#   plyr::mutate(IS_DISTINCT=ifelse(is.na(IS_DISTINCT),0,IS_DISTINCT))
#   
#   inTest <- subset(bentCts.3,select=c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID','TOTAL','IS_DISTINCT'))
 
test_that("Data prep correct",
{
  testOut <- prepBentCts_WSA(inCts=bentCts_test,inTaxa=bentTaxa
                             ,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                             ,ct='TOTAL300'
                             ,taxa_id='TAXA_ID')
  expect_equal(nrow(testOut),nrow(indf_test))
  expect_equal(names(testOut),names(indf_test))
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID'))
  indf.long <- reshape2::melt(indf_test,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID'))
  compOut <- merge(testOut.long,indf.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','TAXA_ID','variable'))
  expect_equal(compOut$value.x,compOut$value.y)
})


test_that("Benthic Taxonomy metric values correct",
          {
            testOut <- calcTaxonomyMets(indf_test,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
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
  testOut <- calcFFGmets(indf_test,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
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
  testOut <- calcHabitMets(indf_test,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
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
  testOut <- calcTolMets(indf_test,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
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
  testOut <- calcDominMets(indf_test,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                         ,dist='IS_DISTINCT',ct='TOTAL')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT') %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
  expect_true(nrow(compOut)==70)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
  })

test_that("All benthic metric values correct",
          {
            testOut <- calcAllBentMets(indf=indf_test,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                       ,dist='IS_DISTINCT',ct='TOTAL',taxa_id='TAXA_ID',ffg='FFG_WSA'
                                       ,habit='HABIT_WSA',ptv='PTV_WSA')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                           ,variable.name='PARAMETER',value.name='RESULT') %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
            expect_true(nrow(compOut)==1250)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
          })


ecoTest <- data.frame(UID=c(11245,11703,11821,12483,12571,12999,13971,14822,15073,15803)
                      ,AGGR_ECO9_2015=c('TPL','CPL','NAP','SAP','SAP','UMW','CPL','NPL'
                                        ,'UMW','XER'),stringsAsFactors=F)
indf.eco <- merge(indf_test,ecoTest,by='UID')
test_that("MMI metrics correct",
          {
            testOut <- calcNRSA_BentMMImets(inCts=indf.eco,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                       ,dist='IS_DISTINCT',ct='TOTAL',taxa_id='TAXA_ID',ffg='FFG_WSA'
                                       ,habit='HABIT_WSA',ptv='PTV_WSA',ecoreg='AGGR_ECO9_2015')
            testOut.long <- reshape2::melt(testOut,id.vars=c('UID','SAMPLE_TYPE','SAMPLE_CAT','AGGR_ECO9_2015')
                                           ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
              plyr::mutate(PARAMETER=as.character(PARAMETER))
            compOut <- merge(bentMet_test,testOut.long,by=c('UID','SAMPLE_TYPE','SAMPLE_CAT','PARAMETER'))
            expect_true(nrow(compOut)==60)
            expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001)
            
          })


testIn <- merge(bentMet_test,ecoTest,by='UID') %>%
  reshape2::dcast(UID+SAMPLE_TYPE+SAMPLE_CAT+AGGR_ECO9_2015~PARAMETER,value.var='RESULT')

test_that("NRSA Benthic MMI scores correct",
{
  testOut <- calcNRSA_BenthicMMI(testIn,sampID=c('UID','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,ecoreg='AGGR_ECO9_2015',totlnind='TOTLNIND')
  testOut.long <- reshape2::melt(testOut,id.vars=c('UID','AGGR_ECO9_2015','SAMPLE_TYPE','SAMPLE_CAT')
                                 ,variable.name='PARAMETER',value.name='RESULT',na.rm=T) %>%
    plyr::mutate(PARAMETER=as.character(PARAMETER))
  bentMMI_test.long <- reshape2::melt(bentMMI_test,id.vars=c('UID')
                                      ,variable.name='PARAMETER',value.name='RESULT')
  compOut <- merge(bentMMI_test.long,testOut.long,by=c('UID','PARAMETER'))
  expect_true(nrow(compOut)==80)
  expect_equal(compOut$RESULT.x,compOut$RESULT.y,tolerance=0.0001) 
  
})