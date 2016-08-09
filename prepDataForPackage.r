library(reshape2)
library(plyr)
library(dplyr)

bugMet <- read.delim("L:/priv/CORfiles/IM-TH007/data/im/nrsa/data/tabfiles/bentMet.tab",sep='\t',stringsAsFactors=F)
bugMet.1 <- filter(bugMet,!is.na(TOTLNIND) & INDEX_VISIT=='YES') %>%
  select(-PUBLICATION_DATE,-INDEX_VISIT,-DATE_COL,-GREAT_RIVER,-FW_ECO9,-FW_ECO3,-SITE_CLASS
                   ,-STRAHLERORDER,-DATE_BENT,-BENT_COM) 
bugMet.long <- melt(bugMet.1,id.vars=c('UID','SITE_ID','YEAR','VISIT_NO','SAMPLE_TYPE','SAMPLE_CAT'),variable.name='PARAMETER'
                    ,value.name='RESULT')

uids <- data.frame(UID=unique(bugMet.long$UID),stringsAsFactors=F)

set.seed(123456)
sampUIDs <- data.frame(UID=sample(uids$UID,10),stringsAsFactors=F)

bugMet.long <- merge(sampUIDs,bugMet.long,by='UID')
nrow(unique(bugMet.long[,c('UID','SAMPLE_TYPE','SAMPLE_CAT')]))

bugCts <- read.delim("L:/priv/CORFiles/IM-TH007/data/im/nrsa/data/tabfiles/bentCts.tab",sep='\t',stringsAsFactors=F)
bugCts.1 <- select(bugCts,UID,SAMPLE_TYPE,SAMPLE_CAT,TAXA_ID,TARGET_TAXON,TOTAL300,IS_DISTINCT300) %>% 
  filter(!is.na(TOTAL300) & SAMPLE_CAT=='P') %>%
  merge(sampUIDs,by='UID')

nrow(unique(bugCts.1[,c('UID','SAMPLE_TYPE','SAMPLE_CAT')]))

bentMet_test <- mutate(bugMet.long,PARAMETER=as.character(PARAMETER),RESULT=as.numeric(RESULT)) 
bentCts_test <- bugCts.1

save(bentMet_test,file="C:/users/kblockso/my documents/temp/aquamet Update Files/aquametAlt/tests/bentMet_test.rda")
save(bentCts_test,file="C:/users/kblockso/my documents/temp/aquamet Update Files/aquametAlt/tests/bentCts_test.rda")

# Now bring in MMI scores and condition class
bentMMI <- read.delim("L:/priv/CORFiles/IM-TH007/data/im/nrsa/data/tabfiles/bentCond.tab",sep='\t',stringsAsFactors=F) %>%
  select(UID,AGGR_ECO9_2015,SAMPLE_TYPE,BENT_MMI_COND,MMI_BENT,COMP_PT,DIVS_PT,FEED_PT,HABT_PT,RICH_PT,TOLR_PT) %>%
  merge(sampUIDs,by='UID')

bentMMI_test <- bentMMI

save(bentMMI_test,file="C:/users/kblockso/my documents/temp/aquamet Update Files/aquametAlt/tests/bentMMI_test.rda")

