library(reshape2)
library(plyr)
library(dplyr)

## BENTHIC MACROINVERTEBRATES

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

## FISH
fishMet <- read.delim("L:/priv/CORfiles/IM-TH007/data/im/nrsa/data/tabfiles/fishMet.tab",sep='\t',stringsAsFactors=F)
fishMet.1 <- filter(fishMet,!is.na(TOTLNIND) & TOTLNIND!=0 & INDEXVIS_FISH=='YES') %>%
  select(-PUBLICATION_DATE,-INDEXVIS_FISH,-DATE_COL,-GREAT_RIVER,-AGGR_ECO3_2015,-AGGR_ECO9_2015,-SITE_CLASS
         ,-INDEX_VISIT,-RT_NRSA,-STRAHLERORDER,-METHOD,-SAMPLED_FISH,-FISH_PROTOCOL,-NOTFISH) 
fishMet.long <- melt(fishMet.1,id.vars=c('UID','SITE_ID','YEAR','VISIT_NO','SAMPLE_TYPE'),variable.name='PARAMETER'
                    ,value.name='RESULT')

fishuids <- data.frame(UID=unique(fishMet.long$UID),stringsAsFactors=F)

set.seed(78910)
sampUIDs.fish <- data.frame(UID=sample(fishuids$UID,10),stringsAsFactors=F)

fishMet.long <- merge(sampUIDs.fish,fishMet.long,by='UID')
nrow(unique(fishMet.long[,c('UID','SAMPLE_TYPE')]))

fishCts <- read.delim("L:/priv/CORFiles/IM-TH007/data/im/nrsa/data/tabfiles/fishCts.tab",sep='\t',stringsAsFactors=F)
fishCts.1 <- select(fishCts,UID,SAMPLE_TYPE,TAXA_ID,FINAL_NAME,FINAL_CT,IS_DISTINCT,ANOM_CT,NON_NATIVE) %>% 
  filter(!is.na(FINAL_CT) & FINAL_CT!=0) %>%
  merge(sampUIDs.fish,by='UID')

nrow(unique(fishCts.1[,c('UID','SAMPLE_TYPE')]))

fishMet_test <- mutate(fishMet.long,PARAMETER=as.character(PARAMETER),RESULT=as.numeric(RESULT)) 
fishCts_test <- fishCts.1

save(fishMet_test,file="C:/users/kblockso/my documents/temp/aquamet Update Files/aquametAlt/tests/fishMet_test.rda")
save(fishCts_test,file="C:/users/kblockso/my documents/temp/aquamet Update Files/aquametAlt/tests/fishCts_test.rda")

