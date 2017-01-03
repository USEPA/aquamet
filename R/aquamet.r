#' aquamet: Calculate metrics used in NRSA and NLA
#' 
#' @docType package
#' @name aquamet 
#' 
#' @importFrom Hmisc "%nin%" capitalize
#' @importFrom gtools smartbind
#' @importFrom plyr ddply mutate summarize summarise rename
#' @importFrom dplyr filter select "%>%" group_by
#' @importFrom reshape2 dcast melt
#' @importFrom stringr str_detect 
#' @import foreach
#' @import RUnit
#' @keywords package
#' @title aquamet



if(getRversion() >= "3.0") utils::globalVariables(c('DOM1PIND','DOM3PIND','DOM5PIND','FAMILY','CHIRDOM1PIND'
                  ,'CHIRDOM3PIND','CHIRDOM5PIND'
                  ,'FFG','ORDER','HABIT','CLASS','PHYLUM','SUBFAMILY','TRIBE','ORTHCHIRPIND','PTV'
                  ,'TRAIT','ANOMPIND','Freq','int','slope','WSAREA','RESULT_WS','NONNATIVE','ALIEN'
                  ,'VELOCITY','MIGRATORY','REPROD','TEMP','NAME','GENUS'
                  ,'INTL','TOLERANCE','NTOL','HABITAT','TROPHIC','TOL_VAL','CLASS'
                  ,'ECO9','MMI_BENT','gf','fp','HABITAT','NAME','NAT','VALUE','SITE'
                  ,'DIRECTION','isFast','isSlow','isPool','characteristicCover','field'
                  ,'standardizedPresent','loc','len','first.SITE','BANK','pct','VALUE.sa'
                  ,'VALUE.fn','ONBANK','TARGET_TAXON','TOTAL','isfast','isslow','ispool'
                  ,'coverType'))
## NULL