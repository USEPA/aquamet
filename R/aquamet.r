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



if(getRversion() >= "3.0") utils::globalVariables(c('ECO9','gf','fp','HABITAT','NAME','NAT','VALUE','SITE'
                  ,'DIRECTION','isFast','isSlow','isPool','characteristicCover','field'
                  ,'standardizedPresent','loc','len','first.SITE','BANK','pct','VALUE.sa'
                  ,'VALUE.fn','ONBANK','isfast','isslow','ispool'
                  ,'coverType'))
## NULL