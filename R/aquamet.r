#' aquamet: Calculate metrics used in NRSA and NLA
#' 
#' @docType package
#' @name aquamet 
#' 
#' @importFrom Hmisc "%nin%" capitalize
#' @importFrom gtools smartbind
#' @importFrom plyr ddply mutate summarize summarise rename
#' @importFrom dplyr filter select "%>%" group_by arrange across all_of anti_join
#' @importFrom reshape2 dcast melt
#' @importFrom stringr str_detect 
#' @importFrom stats aggregate ave coef median na.omit quantile reshape sd weights
#' @importFrom utils read.table write.csv
#' @importFrom tidyr pivot_longer pivot_wider
#' @import foreach
#' @import RUnit
#' @keywords package
#' @title aquamet



if(getRversion() >= "4.0") utils::globalVariables(c('ECO9','gf','fp','HABITAT','NAME','NAT','VALUE','SITE',
                  'DIRECTION','isFast','isSlow','isPool','characteristicCover','field',
                  'standardizedPresent','loc','len','first.SITE','BANK','pct','VALUE.sa',
                  'VALUE.fn','ONBANK','isfast','isslow','ispool',
                  'coverType','CLASS','DDcond_screen','DRAWDOWN_COND','LINE','LOC',
                  'LitCvrQ','LitCvrQc3OE','LitCvrQc3x15','LitRipCvrQ','LitRipCvrQc3OE',
                  'LitRipCvrQc3x15','METHOD','METRIC','PARAMETER','PROTOCOL','RDis_IX',
                  'RVegQ','RVegQc3OE','RVegQc3x15','RfE_LRBS',
                  'RfE_ripveg','RfE_xfc','SAMPLE_TYPE','SLOPE','SPECIES','STATION',
                  'TRANSECT','TRANSPC','UNITS','UNITS.SLOPE','VALUE.BEARING',
                  'VALUE.PROPORTION','VALUE.SLOPE','VALUE.x','amfcFltEmg','atIsland',
                  'bsobang','calcVal','characteristicDiameter','compVal','diam',
                  'diameterClass','error','fcfc','fcfp','first.group','firstRow','group',
                  'horizDD','horizDD_cond','horizDD_cond17','inPopulationEstimate',
                  'inSideChan','inStream','inputMsg','intercept','isBig','isNatural',
                  'keep','lDiam','l_xcmgw','l_xfc_nat','lastStation','lengthClass',
                  'lmax','lmin','locationClass','lowerPct','maxSizen','medianSize',
                  'nSta','n_ba','name','nhorizDD_nomod','normCover','nvertDD','oe',
                  'p75H','p75V','p95H','p95V','poolID','resp','respAdj','sddepth',
                  'sizen','ssiNatBedBld','startLOC','sumVal','taxCat','uid',
                  'upperPct','value','vertDD','vertDD_cond','vertDD_cond17',
                  'width','x','xcb_hnag','xdepth', 'FC', 'V','N','met','variable',
                  'inflType', 'inflLocation', 'coverLocation', 'prop_dd', 'prop_rip',
                  'basicSyntheticInfl', 'second', 'inFirst', 'inSecond', 'type',
                  'classFirst', 'classSecond', 'column', 'type2',
                  'compareAsNumbers', 'sameAbs', 'sameci', 'samesi', 'samecsi',
                  'cover'))
## NULL
