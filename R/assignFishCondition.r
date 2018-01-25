#' @export
#' 
#' @title Assign fish assemblage condition
#' 
#' @description Using ecoregion, fish MMI score, 
#' log10(watershed area), and total number of individuals 
#' to assign condition using NRSA thresholds.
#' 
#' @param inMMI Input data frame which must contain the 
#' variables named in the remaining arguments.
#' 
#' @param sampID A character vector containing the names of all 
#' variables in inMets that specify a unique sample. If not specified, 
#' the default is \emph{UID}
#' 
#' @param ecoreg A string with the name of the ecoregion variable. 
#' Valid values that correspond to regions used in NRSA are
#' CPL, NAP, NPL, SAP, sPL, TPL, UMW, WMT, and XER.
#' 
#' @param mmi A string with the name of the MMI input variable. The
#' default is 'MMI_FISH' because this is the name of the output 
#' from the \code{calcFishMMI()} function. 
#' 
#' @param wsarea A string with the name of the watershed area variable.
#' Watershed area is assumed to be in square km. This variable is required 
#' in sites where fish were sampled but none were collected, along with
#' \emph{totlnind}. For these sites, \emph{mmi} should be missing and
#' \emph{totlnind} should be set to 0. At sites with a \emph{wsarea} <=2 
#' km2, fish are not expected and thus, condition is set as Not Assessed. 
#' 
#' @param totlnind A string with the name of the variable in 
#' \emph{inMMI} with the number of individuals in the fish sample. 
#' This variable is necessary to assign condition in sites where 
#' fish were sampled but none were collected. 
#' In such cases \emph{totlnind} should be 0 and \emph{mmi} missing.
#' 
#' @details If site is not sampled for fish, \emph{mmi} and \emph{totlnind} 
#' should be missing values in the \emph{inMMI} data frame.
#' 
#' @return A data frame containing all of the input variables and the 
#' variable FISH_MMI_COND for each site.  
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#' @keywords survey
#' 
#' @examples{
#'  fishMMIex <- data.frame(SITE_ID = c('Site1','Site2','Site3','Site4',
#'                                    'Site5','Site1','Site6','Site7'),
#'  VISIT_NO = c(1,1,1,1,1,2,1,1), ECO9 = c('CPL','XER','NAP','WMT',
#'                                          'TPL','CPL','SAP','SAP'),
#'  MMI_FISH = c(57.3, 79.2, 63.2, 73.2, NA, 53.1, NA, NA), 
#'  WSAREAKM = c(10, 56, 67, 54, 1.3, 10, 3.4, 2.6), 
#'  TOTLNIND = c(346, 230, 507, 304, 0, 209, 0, NA), stringsAsFactors=FALSE) 
#'  
#'  fishCond <- assignFishCondition(fishMMIex, sampID = c('SITE_ID','VISIT_NO'),
#'  ecoreg = 'ECO9', mmi = 'MMI_FISH', wsarea = 'WSAREAKM',
#'  totlnind = 'TOTLNIND')
#'  
#'  fishCond
#' }
#' 
assignFishCondition <- function(inMMI, sampID='UID', ecoreg='ECOREG', mmi='MMI_FISH', 
                                wsarea='WSAREA', totlnind='TOTLNIND'){

  inMMI.1 <- inMMI 
  
  # Separate necessary traits by whether or not there are missing mmi values
  # If any missing values, we need totlnind and wsarea as well
  if(any(is.na(inMMI.1[,mmi]))){
    necTraits <- c(sampID,ecoreg,wsarea,totlnind,mmi)
    
  }else{
    necTraits <- c(sampID,ecoreg,mmi) 
  }
  
  if(any(necTraits %nin% names(inMMI.1))){
    msgTraits <- which(necTraits %nin% names(inMMI.1))
    print(paste("Some of the traits are missing from the input data frame. The following are required for metric calculations to run:"
                , necTraits[msgTraits]))
    return(NULL)
  }
  
  # Rename variables 
  if(any(is.na(inMMI.1[,mmi]))){

    names(inMMI.1)[names(inMMI.1)==wsarea] <- 'WSAREA'
    names(inMMI.1)[names(inMMI.1)==totlnind] <- 'TOTLNIND'
    
    inMMI.1 <- plyr::mutate(inMMI.1, WSAREA = as.numeric(WSAREA), 
                          TOTLNIND = as.numeric(TOTLNIND))
  }
  
  names(inMMI.1)[names(inMMI.1)==ecoreg] <- 'ECO9'
  names(inMMI.1)[names(inMMI.1)==mmi] <- 'MMI_FISH'
  inMMI.1 <- mutate(inMMI.1, MMI_FISH = as.numeric(MMI_FISH))
  
  # Check to make sure ecoregion variable is included in the input data frame
  ecoCk <- unique(inMMI.1$ECO9)
  ecos <- c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER')
  if(any(ecoCk %nin% ecos)){
    msgEco <- which(ecoCk %nin% ecos)
    print(paste("These ecoregions are not valid: "
                ,paste(ecoCk[msgEco],collapse=',')))
    return(NULL)
  }
  
  ## Now we need to set condition class for each sample, which is based on ECO9
  # First create a table of thresholds by ECO9
  # cond.tholds <- data.frame(ECO9=c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER')
  #                           ,gf=c(57.1,57.5,45.4,59.3,50.0,57.9,37.5,75.7,76.6)
  #                           ,fp=c(46.4,46.8,34.6,48.6,39.3,47.1,26.8,65.0,65.9),stringsAsFactors=F)
  cond.tholds <- data.frame(ECO9=c('CPL','NAP','NPL','SAP','SPL','TPL','UMW','WMT','XER')
                            ,gf=c(57.3,57.6,46.3,60.3,50.2,58.0,39.8,75.9,76.8)
                            ,fp=c(46.8,47.1,35.8,49.8,39.7,47.5,29.3,65.4,66.2),stringsAsFactors=FALSE)
  
  # Need to account for cases where no missing MMI_FISH
  cond.mmi.1 <- merge(inMMI.1,cond.tholds,by='ECO9',all.x=TRUE) %>%
    plyr::mutate(FISH_MMI_COND = ifelse(!is.na(MMI_FISH) & MMI_FISH >= gf, 'Good' 
                          , ifelse(MMI_FISH < fp, 'Poor'
                          , ifelse(MMI_FISH < gf & MMI_FISH >= fp,'Fair', NA)))) %>%
    plyr::mutate(FISH_MMI_COND=ifelse(!is.na(FISH_MMI_COND), FISH_MMI_COND
                         , ifelse(is.na(MMI_FISH) & is.na(TOTLNIND), 'Not Assessed'
                         , ifelse(is.na(MMI_FISH) & TOTLNIND==0 & WSAREA > 2, 'Poor'
                                  , 'Not Assessed')))) %>%
    dplyr::select(-gf, -fp)
  
  condOut <- subset(cond.mmi.1, select = c(sampID, 'FISH_MMI_COND')) %>%
    merge(inMMI, by = c(sampID)) 
  
  return(condOut)
  
}