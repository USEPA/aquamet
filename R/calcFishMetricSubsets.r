#' @export
#' @title Calculate all tolerance-related metrics
#' @description This function calculates all of the tolerance metrics,
#' and if additional trait values are included in the taxalist (habitat,
#' trophic, migr).
#' @param indata Input data frame containing variables as identified  
#' in the arguments for \emph{sampID}, \emph{dist}, \emph{ct}, 
#' \emph{taxa_id}. If a variable for anomaly counts, as specified in 
#' the argument \emph{anom}, is included, additional metrics are 
#' calculated.
#' @param inTaxa Data frame containing fish taxalist, along with autecology
#' traits. At a minimum, this taxalist must contain variables matching 
#' the argument for \emph{tol}. If additional traits are included, as 
#' specified by the arguments \emph{habitat}, \emph{trophic}, \emph{migr},
#' additional metrics that combine tolerance and other traits are also 
#' calculated. The variable specified in the argument \emph{taxa_id} 
#' must match that in \emph{indata}. 
#' @param sampID sampID A character vector containing the names of all 
#' variables in indf that specify a unique sample. If not specified, 
#' the default is \emph{UID}.
#' @param dist A string with the name of the distinctness variable, 
#' which is assumed to have only values of 0 or 1. If not specified, 
#' the default is \emph{IS_DISTINCT}.
#' @param ct A string with the name of the count variable. If not 
#' specified, the default is \emph{TOTAL}.
#' @param taxa_id A string with the name of the taxon ID variable 
#' in \emph{indata} that matches that in \emph{inTaxa}. The default 
#' value is \emph{TAXA_ID}.
#' @param tol A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing tolerance categories. Valid
#' values include S (sensitive), I (intermediate), and T (tolerant).
#' The default value is \emph{TOLERANCE}.
#' @param tolval A string with the name of the variable in the 
#' \emph{inTaxa} taxalist containing numeric tolerance values.
#' Valid values range from 0 to 10. The default value is TOL_VAL.
#' @param habitat A string with the name of the variable in 
#' \emph{inTaxa} containing habitat preference values. Valid
#' values include B (benthic), W (water column), E (???), or
#' blank, if unknown. The default value is \emph{HABITAT}.
#' @param trophic A string with the name of the variable in
#' \emph{inTaxa} containing trophic category values. Valid
#' values include I (invertivore), C (carnivore), O (omnivore),
#' H (herbivore), or blank if unknown. The default value is
#' \emph{inTaxa} is TROPHIC.
#' @param migr A string with the name of the variable in 
#' \emph{inTaxa} containing migratory status value. Valid 
#' values include N (No), Y (Yes), and blank if unknown. The
#' default value is \emph{MIGRATORY}.
#' @return A data frame containing the variables in sampID and 
#' the fish tolerance metrics as additional variables. The names of
#' metrics include INTLNIND, INTLNTAX, INTLPIND, INTLPTAX, MTOLNIND, 
#' MTOLNTAX, MTOLPIND, MTOLPTAX, NTOLNIND, NTOLNTAX, NTOLPIND, 
#' NTOLPTAX, TOLRNIND, TOLRNTAX, TOLRPIND, TOLRPTAX, WTD_TV, 
#' NAT_INTLNIND, NAT_INTLNTAX, NAT_INTLPIND, NAT_INTLPTAX, NAT_MTOLNIND, 
#' NAT_MTOLNTAX, NAT_MTOLPIND, NAT_MTOLPTAX, NAT_NTOLNIND, NAT_NTOLNTAX, 
#' NAT_NTOLPIND, NAT_NTOLPTAX,  NAT_TOLRNIND, NAT_TOLRNTAX, NAT_TOLRPIND, 
#' NAT_TOLRPTAX, NAT_WTD_TV. 
#  
#' Additional metrics calculated if the appropriate additional traits
#' are included in \emph{inTaxa}: INTLINVNIND, INTLINVNTAX, INTLINVPIND, 
#' INTLINVPTAX, INTLLOTNIND, INTLLOTNTAX, INTLLOTPIND, INTLLOTPTAX,
#' INTLMIGRNIND, INTLMIGRNTAX, INTLMIGRPIND, INTLMIGRPTAX,  INTLRHEONIND, 
#' INTLRHEONTAX, INTLRHEOPIND, INTLRHEOPTAX, NTOLBENTNIND, NTOLBENTNTAX, 
#' NTOLBENTPIND, NTOLBENTPTAX, NTOLCARNNIND, NTOLCARNNTAX, 
#' NTOLCARNPIND, NTOLCARNPTAX, NTOLINVNIND, NTOLINVNTAX, NTOLINVPIND, 
#' NTOLINVPTAX, NAT_INTLINVNIND, NAT_INTLINVNTAX, NAT_INTLINVPIND, 
#' NAT_INTLINVPTAX, NAT_INTLLOTNIND, NAT_INTLLOTNTAX, NAT_INTLLOTPIND, 
#' NAT_INTLLOTPTAX, NAT_INTLMIGRNIND, NAT_INTLMIGRNTAX, NAT_INTLMIGRPIND, 
#' NAT_INTLMIGRPTAX, NAT_INTLRHEONIND, NAT_INTLRHEONTAX, NAT_INTLRHEOPIND, 
#' NAT_INTLRHEOPTAX, NAT_NTOLBENTNIND, NAT_NTOLBENTNTAX, NAT_NTOLBENTPIND, 
#' NAT_NTOLBENTPTAX, NAT_NTOLCARNNIND, NAT_NTOLCARNNTAX, NAT_NTOLCARNPIND, 
#' NAT_NTOLCARNPTAX, NAT_NTOLINVNIND, NAT_NTOLINVNTAX, NAT_NTOLINVPIND, 
#' NAT_NTOLINVPTAX.
#' 
#' @author Karen Blocksom \email{Blocksom.Karen@epa.gov}
#'  
calcFishTolMets <- function(indata,inTaxa=NULL,sampID='UID',dist='IS_DISTINCT'
                            ,ct='TOTAL',taxa_id='TAXA_ID',tol='TOLERANCE'
                            ,tolval='TOL_VAL',habitat='HABITAT',trophic='TROPHIC'
                            ,migr='MIGRATORY'){
  
  ctVars <- c(sampID,dist,ct,taxa_id)
  if(any(ctVars %nin% names(inCts))){
    msgTraits <- which(ctVars %nin% names(inCts))
    print(paste("Missing variables in input data frame:",paste(ctVars[msgTraits],collapse=',')))
    return(NULL)
  }
    
  # Combine all values in sampID into one sampID in df
  for(i in 1:length(sampID)){
    if(i==1) indata$SAMPID <- indata[,sampID[i]]
    else indata$SAMPID <- paste(indata$SAMPID,indata[,sampID[i]],sep='.')
  }
  
  # If inTaxa is not specified, the default is the included fishTaxa dataset
  if(is.null(inTaxa)) {
    inTaxa <- fishTaxa
  }
  
  # Taxonomy and traits checks
  necTraits <- c(tol,tolval)
  if(any(necTraits %nin% names(inTaxa))){
    msgTraits <- which(necTraits %nin% names(inTaxa))
    return(paste("Some of the traits are missing from the taxa list. The following are \nrequired for metric calculations to run:\n", necTraits[msgTraits], "\n"))
  }
  optTraits <- c(habitat,trophic,migr)
  if(any(optTraits %nin% names(inTaxa))){
    msgTraits <- which(optTraits %nin% names(inTaxa))
    return(paste("Optional traits are missing from the taxa list. Any tolerance metrics also using these traits will not be calculated:\n",
                 optTraits[msgTraits],"\n"))
  }
  
  inTaxa <- subset(inTaxa,select=names(inTaxa) %in% c(taxa_id,tol,tolval,habitat,trophic,migr))  
  
  # Rename counts and distinct variables to FINAL_CT and IS_DISTINCT
  names(indata)[names(indata)==ct] <- 'FINAL_CT'
  names(indata)[names(indata)==dist] <- 'IS_DISTINCT'
  names(indata)[names(indata)==taxa_id] <- 'TAXA_ID'
  names(inTaxa)[names(inTaxa)==taxa_id] <- 'TAXA_ID'
  
  names(inTaxa)[names(inTaxa)==tol] <- 'TOLERANCE'
  names(inTaxa)[names(inTaxa)==tolval] <- 'TOL_VAL'
  
  if(habitat %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==habitat] <- 'HABITAT'
  }
  
  if(trophic %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==trophic] <- 'TROPHIC'
  }
  
  if(migr %in% names(inTaxa)){
    names(inTaxa)[names(inTaxa)==migr] <- 'MIGRATORY'
  }
  
  # Make sure all necessary columns in inCts are numeric
  inCts <- mutate(indata,FINAL_CT=as.numeric(FINAL_CT),IS_DISTINCT=as.integer(IS_DISTINCT))
  
}