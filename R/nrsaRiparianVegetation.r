#' @export
#' @title Calculate NRSA Riparian Vegetation Metrics
#' @description This function calculates the riparian vegetation 
#' portion of the physical habitat metrics for National Rivers and 
#' Streams Assessment (NRSA) data.  The function requires a data 
#' frame containing the visible riparian damage data file.
#' @param canopyCoverLargeDiameter A data frame containing cover class 
#' values for large diameter canopy trees at each transect for all
#' reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item BANK character value specifying the channel
#'                 bank (left or right) at the transect
#'                 at which the values were recorded.
#'      \item VALUE numeric or character values
#' }
#' @param canopyCoverSmallDiameter A data frame containing cover 
#' class values for small diameter canopy trees at each transect 
#' for all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item BANK character value specifying the channel
#'                 bank (left or right) at the transect
#'                 at which the values were recorded.
#'      \item VALUE numeric or character values
#' }
#' @param canopyVegetationType A data frame containing canopy 
#' cover type data at each transect for all reaches, with the 
#' following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item BANK character value specifying the channel
#'                 bank (left or right) at the transect
#'                 at which the values were recorded.
#'      \item VALUE numeric or character values
#' }
#' @param groundCoverBare A data frame containing cover class data 
#' for bare ground cover at each transect for all reaches, with the 
#' following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item BANK character value specifying the channel
#'                 bank (left or right) at the transect
#'                 at which the values were recorded.
#'      \item VALUE numeric or character values
#' }
#' @param groundCoverNonwoody A data frame containing cover 
#' class data for nonwoody ground cover at each transect for all 
#' reaches, with the following columns:
#' \itemize{
#'        \item SITE integer or character specifying the site 
#'                   visit
#'        \item TRANSECT character value specifying the transect
#'                       for which the value was recorded.
#'        \item BANK character value specifying the channel
#'                   bank (left or right) at the transect
#'                   at which the values were recorded.
#'        \item VALUE numeric or character values
#' }
#' @param groundCoverWoody A data frame containing cover class 
#' data for woody ground cover at each transect for all reaches, 
#' with the following columns:
#' \itemize{
#'    \item SITE integer or character specifying the site 
#'               visit
#'    \item TRANSECT character value specifying the transect
#'                   for which the value was recorded.
#'    \item BANK character value specifying the channel
#'               bank (left or right) at the transect
#'               at which the values were recorded.
#'    \item VALUE numeric or character values
#' }
#' @param understoryCoverNonwoody A data frame containing cover 
#' class data for nonwoody understory cover at each transect for 
#' all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item BANK character value specifying the channel
#'                 bank (left or right) at the transect
#'                 at which the values were recorded.
#'      \item VALUE numeric or character values
#' }
#' @param understoryCoverWoody A data frame containing cover class 
#' data for woody understory cover at each transect for all reaches,
#' with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item BANK character value specifying the channel
#'                 bank (left or right) at the transect
#'                 at which the values were recorded.
#'      \item VALUE numeric or character values
#' }
#' @param understoryVegetationType A data frame containing understory 
#' cover type data at each transect for all reaches, with the following 
#' columns:
#' \itemize{
#'      \item SITE integer or character specifying the site 
#'                 visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item BANK character value specifying the channel
#'                 bank (left or right) at the transect
#'                 at which the values were recorded.
#'      \item VALUE numeric or character values
#' }
#' @param coverCalculationValues A data frame used to convert between 
#' cover class codes to characteristic cover values. Expected to contain
#' the following columns:
#' \itemize{
#'        \item field character values containing all expected values
#'        \item calc numeric values used to calculate numeric 
#'                   metrics.
#' }
#' Note that possible values for variables in the input data frame are
#' provided in the document named "NRSA Documentation.pdf" included in the help
#' directory for the package.
#' @return Either a data frame when metric calculation is successful 
#' or a character string containing an error message when metric 
#' calculation is not successful.  The data frame contains the following 
#' columns:
#' \itemize{
#'    \item SITE - universal ID value
#'    \item METRIC - metric name
#'    \item VALUE - metric value
#' }
#' Metrics calculated (only for boatable sites) include: pcan_c, pcan_d, 
#' pcan_e, pcan_m, pcan_n ,pmid_c, pmid_d, pmid_e, pmid_m, pmid_n, xcl, 
#' xcs, xmw, xmh, xgw, xgh, xgb, xc, xm, xcmw, xcm, xg, xcmgw, xcmg, 
#' xpcan, xpmid, xpgveg, xpmgw, xpcm, xpmg, xpcmg
#' 
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples 
#' head(visripEx)
#' 
#' canCovLD <- dplyr::filter(visripEx,PARAMETER=='CANBTRE') %>% select(SITE,TRANSECT,BANK,VALUE)
#' canCovSD <- subset(visripEx,PARAMETER=='CANSTRE',select=c('SITE','TRANSECT','BANK','VALUE'))
#' canVegT <- subset(visripEx,PARAMETER=='CANVEG',select=c('SITE','TRANSECT','BANK','VALUE'))
#' gCovB <- subset(visripEx,PARAMETER=='BARE',select=c('SITE','TRANSECT','BANK','VALUE'))
#' gCovNW <- subset(visripEx,PARAMETER=='GCNWDY',select=c('SITE','TRANSECT','BANK','VALUE'))
#' gCovW <- subset(visripEx,PARAMETER=='GCWDY',select=c('SITE','TRANSECT','BANK','VALUE'))
#' undCNW <- subset(visripEx,PARAMETER=='UNDNWDY',select=c('SITE','TRANSECT','BANK','VALUE'))
#' undCW <- subset(visripEx,PARAMETER=='UNDWDY',select=c('SITE','TRANSECT','BANK','VALUE'))
#' undVT <- subset(visripEx,PARAMETER=='UNDERVEG',select=c('SITE','TRANSECT','BANK','VALUE'))
#' 
#' exRipVegOut <- nrsaRiparianVegetation(
#' canopyCoverLargeDiameter = canCovLD,
#' canopyCoverSmallDiameter = canCovSD,
#' canopyVegetationType = canVegT,
#' groundCoverBare = gCovB,
#' groundCoverNonwoody = gCovNW,
#' groundCoverWoody = gCovW,
#' understoryCoverNonwoody = undCNW,
#' understoryCoverWoody = undCW,
#' understoryVegetationType = undVT,
#' coverCalculationValues = data.frame(field=c(NA,'0','1','2','3','4')
#'                                   ,calc=c(NA,0,0.05,0.25,0.575,0.875)
#'                                   ,stringsAsFactors=FALSE))
#' 
#' head(exRipVegOut)                                   
 


nrsaRiparianVegetation <- function(canopyCoverLargeDiameter = NULL
                                  ,canopyCoverSmallDiameter = NULL
                                  ,canopyVegetationType = NULL
                                  ,groundCoverBare = NULL
                                  ,groundCoverNonwoody = NULL
                                  ,groundCoverWoody = NULL
                                  ,understoryCoverNonwoody = NULL
                                  ,understoryCoverWoody = NULL
                                  ,understoryVegetationType = NULL
                                  ,coverCalculationValues = data.frame(field=c(NA,'0','1','2','3','4')
                                                                      ,calc=c(NA,0,0.05,0.25,0.575,0.875)
                                                                      ,stringsAsFactors=FALSE
                                                                      )
                                  ) {

################################################################################
# Function: nrsaRiparianVegetation
# Title: Calculate NRSA Riparian Vegetation Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: February 17, 2010
# Description:
#   This function calculates the riparian vegetation portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires a data frame containing the visible riparian damage data
#   file.
# Function Revisions:
#   02/17/10 cws: Created.
#   03/22/10 cws: Added missing call to checkEquals.
#   03/25/10 cws: Changed diff() calls to dfCompare(), nlaLengthen() to
#            dfLengthen().
#   03/31/10 cws: Added on.exit() call.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   07/31/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify name of the data file.  Added argument NRSAdir to the
#            function to identify the directory from which the data file is read
#            and to which the output metrics file is written.
#   12/21/12 tmk: Modified data input to use a data frame containing the data
#            file rather than a csv file.  Modified output to be a data frame
#            rather than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frame to
#            character variables.
#    3/16/16 cws Documenting arguments in comments at top.
#
# Arguments:
# canopyCoverLargeDiameter  dataframe containing cover class values for large 
#                           diameter canopy trees at each transect for all 
#                           reaches, with the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           BANK        character value specifying the channel
#                                       bank (left or right) at the transect
#                                       at which the values were recorded.
#                           VALUE       numeric or character values
#
# canopyCoverSmallDiameter  dataframe containing cover class values for small 
#                           diameter canopy trees at each transect for all 
#                           reaches, with the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           BANK        character value specifying the channel
#                                       bank (left or right) at the transect
#                                       at which the values were recorded.
#                           VALUE       numeric or character values
#
# canopyVegetationType      dataframe containing canopy cover type data at each 
#                           transect for all reaches, with the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           BANK        character value specifying the channel
#                                       bank (left or right) at the transect
#                                       at which the values were recorded.
#                           VALUE       numeric or character values
#
# groundCoverBare           dataframe containing cover class data for bare ground
#                           cover at each transect for all reaches, with the 
#                           following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           BANK        character value specifying the channel
#                                       bank (left or right) at the transect
#                                       at which the values were recorded.
#                           VALUE       numeric or character values
#
# groundCoverNonwoody       dataframe containing cover class data for nonwoody
#                           ground cover at each transect for all reaches, with 
#                           the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           BANK        character value specifying the channel
#                                       bank (left or right) at the transect
#                                       at which the values were recorded.
#                           VALUE       numeric or character values
#
# groundCoverWoody          dataframe containing cover class data for woody
#                           ground cover at each transect for all reaches, with 
#                           the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           BANK        character value specifying the channel
#                                       bank (left or right) at the transect
#                                       at which the values were recorded.
#                           VALUE       numeric or character values
#
# understoryCoverNonwoody   dataframe containing cover class data for nonwoody
#                           understory cover at each transect for all reaches, 
#                           with the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           BANK        character value specifying the channel
#                                       bank (left or right) at the transect
#                                       at which the values were recorded.
#                           VALUE       numeric or character values
#
# understoryCoverWoody      dataframe containing cover class data for woody
#                           understory cover at each transect for all reaches, 
#                           with the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           BANK        character value specifying the channel
#                                       bank (left or right) at the transect
#                                       at which the values were recorded.
#                           VALUE       numeric or character values
#
# understoryVegetationType  dataframe containing understory cover type data at 
#                           each transect for all reaches, with the following 
#                           columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           BANK        character value specifying the channel
#                                       bank (left or right) at the transect
#                                       at which the values were recorded.
#                           VALUE       numeric or character values
#
# coverCalculationValues =  dataframe used to convert between cover class codes
#                           to characteristic cover values. Expected to contain
#                           the following columns:
#                           field   character values containing all expected 
#                                   values
#                           calc    numeric values used to calculate numeric 
#                                   metrics.
# 
#   Note that possible values for variables in the input data frame are
#   provided in the document named "NRSA Documentation.pdf" included in the help
#   directory for the package.
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The data frame contains the following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     RESULT - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

    intermediateMessage('Starting riparian vegetation metrics', loc='start')
    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdf <- function(df, ...) {
        if(is.null(...)) return(NULL)
        else if(all(is.na(...))) return(NULL)

        args <- list(...)
        pName <- args[[1]]
        rc <- df %>% 
              select(SITE, TRANSECT, BANK, VALUE) %>% 
              mutate(PARAMETER=pName)
        return(rc)
    }

    # Recreate old argument from new ones for now, rip out guts later
    visrip <- rbind(absentAsNULL(canopyCoverLargeDiameter, ifdf, 'CANBTRE')
                   ,absentAsNULL(canopyCoverSmallDiameter, ifdf, 'CANSTRE')
                   ,absentAsNULL(canopyVegetationType, ifdf, 'CANVEG')
                   ,absentAsNULL(groundCoverBare, ifdf, 'BARE')
                   ,absentAsNULL(groundCoverNonwoody, ifdf, 'GCNWDY')
                   ,absentAsNULL(groundCoverWoody, ifdf, 'GCWDY')
                   ,absentAsNULL(understoryCoverNonwoody, ifdf, 'UNDNWDY')
                   ,absentAsNULL(understoryCoverWoody, ifdf, 'UNDWDY')
                   ,absentAsNULL(understoryVegetationType, ifdf, 'UNDERVEG')
                   )
    if(is.null(visrip)) return(NULL)

  # Canopy type fractions
  tt <- subset(visrip, PARAMETER=='CANVEG')
  pcan_c <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='C', na.rm=TRUE) }
                     )
  pcan_c$METRIC<-'pcan_c'
  pcan_d <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='D', na.rm=TRUE) }
                     )
  pcan_d$METRIC<-'pcan_d'
  pcan_e <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='E', na.rm=TRUE) }
                     )
  pcan_e$METRIC<-'pcan_e'
  pcan_m <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='M', na.rm=TRUE) }
                     )
  pcan_m$METRIC<-'pcan_m'
  pcan_n <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='N', na.rm=TRUE) }
                     )
  pcan_n$METRIC<-'pcan_n'

  canopyTypes <- rbind(pcan_c, pcan_d, pcan_e, pcan_m, pcan_n)
  intermediateMessage('.1')
  
  # Mid-layer type fractions
  tt <- subset(visrip, PARAMETER=='UNDERVEG')
  pmid_c <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='C', na.rm=TRUE) }
                     )
  pmid_c$METRIC<-'pmid_c'
  pmid_d <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='D', na.rm=TRUE) }
                     )
  pmid_d$METRIC<-'pmid_d'
  pmid_e <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='E', na.rm=TRUE) }
                     )
  pmid_e$METRIC<-'pmid_e'
  pmid_m <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='M', na.rm=TRUE) }
                     )
  pmid_m$METRIC<-'pmid_m'
  pmid_n <- aggregate(list('VALUE' = tt$VALUE)
                     ,list('SITE' = tt$SITE)
                     ,function(x) { mean(x=='N', na.rm=TRUE) }
                     )
  pmid_n$METRIC<-'pmid_n'

  midlayerTypes <- rbind(pmid_c, pmid_d, pmid_e, pmid_m, pmid_n)
  intermediateMessage('.2')


  # Vegetation class area cover characterizations -- individual classes
  # Use arithmetic means of end points to numerically characterize each cover
  # class.
  cover04 <- coverCalculationValues
            
  tt <- merge(subset(visrip, PARAMETER=='CANBTRE')
            ,cover04
            ,by.x='VALUE', by.y='field'
            ,all.x=TRUE
            )
  xcl <- aggregate(list('VALUE'=tt$calc)
                  ,list('SITE'=tt$SITE)
                  ,mean, na.rm=TRUE
                  )
  xcl$METRIC <- 'xcl'

  tt <- merge(subset(visrip, PARAMETER=='CANSTRE')
            ,cover04
            ,by.x='VALUE', by.y='field'
            ,all.x=TRUE
            )
  xcs <- aggregate(list('VALUE'=tt$calc)
                  ,list('SITE'=tt$SITE)
                  ,mean, na.rm=TRUE
                  )
  xcs$METRIC <- 'xcs'

  tt <- merge(subset(visrip, PARAMETER=='UNDWDY')
            ,cover04
            ,by.x='VALUE', by.y='field'
            ,all.x=TRUE
            )
  xmw <- aggregate(list('VALUE'=tt$calc)
                  ,list('SITE'=tt$SITE)
                  ,mean, na.rm=TRUE
                  )
  xmw$METRIC <- 'xmw'

  tt <- merge(subset(visrip, PARAMETER=='UNDNWDY')
            ,cover04
            ,by.x='VALUE', by.y='field'
            ,all.x=TRUE
            )
  xmh <- aggregate(list('VALUE'=tt$calc)
                  ,list('SITE'=tt$SITE)
                  ,mean, na.rm=TRUE
                  )
  xmh$METRIC <- 'xmh'

  tt <- merge(subset(visrip, PARAMETER=='GCWDY')
            ,cover04
            ,by.x='VALUE', by.y='field'
            ,all.x=TRUE
            )
  xgw <- aggregate(list('VALUE'=tt$calc)
                  ,list('SITE'=tt$SITE)
                  ,mean, na.rm=TRUE
                  )
  xgw$METRIC <- 'xgw'

  tt <- merge(subset(visrip, PARAMETER=='GCNWDY')
            ,cover04
            ,by.x='VALUE', by.y='field'
            ,all.x=TRUE
            )
  xgh <- aggregate(list('VALUE'=tt$calc)
                  ,list('SITE'=tt$SITE)
                  ,mean, na.rm=TRUE
                  )
  xgh$METRIC <- 'xgh'

  tt <- merge(subset(visrip, PARAMETER=='BARE')
            ,cover04
            ,by.x='VALUE', by.y='field'
            ,all.x=TRUE
            )
  xgb <- aggregate(list('VALUE'=tt$calc)
                  ,list('SITE'=tt$SITE)
                  ,mean, na.rm=TRUE
                  )
  xgb$METRIC <- 'xgb'
  
  individualCovers <- rbind(xcl, xcs, xmw, xmh, xgw, xgh, xgb)
  intermediateMessage('.3')

  
  # Vegetation class area cover characterizations -- combinations of classes
  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'))
             ,cover04
             ,by.x='VALUE', by.y='field'
             ,all.x=TRUE
             )

  ss <- aggregate(tt$calc
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xc <- aggregate(list('VALUE'=ss$x)
                 ,list('SITE'=ss$SITE)
                 ,mean, na.rm=TRUE
                 )
  xc$METRIC <- 'xc'
  intermediateMessage('.4')

  tt <- merge(subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
             ,cover04
             ,by.x='VALUE', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xm <- aggregate(list('VALUE'=ss$x)
                 ,list('SITE'=ss$SITE)
                 ,mean, na.rm=TRUE
                 )
  xm$METRIC <- 'xm'
  intermediateMessage('.5')

  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE','UNDWDY'))
             ,cover04
             ,by.x='VALUE', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xcmw <- aggregate(list('VALUE'=ss$x)
                 ,list('SITE'=ss$SITE)
                 ,mean, na.rm=TRUE
                 )
  xcmw$METRIC <- 'xcmw'
  intermediateMessage('.6')

  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE','UNDWDY','UNDNWDY'))
             ,cover04
             ,by.x='VALUE', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xcm <- aggregate(list('VALUE'=ss$x)
                 ,list('SITE'=ss$SITE)
                 ,mean, na.rm=TRUE
                 )
  xcm$METRIC <- 'xcm'
  intermediateMessage('.7')

  tt <- merge(subset(visrip, PARAMETER %in% c('GCWDY','GCNWDY'))
             ,cover04
             ,by.x='VALUE', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xg <- aggregate(list('VALUE'=ss$x)
                 ,list('SITE'=ss$SITE)
                 ,mean, na.rm=TRUE
                 )
  xg$METRIC <- 'xg'
  intermediateMessage('.8')

  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE','UNDWDY','GCWDY'))
             ,cover04
             ,by.x='VALUE', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xcmgw <- aggregate(list('VALUE'=ss$x)
                 ,list('SITE'=ss$SITE)
                 ,mean, na.rm=TRUE
                 )
  xcmgw$METRIC <- 'xcmgw'
  intermediateMessage('.9')


  tt <- merge(subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'
                                             ,'UNDWDY','UNDNWDY'
                                             ,'GCWDY','GCNWDY'
                                             )
                    )
             ,cover04
             ,by.x='VALUE', by.y='field'
             ,all.x=TRUE
             )
  ss <- aggregate(tt$calc
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,function(x) {if(all(is.na(x))) { NA } else {sum(x, na.rm=TRUE) } }
                 )
  xcmg <- aggregate(list('VALUE'=ss$x)
                 ,list('SITE'=ss$SITE)
                 ,mean, na.rm=TRUE
                 )
  xcmg$METRIC <- 'xcmg'
  intermediateMessage('.10')

  combinedCovers <- rbind(xc, xm, xcmw, xcm, xg, xcmgw, xcmg)
  intermediateMessage('.11')

  
  # Vegetation class presence characterizations
  visrip$presence <- ifelse(is.na(visrip$VALUE), NA
                    ,ifelse(visrip$VALUE=='0', FALSE, TRUE
                     ))

  tt <- subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'))
  ss <- aggregate(tt$presence
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  xpcan <- aggregate(list('VALUE'=ss$x)
                    ,list('SITE'=ss$SITE)
                    ,mean, na.rm=TRUE
                    )
  xpcan$METRIC <- 'xpcan'

  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
  ss <- aggregate(tt$presence
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  xpmid <- aggregate(list('VALUE'=ss$x)
                    ,list('SITE'=ss$SITE)
                    ,mean, na.rm=TRUE
                    )
  xpmid$METRIC <- 'xpmid'

  tt <- subset(visrip, PARAMETER %in% c('GCWDY','GCNWDY'))
  ss <- aggregate(tt$presence
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  xpgveg <- aggregate(list('VALUE'=ss$x)
                    ,list('SITE'=ss$SITE)
                    ,mean, na.rm=TRUE
                    )
  xpgveg$METRIC <- 'xpgveg'

  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','GCWDY'))
  ss <- aggregate(tt$presence
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,function(x) {if(all(is.na(x))) { NA } else {all(x, na.rm=TRUE) } }
                 )
  xpmgw <- aggregate(list('VALUE'=ss$x)
                    ,list('SITE'=ss$SITE)
                    ,mean, na.rm=TRUE
                    )
  xpmgw$METRIC <- 'xpmgw'

  tt <- subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'))
  cc <- aggregate(list('can'=tt$presence)
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
  uu <- aggregate(list('und'=tt$presence)
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  ss <- merge(cc, uu, by=c('SITE','TRANSECT','BANK'), all=TRUE)
  ss$both <- ss$can & ss$und
  xpcm <- aggregate(list('VALUE'=ss$both)
                    ,list('SITE'=ss$SITE)
                    ,mean, na.rm=TRUE
                    )
  xpcm$METRIC <- 'xpcm'

  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
  uu <- aggregate(list('und'=tt$presence)
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  tt <- subset(visrip, PARAMETER %in% c('GCWDY','GCNWDY'))
  gg <- aggregate(list('gnd'=tt$presence)
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  ss <- merge(uu, gg, by=c('SITE','TRANSECT','BANK'), all=TRUE)
  ss$both <- ss$und & ss$gnd
  xpmg <- aggregate(list('VALUE'=ss$both)
                    ,list('SITE'=ss$SITE)
                    ,mean, na.rm=TRUE
                    )
  xpmg$METRIC <- 'xpmg'

  tt <- subset(visrip, PARAMETER %in% c('CANBTRE','CANSTRE'))
  cc <- aggregate(list('can'=tt$presence)
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  tt <- subset(visrip, PARAMETER %in% c('UNDWDY','UNDNWDY'))
  uu <- aggregate(list('und'=tt$presence)
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  tt <- subset(visrip, PARAMETER %in% c('GCWDY','GCNWDY'))
  gg <- aggregate(list('gnd'=tt$presence)
                 ,list('SITE'=tt$SITE, 'TRANSECT'=tt$TRANSECT, 'BANK'=tt$BANK)
                 ,any
                 )
  ss <- merge(cc, uu, by=c('SITE','TRANSECT','BANK'), all=TRUE)
  ss <- merge(ss, gg, by=c('SITE','TRANSECT','BANK'), all=TRUE)
  ss$all3 <- ss$can & ss$und & ss$gnd
  xpcmg <- aggregate(list('VALUE'=ss$all3)
                    ,list('SITE'=ss$SITE)
                    ,mean, na.rm=TRUE
                    )
  xpcmg$METRIC <- 'xpcmg'

  presences <- rbind(xpcan, xpmid, xpgveg, xpmgw, xpcm, xpmg, xpcmg)
  intermediateMessage('.5')

                           
  # Combine groups of metrics and convert NaN calculations to NA
  mets <- rbind(canopyTypes, midlayerTypes, individualCovers, combinedCovers
               ,presences
               )
  mets$VALUE <- as.character(ifelse(is.nan(as.numeric(mets$VALUE)), NA, mets$VALUE))
  intermediateMessage('.  Done.', loc='end')

  return(mets)
}



# end of file
