#' @export
#' @title Calculate NRSA Fish Cover Metrics
#' @description This function calculates the fish cover 
#' portion of the physical habitat metrics for National 
#' Rivers and Streams Assessment (NRSA) data.  The function 
#' requires data frames containing the channel cover and stream
#' verification form data files.
#' @param algae A data frame containing algae cover class data at each transect for
#'             all reaches, with the following columns:
#'  \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                           for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param boulder A data frame containing boulder cover class data at each 
#' transect for all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param brush A data frame containing brush cover class data at each 
#' transect for all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param liveTree A data frame containing livetree cover class data at each 
#' transect for all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param macrophytes A data frame containing plant cover class data at each 
#' transect for all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                    for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param overhang A data frame containing overhang cover class data at each 
#' transect for all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param structures A data frame containing structural cover class data at each 
#' transect for all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param undercut A data frame containing undercut cover class data at each 
#' transect for all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' @param woodyDebris A data frame containing woody debris cover class data at 
#' each transect for all reaches, with the following columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                           for which the value was recorded.
#'      \item VALUE numeric or character values
#' }
#' 
#' @param classInformation A data frame containing group membership information 
#' for each type of fish cover.  The default value for this argument 
#' reproduces EPA NARS calculations.  Expected to have the following columns:
#' \itemize{
#'        \item coverType character values 'algae', 'boulder', 'brush',
#'                        'liveTree', 'macrophytes', 'overhang',
#'                        'structures', 'undercut', 'woodyDebris'
#'        \item isBig logical values specifying whether the class
#'                    is considered as large for analysis
#'        \item isNatural logical values specifying whether the class
#'                        is considered as natural for analysis
#'  }
#' @param dataInformation A data frame specifying how cover class values 
#' are mapped to presence/absence and to characteristic cover fractions for 
#' analysis.  The default value for this argument reproduces EPA NARS 
#' calculations. Expected to have the following columns:
#' \itemize{
#'        \item field character value specifying the codes
#'              used to record cover values
#'        \item presence logical value specifying whether the
#'              cover value is present (TRUE) or absent 
#'              (FALSE) or missing (NA), used for mean
#'              presence calculations.
#'        \item characteristicCover numeric value specifying the
#'              value used for mean cover
#'              calculations.
#' }
#' @param isUnitTest Logical argument to determine whether errors should be ignored.
#' Should only be used for running a unit test. Default value is FALSE.
#' @param argSavePath character string specifying the path to which data 
#' arguments are saved as csv files, or NULL if those files are not to be 
#' written.
#' @return Either a data frame when metric calculation is successful or a 
#' character string containing an error message when metric calculation is 
#' not successful.  The data frame contains the following columns:
#' \itemize{
#'  \item SITE - universal ID value
#'  \item METRIC - metric name
#'  \item VALUE - metric value
#' }
#' 
#' Metrics calculated include: pfc_alg, pfc_rck, pfc_brs, pfc_lvt, pfc_aqm, 
#' pfc_ohv, pfc_hum, pfc_ucb, pfc_lwd, xfc_alg, xfc_rck, xfc_brs, xfc_lvt, 
#' xfc_aqm, xfc_ohv, xfc_hum, xfc_ucb, xfc_lwd, pfc_all, pfc_big, pfc_nat, 
#' xfc_all, xfc_big, xfc_nat, sdfc_ucb, sdfc_ohv, idrucb, idrohv, iqrucb, 
#' iqrohv
#' 
#' Descriptions for all NRSA metrics can be found at:
#' \href{https://github.com/USEPA/aquamet/blob/master/inst/NRSA_physical_habitat_metrics_descriptions.pdf}{NLA_Physical_Habitat_Metric_Descriptions.pdf}.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' 
#' @examples
#' head(fishcoverEx)
#' 
#' # Subset example dataset within function to create inputs, keeping only SITE,
#'   #  TRANSECT, and VALUE.
#' fishCvrOut <- nrsaFishCover(
#' algae=subset(fishcoverEx,PARAMETER=='ALGAE',
#'                  select=c(SITE,TRANSECT,VALUE)),
#' boulder=subset(fishcoverEx,PARAMETER=='BOULDR',select=c(SITE,TRANSECT,VALUE)),
#' brush=subset(fishcoverEx,PARAMETER=='BRUSH',select=c(SITE,TRANSECT,VALUE)),
#' liveTree=subset(fishcoverEx,PARAMETER=='LVTREE',select=c(SITE,TRANSECT,VALUE)),
#' macrophytes=subset(fishcoverEx,PARAMETER=='MACPHY',select=c(SITE,TRANSECT,VALUE)),
#' overhang=subset(fishcoverEx,PARAMETER=='OVRHNG',select=c(SITE,TRANSECT,VALUE)),
#' structures=subset(fishcoverEx,PARAMETER=='STRUCT',select=c(SITE,TRANSECT,VALUE)),
#' undercut=subset(fishcoverEx,PARAMETER=='UNDCUT',select=c(SITE,TRANSECT,VALUE)),
#' woodyDebris=subset(fishcoverEx,PARAMETER=='WOODY',select=c(SITE,TRANSECT,VALUE))
#' )
#' 
#' head(fishCvrOut)

nrsaFishCover <- function(algae=NULL, boulder=NULL, brush=NULL
                         ,liveTree=NULL, macrophytes=NULL, overhang=NULL
                         ,structures=NULL, undercut=NULL, woodyDebris=NULL
                         ,classInformation=data.frame(name=c('algae', 'boulder', 'brush'
                                                            ,'liveTree', 'macrophytes', 'overhang'
                                                            ,'structures', 'undercut', 'woodyDebris'
                                                            )
                                                     ,isBig=c(FALSE, TRUE, FALSE
                                                             ,FALSE, FALSE, FALSE
                                                             ,TRUE, TRUE,TRUE
                                                             )
                                                     ,isNatural=c(FALSE, TRUE, TRUE
                                                                 ,TRUE, FALSE, TRUE
                                                                 ,FALSE ,TRUE, TRUE
                                                                 )
                                                     ,stringsAsFactors=FALSE
                                                     )
                         ,dataInformation=data.frame(value=c(NA,'','0','1','2','3','4')
                                                    ,presence=c(NA,NA,0L,1L,1L,1L,1L) %>% as.logical()
                                                    ,weights=c(NA,NA,0,0.05,0.25,0.575,0.875)
                                                    ,stringsAsFactors=FALSE
                                                    )
                         ,isUnitTest=FALSE
                         ,argSavePath = NULL
                         ) {
################################################################################
# Function: metsFishCover
# Title: Calculate NRSA Fish Cover Metrics
# Programmers: Marlys Cappert
#              Curt Seeliger
#              Suzanne San Romani
#              Tom Kincaid
# Date: December 15, 2009
# Description:
#   This function calculates the fish cover portion of the physical habitat
#   metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires data frames containing the channel cover and stream
#   verification form data files.
# Metrics:
#   pfc_alg, pfc_rck, pfc_brs, pfc_lvt, pfc_aqm, pfc_ohv, pfc_hum, pfc_ucb,
#   pfc_lwd, xfc_alg, xfc_rck, xfc_brs, xfc_lvt, xfc_aqm, xfc_ohv, xfc_hum,
#   xfc_ucb, xfc_lwd, pfc_all, pfc_big, pfc_nat, xfc_all, xfc_big, xfc_nat,
#   sdfc_ucb, sdfc_ohv, idrucb, idrohv, iqrucb, iqrohv
# Function Revisions:
#   12/15/09 mrc: Copied fcMets from NLA project.
#   12/18/09 mrc: Completed creating mets.
#   12/30/09 mrc: Completed tests.
#   03/23/10 cws: Moved creation of unit test dataframes to separate functions.
#   04/01/10 ssr: Created only-boatable and only-wadeable tests. 
#   09/16/10 cws: Removing hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   07/31/12 tmk: Removed use of ODBC data connection and replaced with data
#            input from csv files using a call to function read.csv.  Added
#            argument tbl to the function to identify names of the data files.
#            Added argument NRSAdir to the function to identify the directory
#            from which data files are read and to which the output metrics file
#            is written.
#   12/14/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Modified output to be a data frame rather
#            than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   12/07/15 cws Modified calling interface. Still need to refactor interior.
#            to handle NULL argument values.
#    2/25/16 cws Documenting arguments in comments at top.
#    3/17/16 cws Deleting old commented out code.
#    3/12/19 cws Changed to use aquametStandardizeArgument() instead of 
#            absentAsNull().
#    3/20/19 cws Added structure checks for arguments coverClassTypes and 
#            coverCalculationValues.  Using the values of coverCalculationValues$field 
#            for legal checks of VALUE in data arguments.
#    3/22/19 cws Modified to use dplyr::rename()
#    3/28/19 cws Standardized metadata argument naming
#   10/13/20 cws Modified to allow '' values in input.
#    8/07/23 cws Added argSavePath argument, as newly needed for 
#            aquametStandardizeArgument
#
# ARGUMENTS:
# algae       dataframe containing algae cover class data at each transect for
#             all reaches, with the following columns:
#               SITE        integer or character specifying the site visit
#               TRANSECT    character value specifying the transect
#                           for which the value was recorded.
#               VALUE       numeric or character values
#
# boulder     dataframe containing boulder cover class data at each transect for
#             all reaches, with the following columns:
#               SITE        integer or character specifying the site visit
#               TRANSECT    character value specifying the transect
#                           for which the value was recorded.
#               VALUE       numeric or character values
#
# brush       dataframe containing brush cover class data at each transect for
#             all reaches, with the following columns:
#               SITE        integer or character specifying the site visit
#               TRANSECT    character value specifying the transect
#                           for which the value was recorded.
#               VALUE       numeric or character values
#
# liveTree    dataframe containing livetree cover class data at each transect for
#             all reaches, with the following columns:
#               SITE        integer or character specifying the site visit
#               TRANSECT    character value specifying the transect
#                           for which the value was recorded.
#               VALUE       numeric or character values
#
# macrophytes dataframe containing plant cover class data at each transect for
#             all reaches, with the following columns:
#               SITE        integer or character specifying the site visit
#               TRANSECT    character value specifying the transect
#                           for which the value was recorded.
#               VALUE       numeric or character values
#
# overhang    dataframe containing overhang cover class data at each transect for
#             all reaches, with the following columns:
#               SITE        integer or character specifying the site visit
#               TRANSECT    character value specifying the transect
#                           for which the value was recorded.
#               VALUE       numeric or character values
#
# structures  dataframe containing structural cover class data at each transect 
#             for all reaches, with the following columns:
#               SITE        integer or character specifying the site visit
#               TRANSECT    character value specifying the transect
#                           for which the value was recorded.
#               VALUE       numeric or character values
#
# undercut    dataframe containing undercut cover class data at each transect 
#             for all reaches, with the following columns:
#               SITE        integer or character specifying the site visit
#               TRANSECT    character value specifying the transect
#                           for which the value was recorded.
#               VALUE       numeric or character values
#
# woodyDebris dataframe containing woody debris cover class data at each transect
#             for all reaches, with the following columns:
#               SITE        integer or character specifying the site visit
#               TRANSECT    character value specifying the transect
#                           for which the value was recorded.
#               VALUE       numeric or character values
#
# coverClassTypes   dataframe containing group membership information for each
#                   type of fish cover.  The default value for this argument
#                   reproduces EPA NARS calculations.  Expected to have the 
#                   following columns:
#                       coverType   character values 'algae', 'boulder', 'brush',
#                                   'liveTree', 'macrophytes', 'overhang',
#                                   'structures', 'undercut', 'woodyDebris'
#                       isBig       logical values specifying whether the class
#                                   is considered as large for analysis
#                       isNatural   logical values specifying whether the class
#                                   is considered as natural for analysis
#
# coverCalculationValues    dataframe specifying how cover class values are
#                           mapped to presence/absence and to characteristic 
#                           cover fractions for analysis.  The default value for
#                           this argument reproduces EPA NARS calculations.
#                           Expected to have the following columns:
#                               field       character value specifying the codes
#                                           used to record cover values
#                               presence    numeric value specifying whether the
#                                           cover value is present (1) or absent 
#                                           (0) or missing (NA), used for mean
#                                           presence calculations.
#                               characteristicCover numeric value specifying the
#                                                   value used for mean cover
#                                                   calculations.
#
# argSavePath   character string specifying the path to which data arguments are
#               saved as csv files, or NULL if those files are not to be written.
#
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The data frame contains the following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     VALUE - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

    intermediateMessage('Fish Cover mets', loc='start')

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
        rc <- df %>% select(SITE,TRANSECT,VALUE) %>% mutate(PARAMETER=pName)
        return(rc)
    }

    # fcData <- rbind(absentAsNULL(algae, ifdf, 'ALGAE')
    #                ,absentAsNULL(boulder, ifdf, 'BOULDR')
    #                ,absentAsNULL(brush, ifdf, 'BRUSH')
    #                ,absentAsNULL(liveTree, ifdf, 'LVTREE')
    #                ,absentAsNULL(macrophytes, ifdf, 'MACPHY')
    #                ,absentAsNULL(overhang, ifdf, 'OVRHNG')
    #                ,absentAsNULL(structures, ifdf, 'STRUCT')
    #                ,absentAsNULL(undercut, ifdf, 'UNDERCUT')
    #                ,absentAsNULL(woodyDebris, ifdf, 'WOODY')
    #                )
    classInformation <- aquametStandardizeArgument(classInformation
                                                  ,struct = list(name='character', isBig='logical', isNatural='logical')
                                                  ,legalValues = list(name = c('algae', 'boulder', 'brush'
                                                                              ,'liveTree', 'macrophytes', 'overhang'
                                                                              ,'structures', 'undercut', 'woodyDebris'
                                                                              )
                                                                     ,isBig = c(FALSE, TRUE)
                                                                     ,isNatural = c(FALSE, TRUE)
                                                                     )
                                                  ,argSavePath = argSavePath
                                                  )
    dataInformation <- aquametStandardizeArgument(dataInformation
                                                 ,struct = list(value=c('integer','character'), presence=c('integer','logical'), weights='double')
                                                 ,legalValues = list(presence=c(NA,FALSE, TRUE))
                                                 ,rangeCheck = list(characteristicCover=c(0,1))
                                                 ,argSavePath = argSavePath
                                                 )

    fcData <- rbind(aquametStandardizeArgument(algae, ifdf=ifdf, 'ALGAE'
                                              ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','character'))
                                              ,legalValues = list(VALUE = dataInformation$value) # list(VALUE = as.integer(c(NA,0,1,2,3,4)))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
                   ,aquametStandardizeArgument(boulder, ifdf=ifdf, 'BOULDR'
                                              ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','character'))
                                              ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) # list(VALUE = as.integer(c(NA,0,1,2,3,4)))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
                   ,aquametStandardizeArgument(brush, ifdf=ifdf, 'BRUSH'
                                              ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','character'))
                                              ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) # list(VALUE = as.integer(c(NA,0,1,2,3,4)))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
                   ,aquametStandardizeArgument(liveTree, ifdf=ifdf, 'LVTREE'
                                              ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','character'))
                                              ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) # list(VALUE = as.integer(c(NA,0,1,2,3,4)))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
                   ,aquametStandardizeArgument(macrophytes, ifdf=ifdf, 'MACPHY'
                                              ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','character'))
                                              ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) # list(VALUE = as.integer(c(NA,0,1,2,3,4)))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
                   ,aquametStandardizeArgument(overhang, ifdf=ifdf, 'OVRHNG'
                                              ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','character'))
                                              ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) # list(VALUE = as.integer(c(NA,0,1,2,3,4)))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
                   ,aquametStandardizeArgument(structures, ifdf=ifdf, 'STRUCT'
                                              ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','character'))
                                              ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) # list(VALUE = as.integer(c(NA,0,1,2,3,4)))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
                   ,aquametStandardizeArgument(undercut, ifdf=ifdf, 'UNDERCUT'
                                              ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','character'))
                                              ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) # list(VALUE = as.integer(c(NA,0,1,2,3,4)))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
                   ,aquametStandardizeArgument(woodyDebris, ifdf=ifdf, 'WOODY'
                                              ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('integer','character'))
                                              ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) # list(VALUE = as.integer(c(NA,0,1,2,3,4)))
                                              ,stopOnError = !isUnitTest
                                              ,argSavePath = argSavePath
                                              )
                   )

    coverClassTypes <- dplyr::rename(classInformation, coverType=name)
    coverCalculationValues <- dplyr::rename(dataInformation, field=value, characteristicCover=weights)
    
    # Create tables for converting field values to calculation values
    cover04 <- dplyr::rename(coverCalculationValues, calc = characteristicCover) %>%
               select(field, calc)

    presence04 <- dplyr::rename(coverCalculationValues, calc = presence) %>%
                  select(field, calc)

    fcTypes <- mutate(coverClassTypes
                     ,PARAMETER = ifelse(coverType == 'algae', 'ALGAE'
                                 ,ifelse(coverType ==  'boulder', 'BOULDR'
                                 ,ifelse(coverType ==  'brush', 'BRUSH'
                                 ,ifelse(coverType ==  'liveTree', 'LVTREE'
                                 ,ifelse(coverType ==  'macrophytes', 'MACPHY'
                                 ,ifelse(coverType ==  'overhang', 'OVRHNG'
                                 ,ifelse(coverType ==  'structures', 'STRUCT'
                                 ,ifelse(coverType ==   'undercut', 'UNDERCUT'
                                 ,ifelse(coverType ==   'woodyDebris', 'WOODY', 'UNEXPECTED'
                                 )))))))))
                     ) %>%
               select(PARAMETER, isBig, isNatural)

    intermediateMessage('.1')

    # Calculate presence mean of each type of fish cover
    # Convert field results to calculable values
    fcPresence<-merge(fcData
                     ,presence04
                     ,by.x='VALUE'
                     ,by.y='field'
                     ,all.x=TRUE
                     ,sort=FALSE
                     )

    fcPMeans<-aggregate(fcPresence$calc
                 ,list('SITE'=fcPresence$SITE
                      ,"PARAMETER"=fcPresence$PARAMETER
                      )
                 ,mean, na.rm=TRUE
                 )
                 
    intermediateMessage('.2')

                          
  meanPresence  <-dplyr::rename(fcPMeans, fcfp=x) # 'x','fcfp') 
   
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'ALGAE', 'pfc_alg' , meanPresence$PARAMETER)      
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'BOULDR', 'pfc_rck' , meanPresence$PARAMETER)     
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'BRUSH', 'pfc_brs' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'LVTREE', 'pfc_lvt' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'MACPHY', 'pfc_aqm' , meanPresence$PARAMETER)  
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'OVRHNG', 'pfc_ohv' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'STRUCT', 'pfc_hum' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'UNDERCUT', 'pfc_ucb' , meanPresence$PARAMETER) 
  meanPresence$PARAMETER <- ifelse(meanPresence$PARAMETER == 'WOODY', 'pfc_lwd' , meanPresence$PARAMETER) 
 
  meanPresence <- dplyr::rename(meanPresence, METRIC=PARAMETER, VALUE=fcfp) #c('PARAMETER','fcfp'),c('METRIC' ,'VALUE'))
                         

    intermediateMessage('.3')


    # Calculate cover mean of each type of fish cover
    # Convert field results to calculable values.
    fcCover<-merge(fcData, cover04
                   ,by.x='VALUE'
                   ,by.y='field'
                   ,all.x=TRUE, sort=FALSE
                   )

    fcCMeans <-aggregate(fcCover$calc
                      ,list('SITE'=fcCover$SITE
                           ,"PARAMETER"=fcCover$PARAMETER
                           )
                      ,mean, na.rm=TRUE
                      )
 
      
  meanCover  <- dplyr::rename(fcCMeans, fcfc=x) #'x','fcfc')  
      
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'ALGAE', 'xfc_alg' , meanCover$PARAMETER)      
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'BOULDR', 'xfc_rck' , meanCover$PARAMETER)     
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'BRUSH', 'xfc_brs' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'LVTREE', 'xfc_lvt' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'MACPHY', 'xfc_aqm' , meanCover$PARAMETER)  
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'OVRHNG', 'xfc_ohv' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'STRUCT', 'xfc_hum' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'UNDERCUT', 'xfc_ucb' , meanCover$PARAMETER) 
  meanCover$PARAMETER <- ifelse(meanCover$PARAMETER == 'WOODY', 'xfc_lwd' , meanCover$PARAMETER) 
 
     
     
  meanCover <- dplyr::rename(meanCover, METRIC=PARAMETER, VALUE=fcfc) #c('PARAMETER','fcfc'),c('METRIC' , 'VALUE'))
                                            

    intermediateMessage('.4')


    # Calculate cover indices: total, large, and natural vegetation
    #grouped presence
    fcPMeans <- merge(fcPMeans, fcTypes, by='PARAMETER', all=TRUE, sort=FALSE)

    fciPAll <- aggregate(list('VALUE'=fcPMeans$x)
                       ,list('SITE'=fcPMeans$SITE)
                       ,sum, na.rm=TRUE
                       )
    
    fciPAll$METRIC <- 'pfc_all'
#    fciPAll  <- rename(fciPAll, 'fciPAll','VALUE')  
    
    ttPBig <- subset(fcPMeans, isBig)
    fciPBig <- aggregate(list('VALUE'=ttPBig$x)
                       ,list('SITE'=ttPBig$SITE)
                       ,sum, na.rm=TRUE
                       )
    fciPBig$METRIC <- 'pfc_big'
#    fciPBig  <- rename(fciPBig,'fciPBig','VALUE')                    

    ttPNatural <- subset(fcPMeans, isNatural)
    fciPNatural <- aggregate(list('VALUE'=ttPNatural$x)
                           ,list('SITE'=ttPNatural$SITE)
                           ,sum, na.rm=TRUE
                           )
    fciPNatural$METRIC <- 'pfc_nat'
#    fciPNatural  <- rename(fciPNatural, 'fciPNatural','VALUE') 
  
    # Calculate cover indices: total, large, and natural vegetation
    # grouped cover
    fcCMeans <- merge(fcCMeans, fcTypes, by='PARAMETER', all=TRUE, sort=FALSE)

    fciCAll <- aggregate(list('VALUE'=fcCMeans$x)
                       ,list('SITE'=fcCMeans$SITE)
                       ,sum, na.rm=TRUE
                       )
    
    fciCAll$METRIC <- 'xfc_all'
#    fciCAll  <- rename(fciCAll, 'fciCAll','VALUE')  
      
    ttCBig <- subset(fcCMeans, isBig)
    fciCBig <- aggregate(list('VALUE'=ttCBig$x)
                       ,list('SITE'=ttCBig$SITE)
                       ,sum, na.rm=TRUE
                       )
    fciCBig$METRIC <- 'xfc_big'
#    fciCBig  <- rename(fciCBig, 'fciCBig','VALUE')                    

    ttCNatural <- subset(fcCMeans, isNatural)
    fciCNatural <- aggregate(list('VALUE'=ttCNatural$x)
                           ,list('SITE'=ttCNatural$SITE)
                           ,sum, na.rm=TRUE
                           )
    fciCNatural$METRIC <- 'xfc_nat'
#    fciCNatural  <- rename(fciCNatural, 'fciCNatural','VALUE')
                       
    intermediateMessage('.5')

    # Calculate cover standard deviation of OHV and UCB
    
  
  
    tt<-aggregate(list(VALUE=fcCover$calc)
                 ,list('SITE'=fcCover$SITE
                      ,"METRIC"=fcCover$PARAMETER
                      )
                 ,sd, na.rm=TRUE
                 )
  
    sd1 <- subset(tt, METRIC %in% c('UNDERCUT', 'OVRHNG'))
    
  sd1$METRIC <- ifelse(sd1$METRIC == 'UNDERCUT', 'sdfc_ucb' , sd1$METRIC) 
  sd1$METRIC <- ifelse(sd1$METRIC == 'OVRHNG', 'sdfc_ohv' , sd1$METRIC) 
#  sd1  <- rename(sd1, 'x', 'VALUE') 
#  sd1  <- rename(sd1, 'parameter', 'METRIC') 
     

    intermediateMessage('.6')


    # Calculate iqr, idr for OVRHNG and UNDERCUT midpoints
    
     tt<-aggregate(list(VALUE=fcCover$calc)
                 ,list('SITE'=fcCover$SITE
                      ,"METRIC"=fcCover$PARAMETER
                      )
                 ,iqr
                 )
  
    idr1 <- subset(tt, METRIC %in% c('UNDERCUT', 'OVRHNG'))
  
  idr1$METRIC <- ifelse(idr1$METRIC == 'UNDERCUT', 'idrucb' , idr1$METRIC) 
  idr1$METRIC <- ifelse(idr1$METRIC == 'OVRHNG', 'idrohv' , idr1$METRIC) 
#  idr1  <- rename(idr1, 'x','VALUE') 
#  idr1  <- rename(idr1, 'parameter','METRIC') 
  
  #iqr
  
       tt<-aggregate(list(VALUE=fcCover$calc)
                 ,list('SITE'=fcCover$SITE
                      ,"METRIC"=fcCover$PARAMETER
                      )
                 ,iqr
                 )
  
    iqr1 <- subset(tt, METRIC %in% c('UNDERCUT', 'OVRHNG'))
  
  iqr1$METRIC <- ifelse(iqr1$METRIC == 'UNDERCUT', 'iqrucb' , iqr1$METRIC) 
  iqr1$METRIC <- ifelse(iqr1$METRIC == 'OVRHNG', 'iqrohv' , iqr1$METRIC) 
#  iqr1  <- rename(iqr1, 'x', 'VALUE') 
#  iqr1  <- rename(iqr1, 'parameter', 'METRIC') 
  
  
  
    intermediateMessage('.7')

    # combine results into a dataframe
    
    
  mets <- rbind (meanPresence
                         , meanCover                  
                         ,fciPNatural
                         ,fciPBig
                         ,fciPAll
                         ,fciCNatural
                         ,fciCBig
                         ,fciCAll
                         ,sd1
                         ,idr1
                         ,iqr1)
    
  
    intermediateMessage(' Done.', loc='end')

    return(mets)
}



# end of file
