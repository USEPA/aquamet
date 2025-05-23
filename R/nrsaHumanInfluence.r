#' @export
#' @title Calculate NRSA Human Influence Metrics
#' @description This function calculates the human influence 
#' portion of the physical habitat metrics for National Rivers 
#' and Streams Assessment (NRSA) data.  The function requires 
#' a data frame containing the visible riparian damage data file.
#' @param buildings A data frame containing building influence data 
#' at each transect for all reaches, with the following columns:
#' \itemize{
#' \item SITE integer or character specifying the site visit
#' \item TRANSECT character value specifying the transect
#' for which the value was recorded.
#' \item VALUE character values specifying the influence 
#' class
#' }
#' @param landfillTrash A data frame containing landfill/trash 
#' influence data at each transect for all reaches, with the 
#' following columns:
#' \itemize{
#'  \item SITE integer or character specifying the site visit
#'  \item TRANSECT character value specifying the transect
#'  for which the value was recorded.
#'  \item VALUE character values specifying the influence 
#'  class
#' }
#' @param logging A data frame containing logging influence data at 
#' each transect for all reaches, with the following columns:
#' \itemize{
#'  \item SITE integer or character specifying the site visit
#'  \item TRANSECT    character value specifying the transect
#'  for which the value was recorded.
#'  \item VALUE character values specifying the influence 
#'  class
#' }
#' @param mining A data frame containing mining influence data at 
#' each transect for all reaches, with the following columns:
#' \itemize{
#'    \item SITE integer or character specifying the site visit
#'    \item TRANSECT character value specifying the transect
#'                   for which the value was recorded.
#'    \item VALUE character values specifying the influence 
#'                class
#' }
#' @param parkLawn A data frame containing park and lawn influence 
#' data at each transect for all reaches, with the following columns:
#' \itemize{
#'  \item SITE integer or character specifying the site visit
#'  \item TRANSECT character value specifying the transect
#'                 for which the value was recorded.
#'  \item VALUE character values specifying the influence 
#'              class
#' }
#' @param pastureRangeHay A data frame containing pasture, range or 
#' hay production influence data at each transect for all reaches, 
#' with the following columns:
#' \itemize{
#'    \item SITE integer or character specifying the site visit
#'    \item TRANSECT character value specifying the transect
#'                   for which the value was recorded.
#'    \item VALUE character values specifying the influence 
#'                class
#' }
#' @param pavementClearedlot A data frame containing pavement influence 
#' data at each transect for all reaches, with the following columns:
#' \itemize{
#'  \item SITE integer or character specifying the site 
#'             visit
#'  \item TRANSECT character value specifying the transect
#'                 for which the value was recorded.
#'  \item VALUE character values specifying the influence 
#'              class
#' }
#' @param pipesInOut A data frame containing irrigation piping influence 
#' data at each transect for all reaches, with the following columns:
#' \itemize{
#'  \item SITE integer or character specifying the site visit
#'  \item TRANSECT character value specifying the transect
#'                 for which the value was recorded.
#'  \item VALUE character values specifying the influence 
#'              class
#' }
#' @param roadsRailroads A data frame containing roads and rails 
#' influence data at each transect for all reaches, with the following 
#' columns:
#' \itemize{
#'  \item SITE integer or character specifying the site visit
#'  \item TRANSECT character value specifying the transect
#'                 for which the value was recorded.
#'  \item VALUE character values specifying the influence 
#'              class
#' }
#' @param rowcrops A data frame containing row crop influence data at 
#' each transect for all reaches, with the following columns:
#' \itemize{
#'  \item SITE integer or character specifying the site visit
#'  \item TRANSECT character value specifying the transect
#'                 for which the value was recorded.
#'  \item VALUE character values specifying the influence 
#'              class
#' }
#' @param wallRevetment A data frame containing bank revetment influence 
#' data at each transect for all reaches, with the following columns:
#' \itemize{
#'    \item SITE integer or character specifying the site visit
#'    \item TRANSECT character value specifying the transect
#'                   for which the value was recorded.
#'    \item VALUE character values specifying the influence 
#'                class
#' }
#' @param dataInformation A data frame containing weighting values for 
#' each influence class.  The default value for this argument reproduces EPA
#' NARS calculations.  Expected to have the following columns:
#' \itemize{
#'    \item value character codes used to specify each influence 
#'                class
#'    \item weights numeric value used to weight each influence class
#'          in combined calculations
#'
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
#' \item SITE - universal ID value
#' \item METRIC - metric name
#' \item VALUE - metric value
#' }
#' Metrics calculated:
#' \itemize{
#'   \item sdb_hall   Std dev human disturbance on bank
#'   \item sdcb_hall	 Std dev human dist. on bank or channel
#'   \item sdc_hall	 Std dev human disturbance in channel
#'   \item sdwcb_hall	 Std dev wted human dist on bank or chan
#'   \item w1h_bldg	 Rip Dist--Buildings (ProxWt Pres)
#'   \item w1h_crop	 Rip Dist--Row Crop (ProxWt Pres)
#'   \item w1h_ldfl	 Rip Dist--Trash/Landfill (ProxWt Pres)
#'   \item w1h_log	 Rip Dist--Logging Activity (ProxWt Pres)
#'   \item w1h_mine	 Rip Dist--Mining Activity (ProxWt Pres)
#'   \item w1h_park	 Rip Dist--Lawn/Park (ProxWt Pres)
#'   \item w1h_pipe	 Rip Dist--Pipes infl/effl (ProxWt Pres)
#'   \item w1h_pstr	 Rip Dist--Pasture/Hayfield (ProxWt Pres)
#'   \item w1h_pvmt	 Rip Dist--Pavement (ProxWt Pres)
#'   \item w1h_road	 Rip Dist--Road/Railroad (ProxWt Pres)
#'   \item w1h_wall	 Rip Dist--Wall/Bank Revet. (ProxWt Pres)
#'   \item w1_hag	 Rip Dist--Sum Agric Types (ProxWt Pres)
#'   \item w1_hall	 Rip Dist--Sum All Types (ProxWt Pres)
#'   \item w1_hnoag	 Rip Dist--Sum NonAg Types (ProxWt Pres)
#'   \item xb_hag	 Rip Dist-Sum Ag Types instrm & in plot
#'   \item xb_hall	 Rip Dist--Sum All Types instrm & on bank
#'   \item xb_hnoag	 Rip Dist Sum-Non ag Types instrm & Plot
#'   \item xcb_hag	 Rip Dist Sum-Ag Types instrm & on Bank
#'   \item xcb_hall	 Rip Dist--Sum All Types instrm & in plot
#'   \item xcb_hnag	 Rip Dist Sum-Non Ag Types instrm & Bank
#'   \item xc_hag	 Rip Dist-Sum of Ag Types in Ripar Plot
#'   \item xc_hall	 Rip Dist--Sum All Types in Ripar Plots
#'   \item xc_hnoag	 Rip Dist Sum-Non Ag Types in Ripar Plot
#'   \item xf_hag	 Rip Dist Sum-Ag Types Beyond Ripar Plot
#'   \item xf_hall	 Rip Dist--Sum All Types beyond Rip Plots
#'   \item xf_hnoag	 Rip Dist Sum-Non Ag Types Beyond Rip Plt
#'   \item x_hag	 Rip Dist Sum-Ag Types rip Plt & Beyond
#'   \item x_hall	 Rip Dist--Sum All Types str plt & beyond
#'   \item x_hnoag	 Rip Dist Sum-Non Ag rip Plt & Beyond
#' } 
#' Descriptions for all NRSA metrics can be found at:
#' \href{https://github.com/USEPA/aquamet/blob/master/inst/NRSA_physical_habitat_metrics_descriptions.pdf}{NLA_Physical_Habitat_Metric_Descriptions.pdf}.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#' head(visripEx)
#' 
#' # Subset example dataset within function to create inputs, keeping only SITE,
#'   #  TRANSECT, and VALUE. 
#' huminflOut <- nrsaHumanInfluence(
#' buildings=subset(visripEx,PARAMETER=='BUILD',
#'  select = c(SITE,TRANSECT,VALUE)),
#' landfillTrash=subset(visripEx,PARAMETER=='LANDFL',
#'  select = c(SITE,TRANSECT,VALUE)),
#' logging=subset(visripEx,PARAMETER=='LOG',
#'  select = c(SITE,TRANSECT,VALUE)),
#' mining=subset(visripEx,PARAMETER=='MINE',
#'  select = c(SITE,TRANSECT,VALUE)),
#' parkLawn=subset(visripEx,PARAMETER=='PARK',
#'  select = c(SITE,TRANSECT,VALUE)),
#' pastureRangeHay=subset(visripEx,PARAMETER=='PAST',
#'  select = c(SITE,TRANSECT,VALUE)),
#' pavementClearedlot=subset(visripEx,PARAMETER=='PAVE',
#'  select = c(SITE,TRANSECT,VALUE)),
#' pipesInOut=subset(visripEx,PARAMETER=='PIPES',
#'  select = c(SITE,TRANSECT,VALUE)),
#' roadsRailroads=subset(visripEx,PARAMETER=='ROAD',
#'  select = c(SITE,TRANSECT,VALUE)),
#' rowcrops=subset(visripEx,PARAMETER=='ROW',
#'  select = c(SITE,TRANSECT,VALUE)),
#' wallRevetment=subset(visripEx,PARAMETER=='WALL',
#'  select = c(SITE,TRANSECT,VALUE))
#'  )
#' 
#' head(huminflOut)


nrsaHumanInfluence <- function(buildings = NULL
                              ,landfillTrash = NULL
                              ,logging = NULL
                              ,mining = NULL
                              ,parkLawn = NULL
                              ,pastureRangeHay = NULL
                              ,pavementClearedlot = NULL
                              ,pipesInOut = NULL
                              ,roadsRailroads = NULL
                              ,rowcrops = NULL
                              ,wallRevetment = NULL
                              ,dataInformation = data.frame(value=c('0','P','C','B')
                                                           #,presence=c(1,1,1,1)
                                                           ,weights=c(0, 0.666667, 1, 1.5)
                                                           ,stringsAsFactors=FALSE
                                                           )
                              ,isUnitTest = FALSE
                              ,argSavePath = NULL
                              ) {

################################################################################
# Function: nrsaHumanInfluence
# Title: Calculate NRSA Human Influence Metrics
# Programmers: Suzanne San Romani
#              Curt Seeliger
#              Tom Kincaid
# Date: February 10, 2010
# Description:
#   This function calculates the human influence portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires a data frame containing the visible riparian damage data
#   file.
# Metrics:
#   sdb_hall	 Std dev human disturbance on bank
#   sdcb_hall	 Std dev human dist. on bank or channel
#   sdc_hall	 Std dev human disturbance in channel
#   sdwcb_hall	 Std dev wted human dist on bank or chan
#   w1h_bldg	 Rip Dist--Buildings (ProxWt Pres)
#   w1h_crop	 Rip Dist--Row Crop (ProxWt Pres)
#   w1h_ldfl	 Rip Dist--Trash/Landfill (ProxWt Pres)
#   w1h_log	 Rip Dist--Logging Activity (ProxWt Pres)
#   w1h_mine	 Rip Dist--Mining Activity (ProxWt Pres)
#   w1h_park	 Rip Dist--Lawn/Park (ProxWt Pres)
#   w1h_pipe	 Rip Dist--Pipes infl/effl (ProxWt Pres)
#   w1h_pstr	 Rip Dist--Pasture/Hayfield (ProxWt Pres)
#   w1h_pvmt	 Rip Dist--Pavement (ProxWt Pres)
#   w1h_road	 Rip Dist--Road/Railroad (ProxWt Pres)
#   w1h_wall	 Rip Dist--Wall/Bank Revet. (ProxWt Pres)
#   w1_hag	 Rip Dist--Sum Agric Types (ProxWt Pres)
#   w1_hall	 Rip Dist--Sum All Types (ProxWt Pres)
#   w1_hnoag	 Rip Dist--Sum NonAg Types (ProxWt Pres)
#   xb_hag	 Rip Dist-Sum Ag Types instrm & in plot
#   xb_hall	 Rip Dist--Sum All Types instrm & on bank
#   xb_hnoag	 Rip Dist Sum-Non ag Types instrm & Plot
#   xcb_hag	 Rip Dist Sum-Ag Types instrm & on Bank
#   xcb_hall	 Rip Dist--Sum All Types instrm & in plot
#   xcb_hnag	 Rip Dist Sum-Non Ag Types instrm & Bank
#   xc_hag	 Rip Dist-Sum of Ag Types in Ripar Plot
#   xc_hall	 Rip Dist--Sum All Types in Ripar Plots
#   xc_hnoag	 Rip Dist Sum-Non Ag Types in Ripar Plot
#   xf_hag	 Rip Dist Sum-Ag Types Beyond Ripar Plot
#   xf_hall	 Rip Dist--Sum All Types beyond Rip Plots
#   xf_hnoag	 Rip Dist Sum-Non Ag Types Beyond Rip Plt
#   x_hag	 Rip Dist Sum-Ag Types rip Plt & Beyond
#   x_hall	 Rip Dist--Sum All Types str plt & beyond
#   x_hnoag	 Rip Dist Sum-Non Ag rip Plt & Beyond
# Function Revisions:
#   02/10/10 ssr: Created.
#   02/19/10 ssr: Removed source() call.
#   03/23/10 cws: Moved creation of test dataframes to separate functions.
#   03/25/10 cws: Changed diff() calls to dfCompare().
#   03/25/10 ssr: Changed metrics names to lowercase.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   07/26/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify name of the data file.  Added argument NRSAdir to the
#            function to identify the directory from which the data file is read
#            and to which the output metrics file is written.
#   12/14/12 tmk: Modified data input to use a data frame containing the data
#            file rather than a csv file.  Modified output to be a data frame
#            rather than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frame to
#            character variables.
#   11/13/15 cws Rewrote calling interface.
#    1/20/16 cws Removed test is.na(ifdf); now using is.function(ifdf) instead.
#    3/17/16 cws Added 'Done' notification at end of calculations.  Removed
#            commented out code. Pretty printed.
#    3/12/19 cws Changed to use aquametStandardizeArgument() instead of 
#            absentAsNull().
#    3/20/19 cws Checking structure and legal values of influenceWeights argument,
#            and using those values in subsequent legal values checks of data.
#    3/25/19 cws Modified to use dplyr::rename()
#    3/29/19 cws Standardized metadata argument naming
#    8/07/23 cws Added argSavePath argument, as newly needed for 
#            aquametStandardizeArgument
#
#  TODO: rewrite interior. Add use of influenceWeights and test it.  Make PARAMETER
#        values less cryptic.
#
# ARGUMENTS:
# buildings         A data frame containing building influence data at each transect 
#                   for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
#
# landfillTrash     A data frame containing landfill/trash influence data at each 
#                   transect for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
# logging           A data frame containing logging influence data at each transect 
#                   for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
#
# mining            A data frame containing mining influence data at each transect 
#                   for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
#
# parkLawn          A data frame containing park and lawn influence data at each  
#                   transect for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
#
# pastureRangeHay   A data frame containing pasture, range or hay production 
#                   influence data at each transect for all reaches, with the 
#                   following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
#
# pavementClearedlot    A data frame containing pavement influence data at each  
#                       transect for all reaches, with the following columns:
#                           SITE        integer or character specifying the site 
#                                       visit
#                           TRANSECT    character value specifying the transect
#                                       for which the value was recorded.
#                           VALUE       character values specifying the influence 
#                                       class
#
# pipesInOut        A data frame containing irrigation piping influence data at each 
#                   transect for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
#
# roadsRailroads    A data frame containing roads and rails influence data at each 
#                   transect for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
#
# rowcrops          A data frame containing row crop influence data at each transect 
#                   for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
#
# wallRevetment     A data frame containing bank revetment influence data at each 
#                   transect for all reaches, with the following columns:
#                       SITE        integer or character specifying the site visit
#                       TRANSECT    character value specifying the transect
#                                   for which the value was recorded.
#                       VALUE       character values specifying the influence 
#                                   class
#
# influenceWeights  A data frame containing weighting values for each influence
#                   class.  The default value for this argument reproduces EPA
#                   NARS calculations.  Expected to have the following columns:
#                       VALUE   character codes used to specify each influence 
#                               class
#                       weights numeric value used to weight each influence class
#                               in combined calculations
#
# argSavePath   character string specifying the path to which data arguments are
#               saved as csv files, or NULL if those files are not to be written.
#
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

    # Print an initial message
    intermediateMessage('Human Influence calculations', loc='start')

    # Standardize data arguments with no data as NULL
    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(!is.function(ifdf)) return(df)
        else return(ifdf(df, ...))
    }
    ifdf <- function(df, pName) {
        rc <- df %>% mutate(PARAMETER=pName) %>% select(SITE,TRANSECT,PARAMETER,VALUE)
    }

    # df <- rbind(absentAsNULL(buildings, ifdf,  'BUILD')
    #            ,absentAsNULL(landfillTrash, ifdf, 'LANDFL')
    #            ,absentAsNULL(logging, ifdf, 'LOG')
    #            ,absentAsNULL(mining, ifdf,  'MINE')
    #            ,absentAsNULL(parkLawn, ifdf, 'PARK')
    #            ,absentAsNULL(pastureRangeHay, ifdf, 'PAST')
    #            ,absentAsNULL(pavementClearedlot, ifdf, 'PAVE')
    #            ,absentAsNULL(pipesInOut, ifdf, 'PIPES')
    #            ,absentAsNULL(roadsRailroads, ifdf, 'ROAD')
    #            ,absentAsNULL(rowcrops, ifdf, 'ROW')
    #            ,absentAsNULL(wallRevetment, ifdf, 'WALL')
    #            )
    dataInformation <- aquametStandardizeArgument(dataInformation, ifdf=NULL
                                                 ,struct = list(value = 'character', weights='double')
                                                 ,legalValues = list(value = c(NA,'','0','P','C','B'))
                                                 ,argSavePath = argSavePath
                                                 )
    df <- rbind(aquametStandardizeArgument(buildings, ifdf=ifdf,  'BUILD'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(landfillTrash, ifdf=ifdf, 'LANDFL'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(logging, ifdf=ifdf, 'LOG'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(mining, ifdf=ifdf,  'MINE'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(parkLawn, ifdf=ifdf, 'PARK'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(pastureRangeHay, ifdf=ifdf, 'PAST'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(pavementClearedlot, ifdf=ifdf, 'PAVE'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(pipesInOut, ifdf=ifdf, 'PIPES'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(roadsRailroads, ifdf=ifdf, 'ROAD'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(rowcrops, ifdf=ifdf, 'ROW'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               ,aquametStandardizeArgument(wallRevetment, ifdf=ifdf, 'WALL'
                                          ,struct = list(SITE=c('integer','character'), TRANSECT='character', VALUE=c('character'))
                                          ,legalValues = list(VALUE = c(NA, '', dataInformation$value)) #list(VALUE = c(NA,'','0','P','C','B'))
                                          ,stopOnError = !isUnitTest
                                          ,argSavePath = argSavePath
                                          )
               )

    influenceWeights <- dplyr::rename(dataInformation, VALUE=value)

    df2 <- df
    df2$var_0 <- ifelse(df$VALUE=='0', 1, 0)
    df2$var_P <- ifelse(df$VALUE=='P', 1, 0)
    df2$var_C <- ifelse(df$VALUE=='C', 1, 0)
    df2$var_B <- ifelse(df$VALUE=='B', 1, 0)
    df2$var_CB <- df2$var_C+ df2$var_B
           
    #    Calculating meanAtTransect
    meanb_hall <- aggregate(list(xb_hall=df2$var_B), list(SITE=df2$SITE
                           ,PARAMETER=df2$PARAMETER), mean, na.rm=T
                           )
    meanc_hall <- aggregate(list(xc_hall=df2$var_C), list(SITE=df2$SITE
                           ,PARAMETER=df2$PARAMETER), mean, na.rm=T
                           ) 
    meancb_hall <- aggregate(list(xcb_hall=df2$var_CB), list(SITE=df2$SITE
                            ,PARAMETER=df2$PARAMETER), mean, na.rm=T
                            )
    meanf_hall <- aggregate(list(xf_hall=df2$var_P), list(SITE=df2$SITE
                           ,PARAMETER=df2$PARAMETER), mean, na.rm=T
                           ) 

    #   sum(meanAtTransect)
    xb_hall <- aggregate(list(xb_hall=meanb_hall$xb_hall)
                        ,list(SITE=meanb_hall$SITE), sum, na.rm=T
                        )
    xc_hall <- aggregate(list(xc_hall=meanc_hall$xc_hall)
                        ,list(SITE=meanc_hall$SITE), sum, na.rm=T
                        ) 
    xcb_hall <- aggregate(list(xcb_hall=meancb_hall$xcb_hall)
                         ,list(SITE=meancb_hall$SITE), sum, na.rm=T
                         )
    xf_hall <- aggregate(list(xf_hall=meanf_hall$xf_hall)
                        ,list(SITE=meanf_hall$SITE), sum, na.rm=T
                        ) 

    #    Creating ag (agriculture parameters) and noag (non-agriculture parameters)
    ag <- subset(df2, (PARAMETER %in% c('ROW', 'PAST')))
    noag <- subset(df2, !(PARAMETER %in% c('ROW', 'PAST')))

    #    Calculating meanAtTransect for agriculture parameters
    meanb_hag <- aggregate(list(xb_hag=ag$var_B), list(SITE=ag$SITE
                          ,PARAMETER=ag$PARAMETER), mean, na.rm=T
                          )
    meanc_hag <- aggregate(list(xc_hag=ag$var_C), list(SITE=ag$SITE
                          ,PARAMETER=ag$PARAMETER), mean, na.rm=T
                          ) 
    meancb_hag <- aggregate(list(xcb_hag=ag$var_CB), list(SITE=ag$SITE
                           ,PARAMETER=ag$PARAMETER), mean, na.rm=T
                           )
    meanf_hag <- aggregate(list(xf_hag=ag$var_P), list(SITE=ag$SITE
                          ,PARAMETER=ag$PARAMETER), mean, na.rm=T
                          ) 

    #   sum(meanAtTransect) for agriculture parameters
    xb_hag <- aggregate(list(xb_hag=meanb_hag$xb_hag), list(SITE=meanb_hag$SITE)
                       ,sum, na.rm=T
                       )
    xc_hag <- aggregate(list(xc_hag=meanc_hag$xc_hag), list(SITE=meanc_hag$SITE)
                       ,sum, na.rm=T
                       ) 
    xcb_hag <- aggregate(list(xcb_hag=meancb_hag$xcb_hag), list(SITE=meancb_hag$SITE)
                       ,sum, na.rm=T
                       )
    xf_hag <- aggregate(list(xf_hag=meanf_hag$xf_hag), list(SITE=meanf_hag$SITE)
                       ,sum, na.rm=T
                       ) 

    #    Calculating meanAtTransect for non-agriculture parameters
    meanb_hnoag <- aggregate(list(xb_hnoag=noag$var_B), list(SITE=noag$SITE
                            ,PARAMETER=noag$PARAMETER), mean, na.rm=T
                            )
    meanc_hnoag <- aggregate(list(xc_hnoag=noag$var_C), list(SITE=noag$SITE
                            ,PARAMETER=noag$PARAMETER), mean, na.rm=T
                            ) 
    meancb_hnoag <- aggregate(list(xcb_hnag=noag$var_CB), list(SITE=noag$SITE
                             ,PARAMETER=noag$PARAMETER), mean, na.rm=T
                             )
    meanf_hnoag <- aggregate(list(xf_hnoag=noag$var_P), list(SITE=noag$SITE
                            ,PARAMETER=noag$PARAMETER), mean, na.rm=T
                            ) 

    #   sum(meanAtTransect) for non-agriculture parameters
    xb_hnoag <- aggregate(list(xb_hnoag=meanb_hnoag$xb_hnoag)
                         ,list(SITE=meanb_hnoag$SITE), sum, na.rm=T
                         )
    xc_hnoag <- aggregate(list(xc_hnoag=meanc_hnoag$xc_hnoag)
                         ,list(SITE=meanc_hnoag$SITE), sum, na.rm=T
                         ) 
    xcb_hnoag <- aggregate(list(xcb_hnag=meancb_hnoag$xcb_hnag)
                          ,list(SITE=meancb_hnoag$SITE), sum, na.rm=T
                          )
    xf_hnoag <- aggregate(list(xf_hnoag=meanf_hnoag$xf_hnoag)
                         ,list(SITE=meanf_hnoag$SITE), sum, na.rm=T
                         ) 

    df3 <- merge(xb_hall, xc_hall, all=T)
    df3 <- merge(df3, xcb_hall, all=T)
    df3 <- merge(df3, xf_hall, all=T)
    df3 <- merge(df3, xb_hag, all=T)
    df3 <- merge(df3, xc_hag, all=T)
    df3 <- merge(df3, xcb_hag, all=T)
    df3 <- merge(df3, xf_hag, all=T)
    df3 <- merge(df3, xb_hnoag, all=T)
    df3 <- merge(df3, xc_hnoag, all=T)
    df3 <- merge(df3, xcb_hnoag, all=T)
    df3 <- merge(df3, xf_hnoag, all=T)

    df3$METRIC <- ''

    df3$x_hall <- df3$xb_hall + df3$xc_hall + df3$xf_hall
    df3$x_hag <-  df3$xb_hag + df3$xc_hag + df3$xf_hag
    df3$x_hnoag <- df3$xb_hnoag + df3$xc_hnoag + df3$xf_hnoag

    #   Creating dataframes for export to csv file
    x_hall <- subset(df3, select=c(SITE,METRIC,x_hall))
    x_hall$METRIC <- 'x_hall'
    x_hall <- dplyr::rename(x_hall, VALUE=x_hall)
    x_hag <- subset(df3, select=c(SITE,METRIC,x_hag))
    x_hag$METRIC <- 'x_hag'
    x_hag <- dplyr::rename(x_hag, VALUE=x_hag) 
    x_hnoag <- subset(df3, select=c(SITE,METRIC,x_hnoag))
    x_hnoag$METRIC <- 'x_hnoag'
    x_hnoag <- dplyr::rename(x_hnoag, VALUE=x_hnoag)
 
    xb_hall <- dplyr::rename(xb_hall, VALUE=xb_hall)
    xb_hall$METRIC <- 'xb_hall' 
    xc_hall <- dplyr::rename(xc_hall, VALUE=xc_hall)
    xc_hall$METRIC <- 'xc_hall' 
    xcb_hall <- dplyr::rename(xcb_hall, VALUE=xcb_hall)
    xcb_hall$METRIC <- 'xcb_hall' 
    xf_hall <- dplyr::rename(xf_hall, VALUE=xf_hall)
    xf_hall$METRIC <- 'xf_hall' 
    xb_hag <- dplyr::rename(xb_hag, VALUE=xb_hag)
    xb_hag$METRIC <- 'xb_hag' 
    xc_hag <- dplyr::rename(xc_hag, VALUE=xc_hag)
    xc_hag$METRIC <- 'xc_hag' 
    xcb_hag <- dplyr::rename(xcb_hag, VALUE=xcb_hag)
    xcb_hag$METRIC <- 'xcb_hag' 
    xf_hag <- dplyr::rename(xf_hag, VALUE=xf_hag)
    xf_hag$METRIC <- 'xf_hag' 
    xb_hnoag <- dplyr::rename(xb_hnoag, VALUE=xb_hnoag)
    xb_hnoag$METRIC <- 'xb_hnoag' 
    xc_hnoag <- dplyr::rename(xc_hnoag, VALUE=xc_hnoag)
    xc_hnoag$METRIC <- 'xc_hnoag' 
    xcb_hnoag <- dplyr::rename(xcb_hnoag, VALUE=xcb_hnag)
    xcb_hnoag$METRIC <- 'xcb_hnag' 
    xf_hnoag <- dplyr::rename(xf_hnoag, VALUE=xf_hnoag)
    xf_hnoag$METRIC <- 'xf_hnoag' 

    #   Standard deviations of sumAtTransect
    sumb_hall <- aggregate(list(sum_b=df2$var_B), list(SITE=df2$SITE, TRANSECT=df2$TRANSECT), sum, na.rm=T)
    sumc_hall <- aggregate(list(sum_c=df2$var_C), list(SITE=df2$SITE, TRANSECT=df2$TRANSECT), sum, na.rm=T)
    sumcb_hall <- aggregate(list(sum_cb=df2$var_CB), list(SITE=df2$SITE, TRANSECT=df2$TRANSECT), sum, na.rm=T)
    df2$wcb = (1.5 * df2$var_B) + df2$var_C
    sumwcb_hall <- aggregate(list(sum_wcb=df2$wcb), list(SITE=df2$SITE, TRANSECT=df2$TRANSECT), sum, na.rm=T)


    sdb_hall <- aggregate(list(VALUE=sumb_hall$sum_b), list(SITE=sumb_hall$SITE), sd)
    sdb_hall$METRIC <- 'sdb_hall'
    sdc_hall <- aggregate(list(VALUE=sumc_hall$sum_c), list(SITE=sumc_hall$SITE), sd)
    sdc_hall$METRIC <- 'sdc_hall'
    sdcb_hall <- aggregate(list(VALUE=sumcb_hall$sum_cb), list(SITE=sumcb_hall$SITE), sd)
    sdcb_hall$METRIC <- 'sdcb_hall'
    sdwcb_hall <- aggregate(list(VALUE=sumwcb_hall$sum_wcb), list(SITE=sumwcb_hall$SITE), sd)
    sdwcb_hall$METRIC <- 'sdwcb_hall'

    #   Weighted sums 
    df3$w1_hall <- (1.5 * df3$xb_hall) + df3$xc_hall + (0.6667 * df3$xf_hall)
    df3$w1_hnoag <- (1.5 * df3$xb_hnoag) + df3$xc_hnoag + (0.6667 * df3$xf_hnoag)
    df3$w1_hag <- (1.5 * df3$xb_hag) + df3$xc_hag + (0.6667 * df3$xf_hag)

    w1_hall <- subset(df3, select=c('SITE', 'METRIC', 'w1_hall'))
    w1_hnoag <- subset(df3, select=c('SITE', 'METRIC', 'w1_hnoag'))
    w1_hag <- subset(df3, select=c('SITE', 'METRIC', 'w1_hag'))
                                  
    # Create table for weighing influence proximity values
    weights0PCB <- data.frame(proximity=c('0', 'P', 'C', 'B')
                             ,calc=     c(0.0, 0.6667, 1.0, 1.5)
                             ,stringsAsFactors=F
                             )

    # Convert proximity classes to numeric values and characterize influence types
    df5 <- merge(df, weights0PCB
                ,by.x='VALUE'
                ,by.y='proximity'
                ,all.x=TRUE
                ,sort=FALSE
                )

    #    Creating W1H_ variables
    w1h_sums <- aggregate(list(VALUE=df5$calc)
                         ,list(SITE=df5$SITE, METRIC=df5$PARAMETER)
                         ,mean, na.rm=T
                         )
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='BUILD', 'w1h_bldg', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='LANDFL', 'w1h_ldfl', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='LOG', 'w1h_log', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='MINE', 'w1h_mine', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PARK', 'w1h_park', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PAST', 'w1h_pstr', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PAVE', 'w1h_pvmt', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='PIPES', 'w1h_pipe', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='ROAD', 'w1h_road', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='ROW', 'w1h_crop', w1h_sums$METRIC)
    w1h_sums$METRIC <- ifelse(w1h_sums$METRIC=='WALL', 'w1h_wall', w1h_sums$METRIC)

    w1_ag <- subset(w1h_sums,(METRIC %in% c('w1h_crop', 'w1h_pstr')))
    w1_hag <- aggregate(list(VALUE=w1_ag$VALUE), list(SITE=w1_ag$SITE), sum, na.rm=T)
    w1_hag$METRIC <- 'w1_hag'

    w1_noag <- subset(w1h_sums, !(METRIC %in% c('w1h_crop', 'w1h_pstr')))
    w1_hnoag <- aggregate(list(VALUE=w1_noag$VALUE), list(SITE=w1_noag$SITE), sum, na.rm=T)
    w1_hnoag$METRIC <- 'w1_hnoag'
    
    w1_hall <- aggregate(list(VALUE=w1h_sums$VALUE),list(SITE=w1h_sums$SITE),sum, na.rm=T)
    w1_hall$METRIC <- 'w1_hall'

    aa <- rbind(xb_hag,xc_hag,xcb_hag,xf_hag,xb_hnoag,xcb_hnoag,xc_hnoag,xf_hnoag
               ,xb_hall,xcb_hall,xc_hall,xf_hall,sdb_hall,sdcb_hall,sdc_hall
               ,sdwcb_hall,w1_hag,w1_hnoag,w1_hall)
    bb <- aa[,c('SITE', 'METRIC', 'VALUE')] 
    hiMets <- rbind(bb,w1h_sums,x_hag,x_hnoag,x_hall)

    intermediateMessage('Human Influence calculations', loc='end')

    return(hiMets) 
}

# end of file
