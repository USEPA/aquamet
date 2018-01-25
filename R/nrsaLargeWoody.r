# nrsaLargeWoody.r
#
#' @export 
#' @title Calculate NRSA Large Woody Debris Metrics
#' @description This function calculates the large woody debris 
#' portion of the physical habitat metrics for National Rivers 
#' and Streams Assessment (NRSA) data.  The function requires data 
#' frames containing the thalweg, channel geometry, bank 
#' geometry, large woody debris, and stream verification form data files.
#' @param bCounts A data frame containing large woody debris class counts 
#' at each transect for boatable reaches , with the following columns:
#' \itemize{
#'    \item SITE integer or character specifying the site visit
#'    \item TRANSECT character value specifying the transect
#'                   for which the value was recorded.
#'    \item CLASS character value specifying the class name
#'                (one of 4 diameter classes, 3 length classes,
#'                and 2 location classes).  These class names are
#'                expected to be defined in the bClassInfo
#'                argument
#'    \item VALUE character values specifying the influence 
#'                class
#' }
#' @param bClassInfo A data frame relating the class names used in 
#' bCounts::CLASS to each combination of size and location classes.  
#' Expected to have the following columns and values in 24 rows:
#' \itemize{
#'      \item diam character value specifying diameter class: SMALL
#'                 MEDIUM, LARGE, EXTRALARGE, as defined in the NRSA
#'                 field protocol manual
#'      \item len character value specifying length class: SMALL
#'                MEDIUM, LARGE, as defined in the NRSA field protocol
#'                manual
#'      \item loc character value specifying location class: DRY, WET
#'                as defined in the NRSA field protocol manual
#'      \item CLASS character value associated with each combination of
#'                  diameter, length and channel location, as it occurs
#'                  in the bCounts argument.
#' }
#' Default values are for EPA NARS processing.
#'
#' @param wCounts A data frame containing large woody debris class counts 
#' at each transect for wadeable reaches , with the following columns:
#' \itemize{
#'    \item SITE integer or character specifying the site visit
#'    \item TRANSECT character value specifying the transect
#'                   for which the value was recorded.
#'    \item CLASS character value specifying the class name
#'                (one of 4 diameter classes, 3 length classes,
#'                and 2 location classes).  These class names are
#'                expected to be defined in the wClassInfo
#'                argument
#'    \item VALUE character values specifying the influence 
#'                class
#' }
#' @param wClassInfo A data frame relating the class names used in 
#' wCounts::CLASS to each combination of size and location classes.  
#' Expected to have the following columns and values in 24 rows:
#' \itemize{
#'    \item diam character value specifying diameter class: SMALL
#'               MEDIUM, LARGE, EXTRALARGE, as defined in the NRSA
#'               field protocol manual
#'    \item len character value specifying length class: SMALL
#'              MEDIUM, LARGE, as defined in the NRSA field protocol
#'              manual
#'    \item loc character value specifying location class: DRY, WET
#'              as defined in the NRSA field protocol manual
#'    \item CLASS character value associated with each combination of
#'                diameter, length and channel location, as it occurs
#'                in the wCounts argument.
#' }
#' Default value is for EPA NARS processing.
#'
#' @param reachlength A data frame containing reach lengths in meters 
#' for each site. Expected to contain the following columns:
#' \itemize{
#'    \item SITE integer or character specifying the site visit
#'    \item VALUE numeric values
#' }
#' @param meanBankfullWidth A data frame containing mean bank-full 
#' widths in meters for each site.  Expected to contain the following 
#' columns:
#' \itemize{
#'  \item SITE integer or character specifying the site visit
#'  \item VALUE numeric values
#'
#' }
#' @return Either a data frame when metric calculation is successful 
#' or a character string containing an error message when metric 
#' calculation is not successful.  The data frame contains the following 
#' columns:
#' \itemize{
#'    \item SITE - universal ID value
#'    \item METRIC - metric name
#'    \item VALUE - metric value
#' }
#' Metrics calculated for wadeable sites include: 
#' c1d, c1dm100, c1t, c1tm100, c1w, c1w_msq, c1wm100, c2d, c2dm100, c2t,
#'   c2tm100, c2w, c2w_msq, c2wm100, c3d, c3dm100, c3t, c3tm100, c3w, c3w_msq,
#'   c3wm100, c4d, c4dm100, c4t, c4tm100, c4w, c4w_msq, c4wm100, c5d, c5dm100,
#'   c5t, c5tm100, c5w, c5w_msq, c5wm100, lgdiatot, lgdrydia, lgdrylen, lglentot,
#'   lgwetdia, lgwetlen, lwddv33, lwddvcal, lwdtv33, lwdtvcal, lwdwv33, lwdwvcal,
#'   mddiatot, mddrydia, mddrylen, mdlentot, mdwetdia, mdwetlen, nc, ns,
#'   rchdldll, rchdldml, rchdldsl, rchdmdll, rchdmdml, rchdmdsl, rchdryt,
#'   rchdsdll, rchdsdml, rchdsdsl, rchdxdll, rchdxdml, rchdxdsl, rchtldll,
#'   rchtldml, rchtldsl, rchtmdll, rchtmdml, rchtmdsl, rchtsdll, rchtsdml,
#'   rchtsdsl, rchtxdll, rchtxdml, rchtxdsl, rchwdt, rchwett, rchwldll, rchwldml,
#'   rchwldsl, rchwmdll, rchwmdml, rchwmdsl, rchwsdll, rchwsdml, rchwsdsl,
#'   rchwxdll, rchwxdml, rchwxdsl, shdrylen, shlentot, shwetlen, smdiatot,
#'   smdrydia, smwetdia, v1d, v1dm100, v1t, v1tm100, v1w, v1w_msq, v1wm100, v2d,
#'   v2dm100, v2t, v2tm100, v2w, v2w_msq, v2wm100, v3d, v3dm100, v3t, v3tm100,
#'   v3w, v3w_msq, v3wm100, v4d, v4dm100, v4t, v4tm100, v4w, v4w_msq, v4wm100,
#'   v5d, v5dm100, v5t, v5tm100, v5w, v5w_msq, v5wm100, xldiatot, xldrydia,
#'   xlwetdia
#'   
#' Metrics calculated for boatable sites include:
#'   c1d, c1dm100, c1t, c1tm100, c1w, c1w_msq, c1wm100, c2d, c2dm100, c2t,
#'   c2tm100, c2w, c2w_msq, c2wm100, c3d, c3dm100, c3t, c3tm100, c3w, c3w_msq,
#'   c3wm100, c4d, c4dm100, c4t, c4tm100, c4w, c4w_msq, c4wm100, c5d, c5dm100,
#'   c5t, c5tm100, c5w, c5w_msq, c5wm100, lgdiatot, lgdrydia, lgdrylen, lglentot,
#'   lgwetdia, lgwetlen, lwddv33, lwddvcal, lwdtv33, lwdtvcal, lwdwv33, lwdwvcal,
#'   mddiatot, mddrydia, mddrylen, mdlentot, mdwetdia, mdwetlen, nc, ns,
#'   rchdldll, rchdldml, rchdldsl, rchdmdll, rchdmdml, rchdmdsl, rchdryt,
#'   rchdsdll, rchdsdml, rchdsdsl, rchdxdll, rchdxdml, rchdxdsl, rchtldll,
#'   rchtldml, rchtldsl, rchtmdll, rchtmdml, rchtmdsl, rchtsdll, rchtsdml,
#'   rchtsdsl, rchtxdll, rchtxdml, rchtxdsl, rchwdt, rchwett, rchwldll, rchwldml,
#'   rchwldsl, rchwmdll, rchwmdml, rchwmdsl, rchwsdll, rchwsdml, rchwsdsl,
#'   rchwxdll, rchwxdml, rchwxdsl, shdrylen, shlentot, shwetlen, smdiatot,
#'   smdrydia, smwetdia, v1d, v1dm100, v1t, v1tm100, v1w, v1w_msq, v1wm100, v2d,
#'   v2dm100, v2t, v2tm100, v2w, v2w_msq, v2wm100, v3d, v3dm100, v3t, v3tm100,
#'   v3w, v3w_msq, v3wm100, v4d, v4dm100, v4t, v4tm100, v4w, v4w_msq, v4wm100,
#'   v5d, v5dm100, v5t, v5tm100, v5w, v5w_msq, v5wm100, xldiatot, xldrydia,
#'   xlwetdia 
#'       
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' 
#' @examples
#' # See Bed Stability vignette for example of calculating large woody debris metrics.
#'   
#nrsaLargeWoody <- function(thalweg, channelgeometry, bankgeometry, wood, visits) {
nrsaLargeWoody <- function(bCounts=NULL
                          ,bClassInfo=data.frame(diam=rep(c('SMALL','MEDIUM','LARGE','EXTRALARGE'), each=3, times=2)
                                                ,len=rep(c('SMALL','MEDIUM','LARGE'), times=4*2)
                                                ,loc=rep(c('DRY','WET'), each=12)
                                                ,stringsAsFactors=FALSE
                                                ) %>% 
                                      mutate(CLASS = sprintf("%s%sD%sL"
                                                            ,substr(loc,1,1)
                                                            ,ifelse(substr(diam,1,1)=='E', 'X', substr(diam,1,1))
                                                            ,substr(len,1,1)
                                                            )
                                            )
                          ,wCounts=NULL
                          ,wClassInfo=data.frame(diam=rep(c('SMALL','MEDIUM','LARGE','EXTRALARGE'), each=3, times=2)
                                                ,len=rep(c('SMALL','MEDIUM','LARGE'), times=4*2)
                                                ,loc=rep(c('DRY','WET'), each=12)
                                                ,stringsAsFactors=FALSE
                                                ) %>% 
                                      mutate(CLASS = sprintf("%s%sD%sL"
                                                            ,substr(loc,1,1)
                                                            ,ifelse(substr(diam,1,1)=='E', 'X', substr(diam,1,1))
                                                            ,substr(len,1,1)
                                                            )
                                            )

                          ,reachlength = NULL
                          ,meanBankfullWidth = NULL
                          ) {
################################################################################
# Function: 
# Title: Calculate NRSA Large Woody Debris Metrics
# Programmers: Marlys Cappert
#              Randy Hjort
#              Suzanne San Romani
#              Curt Seeliger
#              Tom Kincaid
# Date: December 31, 2009
# Description:
#   This function calculates the large woody debris portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires data frames containing the thalweg, channel geometry, bank
#   geometry, large woody debris, and stream verification form data files.
# Wadeable Metrics:
#   c1d, c1dm100, c1t, c1tm100, c1w, c1w_msq, c1wm100, c2d, c2dm100, c2t,
#   c2tm100, c2w, c2w_msq, c2wm100, c3d, c3dm100, c3t, c3tm100, c3w, c3w_msq,
#   c3wm100, c4d, c4dm100, c4t, c4tm100, c4w, c4w_msq, c4wm100, c5d, c5dm100,
#   c5t, c5tm100, c5w, c5w_msq, c5wm100, lgdiatot, lgdrydia, lgdrylen, lglentot,
#   lgwetdia, lgwetlen, lwddv33, lwddvcal, lwdtv33, lwdtvcal, lwdwv33, lwdwvcal,
#   mddiatot, mddrydia, mddrylen, mdlentot, mdwetdia, mdwetlen, nc, ns,
#   rchdldll, rchdldml, rchdldsl, rchdmdll, rchdmdml, rchdmdsl, rchdryt,
#   rchdsdll, rchdsdml, rchdsdsl, rchdxdll, rchdxdml, rchdxdsl, rchtldll,
#   rchtldml, rchtldsl, rchtmdll, rchtmdml, rchtmdsl, rchtsdll, rchtsdml,
#   rchtsdsl, rchtxdll, rchtxdml, rchtxdsl, rchwdt, rchwett, rchwldll, rchwldml,
#   rchwldsl, rchwmdll, rchwmdml, rchwmdsl, rchwsdll, rchwsdml, rchwsdsl,
#   rchwxdll, rchwxdml, rchwxdsl, shdrylen, shlentot, shwetlen, smdiatot,
#   smdrydia, smwetdia, v1d, v1dm100, v1t, v1tm100, v1w, v1w_msq, v1wm100, v2d,
#   v2dm100, v2t, v2tm100, v2w, v2w_msq, v2wm100, v3d, v3dm100, v3t, v3tm100,
#   v3w, v3w_msq, v3wm100, v4d, v4dm100, v4t, v4tm100, v4w, v4w_msq, v4wm100,
#   v5d, v5dm100, v5t, v5tm100, v5w, v5w_msq, v5wm100, xldiatot, xldrydia,
#   xlwetdia
# Boatable Metrics:
#   c1d, c1dm100, c1t, c1tm100, c1w, c1w_msq, c1wm100, c2d, c2dm100, c2t,
#   c2tm100, c2w, c2w_msq, c2wm100, c3d, c3dm100, c3t, c3tm100, c3w, c3w_msq,
#   c3wm100, c4d, c4dm100, c4t, c4tm100, c4w, c4w_msq, c4wm100, c5d, c5dm100,
#   c5t, c5tm100, c5w, c5w_msq, c5wm100, lgdiatot, lgdrydia, lgdrylen, lglentot,
#   lgwetdia, lgwetlen, lwddv33, lwddvcal, lwdtv33, lwdtvcal, lwdwv33, lwdwvcal,
#   mddiatot, mddrydia, mddrylen, mdlentot, mdwetdia, mdwetlen, nc, ns,
#   rchdldll, rchdldml, rchdldsl, rchdmdll, rchdmdml, rchdmdsl, rchdryt,
#   rchdsdll, rchdsdml, rchdsdsl, rchdxdll, rchdxdml, rchdxdsl, rchtldll,
#   rchtldml, rchtldsl, rchtmdll, rchtmdml, rchtmdsl, rchtsdll, rchtsdml,
#   rchtsdsl, rchtxdll, rchtxdml, rchtxdsl, rchwdt, rchwett, rchwldll, rchwldml,
#   rchwldsl, rchwmdll, rchwmdml, rchwmdsl, rchwsdll, rchwsdml, rchwsdsl,
#   rchwxdll, rchwxdml, rchwxdsl, shdrylen, shlentot, shwetlen, smdiatot,
#   smdrydia, smwetdia, v1d, v1dm100, v1t, v1tm100, v1w, v1w_msq, v1wm100, v2d,
#   v2dm100, v2t, v2tm100, v2w, v2w_msq, v2wm100, v3d, v3dm100, v3t, v3tm100,
#   v3w, v3w_msq, v3wm100, v4d, v4dm100, v4t, v4tm100, v4w, v4w_msq, v4wm100,
#   v5d, v5dm100, v5t, v5tm100, v5w, v5w_msq, v5wm100, xldiatot, xldrydia,
#   xlwetdia
# Function Revisions:
#   12/31/09 mrc: Created.
#   01/07/09 mrc: Finished writing the metric for wadeable/boatable.
#   01/21/10 mrc: Copied needed test files to \code\testfiles.
#   03/22/10 rch: Replaced upData with rename.  Modified some aggregate
#            functions to rename the first value.
#   04/06/10 ssr: Updated test for wadeable and boatable. 
#   06/04/10 cws: Using reachlen calculated in metsGeneral instead of value
#            reported by field crew.  Of the 2308 sites, there were 635 sites
#            with differences in the value of c1dm100, counts of (old-new)/old
#            broke out this way:
#            (-10,-5]     (-5,-2]     (-2,-1]   (-1,-0.5] (-0.5,-0.2]
#                   5           0           2           5           2
#            (-0.2,-0.1]  (-0.1,0.1]   (0.1,0.2]   (0.2,0.5]     (0.5,1]
#                      5         481           7          48          80
#            (1,2]
#                0
#   09/16/10 cws: RemovED hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   03/14/10 cws: xbkf_w value obtained from metsChannelMorphology rather than
#            calculating it anew from tblCHANNELGEOMETRY2. Previously this value
#            was NA when any of the bank widths were NA.  Modified unit test.
#   07/30/12 tmk: Removed use of ODBC data connection and replaced with data
#            input from csv files using a call to function read.csv.  Added
#            argument tbl to the function to identify names of the data files.
#            Added argument NRSAdir to the function to identify the directory
#            from which data files are read and to which the output metrics file
#            is written.
#   12/21/12 tmk: Modified data input to use data frames containing data files
#            rather than csv files.  Calculated general and channel morphology
#            metrics directly rather than reading metric calculation results
#            from a file.  Modified output to be a data frame rather than a csv
#            file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frames to
#            character variables.
#   12/10/15 cws Updated calling interface with new args, and then temporarily
#            munging those args back into old dataframes to avoid wading into this 
#            code.
#    2/11/15 cws Modifying calling interface based on local feedback. Condensing
#            class count arguments from 48 separate args to 2 data args plus 2
#            class code definition arguments, similar to nrsaChannelHabitat().
#            May expand those code definition arguments to include size 
#            information.
#    3/01/16 cws Documenting arguments in comments at top.  Removed old code.
#    6/28/17 cws R v 3.3.0 dplyr::mutate() now stops with an error if the 
#            dataframe has zero rows; under 3.1.0 it added a column to the zero 
#            row dataframe and went on.  This means the unit test no longer works.
#            The creation of the reachlen dataframe based on df2 was jiggered to
#            to handle this case, and now the unit test works.
#
# Arguments:
# bCounts       dataframe containing large woody debris class counts at each 
#               transect for boatable reaches , with the following columns:
#                   SITE        integer or character specifying the site visit
#                   TRANSECT    character value specifying the transect
#                               for which the value was recorded.
#                   CLASS       character value specifying the class name
#                               (one of 4 diameter classes, 3 length classes,
#                               and 2 location classes).  These class names are
#                               expected to be defined in the bClassInfo
#                               argument
#                   VALUE       character values specifying the influence 
#                               class
#
# bClassInfo    dataframe relating the class names used in bCounts::CLASS to
#               each combination of size and location classes.  Expected to have
#               the following columns and values in 24 rows:
#                   diam    character value specifying diameter class: SMALL
#                           MEDIUM, LARGE, EXTRALARGE, as defined in the NRSA
#                           field protocol manual
#                   len     character value specifying length class: SMALL
#                           MEDIUM, LARGE, as defined in the NRSA field protocol
#                           manual
#                   loc     character value specifying location class: DRY, WET
#                           as defined in the NRSA field protocol manual
#                   CLASS   character value associated with each combination of
#                           diameter, length and channel location, as it occurs
#                           in the bCounts argument.
#               Default value is for EPA NARS processing.
#
# wCounts       dataframe containing large woody debris class counts at each 
#               transect for wadeable reaches , with the following columns:
#                   SITE        integer or character specifying the site visit
#                   TRANSECT    character value specifying the transect
#                               for which the value was recorded.
#                   CLASS       character value specifying the class name
#                               (one of 4 diameter classes, 3 length classes,
#                               and 2 location classes).  These class names are
#                               expected to be defined in the wClassInfo
#                               argument
#                   VALUE       character values specifying the influence 
#                               class
#
# wClassInfo    dataframe relating the class names used in wCounts::CLASS to
#               each combination of size and location classes.  Expected to have
#               the following columns and values in 24 rows:
#                   diam    character value specifying diameter class: SMALL
#                           MEDIUM, LARGE, EXTRALARGE, as defined in the NRSA
#                           field protocol manual
#                   len     character value specifying length class: SMALL
#                           MEDIUM, LARGE, as defined in the NRSA field protocol
#                           manual
#                   loc     character value specifying location class: DRY, WET
#                           as defined in the NRSA field protocol manual
#                   CLASS   character value associated with each combination of
#                           diameter, length and channel location, as it occurs
#                           in the wCounts argument.
#               Default value is for EPA NARS processing.
#
# reachlength   dataframe containing reach lengths in meters for each site.
#               Expected to contain the following columns:
#                   SITE        integer or character specifying the site visit
#                   VALUE       numeric values
#
# meanBankfullWidth dataframe containing mean bank-full widths in meters for each
#                   site.  Expected to contain the following columns:
#                       SITE        integer or character specifying the site visit
#                       VALUE       numeric values
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

    intermediateMessage('Large woody debris mets', loc='start')

    # Standardize arguments and temporarily reassemble old args from the new ones.
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
        rc <- df %>% select(SITE,TRANSECT,VALUE) %>% 
              mutate(PARAMETER=pName, VALUE=as.numeric(VALUE))
        return(rc)
    }
    ifdfRecode <- function(df, ...) {
        if(is.null(...)) return(NULL)
        else if(all(is.na(...))) return(NULL)

        # Change user CLASS names into our classic PARAMETER names, after
        # deriving those PARAMETER names from the diameter, length and location
        # column values.
        args <- list(...)
        classInfo <- args[[1]]
        rc <- df %>% 
              select(SITE,TRANSECT,CLASS,VALUE) %>%
              merge(classInfo %>% 
                    mutate(PARAMETER = sprintf("%s%sD%sL"
                                              ,substr(loc,1,1)
                                              ,ifelse(substr(diam,1,1)=='E', 'X', substr(diam,1,1))
                                              ,substr(len,1,1)
                                              )
                          )
                   ,by='CLASS', all.x=TRUE
                   ) %>%
              select(SITE,TRANSECT,PARAMETER,VALUE)  
        return(rc)
    }

    boats <- absentAsNULL(bCounts, ifdfRecode, bClassInfo)
    wades <- absentAsNULL(wCounts, ifdfRecode, wClassInfo)

    df1 <- rbind(boats, wades)
    if(is.null(df1)) return (NULL)
    df2 <- reachlength %>% select(SITE, VALUE) %>% mutate(VALUE = as.numeric(VALUE))
    df3 <- meanBankfullWidth %>% select(SITE, VALUE) %>% mutate(VALUE = as.numeric(VALUE))
    protocols <- mutate(df1, PROTOCOL = ifelse(SITE %in% boats$SITE, 'BOATABLE', 'WADEABLE')) %>%
                 select(SITE, PROTOCOL) %>%
                 unique()
    # End of rebuilding old args from new ones...


# for some of the calulations, need variables from other files
# number of transects = numtran
# reachlen = reachlen  (reachlen is in THALWEG2)
# samplen = 20m * numtran
# xbkf_w = mean(BANKWID) calculated in channel morphology metrics, or from tblBANKGEOMETRY2
# nc, ns are the number of transects with non-missing wet or dry responses.  For each transect,
# 12 results are expected for the wet/dry parameters.  If 1 or more is missing, the entire transect
# is scored 0.  The result for each site is 0-numtran for nc/ns.  If there are 12 results for
# the wet/dry at each transect for the entire site nc/ns is 0, if they were all missing, nc/ns=numtran


#nc/ns

   lgWoodNcNs <- df1
   
   lgWoodNcNs$ncns <- 'wet'

   lgWoodNcNs$ncns <- ifelse(lgWoodNcNs$PARAMETER %in% c('DLDLL', 'DLDML','DLDSL', 'DMDLL', 'DMDML'
                                                       ,'DMDSL', 'DSDLL', 'DSDML', 'DSDSL', 'DXDLL'
                                                       ,'DXDML', 'DXDSL'), 'dry' 
                                                       , lgWoodNcNs$ncns) 

    lgWoodWet <- subset(lgWoodNcNs, ncns == 'wet')
    lgWoodDry <- subset (lgWoodNcNs, ncns == 'dry')

     lgWoodWet2<- aggregate(list('x'=lgWoodWet$ncns)
                       ,list('SITE'=lgWoodWet$SITE
                       ,"TRANSECT"=lgWoodWet$TRANSECT
                       
                       )
                       ,count
                       )      

    lgWoodWet2$nc <-  ifelse(lgWoodWet2$x >= 12, 1, 0)   
    
    lgWoodWet2<- aggregate(lgWoodWet2$nc
                       ,list('SITE'=lgWoodWet2$SITE
                       
                       )
                       ,count
                       )  

    lgWoodWet2$METRIC <- 'nc'
    lgWoodWet2 <- rename(lgWoodWet2,'x','VALUE')  

#    lgWoodWet2 <- lgWoodWet2[order(lgWoodWet2$SITE
#                    , lgWoodWet2$TRANSECT
#                    ),]
#    lgWoodWet2 <- first(lgWoodWet2, 'SITE', 'firstSITE')   



  lgWoodDry2<- aggregate(list('x'=lgWoodDry$ncns)
                       ,list('SITE'=lgWoodDry$SITE
                       ,"TRANSECT"=lgWoodDry$TRANSECT
                       
                       )
                       ,count
                       )      

    lgWoodDry2$nc <-  ifelse(lgWoodDry2$x >= 12, 1, 0)   
    
    lgWoodDry2<- aggregate(lgWoodDry2$nc
                       ,list('SITE'=lgWoodDry2$SITE
                       
                       )
                       ,count
                       )  
    lgWoodDry2$METRIC <- 'ns'
    lgWoodDry2 <- rename(lgWoodDry2, 'x','VALUE')                    
 
 #put together (append) the wet (nc) and the dry (ns)       
          
      ncns <-  rbind (lgWoodDry2, lgWoodWet2)  
 
     intermediateMessage('End of ncns', loc='end')                    


#reachlen
    if(nrow(df2)==0) {
        reachlen <- data.frame(SITE=1L, VALUE=1, METRIC='reachlen') %>% subset(FALSE)
    } else {
        reachlen <- mutate(df2, METRIC = 'reachlen')
    }
#   thal$TRANSECT <- NULL
#   thal$STATION <- NULL
#   thal$SAMPLE_TYPE <- NULL
#   thal$FLAG <- NULL
#   thal$UNITS <- NULL
   reachlen$VALUE <- as.numeric(reachlen$VALUE)
   
#   thal$PARAMETER <- ifelse(thal$PARAMETER == 'REACHLENGTH', 'reachlen' , thal$PARAMETER)
#   thal <- rename(thal, 'PARAMETER','METRIC')

    intermediateMessage('end of reachlen', loc='end')

# numtran - several aggregate steps will get us to the number of transects for each SITE

   lgWoodTran <- aggregate(df1$VALUE
                       ,list('SITE'=df1$SITE
                       ,"TRANSECT"=df1$TRANSECT
                       )
                       ,count
                       )      

     lgWoodTranNo <- aggregate( list('TRANSECT'=lgWoodTran$TRANSECT )
                      ,list ('SITE'=lgWoodTran$SITE
                        )
                      ,count
                      )

   lgWoodTranNo$METRIC <- 'numtran'

   lgWoodTranNo <- rename(lgWoodTranNo, 'TRANSECT','VALUE')
   
    intermediateMessage('end of lgWoodTranNo', loc='end')
   
# xbkf_w

  bankgeomean <- mutate(df3, METRIC = 'xbkf_w')
  
  #calculate sums of counts, that serve as the basis for nearly all other metrics
  ##  04MAY10 SSR adding code to make VALUE numeric
  df1$VALUE <- as.numeric(df1$VALUE)

  lgWoodSums <- aggregate(df1$VALUE
                         ,list('SITE'=df1$SITE
                              ,"PARAMETER"=df1$PARAMETER
                              )
                         ,sum, na.rm=TRUE
                         )

  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DLDLL', 'rchdldll' , lgWoodSums$PARAMETER)      
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DLDML', 'rchdldml' , lgWoodSums$PARAMETER)     
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DLDSL', 'rchdldsl' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DMDLL', 'rchdmdll' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DMDML', 'rchdmdml' , lgWoodSums$PARAMETER)  
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DMDSL', 'rchdmdsl' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DSDLL', 'rchdsdll' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DSDML', 'rchdsdml' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DSDSL', 'rchdsdsl' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DXDLL', 'rchdxdll' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DXDML', 'rchdxdml' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'DXDSL', 'rchdxdsl' , lgWoodSums$PARAMETER)   
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WLDLL', 'rchwldll' , lgWoodSums$PARAMETER)      
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WLDML', 'rchwldml' , lgWoodSums$PARAMETER)     
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WLDSL', 'rchwldsl' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WMDLL', 'rchwmdll' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WMDML', 'rchwmdml' , lgWoodSums$PARAMETER)  
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WMDSL', 'rchwmdsl' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WSDLL', 'rchwsdll' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WSDML', 'rchwsdml' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WSDSL', 'rchwsdsl' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WXDLL', 'rchwxdll' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WXDML', 'rchwxdml' , lgWoodSums$PARAMETER) 
  lgWoodSums$PARAMETER <- ifelse(lgWoodSums$PARAMETER == 'WXDSL', 'rchwxdsl' , lgWoodSums$PARAMETER)

  lgWoodSums <- rename(lgWoodSums, c('PARAMETER','x'),c('METRIC' , 'VALUE'))
  
  intermediateMessage('end of lgwoodsums', loc='end')
  
  # append these files with all the necessary columns
  # put all these pieces together
  tt <- rbind (lgWoodTranNo, reachlen, bankgeomean, lgWoodSums, ncns)
   
    
  # reshape this file to do some more sums, calculations
 
  mm <- reshape(tt, idvar='SITE', direction='wide', timevar='METRIC')
  names(mm) <- gsub('VALUE\\.', '', names(mm))                

# these sums need to be across TRANSECTS



#add in the protocols, so we can subset and do the calculations for each
  
  mm <- merge(mm, protocols, by='SITE', all.x=TRUE, all.y=FALSE)
  
  mmw <- subset(mm, PROTOCOL=='WADEABLE')
  
  mmw$rchdryt   <- mmw$rchdsdsl + mmw$rchdsdml + mmw$rchdsdll + mmw$rchdmdsl + mmw$rchdmdml +
                   mmw$rchdmdll + mmw$rchdldsl + mmw$rchdldml + mmw$rchdldll + mmw$rchdxdsl + 
                   mmw$rchdxdml + mmw$rchdxdll
                  
  mmw$rchwett   <- mmw$rchwsdsl + mmw$rchwsdml + mmw$rchwsdll + mmw$rchwmdsl + mmw$rchwmdml + 
                   mmw$rchwmdll + mmw$rchwldsl + mmw$rchwldml + mmw$rchwldll + mmw$rchwxdsl + 
                   mmw$rchwxdml + mmw$rchwxdll  
                 
  mmw$rchwdt    <- mmw$rchwett + mmw$rchdryt                             
  
  mmw$rchtldll  <- mmw$rchwldll + mmw$rchdldll
  
  mmw$rchtldml  <- mmw$rchwldml + mmw$rchdldml
  
  mmw$rchtldsl  <- mmw$rchwldsl + mmw$rchdldsl
  
  mmw$rchtmdll  <- mmw$rchwmdll + mmw$rchdmdll
                                           
  mmw$rchtmdml  <- mmw$rchwmdml + mmw$rchdmdml
                                           
  mmw$rchtmdsl  <- mmw$rchwmdsl + mmw$rchdmdsl
  
  mmw$rchtsdll  <- mmw$rchwsdll + mmw$rchdsdll
                                           
  mmw$rchtsdml  <- mmw$rchwsdml + mmw$rchdsdml
                                           
  mmw$rchtsdsl  <- mmw$rchwsdsl + mmw$rchdsdsl
  
  mmw$rchtxdll  <- mmw$rchwxdll + mmw$rchdxdll
                                           
  mmw$rchtxdml  <- mmw$rchwxdml + mmw$rchdxdml
                                           
  mmw$rchtxdsl  <- mmw$rchwxdsl + mmw$rchdxdsl
  
  mmw$c1d       <-  mmw$rchdldll + mmw$rchdldml + mmw$rchdldsl + mmw$rchdmdll + mmw$rchdmdml + 
                    mmw$rchdmdsl + mmw$rchdsdll + mmw$rchdsdml + mmw$rchdsdsl + mmw$rchdxdll + 
                    mmw$rchdxdml + mmw$rchdxdsl
                  
  mmw$c1w       <- mmw$rchwsdsl + mmw$rchwsdml + mmw$rchwsdll + mmw$rchwmdsl + mmw$rchwmdml + 
                   mmw$rchwmdll + mmw$rchwldsl + mmw$rchwldml + mmw$rchwldll + mmw$rchwxdsl + 
                   mmw$rchwxdml + mmw$rchwxdll 
                  
  mmw$c1t       <- mmw$c1d + mmw$c1w             
  
  mmw$c1dm100   <- (mmw$c1d/mmw$reachlen)*100   
   
  mmw$c1tm100   <- (mmw$c1t/mmw$reachlen)*100
  
  mmw$c1wm100   <- (mmw$c1w/mmw$reachlen)*100

  mmw$c1w_msq   <- mmw$c1w/(mmw$xbkf_w*mmw$reachlen)
  
  
                  
  mmw$c2d       <- mmw$rchdsdml + mmw$rchdsdll + mmw$rchdmdsl + mmw$rchdmdml + mmw$rchdmdll + 
                   mmw$rchdldsl + mmw$rchdldml + mmw$rchdldll + mmw$rchdxdsl + mmw$rchdxdml + mmw$rchdxdll
                  
  mmw$c2w       <- mmw$rchwsdml + mmw$rchwsdll + mmw$rchwmdsl + mmw$rchwmdml + mmw$rchwmdll + 
                   mmw$rchwldsl + mmw$rchwldml + mmw$rchwldll + mmw$rchwxdsl + mmw$rchwxdml + mmw$rchwxdll
                   
  mmw$c2t       <- mmw$c2d + mmw$c2w     
  
  mmw$c2dm100   <- (mmw$c2d/mmw$reachlen)*100   
   
  mmw$c2tm100   <- (mmw$c2t/mmw$reachlen)*100
  
  mmw$c2wm100   <- (mmw$c2w/mmw$reachlen)*100
  
  mmw$c2w_msq   <- mmw$c2w/(mmw$xbkf_w*mmw$reachlen)             
                   
  mmw$c3d       <- mmw$rchdsdll +  mmw$rchdmdml + mmw$rchdmdll + mmw$rchdldml +
                   mmw$rchdldll + 
                   mmw$rchdxdsl + mmw$rchdxdml + mmw$rchdxdll 
                  
  mmw$c3w       <-  mmw$rchwsdll + mmw$rchwmdml + mmw$rchwmdll + mmw$rchwldml +
                    mmw$rchwldll + 
                    mmw$rchwxdsl + mmw$rchwxdml + mmw$rchwxdll  
                  
  mmw$c3t       <- mmw$c3d + mmw$c3w          
  
  mmw$c3dm100   <- (mmw$c3d/mmw$reachlen)*100   
   
  mmw$c3tm100   <- (mmw$c3t/mmw$reachlen)*100
  
  mmw$c3wm100   <- (mmw$c3w/mmw$reachlen)*100
  
  mmw$c3w_msq   <- mmw$c3w/(mmw$xbkf_w*mmw$reachlen)       
                  
  mmw$c4d       <- mmw$rchdmdll + mmw$rchdldml + mmw$rchdldll + mmw$rchdxdml + mmw$rchdxdll
  
  mmw$c4w       <- mmw$rchwmdll + mmw$rchwldml + mmw$rchwldll + mmw$rchwxdml + mmw$rchwxdll
  
  mmw$c4t       <- mmw$c4d + mmw$c4w 
  
  mmw$c4dm100   <- (mmw$c4d/mmw$reachlen)*100   
   
  mmw$c4tm100   <- (mmw$c4t/mmw$reachlen)*100
  
  mmw$c4wm100   <- (mmw$c4w/mmw$reachlen)*100
  
  mmw$c4w_msq   <- mmw$c4w/(mmw$xbkf_w*mmw$reachlen)
  
  mmw$c5d       <- mmw$rchdxdll
  
  mmw$c5w       <- mmw$rchwxdll
  
  mmw$c5t       <- mmw$c5d + mmw$c5w 
  
  mmw$c5dm100   <- (mmw$c5d/mmw$reachlen)*100   
   
  mmw$c5tm100   <- (mmw$c5t/mmw$reachlen)*100
  
  mmw$c5wm100   <- (mmw$c5w/mmw$reachlen)*100
  
  mmw$c5w_msq   <- mmw$c5w/(mmw$xbkf_w*mmw$reachlen)
  
  mmw$mddrydia  <- mmw$rchdmdsl + mmw$rchdmdml + mmw$rchdmdll
  
  mmw$mddrylen  <- mmw$rchdsdml + mmw$rchdmdml + mmw$rchdldml + mmw$rchdxdml
  
  mmw$mdwetdia  <- mmw$rchwmdsl + mmw$rchwmdml + mmw$rchwmdll
  
  mmw$mdwetlen  <- mmw$rchwsdml + mmw$rchwmdml + mmw$rchwldml + mmw$rchwxdml
  
  mmw$mdlentot  <- mmw$mddrylen + mmw$mdwetlen
 
  mmw$mddiatot  <- mmw$mddrydia + mmw$mdwetdia
   
  mmw$lgdrydia  <- mmw$rchdldsl + mmw$rchdldml + mmw$rchdldll
  
  mmw$lgdrylen  <- mmw$rchdsdll + mmw$rchdmdll + mmw$rchdldll + mmw$rchdxdll
  
  mmw$lgwetdia  <- mmw$rchwldsl + mmw$rchwldml + mmw$rchwldll
  
  mmw$lgwetlen  <- mmw$rchwsdll + mmw$rchwmdll + mmw$rchwldll + mmw$rchwxdll
  
  mmw$lglentot  <- mmw$lgdrylen + mmw$lgwetlen
 
  mmw$lgdiatot  <- mmw$lgdrydia + mmw$lgwetdia
  
  mmw$lwddv33   <- ((mmw$rchdsdsl*0.058) + (mmw$rchdsdml*0.182) + (mmw$rchdsdll * 0.438) + 
                   (mmw$rchdmdsl*0.333) + (mmw$rchdmdml*1.042) + (mmw$rchdmdll*2.501) + 
                   (mmw$rchdldsl*0.932) + (mmw$rchdldml*2.911) + (mmw$rchdldll*6.988) + 
                   (mmw$rchdxdsl*3.016) + (mmw$rchdxdml*9.421) + (mmw$rchdxdll*22.619))
                 
  mmw$lwdwv33   <- ((mmw$rchwsdsl*0.058) + (mmw$rchwsdml*0.182) + (mmw$rchwsdll * 0.438) + 
                   (mmw$rchwmdsl*0.333) + (mmw$rchwmdml*1.042) + (mmw$rchwmdll*2.501) + 
                   (mmw$rchwldsl*0.932) + (mmw$rchwldml*2.911) + (mmw$rchwldll*6.988) + 
                   (mmw$rchwxdsl*3.016) + (mmw$rchwxdml*9.421) + (mmw$rchwxdll*22.619))
                 
  mmw$lwddvcal  <- ((mmw$rchdsdsl*0.0529) + (mmw$rchdsdml*0.1557) + (mmw$rchdsdll * 0.5126) +
                   (mmw$rchdmdsl*0.2626) + (mmw$rchdmdml*0.6194) + (mmw$rchdmdll*1.4555) + 
                   (mmw$rchdldsl*0.8076) + (mmw$rchdldml*1.8235) + (mmw$rchdldll*2.4662) + 
                   (mmw$rchdxdsl*10.2484) + (mmw$rchdxdml*10.9818) + (mmw$rchdxdll*27.7153))               
                 
  mmw$lwdwvcal  <- ((mmw$rchwsdsl*0.0529) + (mmw$rchwsdml*0.1557) + (mmw$rchwsdll * 0.5126) + 
                   (mmw$rchwmdsl*0.2626) + (mmw$rchwmdml*0.6194) + (mmw$rchwmdll*1.4555) + 
                   (mmw$rchwldsl*0.8076) + (mmw$rchwldml*1.8235) + (mmw$rchwldll*2.4662) + 
                   (mmw$rchwxdsl*10.2484) + (mmw$rchwxdml*10.9818) + (mmw$rchwxdll*27.7153))                     
                 
  mmw$lwdtv33   <- mmw$lwdwv33 + mmw$lwddv33               
  
  mmw$lwdtvcal  <- mmw$lwdwvcal + mmw$lwddvcal
  
  mmw$shdrylen  <- mmw$rchdsdsl + mmw$rchdmdsl + mmw$rchdldsl + mmw$rchdxdsl
  
  mmw$shwetlen  <- mmw$rchwsdsl + mmw$rchwmdsl + mmw$rchwldsl + mmw$rchwxdsl
  
  mmw$shlentot  <- mmw$shdrylen + mmw$shwetlen
  
  mmw$smdrydia  <- mmw$rchdsdsl + mmw$rchdsdml + mmw$rchdsdll
  
  mmw$smwetdia  <- mmw$rchwsdsl + mmw$rchwsdml + mmw$rchwsdll
  
  mmw$smdiatot  <- mmw$smdrydia + mmw$smwetdia
  
  mmw$v1d       <- ((mmw$rchdsdsl*0.058) + (mmw$rchdsdml*0.182) + (mmw$rchdsdll * 0.438) + 
                   (mmw$rchdmdsl*0.333) + (mmw$rchdmdml*1.042) + (mmw$rchdmdll*2.501) + 
                   (mmw$rchdldsl*0.932) + (mmw$rchdldml*2.911) + (mmw$rchdldll*6.988) + 
                   (mmw$rchdxdsl*3.016) + (mmw$rchdxdml*9.421) + (mmw$rchdxdll*22.62)) 
                 
  mmw$v1w       <- ((mmw$rchwsdsl*0.058) + (mmw$rchwsdml*0.182) + (mmw$rchwsdll * 0.438) + 
                   (mmw$rchwmdsl*0.333) + (mmw$rchwmdml*1.042) + (mmw$rchwmdll*2.501) + 
                   (mmw$rchwldsl*0.932) + (mmw$rchwldml*2.911) + (mmw$rchwldll*6.988) + 
                   (mmw$rchwxdsl*3.016) + (mmw$rchwxdml*9.421) + (mmw$rchwxdll*22.62)) 
                 
  mmw$v1t       <- mmw$v1d + mmw$v1w
  
  mmw$v1dm100   <- (mmw$v1d/mmw$reachlen)*100
   
  mmw$v1tm100   <- (mmw$v1t/mmw$reachlen)*100
  
  mmw$v1wm100   <- (mmw$v1w/mmw$reachlen)*100
  
  mmw$v1w_msq   <- mmw$v1w/(mmw$xbkf_w*mmw$reachlen)
 
  mmw$v2d       <- ((mmw$rchdsdml*0.182) + (mmw$rchdsdll * 0.438) + (mmw$rchdmdsl*0.333) +
                   (mmw$rchdmdml*1.042) + (mmw$rchdmdll*2.501) + (mmw$rchdldsl*0.932) + 
                   (mmw$rchdldml*2.911) + (mmw$rchdldll*6.988) + (mmw$rchdxdsl*3.016) + 
                   (mmw$rchdxdml*9.421) + (mmw$rchdxdll*22.62)) 
                 
  mmw$v2w       <- ((mmw$rchwsdml*0.182) + (mmw$rchwsdll * 0.438) + (mmw$rchwmdsl*0.333) +
                   (mmw$rchwmdml*1.042) + (mmw$rchwmdll*2.501) + (mmw$rchwldsl*0.932) + 
                   (mmw$rchwldml*2.911) + (mmw$rchwldll*6.988) + (mmw$rchwxdsl*3.016) + 
                   (mmw$rchwxdml*9.421) + (mmw$rchwxdll*22.62)) 
                 
  mmw$v2t       <- mmw$v2d +mmw$v2w
  
  mmw$v2dm100   <- (mmw$v2d/mmw$reachlen)*100   
   
  mmw$v2tm100   <- (mmw$v2t/mmw$reachlen)*100
  
  mmw$v2wm100   <- (mmw$v2w/mmw$reachlen)*100
  
  mmw$v2w_msq   <- mmw$v2w/(mmw$xbkf_w*mmw$reachlen)


  mmw$v3d       <- ((mmw$rchdsdll * 0.438) + 
                   (mmw$rchdmdml*1.042) + (mmw$rchdmdll*2.501) +  
                   (mmw$rchdldml*2.911) + (mmw$rchdldll*6.988) + (mmw$rchdxdsl*3.016) + 
                   (mmw$rchdxdml*9.421) + (mmw$rchdxdll*22.62))
                 
  mmw$v3w       <- ((mmw$rchwsdll * 0.438) + 
                   (mmw$rchwmdml*1.042) + (mmw$rchwmdll*2.501) + 
                   (mmw$rchwldml*2.911) + (mmw$rchwldll*6.988) + (mmw$rchwxdsl*3.016) + 
                   (mmw$rchwxdml*9.421) + (mmw$rchwxdll*22.62))  
                 
  mmw$v3t       <- mmw$v3d +mmw$v3w
  
  mmw$v3dm100   <- (mmw$v3d/mmw$reachlen)*100   
   
  mmw$v3tm100   <- (mmw$v3t/mmw$reachlen)*100
  
  mmw$v3wm100   <- (mmw$v3w/mmw$reachlen)*100
  
  mmw$v3w_msq   <- mmw$v3w/(mmw$xbkf_w*mmw$reachlen)
 
  mmw$v4d       <- ((mmw$rchdmdll*2.501) + (mmw$rchdldml*2.911) + 
                    (mmw$rchdldll*6.988) + (mmw$rchdxdml*9.421) + 
                    (mmw$rchdxdll*22.62))
                 
  mmw$v4w       <- ((mmw$rchwmdll*2.501) + (mmw$rchwldml*2.911) + 
                    (mmw$rchwldll*6.988) + (mmw$rchwxdml*9.421) + 
                    (mmw$rchwxdll*22.62))
                 
  mmw$v4t       <- mmw$v4d + mmw$v4w   
  
  mmw$v4dm100   <- (mmw$v4d/mmw$reachlen)*100   
   
  mmw$v4tm100   <- (mmw$v4t/mmw$reachlen)*100
  
  mmw$v4wm100   <- (mmw$v4w/mmw$reachlen)*100
  
  mmw$v4w_msq   <- mmw$v4w/(mmw$xbkf_w*mmw$reachlen)                            
 
  mmw$v5d       <- mmw$rchdxdll*22.62
                 
  mmw$v5w       <- mmw$rchwxdll*22.62
                 
  mmw$v5t       <- mmw$v5d +mmw$v5w  
  
  mmw$v5dm100   <- (mmw$v5d/mmw$reachlen)*100  
   
  mmw$v5tm100   <- (mmw$v5t/mmw$reachlen)*100
  
  mmw$v5wm100   <- (mmw$v5w/mmw$reachlen)*100
  
  mmw$v5w_msq   <- mmw$v5w/(mmw$xbkf_w*mmw$reachlen)
 
  mmw$xldrydia  <- mmw$rchdxdsl + mmw$rchdxdml + mmw$rchdxdll
  
  mmw$xlwetdia  <- mmw$rchwxdsl + mmw$rchwxdml + mmw$rchwxdll
  
  mmw$xldiatot  <- mmw$xldrydia + mmw$xlwetdia
  
  intermediateMessage('.w')
  #now do the calculations for the boatable protocol.
  
  mmb <- subset(mm, PROTOCOL=='BOATABLE')
  
  mmb$rchdryt   <- mmb$rchdsdsl + mmb$rchdsdml + mmb$rchdsdll + mmb$rchdmdsl + mmb$rchdmdml + 
                   mmb$rchdmdll + mmb$rchdldsl + mmb$rchdldml + mmb$rchdldll + mmb$rchdxdsl + 
                   mmb$rchdxdml + mmb$rchdxdll
                  
  mmb$rchwett   <- mmb$rchwsdsl + mmb$rchwsdml + mmb$rchwsdll + mmb$rchwmdsl + mmb$rchwmdml + 
                   mmb$rchwmdll + mmb$rchwldsl + mmb$rchwldml + mmb$rchwldll + mmb$rchwxdsl + 
                   mmb$rchwxdml + mmb$rchwxdll  
                 
  mmb$rchwdt    <- mmb$rchwett + mmb$rchdryt                             
  
  mmb$rchtldll  <- mmb$rchwldll + mmb$rchdldll
  
  mmb$rchtldml  <- mmb$rchwldml + mmb$rchdldml
  
  mmb$rchtldsl  <- mmb$rchwldsl + mmb$rchdldsl
  
  mmb$rchtmdll  <- mmb$rchwmdll + mmb$rchdmdll
                                           
  mmb$rchtmdml  <- mmb$rchwmdml + mmb$rchdmdml
                                           
  mmb$rchtmdsl  <- mmb$rchwmdsl + mmb$rchdmdsl
  
  mmb$rchtsdll  <- mmb$rchwsdll + mmb$rchdsdll
                                           
  mmb$rchtsdml  <- mmb$rchwsdml + mmb$rchdsdml
                                           
  mmb$rchtsdsl  <- mmb$rchwsdsl + mmb$rchdsdsl
  
  mmb$rchtxdll  <- mmb$rchwxdll + mmb$rchdxdll
                                           
  mmb$rchtxdml  <- mmb$rchwxdml + mmb$rchdxdml
                                           
  mmb$rchtxdsl  <- mmb$rchwxdsl + mmb$rchdxdsl
  
  mmb$c1d       <- mmb$rchdsdsl + mmb$rchdsdml + mmb$rchdsdll + mmb$rchdmdsl + mmb$rchdmdml + 
                   mmb$rchdmdll + mmb$rchdldsl + mmb$rchdldml + mmb$rchdldll + mmb$rchdxdsl + 
                   mmb$rchdxdml + mmb$rchdxdsl
                  
  mmb$c1w       <- mmb$rchwsdsl + mmb$rchwsdml + mmb$rchwsdll + mmb$rchwmdsl + mmb$rchwmdml + 
                   mmb$rchwmdll + mmb$rchwldsl + mmb$rchwldml + mmb$rchwldll + mmb$rchwxdsl + 
                   mmb$rchwxdml + mmb$rchwxdll 
                  
  mmb$c1t       <- mmb$c1d + mmb$c1w             
  
  mmb$c1dm100   <- (mmb$c1d/(mmb$numtran*20))*100   
   
  mmb$c1tm100   <- (mmb$c1t/(mmb$numtran*20))*100
  
  mmb$c1wm100   <- (mmb$c1w/(mmb$numtran*20))*100
  
  mmb$c1w_msq   <- mmb$c1w/(mmb$numtran*20*10)
  
  
                  
  mmb$c2d       <- mmb$rchdsdml + mmb$rchdsdll + mmb$rchdmdsl + mmb$rchdmdml + mmb$rchdmdll +                 
                   mmb$rchdldsl + mmb$rchdldml + mmb$rchdldll + mmb$rchdxdsl + mmb$rchdxdml + 
                   mmb$rchdxdsl
                  
  mmb$c2w       <- mmb$rchwsdml + mmb$rchwsdll + mmb$rchwmdsl + mmb$rchwmdml + mmb$rchwmdll +                 
                   mmb$rchwldsl + mmb$rchwldml + mmb$rchwldll + mmb$rchwxdsl + mmb$rchwxdml + 
                   mmb$rchwxdll
                   
  mmb$c2t       <- mmb$c2d + mmb$c2w     
  
  mmb$c2dm100   <- (mmb$c2d/(mmb$numtran*20))*100    
  mmb$c2tm100   <- (mmb$c2t/(mmb$numtran*20))*100
  mmb$c2wm100   <- (mmb$c2w/(mmb$numtran*20))*100
  
  mmb$c2w_msq   <- mmb$c2w/(mmb$numtran*20*10)             
                   
  mmb$c3d       <- mmb$rchdsdll +  mmb$rchdmdml + mmb$rchdmdll +   mmb$rchdldml +                              
                  mmb$rchdldll + mmb$rchdxdsl + mmb$rchdxdml + mmb$rchdxdll 
                  
  mmb$c3w       <-  mmb$rchwsdll + mmb$rchwmdml + mmb$rchwmdll +   mmb$rchwldml +                              
                    mmb$rchwldll + mmb$rchwxdsl + mmb$rchwxdml + mmb$rchwxdll  
                  
  mmb$c3t       <- mmb$c3d + mmb$c3w          
  
  mmb$c3dm100   <- (mmb$c3d/(mmb$numtran*20))*100   
  mmb$c3tm100   <- (mmb$c3t/(mmb$numtran*20))*100  
  mmb$c3wm100   <- (mmb$c3w/(mmb$numtran*20))*100
  
  mmb$c3w_msq   <- mmb$c3w/(mmb$numtran*20*10)       
                  
  mmb$c4d       <- mmb$rchdmdll + mmb$rchdldml + mmb$rchdldll + mmb$rchdxdml + mmb$rchdxdll
  
  mmb$c4w       <- mmb$rchwmdll + mmb$rchwldml + mmb$rchwldll + mmb$rchwxdml + mmb$rchwxdll
  
  mmb$c4t       <- mmb$c4d + mmb$c4w 
  
  mmb$c4dm100   <- (mmb$c4d/(mmb$numtran*20))*100   
  mmb$c4tm100   <- (mmb$c4t/(mmb$numtran*20))*100
  mmb$c4wm100   <- (mmb$c4w/(mmb$numtran*20))*100
  
  mmb$c4w_msq   <- mmb$c4w/(mmb$numtran*20*10)
  
  mmb$c5d       <- mmb$rchdxdll
  
  mmb$c5w       <- mmb$rchwxdll
  
  mmb$c5t       <- mmb$c5d + mmb$c5w 
  
  mmb$c5dm100   <- (mmb$c5d/(mmb$numtran*20))*100   
  mmb$c5tm100   <- (mmb$c5t/(mmb$numtran*20))*100
  mmb$c5wm100   <- (mmb$c5w/(mmb$numtran*20))*100
  
  mmb$c5w_msq   <- mmb$c5w/(mmb$numtran*20*10)
  
  mmb$mddrydia  <- mmb$rchdmdsl + mmb$rchdmdml + mmb$rchdmdll
  
  mmb$mddrylen  <- mmb$rchdsdml + mmb$rchdmdml + mmb$rchdldml + mmb$rchdxdml
  
  mmb$mdwetdia  <- mmb$rchwmdsl + mmb$rchwmdml + mmb$rchwmdll
  
  mmb$mdwetlen  <- mmb$rchwsdml + mmb$rchwmdml + mmb$rchwldml + mmb$rchwxdml
  
  mmb$mdlentot  <- mmb$mddrylen + mmb$mdwetlen
 
  mmb$mddiatot  <- mmb$mddrydia + mmb$mdwetdia
   
  mmb$lgdrydia  <- mmb$rchdldsl + mmb$rchdldml + mmb$rchdldll
  
  mmb$lgdrylen  <- mmb$rchdsdll + mmb$rchdmdll + mmb$rchdldll + mmb$rchdxdll
  
  mmb$lgwetdia  <- mmb$rchwldsl + mmb$rchwldml + mmb$rchwldll
  
  mmb$lgwetlen  <- mmb$rchwsdll + mmb$rchwmdll + mmb$rchwldll + mmb$rchwxdll
  
  mmb$lglentot  <- mmb$lgdrylen + mmb$lgwetlen
 
  mmb$lgdiatot  <- mmb$lgdrydia + mmb$lgwetdia
  
  mmb$lwddv33   <- ((mmb$rchdsdsl*1.042) + (mmb$rchdsdml*2.513) + (mmb$rchdsdll * 5.66) + 
                   (mmb$rchdmdsl*2.911) + (mmb$rchdmdml*6.988) + (mmb$rchdmdll*15.708) + 
                   (mmb$rchdldsl*4.916) + (mmb$rchdldml*11.798) + (mmb$rchdldll*26.546) + 
                   (mmb$rchdxdsl*11.636) + (mmb$rchdxdml*27.925) + (mmb$rchdxdll*62.832))
                 
  mmb$lwdwv33   <- ((mmb$rchwsdsl*1.0472) + (mmb$rchwsdml*2.513) + (mmb$rchwsdll*5.66) +
                    (mmb$rchwmdsl*2.911) + (mmb$rchwmdml*6.988) + (mmb$rchwmdll*15.708) + 
                    (mmb$rchwldsl*4.916) + (mmb$rchwldml*11.798) +(mmb$rchwldll*26.546) + 
                    (mmb$rchwxdsl*11.636) + (mmb$rchwxdml*27.925) + (mmb$rchwxdll*62.832))
                 
  mmb$lwddvcal  <- ((mmb$rchdsdsl*0.0529) + (mmb$rchdsdml*0.1557) + (mmb$rchdsdll * 0.5126) + 
                   (mmb$rchdmdsl*0.2626) + (mmb$rchdmdml*0.6194) + (mmb$rchdmdll*1.4555) + 
                   (mmb$rchdldsl*0.8076) + (mmb$rchdldml*1.8235) + (mmb$rchdldll*2.4662) + 
                   (mmb$rchdxdsl*10.2484) + (mmb$rchdxdml*10.9818) + (mmb$rchdxdll*27.7153))               
                 
  mmb$lwdwvcal  <- ((mmb$rchwsdsl*0.0529) + (mmb$rchwsdml*0.1557) + (mmb$rchwsdll * 0.5126) + 
                   (mmb$rchwmdsl*0.2626) + (mmb$rchwmdml*0.6194) + (mmb$rchwmdll*1.4555) + 
                   (mmb$rchwldsl*0.8076) + (mmb$rchwldml*1.8235) + (mmb$rchwldll*2.4662) + 
                   (mmb$rchwxdsl*10.2484) + (mmb$rchwxdml*10.9818) + (mmb$rchwxdll*27.7153))                     
                 
  mmb$lwdtv33   <- mmb$lwdwv33 + mmb$lwddv33               
  
  mmb$lwdtvcal  <- mmb$lwdwvcal + mmb$lwddvcal
  
  mmb$shdrylen  <- mmb$rchdsdsl + mmb$rchdmdsl + mmb$rchdldsl + mmb$rchdxdsl
  
  mmb$shwetlen  <- mmb$rchwsdsl + mmb$rchwmdsl + mmb$rchwldsl + mmb$rchwxdsl
  
  mmb$shlentot  <- mmb$shdrylen + mmb$shwetlen
  
  mmb$smdrydia  <- mmb$rchdsdsl + mmb$rchdsdml + mmb$rchdsdll
  
  mmb$smwetdia  <- mmb$rchwsdsl + mmb$rchwsdml + mmb$rchwsdll
  
  mmb$smdiatot  <- mmb$smdrydia + mmb$smwetdia
  
  mmb$v1d       <- ((mmb$rchdsdsl*1.047) + (mmb$rchdsdml*2.513) + (mmb$rchdsdll * 5.655) + 
                   (mmb$rchdmdsl*2.909) + (mmb$rchdmdml*6.981) + (mmb$rchdmdll*15.708) + 
                   (mmb$rchdldsl*4.916) + (mmb$rchdldml*11.798) + (mmb$rchdldll*26.546) + 
                   (mmb$rchdxdsl*11.636) + (mmb$rchdxdml*27.925) + (mmb$rchdxdll*62.832)) 
                 
  mmb$v1w       <- ((mmb$rchwsdsl*1.047)  +  (mmb$rchwsdml*2.513)  + (mmb$rchwsdll*5.655)  +
                    (mmb$rchwmdsl*2.909)  +  (mmb$rchwmdml*6.981)  + (mmb$rchwmdll*15.708) +
                    (mmb$rchwldsl*4.916)  +  (mmb$rchwldml*11.798) + (mmb$rchwldll*26.546) +
                    (mmb$rchwxdsl*11.636) +  (mmb$rchwxdml*27.925) + (mmb$rchwxdll*62.832)) 
                 
  mmb$v1t       <- mmb$v1d + mmb$v1w
  
  mmb$v1dm100   <- (mmb$v1d/(mmb$numtran*20))*100   
  mmb$v1tm100   <- (mmb$v1t/(mmb$numtran*20))*100
  mmb$v1wm100   <- (mmb$v1w/(mmb$numtran*20))*100
  
  mmb$v1w_msq   <- mmb$v1w/(mmb$numtran*20*10)
 
  mmb$v2d       <- ((mmb$rchdsdml*2.513) + (mmb$rchdsdll * 5.655) + (mmb$rchdmdsl*2.909) +
                   (mmb$rchdmdml*6.981) + (mmb$rchdmdll*15.708) + (mmb$rchdldsl*4.916) + 
                   (mmb$rchdldml*11.798) + (mmb$rchdldll*26.546) + (mmb$rchdxdsl*11.636) + 
                   (mmb$rchdxdml*27.925) + (mmb$rchdxdll*62.832))  
                 
  mmb$v2w       <- ((mmb$rchwsdml*2.513) + (mmb$rchwsdll * 5.655) + (mmb$rchwmdsl*2.909) +
                   (mmb$rchwmdml*6.981) + (mmb$rchwmdll*15.708) + (mmb$rchwldsl*4.916) + 
                   (mmb$rchwldml*11.798) + (mmb$rchwldll*26.546) + (mmb$rchwxdsl*11.636) + 
                   (mmb$rchwxdml*27.925) + (mmb$rchwxdll*62.832)) 
                 
  mmb$v2t       <- mmb$v2d +mmb$v2w
  
  mmb$v2dm100   <- (mmb$v2d/(mmb$numtran*20))*100   
  mmb$v2tm100   <- (mmb$v2t/(mmb$numtran*20))*100
  mmb$v2wm100   <- (mmb$v2w/(mmb$numtran*20))*100
  
  mmb$v2w_msq   <- mmb$v2w/(mmb$numtran*20*10)


  mmb$v3d       <- ((mmb$rchdsdll * 5.655)+ (mmb$rchdmdml*6.981) + (mmb$rchdmdll*15.708)  + 
                   (mmb$rchdldml*11.798) + (mmb$rchdldll*26.546) + (mmb$rchdxdsl*11.636) + 
                   (mmb$rchdxdml*27.925) + (mmb$rchdxdll*62.832)) 
                 
  mmb$v3w       <- ((mmb$rchwsdll * 5.655)+ (mmb$rchwmdml*6.981) + (mmb$rchwmdll*15.708)  + 
                   (mmb$rchwldml*11.798) + (mmb$rchwldll*26.546) + (mmb$rchwxdsl*11.636) + 
                   (mmb$rchwxdml*27.925) + (mmb$rchwxdll*62.832)) 
                 
  mmb$v3t       <- mmb$v3d +mmb$v3w
  
  mmb$v3dm100   <- (mmb$v3d/(mmb$numtran*20))*100   
  mmb$v3tm100   <- (mmb$v3t/(mmb$numtran*20))*100
  mmb$v3wm100   <- (mmb$v3w/(mmb$numtran*20))*100
  
  mmb$v3w_msq   <- mmb$v3w/(mmb$numtran*20*10)
 
  mmb$v4d       <- ((mmb$rchdmdll*15.708)  + (mmb$rchdldml*11.798)  +
                   (mmb$rchdldll*26.546) + (mmb$rchdxdml*27.925) + (mmb$rchdxdll*62.832)) 
                 
  mmb$v4w       <- ((mmb$rchwmdll*15.708)  + (mmb$rchwldml*11.798) +
                   (mmb$rchwldll*26.546) + (mmb$rchwxdml*27.925) + (mmb$rchwxdll*62.832)) 
                 
  mmb$v4t       <- mmb$v4d +mmb$v4w   
  
  mmb$v4dm100   <- (mmb$v4d/(mmb$numtran*20))*100   
  mmb$v4tm100   <- (mmb$v4t/(mmb$numtran*20))*100
  mmb$v4wm100   <- (mmb$v4w/(mmb$numtran*20))*100
  
  mmb$v4w_msq   <- mmb$v4w/(mmb$numtran*20*10)                            
 
  mmb$v5d       <- mmb$rchdxdll*62.832
                 
  mmb$v5w       <- mmb$rchwxdll*62.832
                 
  mmb$v5t       <- mmb$v5d +mmb$v5w  
  
  mmb$v5dm100   <- (mmb$v5d/(mmb$numtran*20))*100   
  mmb$v5tm100   <- (mmb$v5t/(mmb$numtran*20))*100
  mmb$v5wm100   <- (mmb$v5w/(mmb$numtran*20))*100
  
  mmb$v5w_msq   <- mmb$v5w/(mmb$numtran*20*10)
 
  mmb$xldrydia  <- mmb$rchdxdsl + mmb$rchdxdml + mmb$rchdxdll
  
  mmb$xlwetdia  <- mmb$rchwxdsl + mmb$rchwxdml + mmb$rchwxdll
  
  mmb$xldiatot  <- mmb$xldrydia + mmb$xlwetdia
 
  intermediateMessage('.b')
 #calculations are complete, put these back together and reshape the files
 
 
  xx <- rbind (mmw, mmb)
  xx$numtran <-NULL
  xx$xbkf_w <-NULL
  xx$reachlen <- NULL
  xx$PROTOCOL <- NULL
 
    intermediateMessage('end of mm', loc='end')
 # Transpose wide to long format

#   metsfakeo <- reshape(xx, idvar=c('SITE'), direction='long'
#                 ,varying=names(xx)[names(xx) != 'SITE']
#                 ,times=names(xx)[names(xx) != 'SITE']
#                 ,v.names='VALUE', timevar='METRIC'
#    #             ,drop=c('numtran','xbkf_w','reachlen','protocol'
#    #                   )
#                 )
    metsfakeo <- melt(xx, 'SITE', variable.name='METRIC', value.name='VALUE') %>% mutate(METRIC = as.character(METRIC))

  row.names(metsfakeo)<-NULL
  
  intermediateMessage('  Done.', loc='end')
  return(metsfakeo)
}



# end of file
