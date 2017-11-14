#' @export
#' @title Calculate NRSA Channel Habitat Metrics
#' @description This function calculates the channel habitat 
#' portion of the physical habitat metrics for National Rivers 
#' and Streams Assessment (NRSA) data.  The function requires a 
#' data frame containing the thalweg data file.
#' @param bChannelUnit  A data frame containing channel unit codes from 
#' boatable reaches, with the following columns:
#' \itemize{
#' \item SITE    integer or character specifying the site visit
#' \item VALUE   character values, expected to be as specified in the 
#' bChannelUnitCodeList argument column 'code'. Only values which occur 
#' in the code column will be included in the calculations.
#' }
#' @param bChannelUnitCodeList A data frame specifying the boatable 
#' channel unit codes, meanings and class memberships.  The default value 
#' of this argument is for the EPA NARS processing.  It should 
#' have the following columns:
#' \itemize{
#'  \item code  character values for the channel units as they occur in the 
#'  VALUE column of bChannelUnit
#'  \item metsSuffix  character values which are used as the suffix for 
#'  resulting metric names, e.g., a value of 'falls' results in 'pct_falls' 
#'  as the resulting metric name.
#'  \item category    character values that are used soley to
#'  help the human reader organize things. This column is not used by the code.
#'  \item isFast logical values for each habitat code specifying whether it 
#'  is considered fast or not.  Used to calculate pct_fast and  
#'  pct_slow; if this column does not exist, neither of those metrics will be
#'  calculated. Codes for which this value is NA will not be considered as 
#'  either fast nor slow.
#'  \item isPool logical values for each habitat code specifying whether it 
#'  is considered a pool habitat or not.  Used to calculate 
#'  pct_pool; if this column does not exist, this metric will not be 
#'  calculated.
#' }
#' @param wChannelUnit A data frame containing channel unit codes from wadeable 
#' reaches, with the following columns:
#' \itemize{
#'                  \item SITE    integer or character specifying the site 
#'                                 visit
#'                  \item VALUE   character values, expected to be as specified
#'                                in the wChannelUnitCodeList argument column 
#'                                'code'. Only values which occur in the code
#'                                column will be included in the calculations.
#' }
#' @param wChannelUnitCodeList  A data frame specifying the wadeable 
#' channel unit codes, meanings and class memberships.  The default 
#' value of this argument is for the EPA NARS processing for 2008-9.  
#' The updated version for 2013-14 would be identical to the values used
#' in \emph{bChannelUnitCodeList} above. The data frame input should  
#' have the following columns:
#' \itemize{
#'    \item code        character values for the channel units
#'                      as they occur in the VALUE column of
#'                      bChannelUnit
#'    \item metsSuffix  character values which are used as the
#'                      suffix for resulting metric names, e.g.
#'                      a value of 'falls' results in 'pct_falls'
#'                      as the resulting metric name.
#'    \item category    character values that are used soley to
#'                      help the human reader organize things.
#'                      This column is not used by the code.
#'    \item isFast      logical values for each habitat code
#'                      specifying whether it is considered fast
#'                      or not.  Used to calculate pct_fast and
#'                      pct_slow; if this column does not exist,
#'                      neither of those metrics will be 
#'                      calculated. Codes for which this value
#'                      is NA will not be considered as either
#'                      fast nor slow.
#'    \item isPool      logical values for each habitat code
#'                      specifying whether it is considered a
#'                      pool habitat or not.  Used to calculate
#'                      pct_pool; if this column does not exist,
#'                      this metric will not be calculated.
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
#'  Metrics calculated include: pct_fa, pct_ca, pct_ra, pct_ri, pct_gl,  
#'  pct_fast, pct_slow, pct_pool, pct_pp, pct_pd, pct_pb, pct_pt, pct_pl,
#'  pct_p, pct_dr  
#'  
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' 
#' @examples
#' head(thalwegEx) 
#' 
#' bChan <- subset(thalwegEx,PARAMETER=='CHANUNCD' & SAMPLE_TYPE=='PHAB_THAL')
#' wChan <- subset(thalwegEx,PARAMETER=='CHANUNCD' & SAMPLE_TYPE=='PHAB_THALW')
#' 
#' chanHabOut <- nrsaChannelHabitat(bChannelUnit=bChan, wChannelUnit=wChan)
#' head(chanHabOut)


nrsaChannelHabitat <- function(bChannelUnit = NULL
                              ,bChannelUnitCodeList = data.frame(code=c('FA','RA','RI','GL','PO','CA','DR')
                                                                ,metsSuffix=c('fa','ra','ri','gl','pool','ca','dr')
                                                                ,category=c('FALLS','RAPID','RIFFLE','GLIDE','POOL','CASCADE','DRY')
                                                                ,isFast=c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,NA)
                                                                #,isPool=c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)
                                                                ,stringsAsFactors=FALSE
                                                                )
                              ,wChannelUnit = NULL
                              # NOTE: wChannelUnitCodeList in 1314 is identical to bChannelUnitCodeList
                              ,wChannelUnitCodeList = data.frame(code=c('FA','CA','RA','RI','GL'
                                                                       ,'PB','PP','PD','PL','PT'
                                                                       ,'P','DR'#,'SB'
                                                                       )
                                                                ,metsSuffix=c('fa','ca','ra','ri','gl'
                                                                             ,'pb','pp','pd','pl','pt'
                                                                             ,'p','dr'#,'sb'
                                                                             )
                                                                ,category=c('FALLS','CASCADE','RAPID','RIFFLE','GLIDE'
                                                                           ,'POOL_B','POOL_PLUNGE','POOL_D','POOL_LATERAL','POOL_TRENCH'
                                                                           ,'POOL','DRY'#,'SUBMERGED'
                                                                           )
                                                                ,isFast=c(TRUE,TRUE,TRUE,TRUE,FALSE
                                                                         ,FALSE,FALSE,FALSE,FALSE,FALSE
                                                                         ,FALSE,NA#,FALSE
                                                                         )
                                                                ,isPool=c(FALSE,FALSE,FALSE,FALSE,FALSE
                                                                         ,TRUE,TRUE,TRUE,TRUE,TRUE
                                                                         ,TRUE,FALSE#,FALSE
                                                                         )
                                                                ,stringsAsFactors=FALSE
                                                                )
                              ) {

################################################################################
# Function: nrsaChannelHabitat
# Title: Calculate NRSA Channel Habitat Metrics
# Programmers: Randy Hjort
#              Curt Seeliger
#              Suzanne San Romani
#              Tom Kincaid
# Date: January 4, 2010
# Description:
#   This function calculates the channel habitat portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires a data frame containing the thalweg data file.
# Function Revisions:
#   01/04/10 rch: Copied, plagerized and made up this code.
#   02/18/10 cws: Removed source() of NRSAValidation.r, NA_filler.r and
#            summaryby.r.
#   03/23/10 ssr: Moved creation of unit test dataframes to separate functions.
#   06/03/10 cws: Removed pct_sb from calculations.  Is not legal channel unit
#            code, and does not occur in the 2008-2009 field data, and it
#            collides with the substrate metric pct_sb.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   11/02/10 cws: Modified to handle single-protocol datasets without choking.
#            Also re-prettyprinting.  Modified unit test to test single-protocol
#            calculations.
#   08/03/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify name of the data file.  Added argument NRSAdir to the
#            function to identify the directory from which the data file is read
#            and to which the output metrics file is written.
#   12/20/12 tmk: Modified data input to use a data frame containing the data
#            file rather than a csv file.  Modified output to be a data frame
#            rather than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frame to
#            character variables.
#   11/18/15 cws Updated calling interface.  Changed UID and RESULT to SITE and 
#            VALUE.
#    1/07/15 cws Changing default value of isFast for DR from FALSE to NA, and
#            changing PCT_SLOW calculations from including channel codes that
#            are isFast != TRUE, to include codes that are isFast = FALSE.  Dry
#            sections are neither fast nor slow.  No change to unit test as it 
#            passes.  Deleted section of old code at the bottom.
#    2/23/16 cws Updated argument descriptions, and cleaned up comments a tad.
#
# TODO: Update unit test to see whether DR is handled properly
#
# ARGUMENTS:
# bChannelUnit          dataframe containing channel unit codes from boatable
#                       reaches, with the following columns:
#                           SITE    integer or character specifying the site 
#                                   visit
#                           VALUE   character values, expected to be as specified
#                                   in the bChannelUnitCodeList argument column 
#                                   'code'. Only values which occur in the code
#                                   column will be included in the calculations.
#
# bChannelUnitCodeList  dataframe specifying the boatable channel unit codes, 
#                       meanings and class memberships.  The default value of 
#                       this argument is for the EPA NARS processing.  It should 
#                       have the following columns:
#                           code        character values for the channel units
#                                       as they occur in the VALUE column of
#                                       bChannelUnit
#                           metsSuffix  character values which are used as the
#                                       suffix for resulting metric names, e.g.
#                                       a value of 'falls' results in 'pct_falls'
#                                       as the resulting metric name.
#                           category    character values that are used soley to
#                                       help the human reader organize things.
#                                       This column is not used by the code.
#                           isFast      logical values for each habitat code
#                                       specifying whether it is considered fast
#                                       or not.  Used to calculate pct_fast and
#                                       pct_slow; if this column does not exist,
#                                       neither of those metrics will be 
#                                       calculated. Codes for which this value
#                                       is NA will not be considered as either
#                                       fast nor slow.
#                           isPool      logical values for each habitat code
#                                       specifying whether it is considered a
#                                       pool habitat or not.  Used to calculate
#                                       pct_pool; if this column does not exist,
#                                       this metric will not be calculated.
#
# wChannelUnit          dataframe containing channel unit codes from wadeable
#                       reaches, with the following columns:
#                           SITE    integer or character specifying the site 
#                                   visit
#                           VALUE   character values, expected to be as specified
#                                   in the wChannelUnitCodeList argument column 
#                                   'code'. Only values which occur in the code
#                                   column will be included in the calculations.
#
# wChannelUnitCodeList  dataframe specifying the wadeable channel unit codes, 
#                       meanings and class memberships.  The default value of 
#                       this argument is for the EPA NARS processing.  It should 
#                       have the following columns:
#                           code        character values for the channel units
#                                       as they occur in the VALUE column of
#                                       bChannelUnit
#                           metsSuffix  character values which are used as the
#                                       suffix for resulting metric names, e.g.
#                                       a value of 'falls' results in 'pct_falls'
#                                       as the resulting metric name.
#                           category    character values that are used soley to
#                                       help the human reader organize things.
#                                       This column is not used by the code.
#                           isFast      logical values for each habitat code
#                                       specifying whether it is considered fast
#                                       or not.  Used to calculate pct_fast and
#                                       pct_slow; if this column does not exist,
#                                       neither of those metrics will be 
#                                       calculated. Codes for which this value
#                                       is NA will not be considered as either
#                                       fast nor slow.
#                           isPool      logical values for each habitat code
#                                       specifying whether it is considered a
#                                       pool habitat or not.  Used to calculate
#                                       pct_pool; if this column does not exist,
#                                       this metric will not be calculated.
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
#   metsChannelHabitat.1 - calculate metrics
################################################################################

    intermediateMessage('Channel Habitat mets', loc='start')

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
        else return(df %>% select(SITE,VALUE) %>% subset(VALUE %in% ...))
    }

    bChannelUnit = absentAsNULL(bChannelUnit, ifdf, bChannelUnitCodeList$code)
    wChannelUnit = absentAsNULL(wChannelUnit, ifdf, wChannelUnitCodeList$code)

    intermediateMessage('.1')

    individualPercents <- function(cdData, codeList) {
        theseMets <- NULL
        for (i in 1:nrow(codeList)) {
            tgl <- summaryby(cdData,'count',sprintf("pct_%s", codeList$metsSuffix[i]))
            tyout<-aggregate(list(typesum=cdData$VALUE),list(SITE=cdData$SITE),function(x){sum(x==codeList$code[i],na.rm=TRUE)})
            tgl<-merge(tgl,tyout,by='SITE',all.x=TRUE)
            tgl$VALUE <- (tgl$typesum/tgl$VALUE)*100
            tgl<-tgl[c('SITE','METRIC','VALUE')]
            
            theseMets <- rbind(theseMets, tgl)
            intermediateMessage(sprintf('.%s', codeList$code[i]))
        }
        return(theseMets)
    }
    
    if(is.null(bChannelUnit)) {
        bmets <- NULL
    } else  {
        # go through channel unit data for individual categories
        bmets <- individualPercents(bChannelUnit, bChannelUnitCodeList)
        intermediateMessage('.2')
        
        # Calculate summary sums
        if(is.null(bChannelUnitCodeList$isFast)) {
            pct_fastBoatable <- NULL
            pct_SlowBoatable <- NULL
        } else {
            pct_fastBoatable <- bChannelUnit %>% 
                                mutate(isfast=ifelse(VALUE %in% subset(bChannelUnitCodeList, isFast)$code, 1, 0)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(isfast, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_fast')
            pct_SlowBoatable <- bChannelUnit %>% 
                                mutate(isslow=ifelse(VALUE %in% subset(bChannelUnitCodeList, isFast)$code, 0, 1)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(isslow, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_slow')
            intermediateMessage('.3')
        }
            
        if(is.null(bChannelUnitCodeList$isPool)) {
            pct_poolBoatable <- NULL
        } else {
            pct_poolBoatable <- bChannelUnit %>% 
                                mutate(ispool=ifelse(VALUE %in% subset(bChannelUnitCodeList, isPool)$code, 1, 0)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(ispool, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_pool')
            intermediateMessage('.4')
        }
        
        bmets <- rbind(bmets,pct_fastBoatable,pct_SlowBoatable,pct_poolBoatable)
        intermediateMessage('.5')
    }

    if(is.null(wChannelUnit)) {
        wmets <- NULL
    } else  {
        # go through channel unit data for individual categories
        wmets <- individualPercents(wChannelUnit, wChannelUnitCodeList)
        intermediateMessage('.6')

        # Calculate summary sums
        if(is.null(wChannelUnitCodeList$isFast)) {
            pct_fastWadeable <- NULL
            pct_slowWadeable <- NULL
        } else {
            pct_fastWadeable <- wChannelUnit %>% 
                                mutate(isfast=ifelse(VALUE %in% subset(wChannelUnitCodeList, isFast)$code, 1, 0)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(isfast, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_fast')
            pct_slowWadeable <- wChannelUnit %>% 
                                mutate(isslow=ifelse(VALUE %in% subset(wChannelUnitCodeList, !isFast)$code, 1, 0)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(isslow, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_slow')
            intermediateMessage('.7')
        }
                
        if(is.null(wChannelUnitCodeList$isPool)) {
            pct_poolWadeable <- NULL
        } else {
            pct_poolWadeable <- wChannelUnit %>% 
                                mutate(ispool=ifelse(VALUE %in% subset(wChannelUnitCodeList, isPool)$code, 1, 0)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(ispool, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_pool')
            intermediateMessage('.8')
        }
        
        wmets <- rbind(wmets,pct_slowWadeable,pct_fastWadeable,pct_poolWadeable)
        intermediateMessage('.9')
    }

    mets <- rbind(bmets, wmets)

    intermediateMessage('.Done', loc='end')
    return(mets)
}

# end of file
