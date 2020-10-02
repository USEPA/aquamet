#' @export
#' @title Calculate NRSA Littoral Depth Metrics
#' @description This function calculates the littoral depth 
#' portion of the physical habitat metrics for National Rivers 
#' and Streams Assessment (NRSA) data.  The function requires a 
#' data frame containing the channel depth data file.
#' @param bLittoralDepth A data frame containing littoral depth 
#' data in meters at each transect for all reaches, with the 
#' following columns:
#' \itemize{
#'  \item SITE integer or character specifying the site visit
#'  \item VALUE numeric or character values
#' }
#' Note that possible values for variables in the input data 
#' frame are provided in the document named "NRSA Documentation.pdf" 
#' included in the help directory for the package.
#' @param isUnitTest Logical argument to determine whether errors should be ignored.
#' Should only be used for running a unit test. Default value is FALSE.
#' @return Either a data frame when metric calculation is successful 
#' or a character string containing an error message when metric 
#' calculation is not successful.  The data frame contains the following 
#' columns:
#' \itemize{
#'    \item SITE - universal ID value
#'    \item METRIC - metric name
#'    \item VALUE - metric value
#' }
#' Metrics calculated (only for boatable sites) include: xlit, mxlit, 
#' mnlit, vlit. 
#' 
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' @examples 
#' head(chandepthEx) 
#' 
#' # Subset channel depth example dataset to create input, keeping only SITE
#'   #  and VALUE. This only applies to boatable sites.
#' bLitDep <- subset(chandepthEx, SAMPLE_TYPE=='PHAB_CHANBFRONT' & 
#'   PARAMETER %in% c('POLE','SONAR'), select = c(SITE,VALUE))
#'   # Ensure VALUE is numeric.
#'   bLitDep$VALUE <- as.numeric(bLitDep$VALUE)
#' 
#' litDepOut <- nrsaLittoralDepth(bLitDep)
#' print(litDepOut)
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}

nrsaLittoralDepth <- function(bLittoralDepth=NULL, isUnitTest=FALSE) {

################################################################################
# Function: nrsaLittoralDepth
# Title: Calculate NRSA Littoral Depth Metrics
# Programmers: Randy Hjort
#              Curt Seeliger
#              Tom Kincaid
# Date: January 25, 2010
# Description:
#   This function calculates the littoral depth portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires a data frame containing the channel depth data file.
# Wadeable Metrics:
#   None
# Boatable Metrics:
#   xlit mxlit mnlit vlit
# Function Revisions:
#   01/25/10 rch: Copied, plagerized and made up this code.
#   02/18/10 cws: Removed source() of NRSAValidation.r and summaryby.r.
#   06/01/10 cws: Removed odd code that somehow showed up here.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   11/04/10 cws: Handles missing littoral file (ie when study has no boatable
#            sites) with some grace.
#   11/18/10 cws: Added require(Hmisc) for %nin% operator.
#   07/27/12 tmk: Removed calls to the require() function.  Removed use of ODBC
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
#    2/22/16 cws: Rewritten with new calling interface.  Removed use of summaryby
#            function
#    3/16/16 cws Documenting arguments in comments at top.  Removed old UID 
#            column name from documentation
#    3/13/19 cws Changed to use aquametStandardizeArgument() instead of 
#            absentAsNull().
#   10/02/20 cws Removing reshape2 functions in favour of tidyr due to deprecation.
#
# Arguments:
# bLittoralDepth    dataframe containing littoral depth data in meters at each
#                   transect for all reaches, with the following columns:
#                   SITE        integer or character specifying the site visit
#                   VALUE       numeric or character values
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

  intermediateMessage('Littoral Depth mets', loc='start')

    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdf <- function(df, ...) {
        rc <- df %>% 
              select(SITE, VALUE) %>%
              mutate(VALUE = as.numeric(VALUE))
        return(rc)
    }

    depths <- aquametStandardizeArgument(bLittoralDepth, ifdf=ifdf
                                        ,struct=list(SITE=c('character','integer'), VALUE='double')
                                        ,rangeLimits=list(VALUE = c(0,5))
                                        )
    if(is.null(depths)) return (NULL)

    intermediateMessage('.1')

#     mdx <- summaryby(depths, 'mean', "xlit")
#     mds <- summaryby(depths, 'sd', "vlit")
#     mdm <- summaryby(depths, 'max', "mxlit")
#     mdn <- summaryby(depths, 'min', "mnlit")
# 
#     intermediateMessage('.2')
#     mets <- rbind(mdx,mds,mdm,mdn)
    mets <- depths %>%
            group_by(SITE) %>%
            dplyr::summarise(xlit = protectedMean(VALUE, na.rm=TRUE)
                            ,vlit = sd(VALUE, na.rm=TRUE)
                            ,mxlit = max(VALUE, na.rm=TRUE)
                            ,mnlit = min(VALUE, na.rm=TRUE)
                            ) %>%
            # reshape2::melt('SITE', variable.name='METRIC', value.name='VALUE')
            tidyr::gather(key=METRIC, value=VALUE, -SITE)
        
    intermediateMessage('.2')

    intermediateMessage('. Done.', loc='end')

    return(mets)
 
}

# end of file