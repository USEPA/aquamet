#' @export
#' @title Calculate NLA Fish Cover Metrics
#' @description This function calculates the fish cover portion of the physical
#' habitat metrics for National Lakes Assessment (NLA) data.  
#' @param aquatic A data frame containing aquatic and inundated herbaceous 
#' vegetation fish cover class values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the aquatic and inundated herbaceous
#' vegetation fish cover category.
#' }
#' @param aquatic_dd A data frame containing aquatic and inundated 
#' herbaceous vegetation fish cover class values for drawdown zone, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the aquatic and inundated herbaceous
#' vegetation fish cover category.
#' }
#' @param boulders A data frame containing boulder 
#' fish cover class values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the boulder fish cover category.
#' }
#' @param boulders_dd A data frame containing boulder 
#' fish cover class values for drawdown zone, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the boulder fish cover category.
#' }
#' @param brush A data frame containing woody brush and small woody debris 
#' <0.3 m diameter fish cover class values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the woody brush and small woody debris 
#' <0.3 m diameter fish cover category.
#' }
#' @param brush_dd A data frame containing woody brush and small woody debris 
#' <0.3 m diameter fish cover class values for drawdown zone, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the woody brush and small woody debris 
#' <0.3 m diameter fish cover category.
#' }
#' @param ledges A data frame containing ledges or sharp dropoff 
#' fish cover class values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the ledges or sharp dropoff fish cover category.
#' }
#' @param ledges_dd A data frame containing ledges or sharp dropoff 
#' fish cover class values for drawdown zone, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the ledges or sharp dropoff fish cover category.
#' }
#' @param livetrees A data frame containing inundated live trees 
#' fish cover class values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the inundated live trees fish cover category.
#' }
#' @param livetrees_dd A data frame containing inundated live trees 
#' fish cover class values for drawdown zone, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the inundated live trees fish cover category.
#' }
#' @param overhang A data frame containing overhanging vegetation 
#' fish cover class values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the overhanging vegetation fish cover category.
#' }
#' @param overhang_dd A data frame containing overhanging vegetation 
#' fish cover class values for drawdown zone, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the overhanging vegetation fish cover category.
#' }
#' @param snags A data frame containing woody debris and snags >0.3m diameter 
#' fish cover class values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the woody debris and snags fish cover category.
#' }
#' @param snags_dd A data frame containing woody debris and snags >0.3m diameter
#' fish cover class values for drawdown zone, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the woody debris and snags fish cover category.
#' }
#' @param structures A data frame containing human structure 
#' fish cover class values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the human structure fish cover category.
#' }
#' @param structures_dd A data frame containing human structure
#' fish cover class values for drawdown zone, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, from 0-4 containing the human structure fish cover category.
#' }
#' @param drawdown A data frame indicating presence of drawdown at station, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, indicating drawdown exists at a site.
#' }
#' @param horizontalDistance_dd A data frame containing the horizontal distance 
#' to the high water mark where drawdown exists, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, indicating the horizontal distance to the high water mark when
#' drawdown exists at a site.
#' }
#' @param createSyntheticCovers A logical value which specifies whether to create
#'  synthetic cover values as proportions of drawdown and riparian cover.
#'  This argument should be set to FALSE when the data follows the 2007 NLA
#'  protocol or do not contain drawdown cover data.  The default value is
#'  TRUE. 
#' @param fillinDrawdown A logical value which specifies whether to use the
#'  DRAWDOWN parameter to fill in unrecorded cover and HORIZ_DIST_DD values.
#'  The default value is TRUE.
#' @param fillinDDImpacts_maxDrawdownDist A numeric value, the maximum drawdown
#' distance to fill in impacts for. Default value is 1.5. 
#' @param dataInformation A data frame containing the field fish cover
#' categorical values and corresponding numeric cover values used in calculations, 
#' as well as indicators of cover type presence or absence for each category. The
#' default values (and required column names) are:
#' \itemize{
#' \item value c(NA,'0','1','2','3','4')
#' \item weights c(NA,0,0.05,0.25,0.575,0.875)
#' \item presence c(NA,0, 1, 1, 1, 1) 
#' }  
#' @param isUnitTest Logical argument to determine whether errors should be ignored.
#' Should only be used for running a unit test. Default value is FALSE.
#' @return Either a data frame when metric calculation is successful or a 
#' character string containing an error message when metric calculation 
#' is not successful. The data frame contains the following columns:
#' \itemize{ 
#'     \item SITE - unique site visit identifier
#'     \item METRIC - metric name
#'     \item VALUE - metric value
#'       }
#' The output metrics include:
#' FCFPAQUATIC_DD, FCFPBOULDERS_DD, FCFPBRUSH_DD, FCFPLEDGES_DD, 
#' FCFPLIVETREES_DD, FCFPOVERHANG_DD, FCFPSNAGS_DD, FCFPSTRUCTURES_DD, 
#' FCFPAQUATIC_LIT, FCFPBOULDERS_LIT, FCFPBRUSH_LIT, FCFPLEDGES_LIT, 
#' FCFPLIVETREES_LIT, FCFPOVERHANG_LIT, FCFPSNAGS_LIT, FCFPSTRUCTURES_LIT, 
#' FCFPAQUATIC_SIM, FCFPBOULDERS_SIM, FCFPBRUSH_SIM, FCFPLEDGES_SIM, 
#' FCFPLIVETREES_SIM, FCFPOVERHANG_SIM, FCFPSNAGS_SIM, FCFPSTRUCTURES_SIM, 
#' FCNAQUATIC_DD, FCNAQUATIC_LIT, FCNAQUATIC_SIM, FCNBOULDERS_DD, 
#' FCNBOULDERS_LIT, FCNBOULDERS_SIM, FCNBRUSH_DD, FCNBRUSH_LIT, 
#' FCNBRUSH_SIM, FCNLEDGES_DD, FCNLEDGES_LIT, FCNLEDGES_SIM, 
#' FCNLIVETREES_DD, FCNLIVETREES_LIT, FCNLIVETREES_SIM, FCNOVERHANG_DD, 
#' FCNOVERHANG_LIT, FCNOVERHANG_SIM, FCNSNAGS_DD, FCNSNAGS_LIT, 
#' FCNSNAGS_SIM, FCNSTRUCTURES_DD, FCNSTRUCTURES_LIT, FCNSTRUCTURES_SIM, 
#' FCVAQUATIC_DD, FCVBOULDERS_DD, FCVBRUSH_DD, FCVLEDGES_DD, 
#' FCVLIVETREES_DD, FCVOVERHANG_DD, FCVSNAGS_DD, FCVSTRUCTURES_DD, 
#' FCVAQUATIC_LIT, FCVBOULDERS_LIT, FCVBRUSH_LIT, FCVLEDGES_LIT, 
#' FCVLIVETREES_LIT, FCVOVERHANG_LIT, FCVSNAGS_LIT, FCVSTRUCTURES_LIT, 
#' FCVAQUATIC_SIM, FCVBOULDERS_SIM, FCVBRUSH_SIM, FCVLEDGES_SIM, 
#' FCVLIVETREES_SIM, FCVOVERHANG_SIM, FCVSNAGS_SIM, , FCVSTRUCTURES_SIM, 
#' FCFCAQUATIC_DD, FCFCBOULDERS_DD, FCFCBRUSH_DD, FCFCLEDGES_DD, 
#' FCFCLIVETREES_DD, FCFCOVERHANG_DD, FCFCSNAGS_DD, FCFCSTRUCTURES_DD, 
#' FCFCAQUATIC_LIT, FCFCBOULDERS_LIT, FCFCBRUSH_LIT, FCFCLEDGES_LIT, 
#' FCFCLIVETREES_LIT, FCFCOVERHANG_LIT, FCFCSNAGS_LIT, FCFCSTRUCTURES_LIT, 
#' FCFCAQUATIC_SIM, FCFCBOULDERS_SIM, FCFCBRUSH_SIM, FCFCLEDGES_SIM, 
#' FCFCLIVETREES_SIM, FCFCOVERHANG_SIM, FCFCSNAGS_SIM, FCFCSTRUCTURES_SIM, 
#' FCIALL_DD, FCIALL_LIT, FCIALL_SIM, FCIBIG_DD, 
#' FCIBIG_LIT, FCIBIG_SIM, FCINATURAL_DD, FCINATURAL_LIT, 
#' FCINATURAL_SIM, FCIRIPVEG_DD, FCIRIPVEG_LIT, FCIRIPVEG_SIM, 
#' FCFPALL_DD, FCFPALL_LIT, FCFPALL_SIM, FCNALL_DD, 
#' FCNALL_LIT, FCNALL_SIM.
#' Descriptions for all NLA metrics can be found at:
#' \href{https://github.com/USEPA/aquamet/blob/master/inst/NLA_physical_habitat_metrics_descriptions.pdf}{NLA_Physical_Habitat_Metric_Descriptions.pdf}.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}
#' @examples
#'   head(nlaPhab)
#'   
#'   # Must subset example dataset to create inputs, keeping only SITE, STATION,
#'   #  and VALUE
#'   aquatic <- subset(nlaPhab,PARAMETER=='FC_AQUATIC',select=-PARAMETER)
#'   aquatic_dd <- subset(nlaPhab,PARAMETER=='FC_AQUATIC_DD',select=-PARAMETER)
#'   boulders <- subset(nlaPhab,PARAMETER=='FC_BOULDERS',select=-PARAMETER)
#'   boulders_dd <- subset(nlaPhab,PARAMETER=='FC_BOULDERS_DD',select=-PARAMETER)
#'   brush <- subset(nlaPhab,PARAMETER=='FC_BRUSH',select=-PARAMETER)
#'   brush_dd <- subset(nlaPhab,PARAMETER=='FC_BRUSH_DD',select=-PARAMETER)
#'   ledges <- subset(nlaPhab,PARAMETER=='FC_LEDGES',select=-PARAMETER)
#'   ledges_dd <- subset(nlaPhab,PARAMETER=='FC_LEDGES_DD',select=-PARAMETER)
#'   livetrees <- subset(nlaPhab,PARAMETER=='FC_LIVETREES',select=-PARAMETER)
#'   livetrees_dd <- subset(nlaPhab,PARAMETER=='FC_LIVETREES_DD',select=-PARAMETER)
#'   overhang <- subset(nlaPhab,PARAMETER=='FC_OVERHANG',select=-PARAMETER)
#'   overhang_dd <- subset(nlaPhab,PARAMETER=='FC_OVERHANG_DD',select=-PARAMETER)
#'   snags <- subset(nlaPhab,PARAMETER=='FC_SNAGS',select=-PARAMETER)
#'   snags_dd <- subset(nlaPhab,PARAMETER=='FC_SNAGS_DD',select=-PARAMETER)
#'   structures <- subset(nlaPhab,PARAMETER=='FC_STRUCTURES',select=-PARAMETER)
#'   structures_dd <- subset(nlaPhab,PARAMETER=='FC_STRUCTURES_DD',select=-PARAMETER)
#'   drawdown <- subset(nlaPhab,PARAMETER=='DRAWDOWN',select=-PARAMETER)
#'   
#'   # Ensure VALUE is numeric for this particular subset
#'   horizontalDistance_dd <- subset(nlaPhab,PARAMETER=='HORIZ_DIST_DD',
#'      select=-PARAMETER)
#'   horizontalDistance_dd$VALUE <- with(horizontalDistance_dd, as.numeric(VALUE))
#'   
#'   # Use defaults for fillinDrawdown, createSyntheticCovers, and coverClassInfo
#'   # arguments
#'   exFishCover <- nlaFishCover(aquatic,aquatic_dd,boulders,boulders_dd,brush,
#'       brush_dd,ledges,ledges_dd,livetrees,livetrees_dd,overhang,overhang_dd,
#'       snags,snags_dd,structures,structures_dd,drawdown,horizontalDistance_dd)
#'   
#'   head(exFishCover)
#'  
nlaFishCover <- function(aquatic = NULL
                        ,aquatic_dd = NULL
                        ,boulders = NULL
                        ,boulders_dd = NULL
                        ,brush = NULL
                        ,brush_dd = NULL
                        ,ledges = NULL
                        ,ledges_dd = NULL
                        ,livetrees = NULL
                        ,livetrees_dd = NULL
                        ,overhang = NULL
                        ,overhang_dd = NULL
                        ,snags = NULL
                        ,snags_dd = NULL
                        ,structures = NULL
                        ,structures_dd = NULL
                        ,drawdown = NULL
                        ,horizontalDistance_dd = NULL
                        ,createSyntheticCovers=TRUE
                        ,fillinDrawdown=TRUE
                        ,fillinDDImpacts_maxDrawdownDist=1.5                        # If NA, no DD impacts will be filled in with floodzone values
                        ,dataInformation = data.frame(value = c(NA,'0','1','2','3','4')
						 	                         ,weights = c(NA,0,0.05,0.25,0.575,0.875)
						 	                         ,presence = c(NA,0L,1L,1L,1L,1L) %>% as.logical()
						 	                         ,stringsAsFactors=FALSE
						 	                         )

                        ,isUnitTest = FALSE
                        ) {
################################################################################
# Function: nlaFishCover
# Title: Calculate NLA Fish Cover Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 7, 2008
# Description:
#   This function calculates the fish cover portion of the physical
#   habitat metrics for National Lakes Assessment (NLA) data.  The function
#   requires a data frame containing validated physical habitat data collected
#   using the NLA protocol.  NOTE: These calculations do not currently return
#   every metric for every site, as this did not occur for 2007.  Using the test
#   data for the 2007 unit test shows that site 7545 is lacking
#   FC[FC|FP|V][BRUSH|SNAGS] and site has2NAvalues is lacking another 41
#   metrics.
# Function Revisions:
#   10/07/08 cws: Added fcfpAll and fcnAll.
#   10/09/08 cws: Cover values normalized at each station/subid prior to
#            metrics calculation.
#   10/20/08 cws: Removed normalization, as individual FC categories are 
#            independent.
#   10/21/08 cws: Corrected counts of missing parameters.  These are now zero.
#   11/06/08 cws: Removing code for changing NA counts to zero.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   07/18/13 cws: Adding unit test based on 2007 data, results assumed to be
#            correct with the exception of NA counts, which in 2007 were set
#            to 0 later on (in nlaphab.r).  The metrics function was changed
#            to set those counts to integer 0 within the function.
#   07/22/13 cws: Improved handling NA data values by reordering if/else if/else 
#            testing to check for NA values before others. Unit test still
#            passes.  Modifying unit test data to have a site with an NA value.
#            Upcasing column names, renamed subid to STATION, changed FC_SNAG to
#            FC_SNAGS.
#   07/24/13 cws: Changed output from wide to long; removing rows with NA values
#            that were artifacts of the former wide organization (uid 7545 has
#            no BRUSH or SNAGS data, so FC[FC|FP|V][BRUSH|SNAGS] rows were
#            removed from the expected values in the unit test). Subsetting
#            initial input to remove NA results. 
#   07/31/13 cws: Including drawdown cover values and synthesized cover
#            calculations.  Added argument createSyntheticCovers to specify
#            whether synthetic covers would be calculated.  Unit tests added.
#   08/02/13 cws: Extended calculations to include 'synthetic' covers.  Unit
#            test based on SAS calculation results.
#   08/06/13 cws: Changed names of metrics to add _LIT suffix to riparian
#            metrics when createSyntheticCovers is TRUE, e.g. FCIALL became
#            FCIALL_LIT. Unit test updated.
#   08/07/13 Changed _RIP suffix to _LIT.
#   08/08/13 Updated unit test to reflect changes in calcSynCovers.  This
#            affected 8 values at SITE=7263, which has no HORIZ_DIST_DD.
#   08/12/13 Updated unit test to reflect 9 August changes to calcSynCovers.
#            Moved parameter suffix determination, done twice in this function,
#            to metsFishCover.splitParameterNames(). New *_SYN calculations,
#            which rely on assumptions about the data are included BUT NOT YET
#            ADDED TO THE UNIT TEST SINCE THAT'S TIME CONSUMING.  The values
#            look correct based on hand calculations of FC[FP|N|V]_AQUATIC_SYN
#            for SITE 6160, 6189, 6227.
#   08/13/13 cws: Standardized calculations to use coverSuffix as a by-variable.
#            Notifying user of presence/lack of *_SYN metrics in unit test.
#   08/26/13 cws: Modified to use 10m as maximal drawdown into littoral region
#            instead of 15.  Using expand.data.frame() so that each metric
#            occurs for each site.
#   09/16/13 cws: Changing _SYN metrics to _SIM, as they are simulations of
#            littoral cover rather than synthesized 2007-esque cover values.
#            Using DRAWDOWN parameter to fill in unrecorded drawdown covers and
#            HORIZ_DIST_DD values.  Unit test NOT YET modified accordingly, as
#            Phil wants these values quickly.
#   09/26/13 cws: Added DRAWDOWN fillin to unit test. Based on
#            projects/metsFishCoverTest.sas which gets most things right but
#            includes some extra _LIT* values which are NA and absent from R,
#            and miscalculates 15 FCN*_DD counts in 3 SITE, so the SAS
#          results were corrected.
#   11/06/13 cws: Now including checks of *_SIM values in unit test when filling
#            in values based on DRAWDOWN (using fillinDrawdownData()).  Still
#            removing *_SIM values in unit test when NOT filling in values based
#            on DRAWDOWN.
#   11/12/13 cws: Updated unit test counts after correcting fillinDrawdownData()
#            on this date.  Addressing calculations with simulated cover values
#            when NOT filling in DD values.  Using anonymous functions (like in
#            HI mets) in place of mean() to correctly handle cases where all
#            data is NA.
#   11/14/13 cws: Corrected synthetic calculations when drawdown values are not
#            'filled in', and updated unit test accordingly. Regression test
#            against 2007 values shows zero differences > 1e-13 and 522
#            differences due to 2007 counts being 0 and 2012 counts (FCN*) being
#            NA. These are for 58 SITEs which have no FC data and hence no way
#            inside this function to set those values.
#   12/16/13 cws: Using protectedMean and protectedSum functions in place of
#            anonymous functions.  Regression test with entire 2007 data passed 
#            with expected 522 differences in FCN* values (was 0, now NA when 
#            data is absent).
#   06/12/14 tmk: Removed calls to the require() function.
#    7/10/17 cws Renamed from metsFishCoverNLA to nlaFishCover. Using SITE,
#            METRIC and VALUE column names instead of UID, PARAMETER, RESULT.
#    7/12/17 cws Updated calling interface to aquamet phab standard, updating
#            unit tests as well.
#    3/19/19 cws Added isUnitTest argument for consistency.
#    3/22/19 cws Added validation of meetadata, added legal/range checks of data
#            args. Unit test modified accordingly
#    3/28/19 cws Standardized metadata argument naming
#    3/07/24 cws Renamed fillinDrawdownData to fillinAbsentMissingWithDefaultValue
#    3/08/24 cws Added fillinDDWithRiparianValues to fill in missing/absent
#            drawdown effect values with non-missing riparian zone values when 
#            the horizontal drawdown distance at a station is equal to or less 
#            than the value of fillinDDImpacts_maxDrawdownDist . Added argument
#            fillinDDImpacts_maxDrawdownDist with default value of 1.5
#    3/25/24 cws Modified to not do any drawdown zone fillin if data2007 is TRUE
#            because 2007 does not have any drawdown data. No change made to unit
#            tests. Slight change in handling when fillinDrawdown and 
#            createSyntheticCovers are used but no drawdown data is provided, 
#            as with 2007 data.
#    5/21/24 cws Modified to rename expand.data.frame to expandDataFrame
#
# Arguments:
#   df = a data frame containing fish cover data.  The data frame must include
#     columns that are named as follows:
#       SITE - universal ID value, which uniquely identifies the site location, 
#             date of visit, visit order, habitat type, etc. for which metrics 
#             will be calculated.  For NLA, site ID, year and visit number are
#             used for this purpose.
#       STATION - the subordinate ID value, which identifies the location,
#               habitat type, order of occurence, etc. within a single SITE.
#               For NLA, transect is used for this purpose.
#       PARAMETER - parameter name, which identifies the variables used in
#                   calculations. In wide data frame format, this argument
#                   would be used to name columns.  It is assumed that this
#                   argument has the following values: FC_AQUATIC, FC_BOULDERS, 
#                   FC_BRUSH, FC_LEDGES, FC_TREES, FC_OVERHANG, FC_SNAGS,
#                   and FC_STRUCTURES.  Each fish cover class occurs at least
#                   once in the input dataframe (this is because of how NA
#                   counts are set to 0 at the end of the function.
#       VALUE - parameter values, which are the values used in calculations.
#       UNITS - parameter units, which identifies the units in which the
#               parameter values are recorded.
#   createSyntheticCovers = a logical value, which specifies whether to create
#     synthetic cover values as proportions of drawdown and riparian cover.
#     This argument should be set to FALSE when the data follows the 2007 NLA
#     protocol or do not contain drawdown cover data.  The default value is
#     TRUE.
#   fillinDrawdown = a logical value, which specifies whether to use the
#     DRAWDOWN parameter to fill in unrecorded cover and HORIZ_DIST_DD values.
#     The default value is TRUE.
# Output:
#   A data frame that contains the following columns:
#     SITE - universal ID value
#     PARAMETER - metric name
#     VALUE - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

    # Print initial messages
    intermediateMessage('NLA Fish Cover metrics', loc='start')

	# Standardize arguments, then combine them into single dataframe as expected
    # in the rest of the function
    addClass <- function(df, ...) {
        
        args <- list(...)
        
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(CLASS=args[[1]])
        return(rc)
        
    }
    
    dataInformation <- aquametStandardizeArgument(dataInformation
                                                 ,struct = list(value=c('character','integer'), weights='double', presence=c('logical','integer'))
                                                 ,rangeLimits = list(weights=c(0,1))
                                                 ,legalValues = list(value=c(NA,'','0','1','2','3','4'), presence=c(NA,FALSE,TRUE))
                                                 ,stopOnError = !isUnitTest
                                                 )

    aquatic <- aquametStandardizeArgument(aquatic, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_AQUATIC', stopOnError = !isUnitTest)
    intermediateMessage('.')
    aquatic_dd <- aquametStandardizeArgument(aquatic_dd, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_AQUATIC_DD', stopOnError = !isUnitTest)
    intermediateMessage('.')
    boulders <- aquametStandardizeArgument(boulders, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_BOULDERS', stopOnError = !isUnitTest)
    intermediateMessage('.')
    boulders_dd <- aquametStandardizeArgument(boulders_dd, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_BOULDERS_DD', stopOnError = !isUnitTest)
    intermediateMessage('.')
    brush <- aquametStandardizeArgument(brush, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_BRUSH', stopOnError = !isUnitTest)
    intermediateMessage('.')
    brush_dd <- aquametStandardizeArgument(brush_dd, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_BRUSH_DD', stopOnError = !isUnitTest)
    intermediateMessage('.')
    ledges <- aquametStandardizeArgument(ledges, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_LEDGES', stopOnError = !isUnitTest)
    intermediateMessage('.')
    ledges_dd <- aquametStandardizeArgument(ledges_dd, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_LEDGES_DD', stopOnError = !isUnitTest)
    intermediateMessage('.')
    livetrees <- aquametStandardizeArgument(livetrees, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_LIVETREES', stopOnError = !isUnitTest)
    intermediateMessage('.')
    livetrees_dd <- aquametStandardizeArgument(livetrees_dd, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_LIVETREES_DD', stopOnError = !isUnitTest)
    intermediateMessage('.')
    overhang <- aquametStandardizeArgument(overhang, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_OVERHANG', stopOnError = !isUnitTest)
    intermediateMessage('.')
    overhang_dd <- aquametStandardizeArgument(overhang_dd, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_OVERHANG_DD', stopOnError = !isUnitTest)
    intermediateMessage('.')
    snags <- aquametStandardizeArgument(snags, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_SNAGS', stopOnError = !isUnitTest)
    intermediateMessage('.')
    snags_dd <- aquametStandardizeArgument(snags_dd, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_SNAGS_DD', stopOnError = !isUnitTest)
    intermediateMessage('.')
    structures <- aquametStandardizeArgument(structures, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_STRUCTURES', stopOnError = !isUnitTest)
    intermediateMessage('.')
    structures_dd <- aquametStandardizeArgument(structures_dd, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '', dataInformation$value)), 'FC_STRUCTURES_DD', stopOnError = !isUnitTest)
    intermediateMessage('.')
    horizontalDistance_dd <- aquametStandardizeArgument(horizontalDistance_dd, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','double')), rangeLimits=list(VALUE=c(0,200)), 'HORIZ_DIST_DD', stopOnError = !isUnitTest)
    intermediateMessage('.')
    drawdown <- aquametStandardizeArgument(drawdown, ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('integer','character')), legalValues=list(VALUE=c(NA, '','N','NO','Y','YES')), 'DRAWDOWN', stopOnError = !isUnitTest)
    intermediateMessage('.')
    
    coverClassInfo <- dplyr::rename(dataInformation, field=value, characteristicCover=weights)
    
    df <- rbind(aquatic, aquatic_dd, boulders, boulders_dd, brush, brush_dd
               ,ledges, ledges_dd, livetrees, livetrees_dd, overhang, overhang_dd
               ,snags, snags_dd, structures, structures_dd
               ,horizontalDistance_dd, drawdown
               )
    intermediateMessage('.1')

	
	# Fill in unrecorded cover and HORIZ_DIST_DD based on DRAWDOWN.
	# If 2007-like data is being processed, createSyntheticCovers will be FALSE
	# and there will be no drawdown values to fill in.
	if((fillinDrawdown | createSyntheticCovers) & !all(c('HORIZ_DIST_DD','DRAWDOWN') %in% df$CLASS)) {
	    intermediateMessage('.Done early',loc = 'end')
	    return("Data for both horizontalDistance_dd and drawdown are required to fill-in missing cover values.  Either provide that data or set fillinDrawdown to FALSE")
	}
	  # Fill in missing drawdown values for each class if appropriate NEW LOCATION, USING DF INSTEAD OF FCDATA & HDDATA
	  if(createSyntheticCovers) 
	      df <- fillinDDWithRiparianValues(subset(df, CLASS %nin% c('HORIZ_DIST_DD','DRAWDOWN') & !is.na(VALUE)) #fcData
	                                          ,subset(df, CLASS %in% 'HORIZ_DIST_DD' & !is.na(VALUE)) #hdData
	                                          ,fillinDDImpacts_maxDrawdownDist
	                                          ) %>%
              rbind(subset(df, CLASS %in% c('HORIZ_DIST_DD','DRAWDOWN') & !is.na(VALUE))) # add these values back in!!!!
    
	if(fillinDrawdown) {
		intermediateMessage('.fill')
		df <- fillinAbsentMissingWithDefaultValue(df, fillinValue='0', fillinHORIZ_DIST_DD='0')
	} 
    intermediateMessage('.2')

	
	# Filter out any unnecessary data
	fcData <- subset(df, CLASS %nin% c('HORIZ_DIST_DD','DRAWDOWN') & !is.na(VALUE))
	hdData <- subset(df, CLASS %in% 'HORIZ_DIST_DD' & !is.na(VALUE))
    intermediateMessage('.3')
	
	  # # Fill in missing drawdown values for each class if appropriate OLD LOCATION
	  # if(createSyntheticCovers) 
	  #     fcData <- fillinDDWithRiparianValues(fcData, hdData, fillinDDImpacts_maxDrawdownDist)
	
	
	# Assign numeric values for cover classes and assign cover types to
	# specific groups.
	fcDataCalcs <- merge(fcData
					    ,coverClassInfo
						,by.x='VALUE'
						,by.y='field'
						,all.x=TRUE
						,sort=FALSE
					  	)
    intermediateMessage('.4')
						
	
	# Simulate cover values from riparian and drawdown cover values if requested. 
	# Values of HORIZ_DIST_DD are used for these calculations
	if(createSyntheticCovers) {
		if('HORIZ_DIST_DD' %nin% df$CLASS) {
    		intermediateMessage('.Done early', loc = 'end')
		    return("Data for both horizontalDistance_dd is required to calculate synthetic drawdown cover values.  Either provide that data or set createSyntheticCovers to FALSE")
		}
        intermediateMessage('.synth')
		
		horizDist <- within(subset(hdData, CLASS %in% 'HORIZ_DIST_DD' & !is.na(VALUE))
						   ,{characteristicCover <- NA
							 presence <- NA
						    }
				           )
        intermediateMessage('.s1')
		synValues <- calcSynCovers(rbind(fcDataCalcs,horizDist), 10, assumptions=FALSE)
		synValues$CLASS <- gsub('(_SYN)$','_SIM', synValues$CLASS)
        intermediateMessage('.s2')
								
		# Make the resulting dataframe look like the data we're calculating metrics with.
		newValues <- mutate(synValues[c('SITE','STATION','CLASS','VALUE','characteristicCover')]
						   # ,SAMPLE_TYPE = 'PHAB'
						   # ,FORM_TYPE = ''
						   # ,FLAG = as.character(NA)
						   ,presence = ifelse(is.na(characteristicCover),	NA
							 	  	  ,ifelse(characteristicCover > 0, 	1, 	0
									   ))
						   )
        intermediateMessage('.s3')
        
		fcDataCalcs <- rbind(fcDataCalcs, newValues)
	}				
    intermediateMessage('.5')
	

	# Classify CLASSs by cover location: riparian, drawdown or synthetic
	# This classification will be used to create suffixes for the metrics later on
	# For 2007, all locations will be riparian
	splitParams <- nlaFishCover.splitParameterNames(fcDataCalcs$CLASS)
    intermediateMessage('.6')
	fcDataCalcs <- mutate(fcDataCalcs
						 ,coverSuffix = ifelse(splitParams$suffix == '', '_LIT', splitParams$suffix)
						 ,CLASS = splitParams$base
				         )
	intermediateMessage('.7')

	
	fcMets <- nlaFishCover.calculateMets(fcDataCalcs)
    intermediateMessage('.8')

	
	# When not calculating synthetic cover values, rename the _LIT metrics back to 2007 names (no suffix). 	
	fcMets <- within(fcMets
					,{METRIC <- paste0(METRIC, coverSuffix)
					  coverSuffix <- NULL
					 }
					)
	
	if(!createSyntheticCovers) {
		fcMets <- within(fcMets
						,METRIC <- ifelse(grepl('_LIT$', METRIC), substr(METRIC, 1, nchar(METRIC)-4), METRIC)
						)
	}
	
	intermediateMessage(' Done.', loc='end')
	
	return(fcMets)
}

#' @keywords internal
nlaFishCover.calculateMets <- function(fcDataCalcs)
#
{
	# Calculate mets for individual cover types - counts, means, presence, sd
	indivCovers <- nlaFishCover.indivCovers(fcDataCalcs)
  	intermediateMessage('.m1')

#	fcMeans <- subset(indivCovers, grepl('^FCFC', CLASS))

	fcIndices <- nlaFishCover.groupIndices(fcDataCalcs)
  	intermediateMessage('.m2')
	
    
    # Determine fish cover presence at each station, then calculate mean
    # presence and count for site.
    pp <- aggregate(list(any=fcDataCalcs$characteristicCover)
                   ,list(SITE = fcDataCalcs$SITE
                        ,STATION = fcDataCalcs$STATION
                        ,CLASS = fcDataCalcs$CLASS
						,coverSuffix = fcDataCalcs$coverSuffix
                        )
                   ,function(x) { (if(is.na(x)) return(NA)		# recode cover to P/A
								   else if(x==0) return(0) 
                                   else return(1)
                                  )
                                }
                   )
				   
	fcExists <- aggregate(list(fcExists = pp$any)
                   		 ,list(SITE = pp$SITE
                         	  ,STATION = pp$STATION
							  ,coverSuffix = pp$coverSuffix
							  )
                   		 ,function(x) { return(any(x>0)) }
                   		 )
						 

	meanAll <- aggregate(list(VALUE = fcExists$fcExists)
                   		,list(SITE = fcExists$SITE
							 ,coverSuffix = fcExists$coverSuffix
							 )
                   		,protectedMean, na.rm=TRUE
                   		)
	meanAll$METRIC <- 'FCFPALL'


    nAll <- aggregate(list(VALUE = fcExists$fcExists)
                   	 ,list(SITE = fcExists$SITE, coverSuffix = fcExists$coverSuffix)
                     ,count
                     )
	nAll$METRIC <- 'FCNALL'
	
  	intermediateMessage('.m3')


    # combine results into a dataframe.  
	all <- rbind(indivCovers, fcIndices, meanAll, nAll)
  	intermediateMessage('.m4')

    return(all)
}

#' @keywords internal
nlaFishCover.splitParameterNames <- function(parameters)
# Splits the CLASS value into the base and suffix portions,
# e.g. 'FC_BOULDERS_DD' becomes 'FC_BOULDERS' and '_DD'.  Returns
# named list of two character vectors, base and suffix with
# these values, in the same order as the input vector
#
# ARGUMENTS:
# parameters	character vector of parameter names
#
{
	rxExpectedSuffices <- '(_DD|_LIT|_SYN|_SIM|_SYN0)'

	base <- ifelse(grepl(paste0('^(.+)', rxExpectedSuffices, '$'), parameters)
			 	  ,gsub(paste0('^(.+)', rxExpectedSuffices, '$'), '\\1', parameters)
				  ,parameters 
				  )
	suffix <- ifelse(grepl(paste0('^(.+)', rxExpectedSuffices, '$'), parameters)
				    ,gsub(paste0('^(.+)', rxExpectedSuffices, '$'), '\\2', parameters)
					,''
					)

	rc <- list(base=base, suffix=suffix)
	
	return(rc)
}

#' @keywords internal
nlaFishCover.indivCovers <- function(fcDataCalcs)
# Calculate simple mets for individual cover types
# ARGUMENTS:
# fcDataCalcs	dataframe with fish cover data that is augmented with 
#				  characteristic cover values and presence values
#
{
	# Calculate presence mean of each type of fish cover
	# Convert field results to calculable values
	intermediateMessage('.indivCovers')
	tt<-aggregate(list(VALUE = fcDataCalcs$presence)
				 ,list(SITE = fcDataCalcs$SITE
					  ,CLASS = fcDataCalcs$CLASS
					  ,coverSuffix = fcDataCalcs$coverSuffix
					  )
				 ,protectedMean, na.rm=TRUE
				 )
	meanPresence <- mutate(tt
	                      ,METRIC = gsub('^FC_(.+)$', 'FCFP\\1', CLASS)
	                      ,CLASS = NULL
	                      )
	intermediateMessage('.i1')

	
	# Calculate cover counts of each type of fish cover
	tt<-aggregate(list(VALUE = fcDataCalcs$characteristicCover)
				 ,list(SITE = fcDataCalcs$SITE
					  ,CLASS = fcDataCalcs$CLASS
					  ,coverSuffix = fcDataCalcs$coverSuffix
					  )
				 ,count
				 )
	allCounts <- expandDataFrame(tt, c('SITE','CLASS','coverSuffix'))
	nCover <- within(allCounts
					,{METRIC <- gsub('^FC_(.+)$', 'FCN\\1', CLASS)
					  VALUE <- ifelse(is.na(VALUE), 0L, VALUE)
					  CLASS <- NULL
					 }
					)
	intermediateMessage('.i2')
	
	
	# Calculate cover standard deviation of each type of fish cover
	tt<-aggregate(list(VALUE = fcDataCalcs$characteristicCover)
				 ,list(SITE = fcDataCalcs$SITE
					  ,CLASS = fcDataCalcs$CLASS
					  ,coverSuffix = fcDataCalcs$coverSuffix
					  )
				 ,sd, na.rm=TRUE
				 )
#	allV <- expandDataFrame(tt, c('SITE','CLASS','coverSuffix'))
	sdCover <- within(tt,{METRIC <- gsub('^FC_(.+)$', 'FCV\\1', CLASS)}) %>%
	           mutate(CLASS = NULL)
	intermediateMessage('.i3')
	
	
	# Calculate cover mean of each type of fish cover
	# Convert field results to calculable values.
	fcMeans<-aggregate(list(VALUE = fcDataCalcs$characteristicCover)
					  ,list(SITE = fcDataCalcs$SITE
						   ,CLASS = fcDataCalcs$CLASS
						   ,coverSuffix = fcDataCalcs$coverSuffix
					  	   )
					  ,protectedMean, na.rm=TRUE
					  )
#	allFC <- expandDataFrame(fcMeans, c('SITE','CLASS','coverSuffix'))
	meanCover <- within(fcMeans, METRIC <- gsub('^FC_(.+)$', 'FCFC\\1', CLASS)) %>%
	             mutate(CLASS = NULL)
	
	intermediateMessage('.i4')
#print(str(meanPresence));print(str(nCover));print(str(sdCover));print(str(meanCover))
	rc <- rbind(meanPresence, nCover, sdCover, meanCover)
	intermediateMessage('.i5')
	return(rc)
}

#' @keywords internal
nlaFishCover.groupIndices <- function(fcDataCalcs)
# Calculate cover indices: total, large, natural and riparian vegetation
# grouped cover 
#
# ARGUMENTS:
# fcMeans	dataframe with mean cover for each cover type at a site, with columns 
#             SITE, CLASS, VALUE, coverSuffix.
{

	fcMeans<-aggregate(list(VALUE = fcDataCalcs$characteristicCover)
					  ,list(SITE = fcDataCalcs$SITE
						   ,CLASS = fcDataCalcs$CLASS
						   ,coverSuffix = fcDataCalcs$coverSuffix
						   )
					  ,protectedMean, na.rm=TRUE
					  )
	intermediateMessage('.g1')
	fcMeans<-within(fcMeans
				   ,{isBig <- grepl('^FC_(BOULDERS|LEDGES|OVERHANG|STRUCTURES)', CLASS) 
					 isNatural <- !grepl('^FC_STRUCTURES', CLASS)
					 isRipVegetation <-grepl('^FC_(BRUSH|LIVETREES|SNAGS)', CLASS)
				    }
				   )
	intermediateMessage('.g2')
				   
	fciAll <- aggregate(list(VALUE = fcMeans$VALUE)
					   ,list(SITE = fcMeans$SITE
			                ,coverSuffix = fcMeans$coverSuffix
					        )
					   ,protectedSum, na.rm=TRUE
					   )
	fciAll$METRIC <- 'FCIALL'
	intermediateMessage('.g3')


	tt <- subset(fcMeans, isBig)
	fciBig <- aggregate(list(VALUE = tt$VALUE)
					   ,list(SITE = tt$SITE
				 		    ,coverSuffix = tt$coverSuffix
							)
#					   ,sum, na.rm=TRUE
					   ,protectedSum, na.rm=TRUE
					   )
	fciBig$METRIC <- 'FCIBIG'
	intermediateMessage('.g4')
	
	
	tt <- subset(fcMeans, isNatural)
	fciNatural <- aggregate(list(VALUE = tt$VALUE)
						   ,list(SITE = tt$SITE
								,coverSuffix = tt$coverSuffix
				   				)
#						   ,sum, na.rm=TRUE
						   ,protectedSum, na.rm=TRUE
						   )
	fciNatural$METRIC <- 'FCINATURAL'
	intermediateMessage('.g5')
	
	
	tt <- subset(fcMeans, isRipVegetation)
	fciRipVeg <- aggregate(list(VALUE = tt$VALUE)
						  ,list(SITE = tt$SITE
							   ,coverSuffix = tt$coverSuffix
				  			   )
#						  ,sum, na.rm=TRUE
						  ,protectedSum, na.rm=TRUE
						  )
	fciRipVeg$METRIC <- 'FCIRIPVEG'
	intermediateMessage('.g6')

	groupIndices <- rbind(fciAll, fciBig, fciNatural, fciRipVeg)[c('SITE','METRIC','VALUE', 'coverSuffix')]
	intermediateMessage('.g7')
	
	return(groupIndices)
}

# end of file