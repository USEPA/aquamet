#' @export
#' @title Calculate NLA Human Influence Metrics
#' @description This function calculates the human influence 
#' portion of the physical habitat metrics for National Lakes Assessment 
#' (NLA) data.  
#' @param buildings A data frame containing buildings human influence 
#'  proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param buildings_dd A data frame containing buildings human influence 
#'  proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param commerical A data frame containing commercial human influence 
#'  proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param commercial_dd A data frame containing commercial human influence 
#'  proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param crops A data frame containing row crops human influence 
#'  proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param crops_dd A data frame containing row crops human influence 
#'  proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param docks A data frame containing docks or boats human influence 
#'  proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param docks_dd A data frame containing docks or boats human influence 
#'  proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param landfill A data frame containing landfill/trash human influence 
#'  proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param landfill_dd A data frame containing landfill/trash human influence 
#'  proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param lawn A data frame containing lawn human influence 
#'  proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param lawn_dd A data frame containing lawn human influence 
#'  proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param orchard A data frame containing orchard human influence 
#'  proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param orchard_dd A data frame containing orchard human influence 
#'  proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param other A data frame containing other human influence 
#'  proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param other_dd A data frame containing other human influence 
#'  proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param park A data frame containing park facilities/man-made beach human 
#' influence proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param park_dd A data frame containing park facilities/man-made beach 
#' human influence proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param pasture A data frame containing pasture/range/hay field human 
#' influence proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param pasture_dd A data frame containing pasture/range/hay field human 
#' influence proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param powerlines A data frame containing power lines human influence 
#' proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param powerlines_dd A data frame containing power lines human influence 
#' proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param roads A data frame containing roads or railroad human influence 
#'  proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param roads_dd A data frame containing roads or railroad human influence 
#'  proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param walls A data frame containing walls, dikes, or revetments human 
#' influence proximity class values from riparian zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param walls_dd A data frame containing walls, dikes, or revetments 
#' human influence proximity class values from drawdown zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 0, P, or C representing the proximity 
#' category.
#' }
#' @param drawdown A data frame containing the presence of drawdown at stations. 
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an integer value, or character value that is castable to an 
#' integer, indicating the presence of drawdown at a station.
#' }
#' @param horizontalDistance_dd A data frame containing the horizontal 
#' distance from waterline to high water mark (m) when drawdown is present, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE an numeric value, or character value that is castable to an 
#' numeric.
#' }
#' @param data2007 A logical value, which equals TRUE if 2007 data is being
#' processed.  The default value is FALSE.
#' @param fillinDrawdown A logical value, which specifies whether to use the
#' DRAWDOWN parameter to fill in unrecorded cover and horizontalDistance_dd 
#' values. The default value is TRUE. 
#' @param proximityWeights A data frame relating categorical proximity values 
#' to various numeric weights for different types of metrics. The default data frame
#' consists of the following values:
#' \itemize{
#' \item proximity c('0','P','C')
#' \item calc c(0.0, 0.5, 1.0)  
#' \item circa c(0, 0, 1)
#' \item present c(0, 1, 1)
#' } 
#' @return Either a data frame when metric calculation is successful or a 
#' character string containing an error message when metric calculation 
#' is not successful. The data frame contains the following columns:
#' \itemize{ 
#'     \item SITE - unique site visit identifier
#'     \item METRIC - metric name
#'     \item VALUE - metric value
#'       }
#' The output metrics include:
#'  HIFPANY_DD, HIFPANY_RIP, HIFPANY_SYN, HIFPANYCIRCA_DD, 
#'  HIFPANYCIRCA_RIP, HIFPANYCIRCA_SYN, HIIAG_DD, HIIAG_RIP, 
#'  HIIAG_SYN, HIIAGCIRCA_DD, HIIAGCIRCA_RIP, HIIAGCIRCA_SYN, 
#'  HIIALL_DD, HIIALL_RIP, HIIALL_SYN, HIIALLCIRCA_DD, 
#'  HIIALLCIRCA_RIP, HIIALLCIRCA_SYN, HIINONAG_DD, HIINONAG_RIP, 
#'  HIINONAG_SYN, HIINONAGCIRCA_DD, HIINONAGCIRCA_RIP, HIINONAGCIRCA_SYN, 
#'  HINAG_DD, HINAG_RIP, HINAG_SYN, HINALL_DD, 
#'  HINALL_RIP, HINALL_SYN, HINBUILDINGS_DD, HINBUILDINGS_RIP, 
#'  HINBUILDINGS_SYN, HINCOMMERCIAL_DD, HINCOMMERCIAL_RIP, HINCOMMERCIAL_SYN, 
#'  HINCROPS_DD, HINCROPS_RIP, HINCROPS_SYN, HINDOCKS_DD, 
#'  HINDOCKS_RIP, HINDOCKS_SYN, HINLANDFILL_DD, HINLANDFILL_RIP, 
#'  HINLANDFILL_SYN, HINLAWN_DD, HINLAWN_RIP, HINLAWN_SYN, 
#'  HINNONAG_DD, HINNONAG_RIP, HINNONAG_SYN, HINORCHARD_DD, 
#'  HINORCHARD_RIP, HINORCHARD_SYN, HINOTHER_DD, HINOTHER_RIP, 
#'  HINOTHER_SYN, HINPARK_DD, HINPARK_RIP, HINPARK_SYN, 
#'  HINPASTURE_DD, HINPASTURE_RIP, HINPASTURE_SYN, HINPOWERLINES_DD, 
#'  HINPOWERLINES_RIP, HINPOWERLINES_SYN, HINROADS_DD, HINROADS_RIP, 
#'  HINROADS_SYN, HINWALLS_DD, HINWALLS_RIP, HINWALLS_SYN, 
#'  HIPWAG_DD, HIPWAG_RIP, HIPWAG_SYN, HIPWALL_DD, 
#'  HIPWALL_RIP, HIPWALL_SYN, HIPWBUILDINGS_DD, HIPWBUILDINGS_RIP, 
#'  HIPWBUILDINGS_SYN, HIPWCOMMERCIAL_DD, HIPWCOMMERCIAL_RIP, HIPWCOMMERCIAL_SYN
#'  HIPWCROPS_DD, HIPWCROPS_RIP, HIPWCROPS_SYN, HIPWDOCKS_DD, 
#'  HIPWDOCKS_RIP, HIPWDOCKS_SYN, HIPWLANDFILL_DD, HIPWLANDFILL_RIP, 
#'  HIPWLANDFILL_SYN, HIPWLAWN_DD, HIPWLAWN_RIP, HIPWLAWN_SYN, 
#'  HIPWNONAG_DD, HIPWNONAG_RIP, HIPWNONAG_SYN, HIPWORCHARD_DD, 
#'  HIPWORCHARD_RIP, HIPWORCHARD_SYN, HIPWOTHER_DD, HIPWOTHER_RIP, 
#'  HIPWOTHER_SYN, HIPWPARK_DD, HIPWPARK_RIP, HIPWPARK_SYN, 
#'  HIPWPASTURE_DD, HIPWPASTURE_RIP, HIPWPASTURE_SYN, HIPWPOWERLINES_DD, 
#'  HIPWPOWERLINES_RIP, HIPWPOWERLINES_SYN, HIPWROADS_DD, HIPWROADS_RIP, 
#'  HIPWROADS_SYN, HIPWWALLS_DD, HIPWWALLS_RIP, HIPWWALLS_SYN
#'  
#' Descriptions for all metrics are included in 
#' \emph{NLA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#'   head(nlaPhabEx)
#'   
#'   buildings <- subset(nlaPhabEx,PARAMETER=='HI_BUILDINGS',select=-PARAMETER)
#'   buildings_dd <- subset(nlaPhabEx,PARAMETER=='HI_BUILDINGS_DD',select=-PARAMETER)
#'   commercial <- subset(nlaPhabEx,PARAMETER=='HI_COMMERCIAL',select=-PARAMETER)
#'   commercial_dd <- subset(nlaPhabEx,PARAMETER=='HI_COMMERCIAL_DD',select=-PARAMETER)
#'   crops <- subset(nlaPhabEx,PARAMETER=='HI_CROPS',select=-PARAMETER)
#'   crops_dd <- subset(nlaPhabEx,PARAMETER=='HI_CROPS_DD',select=-PARAMETER)
#'   docks <- subset(nlaPhabEx,PARAMETER=='HI_DOCKS',select=-PARAMETER)
#'   docks_dd <- subset(nlaPhabEx,PARAMETER=='HI_DOCKS_DD',select=-PARAMETER)
#'   landfill <- subset(nlaPhabEx,PARAMETER=='HI_LANDFILL',select=-PARAMETER)
#'   landfill_dd <- subset(nlaPhabEx,PARAMETER=='HI_LANDFILL_DD',select=-PARAMETER)
#'   lawn <- subset(nlaPhabEx,PARAMETER=='HI_LAWN',select=-PARAMETER)
#'   lawn_dd <- subset(nlaPhabEx,PARAMETER=='HI_LAWN_DD',select=-PARAMETER)
#'   orchard <- subset(nlaPhabEx,PARAMETER=='HI_ORCHARD',select=-PARAMETER)
#'   orchard_dd <- subset(nlaPhabEx,PARAMETER=='HI_ORCHARD_DD',select=-PARAMETER)
#'   other <- subset(nlaPhabEx,PARAMETER=='HI_OTHER',select=-PARAMETER)
#'   other_dd <- subset(nlaPhabEx,PARAMETER=='HI_OTHER_DD',select=-PARAMETER)
#'   park <- subset(nlaPhabEx,PARAMETER=='HI_PARK',select=-PARAMETER)
#'   park_dd <- subset(nlaPhabEx,PARAMETER=='HI_PARK_DD',select=-PARAMETER)
#'   pasture <- subset(nlaPhabEx,PARAMETER=='HI_PASTURE',select=-PARAMETER)
#'   pasture_dd <- subset(nlaPhabEx,PARAMETER=='HI_PASTURE_DD',select=-PARAMETER)
#'   powerlines <- subset(nlaPhabEx,PARAMETER=='HI_POWERLINES',select=-PARAMETER)
#'   powerlines_dd <- subset(nlaPhabEx,PARAMETER=='HI_POWERLINES_DD',select=-PARAMETER)
#'   roads <- subset(nlaPhabEx,PARAMETER=='HI_ROADS',select=-PARAMETER)
#'   roads_dd <- subset(nlaPhabEx,PARAMETER=='HI_ROADS_DD',select=-PARAMETER)
#'   walls <- subset(nlaPhabEx,PARAMETER=='HI_WALLS',select=-PARAMETER)
#'   walls_dd <- subset(nlaPhabEx,PARAMETER=='HI_WALLS_DD',select=-PARAMETER)
#'   drawdown <- subset(nlaPhabEx,PARAMETER=='DRAWDOWN',select=-PARAMETER)
#'   horizontalDistance_dd <- subset(nlaPhabEx,PARAMETER=='HORIZ_DIST_DD',select=-PARAMETER)
#'   
#'   # Use defaults for data2007, fillinDrawdown, and proximityWeights
#'   # arguments
#'   exHumInfl <- nlaHumanImpact(buildings, buildings_dd, commercial, commercial_dd,
#'   crops, crops_dd,docks, docks_dd, landfill, landfill_dd, lawn, lawn_dd, orchard,
#'   orchard_dd, other, other_dd, park, park_dd, pasture, pasture_dd, powerlines,
#'   powerlines_dd, roads, roads_dd, walls, walls_dd, drawdown, horizontalDistance_dd)
#'   
#'   head(exHumInfl)
#'  
#' @keywords survey

nlaHumanImpact <- function(buildings = NULL
                          ,buildings_dd = NULL
                          ,commercial = NULL
                          ,commercial_dd = NULL
                          ,crops = NULL
                          ,crops_dd = NULL
                          ,docks = NULL
                          ,docks_dd = NULL
                          ,landfill = NULL
                          ,landfill_dd = NULL
                          ,lawn = NULL
                          ,lawn_dd = NULL
                          ,orchard = NULL
                          ,orchard_dd = NULL
                          ,other = NULL
                          ,other_dd = NULL
                          ,park = NULL
                          ,park_dd = NULL
                          ,pasture = NULL
                          ,pasture_dd = NULL
                          ,powerlines = NULL
                          ,powerlines_dd = NULL
                          ,roads = NULL
                          ,roads_dd = NULL
                          ,walls = NULL
                          ,walls_dd = NULL
                          ,drawdown = NULL
                          ,horizontalDistance_dd = NULL                             # required for calculation of synthetic values mimicking 2007 metrics
                          ,data2007=FALSE                                           # if TRUE, synthetic values mimicking 2007 metrics will not be calculated, and _RIParian metric names will lose that suffix
                          ,fillinDrawdown=TRUE
                          ,proximityWeights = data.frame(proximity=c('0', 'P', 'C') # Define weighing influence proximity values 
                          	                            ,calc=     c(0.0, 0.5, 1.0)
                          	                            ,circa=    c(0,   0,   1)
                          	                            ,present=  c(0,   1,   1)
                                                        ,stringsAsFactors=FALSE
                          	                            )
                          ) {

################################################################################
# Function: nlaHumanImpact
# Title: Calculate NLA Human Impact Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 21, 2008
# Description:
#   This function calculates the human impact portion of the physical habitat
#   metrics for National Lakes Assessment (NLA) data.  The function requires a
#   data frame containing validated physical habitat data collected using the
#   NLA protocol.
# Function Revisions:
#   10/21/08 cws: corrected spelling of mets names. Correcting counts of missing 
#            parameters.  These are now zero.
#   11/04/08 cws: Added additional mets as specified by PRK.  Reorganizing code
#            to simplify additional calculations
#   11/06/08 cws: Removing code for changing NA counts to zero.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   07/18/13 cws: Adding unit test based on 2007 data, results assumed to be
#            correct with the exception of NA counts, which in 2007 were set
#            to 0 later on (in nlaphab.r).
#   07/22/13 cws: Upcased column names, changed subid to STATION. Unit test
#            works.
#   07/24/13 cws: Upcased metric names; filtering NA RESULT values from input
#            data;  returning metrics in long instead of wide organization.
#   08/02/13 cws: Added handling of separate drawdown parameters.  Still passes 
#            unit test with 2007 data.
#   08/06/13 cws: Changed names of metrics to add _RIP suffix to riparian
#            metrics when createSyntheticCovers is TRUE.
#   08/07/13 cws: Changed _RIP suffix to _LIT.
#   08/21/13 cws: Changed _LIT suffix back to _RIP. Refactored function to
#            separate types of calculations.  Changed argument
#            createSyntheticCovers to data2007, as synthetic calculations are
#            not currently defined for this data.
#   08/23/13 cws: Allowing HI_OTHER, HI_OTHER_DD parameters in non-2007 data.
#            These have isAg set to FALSE.  Since may or may not be
#            agricultural, isAg could reasonably be set to NA for these, but
#            that would require additional changes to the coding (NA values in
#            aggregate grouping columns cause those rows to be eliminated;
#            ifelse() calls would also require modification).  Unit test created
#            on agreement with results from projects/metsHumanInfluenceTest.sas.
#   08/30/13 cws: Adding calculation of synthetic influence values
#   09/04/13 cws: Using DRAWDOWN parameter to fill in unrecorded drawdown covers
#            and HORIZ_DIST_DD values.  Added DRAWDOWN to unit test data.  Unit
#            test NOT YET modified accordingly, as Phil wants these values
#            quickly.
#   11/14/13 cws: correcting synthetic calculations based on comparisons with
#            SAS output.  Updated unit test for data with DRAWDOWN and filled in
#            drawdown values.  Some aggregate calculations now use anonymous
#            functions to return NA when all the values are NA; sum() returns 0
#            and mean() returns NaN when all values are NA and na.rm=TRUE. Unit
#            test metsHumanImpactTest.withDrawDown() now has appropriate test
#            data.  These changes resulted in additional rows of results where
#            counts are zero (e.g. HINAG) and other values are NA (e.g. HIIAG,
#            HIPWAG).  Otherwise the results are unchanged.
#   12/12/13 cws: Changed remaining aggregate calls to mean() with anonymous
#            function to avoid NaN results when all values are NA.
#   12/13/13 cws: Using protectedmean() and protectedSum() instead of anonymous
#            functions to avoid NaN results when all values are NA.
#   12/16/13 cws: Regression test with entire 2007 data passed with expected 855
#            differences in HIN* values (was 0, now NA when data is absent).
#   06/12/14 tmk: Removed calls to the require() function.
#    7/12/17 cws Updated calling interface to aquamet phab standard, updating
#            unit tests as well.
#
# Arguments:
#   df = a data frame containing human influence data.  The data frame must
#     include columns that are named as follows:
#       SITE - universal ID value, which uniquely identifies the site location, 
#             date of visit, visit order, habitat type, etc. for which metrics 
#             will be calculated.  For NLA, site ID, year and visit number are
#             used for this purpose.
#       STATION - the subordinate ID value, which identifies the location,
#               habitat type, order of occurence, etc. within a single SITE.
#               For NLA, transect is used for this purpose.
#       CLASS - parameter name, which identifies the variables used in
#                   calculations. In wide data frame format, this argument
#                   would be used to name columns.  It is assumed that this
#                   argument has the following values: HI_BUILDINGS,
#                   HI_COMMERCIAL, HI_CROPS, HI_DOCKS, HI_LANDFILL, HI_LAWN,
#                   HI_ORCHARD, HI_PARK, HI_PASTURE, HI_POWERLINES, HI_ROADS,
#                   and HI_WALLS.
#       VALUE - parameter values, which are the values used in calculations.
#       UNITS - parameter units, which identifies the units in which the
#               parameter values are recorded.
#   data2007 = a logical value, which equals TRUE if 2007 data is being
#     processed.  The default value is FALSE.
#   fillinDrawdown = a logical value, which specifies whether to use the
#     DRAWDOWN parameter to fill in unrecorded cover and HORIZ_DIST_DD values.
#     The default value is TRUE.
# Output:
#   A data frame that contains the following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     VALUE - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

    # Print initial messages
	intermediateMessage('NLA human influence metrics', loc='start')
    
    addParameter <- function(df, ...) {
        
        args <- list(...)
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(CLASS = args[[1]])
        return(rc)
    }
    buildings <-     buildings %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),     'HI_BUILDINGS')
    buildings_dd <-  buildings_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),  'HI_BUILDINGS_DD')
    commercial <-    commercial %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),    'HI_COMMERCIAL')
    commercial_dd <- commercial_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), 'HI_COMMERCIAL_DD')
    crops <-         crops %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),         'HI_CROPS')
    crops_dd <-      crops_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),      'HI_CROPS_DD')
    docks <-         docks %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),         'HI_DOCKS')
    docks_dd <-      docks_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),      'HI_DOCKS_DD')
    landfill <-      landfill %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),      'HI_LANDFILL')
    landfill_dd <-   landfill_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),   'HI_LANDFILL_DD')
    lawn <-          lawn %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),          'HI_LAWN')
    lawn_dd <-       lawn_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),       'HI_LAWN_DD')
    orchard <-       orchard %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),       'HI_ORCHARD')
    orchard_dd <-    orchard_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),    'HI_ORCHARD_DD')
    other <-         other %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),         'HI_OTHER')
    other_dd <-      other_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),      'HI_OTHER_DD')
    park <-          park %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),          'HI_PARK')
    park_dd <-       park_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),       'HI_PARK_DD')
    pasture <-       pasture %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),       'HI_PASTURE')
    pasture_dd <-    pasture_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),    'HI_PASTURE_DD')
    powerlines <-    powerlines %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),    'HI_POWERLINES')
    powerlines_dd <- powerlines_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), 'HI_POWERLINES_DD')
    roads <-         roads %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),         'HI_ROADS')
    roads_dd <-      roads_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),      'HI_ROADS_DD')
    walls <-         walls %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),         'HI_WALLS')
    walls_dd <-      walls_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),      'HI_WALLS_DD')
    drawdown <-      drawdown %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'),      'DRAWDOWN')
    horizontalDistance_dd <- horizontalDistance_dd %>% aquametStandardizeArgument(ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), 'HORIZ_DIST_DD')

    df <- rbind(buildings, buildings_dd, commercial, commercial_dd
               ,crops, crops_dd, docks, docks_dd
               ,landfill, landfill_dd, lawn, lawn_dd
               ,orchard, orchard_dd, other, other_dd
               ,park, park_dd, pasture, pasture_dd
               ,powerlines, powerlines_dd, roads, roads_dd
               ,walls, walls_dd
               ,drawdown, horizontalDistance_dd
               )
	intermediateMessage('.1')
    
	impactClasses <- setdiff(df$CLASS, c('DRAWDOWN','HORIZ_DIST_DD'))
	agClasses <- c('HI_CROPS','HI_CROPS_DD','HI_ORCHARD','HI_ORCHARD_DD','HI_PASTURE','HI_PASTURE_DD')
	hiTypes <- data.frame(CLASS=impactClasses
	                     ,isAg=impactClasses %in% agClasses
	                     ,stringsAsFactors=FALSE
	                     )

	# Fill in unrecorded cover and HORIZ_DIST_DD based on DRAWDOWN.
	if(fillinDrawdown) {
		intermediateMessage('.drawdownFillin')
		tt <- subset(df, CLASS %in% c(impactClasses,'HORIZ_DIST_DD','DRAWDOWN'))
		dfStart <- fillinDrawdownData(tt, fillinValue='0', fillinHORIZ_DIST_DD='0')
	} else {
		intermediateMessage(".NoDrawdownFillin'")
		dfStart <- df
	}
	intermediateMessage('.2')

  	hiData <- subset(dfStart, CLASS %in% impactClasses & !is.na(VALUE))
	intermediateMessage('.3')
	
  
  	# Convert proximity classes to numeric values and characterize influence
  	# types
  	hiData <- merge(hiData, proximityWeights
                   ,by.x='VALUE'
                   ,by.y='proximity'
                   ,all.x=TRUE
                   ,sort=FALSE
                   )
  	hiData <- merge(hiData, hiTypes
                   ,by.x='CLASS'
                   ,by.y='CLASS'
                   ,all.x=TRUE
                   ,sort=FALSE
                   )
	intermediateMessage('.4')
				   

	# Create synthetic influence values if not 2007esque data
	if(!data2007) {
		if('HORIZ_DIST_DD' %nin% dfStart$CLASS) {
		    intermediateMessage('.Done early',loc = 'end')
		    return('Synthesizing 2007-esque values requires values for the drawdown horizontal distance (argument horizontalDistance_dd)')
		}
		intermediateMessage('.Synthesizing')
		horizDist <- within(subset(dfStart, CLASS %in% 'HORIZ_DIST_DD' & !is.na(VALUE))
						   ,{calc <- NA
							 circa <- NA
							 present <- NA
							 isAg <- NA
							}
						   )

		intermediateMessage('.a')
		synValues <- calcSynInfluence(rbind(hiData,horizDist))
		intermediateMessage('.b')
		
		# Make the resulting dataframe look like the data we're calculating metrics with.
		newValues <- within(merge(synValues
		                         ,hiTypes %>% 
		                          subset(grepl('^.+_DD$', CLASS)) %>% 
		                          mutate(CLASS = sub('^(.+)_DD$', '\\1_SYN', CLASS)) # 
								 ,'CLASS'
								 ,all.x=TRUE
						 		 )
						   ,{circa <- ifelse(calc >= (1.0 - 1e-15), TRUE, FALSE)
							 present <- ifelse(calc > (1e-15), TRUE, FALSE)
							}
						   )

		intermediateMessage('.c')
		hiData <- rbind(hiData, newValues)
		intermediateMessage('.d')
	}


	# Classify CLASSs by cover location: riparian, drawdown or synthetic
	# This classification will be used to create suffixes for the metrics later
	# on.  For 2007, all locations will be riparian
	intermediateMessage('.splitting')
	splitParams <- nlaFishCover.splitParameterNames(hiData$CLASS)
	intermediateMessage('.classifying')
	hiData <- within(hiData
					,{coverSuffix <- ifelse(splitParams$suffix=='', '_RIP', splitParams$suffix)
					  CLASS <- splitParams$base
					 }
					)
	intermediateMessage('.calculating')
					
	hiMets <- nlaHumanImpact.calculateMets(hiData)
	
	
	# METRIC gets name of metric including suffix
	hiMets <- within(hiMets
					,{METRIC <- paste0(METRIC, coverSuffix)
					  coverSuffix <- NULL
					 }
					)
	
	# When calculating values for 2007 study, rename the _RIP metrics back to 
	# 2007 names (no suffix). 	
	if(data2007) {
		hiMets <- within(hiMets
						,{METRIC <- ifelse(grepl('_RIP$', METRIC)
				                             ,substr(METRIC, 1, nchar(METRIC)-4)
							 				 ,METRIC
							 				 )
					  	 }
						)
	}
	intermediateMessage(' Done.', loc='end')
	
	return(hiMets)
}


nlaHumanImpact.calculateMets <- function(hiData)
# Do all the calculationy stuff
{
	intermediateMessage('.WeightedIndividual')
	weightedIndividualInfluence <- nlaHumanImpact.weightedIndividualInfluence(hiData)
	intermediateMessage('.Presence')
	anyPresence <- nlaHumanImpact.anyPresence(hiData)
	intermediateMessage('.Overall')
	overallInfluence <- nlaHumanImpact.overallInfluence(hiData)
	intermediateMessage('.Circa')
	circaInfluence <- nlaHumanImpact.circaInfluence(hiData)
	intermediateMessage('.WeightedGroup')
	weightedGroupInfluence <- nlaHumanImpact.weightedGroupInfluence(hiData)
	intermediateMessage('.X')
	all <- rbind(weightedIndividualInfluence, anyPresence, overallInfluence
			   	,circaInfluence, weightedGroupInfluence
			   	)
	intermediateMessage('.Y')
	rc <- expand.data.frame(all, c('SITE','METRIC','coverSuffix'))
	intermediateMessage('.Z')
	rc$VALUE <- with(rc, ifelse(grepl('^HIN', METRIC) & is.na(VALUE), 0, VALUE))
	return(rc)
}


nlaHumanImpact.weightedIndividualInfluence <- function(hiData)
# Calculate means and counts for individual influence classes
{
  	# Calculate means for individual influence classes
  	tt <- aggregate(list(VALUE = hiData$calc)
                   ,list(SITE = hiData$SITE
                        ,CLASS = hiData$CLASS
						,coverSuffix = hiData$coverSuffix
		   				,isAg = hiData$isAg
                        )
                   ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
                   )
  	separateMeans <- within(tt
				           ,{METRIC <- gsub('^HI_(.+)$', 'HIPW\\1', CLASS)
				             CLASS <- NULL
						     isAg <- NULL
					   	    }
						   )
						 
  	intermediateMessage('.a')

	# Determine sample sizes for individual influence classes
	tt <- aggregate(list(VALUE = hiData$calc)
				   ,list('SITE'=hiData$SITE
						,'CLASS'=hiData$CLASS
						,coverSuffix = hiData$coverSuffix
						)
				   ,count
				   )
	
	separateCounts <- mutate(tt
	                        ,METRIC = gsub('^HI_(.+)$', 'HIN\\1', CLASS)
	                        ,CLASS = NULL
	                        )
	intermediateMessage('.b')
	
	rc <- rbind(separateMeans, separateCounts)
	
	return(rc)
}


nlaHumanImpact.overallInfluence <- function(hiData)	
# Calculate overall influence indicies, defined as the sum of the individual
# mean influences at a site.  These influences are summed as a whole, and
# separately for agricultural and nonagricultural influences.
{
	hiMeans <- aggregate(list(VALUE = hiData$calc)
						,list(SITE = hiData$SITE
							 ,CLASS = hiData$CLASS
							 ,coverSuffix = hiData$coverSuffix
							 ,isAg = hiData$isAg
							 )
						,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
						)
	hiiAll <- aggregate(list(VALUE = hiMeans$VALUE)
                       ,list('SITE'=hiMeans$SITE, coverSuffix=hiMeans$coverSuffix)
                       ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                       )
    hiiAll$METRIC <- 'HIIALL'
    intermediateMessage('.a')

  
    tt <- aggregate(list(VALUE = hiMeans$VALUE)
                   ,list('SITE'=hiMeans$SITE, coverSuffix=hiMeans$coverSuffix, isAg=hiMeans$isAg)
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                   )
    hiiSep <- within(tt
				    ,{METRIC <- ifelse(isAg, 'HIIAG','HIINONAG')
					  isAg <- NULL
				     }
		            )

    intermediateMessage('.b')

	hiiOverall <- rbind(hiiAll, hiiSep)
	
	return(hiiOverall)
}


nlaHumanImpact.circaInfluence <- function(hiData)
# Calculate circa influence indicies, which limit calculation to include
# only influences found within the plot area at a station.  These are
# the sum of the site mean circa-weights, summed as a whole and 
# separately for agricultural and nonagricultural influences.  Another
# way to think of the means are as the fractional presence of influences
# recorded within the station plot (recorded as 'C' for circa).
{
    hiCircaMeans <- aggregate(hiData$circa
                             ,list(SITE = hiData$SITE
                                  ,CLASS = hiData$CLASS
								  ,coverSuffix = hiData$coverSuffix
								  ,isAg = hiData$isAg
								  )
				 			 ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
							 )
    hiiAllCirca <- aggregate(list(VALUE = hiCircaMeans$x)
                            ,list(SITE = hiCircaMeans$SITE
				                 ,coverSuffix = hiCircaMeans$coverSuffix
			                     )
                            ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
							)
    hiiAllCirca$METRIC <- 'HIIALLCIRCA'
    intermediateMessage('.a')

  
    tt <- aggregate(list(VALUE = hiCircaMeans$x)
                   ,list(SITE = hiCircaMeans$SITE
		                ,coverSuffix = hiCircaMeans$coverSuffix
	  				    ,isAg = hiCircaMeans$isAg
	  				    )
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
		   		   )
    hiiSepCirca <- within(tt
				  	     ,{METRIC <- ifelse(isAg, 'HIIAGCIRCA','HIINONAGCIRCA')
  					   	   isAg <- NULL
					      }
		          	     )
						 
    hiiOverallCirca <- rbind(hiiAllCirca, hiiSepCirca)
    intermediateMessage('.b')
	
	return(hiiOverallCirca)
}

  
nlaHumanImpact.anyPresence <- function(hiData)  
# Calculate fractional presence of any disturbance, both anywhere
# at the station (circa and proximal), and just within the study plot (circa).
{
  	tt <- aggregate(hiData$present
                   ,list('SITE'=hiData$SITE
                      	,'STATION'=hiData$STATION
					  	,coverSuffix = hiData$coverSuffix
                      	)
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                   )
	tt0<-tt
    tt$x <- ifelse(tt$x==0,0,1)	# note: x==NA translates as NA

  	hiAny <- aggregate(list(VALUE = tt$x)
                      ,list('SITE'=tt$SITE, coverSuffix = tt$coverSuffix)
                      ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
                      )
	hiAny$METRIC <- 'HIFPANY'
    intermediateMessage('.a')


  	tt <- aggregate(hiData$circa
                   ,list('SITE'=hiData$SITE
                      	,'STATION'=hiData$STATION
					  	,coverSuffix = hiData$coverSuffix
	  				  	)
                   ,protectedSum, na.rm=TRUE	# sum of all NA is 0, should be NA
                   )
  	tt$x <- ifelse(tt$x==0,0,1)	# note: x==NA translates as NA

  	hiAnyCirca <- aggregate(list(VALUE = tt$x)
                           ,list('SITE'=tt$SITE, coverSuffix = tt$coverSuffix)
                           ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
                           )
  	hiAnyCirca$METRIC <- 'HIFPANYCIRCA'

  	hiAny <- rbind(hiAny, hiAnyCirca)
  	intermediateMessage('.b')

	return(hiAny)
}


nlaHumanImpact.weightedGroupInfluence <- function(hiData)
# Calculate weighted influence of agricultural, nonagricultural and total impacts
{
  	# Calculate mean agricultural influence and sample size
  	hiAg <- subset(hiData, isAg==TRUE)

    agMean <- aggregate(list(VALUE = hiAg$calc)
                 	   ,list('SITE'=hiAg$SITE, coverSuffix = hiAg$coverSuffix)
					   ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
		         	   )
    agMean$METRIC <- 'HIPWAG'

  	agCount <- aggregate(list(VALUE = hiAg$calc)
						,list('SITE'=hiAg$SITE, coverSuffix = hiAg$coverSuffix)
						,count
						)
	agCount$METRIC <- 'HINAG'

  	intermediateMessage('.a')


  	# Calculate mean nonagricultural influence
  	hiNonAg <- subset(hiData, isAg==FALSE)
  	nonagMean <- aggregate(list(VALUE = hiNonAg$calc)
  						  ,list('SITE'=hiNonAg$SITE, coverSuffix = hiNonAg$coverSuffix)
						  ,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
						  )
  	nonagMean$METRIC <- 'HIPWNONAG'
 
	nonagCount <- aggregate(list(VALUE = hiNonAg$calc)
						   ,list('SITE'=hiNonAg$SITE, coverSuffix = hiNonAg$coverSuffix)
				   		   ,count
						   )
	nonagCount$METRIC <- 'HINNONAG'

  	intermediateMessage('.b')


  	# Calculate mean total human influence
  	totMean <- aggregate(list(VALUE = hiData$calc)
  				 		,list('SITE'=hiData$SITE, coverSuffix = hiData$coverSuffix)
		 		 		,protectedMean, na.rm=TRUE	# mean of all NA is NaN, should be NA
		 		 		)
  	totMean$METRIC <- 'HIPWALL'
  
	totCount <- aggregate(list(VALUE = hiData$calc)
  				 	 	 ,list('SITE'=hiData$SITE, coverSuffix = hiData$coverSuffix)
		 		 	 	 ,count
				 	 	 )
	totCount$METRIC <- 'HINALL'

  	intermediateMessage('.c')

	rc <- rbind(agMean, agCount, nonagMean, nonagCount, totMean, totCount)
  	return(rc)
}

# end of file