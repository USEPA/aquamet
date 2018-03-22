#' @export
#' @title Calculate NLA Station Information Metrics
#' @description This function calculates the station information portion 
#' of the physical habitat metrics for National Lakes Assessment (NLA) data.
#' The function requires a data frame containing validated physical habitat 
#' data collected using the NLA protocol.  The passed data frame must contain 
#' records for all stations sampled at each site, or the station counts used 
#' to calculate island metrics will not be correct.  
#' @param isIsland A data frame containing value indicating whether station is
#' on an island, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE character value of N or NO if not an island and Y or YES if 
#' station is located on an island.
#' }
#' @param stationDepth A data frame containing depth in meters at the station, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a numeric values or a character value castable to numeric 
#' containing the depth in meters at the station 
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
#' SIFPISLAND, SINDEPTH, SIVDEPTH, SIXDEPTH.
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
#'   isIsland <- subset(nlaPhabEx,PARAMETER=='ISLAND',select=-PARAMETER)
#'   depth <- subset(nlaPhabEx,PARAMETER=='DEPTH_AT_STATION',select=-PARAMETER)
#'   
#'   exStationInfo <- nlaStationInformation(isIsland, depth)
#'   
#'   head(exStationInfo)
#'  
#' @keywords survey
nlaStationInformation <- function(isIsland = NULL, stationDepth = NULL) {

################################################################################
# Function: nlaStationInformation
# Title: Calculate NLA Station Information Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 14, 2008
# Description:
#   This function calculates the station information portion of the physical
#   habitat metrics for National Lakes Assessment (NLA) data.  The function
#   requires a data frame containing validated physical habitat data collected
#   using the NLA protocol.  The passed data frame must contain records for all
#   stations sampled at each site, or the station counts used to calculate
#   island metrics will not be correct.  Note: The passed data frame contains
#   records for all stations sampled at each site, or the station counts used to
#   calculate island mets will not be correct.
# Function Revisions:
#   10/14/08 cws: Started.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   01/28/14 cws: Started unit test development.
#   01/30/14 cws: Discovered that the station count used to determine SIFPISLAND 
#            is dependent on how (and whether) the input dataframe is subsetted 
#            from the body of phab data.  The 2007 unit test input data for this 
#            was modified to account for this problem.   Unit test for 2012 data
#            added, along with data standardization to convert 2012 data to 2007
#            coding.  Regression to 2007 results shows the following 
#            differences: There are 171 absent SI[NXV]DEPTH values due to 62 
#            UIDs that do not have any DEPTH_AT_STATION values recorded.  These 
#            differences are expected.  There are another 5 cases of SIFPISLAND 
#            (with values of zero) which are absent in earlier calculations. 
#            There are 5 cases of SIFPISLAND (7771,8017,8042,8177,8659) with 
#            different values suggesting a change in the denominator (number of 
#            transects).  I have not been able to track down the cause of this, 
#            however the old metrics code from 2007 agrees with the new values 
#            for these sites.  There are 60 differences in SI[NVX]DEPTH due 
#            solely to floating point precision differences.
#   06/12/14 tmk: Removed calls to the require() function.
#   07/18/17 cws Updated calling interface after creating this from 
#            metsStationInformation.
#    7/19/17 cws Changed SIFPISLAND calculation to use only those values. Previous
#            code was based on calculation for 2007 coding of ISLAND, which
#            contained only true ('X') values, leaving both unrecorded values
#            and false values to be recorded as absences. Calcualtion of 
#            frequencies requires use of other data values to determine the 
#            total number of stations sampled. Since 2012, false/NO values are
#            explicitely coded. Since these codings require different handling,
#            this function is changed to expect both Y/YES and N/NO values. Two
#            additional sites with NA values were added in the 2012 data to 
#            exercise the cases where NA values occur.
#
# Arguments:
#   df = a data frame containing station information data.  The data frame must
#     include columns that are named as follows:
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
#                   argument has the following values: ISLAND and
#                   DEPTH_AT_STATION.
#       VALUE - parameter values, which are the values used in calculations.
#       UNITS - parameter units, which identifies the units in which the
#               parameter values are recorded.
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
    intermediateMessage('NLA station information metrics', loc='start')
    
    addClass <- function(df, ...) {
        
        args <- list(...)
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(CLASS = args[[1]])
        return(rc)
    }
    
    isIsland <- isIsland %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), 'ISLAND')
    stationDepth <- stationDepth %>% aquametStandardizeArgument(ifdf=addClass, struct=list(SITE=c('integer','character'), STATION='character', VALUE=c('double','character')), 'DEPTH_AT_STATION')
    df <- rbind(isIsland, stationDepth)

    if(is.null(isIsland)){
        island <- data.frame(SITE=as.integer(NULL), METRIC=as.character(NULL), VALUE=as.character(NULL), stringsAsFactors=FALSE)
    }else{
        island <- nlaStationInformation.islandStations(isIsland)
    }
    if(is.null(stationDepth)){
        stationDepths <- data.frame(SITE=as.integer(NULL), METRIC=as.character(NULL), VALUE=as.character(NULL), stringsAsFactors=FALSE)
    }else{
    	stationDepths <- nlaStationInformation.stationDepths(stationDepth)
    }
	rc <- within(rbind(island, stationDepths), VALUE <- as.character(VALUE))

	intermediateMessage(' Done.', loc='end')
	
	return(rc)
}

nlaStationInformation.islandStations <- function(df)
# calculate mets for stations at islands
{
    intermediateMessage('.1')
    siIsland <- df %>% 
                mutate(atIsland = ifelse(is.na(VALUE)       , NA
                                 ,ifelse(grepl('^Y.*',VALUE), TRUE, FALSE
                                  ))
                      ) %>% 
                group_by(SITE) %>% 
                summarise(VALUE=protectedMean(atIsland, na.rm = TRUE)) %>% 
                mutate(METRIC = 'SIFPISLAND') %>% 
                as.data.frame()
    intermediateMessage('.2')
# 	# Standardize values by converting to 2007-esque organization:
# 	# Y,YES becomes X, otherwise they are NA
# 	standardized <- within(df
# 						  ,VALUE <- ifelse(CLASS=='ISLAND' & 
# 										    (VALUE=='X' | grepl('^Y.*',VALUE))
# 				  						   ,'X'
# 										   ,NA
# 						   				   )
# 						  )
# 
# 	# Determine number of stations in each lake
#     ss <- aggregate(list(nSta = standardized$STATION)
#                    ,list(SITE = standardized$SITE)
#                    ,function(x) { count(unique(x)) }
#                    )
# 
#     intermediateMessage('.1')
# 
# 
#   	# Count islands in each site, and determine their fractional presence
#   	IslData <- subset(standardized, CLASS=='ISLAND')
# 
#   	nn <- aggregate(list(nIsl = I(IslData$VALUE))
#                    ,list(SITE = IslData$SITE)
#                    ,count
#                    )
# 
#     siIsland <- merge(ss, nn, by='SITE', all=TRUE, sort=FALSE)
#     siIsland <- within(siIsland
# 					  ,{VALUE <- ifelse(is.na(nIsl), 0, nIsl / nSta)
# 					    METRIC <- 'SIFPISLAND'
# 					    nIsl <- NULL
# 					    nSta <- NULL
# 				  	   }
# 		  			  )
#     intermediateMessage('.2')
# 
 	return(siIsland)
}


nlaStationInformation.stationDepths <- function(df)
# calculate mets for stations at islands
{
	# # Standardize to include UNITS column if absent, and populate
	# # it with assumed measurement units
	# if('UNITS' %nin% names(df)) {
	# 	df$UNITS <- 'm'
	# }
	
  	# Characterize station depths, converting ft to m as necessary
  	depths <- subset(df, CLASS=='DEPTH_AT_STATION')
  	depths$VALUE <- as.numeric(depths$VALUE)
  	# depths$UNITS <- trimws(tolower(depths$UNITS))
  # 	depths$VALUE <- ifelse(depths$UNITS=='ft'
  #                          ,as.numeric(depths$VALUE) * 0.3048
  #                          ,as.numeric(depths$VALUE)
  #                          )
  #   depths$VALUE[!(depths$UNITS %in% c('m','ft',''))] <- NA

  	xDepth <- aggregate(list(VALUE = as.numeric(depths$VALUE))
                       ,list(SITE = depths$SITE)
                       ,mean, na.rm=TRUE
                       )
  	xDepth$METRIC <- 'SIXDEPTH'

	
  	vDepth <- aggregate(list(VALUE = as.numeric(depths$VALUE))
                       ,list(SITE = depths$SITE)
                       ,sd, na.rm=TRUE
                       )
	vDepth$METRIC <- 'SIVDEPTH'
	
	
  	nDepth <- aggregate(list(VALUE = as.numeric(depths$VALUE))
                       ,list(SITE = depths$SITE)
                       ,count
                       )
	nDepth$METRIC <- 'SINDEPTH'
					 
  	intermediateMessage('.3')

  
  	# Combine calculations and return
	stationDepths <- rbind(xDepth, vDepth, nDepth)

  	return(stationDepths)
}



# end of file
