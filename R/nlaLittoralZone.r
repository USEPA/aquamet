#' @export
#' @title Calculate NLA Littoral Zone Metrics
#' @description This function calculates the littoral zone 
#' portion of the physical habitat metrics for National Lakes Assessment 
#' (NLA) data.  
#' @param littoralFilm A data frame containing surface film type in 
#' the littoral zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE For data collected in 2007, a character value of N (none), 
#' A (algal mat), S (scum), P (oily), or O (other). For data collected in 2012
#' or beyond, a character value of NONE, SCUM, ALGAL_MAT, OILY, or 
#' OTHER_XXXX, where XXXX represents the name of the other film type.
#' }
#' @param data2007 A logical value taking the value TRUE if the data being 
#' processed are from NLA 2007 and FALSE if not. The default value is FALSE.
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
#' LZOFILM, LZFPFILM, LZFPALGAE, LZFPNONE, LZFPOILY, LZFPOTHER, LZFPSCUM 
#'  
#' Descriptions for all NLA metrics can be found at:
#' \href{https://github.com/USEPA/aquamet/blob/master/inst/NLA_physical_habitat_metrics_descriptions.pdf}{NLA_Physical_Habitat_Metric_Descriptions.pdf}.
#' 
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#'   head(nlaPhab)
#'   
#'   # Must subset example dataset to create inputs, keeping only SITE, STATION,
#'   #  and VALUE
#'   surfFilm <- subset(nlaPhab,PARAMETER=='SURFACE_FILM',select=-PARAMETER)
#'
#'   exLitZone <- nlaLittoralZone(surfFilm, data2007=FALSE)
#'   
#'   head(exLitZone)


nlaLittoralZone <- function(littoralFilm, data2007=FALSE
                           ,isUnitTest = FALSE
                           ) {

################################################################################
# Function: nlaLittoralZone
# Title: Calculate NLA Littoral Zone Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 14, 2008
# Description:
#   This function calculates the littoral zone portion of the physical
#   habitat metrics for National Lakes Assessment (NLA) data.  The function
#   requires a data frame containing validated physical habitat data collected
#   using the NLA protocol.
# Function Revisions:
#   10/14/08 cws: Started.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   12/19/13 cws: Developed unit test using 2007 and 2012 data and refactored. 
#            Upcased column names and metric names; now returning long dataframe  
#            instead of wide.  Added LZFPALGAE, LZFPNONE, LZFPOILY, LZFPOTHER, 
#            LZFPSCUM calculations and removed LZIFILMVARIETY from results when 
#            using non-2007 data. Using modalValues() instead of modalvalue()
#            to allow reporting of ties in LZOFILM.  Regression tested results 
#            with entire 2007 data successfully, with the following expected 
#            differences: 177 rows in the 2007 results for 59 sites which do not 
#            have any SURFACE_FILM values recorded, and 3 different values each 
#            of LZFPFILM and LZIFILMVARIETY due to floating point truncation, 
#            and 18 different values of LZOFILM because those sites have 
#            multiple modal values.
#   01/30/14 cws: Removed commented out code.
#   06/12/14 tmk: Removed calls to the require() function.
#    7/14/17 cws Renamed metsLittoralZone to nlaLittoralZone. Updated call interface
#            which required little work because there's only one kind of data
#    3/19/19 cws Added isUnitTest argument for consistency.
#    3/21/19 cws Added validation checking of data argument.
#
# Arguments:
#   df = a data frame containing littoral zone data.  The data frame must
#     include columns that are named as follows:
#       SITE - universal ID value, which uniquely identifies the site location, 
#             date of visit, visit order, habitat type, etc. for which metrics 
#             will be calculated.  For NLA, site ID, year and visit number are
#             used for this purpose.
#       STATION - the subordinate ID value, which identifies the location,
#               habitat type, order of occurence, etc. within a single SITE.
#               For NLA, transect is used for this purpose.
#       VALUE - parameter values, which are the values used in calculations.
#   data2007 = a logical value, which equals TRUE if 2007 data is being
#     processed.  The default value is FALSE.
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
    intermediateMessage('NLA littoral zone metrics', loc='start')

    if(data2007) {
        df <- aquametStandardizeArgument(littoralFilm
                                        ,struct = list(SITE=c('integer','character'), STATION='character', VALUE='character')
                                        ,legalValues = list(VALUE = c(NA,'','A','N','P','O','S'))
                                        ,stopOnError = !isUnitTest
                                        )
    } else {
         df <- aquametStandardizeArgument(littoralFilm
                                        ,struct = list(SITE=c('integer','character'), STATION='character', VALUE='character')
                                        ,legalValues = list(VALUE = c('^(|ALGAL_MAT|NONE|OILY|OTHER.*|SCUM)$', isrx=TRUE))
                                        ,stopOnError = !isUnitTest
                                        )
    }

	lzData <- nlaLittoralZone.prepareInput(df, data2007)
    intermediateMessage('.1')


    # Determine mode(s) of the surface film type
	modeFilm <- nlaLittoralZone.modeFilm(lzData, data2007)
	intermediateMessage('.2')


    # Determine fractional presence of any type of film other than None
	presences <- nlaLittoralZone.fractionalPresences(lzData, data2007)
    intermediateMessage('.3')


    # Determine mean number of any type of film other than None occuring
    # at a station
	variety <- nlaLittoralZone.filmVariety(lzData, data2007)
    intermediateMessage('.4')


    # Assemble nla and send 'em back.
    lz <- rbind(modeFilm, presences, variety)
    intermediateMessage(' Done.', loc='end')

    return(lz)
}

#' @keywords internal
nlaLittoralZone.prepareInput <- function(lzData, data2007)
# Prepare Littoral zone data for subsequent calculations.
# Returns standardized dataframe with columns SITE, STATION, VALUE.
{
	if(data2007) {
		
		lzData <- within(lzData
			 	 		,VALUE <- ifelse(VALUE=='A', 'ALGAL_MAT'
								 ,ifelse(VALUE=='N', 'NONE'
								 ,ifelse(VALUE=='',  'NONE'					  
								 ,ifelse(VALUE=='O', 'OTHER'
								 ,ifelse(VALUE=='P', 'OILY'
								 ,ifelse(VALUE=='S', 'SCUM', 'UNKNOWN'
						    	  ))))))
						)
						
	} else {
		
		lzData$VALUE <- with(lzData, ifelse(grepl('^OTHER.+$', VALUE), 'OTHER', VALUE))
		
	}

	return(lzData)
}

#' @keywords internal
nlaLittoralZone.modeFilm <- function(df, data2007)
# Determine most common film type.  Returns dataframe with
# columns SITE, METRIC, VALUE
{
	modeFilm <- aggregate(list(VALUE=df$VALUE)
						 ,list(SITE=df$SITE)
						 ,modalValues, na.rm=TRUE
						 )
	modeFilm$METRIC <- 'LZOFILM'
	modeFilm$VALUE <- as.character(modeFilm$VALUE)	# modalvalue() returns factor, darn it.
	
	if(data2007) {
		modeFilm$VALUE <- with(modeFilm
						       , gsub('ALGAL_MAT',	'A'
							    ,gsub('NONE', 	 	'N'
							    ,gsub('OTHER', 	 	'O'
							    ,gsub('OILY', 	 	'P'
							    ,gsub('SCUM', 	 	'S', VALUE
												)))))
							  )
	}
	
	return(modeFilm)
}

#' @keywords internal
nlaLittoralZone.fractionalPresences <- function(df, data2007)
# Determines fractional presences of film types, including 'any' film other 
# than NONE. Returns dataframe with columns SITE, METRIC, VALUE.
{
	df$any <- with(df, ifelse(VALUE=='NONE', 0, 1))
	fpAny <- aggregate(list(VALUE=df$any)
					  ,list(SITE=df$SITE)
					  ,protectedMean, na.rm=TRUE
					  )
	fpAny$METRIC <- 'LZFPFILM'
	
	
	if(data2007) {
		fpIndivs <- NULL
	} else {
		
		tt <- aggregate(list(VALUE = df$VALUE=='ALGAL_MAT')
				,list(SITE=df$SITE)
				,protectedMean, na.rm=TRUE
		)
		fpAlgae <- within(tt, METRIC <- 'LZFPALGAE')
		
		tt <- aggregate(list(VALUE = df$VALUE=='NONE')
				,list(SITE=df$SITE)
				,protectedMean, na.rm=TRUE
		)
		fpNone <- within(tt, METRIC <- 'LZFPNONE')
		
		tt <- aggregate(list(VALUE = df$VALUE=='OILY')
				,list(SITE=df$SITE)
				,protectedMean, na.rm=TRUE
		)
		fpOily <- within(tt, METRIC <- 'LZFPOILY')
		
		tt <- aggregate(list(VALUE = df$VALUE=='OTHER')
				,list(SITE=df$SITE)
				,protectedMean, na.rm=TRUE
		)
		fpOther <- within(tt, METRIC <- 'LZFPOTHER')
		
		tt <- aggregate(list(VALUE = df$VALUE=='SCUM')
				,list(SITE=df$SITE)
				,protectedMean, na.rm=TRUE
		)
		fpScum <- within(tt, METRIC <- 'LZFPSCUM')
		
		fpIndivs <- rbind(fpAlgae, fpNone, fpOily, fpOther, fpScum)
		
	}
	
	rc <- rbind(fpAny, fpIndivs)
	
	return(rc)
}

#' @keywords internal
nlaLittoralZone.filmVariety <- function(df, data2007)
# Determines variety of film types, including 'any' film other 
# than NONE. Returns dataframe with columns SITE, METRIC, VALUE.
#
# NOTE: This metric is deprecated from post-2007 calculations as it 
# is neither well defined nor calculated correctly. 
{
	if(data2007) {
		
		df$any <- with(df, ifelse(VALUE=='NONE', 0, 1))
		tt <- aggregate(df$any
					   ,list(SITE=df$SITE, STATION=df$STATION)
					   ,sum, na.rm=TRUE
					   )
		variety <- aggregate(list(VALUE=tt$x)
							,list(SITE=tt$SITE)
							,mean, na.rm=TRUE
							)
		variety$METRIC <- 'LZIFILMVARIETY'

	} else {
		
		variety <- NULL	# metric is deprecated
		
	}
	
	return(variety)	
}

# end of file