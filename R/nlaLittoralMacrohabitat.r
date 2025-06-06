#' @export
#' @title Calculate NLA Littoral Macrohabitat Metrics
#' @description This function calculates the littoral fish macrohabitat 
#' portion of the physical habitat metrics for National Lakes Assessment 
#' (NLA) data. These data were only collected in NLA 2007.  
#' @param artificial A data frame containing artificial fish cover type 
#'  values from the littoral zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of X representing the presence of that 
#' cover type.
#' }
#' @param boulders A data frame containing boulder fish cover type
#' values from littoral zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of X representing the presence of that 
#' cover type.
#' }
#' @param coverExtent A data frame containing cover class values from 
#' littoral zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of 'CONTINUOUS COVER', 'PATCHY COVER', 
#'  or 'NO COVER'.
#' }
#' @param humanDisturbance A data frame containing human disturbance class
#'  values from the littoral zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of NONE, LOW, MODERATE, or HEAVY.
#' }
#' @param noCover A data frame containing values indicating no fish cover in
#' littoral zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of X representing the no other cover types
#' selected.
#' }
#' @param substrate A data frame containing dominant substrate values 
#' from littoral zone, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of B (bedrock), C (cobble/boulder), 
#' M (mud/muck), or S (sand/gravel) representing the dominant substrate type.
#' }
#' @param vegetation A data frame containing vegetation littoral fish cover
#'  values, with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of X representing the vegetation fish cover 
#' types selected.
#' }
#' @param woody A data frame containing woody littoral fish cover values, 
#' with the columns:
#' \itemize{
#' \item SITE an integer or character value identifying a single site 
#' visit.
#' \item STATION a character value identifying the station within the SITE
#' \item VALUE a character value of X representing the woody fish cover 
#' types selected.
#' }
#' @param dataInformation A data frame used to convert between 
#' cover class codes to characteristic cover values. Expected to contain
#' the following columns:
#' \itemize{
#'        \item value - values containing all expected values
#'        \item weights - numeric values used to calculate numeric 
#'                   metrics.
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
#' LMFPBEDROCK,, LMFPCOBBLE,, LMFPMUD,, LMFPSAND, LMNSUBSTRATE, LMOSUBSTRATE, 
#' LMOCOVER, LMNCOVER, LMFPLITTLE, LMFPPATCHY, LMFPCONTINUOUS, LMPWHUMAN, 
#' LMNHUMAN, LMNCOVERTYPES, LMFPARTIFICIAL, LMFPWOODY, LMFPVEG, LMFPBOULDERS, 
#' LMFPNONE
#'  
#' Descriptions for all NLA metrics can be found at:
#' \href{https://github.com/USEPA/aquamet/blob/master/inst/NLA_physical_habitat_metrics_descriptions.pdf}{NLA_Physical_Habitat_Metric_Descriptions.pdf}.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}
#' @examples
#'   head(nlaPhabEx07)
#'   
#'   # Must subset example dataset to create inputs, keeping only SITE, STATION,
#'   #  and VALUE
#'   artificial <- subset(nlaPhabEx07,PARAMETER=='COVER_ARTIFICIAL',select=-PARAMETER)
#'   boulders <- subset(nlaPhabEx07,PARAMETER=='COVER_BOULDERS',select=-PARAMETER)
#'   coverExtent <- subset(nlaPhabEx07,PARAMETER=='COVER_CLASS',select=-PARAMETER)
#'   humanDisturbance <- subset(nlaPhabEx07,PARAMETER=='HUMAN_DISTURBANCE',select=-PARAMETER)
#'   noCover <- subset(nlaPhabEx07,PARAMETER=='COVER_NONE',select=-PARAMETER)
#'   substrate <- subset(nlaPhabEx07,PARAMETER=='DOM_SUBSTRATE',select=-PARAMETER)
#'   vegetation <- subset(nlaPhabEx07,PARAMETER=='COVER_VEG',select=-PARAMETER)
#'   woody <- subset(nlaPhabEx07,PARAMETER=='COVER_WOODY',select=-PARAMETER)
#'
#'   exLitMacro <- nlaLittoralMacrohabitat(artificial,boulders,coverExtent,humanDisturbance,
#'   noCover,substrate,vegetation,woody)
#'   
#'   head(exLitMacro)


nlaLittoralMacrohabitat <- function(artificial = NULL
                                   ,boulders = NULL
                                   ,coverExtent = NULL
                                   ,humanDisturbance = NULL
                                   ,noCover = NULL
                                   ,substrate = NULL
                                   ,vegetation = NULL
                                   ,woody = NULL
	                               ,dataInformation = data.frame(value=c('NONE','LOW','MODERATE','HEAVY')
			                                                    ,weights =c(0, 0.2, 0.5, 1.0)
			                                                    ,stringsAsFactors=FALSE
                                                                )
                                   ,isUnitTest = FALSE
                                   ) {

################################################################################
# Function: nlaLittoralMacrohabitat
# Title: Calculate NLA Littoral Fish Macrohabitat Metrics
# Programmers: Curt Seeliger
#              Tom Kincaid
# Date: October 7, 2008
# Description:
#   This function calculates the littoral fish macrohabitat portion of the
#   physicalhabitat metrics for National Lakes Assessment (NLA) data.  The
#   function requires a data frame containing validated physical habitat data
#   collected using the NLA protocol.
# Function Revisions:
#   10/07/08 cws: Added lmnHuman calculation.
#   10/21/08 cws: Correcting counts of missing parameters.  These are now zero.
#   11/04/08 cws: Correcting value of highest rank of HUMAN_DISTURBANCE from
#            HIGH to HEAVY.
#   11/07/08 cws: Renaming lmnClass to lmnCover, renaming old lmnCover to 
#            lmnCoverTypes.  
#   11/12/08 cws: Allowing lmoCover to have multiple values just like other
#            mode mets.
#   03/08/13 cws: Copied from 2007 study and renamed.
#   02/19/14 cws: Recreated from R environment saved 5 February after 
#            accidentally blowing away the source files on the 3rd. holy shit
#   02/20/14 cws: Metrics not used as 2012 phab does not include these data. 
#   06/12/14 tmk: Removed calls to the require() function.
#   07/14/17 cws Renamed metsLittoralMacrohabitat to nlaLittoralMacrohabitat.
#            Changed UID to SITE, RESULT to VALUE, and output uses METRIC instead
#            of PARAMETER. Updated calling interface.
#    3/19/19 cws Added isUnitTest argument for consistency.
#    3/22/19 cws Added validation of data args. Added humanDisturbanceWeights
#            metadata argument. Modified to use dplyr::rename.
#    3/26/19 cws Removed errant argument 'strings'.
#    3/28/19 cws Standardized metadata argument naming
#    9/28/20 cws Removing reshape2 functions in favour of tidyr due to deprecation.
#   10/02/20 cws Removed commented out code using reshape2 functions.
#    7/01/21 cws Converted tidyr functions gather to pivot_longer and spread to
#            pivot_wider
#
# Arguments:
#   df = a data frame containing littoral fish macrohabitat data.  The
#     data frame must include columns that are named as follows:
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
#                   argument has the following values: DOM_SUBSTRATE,
#                   COVER_CLASS, HUMAN_DISTURBANCE, COVER_ARTIFICIAL,
#                   COVER_BOULDERS, COVER_FILL, COVER_NONE, COVER_VEG,
#                   and COVER_WOODY.
#       VALUE - parameter values, which are the values used in calculations.
# Output:
#   A data frame that contains the following columns:
#     SITE - universal ID value
#     METRIC - metric name
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

    # Print initial messages
    intermediateMessage('NLA littoral fish macrohabitat metrics', loc='start')
    addParameter <- function(df, ...) {
        
        args <- list(...)
        if(is.null(args)) return(NULL)
        else if(all(is.na(args))) return(NULL)
        
        rc <- df %>% mutate(PARAMETER = args[[1]])
        return(rc)
    }
    
    humanDisturbanceWeights <- aquametStandardizeArgument(dataInformation, struct=list(value='character', weights='double'), legalValues=list(value=c('NONE','LOW','MODERATE','HEAVY')), rangeLimits=list(weights=c(0,1)), stopOnError = !isUnitTest)
    
    artificial <- aquametStandardizeArgument(artificial, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'','X')), 'COVER_ARTIFICIAL', stopOnError = !isUnitTest)
    boulders <- aquametStandardizeArgument(boulders, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'','X')),       'COVER_BOULDERS', stopOnError = !isUnitTest)
    coverExtent <- aquametStandardizeArgument(coverExtent, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'','NO COVER','PATCHY COVER','CONTINUOUS COVER')), 'COVER_CLASS', stopOnError = !isUnitTest)
    humanDisturbance <- aquametStandardizeArgument(humanDisturbance, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'','X', dataInformation$value)),    'HUMAN_DISTURBANCE', stopOnError = !isUnitTest)
    noCover <- aquametStandardizeArgument(noCover, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'','X')),         'COVER_NONE', stopOnError = !isUnitTest)
    substrate <- aquametStandardizeArgument(substrate, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'','B','C','M','S')),    'DOM_SUBSTRATE', stopOnError = !isUnitTest)
    vegetation <- aquametStandardizeArgument(vegetation, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'','X')),   'COVER_VEG', stopOnError = !isUnitTest)
    woody <- aquametStandardizeArgument(woody, ifdf=addParameter, struct=list(SITE=c('integer','character'), STATION='character', VALUE='character'), legalValues=list(VALUE=c(NA,'','X')),             'COVER_WOODY', stopOnError = !isUnitTest)

    humanDisturbanceWeights <- dplyr::rename(dataInformation, field=value, calc=weights)    
    df <- rbind(artificial, boulders, coverExtent, humanDisturbance, noCover, substrate, vegetation, woody)
    
	substrate <- nlaLittoralMacrohabitat.substrate(df)
	cover <- nlaLittoralMacrohabitat.cover(df)
	humanDist <- nlaLittoralMacrohabitat.humanDist(df, humanDisturbanceWeights)
	coverTypes <- nlaLittoralMacrohabitat.coverTypes(df)

	rc <- rbind(substrate, cover, humanDist, coverTypes)
	
	intermediateMessage(' Done.', loc='end')
	
	return(rc)
}

#' @keywords internal
nlaLittoralMacrohabitat.cover <- function(df)
# Cover class calculations using COVER_CLASS
{
	coverClass <- subset(df, PARAMETER=='COVER_CLASS')
	
	# No metrics if no data
	if(nrow(coverClass) == 0) return(NULL)
	
	
	countCover <- aggregate(list(VALUE = coverClass$VALUE)
			,list(SITE = coverClass$SITE)
			,count
	)
	countCover <- within(countCover
			,{METRIC <- 'LMNCOVER'
				VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			}
	)
	
	
	noCover <- aggregate(list(VALUE = coverClass$VALUE=='NO COVER')
			,list(SITE = coverClass$SITE)
			,mean, na.rm=TRUE
	)
	noCover <- within(noCover
			,{METRIC <- 'LMFPLITTLE'
				VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			}
	)
	
	
	patchyCover <- aggregate(list(VALUE = coverClass$VALUE=='PATCHY COVER')
			,list(SITE = coverClass$SITE)
			,mean, na.rm=TRUE
	)
	patchyCover <- within(patchyCover
			,{METRIC <- 'LMFPPATCHY'
				VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			}
	)
	
	
	contCover <- aggregate(list(VALUE = coverClass$VALUE=='CONTINUOUS COVER')
			,list(SITE = coverClass$SITE)
			,mean, na.rm=TRUE
	)
	contCover <- within(contCover
			,{METRIC <- 'LMFPCONTINUOUS'
				VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			}
	)
	
	
	# Determine cover amount mode
	tt <- within(rbind(noCover, patchyCover, contCover)
			,coverAmount <- ifelse(METRIC == 'LMFPCONTINUOUS', 'CONTINUOUS COVER'
					,ifelse(METRIC == 'LMFPLITTLE', 'NO COVER'
							,ifelse(METRIC == 'LMFPPATCHY', 'PATCHY COVER', 'UNKNOWN COVER'
							))) 
	)
	modeCover <- within(modalClasses(tt, 'coverAmount', 'VALUE')
			,{VALUE <- modalClasses
				METRIC <- 'LMOCOVER'
				modalClasses <- NULL
			}
	)
	
	
	rc <- rbind(modeCover, countCover, noCover, patchyCover, contCover)
	
	intermediateMessage('.3')
	
	return(rc)
}

#' @keywords internal
nlaLittoralMacrohabitat.coverTypes <- function(df)
# Cover types.  Zero or more are recorded at each station, though ideally at least one is.
{
	coverTypes <- subset(df, PARAMETER %in% c('COVER_ARTIFICIAL', 'COVER_BOULDERS'
					,'COVER_FILL', 'COVER_NONE'
					,'COVER_VEG', 'COVER_WOODY'
			)
			,select=c('SITE','STATION','PARAMETER','VALUE')
	)
	
	# No mets if no data
	if(nrow(coverTypes) == 0) return(NULL)				
	
	
	# Count stations at which we have ANY cover types recorded
	nCoverLocs <- aggregate(list(VALUE = I(coverTypes$STATION))
			,list(SITE = coverTypes$SITE)
			,function(x) #count
			{
				return(length(unique(x)))
			}
	)
	nCoverLocs$METRIC <- 'LMNCOVERTYPES'
	
	
	tt <- aggregate(list(VALUE = I(coverTypes$VALUE=='X'))
			,list('SITE'=coverTypes$SITE, 'PARAMETER'=coverTypes$PARAMETER)
			,count
	)
	
	tt <-merge(tt, within(nCoverLocs, {totN <- VALUE; METRIC <- NULL; VALUE <- NULL;})
			,by='SITE'
			,all.x=TRUE
			,sort=FALSE
	)
	tt$VALUE <- ifelse(tt$totN==0
			,0
			,tt$VALUE / tt$totN
	)
	tt$VALUE[is.na(tt$VALUE)] <- 0
	
	typeMeans <- tt %>% select(SITE, PARAMETER, VALUE) %>%
	             mutate(PARAMETER = ifelse(PARAMETER == 'COVER_ARTIFICIAL', 'LMFPARTIFICIAL'
	                               ,ifelse(PARAMETER == 'COVER_BOULDERS', 'LMFPBOULDERS'
	                               ,ifelse(PARAMETER == 'COVER_FILL', 'LMFPFILL'
	                               ,ifelse(PARAMETER == 'COVER_NONE', 'LMFPNONE'
	                               ,ifelse(PARAMETER == 'COVER_VEG', 'LMFPVEG'
	                               ,ifelse(PARAMETER == 'COVER_WOODY', 'LMFPWOODY', 'IGNORED_VALUE' # (if any)
	                                ))))))
	                   ) %>%
	             subset(PARAMETER != 'IGNORED_VALUE') %>%
                 # tidyr::spread(PARAMETER, VALUE)
	             tidyr::pivot_wider(names_from=PARAMETER, values_from=VALUE)
	
	typeMeans <- typeMeans %>%
	             # tidyr::gather(PARAMETER, VALUE, -SITE) %>%
	             tidyr::pivot_longer(c(-SITE), names_to='PARAMETER', values_to='VALUE') %>%
	             mutate(PARAMETER = as.character(PARAMETER)) %>% 
	             dplyr::rename(METRIC=PARAMETER)

	intermediateMessage('.6')
	
	rc <- rbind(nCoverLocs, typeMeans)
	intermediateMessage('.7')
	
	return(rc)
}

#' @keywords internal
nlaLittoralMacrohabitat.humanDist <- function(df, hdWeights)
# Human disturbance level mets based on HUMAN_DISTURBANCE
{
	humDist    <- subset(df
			,PARAMETER=='HUMAN_DISTURBANCE'
			,select=c('SITE','STATION','PARAMETER','VALUE')
	)
	
	# No mets if no data
	if(nrow(humDist) == 0) return(NULL)				
	
	humDist <- merge(humDist, hdWeights, by.x='VALUE', by.y='field')

	weightedDist <- aggregate(list(VALUE = humDist$calc)
			,list(SITE = humDist$SITE)
			,mean, na.rm=TRUE
	)
	weightedDist$METRIC <- 'LMPWHUMAN'
	
	
	nWeightedDist <- aggregate(list(VALUE = humDist$calc)
			,list(SITE = humDist$SITE)
			,count
	)
	nWeightedDist$METRIC <- 'LMNHUMAN'
	
	intermediateMessage('.5')
	
	rc <- rbind(weightedDist, nWeightedDist)
	return(rc)
}

#' @keywords internal
nlaLittoralMacrohabitat.substrate <- function(df)
# Determine fractional presence of various substrates
{
	domSub <- subset(df, PARAMETER=='DOM_SUBSTRATE')
	
	# No mets if no data
	if(nrow(domSub) == 0) return(NULL)
	
	countSub <- aggregate(list(VALUE = domSub$VALUE)
			,list(SITE=domSub$SITE)
			,count
	)
	countSub <- within(countSub
			,{METRIC <- 'LMNSUBSTRATE'
				VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			}
	)
	
	bSub <- aggregate(list(VALUE = domSub$VALUE=='B')
			,list(SITE = domSub$SITE)
			,mean, na.rm=TRUE
	)
	bSub <- within(bSub
			,{METRIC <- 'LMFPBEDROCK'
				VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			}
	)
	
	cSub <- aggregate(list(VALUE = domSub$VALUE=='C')
			,list(SITE =domSub$SITE)
			,mean, na.rm=TRUE
	)
	cSub <- within(cSub
			,{METRIC <- 'LMFPCOBBLE'
				VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			}
	)
	
	mSub <- aggregate(list(VALUE = domSub$VALUE=='M')
			,list(SITE = domSub$SITE)
			,mean, na.rm=TRUE
	)
	mSub <- within(mSub
			,{METRIC <- 'LMFPMUD'
				VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			}
	)
	
	sSub <- aggregate(list(VALUE = domSub$VALUE=='S')
			,list(SITE = domSub$SITE)
			,mean, na.rm=TRUE
	)
	sSub <- within(sSub
			,{METRIC <- 'LMFPSAND'
				VALUE <- ifelse(is.na(VALUE), 0, VALUE)
			}
	)
	
	intermediateMessage('.1')
	
	fracSubs <- rbind(bSub, cSub, mSub, sSub, countSub)
	
	
	# determine substrate type mode
	tt <- within(subset(fracSubs, grepl('^LMFP', METRIC))
			,type <- gsub('^LMFP(.+)$', '\\1', METRIC)
	)
	tt <- within(tt, type <- capitalize(tolower(type)))	
	
	modeSub <- modalClasses(tt, "type", "VALUE")
	modeSub <- within(modeSub
			,{METRIC <- 'LMOSUBSTRATE'
			 	VALUE <- gsub('Cobble', 'C'	# convert to 2007 expectations
						,gsub('Mud', 'M'
						,gsub('Sand', 'S'
						,gsub('Bedrock', 'B', modalClasses
						 ))))
				modalClasses <- NULL
			}
	)
	
	
	intermediateMessage('.2')
	
	rc <- rbind(fracSubs, modeSub)
	return(rc)
}

# end of file
