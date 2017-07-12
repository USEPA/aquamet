################################################################################
# sharedSupport.r
#
# Contains functions and definitions of general utility for the EPA assessment
# metrics calculations
#
# 07/28/08 cws created
# 09/17/08 cws removed inclusion of _nlaOptions.r
#  9/18/08 cws rewrote rename() to correctly rename columns.
#  9/22/08 cws rewrote modalClass() to simplify it and to correctly assign
#          class names to the modal values.  Like the previous fix to rename(),
#          this problem was based on an incorrect assumption about the order 
#          of column names, and in this case was fixed by enforcing the 
#          expected order in the function.  Created renameTest(), for obvious 
#          reasons, and funcTestOutput() to support it.
# 10/09/08 cws Added definition of normalizedCover().
# 10/14/08 cws fixed normalizedCover() bug which resulted in tt$.coverNorm
#          being type data.frame, which later prevented merge() or rbind() from 
#          completing within memory constraints.  tt[coverValue] required
#          unlist()ing to result in proper type of tt$.coverNorm.
# 10/17/08 cws idr() changed to use type 2 quantile calculation method, as this
#          matches SAS output using proc univariate.  Created iqr() to calculate
#          IQR using choice of methods.
# 10/20/08 cws Modified normalizedCover() to allow classes summing to less than
#          100%, for those cases where the categories do not exhaust all 
#          possibilities.
# 10/21/08 cws Completed normalizedCoverTest().
# 10/29/08 cws Removed deprecated functions, added nlaWiden and nlaLengthen
# 11/18/08 cws Added definitions for trimws() and uidSeparate(), and rewrote
#          uidCreate() to be more general (and quicker).
#  2/02/09 cws Changed output of diff() to denote dataframes being compared as
#          First and Second instead of x and y.  Numeric and non-numeric 
#          values also tested more thoroughly.
#  2/13/09 cws Fixed comments for nlaLengthen().
#  2/23/09 cws diff() now handles factors and datetimes for comparison, both
#          using coercion.
#  6/18/09 cws Changed nlamets.r to nlaSupport.r
#  8/31/09 cws Removed optional tests (relying on boolean testFunctions) of
#          the functions defined here, relying on RUnit harness instead.
#  9/02/09 cws Added definitions of lag(), lead(), first(), and their unit 
#          tests.
#  9/14/09 cws Added definition for last() and unit test.
#  9/23/09 cws Modified first() and last() to handle missing values as a
#          comparable value different than any other.  Previously, this resulted
#          in NA values in the 'first' and 'last' flag columns.
#  3/04/10 cws Modified unit tests to use RUnit instead of funcTest().
#  3/25/10 cws Renamed diff as dfCompare, nlaWiden to dfWiden and nlaLengthen to
#          dfLengthen.  Renamed file from nlaSupport.r to sharedSupport.r.
# 12/15/11 cws Adding sourcing of shared support functions that are not defined 
#          in this file
#  3/02/12 cws Added selectElements definition
#  5/15/12 cws Modified dfCompare to handle NaN and Inf values; was failing due
#          to inability to operate on them with abs(vx-vy).  Also simplified
#          check for incomparable values using xor() rather than mix of & and |.
#  5/24/12 cws Added unit test for dfCompare (finally!).
# 06/01/12 tmk Removed the call to require(RUnit).  Deleted creation of the
#              global variable intermediateMessages.  Added an argument named
#              intermediateMessages to the intermediateMessage function using a
#              default value of TRUE for the argument.  Removed calls to
#              source().
# 08/09/12 tmk Added functions assignTaxCat, interpolatePercentile, modalCount,
#          nWadeableStationsPerTransect, and summaryby.
#  9/21/12 cws Adding lsos.r definition
# 12/21/12 tmk: Removed RUnit functions.
# 01/10/13 tmk Added function convert_to_char, which converts factors to
#          character variables in a data frame.
#  7/22/13 cws Updated normalizedCover() to assume input dataframe has UID and 
#          STATION instead of uid and subid.  It is currently only used within 
#          NLA metrics. 
#  7/23/13 cws sourcing expand.data.frame
#  9/11/13 cws sourcing is.number
# 12/13/13 cws sourcing protectedMean and protectedSum
# 12/19/13 cws sourcing modalValues
#  1/21/13 cws sourcing modalClasses
#  2/06/14 cws sourcing massGet
# 05/28/14 tmk Added a global variables declaration to avoid warning messages
#          while running Rcmd check.
# 10/21/15 cws Changed UID to SITE in interpolatePercentile
# 10/21/15 cws Changed UID to SITE in nWadeableStationsPerTransect
# 11/16/15 cws Changed UID to SITE and RESULT to VALUE in summaryby
#  7/07/17 cws Changed UID to SITE and RESULT to VALUE in normalizedCover,
#          calcSynCovers, calcSynInfluence, fillinDrawdownData and modalClasses.
#          Changed PARAMETER to CLASS in normalizedCover.
#  7/11/17 cws Changed PARAMETER to CLASS in all remaining functions.
#
################################################################################

if(getRversion() >= "3.0")
   utils::globalVariables(c("calc", "cover_dd", "cover_lit", "coverSuffix", "infl_rip", "infl_dd", 
                            "isAg", "lsub_d25", "lsub_d75", "presence")
                          )

#' @keywords internal
#' @export
assignTaxCat <- function(lt) {

##  assignTaxCat.r
##  Created SSR 03/11/2010
##  Assigning taxonomic category based on crew entry
##  This is variable from one study to another, and needs to
##  be updated to match current datasets.

lt$taxCat <- NA

#  Deciduous species
lt$taxCat <- ifelse(lt$SPECIES=='ACACIA', 'Acacia/Mesquite', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='MESQUITE', 'Acacia/Mesquite', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='ALDER' |
                     lt$SPECIES=='ALDER/BIRCH' |
                     lt$SPECIES=='BIRCH' |
                     lt$SPECIES=='RIVER BIRCH' |
                     lt$SPECIES=='YELLOW BIRCH', 'Alder/Birch', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='ASH' |
                     lt$SPECIES=='GREEN ASH' |
                     lt$SPECIES=='WHITE ASH' |
                     lt$SPECIES=='ASH-BASSWOOD', 'Ash', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='MAPLE' |
                     lt$SPECIES=='RED MAPLE' |
                     lt$SPECIES=='MAPLE/BOXELDER' |
                     lt$SPECIES=='SUGAR MAPLE' |
                     lt$SPECIES=='SILVER MAPLE' |
                     lt$SPECIES=='BIG TOOTHED MAPLE' |
                     lt$SPECIES=='ACER RUBRUM' |
                     lt$SPECIES=='ACER NEGUNDO' |
                     lt$SPECIES=='ELDER/MAPLE' |
                     lt$SPECIES=='BOXELDER' |
                     lt$SPECIES=='ELDER', 'Maple/Boxelder', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='OAK' |
                     lt$SPECIES=='POST OAK' |
                     lt$SPECIES=='WILLOW OAK' |
                     lt$SPECIES=='WATER OAK' |
                     lt$SPECIES=='WHITE OAK' |
                     lt$SPECIES=='RED OAK' |
                     lt$SPECIES=='OAK (GAMBEL)' |
                     lt$SPECIES=='BEECH (OAK FAMILY)' |
                     lt$SPECIES=='NORTHERN RED OAK' |
                     lt$SPECIES=='CHESTNUT OAK' |
                     lt$SPECIES=='SOUTHERN RED OAK' |
                     lt$SPECIES=='PIN OAK', 'Maple/Boxelder', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='POPLAR/COTTONWOOD' |
                     lt$SPECIES=='POPLAR' |
                     lt$SPECIES=='TULIP POPLAR' |
                     lt$SPECIES=='YELLOW POPLAR' |
                     lt$SPECIES=='ASPEN' |
                     lt$SPECIES=='OTHER (BIG TOOTH ASPEN)' |
                     lt$SPECIES=='OTHER (ASPEN)' |
                     lt$SPECIES=='COTTONWOOD' |
                     lt$SPECIES=='NARROWLEAF COTTONWOOD' |
                     lt$SPECIES=='FREMONT COTTONWOOD' |
                     lt$SPECIES=='EASTERN COTTONWOOD', 'Poplar/Cottonwood', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='SYCAMORE' |
                     lt$SPECIES=='ARIZONA SYCAMORE' |
                     lt$SPECIES=='PLATANUS OCCIDENTALIS', 'Sycamore', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='WILLOW' |
                     lt$SPECIES=='BLACK WILLOW' |
                     lt$SPECIES=='BUTTON WILLOW' |
                     lt$SPECIES=='SALIX SP.' |
                     lt$SPECIES=='GOODING WILLOW', 'Willow', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='OTHER DECIDUOUS' |
                     lt$SPECIES=='UNKNOWN DECIDUOUS' |
                     lt$SPECIES=='WESTERN LARCH' |
                     lt$SPECIES=='UNKNOWN OR OTHER DECIDUOUS' |
                     (lt$SPECIES=='OTHER' & lt$TREE_TYP=='Deciduous') |
                     (lt$SPECIES=='UNKNOWN' & lt$TREE_TYP=='Deciduous'), 'Unknown or Other Deciduous', lt$taxCat)
#  All others marked as deciduous left get lumped into 'Unknown or Other Deciduous'
lt$taxCat <- ifelse(is.na(lt$taxCat) & lt$TREE_TYP=='Deciduous', 'Unknown or Other Deciduous', lt$taxCat)

# Coniferous species
lt$taxCat <- ifelse(lt$SPECIES=='CEDAR' |
                     lt$SPECIES=='WHITE CEDAR' |
                     lt$SPECIES=='WESTERN RED CEDAR' |
                     lt$SPECIES=='BALD CYPRESS' |
                     lt$SPECIES=='CYPRESS' |
                     lt$SPECIES=='POND CYPRESS' |
                     lt$SPECIES=='SEQUOIA', 'Cedar/Cypress/Sequoia', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='FIR' |
                     lt$SPECIES=='DOUGLAS FIR' |
                     lt$SPECIES=='SUBALPINE FIR' |
                     lt$SPECIES=='HEMLOCK' |
                     lt$SPECIES=='EASTERN HEMLOCK' |
                     lt$SPECIES=='FIR/HEMLOCK', 'Firs/Hemlock', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='JUNIPER' |
                     lt$SPECIES=='JUNIPER (NEARLY DEAD)' |
                     lt$SPECIES=='UTAH JUNIPER' |
                     lt$SPECIES=='ALLIGATOR JUNIPER' |
                     lt$SPECIES=='PINYON JUNIPER', 'Juniper', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='PINE' |
                     lt$SPECIES=='WHITE PINE' |
                     lt$SPECIES=='RED PINE' |
                     lt$SPECIES=='LOBLOLLY PINE' |
                     lt$SPECIES=='LODGEPOLE PINE' |
                     lt$SPECIES=='PONDEROSA PINE', 'Pine', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='SPRUCE' |
                     lt$SPECIES=='ENGELMANN SPRUCE' |
                     lt$SPECIES=='BLUE SPRUCE', 'Spruce', lt$taxCat)
lt$taxCat <- ifelse(lt$SPECIES=='OTHER CONIFER' |
                     (lt$SPECIES=='UNKNOWN' & lt$TREE_TYP=='Coniferous') |
                     (lt$SPECIES=='UNKNOWN CONIFER' & lt$TREE_TYP=='Coniferous'), 'Unknown or Other Conifer', lt$taxCat)
#  All others marked as coniferous get lumped into 'Unknown or Other Deciduous'
lt$taxCat <- ifelse(is.na(lt$taxCat) & lt$TREE_TYP=='Coniferous', 'Unknown or Other Conifer', lt$taxCat)

# Broadleaf evergreen species
lt$taxCat <- ifelse(lt$SPECIES=='LIVE OAK' |
                    lt$SPECIES=='UNKNOWN BROADLEAF' |
                    (lt$SPECIES=='UNKNOWN' & lt$TREE_TYP=='Broadleaf Evergreen'), 'Unknown or Other Broadleaf Evergreen', lt$taxCat)
#  All others marked as broadleaf evergreen left get lumped into 'Unknown or Other Deciduous'
lt$taxCat <- ifelse(is.na(lt$taxCat) & lt$TREE_TYP=='Broadleaf Evergreen', 'Unknown or Other Broadleaf Evergreen', lt$taxCat)

# Snags
lt$taxCat <- ifelse(lt$SPECIES=='DEAD ASH' |
                     lt$SPECIES=='ELM (DEAD)' |
                     lt$SPECIES=='DEAD COTTONWOOD' |
                     lt$SPECIES=='SNAG' |
                     lt$SPECIES=='JUNIPER (SNAG)' |
                     lt$SPECIES=='PONDEROSA FIRE SNAG' |
                     lt$SPECIES=='SNAG COTTONWOOD', 'Snag', lt$taxCat)

return(lt)
}


#' @keywords internal
#' @export
calcSynCovers <- function(coverData, maxDrawdown, assumptions=FALSE) {	
# Creates synthesized 2007-like characteristicCover values from NLA drawdown 
# and riparian cover values, using the relation developed by Phil Kaufmann:
#
#	Cover_syn = Prop_dd * Cover_dd + Prop_lit * Cover_lit
#
# where
#	Prop_dd = max(1, HORIZ_DIST_DD/maxDrawdown)
#	Prop_lit = 1 - Prop_dd
#
# NOTE: In most cases it is expected that the synthetic cover value fall between
# the riparian and drawdown values.  This relationship may not hold when the
# synthetic value is missing due to missing HORIZ_DIST_DD and the riparian and
# drawdown values are sufficiently larger or smaller than the the mean cover.
#
# ARGUMENTS:
# coverData		dataframe containing cover data with the following columns:
#                 SITE, STATION, CLASS, VALUE, characteristicCover.
# maxDrawdown	numeric value containing the maximum horizontal drawdown distance
#				  to be considered when determining the drawdown cover weighting.
#				  This value is 10 for littoral Fish Cover, and 15 for Riparian 
#				  Vegetation.
# assumptions	logical value determining whether to make assumptions about
#				  missing drawdown cover values when calculating synthetic covers.
#
# ASSUMPTIONS:
# Values of CLASS follow the form COVERNAME_DD and COVERNAMEother, where the
#   suffix 'other' can be absent, or _RIP, just not _DD.  That should be easy.
# Data includes HORIZ_DIST_DD
# Values of characteristicCover is numeric
# Values of VALUE is numeric, at least for CLASS=='HORIZ_DIST_DD'
#

	# Determine proportion for each station:
	props <- within(subset(coverData, CLASS=='HORIZ_DIST_DD')
	               ,{VALUE <- as.numeric(VALUE)
					 prop_dd <- ifelse(VALUE < maxDrawdown, VALUE/maxDrawdown, 1)	# i.e. min(1, as.numeric(VALUE)/15)
					 prop_lit <- 1 - prop_dd
				    }
			       )[c('SITE','STATION','prop_lit','prop_dd')]

	# Calculate synthetic value based on the dd/rip proportions.
	# There are three cases where we can determine the synthetic value without 
	# both covers and the proportion value, otherwise missing values preclude
	# further calculation without making assumptions about the data:
	# 	a) if prop_dd is 1 and cover_dd is not missing, then syn = cover_dd
	#	b) if prop_dd is 0 and cover_lit is not missing, then syn = cover_lit
	#	c) if both cover_dd and cover_lit are not missing, then syn = cover_dd OR cover_lit
#print('coverData');print(head(coverData))
	covers <- dcast(within(subset(coverData, CLASS != 'HORIZ_DIST_DD')
	                      ,{coverType <- gsub('^(.+)_DD$', '\\1', CLASS)
							coverLocation <- ifelse(grepl('_DD$', CLASS), 'cover_dd', 'cover_lit')
						   }
		                  )
				   ,SITE+STATION+coverType~coverLocation
		           ,value.var='characteristicCover'
				   )
#print('.0')	
	covers <- merge(covers, props, by=c('SITE','STATION'), all.x=TRUE)
#print('.1')	
	
	coverProps <- within(covers	
					    ,{assumptionMade <- assumptions & is.na(prop_dd) & is.na(cover_dd) & !is.na(cover_lit)
						  basicSyntheticCover <- ifelse(assumptionMade, cover_lit					# rely on assumption
						  						,ifelse(is.na(cover_dd) & is.na(cover_lit), NA
						                   		,ifelse(is.na(cover_dd) | is.na(cover_lit)
													   ,ifelse(is.na(prop_dd), NA
									   				   ,ifelse(abs(prop_dd - 1) < 1e-15, cover_dd	# case (a)
													   ,ifelse(abs(prop_dd) < 1e-15, cover_lit		# case (b)
																	               , NA
														)))
												# both covers are present if we get this far 
												,ifelse(abs(cover_dd - cover_lit) < 1e-15, cover_dd	# case (c)
													   ,prop_dd * cover_dd + prop_lit * cover_lit	# normal case
											     ))))
						  syn <- as.numeric(substr(as.character(basicSyntheticCover), 1, 18))
					     }
					    )
#print('.2')	

	# Reorganize calculation to long format with expected CLASS name
	rc <- within(coverProps
				,{CLASS <- paste0(coverType, '_SYN')
				  VALUE <- NA
				  characteristicCover <- ifelse(is.nan(syn), NA, syn)
				 }
		        )[c('SITE','STATION','CLASS','VALUE','characteristicCover', 'assumptionMade')]
#print('.3')	

	return(rc)
}


#' @keywords internal
#' @export
calcSynInfluence <- function(influenceData) {
# Creates synthesized 2007-like weighted influence values from NLA drawdown 
# and riparian human influence values, using the following relation:
#
#					Riparian influence
# 				   |0		|	P			|	C          |
#               ---+---------------------------------------+
# Drawdown		0  |0		|	P*prop_rip	|	C*prop_rip |
# influence		   |		| 				| 			   |
#               ---+---------------------------------------+
#               P  |P       |	P			|	C*prop_rip |
#				   |		| 				| + P*prop_dd  |
#               ---+---------------------------------------+
#               C  |C       |	C			|	C		   |
#				   |		| 				| 			   |
#               ---+---------------------------------------+
#
# where
#	prop_dd = max(1, HORIZ_DIST_DD/15)
#	prop_rip= 1 - prop_dd
#   0,P,C   = numeric weights associated with those same influence codes.
#             (currently 0.0, 0.5 and 1.0)
#
# In the case where HORIZ_DIST_DD is zero, the synthetic influence is identical to
# the riparian zone influence.  It's possible that nonzero drawdown influence was 
# also (illogically) noted, but those values are ignored in this case.
#
# This differs from the synthetic riparian cover calculations because of the 
# definitions and spatial relationships of the levels of influence intensity.  
# For example, a 'C' rating in the drawdown zone is automatically 'C' in the
# synthesized 2007 zone regardless of the influence rating in the riparian zone.
# 
#
# ARGUMENTS:
# influenceData		dataframe containing influence data with the following columns:
#                 SITE, STATION, CLASS, VALUE, calc.
#
# ASSUMPTIONS:
# Values of CLASS follow the form INFLUENCENAME_DD and INFLUENCENAMEother, where the
#   suffix 'other' can be absent, or _RIP, just not _DD.  That should be easy.
# Data includes HORIZ_DIST_DD
# Values of calc are numeric
# Values of VALUE is castable to numeric, at least for CLASS=='HORIZ_DIST_DD'
#

	maxDrawdown <- 15
	
	# Determine proportion for each station:
	props <- within(subset(influenceData, CLASS=='HORIZ_DIST_DD')
	               ,{VALUE <- as.numeric(VALUE)
					 prop_dd <- ifelse(VALUE < maxDrawdown, VALUE/maxDrawdown, 1)	# i.e. min(1, as.numeric(VALUE)/15)
					 prop_rip <- 1 - prop_dd
				    }
			       )[c('SITE','STATION','prop_rip','prop_dd')]

	# Calculate synthetic values based on the dd/rip proportions.
	# There are three cases where we can determine the synthetic value without 
	# both influences and the proportion value, otherwise missing values preclude
	# further calculation without making assumptions about the data:
	# 	a) if prop_dd is 1 and infl_dd is not missing, then syn = infl_dd
	#	b) if prop_dd is 0 and infl_rip is not missing, then syn = infl_rip
	#	c) if both infl_rip and infl_rip are equal, then syn = infl_dd OR infl_rip
	influences <- dcast(within(subset(influenceData, CLASS != 'HORIZ_DIST_DD')
	                      	  ,{inflType <- gsub('^(.+)_DD$', '\\1', CLASS)
								inflLocation <- ifelse(grepl('_DD$', CLASS), 'infl_dd', 'infl_rip')
						   	   }
		                      )
				   	   ,SITE+STATION+inflType~inflLocation
		           	   ,value.var='calc'
				   	   )
	
	influences <- merge(influences, props, by=c('SITE','STATION'), all.x=TRUE)

	inflProps <- within(influences	
					   ,{basicSyntheticInfl <- ifelse(is.na(prop_dd)
						                             ,ifelse(abs(infl_rip - infl_dd) < 1e-15, infl_rip	# case (c)
																	 						, NA
						                                    )
											  ,ifelse(abs(prop_dd) < 1e-15, infl_rip					# case (b); also
															  											# infl_dd should be zero here.
															  											# ignore any illogical cases
											  ,ifelse(is.na(infl_dd), NA
						                      ,ifelse(infl_dd == 1.0
							  						 ,ifelse(abs(prop_dd) < 1e-15, infl_rip				# this case is illogical, 
															 					 , 1.0
															)
											  ,ifelse(infl_dd == 0.5
							  						 ,ifelse(abs(prop_dd - 1) < 1e-15, infl_dd			# case (a)
													 ,ifelse(is.na(infl_rip), NA
					 								 ,ifelse(infl_rip == 1.0, prop_dd * infl_dd + prop_rip * infl_rip
															                , 0.5
	 														)))
											  ,ifelse(infl_dd == 0.0
													 ,ifelse(abs(prop_dd - 1) < 1e-15, infl_dd			# case (a)
													 ,ifelse(is.na(infl_rip), NA
							                                                , prop_dd * infl_dd + prop_rip * infl_rip
													  ))
													,NA													# unexpected infl_dd value
											   ))))))
													
						 syn <- as.numeric(substr(as.character(basicSyntheticInfl), 1, 18))
					    }
					   )

	# Reorganize calculation to long format with expected CLASS name
	rc <- within(inflProps
				,{CLASS <- paste0(inflType, '_SYN')
				  VALUE <- NA
				  calc <- ifelse(is.nan(syn), NA, syn)
				 }
		        )[c('SITE','STATION','CLASS','VALUE','calc')]

	return(rc)
}


#' @keywords internal
#' @export
convert_to_char <- function(df) {

################################################################################
# Function: convert_to_char
# Purpose: Convert factors to character variables in a data frame
# Programmers: Tom Kincaid
# Date: January 10, 2013
# Description:
#   This function converts factors to character variables in a data frame
################################################################################

  for(i in 1:ncol(df)) {
    if(is.factor(df[,i])) {
      df[,i] <- as.character(df[,i])
    }
  }
  df
}


#' @keywords internal
#' @export
count <- function(x) {
# Returns the number of non-missing values in the vector, used to determine
# sample size.  
#
# NOTE: When using this function with aggregate(), it is sometimes necessary 
# to convert x to factors using as.factors() to prevent the output from 
# creating a vector 'if (stringsAsFactors) factor(x) else x' instead of 'x'.
# I do not know why.
#
# ARGUMENTS:
# x        vector of values to count.
 

    length(na.omit(x))
}


#' @keywords internal
#' @export
dfCompare <- function(df1, df2, byVars, zeroFudge=1e-17, verbose=FALSE) {
# Used to find differences in data frames.  Comparisons will be restricted to
# records with equivalent keys and to common columns.  Rows and columns ocuring
# in only one of the data frames will be noted in the output.
#
# Returns a Nx1 matrix of differences.
#
# ARGUMENTS:
# df1, df2  Dataframes to compare.  These must share key variables.
# byVars    Key variables which uniquely identify specific rows in each
#             dataframe.
# zeroFudge Largest difference between numeric values that is regarded as zero.
# verbose   Logical value, if TRUE column names will be printed as they are
#             compared.
#


  nFlagged<-0
  diffList <- NULL
  notNumber <- '[a-df-zA-DF-Z _:;=\\/\\|\\?\\*\\(\\)\\$\\^%,<>]'

  # Restrict comparison to common variables, and list variables which
  # occur in only one dataframe.
  n1 <- names(df1)[!(names(df1) %in% byVars)]
  n2 <- names(df2)[!(names(df2) %in% byVars)]
  only1 <- n1[!(n1 %in% n2)]
  only2 <- n2[!(n2 %in% n1)]

  if (length(only1) > 0) {
      diffList <- rbind(diffList
                       ,paste("Variables in first file not in second:"
                             ,paste(only1, collapse=', ')
                             ,sep=''
                             )
                       )
  }
  if (length(only2) > 0) {
      diffList <- rbind(diffList
                       ,paste("Variables in second file not in first:"
                             ,paste(only2, collapse=', ')
                             ,sep=''
                             )
                       )
  }


  # Restrict comparison to common rows as defined by values of key variables
  # listed in byVars argument, and listing those values which occur in only
  # one dataframe.  Note that the [,1] terminus converts the dataframe to an 
  # unnamed vector.
  for(k in 1:length(byVars)) {
      if(k==1) {
          keyValues1 <- df1[byVars[k]][,1]
          keyValues2 <- df2[byVars[k]][,1]
      } else {
          keyValues1 <- paste(keyValues1,df1[byVars[k]][,1], sep=' | ')
          keyValues2 <- paste(keyValues2,df2[byVars[k]][,1], sep=' | ')
      }
  }
  rows1 <- keyValues1[!(keyValues1 %in% keyValues2)]
  rows2 <- keyValues2[!(keyValues2 %in% keyValues1)]

  if (length(rows1) > 0) {
      diffList <- rbind(diffList
                       ,as.matrix(paste("Row in first file not in second:"
                                       ,rows1
                                       ,condense=' '
                                       )
                                 ,ncol=1
                                 )
                       )
  }
  if (length(rows2) > 0) {
      diffList <- rbind(diffList
                       ,as.matrix(paste("Row in second file not in first:"
                                       ,rows2
                                       ,condense=' '
                                       )
                                 ,ncol=1
                                 )
                       )
  }


  # List common variables and prepare for comparison.
  vars <- intersect(n1,n2)
  both <- merge(subset(df1, select=c(byVars, vars))
               ,subset(df2, select=c(byVars, vars))
               ,by=byVars
               ,all=FALSE, sort=TRUE
#               ,suffixes=c('First','Second')
               )


  # Go through each common variable and look for differences
  # Convert factors to 'real' values to allow comparisons.  The class()
  # function returns two classes for some objects (e.g. "POSIXt"  "POSIXct")
  # so any() is used to aggregate the resulting boolean values.
  for(m in 1:length(vars)) {

      if(verbose) print(paste("Checking", vars[m]))

      vx <- both[,paste(vars[m],'.x',sep='')] 
      vy <- both[,paste(vars[m],'.y',sep='')] 
      if(typeof(vx) != typeof(vy)) {
          rbind(diffList
               ,sprintf("Type mismatch for %s: First=<%s> Second=<%s>, converting to character"
                       ,vars[m], typeof(vx),typeof(vy)
                       )
               )
          vx <- as.character(vx)
          vy <- as.character(vy)
      }
      
      if(any(class(vx)=='factor')) {
          if(mode(vx)=='numeric') {
              if(verbose) print("(Converting factor to numeric in first df)")
              vx <- as.numeric(vx)
          } else if(mode(vx)=='character') {
              if(verbose) print("(Converting factor to character in first df)")
              vx <- as.character(vx)
          } else {
              print(paste("Unexpected mode in first df: ", typeof(vx)))
          }   
      }
      if(any(class(vy)=='factor')) {
          if(mode(vy)=='numeric') {
              if(verbose) print("(Converting factor to numeric in second df)")
              vy <- as.numeric(vy)
          } else if(mode(vy)=='character') {
              if(verbose) print("(Converting factor to character in second df)")
              vy <- as.character(vy)
          } else {
              print(paste("Unexpected mode in first df: ", typeof(vy)))
          }   
      }

      for(s in 1:length(both[,1])) {
          
          flagDiff <- FALSE
          if (is.na(vx[s]) & is.na(vy[s])) {
              # Do nothing.
        
          } else if(xor(is.na(vx[s]), is.na(vy[s]))) {
              
              flagDiff <- TRUE
              nFlagged <- nFlagged+1
              
          } else if (mode(vx[s]) == 'numeric' & mode(vy[s]) == 'numeric') {    # handle numeric values
              
              if (is.nan(vx[s]) & is.nan(vy[s])) {
                  # Do nothing.
            
              } else if(xor(is.nan(vx[s]), is.nan(vy[s]))) {
              
                  flagDiff <- TRUE
                  nFlagged <- nFlagged+1
              
              } else if(xor(is.infinite(vx[s]), is.infinite(vy[s]))) {
              
                  flagDiff <- TRUE
                  nFlagged <- nFlagged+1
              
              } else if (is.infinite(vx[s]) & is.infinite(vy[s])) {
                  
                  # these are comparable, but can't operate on them
                  if(vx[s] != vy[s]) {
                      flagDiff <- TRUE
                      nFlagged <- nFlagged+1                      
                  }
                  
              } else if (abs(as.double(vx[s] - vy[s])) > zeroFudge) {
                  
                  flagDiff <- TRUE
                  nFlagged <- nFlagged+1
                  
              }
              
          } else {                                                              # handle character values
              
              if(as.character(vx[s]) != as.character(vy[s]) ) {
                  flagDiff <- TRUE
                  nFlagged <- nFlagged+1
              }
          }

          if(flagDiff) {
              for(k in 1:length(byVars)) {
                  if(k==1) {
                      keyValues <- paste(byVars[k]
                                        ,both[s,names(both)==byVars[k]]
                                        ,sep='='
                                        )
                  } else {
                      keyValues <- paste(keyValues
                                        ,paste(byVars[k]
                                              ,both[s,names(both)==byVars[k]]
                                              ,sep='='
                                              )
                                        ,sep=', '
                                        )
                  }
              }
              diffList <- rbind(diffList,
                                sprintf("Difference at %s; %s: First=<%s> Second=<%s>"
                                       ,keyValues, vars[m]
                                       ,as.character(vx[s]) 
                                       ,as.character(vy[s]) 
                                       )
                               )
          }
      }
  }

  return(diffList)
} 


#' @keywords internal
#' @export
dfLengthen <- function(df, keys, name, value, values) {
# Converts a data frame from 'wide' format (values listed horizontally with 
# separate columns for each variable) to 'narrow' format (variables listed 
# vertically in a single column).  The returned data frame will contain a 
# column containing the names of each variable, and another column containing 
# the values of those variables.
# e.g. 'narrow' format:                'wide' format:
#      key1 key2 variable   value      key1 key2 meanX stX
#      foo  1    meanX      0.7        foo  1    0.7   4.7
#      foo  1    sdX        4.7        foo  2    1.3   NA
#      foo  2    meanX      1.3        bar  1    NA    2.27
#      bar  1    sdX        2.27
#
# ARGUMENTS:
# df         Data frame in 'wide' format to be converted.
# keys       Vector of column names in df which together uniquely identify
#            a location to which the variables are associated. e.g. 'uid'
#            or c('site_id','year','visit_no') or c('key1','key2).
# name       Name of column to contain the variable names, e.g. 'variable'
# value      Name of column to contain the variable values, e.g. 'value'
# values     Vector of column names with values to include in the output
#              e.g. c('meanX', 'stX')
#
# ASSUMPTIONS:
# There is one row for each combination of variables listed in the keys
#   argument.
#

  for (i in 1:length(values)) {
    tt <- df[c(keys, values[i])]
    tt <- rename(tt, values[i], value)
    tt[name]<-values[i]

    if(i==1) {
      long <- tt
    } else {
      long <- rbind(long, tt)
    }

    #cat(paste('.',as.character(i),sep=''))
  }

  return(long)
}


#' @keywords internal
#' @export
dfWiden <- function(df, keys, name, values, makeNumeric=TRUE) {
# Converts a data frame from 'narrow' format (variables listed vertically in a 
# single column) to 'wide' format (values listed horizontally with separate
# columns for each variable).  The returned data frame will contain a column
# for each key listed, and have a column for each variable name and given that 
# name.
#
# e.g. 'narrow' format:                'wide' format:
#      key1 key2 variable   value      key1 key2 meanX stX
#      foo  1    meanX      0.7        foo  1    0.7   4.7
#      foo  1    sdX        4.7        foo  2    1.3   NA
#      foo  2    meanX      1.3        bar  1    NA    2.27
#      bar  1    sdX        2.27
#
# ARGUMENTS:
# df           Data frame in 'narrow' format to be converted.
# keys         Vector of column names in df which together uniquely identify
#              a location to which the variables are associated. e.g. 'uid'
#              or c('site_id','year','visit_no').
# name         Name of column with the variable names
# values       Name of column with the variable values.
# makeNumeric  Specify whether to allow columns which are entirely numeric
#              to remain as character type.  If TRUE, these columns will
#              be attempted to convert into type numeric, otherwise no
#              conversion will be attempted.
#
# ASSUMPTIONS:
# A variable occurs no more than once within a combination of variables listed
# in the keys argument.
#

  # regexp of a string that isn't a number
  notNumber <- '[a-df-zA-DF-Z _:;=\\/\\|\\?\\*\\(\\)\\$\\^%,<>]'

  # Get list of variable names
  names <- unique(df[,name])

  for (i in 1:length(names)) {
    # Separate input data frame by variable name, convert those values to 
    # numeric if specified, properly name the new column, and finally
    # merge them horizontally.
    tt <- subset(df, get(name)==names[i])[c(keys,values)]
    if (makeNumeric 
       & is.character(tt[,values])
       & length(grep(notNumber,tt[,values])) == 0
       ) {
       tt[,values] <- as.numeric(tt[,values])
    }
    tt <- rename(tt, values, names[i])

    if(i==1) {
      wide <- tt
    } else {
      wide <- merge(wide, tt, by=keys, all=TRUE)
    }

    #cat(paste('.',as.character(i),sep=''))
  }

  return(wide)

}


#' @keywords internal
#' @export
expand.data.frame <- function(df, cols) {
# Expands a data frame so that it contains a cartesian product of values in each  
# of the named columns.  Columns not named in the cols argument will have NA
# values in the new rows.  This is useful, for example, to ensure that a  
# calculation of means contains rows for all sites and all parameters. This
# expanded dataframe is returned.
#
# ARGUMENTS:
# df		dataframe to modify
# cols		character vector of column names occuring in df,
#
# ASSUMPTIONS:
# All columns listed in cols are names of columns in df 

	# List values in each column
	colList <- list()
	for(i in 1:length(cols)) {
		colList <- c(colList, unique(df[cols[i]]))
	}

	# Expand data to contain rows for all combinations of values in the 
	# specified columns
	expanded <- merge(df, expand.grid(colList, stringsAsFactors=FALSE)
				     ,by=cols, all=TRUE
					 )
	
	return(expanded)
}


#' @keywords internal
#' @export
fillinDrawdownData <- function(df, fillinValue='0', fillinHORIZ_DIST_DD='0') {
# Expands the provided NLA data to include rows for all data expected at each  
# site and fills in unrecorded drawdown values based on the value of DRAWDOWN.  
#
# Data is 'expected' at a station if any cover/influence/DRAWDOWN data is recorded at that
# station.  Thus if a value for bare ground cover is recorded at a station, then 
# all the Riparian Vegetation values (in both Riparian and Drawdown zones) are 
# considered 'expected'.  This expectation does NOT carry over into other stations
# as they may not have been sampled.
#
# Returns dataframe with adjusted values and potentially additional rows.  Rows
# with CLASS == 'DRAWDOWN' and rows for which we can not fill in a missing 
# value in the input data may not occur in the returned dataframe.
#
# ARGUMENTS:
# df			dataframe with NLA cover/influence data.  Must contain columns SITE,
#	             STATION, CLASS, VALUE.  It is also expected to contain rows
#   	          with values for CLASS=DRAWDOWN and HORIZ_DIST_DD.
# fillinValue	character value to which unrecorded cover/influence values will 
#				  be set.
# fillinHORIZ_DIST_DD	character value to which unrecorded HORIZ_DIST_DD values 
#						  will be set.
# 
# ASSUMPTIONS:
#

	# If there's no DRAWDOWN values then there's nothing to do.
	if('DRAWDOWN' %nin% df$CLASS) {
		print("No 'fill-in' of data because DRAWDOWN values not provided")
		return(df)
	}
	
	
	# Expand cover/influence data so that all expected rows occur for each site.
	dfExpanded <- fillinDrawdownData.expansion(df)	#subset(df, CLASS != 'DRAWDOWN'))
	
	# Filter out the added rows we can't assign a VALUE value to
	drawdown <- within(subset(df, CLASS == 'DRAWDOWN')
					  ,{DRAWDOWN <- ifelse(is.na(VALUE), NA
								   ,ifelse(grepl('^N', VALUE), 'NO'
								   ,ifelse(grepl('^Y', VALUE), 'YES'
															  , NA
									)))
						VALUE <- NULL
						CLASS <- NULL
					   }
					  )[c('SITE','STATION','DRAWDOWN')]
	
	expectedStations <- subset(merge(dfExpanded		#subset(df, CLASS != 'DRAWDOWN')
							        ,drawdown
								    ,by=c('SITE','STATION'), all.x=TRUE
									)
							  ,TRUE 	# !(is.na(DRAWDOWN) & is.na(VALUE))
							  )

							  
	# Use value of DRAWDOWN to fill in missing values.
	dfFilledin <- within(expectedStations
					    ,VALUE <- ifelse(is.na(VALUE) & DRAWDOWN == 'NO'
								   	  	 ,ifelse(CLASS == 'HORIZ_DIST_DD', fillinHORIZ_DIST_DD
									  									  	 , fillinValue
									   	  )
						  				 ,VALUE
								 		 )
						)	


	# Return filled in dataframe after getting rid of added DRAWDOWN column
	rc <- dfFilledin[names(df)]
	return(rc)
	
}


#' @keywords internal
#' @export
fillinDrawdownData.expansion <- function(df) {
# Expands cover/influence data so each site has rows for each value expected at 
# each station.
#
# ARGUMENTS:	
# df		dataframe with NLA cover/influence data.  Must contain columns SITE,
#             STATION, CLASS, VALUE.  Must contain all CLASS values that
# 			  are expected (e.g. covers, HORIZ_DIST_DD, DRAWDOWN), though they 
#		 	  do not need to occur at each station or even at each site.  Must 
#             NOT contain any additional CLASS values.
#
# ASSUMPTIONS:
# 

	expectedParameters <- subset(data.frame(CLASS = setdiff(unique(df$CLASS), 'DRAWDOWN')
										   ,stringsAsFactors=FALSE
						   				   )
			                    ,grepl('_DD$', CLASS)
								)
	
	listStations <- lapply(split(df, paste(df$SITE, df$STATION))
						  ,function(x) { 
								y <- within(merge(x, expectedParameters, by='CLASS', all=TRUE)
										   ,{SITE <- unique(x$SITE)
											 STATION <- unique(x$STATION)
											}
										   )
							    return(y)
						   }
						  )
	fullStations <- do.call(rbind, listStations)
	rownames(fullStations) <- NULL
	
	return(fullStations)
}


#' @keywords internal
#' @export
first <- function(df, v, first.v) {
# Mimics the SAS FIRST. operator by creating a new column containing a boolean
# flag which is TRUE if the row has different by-variable values than the
# preceding row.  Returns dataframe with flag in added column.
#
# Ordering the input dataframe prior to using this function is not required, but
# will be useful.
#
# ARGUMENTS:
# df        dataframe to add 'first' flag column to
# v         name of existing column in dataframe with values to
# lead.v    names of new column in dataframe to contain first flag values
#

  # Lag named column by one row into temporary column
  df <- lag(df, v, '..vLag', offset=1)

  # A row is flagged 'first' if the named column is changed from the previous
  # row, otherwise they are not.  The comparison must be coerced into a vector
  # to allow the 'first' column to be named properly.  Missing (NA) values in
  # either the data column or it's lag() value are treated as comparable values
  # A row is flagged as first if the value v is not the same as the lagged value
  # of v OR if their inequality is NA and v and lag(v) are not both NA.
  # The old check ( df[first.v] <- as.vector(df[v] != df['..vLag']) ) resulted
  # in first=NA if either v or vLag were NA.
  df[first.v] <- as.vector(ifelse(is.na(df[v]) | is.na(df['..vLag'])
                                 ,is.na(df[v]) != is.na(df['..vLag'])
                                 ,df[v] != df['..vLag']
                                 )
                          )

  # The first row must be 'first' by definition; if it is not explicitly set,
  # the comparison will be NA.
  df[1,first.v] <- TRUE

  # Drop temporary column
  df['..vLag'] <- NULL

  return(df)
}


#' @keywords internal
#' @export
gmean <- function(x) {

# Returns the geometric mean of the specified data
#
# ARGUMENTS:
# x        Vector of values of interest.
#

  prod(x)^(1/count(x))
}


#' @keywords internal
#' @export
idr <- function(x, method=2) {
# Returns interdecile range (90%-10% of population, assuming gaussian
# distribution) of the specified data.  Quantile calculation type 2 matches
# SAS to within 10^-6 100% of the time (for 2008 data); this is at odds with
# the R documentation which specifies type 3 imitates SAS.  FWIW, type 5 
# differs from SAS only 7% of the time under the same conditions.
#
# ARGUMENTS:
# x        Vector of values of interest.
#
 
    quantile(x, probs=0.90, na.rm=TRUE, names=FALSE, type=method) - 
      quantile(x, probs=0.10, na.rm=TRUE, names=FALSE, type=method)
}


#' @keywords internal
#' @export
iqr <- function(x, method=2) {
# Returns interquartile range (75%-25% of population, assuming gaussian
# distribution) of the specified data.  Quantile calculation type 2 matches
# SAS to within 10^-6 100% of the time (for 2008 data); this is at odds with
# the R documentation which specifies type 3 imitates SAS.  FWIW, type 5 
# differs from SAS only 7% of the time under the same conditions.
#
# ARGUMENTS:
# x        Vector of values of interest.
#

    quantile(x, probs=0.75, na.rm=TRUE, names=FALSE, type=method) - 
      quantile(x, probs=0.25, na.rm=TRUE, names=FALSE, type=method)
}


#' @keywords internal
#' @export
interpolatePercentile <- function(df, classVar, percentile, pctlVar, classBounds) {

# 03/02/10 cws Created
# 03/25/10 cws Changed diff() calls to dfCompare().
# 10/21/15 cws Changed UID to SITE in interpolatePercentile

# Estimate size at a specified percentile within a sample based on recorded
# class memberships and the size boundaries of those classes at each SITE.
# Originally developed for use with NRSA substrate particle diameters.
#
# Returns dataframe with columns SITE and name specified by pctlVar argument
#
# ARGUMENTS
# df            dataframe with class data.
# classVar      Name of column in data containing the sampled class names, quoted.
# percentile    Real value ranging from 0 to 100 inclusive, specifying the
#                 population percentile to calculate
# pcltVar       Name of column in which calculated sizes are placed, quoted.
# classBounds   Dataframe describing max and min size of each class, containing
#                 three columns:
#                   - name specified in classVar argument
#                   min - minimum class size
#                   max - maximum class size
#
# ASSUMPTIONS:
# The dataframe df will have column SITE specifying the site, and the column
#   with class information as specified by the classVar argument.
# The dataframe df will contain only those classes for which class size
#   information is made available in the classSizes argument.

# df <- dplyr::rename(df, SITE=UID)      # uncomment this line for use with old mets* functions

  # Count class occurrences and total sample sizes at each site, and calculate
  # percent occurence of each class at a site.
  df <- subset(df, !is.na(classVar))
  classCounts <- aggregate(list('classCount'=df[[classVar]])
                          ,list('SITE'=df$SITE, 'CLASS'=df[[classVar]])
                          ,count
                          )
  sampleSizes <- aggregate(list('totalCount'=df[[classVar]])
                          ,list('SITE'=df$SITE)
                          ,count
                          )
  classPcts <- merge(classCounts, sampleSizes, by='SITE')
  classPcts$pct <- 100 * classPcts$classCount / classPcts$totalCount

  # Calculate cumulative percentages for each size class.  These will be the upper
  # bound of the percentage for that classes diameter range, and will be the lower
  # bound of the percentage for the next larger class.  The classes must be
  # ordered from small to large prior to this calculation, so this is a good time
  # to fold in the bounds for each class so that we can then order by ascending
  # size.  (Note that while ave() does not require inclusion of SITE in the order to
  # to correctly calculate cumulative summations, it is required by lag() in the
  # next step).
  classPcts <- merge(classPcts, classBounds, by='CLASS', all.x=TRUE)
  classPcts <- classPcts[order(classPcts$SITE, classPcts$min),]

  classPcts$upperPct <- ave(classPcts$pct, classPcts$SITE, FUN=cumsum)
  classPcts <- first(classPcts, 'SITE', 'start')
  classPcts <- lag(classPcts, 'upperPct', 'lowerPct')
  classPcts[classPcts$start,]$lowerPct <- 0

  # Use linear interpolation to find the diameter at the specified percentile
  # of the population sample.
  tt <- subset(classPcts, lowerPct < percentile & percentile <= upperPct)
  tt[pctlVar] <- with(tt, min+ (max-min) * (percentile-lowerPct)/(upperPct-lowerPct))
  tt <- tt[c('SITE',pctlVar)]

# tt <- dplyr::rename(tt, UID=SITE)      # uncomment this line for use with old mets* functions
  return(tt)
}


#' @keywords internal
#' @export
is.subset <- function(a, b) {
# Returns TRUE if elements in vector a are a subset of the elements in vector 
# b, or FALSE otherwise.
#
# ARGUMENTS:
# a        A vector of elements of which all elements are expected to be
#          found in vector b.
# b        A vector of elements which is expected to contain all of the 
#          elements of a.
#

  return(sum( (unique(a) %in% unique(b)) ) == sum( rep(TRUE,length(a))) )
}


#' @keywords internal
#' @export
intermediateMessage <- function(text, loc='middle', intermediateMessages=TRUE) {
# Display brief message(s) during metrics calculation, if enabled by 
# intermediateMessages
#
# ARGUMENTS:
# text     Character variable with text to display
# loc      Either 'start','middle' or 'end', used to describe whether
#          the location of a section for which intermediate results are
#          wanted.  This value is used to affect the display format.
# intermediateMessages  A logical variable indicating whether messages should be
#                       printed.  The default value is TRUE.

  if(intermediateMessages) {
      outText <- text
      if(loc=='start') outText <- paste('  ',outText, sep='')
      if(loc=='end') outText <- paste(outText, '\n', sep='')
      cat(outText)
  }
}

#' @keywords internal
#' @export
lag <- function(df, v, lag.v, offset=1) {
# Mimics the SAS LAGx() function by creating new column(s) containing the
# values of the specified column(s) from the previous row.
#
# Returns the input dataframe with the lagged values in new column(s) with the
# specified name(s).
#
# e.g. lag(df, 'substrate', 'substrate.2', offset=2)
#         input df: key substrate     output df: key substrate substrate.2
#                   1   FN                       1   FN        <NA>
#                   2   GF                       2   GF        <NA>
#                   3   SA                       3   SA        FN
#                   4   XB                       4   XB        GF
#                   5   CB                       5   CB        SA
#
#
#
# ARGUMENTS:
# df        dataframe to add lag columns to
# v         names of existing columns in dataframe with values to lag
# lag.v     names of new columns in dataframe to contain lagged values
# offset    integer value specifying the number of rows to lag the value by,
#             defaulting to 0.
#
# ASSUMPTIONS:
#

  # Sanity check
  if(length(v) != length(lag.v)) {
       stop("Error: number of column names not same as number of lagged names")
  }

  # Lagging single columns is slightly different than multiple columns as
  # I've been unable to get one method that works for both.
  if(length(v)==1) {
      df[lag.v] <-     c(matrix(rep(NA,offset*length(v))
                               ,nrow=offset, ncol=length(v)
                               )
                        ,df[1:(nrow(df)-offset),v]
                        )
  } else {
      df[lag.v] <- rbind(matrix(rep(NA,offset*length(v))
                               ,nrow=offset, ncol=length(v)
                               ,dimnames=list(NULL,v)
                               )
                        ,df[1:(nrow(df)-offset),v]
                        )
  }

  return(df)
}


#' @keywords internal
#' @export
last <- function(df, v, last.v) {
# Mimics the SAS LAST. operator by creating a new column containing a boolean
# flag which is TRUE if the row has different by-variable values than the
# following row.  Returns dataframe with flag in added column.
#
# Ordering the input dataframe prior to using this function is not required, but
# will be useful.
#
# ARGUMENTS:
# df        dataframe to add 'first' flag column to
# v         name of existing column in dataframe with values to
# last.v    names of new column in dataframe to contain first flag values
#

  # Lead named column by one row into temporary column
  df <- lead(df, v, '..vLead', offset=1)

  # A row is flagged 'last' if the named column changes in the next row,
  # otherwise they are not.  The comparison must be coerced into a vector
  # to allow the 'last' column to be named properly.  The last row must be
  # 'last' by definition; if it is not explicitly set, the comparison will be
  # NA.
  #df[last.v] <- as.vector(df[v] != df['..vLead'])
  # A row is flagged 'last' if the named column is changed from the previous
  # row, otherwise they are not.  The comparison must be coerced into a vector
  # to allow the 'last' column to be named properly.  Missing (NA) values in
  # either the data column or it's lead() value are treated as comparable values
  # A row is flagged as last if the value v is not the same as the leaded value
  # of v OR if their inequality is NA and v and lead(v) are not both NA.
  # The old check ( df[last.v] <- as.vector(df[v] != df['..vLead']) ) resulted
  # in first=NA if either v or vLead were NA.
  df[last.v] <- as.vector(ifelse(is.na(df[v]) | is.na(df['..vLead'])
                                 ,is.na(df[v]) != is.na(df['..vLead'])
                                 ,df[v] != df['..vLead']
                                 )
                          )

  # The last row must be 'last' by definition; if it is not explicitly set, the
  # comparison will be NA.
  df[nrow(df), last.v] <- TRUE

  # Drop temporary column
  df['..vLead'] <- NULL

  return(df)
}

#' @keywords internal
#' @export
lead <- function(df, v, lead.v, offset=1) {
# Antimimics the SAS LAGx() function by creating a new column containing the
# values of the specified column from the next row.
#
# Returns the input dataframe with the anti-lagged values in a new column with
# the specified name.
#
# e.g. lead(df, 'substrate', 'substrate.2', offset=2)
#         input df: key substrate     output df: key substrate substrate.2
#                   1   FN                       1   FN        SA
#                   2   GF                       2   GF        XB
#                   3   SA                       3   SA        CB
#                   4   XB                       4   XB        <NA>
#                   5   CB                       5   CB        <NA>
#
#
#
# ARGUMENTS:
# df        dataframe to add lead columns to
# v         names of existing columns in dataframe with values to lead
# lead.v    names of new columns in dataframe to contain lead values
# offset    integer value specifying the number of rows to lead the value by,
#             defaulting to 0.
#
# ASSUMPTIONS:
#

  # Sanity check
  if(length(v) != length(lead.v)) {
       stop("Error: number of column names not same as number of lagged names")
  }

  # Leading single columns is slightly different than multiple columns as
  # I've been unable to get one method that works for both.
  if(length(v)==1) {
      df[lead.v] <-     c(df[(1+offset):nrow(df), v]
                         ,matrix(rep(NA,offset*length(v))
                                ,nrow=offset, ncol=length(v)
                                )
                         )
  } else {
      df[lead.v] <- rbind(df[(1+offset):nrow(df), v]
                         ,matrix(rep(NA,offset*length(v))
                                ,nrow=offset, ncol=length(v)
                                ,dimnames=list(NULL,v)
                                )
                         )
  }

  return(df)
}


#' @keywords internal
#' @export
modalClass <- function(x, values, classes) {
# Determines most common classes, based on counts for each class.  Counts are
# expected to occur in the same row (i.e. long format).  This function is
# intended to be called from within a loop.  In the case of ties, all modal
# classes will be included, separated by a comma and a space. e.g.
#
#     foo<-subset(df, select='uid')
#     foo$mode <- NA
#     for(i in 1:nrow(df)) {
#       foo$mode[i] <- modalClass(df[i]
#                                , c('count1','count2','count3'...)
#                                , c('ones','twos','threes',...)
#                                )
#     }
#
#
# ARGUMENTS:
# x        named values of interest, e.g. a single row of a dataframe
# values   list of column names in x containing cover or other values on
#          which the modal class will be based.
# classes  list of class names associated with the columns, given in the order
#          that the values are listed.
#
# ASSUMPTIONS
# Column names listed by values arg occur in x.
# The list of class names is in the same order as the list of 
# The number of elements in values is the number of elements in classes.
#

  # separate quantities of interest in the row, just as a shortcut
  qq <- x[values]

  if(all(is.na(qq))) {
      # Mode is undefined if all values in row are missing.
      modalClasses <- NA

  } else {
      # determine locations of maximum value as a series of boolean values
      # and change any NA to FALSE
      bb <- qq==max(qq, na.rm=TRUE)
      bb <- bb==TRUE & !is.na(bb)

      # select the most common class names based on the maxima of the values
      # which were just selected.  
      modalClasses <- paste(classes[bb], collapse=', ')
  }

  return(modalClasses)
}


#' @keywords internal
#' @export
modalClasses <- function(df, classes, weights, delim = ', ') {
# Determines most common classes at a site based on weights for each.  
# Ties are allowed and separated by the string given by argument delim.
# Returns dataframe with columns SITE and modalClass.
#
# ARGUMENTS:
# df		dataframe with class information, with columns SITE and those 
#			  specified by classes and weights arguments.
# classes	name of column in df with names of each class
# weights	name of column in df with weights for each class 
# delim		character string used to delimit ties

	# Would use aggregate() or ddply(), but we need both classes and weights 
	# columns together so we can use the value of weights to select the values
	# of classes.
	eachSite <- lapply(split(df, df$SITE)
			       	  ,function(site, classCol, weightCol) {
						  
						if(all(is.na(site[[weightCol]]))) {
							maxWt <- NA
						} else {
							maxWt <- max(site[[weightCol]], na.rm=TRUE)
						}
						
				 	    results <- sort(subset(site, site[[weightCol]] == maxWt)[[classCol]]
									   ,na.last=TRUE
							   		   )
						rc <- data.frame(SITE=unique(site$SITE)
									    ,modalClasses=paste(results, collapse=delim)
									  	,stringsAsFactors=FALSE
										)
										
						return(rc)
					   }
				   	  ,classes, weights
				   	  )
	modes <- do.call(rbind, eachSite)
	row.names(modes) <- NULL
	
	return(modes)
}


#' @keywords internal
#' @export
modalCount <- function(x) {

# 03/10/10 cws created
# Returns the number of occurences as an integer of the most common (mode)
# element in x.

  v <- modalvalue(x)
  n <- sum(x==v)

  return(n)
}


#' @keywords internal
#' @export
modalvalue <- function(x, na.rm=FALSE) {
# Returns the mode of the specified data. This function was taken from the R 
# wiki site:
# http://wiki.r-project.org/rwiki/doku.php?id=tips:stats-basic:modalvalue
#
# NOTE: It might be good to make sure x is a vector of numbers instead of just 
# flattening it with unlist().
# NOTE: When using this function with aggregate(), it is sometimes necessary 
# to convert x to factors using as.factors() to prevent the output from 
# creating a vector 'if (stringsAsFactors) factor(x) else x' instead of 'x'.
# I do not know why.
#
# ARGUMENTS:
# x        vector of values of interest
# na.rm    If TRUE, will remove NA values from consideration

    x <- unlist(x);
    if(na.rm) x <- x[!is.na(x)]
    u <- unique(x);
    n <- length(as.factor(u));
    frequencies <- rep(0, n);
    for(i in 1:n)
    {
        if(is.na(u[i]))
        {
            frequencies[i] <- sum(is.na(x))
        } else
        {
            frequencies[i] <- sum(x==u[i], na.rm=TRUE)
        }
    }
    u[which.max(frequencies)]
}

#' @keywords internal
#' @export
modalValues <- function(x, delim='&', na.rm=FALSE) {
# Returns character string containing modal values of the input vector, 
# separating ties with the specified character.  This ability to report tied 
# modal values distinguishes this function from modalvalue(). This function
# also differs by always returning a character value.
#
# ARGUMENTS:
# x			vector of values of interest
# delim		character string used to separate tied values
# na.rm		logical, if TRUE, will remove NA values from consideration
# 
# ASSUMPTIONS:
# Vector x is of length > 0

#	x <- unlist(x)				# Coerce list into a vector
	if(na.rm) x <- x[!is.na(x)]
	u <- unique(x)
	n <- length(u)
	
	if(n==0) {
		rc <- as.character(NA)					# Handle case where x is now empty
	} else {
		frequencies <- rep(0, n)
	
		for(i in 1:n) {
			if(is.na(u[i])) {
				frequencies[i] <- sum(is.na(x))		# Count missing values this way
			} else {
				frequencies[i] <- sum(x == u[i], na.rm=TRUE)	# count nonmissing values this way
			}
		}
	
#		rc <- u[which.max(frequencies)]
		modes <- u[frequencies == max(frequencies)]
		if(all(is.na(modes))) {
			rc <- as.character(NA)
		} else {
			rc <- paste(sort(modes, na.last=TRUE), collapse=delim)
		}
	}

	return(rc)
}


#' @keywords internal
#' @export
normalizedCover <- function(df, coverValue, coverNorm, allowTotalBelow100=FALSE) {
# Calculates normalized areal cover values so they total 100% at each
# station/subid.  Returns the input data frame with an additional column
# containing the normalized cover values.
#
#   coverNorm <- coverValue / sum(coverValue at each station)
#
# ARGUMENTS:
# df                 Input data frame containing cover values to be normalized.
#                    This is expected to have the same columns as the data frame
#                    with the NLA phab data, particularly SITE, STATION and the 
#                    column specified by the coverValue argument.
# coverValue         Name of column in data frame with cover values.
# coverNorm          Name of new column with normalized cover values.
# allowTotalBelow100 Set to TRUE if the set of cover categories being 
#                    normalized do not exhaust all cover possibilities and thus
#                    are allowed to sum to below 100% but not over 100%.  A 
#                    FALSE value will result in values which are always 
#                    normalized to 100%.
#
# ASSUMPTIONS:
# The input data frame contains only those parameters to include in the
#   normalization.
# The input data frame uses columns  and subid to uniquely identify
#   stations in a site.
# The cover values range from 0-1, inclusive.
# The input data frame does not contain columns named .sumCover, .coverNorm or
#   .nValidValues
#

  # calculate total recorded cover at each station and fold it back into
  # the original dataframe
  tt <- aggregate(df[coverValue]
                 ,list('SITE'=df$SITE, 'STATION'=df$STATION)
                 ,sum, na.rm=TRUE
                 )
  tt <- rename(tt, coverValue, '.sumCover')

  tt <- merge(df, tt, by=c('SITE','STATION'), all.x=TRUE)

  # count number of values; will be zero if all NA or NaN, then fold into
  # the dataframe.  Zero counts of data for a class mean we can't
  # normalize data for that class.
  tt2 <- aggregate(df[coverValue]
                 ,list('SITE'=df$SITE, 'CLASS'=df$CLASS)
                 ,count
                 )
  tt2 <- rename(tt2, coverValue, '.nValidValues')

  tt <- merge(tt,tt2, by=c('SITE','CLASS'), all.x=TRUE)

  # Normalize cover.  Calculate normalizing factor, handling special cases:
  #   a) no numeric values (.nValidValues==0, i.e. all NA or NaN)
  #   b) zero total cover (.sumCover==0)
  factor <- ifelse(allowTotalBelow100 & tt$.sumCover <= 1, 1, 1/tt$.sumCover)
  factor[tt$.sumCover==0] <- 0
  factor[tt$.nValidValues==0] <- NA

  tt$.coverNorm <- unlist(tt[coverValue]) * factor

  tt$.sumCover <- NULL
  tt$.nValidValues <- NULL

  tt <- rename(tt, '.coverNorm', coverNorm)

  return(tt)
}


#' @keywords internal
#' @export
nWadeableStationsPerTransect <- function(thal) {

# Estimates the intended number of wadeable thalweg stations to be visited at
# an NRSA study site.
#
#  2/11/10 cws created
#  3/09/10 cws intended number of stations is based on most common last station
#          at a reach instead of maximum last station at a reach.  Thus if a
#          crew samples 11 stations at a single transect and 9 everywhere else,
#          the expected number of transects is still 9.
#  3/10/10 cws Handling case of no clear mode; correcting overall logic of test.
#

# Estimates the intended number of wadeable thalweg stations at each transect
# which are considered sampled (even if there is no data) for the purposes of
# calculating residual pools and channel lengths.  The number of stations at
# a transect is calculated as the greater of either the number of stations
# occuring in the dataframe for that transect, or the most common count of
# stations (i.e. station mode) occuring at that site.
#
# Currently, side-channel transects are ignored.
#
# Returns a dataframe with SITE, TRANSECT and nSta=number of stations which
# 'should be' at the transect.
#
# ARGUMENTS:
# thal      dataframe of thalweg data for wadeable reaches
#
# ASSUMPTIONS:
# At most 1 station was sampled at any K transect.
#
#thal <- dplyr::rename(thal, SITE=UID)      # uncomment this line for use with old mets* functions

  thal <- subset(thal, TRANSECT %in% LETTERS[1:11])
  
  staLast <- aggregate(list('staLast'=thal$STATION)
                      ,list('SITE'=thal$SITE, 'TRANSECT'=thal$TRANSECT)
                      ,max, na.rm=TRUE
                      )
  staMode <-aggregate(list('staLastMode'=staLast$staLast)
                     ,list('SITE'=staLast$SITE)
                     ,modalvalue
                     )
  staModeCount <-aggregate(list('staModeCount'=staLast$staLast)
                          ,list('SITE'=staLast$SITE)
                          ,modalCount
                          )

  tt <- merge(staLast, staMode, by='SITE')
  tt <- merge(tt, staModeCount, by='SITE')

  # calculate nSta at each transect, taking advantage of the fact that stations
  # are numeric, and thus that the last station to be expected for a transect
  # is the number of transects to be expected at that transect, adding 1 to
  # the station to account for station numbering starting at 0.
  tt$lastSta <-  ifelse(tt$staModeCount > 6
                       # if a clear mode exists, 'top off' the transect to at
                       # least the most common last station of transects A-J
                       ,ifelse(tt$TRANSECT %in% LETTERS[1:10]
                              ,ifelse(tt$staLast < tt$staLastMode
                                     ,tt$staLastMode
                                     ,tt$staLast
                                     )
                              # if it exists, transect K should only have station 0
                              ,0
                              )
                        # do not change last station if no clear mode
                       ,tt$staLast
                       )
  tt$nSta <- tt$lastSta+1
  
  tt<-subset(tt, select=c(SITE,TRANSECT,nSta))                           

# tt <- dplyr::rename(tt, UID=SITE)      # uncomment this line for use with old mets* functions
  return(tt)
}


#' @keywords internal
#' @export
protectedMean <- function(x, na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE, ...) {
# Calculates mean using mean(), but can trap additional incalculable values.
#
# Note: The default values for dealing with incalculable values mirrors mean() 
# while also providing protection against NaN results from a mean of zero
# nonmissing values.  
#
# ARGUMENTS:
# x			vector argument to mean
# na.rm		logical, calculation ignores NA values if TRUE
# inf.rm	logical, calculation ignores Inf, -Inf values if TRUE
# nan.rm	logical, calculation ignores NaN values if TRUE
# ...		additional arguments passed to or from other methods, e.g. trim

	if(inf.rm & length(x) > 0) {
		x <- x[!is.infinite(x)]
	}

	if(nan.rm & length(x) > 0) {
		x <- x[!is.nan(x)]
	}

	if (length(x) == 0) {
		rc <- as.numeric(NA)
	}
	else if(all(is.na(x)))	{
		rc <- as.numeric(NA)
	}
	else {
#print(sprintf("mean(%s)", paste(x, collapse=',')))
		rc <- mean(x, na.rm=na.rm, ...)	# mean of all NA is NaN, should be NA
	}
	
	return(rc)
	
}


#' @keywords internal
#' @export
protectedSum <- function(x, na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE, ...) {
# Calculates a vector sum using sum(), but can trap additional incalculable values.
#
# Note: The default values for dealing with incalculable values mirrors mean() 
# while also providing protection against NaN results from a mean of zero
# nonmissing values.  
#
# ARGUMENTS:
# x			vector argument to mean
# na.rm		logical, calculation ignores NA values if TRUE
# inf.rm	logical, calculation ignores Inf, -Inf values if TRUE
# nan.rm	logical, calculation ignores NaN values if TRUE
# ...		additional arguments passed to or from other methods, e.g. trim

	if(inf.rm & length(x) > 0) {
		x <- x[!is.infinite(x)]
	}

	if(nan.rm & length(x) > 0) {
		x <- x[!is.nan(x)]
	}

	if (length(x) == 0) {
		rc <- as.numeric(NA)
	}
	else if(all(is.na(x)))	{
		rc <- as.numeric(NA)
	}
	else {
#print(sprintf("mean(%s)", paste(x, collapse=',')))
		rc <- sum(x, na.rm=na.rm, ...)	# mean of all NA is NaN, should be NA
	}
	
	return(rc)
	
}


#' @keywords internal
#' @export
rename <- function(df, old, new) {
  # This method assumes that names in old & new are listed in the same
  # order as the names in df 
  # names(df)[names(df) %in% old] <- new
  if(length(old) != length(new)) {
      print("ERROR: rename() used with different lengths for old and new names")
      return(NULL)
  }

  for(i in 1:length(old)) {
      names(df)[names(df)==old[i]] <- new[i]
  }

  return(df)
}


#' @keywords internal
#' @export
summaryby <- function(xxx,yyy,zzz) {

    # This function uses aggregate to determine a statistic (mean, count, sd,
    # range, max, min) of VALUE for each SITE in a data table.  Inputs are  the table
    # name (xxx), the desired statistic (yyy), and the new name for the statistic (zzz).
    # The output is a new dataset with variables SITE, Metric and Result.
# xxx <- xxx %>% dplyr::rename(SITE=UID, VALUE=RESULT)
    www <- if(yyy == 'count' ) {
               aggregate( list('VALUE'=xxx$VALUE), list(SITE=xxx$SITE), get(yyy))
                  
           } else if(yyy=='max' | yyy=='min' | yyy=='sum')  {
               aggregate( list('VALUE'=xxx$VALUE), list(SITE=xxx$SITE), function(x){ifelse(all(is.na(x)), NA, get(yyy)(x, na.rm=TRUE))})

           } else {
               aggregate( list('VALUE'=xxx$VALUE), list(SITE=xxx$SITE), yyy, na.rm=TRUE)
           }

    www$METRIC<- zzz
    www <- www[c('SITE','METRIC','VALUE')]

# www <- www %>% dplyr::rename(UID=SITE, RESULT=VALUE)
 
    return(www)
}


#' @keywords internal
#' @export
trimws <- function(text) {
# Trims leading and trailing white space from a character string.
# Taken from http://togaware.com/datamining/survivor/Trim_Whitespace.html
#
# ARGUMENTS:
# text     character string from which leading and trailing white space
#            will be removed
#

    gsub("^[[:blank:]]*", "", gsub("[[:blank:]]*$", "", text))
}


#' @keywords internal
#' @export
uidCreate <- function(df, keys, sep='+') {
# Returns a vector consisting of a single unique identifier for the specified 
# dataframe using the specified list of keys.  This is done to replace multiple
# keys with a single one prior to metrics computation.
#
# Arguments:
# df       The data frame for which the unique identifiers will be developed.
# keys     A list of names in the dataframe which combine to uniquely specify
#            a location for which metrics will be calculated.
# sep      A character string used to distinguish between segments of the
#             individual keys in the uid.
# 


  for(k in 1:length(keys)) {
      if(k==1) {
          uniqueID<-df[,keys[k]]
      } else {
          uniqueID<-paste(uniqueID, df[,keys[k]], sep=sep)
      }
  }

  return(uniqueID)
}


#' @keywords internal
#' @export
uidSeparate <- function(df, uidName, keyNames, sep='+') {
# Returns the dataframe consisting of the initial dataframe with additional
# columns containing the components of the uid.  This function reverses the
# operation of uidCreate().
#
# ARGUMENTS:
# df        The data frame for which separate keys will be created from the
#             single variable named in 'uidName'
# uidName   A character string naming the variable containing the unique
#             identifier for each site visit.
# keyNames  A character vector containing the names of columns to be created
#             in the dataframe and containing portions of the uid values.
# sep       A character string used to distinguish between segments of the
#             individual keys in the uid.
#


  for (r in 1:length(df[,1])) {
      # for each row in the input dataframe, parse the uid and put the
      # individual parts in the specified variables.  The use of as.character
      # converts the uidName column from a factor if it needs to be.
      keyParts<- unlist(strsplit(as.character(df[r,uidName]), sep, fixed=TRUE))
      for (k in 1:length(keyNames)) {
          df[r,keyNames[k]] <- keyParts[k]
      }
  }

  return(df)

}



# end of file