#' Example data for NLA physical habitat field measurements
#'
#' @format a data frame with 7449 rows and 4 variables:
#' \describe{
#'  \item{SITE}{int Site visit identifier}
#'  \item{STATION}{chr Station, range from A-I}
#'  \item{PARAMETER}{chr Parameter name for field measurement}
#'  \item{VALUE}{chr Value of parameter, combination of numeric and string
#'  values}
#' }
#' @source aquamet package
"nlaPhab"

#' Example data for NRSA channel measurements
#'
#' @format a data frame with 1865 rows and 5 variables
#' \describe{
#'  \item{SITE}{Site visit identifier}
#'  \item{TRANSECT}{Transect value, range A-K}
#'  \item{SAMPLE_TYPE}{Sample type, corresponding to field form used, values
#'  BANKW, FISHCOVW, LWDW, BANKB, CONSTB, FISHCOVB, LITTORALB, LWDB}
#'  \item{PARAMETER}{Parameter name for field measurment}
#'  \item{VALUE}{Value of parameter}
#' }
#' @source aquamet package
"nrsaChannel"

#' Example data for NRSA channel constraint characteristics
#'
#' @format a data frame with 25 rows and 4 variables
#' \describe{
#'  \item{SITE}{Site visit identifier}
#'  \item{SAMPLE_TYPE}{Sample type, corresponding to field form used, value
#'  CONSTRAINT}
#'  \item{PARAMETER}{Parameter name for field measurment}
#'  \item{VALUE}{Value of parameter}
#' }
#' @source aquamet package
"nrsaChanconst"

#' Example data for NRSA channel depth measurements in non-wadeable sites
#'
#' @format a data frame with 132 rows and 5 variables
#' \describe{
#'  \item{SITE}{Site visit identifier}
#'  \item{SAMPLE_TYPE}{Sample type: CHANDEPTHB}
#'  \item{TRANSECT}{Transect value, range A-K}
#'  \item{LINE}{Line number from form}
#'  \item{PARAMETER}{Parameter name for field measurment}
#'  \item{VALUE}{Value of parameter}
#' }
#' @source aquamet package
"nrsaLittoral"

#' Example data for NRSA channel geometry, e.g., slope and bearing measurements
#'
#' @format a data frame with 497 rows and 6 variables
#' \describe{
#'  \item{SITE}{Site visit identifier}
#'  \item{TRANSECT}{Transect value, range A-K}
#'  \item{SAMPLE_TYPE}{Sample type, corresponding to field form used, values
#'  PHAB_SLOPE and PHAB_CHANBFRONT}
#'  \item{REP}{Replicate for measurement}
#'  \item{PARAMETER}{Parameter name for field measurment}
#'  \item{VALUE}{Value of parameter}
#' }
#' @source aquamet package
"nrsaSlope"

#' Example data for NRSA channel cross-section measurements for wadeable streams
#'
#' @format a data frame with 560 rows and 6 variables
#' \describe{
#'  \item{SITE}{Site visit identifier}
#'  \item{TRANSECT}{Transect value, range A-K}
#'  \item{BANK}{Bank where measurement taken, values, LF, RT, NONE}
#'  \item{SAMPLE_TYPE}{Sample type, corresponding to field form used, values
#'  PHAB_CHANW and PHAB_THALW}
#'  \item{PARAMETER}{Parameter name for field measurment}
#'  \item{VALUE}{Value of parameter}
#' }
#' @source aquamet package
"nrsaChanxsec"

#' Example data for NRSA legacy riparian vegetation
#'
#' @format a data frame with 264 rows and 4 variables
#' \describe{
#'  \item{SITE}{Site visit identifier}
#'  \item{SAMPLE_TYPE}{Sample type: RIPLEG}
#'  \item{TRANSECT}{Transect value, range A-K}
#'  \item{PARAMETER}{Parameter name for field measurment}
#'  \item{VALUE}{Value of parameter}
#' }
#' @source aquamet package
"nrsaInvleg"

#' Example data for NRSA thalweg-related measurements
#'
#' @format a data frame with 3065 rows and 6 variables
#' \describe{
#'  \item{SITE}{Site visit identifier}
#'  \item{TRANSECT}{Transect value, range A-K}
#'  \item{STATION}{Station between transects}
#'  \item{SAMPLE_TYPE}{Sample type, corresponding to field form used, values
#'  PHAB_THAL and PHAB_THALW}
#'  \item{PARAMETER}{Parameter name for field measurment}
#'  \item{VALUE}{Value of parameter}
#' }
#' @source ALL_THE_NRSA database
"nrsaThalweg"

#' Example data for NRSA visual riparian estimates
#'
#' @format a data frame with 2200 rows and 6 variables
#' \describe{
#'  \item{SITE}{Site visit identifier}
#'  \item{TRANSECT}{Transect value, range A-K}
#'  \item{BANK}{Bank where measurement taken, values, LF, RT}
#'  \item{SAMPLE_TYPE}{Sample type, corresponding to field form used, values
#'  PHAB_CHANW and PHAB_CHANB}
#'  \item{PARAMETER}{Parameter name for field measurment}
#'  \item{VALUE}{Value of parameter}
#' }
#' @source ALL_THE_NRSA database
"nrsaChanrip"

#' Example data for NRSA visit information for example
#'
#' @format a data frame with 4 rows and 2 variables
#' \describe{
#'  \item{SITE}{Site visit identifier}
#'  \item{VALXSITE}{Sampling methodology used}
#' }
#' @source aquamet package
"nrsaVisits"
