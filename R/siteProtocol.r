siteProtocol <- function(sites, visits) {

################################################################################
# siteProtocol.r
#
# 03/25/10 cws: Changed protocol to PROTOCOL for consistency.
# 09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#          instead.
# 06/04/12 tmk: Removed use of ODBC data connection and replaced with data
#          input from a csv file.  In function siteProtocol.1, replaced existing
#          values with character values in the input to argument select in the
#          call to subset.
# 08/15/12 tmk: Modified data input to use a data frame containing the stream
#          verification form data file rather than a csv file.
# 11/19/15 cws Uses SITE instead of UID now.
#
# Relates NRSA UID values to the protocol used to sample them, based on the
# information recorded on the Stream Verification form.
# Returns a dataframe with columns UID and protocol, and one row per unique
# UID value if successful, or a character string describing the error if
# a problem was encountered.
#
# Arguments:
#   sites = a vector of UID values.
#   visits = a data frame containing the stream verification form data file.
#
# ASSUMPTIONS:
#   Table tblVISITS2 has column VALXSITE, which may have values ACCDENIED,
#   ALTERED, BOATABLE, DRYNOVISIT, DRYVISIT, IMPOUNDED, INACCPERM, INACCTEMP,
#   INTWADE, MAPERROR, NOTBOAT, NOTWADE, OTHR_NSP, OTHR_NST, PARBYBOAT,
#   PARBYWADE, WADEABLE, WETLAND or NA
################################################################################

# Determine protocol values
  rr <- siteProtocol.1(sites, visits)

# Return results
  return(rr)
}



siteProtocol.1 <- function(sites, siteInfo) {

# Does the work for siteProtocol() relying on VALXSITE
# ARGUMENTS:
#  sites     A vector of UID values
#   siteInfo  A data frame with site information found in tblVISITS2

# Use information to assess which protocol was used.
  tt <- subset(siteInfo, SITE %in% unique(sites))

# Make easy determinations of BOATABLE, WADEABLE, NONE (unsampled) or
# nothing at all if something slips through the cracks.
  tt$PROTOCOL <- ''
  selection <- tt$VALXSITE %in% c('BOATABLE','PARBYBOAT','ALTERED')
  if(any(selection)) tt[selection,]$PROTOCOL <- 'BOATABLE'

  selection <- tt$VALXSITE %in% c('INTWADE','PARBYWADE','WADEABLE','ALTERED')
  if(any(selection)) tt[selection,]$PROTOCOL <- 'WADEABLE'

  selection <- is.na(tt$VALXSITE) |
               !(tt$VALXSITE %in% c('BOATABLE','PARBYBOAT','ALTERED','INTWADE'
                                   ,'PARBYWADE','WADEABLE')
                )
  if(any(selection)) tt[selection,]$PROTOCOL <- 'NONE'
  tt <- subset(tt, select=c('SITE', 'PROTOCOL'))
  
# Return results
  return(tt)
}
