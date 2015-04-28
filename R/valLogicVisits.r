# valLogicVisits.r
#
# Checks for consistency between tblVISITS2 and the data tables.
# Currently just checks VALXSITE to make sure siteProtocol() is accurate.
#
# 05/26/10 cws created
# 09/16/10 cws Removing hardcoding of NRSA database name, using NRSAdbName
#          instead.
# 11/24/10 cws rewriting to use standard validation interface.
# 12/16/10 cws valLogicVisits.protocolConsistency modified to create its own
#          RODBC connection to the database, as it should have all this time.
#          Also, valLogicVisits.sampleConsistency is deprecated as those
#          columns are no longer in tblVISITS2.
#

require(RODBC)

valLogicVisits <- function()
#
{
  intermediateMessage('valLogicVisits begun ', loc='start')
  chan <- odbcConnect(NRSAdbName)
  visits <- fetchNRSATable(chan, 'tblVISITS2')

  # Accumulate problems here.
  probs<-NULL
  
  # Check for inconsistent sample protocols
  intermediateMessage('.1')
  pp <- valLogicVisits.protocolConsistency(visits)
  if(!is.null(pp)) probs <- rbind(probs, pp)
print(dim(probs)); print(dim(pp))

  
  # Check for cases where VISIT_NO==0 xor DATE_COL==January 1 of a year.  These
  # values together indicate a recon site, and should only occur together.
  intermediateMessage('.2')
  tt <- subset(visits
              ,((VISIT_NO == 0) + (format(DATE_COL, '%d%m') == '0101')) == 1
              )
  pp <- sprintf("At UID=%s, recon site values are inconsistent; VISIT_NO=%s, DATE_COL=%s"
               ,tt$UID, tt$VISIT_NO, tt$DATE_COL
               )
  if(!is.null(pp)) probs <- rbind(probs, pp)
print(dim(probs)); print(dim(pp))


  # Check for cases where both SAMPLED and NOT_SAMPLED are missing.
  intermediateMessage('.3')
  tt <- subset(visits, is.na(SAMPLED) & is.na(NOT_SAMPLED))
  pp <- sprintf("At UID=%s, SAMPLED and NOT_SAMPLED are missing; XSTATUS=%s, SITESAMP=%s"
               ,tt$UID, tt$XSTATUS, tt$SITESAMP
               )
  if(!is.null(pp)) probs <- rbind(probs, pp)
print(dim(probs)); print(dim(pp))


  # Check for inconsistency in sample information
#  intermediateMessage('.4')
#  pp <- valLogicVisits.sampleConsistency(visits)
#  if(!is.null(pp)) probs <- rbind(probs, pp)


  intermediateMessage(' .Done.', loc='end')
  return(probs)
}


valLogicVisits.protocolConsistency <- function(visits)
# Look for inconsistencies in sampling protocol.  Returns Nx1 matrix of
# cases in which VALXSITE in tblVISITS does not match the protocol consistency
# specified by the SAMPLE_TYPE values found in a selection of data tables.
# Also prints warnings about inconsistencies between selected data tables.
#
# ARGUMENTS:
# visits    dataframe containing contents tblVISITS2.
#
{
  chan <- odbcConnect(NRSAdbName)
  on.exit(odbcClose(chan))
  
  # VISITS::VALXSITE should be consistent with SAMPLE_TYPE in tables.  Since
  # only UID and SAMPLE_TYPE are needed, we'll use sqlFetch() instead of
  # fetchNRSATable() to save time and let the server do the summarization.
  sample_types <- NULL
  for(tName in c('tblBANKGEOMETRY2','tblCHANCOV2','tblCHANNELGEOMETRY2'
                ,'tblFISHCOVER2','tblTHALWEG2','tblWOOD2'
                )
     ) {
#      intermediateMessage(sprintf(' .reading %s', tName))
      qq <- sprintf('select BATCHNO, SAMPLE_TYPE, count(*) AS N
                       from %s
                       group by BATCHNO, SAMPLE_TYPE
                    '
                   ,tName
                   )
      stCounts <- sqlQuery(chan, qq, stringsAsFactors=FALSE)
      if(!is.data.frame(stCounts)) {
          print(sprintf("Could not read table %s", tName))
          next;
      }
      stCounts$BATCHNO <- trimws(as.character(stCounts$BATCHNO))
      stCounts$SAMPLE_TYPE <- trimws(stCounts$SAMPLE_TYPE)
      stCounts$stProtocol <- ifelse(stCounts$SAMPLE_TYPE %in% c('PHAB_CHANBFRONT','PHAB_CHANB','PHAB_THAL','PHAB_CHANBFRONT')
                                   ,'BOATABLE'
                                   ,ifelse(stCounts$SAMPLE_TYPE %in% c('PHAB_CHANW','PHAB_THALW','PHAB_SLOPE')
                                          ,'WADEABLE'
                                          ,'XXXXXXXX'
                                          )
                                   )
      if(any(stCounts$stProtocol=='XXXXXXXX')) {
          cat(sprintf("unknown SAMPLE_TYPE %s", subset(stCounts, stProtocol=='XXXXXXXX')$SAMPLE_TYPE))
      }
      
      tt <- aggregate(list(stConsistent=stCounts$stProtocol)
                     ,list(BATCHNO=stCounts$BATCHNO, stProtocol=stCounts$stProtocol)
                     ,function(x) { length(unique(x))==1 }
                     )
     sample_types <- rbind(sample_types, tt)
  }
  sample_types <- rename(sample_types, 'BATCHNO', 'UID')
  
  
  # Accumulate detected problems here
  probs <- NULL

  # Report occurences of SAMPLE_TYPE protocols inconsistent within a table
  if(any(!sample_types$stConsistent)) {
      tt <- with(subset(sample_types, !stConsistent)
                ,aggregate(list(UID=UID)
                          ,list(stProtocol=stProtocol)
                          ,function(x) { paste(x, collapse=', ') }
                          )
                )
      print("WARNING: There are sites with SAMPLE_TYPE inconsistencies:")
      print(tt)
  }

  if(any(sample_types$stConsistent)) {
      # Determine protocol at each site based on summary of all tables.
      # Report occurences of SAMPLE_TYPE protocols inconsistent across tables.
      tt <- with(subset(sample_types, stConsistent)
                ,aggregate(list(tablesConsistent=stProtocol)
                          ,list(UID=UID)
                          ,function(x) { count(unique(x)) == 1 }
                          )
                )
      actual <- merge(unique(subset(sample_types, stConsistent))
                     ,tt
                     ,by='UID'
                     )

      # User should know of inconsistencies in SAMPLE_TYPE between tables.  There
      # is no place for this kind of information in the standard validation
      # process, so write the warnings to the output.
      if(any(!actual$tablesConsistent)) {
          tt <- with(subset(actual, !tablesConsistent)
                    ,aggregate(list(UID=UID)
                              ,list(stProtocol=stProtocol)
                              ,function(x) { paste(x, collapse=', ') }
                              )
                    )
          print("Warning: There are sites with protocol inconsistencies across tables:")
          print(tt)
      }
      
      if(any(actual$tablesConsistent)) {
          # Compare the sampling protocol in tblVISITS with the protocol used
          # based on the SAMPLE_TYPE values in the tables (where these are
          # consistent).
          visitsProtocol <- siteProtocol(unique(actual$UID))
          tt <- merge(visitsProtocol, actual[c('UID','stProtocol')], by='UID')
          
          if(any(tt$PROTOCOL != tt$stProtocol)) {
#              print("WARNING: There are sites for which reported sampling protocol does not agree with SAMPLE_TYPE in tables:")
#              print(subset(tt, tt$PROTOCOL != tt$stProtocol))
               pp <- with(merge(visits
                               ,subset(tt, PROTOCOL != stProtocol)
                               ,by='UID'
                               )
                         ,sprintf("At UID=%s, mismatched protocols between tblVISITS and other tables, VALXSITE=%s but data tables specify %s"
                                 ,tt$UID, tt$VALXSITE, tt$stProtocol
#                                 ,pp$UID, pp$VALXSITE, pp$stProtocol
                                 )
                         )
               probs <- rbind(probs, pp)
          }
      }

  }

  if(!is.null(probs)) probs <- as.matrix(probs, ncol=1)
  return(probs)
}


valLogicVisits.sampleConsistencyDEPRECATED <- function(visits)
# Check for cases where SAMP_STATUS indicates there are samples, but individual
# sample columns indicate otherwise, or visa vers.  First, standardize
# individual sample columns to make code cleaner.
#
# ARGUMENTS:
# visits    dataframe containing contents tblVISITS2.
#
{
  tt <- transform(visits
                 ,CHEM2 = CHEM=='Y' & !is.na(CHEM)
                 ,WCHL2 = WCHL=='Y' & !is.na(WCHL)
                 ,PPCP2 = PPCP=='Y' & !is.na(PPCP)
                 ,PERI2 = PERI=='Y' & !is.na(PERI)
                 ,PCHL2 = PCHL=='Y' & !is.na(PCHL)
                 ,PBIO2 = PBIO=='Y' & !is.na(PBIO)
                 ,PAPA2 = PAPA=='Y' & !is.na(PAPA)
                 ,ENTE2 = ENTE=='Y' & !is.na(ENTE)
                 ,SEDE2 = SEDE=='Y' & !is.na(SEDE)
                 ,FTIS2 = FTIS=='Y' & !is.na(FTIS)
                 ,BERW2 = BERW=='Y' & !is.na(BERW)
                 ,VERT2 = VERT=='Y' & !is.na(VERT)
                 ,BELG2 = BELG=='Y' & !is.na(BELG)
                 ,PHYT2 = PHYT=='Y' & !is.na(PHYT)
                 ,SNAG2 = SNAG=='Y' & !is.na(SNAG)
                 ,KICK2 = KICK=='Y' & !is.na(KICK)
                 ,WPFC2 = WPFC=='Y' & !is.na(WPFC)
                 )
  tt <- subset(tt
              ,((SAMP_STATUS=='No Samps') +
                (CHEM2 | WCHL2 | PPCP2 | PERI2 | PCHL2 | PBIO2 | PAPA2 | ENTE2 |
                 SEDE2 | FTIS2 | BERW2 | VERT2 | BELG2 | PHYT2 | SNAG2 | KICK2 |
                 WPFC2
                ) != 1
               )
              )
  tt$samplesTaken <- with(tt, paste(ifelse(CHEM2, 'CHEM', NA)
                                   ,ifelse(WCHL2, 'WCHL2', NA)
                                   ,ifelse(PPCP2, 'PPCP2', NA)
                                   ,ifelse(PERI2, 'PERI2', NA)
                                   ,ifelse(PCHL2, 'PCHL2', NA)
                                   ,ifelse(PBIO2, 'PBIO2', NA)
                                   ,ifelse(PAPA2, 'PAPA2', NA)
                                   ,ifelse(ENTE2, 'ENTE2', NA)
                                   ,ifelse(SEDE2, 'SEDE2', NA)
                                   ,ifelse(FTIS2, 'FTIS2', NA)
                                   ,ifelse(BERW2, 'BERW2', NA)
                                   ,ifelse(VERT2, 'VERT2', NA)
                                   ,ifelse(BELG2, 'BELG2', NA)
                                   ,ifelse(PHYT2, 'PHYT2', NA)
                                   ,ifelse(SNAG2, 'SNAG2', NA)
                                   ,ifelse(KICK2, 'KICK2', NA)
                                   ,ifelse(WPFC2, 'WPFC2', NA)
                                   ,sep=', '
                                   )
                         )
  tt$samplesTaken <- gsub("(NA|, NA)", "", gsub("NA, ", "", tt$samplesTaken))
  tt$samplesTaken <- ifelse(tt$samplesTaken=='', 'None', tt$samplesTaken)

  tt$samplesNotTaken <- with(tt, paste(ifelse(!CHEM2, 'CHEM', NA)
                                      ,ifelse(!WCHL2, 'WCHL2', NA)
                                      ,ifelse(!PPCP2, 'PPCP2', NA)
                                      ,ifelse(!PERI2, 'PERI2', NA)
                                      ,ifelse(!PCHL2, 'PCHL2', NA)
                                      ,ifelse(!PBIO2, 'PBIO2', NA)
                                      ,ifelse(!PAPA2, 'PAPA2', NA)
                                      ,ifelse(!ENTE2, 'ENTE2', NA)
                                      ,ifelse(!SEDE2, 'SEDE2', NA)
                                      ,ifelse(!FTIS2, 'FTIS2', NA)
                                      ,ifelse(!BERW2, 'BERW2', NA)
                                      ,ifelse(!VERT2, 'VERT2', NA)
                                      ,ifelse(!BELG2, 'BELG2', NA)
                                      ,ifelse(!PHYT2, 'PHYT2', NA)
                                      ,ifelse(!SNAG2, 'SNAG2', NA)
                                      ,ifelse(!KICK2, 'KICK2', NA)
                                      ,ifelse(!WPFC2, 'WPFC2', NA)
                                      ,sep=', '
                                      )
                            )
  tt$samplesNotTaken <- gsub("(NA|, NA)", "", gsub("NA, ", "", tt$samplesNotTaken))
  tt$samplesNotTaken <- ifelse(tt$samplesNotTaken=='', 'None', tt$samplesNotTaken)

  tt$sampleSummary <- ifelse(tt$SAMP_STATUS == 'No Samps' & !is.na(tt$SAMP_STATUS)
                            ,paste(tt$samplesTaken, "marked as taken")
                            ,paste(tt$samplesTaken, "unmarked as taken")
                            )
  pp <- sprintf("At UID=%s we have inconsistent sample information; SAMP_STATUS=<%s>, %s"
               ,tt$UID, tt$SAMP_STATUS, tt$sampleSummary
               )

  if(!is.null(pp)) pp <- as.matrix(pp, ncol=1)
  return(pp)
}

# end of file