# valUpdateNRSAGPSdata.r
#
# Modifies *post-validation* GPS data prior to updating the database.
# This is used after exportNRSAdata.r, GIS calculations and importNRSAdata.r,
# and before valUpdateTable().  It reverses the modifications made to values
# of keys in this process; without this change the updated rows would not
# match up correctly.
#
# This program assumes NRSAvalidation.r has been sourced.
#
# 09/16/2010 cws Removing hardcoding of NRSA database name, using NRSAdbName
#            instead.
# 11/04/2010 cws removed hardcoding of location of csv file with GPS updates
#

require(RODBC)

valUpdateNRSAGPSdata <- function(updateTable=FALSE)
{
  # Read in the csv with GPS coordinates after it has undergone validation.
  # In exportNRSAdata.r, missing LINE values are eventually set to 999 so they
  # are ordered correctly for validation.  Reverse that here so they will
  # match the current data.
  valResults <- readNRSAValidationResults(paste(NRSAvalidationLocation,'valGISCalculations.csv',  sep=''))
  if(!is.data.frame(valResults)) {
      stop(valResults)
  }
  
  # Make sure there's no duplicate rows in the updates
  names(table(table(with(valResults, paste(UID, SAMPLE_TYPE, TRANSECT, LINE, TRANLINE, BANK, PARAMETER))))) == '1'

  if(any(valResults$LINE==999)) {
      valResults[valResults$LINE==999,]$LINE <- NA
  }
  # Check if any rows were added during validation.  These may have different
  # values of BANK and TRANLINE than are in the database table, which means the
  # table will not be updated accurately.  If any such rows are found, display
  # them, along with the current values of TRANLINE and BANK.
  addedRows <- subset(valResults, !grepl('Possibly incorrect.*', TESTDESCRIPTION))
  if(nrow(addedRows)>0) {
      chan <- odbcConnect(NRSAdbName)
      cg <- fetchNRSATable(chan, 'tblCHANNELGEOMETRY2')
      currentValues <- subset(cg
                             ,paste(UID,SAMPLE_TYPE,TRANSECT,PARAMETER,LINE) %in%
                              with(addedRows
                                  ,paste(UID,SAMPLE_TYPE,TRANSECT,PARAMETER,LINE)
                                  )
                             )
      tt <- merge(addedRows[c('UID','SAMPLE_TYPE','TRANSECT','PARAMETER','LINE','BANK','TRANLINE')]
                 ,currentValues[c('UID','SAMPLE_TYPE','TRANSECT','PARAMETER','LINE','BANK','TRANLINE')]
                 ,by=c('UID','SAMPLE_TYPE','TRANSECT','PARAMETER','LINE')
                 ,all.x=TRUE, suffixes=c('.is','.shouldBe')
                 )[c('UID','SAMPLE_TYPE','TRANSECT','PARAMETER','LINE'
                    ,'TRANLINE.is','TRANLINE.shouldBe','BANK.is','BANK.shouldBe'
                    )]
      checkThese<-tt[order(tt$UID,tt$SAMPLE_TYPE,tt$TRANSECT,tt$PARAMETER,tt$LINE),]
      
      # Check for added rows that do NOT match the values of BANK and TRANLINE
      # in the database table.  These are very likely incorrect.  Flag them with
      # a 'check'.  Likewise flag rows with missing keys.
      # Note: Set LINE NA values to 999 so aggregate will work, then set them back.
      tt$matches <- with(tt, BANK.is==BANK.shouldBe & TRANLINE.is==TRANLINE.shouldBe)
      tt$LINE <- ifelse(is.na(tt$LINE), 999, tt$LINE)
      rr <- aggregate(list(Flags=tt$matches)
                     ,list(UID=tt$UID
                          ,SAMPLE_TYPE=tt$SAMPLE_TYPE
                          ,TRANSECT=tt$TRANSECT
                          ,PARAMETER=tt$PARAMETER
                          ,LINE=tt$LINE
                          )
                     ,function(x) {
                          if (all(is.na(x))) {
                              return('check keys')
                          } else if (any(x, na.rm=TRUE)) {
                              return('')
                          } else {
                              return('check TRANLINE and BANK')
                          }
                      }
                     )
      rr$LINE <- ifelse(rr$LINE==999, NA, tt$LINE)
      checkThese <- subset(merge(checkThese, rr
                                ,by=c('UID','SAMPLE_TYPE','TRANSECT','PARAMETER','LINE')
                                )
                          ,Flags != ''
                          )

      print("NOTE: User should check these rows prior to updating table with ")
      print("      GPS values.  Key values in the validation results MUST match")
      print("      all keys in tblCHANNELGEOMETRY2, or it will not be correctly")
      print("      updated and likely result in structural errors.")
      print("      Use this listing to update the values of TRANLINE and BANK")
      print("      in the validation worksheet.  After making the necessary")
      print("      changes, resubmit the validation file with updateTable=TRUE.")
      print(checkThese)

  }

  # Drop rows that just need to be deleted.  Handle those separately
  valUpdates <- subset(valResults, COMMENTS != "Records removed as coordinates are the same as at the main transect.")
  valDeletes <- subset(valResults, COMMENTS == "Records removed as coordinates are the same as at the main transect.")

  if(updateTable) {
      # Write the data back to a temporary csv and update the database.
      rc <- writeNRSAValidationResults(valUpdates, paste(NRSACalcLocation,'valGISCalculationsTemp.csv',sep=''))
      if(!is.null(rc)) {
          print(rc)
          stop()
      }

      rc <- valUpdateTable(paste(NRSACalcLocation,'valGISCalculationsTemp.csv',sep=''), 'tblCHANNELGEOMETRY2')
      if(is.null(rc)) {
          print("Updating NRSA GPS data is done.")
      } else {
          print(rc)
      }
      
      # Delete rows as well
      if(nrow(valDeletes)>0) {
          print(sprintf("Remember to delete %i rows in valDeletes:", nrow(valDeletes)))
      }
      
  } else {
      print("Database was NOT updated.")
  }
  
}
# end of file