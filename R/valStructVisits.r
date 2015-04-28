# valStructVisits.r
#
# 10/02/09 cws Created
# 10/27/09 mrc removed errant comma in list of args.
#  2/25/10 cws moved source() calls to NRSAvalidation.r
# 11/22/10 cws added additional structure tests, and created unit test.

require(RODBC)

valStructVisits <- function(df, test='all')
# Performs structure checks on the NRSA table tblIVISITS2.  Returns
# NULL if no errors have been found, or an Nx1 character matrix describing the
# detected errors.
#
# ARGUMENTS:
# df        dataframe of NRSA site visit data
# test      String describing which tests to perform.  May be one of the
#             following:
#             'all'       default, performs all tests
#             'vital'     performs only vital tests
#             'nonvital'  performs only nonvital tests
#             'synopsis'  performs all tests, returning counts of detected
#                           errors for all tests
#
# ASSUMPTIONS:
#
{
  intermediateMessage('Structure validation of Visit/Verification form data '
                     ,loc='start'
                     )

  probs <- NULL

  # sanity checks
  if(!(test %in% c('all', 'vital', 'nonvital', 'synopsis'))) {
      probs<-"Error: test argument must be 'all', 'vital', 'nonvital' or 'synopsis'"
      return(as.matrix(probs))
  }

  # Check for correct column names no matter what, and stop early if there
  # are any errors, since nothing else will work without them
  intermediateMessage('.1')
  probs <- stValColumnPresence(df, c('UID','SITE_ID','DATE_COL','VISIT_NO'
                                    ,'SAMPLE_TYPE','STATE','TEAM_ID','XLAT_DEG'
                                    ,'XLAT_MIN','XLAT_SEC','XLON_DEG','XLON_MIN'
                                    ,'XLON_SEC','XLAT_DD','XLON_DD','VALXSITE'
                                    ,'XSTATUS','SITESAMP','GENCOM'
                                    ,'NOT_SAMPLED_TEMP','SAMPLED','NOT_SAMPLED'
                                    ,'NO_ACCESS','SAMP_STATUS','CHEM','WCHL'
                                    ,'PPCP','PERI','PCHL','PBIO','PAPA','ENTE'
                                    ,'SEDE','FTIS','BERW','VERT','BELG','PHYT'
                                    ,'SNAG','KICK','WPFC','STATUS_COM'
                                    )
                              )
  if(!is.null(probs)) {
      colnames(probs)[1] <-'User.MUST.fix.these.errors.before.proceding'
      intermediateMessage('.  Done (terminated early).', loc='end')
      return(probs)
  }

  #  Perform 'vital' tests if requested
  if(test %in% c('all','vital','synopsis')) {

      # Check for missing UID values
      intermediateMessage('.2')
      pp <- stValMissingValues(df, 'UID')
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing UID values"
                               ,"Missing UID values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      
      # SITE_ID, DATE_COL, VISIT_NO combination on each row is unique in the table
      intermediateMessage('.3')
      df$YEAR <- format(df$DATE_COL, '%Y')
      pp <- stValCountRows(df, c('SITE_ID','YEAR','VISIT_NO'), 1)
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Missing UID values"
                               ,"Missing UID values exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }
      
      # Check that a visit 2 to a site exists if and only if a visit 1 exists
      # earlier within the same calendar year. Using a character based standin
      # for VISIT_NO prevents selecting undefined columns.
      pp <- NULL
      if(any(df$VISIT_NO > 1)) {
          intermediateMessage('.4')
          df$vv<-paste('v',df$VISIT_NO,sep='')
          tt <- reshape(df[c('SITE_ID','DATE_COL','vv')], direction='wide'
                       ,v.names='DATE_COL', idvar='SITE_ID', timevar='vv'
                       )
          errs <- subset(tt, (is.na(DATE_COL.v1) & !is.na(DATE_COL.v2)) |
                             (DATE_COL.v1 > DATE_COL.v2)
                        )
          pp <- with(subset(df, SITE_ID %in% errs$SITE_ID)
                    ,sprintf("Incorrect relationship among site variables SITE_ID=%s DATE_COL=%s VISIT_NO=%s"
                            ,SITE_ID, DATE_COL, VISIT_NO
                            )
                    )
      }
      if(length(pp)==0) {
          pp <- NULL
      } else {
          pp <- as.matrix(pp, ncol=1)
      }
      if(test=='synopsis') {
          probs <- rbind(probs
                        ,ifelse(is.null(pp)
                               ,"No Incorrect relationship between VISIT_NO and DATE_COL"
                               ,"Incorrect relationship between VISIT_NO and DATE_COL exist (vital)"
                               )
                        )
      } else {
          probs <- rbind(probs, pp)
      }

  }

  intermediateMessage('.  Done', loc='end')
  return(probs)
}


valStructVisitsTest <- function()
# Tests valStructVisits()
{
  # Create test data
  baseTest <- data.frame(UID=9000+1:20
                        ,SITE_ID=sprintf("Place %02d", c(1:15,11:15))
                        ,DATE_COL=as.POSIXct(sprintf("2010-11-%d", 1:20))
                        ,VISIT_NO=c(rep(1,15),rep(2,5))
                        ,SAMPLE_TYPE='VERIF'
                        ,STATE=rep(c('Aphasia','Confusion','Euphoria','Sleep'), 5)
                        ,TEAM_ID=rep(c('Good','Bad','Ugly','Indifferent'), 5)
                        ,XLAT_DEG=40 + 1:20
                        ,XLAT_MIN=30 + 1:20
                        ,XLAT_SEC=20 + 21:40
                        ,XLON_DEG=40 + 1:20
                        ,XLON_MIN=30 + 1:20
                        ,XLON_SEC=20 + 21:40
                        ,XLAT_DD= rep(45.678901, 20)
                        ,XLON_DD= rep(45.678901, 20)
                        ,VALXSITE=rep(c('WADEABLE','BOATABLE'), 10)
                        ,XSTATUS=rep('SAMPLEABLE', 20)
                        ,SITESAMP=rep('Y', 20)
                        ,GENCOM=rep('General comments', 20)
                        ,NOT_SAMPLED_TEMP= rep(NA, 20)
                        ,SAMPLED=rep(c('WADEABLE','BOATABLE'), 10)
                        ,NOT_SAMPLED= rep(NA, 20)
                        ,NO_ACCESS= rep(NA, 20)
                        ,SAMP_STATUS= rep('Another comment field', 20)
                        ,CHEM= rep('Y', 20)
                        ,WCHL= rep('Y', 20)
                        ,PPCP= rep('Y', 20)
                        ,PERI= rep('Y', 20)
                        ,PCHL= rep('Y', 20)
                        ,PBIO= rep('Y', 20)
                        ,PAPA= rep('Y', 20)
                        ,ENTE= rep('Y', 20)
                        ,SEDE= rep('Y', 20)
                        ,FTIS= rep('Y', 20)
                        ,BERW= rep('Y', 20)
                        ,VERT= rep('Y', 20)
                        ,BELG= rep('Y', 20)
                        ,PHYT= rep('Y', 20)
                        ,SNAG= rep('Y', 20)
                        ,KICK= rep('Y', 20)
                        ,WPFC= rep('Y', 20)
                        ,STATUS_COM=rep('Big comment field', 20)
                        ,stringsAsFactors=FALSE
                        )

  # Test perfect data with initial and second visits
  rr <- valStructVisits(baseTest)
  checkEquals(NULL, rr, "Error: valStructVisits detects errors in perfect data")
  
  # Test perfect data without second visits
  rr <- valStructVisits(subset(baseTest, VISIT_NO==1))
  checkEquals(NULL, rr, "Error: valStructVisits detects errors in perfect data with no second visits")

  # Create and test with imperfect data.  DATE_COL gets special treatment because
  # it otherwise gets coerced to 'num'.
  realTest <- baseTest
  realTest$DATE_COL <- as.character(realTest$DATE_COL)
  realTest <- transform(realTest
                        # Make a UID missing
                       ,UID=ifelse(UID==9001, NA, UID)

                        # Make 2 duplicate SITE_ID-DATE_COL-VISIT_NO combos
                        # for 'unexpected row count' msgs for Place 03 and
                        # Place 11.
                       ,SITE_ID=ifelse(UID==9002, 'Place 03', SITE_ID)
                       ,VISIT_NO=ifelse(UID==9016, 1, VISIT_NO)
                       )
  realTest <- transform(realTest
                        # Make a VISIT_NO 2 exist without a VISIT_NO 1 such that
                        # there are duplicate VISIT_ID 2s, for TWO 'Incorrect
                        # relationship among site variables' msgs (one for each
                        # visit to Place 12) as well as another 'unexpected row
                        # count' msg.
                       ,VISIT_NO=ifelse(UID==9012 & !is.na(UID), 2, VISIT_NO)
                       )

  realTest <- transform(realTest
                        # Make a VISIT_NO 2 exist without a VISIT_NO 1 such that
                        # there is only one visit to this site. for another
                        #  'Incorrect relationship among site variables' msg.
                       ,VISIT_NO=ifelse(UID==9005 & !is.na(UID), 2, VISIT_NO)

                        # Make a VISIT_NO 2 occur before VISIT_ID 1, which will
                        # create 2 'Incorrect relationship among site variables'
                        # msgs, one for each visit to Place 13.
                       ,DATE_COL=ifelse(UID==9018 & !is.na(UID), '2010-01-20', as.character(DATE_COL))
                       )
  realTest$DATE_COL <- as.POSIXct(realTest$DATE_COL)
  
  rr <- valStructVisits(realTest)
  
  ee <- rbind("Column UID has 1 missing values"
             ,"Unexpected row count at (SITE_ID,YEAR,VISIT_NO)=(Place 03,2010,1), n=2"
             ,"Unexpected row count at (SITE_ID,YEAR,VISIT_NO)=(Place 11,2010,1), n=2"
             ,"Unexpected row count at (SITE_ID,YEAR,VISIT_NO)=(Place 12,2010,2), n=2"
             ,"Incorrect relationship among site variables SITE_ID=Place 05 DATE_COL=2010-11-05 VISIT_NO=2"
             ,"Incorrect relationship among site variables SITE_ID=Place 12 DATE_COL=2010-11-12 VISIT_NO=2"
             ,"Incorrect relationship among site variables SITE_ID=Place 13 DATE_COL=2010-11-13 VISIT_NO=1"
             ,"Incorrect relationship among site variables SITE_ID=Place 12 DATE_COL=2010-11-17 VISIT_NO=2"
             ,"Incorrect relationship among site variables SITE_ID=Place 13 DATE_COL=2010-01-20 VISIT_NO=2"
             )
  checkEquals(ee, rr
             ,"Error: valStructVisits does not correctly identify problems."
             )

}

# end of file