nrsaChannelHabitat <- function(bChannelUnit = NULL
                              ,bChannelUnitCodeList = data.frame(code=c('FA','RA','RI','GL','PO','CA','DR')
                                                                ,metsSuffix=c('fa','ra','ri','gl','pool','ca','dr')
                                                                ,category=c('FALLS','RAPID','RIFFLE','GLIDE','POOL','CASCADE','DRY')
                                                                ,isFast=c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE)
                                                                #,isPool=c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)
                                                                ,stringsAsFactors=FALSE
                                                                )
                              ,wChannelUnit = NULL
                              # NOTE: wChannelUnitCodeList in 1314 is identical to bChannelUnitCodeList
                              ,wChannelUnitCodeList = data.frame(code=c('FA','CA','RA','RI','GL'
                                                                       ,'PB','PP','PD','PL','PT'
                                                                       ,'P','DR'#,'SB'
                                                                       )
                                                                ,metsSuffix=c('fa','ca','ra','ri','gl'
                                                                             ,'pb','pp','pd','pl','pt'
                                                                             ,'p','dr'#,'sb'
                                                                             )
                                                                ,category=c('FALLS','CASCADE','RAPID','RIFFLE','GLIDE'
                                                                           ,'POOL_B','POOL_PLUNGE','POOL_D','POOL_LATERAL','POOL_TRENCH'
                                                                           ,'POOL','DRY'#,'SUBMERGED'
                                                                           )
                                                                ,isFast=c(TRUE,TRUE,TRUE,TRUE,FALSE
                                                                         ,FALSE,FALSE,FALSE,FALSE,FALSE
                                                                         ,FALSE,FALSE#,FALSE
                                                                         )
                                                                ,isPool=c(FALSE,FALSE,FALSE,FALSE,FALSE
                                                                         ,TRUE,TRUE,TRUE,TRUE,TRUE
                                                                         ,TRUE,FALSE#,FALSE
                                                                         )
                                                                ,stringsAsFactors=FALSE
                                                                )
                              ) {

################################################################################
# Function: metsChannelHabitat
# Title: Calculate NRSA Channel Habitat Metrics
# Programmers: Randy Hjort
#              Curt Seeliger
#              Suzanne San Romani
#              Tom Kincaid
# Date: January 4, 2010
# Description:
#   This function calculates the channel habitat portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires a data frame containing the thalweg data file.
# Function Revisions:
#   01/04/10 rch: Copied, plagerized and made up this code.
#   02/18/10 cws: Removed source() of NRSAValidation.r, NA_filler.r and
#            summaryby.r.
#   03/23/10 ssr: Moved creation of unit test dataframes to separate functions.
#   06/03/10 cws: Removed pct_sb from calculations.  Is not legal channel unit
#            code, and does not occur in the 2008-2009 field data, and it
#            collides with the substrate metric pct_sb.
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#            instead.
#   11/02/10 cws: Modified to handle single-protocol datasets without choking.
#            Also re-prettyprinting.  Modified unit test to test single-protocol
#            calculations.
#   08/03/12 tmk: Removed calls to the require() function.  Removed use of ODBC
#            data connection and replaced with data input from csv files using a
#            call to function read.csv.  Added argument tbl to the function to
#            identify name of the data file.  Added argument NRSAdir to the
#            function to identify the directory from which the data file is read
#            and to which the output metrics file is written.
#   12/20/12 tmk: Modified data input to use a data frame containing the data
#            file rather than a csv file.  Modified output to be a data frame
#            rather than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frame to
#            character variables.
#   11/18/15 cws Updated calling interface.  Changed UID and RESULT to SITE and 
#            VALUE.
#
# Arguments:
#   thalweg = a data frame containing the thalweg data file.  The data frame
#     must include columns that are named as follows:
#       UID - universal ID value
#       SAMPLE_TYPE - sample type
#       TRANSECT - transect label
#       STATION - station number along thalweg between transects
#       PARAMETER - identifier for each measurement, assessment, score, etc.
#       RESULT - measurement associated with PARAMETER column
#       UNITS - units of the RESULT measurement
#       FLAG - flag
#   Note that possible values for variables in the input data frame are
#   provided in the document named "NRSA Documentation.pdf" included in the help
#   directory for the package.
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The data frame contains the following columns:
#     UID - universal ID value
#     METRIC - metric name
#     RESULT - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
#   metsChannelHabitat.1 - calculate metrics
################################################################################

# # Print an initial message
#   cat('Channel Habitat calculations:\n')
# 
# # Convert factors to character variables in the thalweg data frame
#   intermediateMessage('.1 Convert factors to character variables.', loc='end')
#   thalweg <- convert_to_char(thalweg)
# 
# # Calculate the metrics
#   intermediateMessage('.2 Call function metsChannelHabitat.1.', loc='end')
#   mets <- metsChannelHabitat.1(thalweg)
#   row.names(mets) <- 1:nrow(mets)
# 
# # Print an exit message
#   intermediateMessage('Done.', loc='end')
# 
# # Return results
#   return(mets)
# }
# 
# 
# 
# metsChannelHabitat.1 <- function(indat) {
# 
# # Does all the real work for metsChannelHabitat.
# # Returns a dataframe of calculations if successful
# # or a character string describing the problem if
# # one was encountered.
# #
# # ARGUMENTS:
# # indat		dataframe of channel data.
# # protocols	dataframe relating UID to the
# #			  sampling protocol used at the site.
# #

    intermediateMessage('Channel Habitat mets', loc='start')

    absentAsNULL <- function(df, ifdf, ...) {
        if(is.null(df)) return(NULL)
        else if(!is.data.frame(df)) return(NULL)
        else if(nrow(df) == 0) return (NULL)
        else if(is.function(ifdf)) return(ifdf(df, ...))
        else return(df)
    }
    ifdf <- function(df, ...) {
        if(is.null(...)) return(NULL)
        else if(all(is.na(...))) return(NULL)
        else return(df %>% select(SITE,VALUE) %>% subset(VALUE %in% ...))
    }

    bChannelUnit = absentAsNULL(bChannelUnit, ifdf, bChannelUnitCodeList$code)
    wChannelUnit = absentAsNULL(wChannelUnit, ifdf, wChannelUnitCodeList$code)

    intermediateMessage('.1')

    individualPercents <- function(cdData, codeList) {
        theseMets <- NULL
        for (i in 1:nrow(codeList)) {
            tgl <- summaryby(cdData,'count',sprintf("pct_%s", codeList$metsSuffix[i]))
            tyout<-aggregate(list(typesum=cdData$VALUE),list(SITE=cdData$SITE),function(x){sum(x==codeList$code[i],na.rm=TRUE)})
            tgl<-merge(tgl,tyout,by='SITE',all.x=TRUE)
            tgl$VALUE <- (tgl$typesum/tgl$VALUE)*100
            tgl<-tgl[c('SITE','METRIC','VALUE')]
            
            theseMets <- rbind(theseMets, tgl)
            intermediateMessage(sprintf('.%s', codeList$code[i]))
        }
        return(theseMets)
    }
    
    if(is.null(bChannelUnit)) {
        bmets <- NULL
    } else  {
        # go through channel unit data for individual categories
        bmets <- individualPercents(bChannelUnit, bChannelUnitCodeList)
        intermediateMessage('.2')
        
        # Calculate summary sums
        if(is.null(bChannelUnitCodeList$isFast)) {
            pct_fastBoatable <- NULL
            pct_SlowBoatable <- NULL
        } else {
            pct_fastBoatable <- bChannelUnit %>% 
                                mutate(isfast=ifelse(VALUE %in% subset(bChannelUnitCodeList, isFast)$code, 1, 0)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(isfast, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_fast')
            pct_SlowBoatable <- bChannelUnit %>% 
                                mutate(isslow=ifelse(VALUE %in% subset(bChannelUnitCodeList, isFast)$code, 0, 1)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(isslow, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_slow')
            intermediateMessage('.3')
        }
            
        if(is.null(bChannelUnitCodeList$isPool)) {
            pct_poolBoatable <- NULL
        } else {
            pct_poolBoatable <- bChannelUnit %>% 
                                mutate(ispool=ifelse(VALUE %in% subset(bChannelUnitCodeList, isPool)$code, 1, 0)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(ispool, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_pool')
            intermediateMessage('.4')
        }
        
        bmets <- rbind(bmets,pct_fastBoatable,pct_SlowBoatable,pct_poolBoatable)
        intermediateMessage('.5')
    }

    if(is.null(wChannelUnit)) {
        wmets <- NULL
    } else  {
        # go through channel unit data for individual categories
        wmets <- individualPercents(wChannelUnit, wChannelUnitCodeList)
        intermediateMessage('.6')

        # Calculate summary sums
        if(is.null(wChannelUnitCodeList$isFast)) {
            pct_fastWadeable <- NULL
            pct_slowWadeable <- NULL
        } else {
            pct_fastWadeable <- wChannelUnit %>% 
                                mutate(isfast=ifelse(VALUE %in% subset(wChannelUnitCodeList, isFast)$code, 1, 0)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(isfast, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_fast')
            pct_slowWadeable <- wChannelUnit %>% 
                                mutate(isslow=ifelse(VALUE %in% subset(wChannelUnitCodeList, isFast)$code, 0, 1)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(isslow, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_slow')
            intermediateMessage('.7')
        }
                
        if(is.null(wChannelUnitCodeList$isPool)) {
            pct_poolWadeable <- NULL
        } else {
            pct_poolWadeable <- wChannelUnit %>% 
                                mutate(ispool=ifelse(VALUE %in% subset(wChannelUnitCodeList, isPool)$code, 1, 0)) %>%
                                group_by(SITE) %>%
                                dplyr::summarise(VALUE=100*protectedMean(ispool, na.rm=TRUE)) %>%
                                mutate(METRIC='pct_pool')
            intermediateMessage('.8')
        }
        
        wmets <- rbind(wmets,pct_slowWadeable,pct_fastWadeable,pct_poolWadeable)
        intermediateMessage('.9')
    }

    mets <- rbind(bmets, wmets)

    intermediateMessage('.Done', loc='end')
    return(mets)
}


##############################################################################################################################
oldChannelHabitatCode <- function(thalweg) {
  indat <- thalweg
  cdData <- subset(indat
                  ,PARAMETER == 'CHANUNCD' &
                   ((VALUE %in% c('FA','CA','RA','RI','GL','PB','PP','PD','PL'
                                  ,'PT','P','DR','SB'
                                  )
                     &
                     SAMPLE_TYPE == 'PHAB_THALW'
                    )
                    |
                    (VALUE %in% c('FA','RA','RI','GL','PO','CA','DR')
                     &
                     SAMPLE_TYPE == 'PHAB_THAL'
                    )
                   ) &
                   TRANSECT %in% LETTERS[1:11]
                  )

  intermediateMessage('.1')

  # Metrics common to both wadeable and boatable protocols
  tgl <- summaryby(cdData,'count',"pct_gl")
  tyout<-aggregate( list(typesum=cdData$VALUE),list(SITE=cdData$SITE),function(x){sum(x=='GL',na.rm=TRUE)})
  tgl<-merge(tgl,tyout,by='SITE',all.x=TRUE)
  #tgl$typesum<- ifelse( is.na(tgl$yessum),0,tgl$yessum)
  tgl$VALUE <- (tgl$typesum/tgl$VALUE)*100
  tgl<-tgl[c('SITE','METRIC','VALUE')]

  tri <- summaryby(cdData,'count',"pct_ri")
  tyout<-aggregate( list(typesum=cdData$VALUE),list(SITE=cdData$SITE),function(x){sum(x=='RI',na.rm=TRUE)})
  tri<-merge(tri,tyout,by='SITE',all.x=TRUE)
  #tri$typesum<- ifelse( is.na(tri$yessum),0,tri$yessum)
  tri$VALUE <- (tri$typesum/tri$VALUE)*100
  tri<-tri[c('SITE','METRIC','VALUE')]

  tra <- summaryby(cdData,'count',"pct_ra")
  tyout<-aggregate( list(typesum=cdData$VALUE),list(SITE=cdData$SITE),function(x){sum(x=='RA',na.rm=TRUE)})
  tra<-merge(tra,tyout,by='SITE',all.x=TRUE)
  #tra$typesum<- ifelse( is.na(tra$yessum),0,tra$yessum)
  tra$VALUE <- (tra$typesum/tra$VALUE)*100
  tra<-tra[c('SITE','METRIC','VALUE')]

  tca <- summaryby(cdData,'count',"pct_ca")
  tyout<-aggregate( list(typesum=cdData$VALUE),list(SITE=cdData$SITE),function(x){sum(x=='CA',na.rm=TRUE)})
  tca<-merge(tca,tyout,by='SITE',all.x=TRUE)
  #tca$typesum<- ifelse( is.na(tca$yessum),0,tca$yessum)
  tca$VALUE <- (tca$typesum/tca$VALUE)*100
  tca<-tca[c('SITE','METRIC','VALUE')]

  tfa <- summaryby(cdData,'count',"pct_fa")
  tyout<-aggregate( list(typesum=cdData$VALUE),list(SITE=cdData$SITE),function(x){sum(x=='FA',na.rm=TRUE)})
  tfa<-merge(tfa,tyout,by='SITE',all.x=TRUE)
  #tfa$typesum<- ifelse( is.na(tfa$yessum),0,tfa$yessum)
  tfa$VALUE <- (tfa$typesum/tfa$VALUE)*100
  tfa<-tfa[c('SITE','METRIC','VALUE')]

  tdr <- summaryby(cdData,'count',"pct_dr")
  tyout<-aggregate( list(typesum=cdData$VALUE),list(SITE=cdData$SITE),function(x){sum(x=='DR',na.rm=TRUE)})
  tdr<-merge(tdr,tyout,by='SITE',all.x=TRUE)
  #tdr$typesum<- ifelse( is.na(tdr$yessum),0,tdr$yessum)
  tdr$VALUE <- (tdr$typesum/tdr$VALUE)*100
  tdr<-tdr[c('SITE','METRIC','VALUE')]

  intermediateMessage('.2')
  
  # Boatable-only protocol metric
  btdata  <- subset(cdData,SAMPLE_TYPE == 'PHAB_THAL')
  if(nrow(btdata)>0) {
      tpo <- summaryby(btdata,'count',"pct_po")
      tyout<-aggregate( list(typesum=btdata$VALUE),list(SITE=btdata$SITE),function(x){sum(x=='PO',na.rm=TRUE)})
      tpo<-merge(tpo,tyout,by='SITE',all.x=TRUE)
      #tpo$typesum<- ifelse( is.na(tpo$yessum),0,tpo$yessum)
      tpo$VALUE <- (tpo$typesum/tpo$VALUE)*100
      tpo<-tpo[c('SITE','METRIC','VALUE')]
  }
  intermediateMessage('.3')

  #subset data for wadeable  only VALUE values

  intermediateMessage('.4')
  wddata  <- subset(cdData,SAMPLE_TYPE == 'PHAB_THALW')
  if(nrow(wddata)>0) {
      tpp <- summaryby(wddata,'count',"pct_pp")
      tyout<-aggregate( list(typesum=wddata$VALUE),list(SITE=wddata$SITE),function(x){sum(x=='PP',na.rm=TRUE)})
      tpp<-merge(tpp,tyout,by='SITE',all.x=TRUE)
      #tpp$typesum<- ifelse( is.na(tpp$yessum),0,tpp$yessum)
      tpp$VALUE <- (tpp$typesum/tpp$VALUE)*100
      tpp<-tpp[c('SITE','METRIC','VALUE')]

      tpd <- summaryby(wddata,'count',"pct_pd")
      tyout<-aggregate( list(typesum=wddata$VALUE),list(SITE=wddata$SITE),function(x){sum(x=='PD',na.rm=TRUE)})
      tpd<-merge(tpd,tyout,by='SITE',all.x=TRUE)
      #tpd$typesum<- ifelse( is.na(tpd$yessum),0,tpd$yessum)
      tpd$VALUE <- (tpd$typesum/tpd$VALUE)*100
      tpd<-tpd[c('SITE','METRIC','VALUE')]

      tpb <- summaryby(wddata,'count',"pct_pb")
      tyout<-aggregate( list(typesum=wddata$VALUE),list(SITE=wddata$SITE),function(x){sum(x=='PB',na.rm=TRUE)})
      tpb<-merge(tpb,tyout,by='SITE',all.x=TRUE)
      #tpb$typesum<- ifelse( is.na(tpb$yessum),0,tpb$yessum)
      tpb$VALUE <- (tpb$typesum/tpb$VALUE)*100
      tpb<-tpb[c('SITE','METRIC','VALUE')]


      tpl <- summaryby(wddata,'count',"pct_pl")
      tyout<-aggregate( list(typesum=wddata$VALUE),list(SITE=wddata$SITE),function(x){sum(x=='PL',na.rm=TRUE)})
      tpl<-merge(tpl,tyout,by='SITE',all.x=TRUE)
      #tpl$typesum<- ifelse( is.na(tpl$yessum),0,tpl$yessum)
      tpl$VALUE <- (tpl$typesum/tpl$VALUE)*100
      tpl<-tpl[c('SITE','METRIC','VALUE')]


      tpt <- summaryby(wddata,'count',"pct_pt")
      tyout<-aggregate( list(typesum=wddata$VALUE),list(SITE=wddata$SITE),function(x){sum(x=='PT',na.rm=TRUE)})
      tpt<-merge(tpt,tyout,by='SITE',all.x=TRUE)
      #tpt$typesum<- ifelse( is.na(tpt$yessum),0,tpt$yessum)
      tpt$VALUE <- (tpt$typesum/tpt$VALUE)*100
      tpt<-tpt[c('SITE','METRIC','VALUE')]

      tp <- summaryby(wddata,'count',"pct_p")
      tyout<-aggregate( list(typesum=wddata$VALUE),list(SITE=wddata$SITE),function(x){sum(x=='P',na.rm=TRUE)})
      tp<-merge(tp,tyout,by='SITE',all.x=TRUE)
      #tp$typesum<- ifelse( is.na(tp$yessum),0,tp$yessum)
      tp$VALUE <- (tp$typesum/tp$VALUE)*100
      tp<-tp[c('SITE','METRIC','VALUE')]

#               tsb <- summaryby(wddata,'count',"pct_sb")
#               tyout<-aggregate( list(typesum=wddata$RESULT),list(UID=wddata$UID),function(x){sum(x=='SB',na.rm=TRUE)})
#               tsb<-merge(tsb,tyout,by='UID',all.x=TRUE)
#               #tsb$typesum<- ifelse( is.na(tsb$yessum),0,tsb$yessum)
#               tsb$RESULT <- (tsb$typesum/tsb$RESULT)*100
#               tsb<-tsb[c('UID','METRIC','RESULT')]

  }
  intermediateMessage('.5')

  #compute summed metrics

  pfast<-rbind(tfa,tca,tra,tri)
  tfast<-summaryby(pfast,'sum','pct_fast')


  wSlow <- NULL
  wPool <- NULL
  wmets <- NULL
  if (nrow(wddata)>0) {
      wSlow <- rbind(tpp,tpd,tpb,tpl,tpt,tp)
      wPool <- rbind(tpp,tpd,tpb,tpl,tpt,tp)
      wmets <- rbind(tpp,tpd,tpb,tpl,tpt,tp)
  }
  bSlow <- NULL
  bPool <- NULL
  bmets <- NULL
  if (nrow(btdata)>0) {
      bSlow <- tpo
      bPool <- tpo
      bmets <- tpo
  }

#  pslow <- rbind(tpp,tpd,tpb,tpl,tpt,tp,tgl,tpo)
  pslow <- rbind(wSlow,bSlow,tgl)
  tslow <- summaryby(pslow,'sum','pct_slow')

#  ppool <- rbind(tpp,tpd,tpb,tpl,tpt,tp,tpo)
  ppool <- rbind(wPool,bPool)
  tpool <- summaryby(ppool,'sum','pct_pool')

  intermediateMessage('.6')
#  mets <- rbind(tdr,tp,tpt,tpl,tpb,tpd,tpp,tgl,tri,tra,tca,tfa,tfast,tslow,tpool)
  mets <- rbind(tgl,tri,tra,tca,tfa,tdr,wmets,tfast,tslow,tpool)

  intermediateMessage('.  Done.', loc='end')
   
  return(mets)
    
}



# end of file
