fishMet <- function(inCts, inTaxa=NULL, sampID="UID", keepHerps=FALSE,
   dist="IS_DISTINCT", ct="TOTAL") {

################################################################################
# Function: fishMet
# Title: Calculate NRSA Fish Metrics
# Programmers: Karen Blocksom
#              Tom Kincaid
# Date: May 8, 2013
# Description:
#   This function calculates the NRSA fish metrics.  It assumes input counts are
#   organized so that a single column represents the unique sample, with each
#   row representing the count for a single taxon in that sample.  However, the
#   function does sum by TAXA_ID, so it can account for cases where summing has
#   not already been done.
# Function Revisions:
#   05/08/13 kab: Created.
#   10/31/13 kab: Modified to use fish taxa list as default for inTaxa.
#   11/08/13 kab: Modified to add descriptions of traits created for metric
#            calculations.
#   03/04/14 kab: Modified to assign 0 metric values for sites with TOTLNIND=0,
#            added calculation of NAT_PIND
#   03/13/14 tmk: Removed calls to the require() and source() functions.
#            Removed the call to read.table() to input the inTaxa data frame
#            and replaced it with assignment of the fishTaxa data frame that is
#            included in the sysdata.rda file to inTaxa.  Modified to return a
#            character string when an error occurs.
#   05/19/14 kab: Added in code to make sure all metrics are calculated even if 
#            no individuals with trait are in samples. Changed input data frame
#            name from inAll to inCts
#   11/13/14 kab: Modified to determine whether variable ANOM_CT exists among
#            the input counts and, if not, set ANOMPIND equal to missing
# Arguments:
#   inCts = a data frame with a variable containing counts (with data already
#     summed by taxon and sample) and with a variable indicating distinctness of
#     each taxon in a sample (coded 0/1).  If anomalies are included in a
#     separate column in the inCts data frame, the name of the column is assumed
#     to be "ANOM_CT".
#   inTaxa = a data frame containing the taxalist in wide format with
#     variables for taxonomy as well as any other traits of interest.  If this
#     argument is NULL, then the data frame is  assigned from the fishTaxa data
#     frame that is included in the sysdata.rda file.  Assumes the following
#     traits are included: FAM_OR_CLS, GENUS, HABITAT, HERP, MIGRATORY, REPROD,
#     TEMP, TOL_VAL, TOLERANCE, TROPHIC, VEL, FINAL_NAME.  If traits have _GLEC
#     or _NRSA at the end of the name, the name is truncated to the above trait
#     names within the function.  The default is NULL.
#   sampID = a character vector indicating names of variable(s) necessary to
#     identify a unique sample.  The default is "UID".
#   keepHerps = a logical value that indicates whether or not to include
#     amphibians and reptiles in counts, which requires parameter "HERP" in the
#     taxa list with values only for reptiles or amphibians and otherwise
#     missing or blank.  The default is FALSE.
#   dist = name of the variable in the inCts data frame indicating distinctness.
#     The default is "IS_DISTINCT".
#   ct = name of the variable in the inCts data frame providing number of
#     organisms counted for a taxon in a sample.  The default is "TOTAL".
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The first columns of the data frame contain the variables
#   specified in the sampID argument.  Subsequent columns are named for each
#   metric and contain metric values.  A list of metrics is provided in the
#   document named "Fish_Metric_Descriptions.pdf" included in the help directory
#   for the package.
# Other Functions Required:
#   runFishMetrics - calculate metrics
################################################################################

	# If necessary, load the fishTaxa data frame and assign it to inTaxa.
	# fishTaxa <- read.table("fishTaxa.tab", sep="\t", header=TRUE,
 	#    stringsAsFactors=FALSE)
 	if(is.null(inTaxa)) {
		inTaxa <- fishTaxa
	}

	## We need specific combinations of certain traits for certain groups of fish, so first create combos of traits
	## that we need in the taxa list, then create a separate parameter list
	## for each dataset (native, non-native, all)
	## inTaxa includes the following trait names: TOLERANCE,VEL,TROPHIC,MIGR,HABITAT,REPROD,TEMP,FAM_OR_CLS,GENUS,FINAL_NAME
	## All names are assumed to be uppercase
	
	# Combine all values in sampID into one sampID in df
	for(i in 1:length(sampID)){
		if(i==1) inCts$SAMPID <- inCts[,sampID[i]]
		else inCts$SAMPID <- paste(inCts$SAMPID,inCts[,sampID[i]],sep='.')
	}
	# Rename counts and distinct variables to FINAL_CT and IS_DISTINCT
	names(inCts)[names(inCts)==ct] <- 'FINAL_CT'
	names(inCts)[names(inCts)==dist] <- 'IS_DISTINCT'
	
	# Make sure all necessary columns in inCts are numeric
	inCts <- mutate(inCts,FINAL_CT=as.numeric(FINAL_CT),IS_DISTINCT=as.integer(IS_DISTINCT),TAXA_ID=as.integer(TAXA_ID))
	if(length(grep("ANOM_CT", names(inCts))) > 0) {
    ANOM <- TRUE
    inCts$ANOM_CT <- as.integer(inCts$ANOM_CT) 
	}else{
    ANOM <- FALSE
	}
	
	# In case inTaxa has _NRSA or _GLEC on the end of trait names (as provided on NARS site)
	names(inTaxa) <- gsub('_GLEC|_NRSA','',names(inTaxa))
	# Because we use MIGR below for metric name, we need to change it here
	inTaxa <- plyr::rename(inTaxa,c('MIGR'='MIGRATORY'))
	# Assume that inTaxa could contain factors, so make sure all columns are character
	inTaxa[,names(inTaxa) %nin% c('TAXA_ID')] <- lapply(inTaxa[,names(inTaxa) %nin% c('TAXA_ID')],as.character)
	# Make sure all of the required traits are included in the taxalist (with the assumed names)
	necTraits <- c('FAM_OR_CLS','GENUS','HABITAT','HERP','MIGRATORY','REPROD','TEMP','TOL_VAL','TOLERANCE','TROPHIC','VEL','FINAL_NAME')
	if(any(necTraits %nin% names(inTaxa))){
		msgTraits <- which(necTraits %nin% names(inTaxa))
		return(paste("Some of the traits are missing from the taxa list. The following are \nrequired for metric calculations to run:\n", necTraits[msgTraits], "\n"))
	}
	
	# Set aside observations with TAXA_ID=99999, because this indicates that no fish were collected at that site.
	noFish <- subset(inCts,TAXA_ID==99999,select=c('SAMPID'))
	if(nrow(noFish)>0){
		noFish <- mutate(noFish,TOTLNIND=0,FLAG='Site sampled, no fish collected.')
	}
	
	## for inCts1, keep only observations without missing or zero FINAL_CT values or TAXA_ID and TAXA_ID!=99999
	inCts1 <- subset(inCts,TAXA_ID!=99999 & !is.na(TAXA_ID) & !is.na(FINAL_CT) & FINAL_CT!=0)
	
	## Now sum by TAXA_ID for ANOM_CT and for FINAL_CT for each sample
	# Two approaches depending on whether or not NON_NATIVE occurs in the counts data frame
	if('NON_NATIVE' %in% names(inCts1) & ANOM==TRUE){
		inCts2 <- ddply(inCts1,c('SAMPID','TAXA_ID','NON_NATIVE'),summarise,IS_DISTINCT=max(IS_DISTINCT),FINAL_CT=sum(FINAL_CT),ANOM_CT=sum(ANOM_CT))
    CALCNAT <- 'Y'
	}else if('NON_NATIVE' %in% names(inCts1) & ANOM==FALSE){
    inCts2 <- ddply(inCts1,c('SAMPID','TAXA_ID','NON_NATIVE'),summarise,IS_DISTINCT=max(IS_DISTINCT),FINAL_CT=sum(FINAL_CT))
    CALCNAT <- 'Y'
	}else if('NON_NATIVE' %nin% names(inCts1) & ANOM==TRUE){
	  inCts2 <- ddply(inCts1,c('SAMPID','TAXA_ID'),summarise,IS_DISTINCT=max(IS_DISTINCT),FINAL_CT=sum(FINAL_CT),ANOM_CT=sum(ANOM_CT))
	  CALCNAT <- 'N'
	}else{
		inCts2 <- ddply(inCts1,c('SAMPID','TAXA_ID'),summarise,IS_DISTINCT=max(IS_DISTINCT),FINAL_CT=sum(FINAL_CT))
    CALCNAT <- 'N'
	}

	# Find all samples with a missing TAXA_ID, which means there are no counts for the site, and output the rows so the user can 
	## verify no sample was collected
	if(nrow(subset(inCts,is.na(TAXA_ID)))>0) {
		print("Make sure these missing TAXA_ID values are valid.")
		print(subset(inCts,is.na(TAXA_ID)))
	}
	
	## Apply keepHerps argument, dropping amphibians and reptiles if keepHerps=FALSE
	if(keepHerps==FALSE){
		inTaxa <- subset(inTaxa,is.na(HERP)|HERP=='')
		## Now use this revised list to get rid of extra taxa from counts before calculating TOTLNIND and TOTLNTAX in runFishMetrics()
		inCts2 <- merge(inCts2,subset(inTaxa,select=c('TAXA_ID')),by='TAXA_ID')
	}
	
	inTaxa$NTOL <- NA # Non-tolerance taxa, includes sensitive + intermediate
	inTaxa$NTOL[with(inTaxa,TOLERANCE %in% c('S','I'))] <- 1
	inTaxa$INTL <- NA # Intolerant taxa (sensitive)
	inTaxa$INTL[with(inTaxa,TOLERANCE=='S')] <- 1
	inTaxa$MTOL <- NA # Moderately tolerant taxa (intermediate)
	inTaxa$MTOL[with(inTaxa,TOLERANCE=='I')] <- 1
	inTaxa$TOLR <- NA # Tolerant taxa
	inTaxa$TOLR[with(inTaxa,TOLERANCE=='T')] <- 1
	inTaxa$INTLRHEO <- NA # Intolerant rheophilic taxa
	inTaxa$INTLRHEO[with(inTaxa,INTL==1 & VEL=='R')] <- 1
	inTaxa$INTLLOT <- NA # Intolerant lotic taxa
	inTaxa$INTLLOT[with(inTaxa,INTL==1 & VEL %in% c('R','O'))] <- 1
	inTaxa$NTOLBENT <- NA # Non-tolerant benthic taxa
	inTaxa$NTOLBENT[with(inTaxa,NTOL==1 & HABITAT=='B')] <- 1
	inTaxa$NTOLCARN <- NA # Non-tolerant carnivores
	inTaxa$NTOLCARN[with(inTaxa,NTOL==1 & TROPHIC=='C')] <- 1
	inTaxa$INTLCARN <- NA # Intolerant carnivores
	inTaxa$INTLCARN[with(inTaxa,INTL==1 & TROPHIC=='C')] <- 1
	inTaxa$INTLINV <- NA # Intolerant invertivores
	inTaxa$INTLINV[with(inTaxa,INTL==1 & TROPHIC=='I')] <- 1
	inTaxa$NTOLINV <- NA # Non-tolerant invertivores
	inTaxa$NTOLINV[with(inTaxa,NTOL==1 & TROPHIC=='I')] <- 1
	inTaxa$INTLMIGR <- NA # Intolerant migratory taxa
	inTaxa$INTLMIGR[with(inTaxa,INTL==1 & MIGRATORY=='Y')] <- 1
	inTaxa$MIGR <- NA # Migratory taxa
	inTaxa$MIGR[with(inTaxa,MIGRATORY=='Y')] <- 1
	inTaxa$LITH <- NA # Lithophilic taxa
	inTaxa$LITH[with(inTaxa,REPROD=='C')] <- 1 
	inTaxa$COLD <- NA # Cold water taxa
	inTaxa$COLD[with(inTaxa,TEMP=='CD')] <- 1
	inTaxa$SALM <- NA # Salmonid taxa
	inTaxa$SALM[with(inTaxa,toupper(FAM_OR_CLS)=='SALMONIDAE')] <- 1
	inTaxa$ICTA <- NA # Ictalurid taxa
	inTaxa$ICTA[with(inTaxa,toupper(FAM_OR_CLS)=='ICTALURIDAE')] <- 1
	inTaxa$BENTINV <- NA # Benthic Invertivores
	inTaxa$BENTINV[with(inTaxa,HABITAT=='B' & TROPHIC=='I')] <- 1
	inTaxa$CATO <- NA # Catostomid taxa
	inTaxa$CATO[with(inTaxa,toupper(FAM_OR_CLS)=='CATOSTOMIDAE')] <- 1
	inTaxa$RBCATO <- NA # Round-bodies catostomids (suckers)
	inTaxa$RBCATO[with(inTaxa,toupper(GENUS) %in% c('MOXOSTOMA', 'HYPENTELIUM', 'MINYTREMA', 'ERIMYZON' , 'CATOSTOMUS', 'CYCLEPTUS', 'PANTOSTEUS' , 'THOBURNIA'))] <- 1
	inTaxa$CENT <- NA # Centrarchidae taxa
	inTaxa$CENT[with(inTaxa,toupper(FAM_OR_CLS)=='CENTRARCHIDAE' & toupper(GENUS)!='MICROPTERUS')] <- 1
	inTaxa$CYPR <- NA # Cyprinidae taxa
	inTaxa$CYPR[with(inTaxa,toupper(FAM_OR_CLS)=='CYPRINIDAE' & FINAL_NAME %nin% c('COMMON CARP','GOLDFISH','BIGHEAD CARP','GRASS CARP','MIRROR CARP'))] <- 1
	inTaxa$LOT <- NA # Lotic taxa
	inTaxa$LOT[with(inTaxa,VEL %in% c('O','R'))] <- 1
	inTaxa$RHEO <- NA # Rheophilic taxa
	inTaxa$RHEO[with(inTaxa,VEL=='R')] <- 1
	inTaxa$INV <- NA # Invertivores
	inTaxa$INV[inTaxa$TROPHIC=='I'] <- 1
	inTaxa$CARN <- NA # Carnivores
	inTaxa$CARN[inTaxa$TROPHIC=='C'] <- 1
	inTaxa$OMNI <- NA # Omnivores
	inTaxa$OMNI[inTaxa$TROPHIC=='O'] <- 1
	inTaxa$HERB <- NA # Herbivores
	inTaxa$HERB[inTaxa$TROPHIC=='H'] <- 1
	
	##This just retains the variables that we are using in metrics, but it's not really necessary to get rid of them
	taxatraits <- inTaxa[,c("TAXA_ID","NTOL","MTOL","INTL","TOLR","INTLRHEO","INTLLOT","NTOLBENT","NTOLCARN","INTLINV","NTOLINV","INTLMIGR"
							,"MIGR","LITH","COLD","SALM","ICTA","BENTINV","CATO","RBCATO","CENT","CYPR","LOT","RHEO","TOL_VAL","INV","CARN"
							,"OMNI","HERB")]
	## Melt data frame into long format
	taxalong <- melt(taxatraits,id.vars='TAXA_ID',variable.name='PARAMETER',value.name='RESULT',na.rm=TRUE)
	
	##Set parameter list for each dataset: all fish, native fish, alien or non-native fish
	params <- c("NTOL","MTOL","INTL","TOLR","INTLRHEO","INTLLOT","NTOLBENT","NTOLCARN","INTLINV","NTOLINV"
			,"INTLMIGR","MIGR","LITH","COLD","SALM","ICTA","BENTINV","CATO","RBCATO","CENT","CYPR","LOT","RHEO","INV","CARN","OMNI","HERB")
	print("Ready to run metrics.")
	# Run metrics on full dataset
	allfishMetrics <- runFishMetrics(inCts2,params,"",taxalong)
	
	## If the variable NON_NATIVE is included and populated in inCts1, create inNative data frame
	#if(length(grep('NON_NATIVE',names(inCts2)))>0){
  if(CALCNAT=='Y'){
		if(any(unique(inCts2$NON_NATIVE) %nin% c('Y','N'))){
			print("No native and alien datasets were created because NON_NATIVE must only be 'Y' or 'N' values")
		}else{
			inNative <- subset(inCts2,NON_NATIVE=='N')
			inAlien <- subset(inCts2,NON_NATIVE=='Y')
			if(length(inNative)>0){
				print("About to run native metrics.")
				nativeMetrics <- runFishMetrics(inNative,params,"NAT_",taxalong)
				merge1 <- merge(allfishMetrics,nativeMetrics,by='SAMPID',all.x=TRUE)
				##Calculate proportions of native and alien taxa and individuals
				merge1 <- mutate(merge1,NAT_PTAX=round((NAT_TOTLNTAX/TOTLNTAX)*100,2),NAT_PIND=round((NAT_TOTLNIND/TOTLNIND)*100,2))
			}else{
				merge1 <- allfishMetrics
			}
			if(length(inAlien)>0){
					print('Calculating alien metrics')
					## We only want percentages of individuals and taxa that are alien, so sum FINAL_CT and IS_DISTINCT 
					alienMetrics <- ddply(inAlien,"SAMPID",summarise,ALIENNIND=sum(FINAL_CT),ALIENNTAX=sum(IS_DISTINCT))
					merge2 <- merge(merge1,alienMetrics,by="SAMPID",all.x=TRUE)
					## Now divide by the total number of individuals and taxa to get % alien individuals and taxa
					merge2 <- mutate(merge2,ALIENPIND=round((ALIENNIND/TOTLNIND)*100,2),ALIENPTAX=round((ALIENNTAX/TOTLNTAX)*100,2))
						msgAlien <- with(merge2,which(is.na(ALIENNIND)))
						merge2$ALIENNIND[msgAlien] <- 0
						merge2$ALIENPIND[msgAlien] <- 0
						merge2$ALIENNTAX[msgAlien] <- 0
						merge2$ALIENPTAX[msgAlien] <- 0
			} else{
					merge2 <- merge1
					merge2$ALIENPIND <- 0
					merge2$ALIENNIND <- 0
					merge2$ALIENPTAX <- 0
					merge2$ALIENNTAX <- 0
				}
				
		}
  }else{
    merge2 <- allfishMetrics
  }
	
	if(ANOM==TRUE){
		print('Calculating anomalies metrics')
		inAnom<-subset(inCts2,ANOM_CT!=0 & !is.na(ANOM_CT))
		if(nrow(inAnom)>0){
			inAnom <- ddply(inAnom,"SAMPID",summarise,ANOMNIND=sum(ANOM_CT))
			merge3 <- merge(merge2,inAnom,by="SAMPID",all.x=TRUE)
				merge3$ANOMNIND[which(is.na(merge3$ANOMNIND))] <- 0
			merge3 <- mutate(merge3,ANOMPIND=round((ANOMNIND/TOTLNIND)*100,2))
		}else{
			merge3 <- merge2
			merge3$ANOMPIND <- 0
			merge3$ANOMNIND <- 0
		}
	}else{
    merge3 <- merge2
    merge3$ANOMPIND <- NA
    merge3$ANOMNIND <- NA
	}

## Now create empty dataset with all metric names to make sure all are included in output even if no individuals with traits
if(CALCNAT=='Y'){
df.empty <- data.frame(t(rep(NA,230)),stringsAsFactors=FALSE) ## FIX NUMBER OF VARIABLES AND ADD CODE TO SEPARATE IF NATIVE TRAIT SUPPLIED OR NOT
names(df.empty) <- c('TOTLNIND','TOTLNTAX','BENTINVNIND','BENTINVNTAX','BENTINVPIND','BENTINVPTAX','CARNNIND','CARNNTAX','CARNPIND','CARNPTAX','CATONIND','CATONTAX'
                     ,'CATOPIND','CATOPTAX','CENTNIND','CENTNTAX','CENTPIND','CENTPTAX','COLDNIND','COLDNTAX','COLDPIND','COLDPTAX','CYPRNIND','CYPRNTAX','CYPRPIND'
                     ,'CYPRPTAX','HERBNIND','HERBNTAX','HERBPIND','HERBPTAX','ICTANIND','ICTANTAX','ICTAPIND','ICTAPTAX','INTLINVNIND','INTLINVNTAX','INTLINVPIND'
                     ,'INTLINVPTAX','INTLLOTNIND','INTLLOTNTAX','INTLLOTPIND','INTLLOTPTAX','INTLMIGRNIND','INTLMIGRNTAX','INTLMIGRPIND','INTLMIGRPTAX','INTLNIND'
                     ,'INTLNTAX','INTLPIND','INTLPTAX','INTLRHEONIND','INTLRHEONTAX','INTLRHEOPIND','INTLRHEOPTAX','INVNIND','INVNTAX','INVPIND','INVPTAX','LITHNIND'
                     ,'LITHNTAX','LITHPIND','LITHPTAX','LOTNIND','LOTNTAX','LOTPIND','LOTPTAX','MIGRNIND','MIGRNTAX','MIGRPIND','MIGRPTAX','MTOLNIND','MTOLNTAX'
                     ,'MTOLPIND','MTOLPTAX','NTOLBENTNIND','NTOLBENTNTAX','NTOLBENTPIND','NTOLBENTPTAX','NTOLCARNNIND','NTOLCARNNTAX','NTOLCARNPIND','NTOLCARNPTAX'
                     ,'NTOLINVNIND','NTOLINVNTAX','NTOLINVPIND','NTOLINVPTAX','NTOLNIND','NTOLNTAX','NTOLPIND','NTOLPTAX','OMNININD','OMNINTAX','OMNIPIND','OMNIPTAX'
                     ,'RBCATONIND','RBCATONTAX','RBCATOPIND','RBCATOPTAX','RHEONIND','RHEONTAX','RHEOPIND','RHEOPTAX','SALMNIND','SALMNTAX','SALMPIND','SALMPTAX'
                     ,'TOLRNIND','TOLRNTAX','TOLRPIND','TOLRPTAX','WTD_TV','NAT_TOTLNIND','NAT_TOTLNTAX','NAT_BENTINVNIND','NAT_BENTINVNTAX','NAT_BENTINVPIND'
                     ,'NAT_BENTINVPTAX','NAT_CARNNIND','NAT_CARNNTAX','NAT_CARNPIND','NAT_CARNPTAX','NAT_CATONIND','NAT_CATONTAX','NAT_CATOPIND','NAT_CATOPTAX'
                     ,'NAT_CENTNIND','NAT_CENTNTAX','NAT_CENTPIND','NAT_CENTPTAX','NAT_COLDNIND','NAT_COLDNTAX','NAT_COLDPIND','NAT_COLDPTAX','NAT_CYPRNIND'
                     ,'NAT_CYPRNTAX','NAT_CYPRPIND','NAT_CYPRPTAX','NAT_HERBNIND','NAT_HERBNTAX','NAT_HERBPIND','NAT_HERBPTAX','NAT_ICTANIND','NAT_ICTANTAX'
                     ,'NAT_ICTAPIND','NAT_ICTAPTAX','NAT_INTLINVNIND','NAT_INTLINVNTAX','NAT_INTLINVPIND','NAT_INTLINVPTAX','NAT_INTLLOTNIND','NAT_INTLLOTNTAX'
                     ,'NAT_INTLLOTPIND','NAT_INTLLOTPTAX','NAT_INTLMIGRNIND','NAT_INTLMIGRNTAX','NAT_INTLMIGRPIND','NAT_INTLMIGRPTAX','NAT_INTLNIND','NAT_INTLNTAX'
                     ,'NAT_INTLPIND','NAT_INTLPTAX','NAT_INTLRHEONIND','NAT_INTLRHEONTAX','NAT_INTLRHEOPIND','NAT_INTLRHEOPTAX','NAT_INVNIND','NAT_INVNTAX'
                     ,'NAT_INVPIND','NAT_INVPTAX','NAT_LITHNIND','NAT_LITHNTAX','NAT_LITHPIND','NAT_LITHPTAX','NAT_LOTNIND','NAT_LOTNTAX','NAT_LOTPIND','NAT_LOTPTAX'
                     ,'NAT_MIGRNIND','NAT_MIGRNTAX','NAT_MIGRPIND','NAT_MIGRPTAX','NAT_MTOLNIND','NAT_MTOLNTAX','NAT_MTOLPIND','NAT_MTOLPTAX','NAT_NTOLBENTNIND'
                     ,'NAT_NTOLBENTNTAX','NAT_NTOLBENTPIND','NAT_NTOLBENTPTAX','NAT_NTOLCARNNIND','NAT_NTOLCARNNTAX','NAT_NTOLCARNPIND','NAT_NTOLCARNPTAX'
                     ,'NAT_NTOLINVNIND','NAT_NTOLINVNTAX','NAT_NTOLINVPIND','NAT_NTOLINVPTAX','NAT_NTOLNIND','NAT_NTOLNTAX','NAT_NTOLPIND','NAT_NTOLPTAX'
                     ,'NAT_OMNININD','NAT_OMNINTAX','NAT_OMNIPIND','NAT_OMNIPTAX','NAT_RBCATONIND','NAT_RBCATONTAX','NAT_RBCATOPIND','NAT_RBCATOPTAX','NAT_RHEONIND'
                     ,'NAT_RHEONTAX','NAT_RHEOPIND','NAT_RHEOPTAX','NAT_SALMNIND','NAT_SALMNTAX','NAT_SALMPIND','NAT_SALMPTAX','NAT_TOLRNIND','NAT_TOLRNTAX'
                     ,'NAT_TOLRPIND','NAT_TOLRPTAX','NAT_WTD_TV','NAT_PTAX','NAT_PIND','ALIENNIND','ALIENNTAX','ALIENPIND','ALIENPTAX','ANOMNIND','ANOMPIND')
}else{
  df.empty <- data.frame(t(rep(NA,113)),stringsAsFactors=FALSE) ## FIX NUMBER OF VARIABLES AND ADD CODE TO SEPARATE IF NATIVE TRAIT SUPPLIED OR NOT
  names(df.empty) <- c('TOTLNIND','TOTLNTAX','BENTINVNIND','BENTINVNTAX','BENTINVPIND','BENTINVPTAX','CARNNIND','CARNNTAX','CARNPIND','CARNPTAX','CATONIND','CATONTAX'
                       ,'CATOPIND','CATOPTAX','CENTNIND','CENTNTAX','CENTPIND','CENTPTAX','COLDNIND','COLDNTAX','COLDPIND','COLDPTAX','CYPRNIND','CYPRNTAX','CYPRPIND'
                       ,'CYPRPTAX','HERBNIND','HERBNTAX','HERBPIND','HERBPTAX','ICTANIND','ICTANTAX','ICTAPIND','ICTAPTAX','INTLINVNIND','INTLINVNTAX','INTLINVPIND'
                       ,'INTLINVPTAX','INTLLOTNIND','INTLLOTNTAX','INTLLOTPIND','INTLLOTPTAX','INTLMIGRNIND','INTLMIGRNTAX','INTLMIGRPIND','INTLMIGRPTAX','INTLNIND'
                       ,'INTLNTAX','INTLPIND','INTLPTAX','INTLRHEONIND','INTLRHEONTAX','INTLRHEOPIND','INTLRHEOPTAX','INVNIND','INVNTAX','INVPIND','INVPTAX','LITHNIND'
                       ,'LITHNTAX','LITHPIND','LITHPTAX','LOTNIND','LOTNTAX','LOTPIND','LOTPTAX','MIGRNIND','MIGRNTAX','MIGRPIND','MIGRPTAX','MTOLNIND','MTOLNTAX'
                       ,'MTOLPIND','MTOLPTAX','NTOLBENTNIND','NTOLBENTNTAX','NTOLBENTPIND','NTOLBENTPTAX','NTOLCARNNIND','NTOLCARNNTAX','NTOLCARNPIND','NTOLCARNPTAX'
                       ,'NTOLINVNIND','NTOLINVNTAX','NTOLINVPIND','NTOLINVPTAX','NTOLNIND','NTOLNTAX','NTOLPIND','NTOLPTAX','OMNININD','OMNINTAX','OMNIPIND','OMNIPTAX'
                       ,'RBCATONIND','RBCATONTAX','RBCATOPIND','RBCATOPTAX','RHEONIND','RHEONTAX','RHEOPIND','RHEOPTAX','SALMNIND','SALMNTAX','SALMPIND','SALMPTAX'
                       ,'TOLRNIND','TOLRNTAX','TOLRPIND','TOLRPTAX','WTD_TV','ANOMNIND','ANOMPIND')  
} 

## Merge empty df with allMetrics and drop missing UID observation. Now any missing metrics will be missing values in finalout1
allfishMetrics1 <- subset(smartbind(merge3,df.empty),!is.na(SAMPID))
## Melt data frame, keeping missing values, then fill in missing values with 0 as long as it isn't WTD_TV
if(ANOM==TRUE){
  allfishMetrics1.long <- mutate(melt(allfishMetrics1,id.vars='SAMPID')
                               ,value=ifelse(variable %nin% c('WTD_TV','NAT_WTD_TV') & is.na(value),0,value))
}else{
  allfishMetrics1.long <- mutate(melt(allfishMetrics1,id.vars='SAMPID')
                                 ,value=ifelse(variable %nin% c('WTD_TV','NAT_WTD_TV','ANOMPIND','ANOMNIND') & is.na(value),0,value))
}
allfishMetrics2 <- dcast(allfishMetrics1.long,SAMPID~variable)
if(CALCNAT=='Y'){
  allfishMetrics2 <- allfishMetrics2[,c('SAMPID','TOTLNIND','TOTLNTAX','BENTINVNIND','BENTINVNTAX','BENTINVPIND','BENTINVPTAX','CARNNIND','CARNNTAX','CARNPIND','CARNPTAX','CATONIND','CATONTAX'
                                        ,'CATOPIND','CATOPTAX','CENTNIND','CENTNTAX','CENTPIND','CENTPTAX','COLDNIND','COLDNTAX','COLDPIND','COLDPTAX','CYPRNIND','CYPRNTAX','CYPRPIND'
                                        ,'CYPRPTAX','HERBNIND','HERBNTAX','HERBPIND','HERBPTAX','ICTANIND','ICTANTAX','ICTAPIND','ICTAPTAX','INTLINVNIND','INTLINVNTAX','INTLINVPIND'
                                        ,'INTLINVPTAX','INTLLOTNIND','INTLLOTNTAX','INTLLOTPIND','INTLLOTPTAX','INTLMIGRNIND','INTLMIGRNTAX','INTLMIGRPIND','INTLMIGRPTAX','INTLNIND'
                                        ,'INTLNTAX','INTLPIND','INTLPTAX','INTLRHEONIND','INTLRHEONTAX','INTLRHEOPIND','INTLRHEOPTAX','INVNIND','INVNTAX','INVPIND','INVPTAX','LITHNIND'
                                        ,'LITHNTAX','LITHPIND','LITHPTAX','LOTNIND','LOTNTAX','LOTPIND','LOTPTAX','MIGRNIND','MIGRNTAX','MIGRPIND','MIGRPTAX','MTOLNIND','MTOLNTAX'
                                        ,'MTOLPIND','MTOLPTAX','NTOLBENTNIND','NTOLBENTNTAX','NTOLBENTPIND','NTOLBENTPTAX','NTOLCARNNIND','NTOLCARNNTAX','NTOLCARNPIND','NTOLCARNPTAX'
                                        ,'NTOLINVNIND','NTOLINVNTAX','NTOLINVPIND','NTOLINVPTAX','NTOLNIND','NTOLNTAX','NTOLPIND','NTOLPTAX','OMNININD','OMNINTAX','OMNIPIND','OMNIPTAX'
                                        ,'RBCATONIND','RBCATONTAX','RBCATOPIND','RBCATOPTAX','RHEONIND','RHEONTAX','RHEOPIND','RHEOPTAX','SALMNIND','SALMNTAX','SALMPIND','SALMPTAX'
                                        ,'TOLRNIND','TOLRNTAX','TOLRPIND','TOLRPTAX','WTD_TV','NAT_TOTLNIND','NAT_TOTLNTAX','NAT_BENTINVNIND','NAT_BENTINVNTAX','NAT_BENTINVPIND'
                                        ,'NAT_BENTINVPTAX','NAT_CARNNIND','NAT_CARNNTAX','NAT_CARNPIND','NAT_CARNPTAX','NAT_CATONIND','NAT_CATONTAX','NAT_CATOPIND','NAT_CATOPTAX'
                                        ,'NAT_CENTNIND','NAT_CENTNTAX','NAT_CENTPIND','NAT_CENTPTAX','NAT_COLDNIND','NAT_COLDNTAX','NAT_COLDPIND','NAT_COLDPTAX','NAT_CYPRNIND'
                                        ,'NAT_CYPRNTAX','NAT_CYPRPIND','NAT_CYPRPTAX','NAT_HERBNIND','NAT_HERBNTAX','NAT_HERBPIND','NAT_HERBPTAX','NAT_ICTANIND','NAT_ICTANTAX'
                                        ,'NAT_ICTAPIND','NAT_ICTAPTAX','NAT_INTLINVNIND','NAT_INTLINVNTAX','NAT_INTLINVPIND','NAT_INTLINVPTAX','NAT_INTLLOTNIND','NAT_INTLLOTNTAX'
                                        ,'NAT_INTLLOTPIND','NAT_INTLLOTPTAX','NAT_INTLMIGRNIND','NAT_INTLMIGRNTAX','NAT_INTLMIGRPIND','NAT_INTLMIGRPTAX','NAT_INTLNIND','NAT_INTLNTAX'
                                        ,'NAT_INTLPIND','NAT_INTLPTAX','NAT_INTLRHEONIND','NAT_INTLRHEONTAX','NAT_INTLRHEOPIND','NAT_INTLRHEOPTAX','NAT_INVNIND','NAT_INVNTAX'
                                        ,'NAT_INVPIND','NAT_INVPTAX','NAT_LITHNIND','NAT_LITHNTAX','NAT_LITHPIND','NAT_LITHPTAX','NAT_LOTNIND','NAT_LOTNTAX','NAT_LOTPIND','NAT_LOTPTAX'
                                        ,'NAT_MIGRNIND','NAT_MIGRNTAX','NAT_MIGRPIND','NAT_MIGRPTAX','NAT_MTOLNIND','NAT_MTOLNTAX','NAT_MTOLPIND','NAT_MTOLPTAX','NAT_NTOLBENTNIND'
                                        ,'NAT_NTOLBENTNTAX','NAT_NTOLBENTPIND','NAT_NTOLBENTPTAX','NAT_NTOLCARNNIND','NAT_NTOLCARNNTAX','NAT_NTOLCARNPIND','NAT_NTOLCARNPTAX'
                                        ,'NAT_NTOLINVNIND','NAT_NTOLINVNTAX','NAT_NTOLINVPIND','NAT_NTOLINVPTAX','NAT_NTOLNIND','NAT_NTOLNTAX','NAT_NTOLPIND','NAT_NTOLPTAX'
                                        ,'NAT_OMNININD','NAT_OMNINTAX','NAT_OMNIPIND','NAT_OMNIPTAX','NAT_RBCATONIND','NAT_RBCATONTAX','NAT_RBCATOPIND','NAT_RBCATOPTAX','NAT_RHEONIND'
                                        ,'NAT_RHEONTAX','NAT_RHEOPIND','NAT_RHEOPTAX','NAT_SALMNIND','NAT_SALMNTAX','NAT_SALMPIND','NAT_SALMPTAX','NAT_TOLRNIND','NAT_TOLRNTAX'
                                        ,'NAT_TOLRPIND','NAT_TOLRPTAX','NAT_WTD_TV','NAT_PTAX','NAT_PIND','ALIENNIND','ALIENNTAX','ALIENPIND','ALIENPTAX','ANOMNIND','ANOMPIND')]
}else{
  allfishMetrics2 <- allfishMetrics2[,c('SAMPID','TOTLNIND','TOTLNTAX','BENTINVNIND','BENTINVNTAX','BENTINVPIND','BENTINVPTAX','CARNNIND','CARNNTAX','CARNPIND','CARNPTAX','CATONIND','CATONTAX'
                                                   ,'CATOPIND','CATOPTAX','CENTNIND','CENTNTAX','CENTPIND','CENTPTAX','COLDNIND','COLDNTAX','COLDPIND','COLDPTAX','CYPRNIND','CYPRNTAX','CYPRPIND'
                                                   ,'CYPRPTAX','HERBNIND','HERBNTAX','HERBPIND','HERBPTAX','ICTANIND','ICTANTAX','ICTAPIND','ICTAPTAX','INTLINVNIND','INTLINVNTAX','INTLINVPIND'
                                                   ,'INTLINVPTAX','INTLLOTNIND','INTLLOTNTAX','INTLLOTPIND','INTLLOTPTAX','INTLMIGRNIND','INTLMIGRNTAX','INTLMIGRPIND','INTLMIGRPTAX','INTLNIND'
                                                   ,'INTLNTAX','INTLPIND','INTLPTAX','INTLRHEONIND','INTLRHEONTAX','INTLRHEOPIND','INTLRHEOPTAX','INVNIND','INVNTAX','INVPIND','INVPTAX','LITHNIND'
                                                   ,'LITHNTAX','LITHPIND','LITHPTAX','LOTNIND','LOTNTAX','LOTPIND','LOTPTAX','MIGRNIND','MIGRNTAX','MIGRPIND','MIGRPTAX','MTOLNIND','MTOLNTAX'
                                                   ,'MTOLPIND','MTOLPTAX','NTOLBENTNIND','NTOLBENTNTAX','NTOLBENTPIND','NTOLBENTPTAX','NTOLCARNNIND','NTOLCARNNTAX','NTOLCARNPIND','NTOLCARNPTAX'
                                                   ,'NTOLINVNIND','NTOLINVNTAX','NTOLINVPIND','NTOLINVPTAX','NTOLNIND','NTOLNTAX','NTOLPIND','NTOLPTAX','OMNININD','OMNINTAX','OMNIPIND','OMNIPTAX'
                                                   ,'RBCATONIND','RBCATONTAX','RBCATOPIND','RBCATOPTAX','RHEONIND','RHEONTAX','RHEOPIND','RHEOPTAX','SALMNIND','SALMNTAX','SALMPIND','SALMPTAX'
                                                   ,'TOLRNIND','TOLRNTAX','TOLRPIND','TOLRPTAX','WTD_TV','ANOMNIND','ANOMPIND')]
}

# Add back noFish samples, then melt and fill in RESULT=0, then re-cast
	if(nrow(noFish)>0){
		merge4 <- melt(smartbind(allfishMetrics2,noFish),id.vars='SAMPID',na.rm=FALSE)
			merge4 <- mutate(merge4,value=ifelse(variable %nin% c('WTD_TV','NAT_WTD_TV','ANOMPIND','ANOMNIND','FLAG') & is.na(value),0,value))
		merge5 <- dcast(merge4,SAMPID~variable)	
	}else{
		merge5 <- allfishMetrics2
	}
	# Add back sample info included in original data frame in the variables listed in sampID
	finmetrics <- merge(unique(subset(inCts,select=c(sampID,'SAMPID'))),merge5,by='SAMPID')
	
	finalout <- subset(finmetrics,select=-SAMPID)	
	return(finalout)
	
}
