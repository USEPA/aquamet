invertMet <- function(inCts, inTaxa=NULL, sampID="UID", dist="IS_DISTINCT",
   ct="TOTAL", taxlevel="WSA") {

################################################################################
# Function: invertMet
# Title: Calculate NRSA Invertebrate Metrics
# Programmers: Karen Blocksom
#              Tom Kincaid
# Date: May 10, 2013
# Description:
#   This function calculates the NRSA invertebrate metrics.  It assumes input
#   counts are organized so that a single column represents the unique sample,
#   with each row representing the count for a single taxon in that sample.
# Function Revisions:
#   05/10/13 kab: Created.
#   08/09/13 kab: Updated to reflect Tolerance categories used by A. Herlihy in
#            WSA.
#   10/31/13 kab: Updated to use default invertebrate taxa list.
#   11/08/13 kab: Updated to add labeling for creation of trait fields.
#   03/13/14 tmk: Removed calls to the require() and source() functions.
#            Removed the call to read.table() to input the inTaxa data frame
#            and replaced it with assignment of the bentTaxa data frame that is
#            included in the sysdata.rda file to inTaxa.  Modified to return a
#            character string when an error occurs.
#   03/17/14 kab: Updated to add condition to fixTaxa statement where
#            aggregating for WSA taxlevel - add FAMILY!=''.
#   05/19/14 kab: Added in code to make sure all metrics are calculated even if 
#            no individuals with trait are in samples. Changed input data frame
#            name from inAll to inCts
# Arguments:
#   inCts = a data frame with a variable containing counts (with data already
#     summed by taxon and sample) and with a variable indicating distinctness of
#     each taxon in a sample (coded 0/1).
#   inTaxa = a data frame containing the taxalist in wide format with
#     variables for taxonomy as well as any other traits of interest.  If this
#     argument is NULL, then the data frame is assigned from the bentTaxa data
#     frame that is included in the sysdata.rda file.  The required fields in
#     this data frame depend on the value of the field taxlevel. If
#     taxlevel="WSA", the following fields are required: PHYLUM, CLASS, ORDER,
#     FAMILY, TRIBE, SUBFAMILY, GENUS, PTV_WSA, HABIT_WSA, FFG_WSA, and
#     TARGET_TAXON.  Otherwise, the following fields are required: PHYLUM,
#     CLASS, ORDER, FAMILY, TRIBE, SUBFAMILY, GENUS, PTV,HABIT, and FFG.  The
#     default is NULL.
#   sampID = a character vector indicating names of variable(s) necessary to
#     identify a unique sample.  The default is "UID".
#   dist = name of the variable in the inCts data frame indicating distinctness.
#     The default is "IS_DISTINCT".
#   ct = name of the variable in the inCts data frame providing number of
#     organisms counted for a taxon in a sample.  The default is "TOTAL".
#   taxlevel = level of taxonomic aggregation used in metric calculations.  This
#     argument also is used as the value of METRIC_TAXA_ROOT in the output file.
#     The default is "WSA", which reflects the taxonomic resolution employed in
#     the WSA-based MMI used for NRSA.  If the value is "WSA", further
#     aggregation of input data and subsequent re-evaluation of distinctness
#     occurs within the function.  Otherwise, taxonomic resolution is used as
#     is.
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The first columns of the data frame contain the variables
#   specified in the sampID argument.  Subsequent columns are named for each
#   metric and contain metric values.  A list of metrics is provided in the
#   document named "Invert_Metric_Descriptions.pdf" included in the help
#   directory for the package.
# Other Functions Required:
#   assignDistinct - assign distinct taxa names to samples
#   runInvertMetrics - calculate metrics
################################################################################

# If necessary, load the bentTaxa data frame and assign it to inTaxa.  Though
# NON_TARGET taxa are included in the table provided by NRSA, we need to exclude
# them from our calculations.
# bentTaxa <- subset(read.table("bentTaxa.tab", sep="\t", header=TRUE,
#    stringsAsFactors=FALSE), is.na(NON_TARGET))
if(is.null(inTaxa)) {
   inTaxa <- bentTaxa
   inTaxa <- subset(inTaxa, is.na(NON_TARGET) | NON_TARGET == "")
}

# Combine all values in sampID into one sampID in df
for(i in 1:length(sampID)){
	if(i==1) inCts$SAMPID <- inCts[,sampID[i]]
	else inCts$SAMPID <- paste(inCts$SAMPID,inCts[,sampID[i]],sep='.')
}
## For each metric except HBI and diversity indices, calculate no. taxa, % taxa, and % individuals
## This code assumes that the following are columns in the taxa file: PHYLUM, CLASS, ORDER, FAMILY, SUBFAMILY, TRIBE, HABIT, FFG, PTV, 
##			TARGET_TAXON, 
## The two-letter codes for HABIT and FFG are assumed to match those used for NRSA. All names are assumed to be uppercase
if(taxlevel=='WSA'){
	necTraits <- c('PHYLUM','CLASS','ORDER','FAMILY','TRIBE','SUBFAMILY','GENUS','PTV_WSA','HABIT_WSA','FFG_WSA','TARGET_TAXON')
}else{
	necTraits <- c('PHYLUM','CLASS','ORDER','FAMILY','TRIBE','SUBFAMILY','PTV','HABIT','FFG')
}
if(any(necTraits %nin% names(inTaxa))){
	msgTraits <- which(necTraits %nin% names(inTaxa))
	return(paste("Some of the traits are missing from the taxa list. The following are \nrequired for metric calculations to run:\n", necTraits[msgTraits], "\n"))
}
if(taxlevel=='WSA'){
	## Need to make sure inTaxa does not contain HABIT, FFG, or PTV before changing names of _WSA variables
	inTaxa <- inTaxa[,names(inTaxa) %in% c('TAXA_ID','NON_TARGET',necTraits)]
	inTaxa <- plyr::rename(inTaxa,c('HABIT_WSA'='HABIT','FFG_WSA'='FFG','PTV_WSA'='PTV'))
} 
# Make sure that PTV values are numeric, or the code below doesn't work correctly for taxa with PTVs of 10
inTaxa$PTV <- as.numeric(inTaxa$PTV)

inTaxa$EPT_<-NA # Ephemeroptera, Plecoptera, and Trichoptera taxa
inTaxa$EPT_[with(inTaxa,ORDER %in% c('PLECOPTERA','EPHEMEROPTERA','TRICHOPTERA'))]<-1
inTaxa$EPHE<-NA # Ephemeroptera taxa
inTaxa$EPHE[with(inTaxa,ORDER %in% c('EPHEMEROPTERA'))]<-1
inTaxa$PLEC<-NA # Plectoptera taxa
inTaxa$PLEC[with(inTaxa,ORDER %in% c('PLECOPTERA'))]<-1
inTaxa$TRIC<-NA # Trichoptera taxa
inTaxa$TRIC[with(inTaxa,ORDER %in% c('TRICHOPTERA'))]<-1
inTaxa$CHIR<-NA # Chironomidae taxa
inTaxa$CHIR[with(inTaxa,FAMILY %in% c('CHIRONOMIDAE'))]<-1
inTaxa$CRUS<-NA # Crustacea taxa
inTaxa$CRUS[with(inTaxa,CLASS %in% c('MALACOSTRACA','MAXILLOPODA','BRANCHIOPODA'
						,'CEPHALOCARIDA','OSTRACODA','REMIPEDIA'))]<-1
inTaxa$NOIN<-NA # Non-insect taxa
inTaxa$NOIN[with(inTaxa,CLASS %nin% c('INSECTA'))]<-1
inTaxa$DIPT<-NA # Diptera taxa
inTaxa$DIPT[with(inTaxa,ORDER %in% c('DIPTERA'))]<-1
inTaxa$MOLL<-NA # Mollusca taxa
inTaxa$MOLL[with(inTaxa,PHYLUM %in% c('MOLLUSCA'))]<-1
inTaxa$COFI<-NA # Collector-filterers
inTaxa$COFI[regexpr('CF',inTaxa$FFG)>0 & !is.na(inTaxa$FFG)]<-1
inTaxa$COGA<-NA # Collector-gatherers
inTaxa$COGA[regexpr('CG',inTaxa$FFG)>0 & !is.na(inTaxa$FFG)]<-1
inTaxa$PRED<-NA # Predators
inTaxa$PRED[regexpr('PR',inTaxa$FFG)>0 & !is.na(inTaxa$FFG)]<-1
inTaxa$SHRD<-NA # Shredders
inTaxa$SHRD[regexpr('SH',inTaxa$FFG)>0 & !is.na(inTaxa$FFG)]<-1
inTaxa$SCRP<-NA # Scrapers
inTaxa$SCRP[regexpr('SC',inTaxa$FFG)>0 & !is.na(inTaxa$FFG)]<-1
## Change: Set all habit metrics for all taxa instead of just insects. Previously: WSA MMI metric calculations for HABIT were based on all taxa, and for NRSA on only Insects, as those are the only reliable taxa 
##	with habit info 
#if(taxlevel=='WSA'){
#	inTaxa$CLMB<-NA
#	inTaxa$CLMB[regexpr('CB',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
#	inTaxa$CLNG<-NA
#	inTaxa$CLNG[regexpr('CN',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
#	inTaxa$BURR<-NA
#	inTaxa$BURR[regexpr('BU',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
#	inTaxa$SPWL<-NA
#	inTaxa$SPWL[regexpr('SP',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
#	inTaxa$SWIM<-NA
#	inTaxa$SWIM[regexpr('SW',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
#}else{
#	inTaxa$CLMB<-NA
#	inTaxa$CLMB[regexpr('CB',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT) & inTaxa$CLASS %in% c('INSECTA')]<-1
#	inTaxa$CLNG<-NA
#	inTaxa$CLNG[regexpr('CN',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT) & inTaxa$CLASS %in% c('INSECTA')]<-1
#	inTaxa$BURR<-NA
#	inTaxa$BURR[regexpr('BU',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT) & inTaxa$CLASS %in% c('INSECTA')]<-1
#	inTaxa$SPWL<-NA
#	inTaxa$SPWL[regexpr('SP',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT) & inTaxa$CLASS %in% c('INSECTA')]<-1
#	inTaxa$SWIM<-NA
#	inTaxa$SWIM[regexpr('SW',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT) & inTaxa$CLASS %in% c('INSECTA')]<-1
#}
inTaxa$CLMB<-NA # Climbers
inTaxa$CLMB[regexpr('CB',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
inTaxa$CLNG<-NA # Clingers
inTaxa$CLNG[regexpr('CN',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
inTaxa$BURR<-NA # Burrowers
inTaxa$BURR[regexpr('BU',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
inTaxa$SPWL<-NA # Sprawlers
inTaxa$SPWL[regexpr('SP',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
inTaxa$SWIM<-NA # Swimmers
inTaxa$SWIM[regexpr('SW',inTaxa$HABIT)>0 & !is.na(inTaxa$HABIT)]<-1
inTaxa$TOLR<-NA # Tolerant taxa (tol value >=7)
inTaxa$TOLR[with(inTaxa,PTV>=7)]<-1
inTaxa$FACL<-NA # Facultative tolerance (tol value >3 and <7)
inTaxa$FACL[with(inTaxa,PTV>3 & PTV<7)]<-1
inTaxa$INTL<-NA # Intolerant taxa (tol val <=3)
inTaxa$INTL[with(inTaxa,PTV<=3)]<-1
inTaxa$NTOL<-NA # Non-tolerant taxa (tol val <6)
inTaxa$NTOL[with(inTaxa,PTV<6)]<-1
inTaxa$STOL<-NA # Super tolerant taxa (tol val >=8)
inTaxa$STOL[with(inTaxa,PTV>=8)]<-1
inTaxa$TUBINAID<-NA # Tubificid and Naidid worms
inTaxa$TUBINAID[with(inTaxa,FAMILY %in% c('TUBIFICIDAE','NAIDIDAE'))]<-1
inTaxa$TL01<-NA # Tolerance value <2
inTaxa$TL01[with(inTaxa,PTV<2)]<-1
inTaxa$TL23<-NA # Tolerance value >=2 and <4
inTaxa$TL23[with(inTaxa,PTV>=2 & PTV<4)]<-1
inTaxa$TL45<-NA # Tolerance value >=4 and <6
inTaxa$TL45[with(inTaxa,PTV>=4 & PTV<6)]<-1
inTaxa$TL67<-NA # Tolerance value >=6 and <8
inTaxa$TL67[with(inTaxa,PTV>=6 & PTV<8)]<-1
inTaxa$AMPH<-NA # Amphipods
inTaxa$AMPH[with(inTaxa,ORDER %in% c('AMPHIPODA'))]<-1
inTaxa$COFITRIC<-NA # Collector-filterer Trichoptera
inTaxa$COFITRIC[with(inTaxa,COFI==1 & TRIC==1)]<-1
inTaxa$EPOT<-NA # Ephemeroptera, Plecoptera, Odonata, and Trichoptera taxa
inTaxa$EPOT[with(inTaxa,TRIC==1|EPHE==1|PLEC==1|ORDER %in% c('ODONATA'))]<-1
inTaxa$HEMI<-NA # Hemiptera taxa
inTaxa$HEMI[with(inTaxa,ORDER %in% c('HEMIPTERA'))]<-1
inTaxa$MITE<-NA # Mites
inTaxa$MITE[with(inTaxa,ORDER %in% c('TROMBIDIFORMES','SARCOPTIFORMES'))]<-1
inTaxa$ODON<-NA # Odonata
inTaxa$ODON[with(inTaxa,ORDER %in% c('ODONATA'))]<-1
inTaxa$OLLE<-NA # Oligochaetes and leeches
inTaxa$OLLE[with(inTaxa,CLASS %in% c('OLIGOCHAETA','HIRUDINEA'))]<-1
inTaxa$ORTH<-NA # Chironomids in subfamily Orthocladiinae
inTaxa$ORTH[with(inTaxa,SUBFAMILY %in% c('ORTHOCLADIINAE'))]<-1
inTaxa$TANY<-NA # Chironomids in tribe Tanytarsini
inTaxa$TANY[with(inTaxa,TRIBE %in% c('TANYTARSINI'))]<-1

# Drop non-target taxa if included in taxalist
if(length(grep('NON_TARGET',names(inTaxa)))>0) {
	inTaxa <- subset(inTaxa,is.na(NON_TARGET)|NON_TARGET=='')
}

params<-c('EPT_','EPHE','PLEC','TRIC','CHIR','CRUS','DIPT','MOLL','NOIN','COFI','COGA','PRED','SHRD','SCRP'
		,'CLMB','CLNG','BURR','SPWL','SWIM','TOLR','FACL','INTL','NTOL','STOL','TL01','TL23','TL45','TL67'
		,'TUBINAID','AMPH','COFITRIC','EPOT','HEMI','MITE','ODON','OLLE','ORTH','TANY')


taxalong <- melt(inTaxa[,c('TAXA_ID',params,'PTV')],id.vars=c('TAXA_ID'),variable.name="PARAMETER",value.name="RESULT",na.rm=TRUE)
taxalong$PARAMETER <- as.character(taxalong$PARAMETER)

# Set aside observations with TAXA_ID=999999, because this indicates that no invertebrates were collected at that site,
# although a sample WAS collected.
noBugs <- subset(inCts,TAXA_ID==999999,select=c(sampID,'SAMPID'))
	if(nrow(noBugs)>0) {
		noBugs <- mutate(noBugs,TOTLNIND=0,FLAG='Site sampled, no invertebrates collected.')
	}
# Make sure there are no NON_TARGET individuals in inCts before sending to metric calculation function, and keep only relevant variables
inCts1 <- subset(merge(inCts,subset(inTaxa,select='TAXA_ID')),select=c(sampID,'SAMPID','TAXA_ID',dist,ct))
# Rename counts and distinct variables to FINAL_CT and IS_DISTINCT
	names(inCts1)[names(inCts1)==ct] <- 'FINAL_CT'
	names(inCts1)[names(inCts1)==dist] <- 'IS_DISTINCT'
# Remove any records with NA as the count. This may mean the sample was not collected or, in the case of NRSA, that the taxon was not part of the
#	300 count random subsample
inCts1 <- subset(inCts1,!is.na(FINAL_CT) & FINAL_CT>0)

## Now we need to aggregate data if taxlevel='WSA'
if(taxlevel=='WSA'){
	## Merge benthic data with taxalist in wide format (inTaxa)
	toFix <- merge(inCts1,inTaxa,by='TAXA_ID')
	## Roll mites, oligochaetes, and polychaetes up to family level
	fixTaxa <- with(toFix,which(CLASS %in% c('ARACHNIDA','POLYCHAETA','OLIGOCHAETA') & !is.na(FAMILY) & FAMILY!=''))
	toFix$TARGET_TAXON[fixTaxa] <- toFix$FAMILY[fixTaxa]
	toFix1 <- merge(toFix,subset(inTaxa,select=c('TAXA_ID','TARGET_TAXON')),by='TARGET_TAXON',all.x=TRUE)
	toFix1 <- plyr::rename(toFix1,c('TAXA_ID.y'='TAXA_ID'))
	toFix1 <- subset(toFix1,select=names(toFix1) %nin% c('TAXA_ID.x'))
	
	## Combine Cricotopus/Orthocladius and Thienemannimyia Genus Gr. into Chironomidae, added 7/22/2013
	toFix1$TAXA_ID[toFix1$TARGET_TAXON %in% c('CRICOTOPUS/ORTHOCLADIUS','THIENEMANNIMYIA GENUS GR.')] <- 3581
	toFix1$TARGET_TAXON[toFix1$TARGET_TAXON %in% c('CRICOTOPUS/ORTHOCLADIUS','THIENEMANNIMYIA GENUS GR.')] <- 'CHIRONOMIDAE'
	
	## Combine all Ceratopogoninae taxa to family level
	toFix1$TAXA_ID[toFix1$TARGET_TAXON %in% c('CERATOPOGONINAE')] <- 3566
	toFix1$TARGET_TAXON[toFix1$TARGET_TAXON %in% c('CERATOPOGONINAE')] <- 'CERATOPOGONIDAE'
	
	agg1 <- ddply(toFix1,c(sampID,'SAMPID','TAXA_ID'),summarise,FINAL_CT=sum(FINAL_CT))
	agg2<-merge(agg1,inTaxa,by='TAXA_ID')
	agg2<-assignDistinct(agg2,c('SAMPID',sampID))
	agg2$IS_DISTINCT[is.na(agg2$IS_DISTINCT)]<-0
	
	indata<-subset(agg2,select=c(sampID,'SAMPID','TAXA_ID','FINAL_CT','IS_DISTINCT'))

}else{
	indata <- inCts1
}

print("Ready to run metrics")
allMetrics <- runInvertMetrics(indata,params,taxalong)
print("Done running metrics. Now merging back with sample info and full list of metrics")
## Make sure all metrics are included in dataset, regardless of whether they were calculated - if no individuals of a particular trait were
##  present in the input dataframe, the values for metrics for that trait wouldn't have been calculated
df.empty <- data.frame(expand.grid(SAMPID=unique(allMetrics$SAMPID),variable=c('TOTLNIND','TOTLNTAX','AMPHNIND','AMPHNTAX','AMPHPIND','AMPHPTAX','BURRNIND','BURRNTAX','BURRPIND'
                                                                    ,'BURRPTAX','CHIRNIND','CHIRNTAX','CHIRPIND','CHIRPTAX','CLMBNIND','CLMBNTAX','CLMBPIND','CLMBPTAX'
                                                                    ,'CLNGNIND','CLNGNTAX','CLNGPIND','CLNGPTAX','COFININD','COFINTAX','COFIPIND','COFIPTAX','COFITRICNIND'
                                                                    ,'COFITRICNTAX','COFITRICPIND','COFITRICPTAX','COGANIND','COGANTAX','COGAPIND','COGAPTAX','CRUSNIND'
                                                                    ,'CRUSNTAX','CRUSPIND','CRUSPTAX','DIPTNIND','DIPTNTAX','DIPTPIND','DIPTPTAX','EPHENIND','EPHENTAX'
                                                                    ,'EPHEPIND','EPHEPTAX','EPOTNIND','EPOTNTAX','EPOTPIND','EPOTPTAX','EPT_NIND','EPT_NTAX','EPT_PIND'
                                                                    ,'EPT_PTAX','FACLNIND','FACLNTAX','FACLPIND','FACLPTAX','HEMININD','HEMINTAX','HEMIPIND','HEMIPTAX'
                                                                    ,'INTLNIND','INTLNTAX','INTLPIND','INTLPTAX','MITENIND','MITENTAX','MITEPIND','MITEPTAX','MOLLNIND'
                                                                    ,'MOLLNTAX','MOLLPIND','MOLLPTAX','NOINNIND','NOINNTAX','NOINPIND','NOINPTAX','NTOLNIND','NTOLNTAX'
                                                                    ,'NTOLPIND','NTOLPTAX','ODONNIND','ODONNTAX','ODONPIND','ODONPTAX','OLLENIND','OLLENTAX','OLLEPIND'
                                                                    ,'OLLEPTAX','ORTHNIND','ORTHNTAX','ORTHPIND','ORTHPTAX','PLECNIND','PLECNTAX','PLECPIND','PLECPTAX'
                                                                    ,'PREDNIND','PREDNTAX','PREDPIND','PREDPTAX','SCRPNIND','SCRPNTAX','SCRPPIND','SCRPPTAX','SHRDNIND'
                                                                    ,'SHRDNTAX','SHRDPIND','SHRDPTAX','SPWLNIND','SPWLNTAX','SPWLPIND','SPWLPTAX','STOLNIND','STOLNTAX'
                                                                    ,'STOLPIND','STOLPTAX','SWIMNIND','SWIMNTAX','SWIMPIND','SWIMPTAX','TANYNIND','TANYNTAX','TANYPIND'
                                                                    ,'TANYPTAX','TL01NIND','TL01NTAX','TL01PIND','TL01PTAX','TL23NIND','TL23NTAX','TL23PIND','TL23PTAX'
                                                                    ,'TL45NIND','TL45NTAX','TL45PIND','TL45PTAX','TL67NIND','TL67NTAX','TL67PIND','TL67PTAX','TOLRNIND'
                                                                    ,'TOLRNTAX','TOLRPIND','TOLRPTAX','TRICNIND','TRICNTAX','TRICPIND','TRICPTAX','TUBINAIDNIND'
                                                                    ,'TUBINAIDNTAX','TUBINAIDPIND','TUBINAIDPTAX','WTD_TV','ORTHCHIRPIND','HPRIME','DOM1PIND','DOM3PIND'
                                                                    ,'DOM5PIND','CHIRDOM1PIND','CHIRDOM3PIND','CHIRDOM5PIND')
                        ,stringsAsFactors=FALSE))
## Merge empty df with allMetrics and drop missing UID observation. Now any missing metrics will be missing values in finalout1
allMetrics1 <- melt(allMetrics,id.vars='SAMPID')
allMetrics2 <- merge(allMetrics1,df.empty,by=c('SAMPID','variable'),all.y=TRUE)
## Melt data frame, keeping missing values, then fill in missing values with 0 as long as it isn't WTD_TV
allMetrics2 <- mutate(allMetrics2,value=ifelse(variable!='WTD_TV' & is.na(value),0,value))
allMetrics3 <- dcast(allMetrics2,SAMPID~variable)
# Reorder to a predetermined order
allMetrics3 <- allMetrics3[, c('SAMPID','TOTLNIND','TOTLNTAX','AMPHNIND','AMPHNTAX','AMPHPIND','AMPHPTAX','BURRNIND','BURRNTAX','BURRPIND'
                               ,'BURRPTAX','CHIRNIND','CHIRNTAX','CHIRPIND','CHIRPTAX','CLMBNIND','CLMBNTAX','CLMBPIND','CLMBPTAX'
                               ,'CLNGNIND','CLNGNTAX','CLNGPIND','CLNGPTAX','COFININD','COFINTAX','COFIPIND','COFIPTAX','COFITRICNIND'
                               ,'COFITRICNTAX','COFITRICPIND','COFITRICPTAX','COGANIND','COGANTAX','COGAPIND','COGAPTAX','CRUSNIND'
                               ,'CRUSNTAX','CRUSPIND','CRUSPTAX','DIPTNIND','DIPTNTAX','DIPTPIND','DIPTPTAX','EPHENIND','EPHENTAX'
                               ,'EPHEPIND','EPHEPTAX','EPOTNIND','EPOTNTAX','EPOTPIND','EPOTPTAX','EPT_NIND','EPT_NTAX','EPT_PIND'
                               ,'EPT_PTAX','FACLNIND','FACLNTAX','FACLPIND','FACLPTAX','HEMININD','HEMINTAX','HEMIPIND','HEMIPTAX'
                               ,'INTLNIND','INTLNTAX','INTLPIND','INTLPTAX','MITENIND','MITENTAX','MITEPIND','MITEPTAX','MOLLNIND'
                               ,'MOLLNTAX','MOLLPIND','MOLLPTAX','NOINNIND','NOINNTAX','NOINPIND','NOINPTAX','NTOLNIND','NTOLNTAX'
                               ,'NTOLPIND','NTOLPTAX','ODONNIND','ODONNTAX','ODONPIND','ODONPTAX','OLLENIND','OLLENTAX','OLLEPIND'
                               ,'OLLEPTAX','ORTHNIND','ORTHNTAX','ORTHPIND','ORTHPTAX','PLECNIND','PLECNTAX','PLECPIND','PLECPTAX'
                               ,'PREDNIND','PREDNTAX','PREDPIND','PREDPTAX','SCRPNIND','SCRPNTAX','SCRPPIND','SCRPPTAX','SHRDNIND'
                               ,'SHRDNTAX','SHRDPIND','SHRDPTAX','SPWLNIND','SPWLNTAX','SPWLPIND','SPWLPTAX','STOLNIND','STOLNTAX'
                               ,'STOLPIND','STOLPTAX','SWIMNIND','SWIMNTAX','SWIMPIND','SWIMPTAX','TANYNIND','TANYNTAX','TANYPIND'
                               ,'TANYPTAX','TL01NIND','TL01NTAX','TL01PIND','TL01PTAX','TL23NIND','TL23NTAX','TL23PIND','TL23PTAX'
                               ,'TL45NIND','TL45NTAX','TL45PIND','TL45PTAX','TL67NIND','TL67NTAX','TL67PIND','TL67PTAX','TOLRNIND'
                               ,'TOLRNTAX','TOLRPIND','TOLRPTAX','TRICNIND','TRICNTAX','TRICPIND','TRICPTAX','TUBINAIDNIND'
                               ,'TUBINAIDNTAX','TUBINAIDPIND','TUBINAIDPTAX','WTD_TV','ORTHCHIRPIND','HPRIME','DOM1PIND','DOM3PIND'
                               ,'DOM5PIND','CHIRDOM1PIND','CHIRDOM3PIND','CHIRDOM5PIND')]
print("Full set of metrics created.")
#keepMets <- grep('PIND|TOTLNIND|NTAX|PTAX|WTD_TV|HPRIME|UID|SAMPLE_|SAMPID',names(allMetrics),ignore.case=TRUE,value=TRUE)
#allMetrics1 <- allMetrics[,keepMets]
# Add back sample info included in original data frame in the variables listed in sampID
finMetrics <- merge(unique(subset(indata,select=c(sampID,'SAMPID'))),allMetrics3,by='SAMPID')

# Now we add back in the samples with no bugs
if(nrow(noBugs)>0){
	finMetrics1 <- smartbind(finMetrics,noBugs)
	finMetrics2 <- melt(finMetrics1,id.vars=c(sampID,'FLAG'))
		finMetrics2 <- mutate(finMetrics2,value=ifelse(is.na(value) & variable!='WTD_TV',0,value))
		form1 <- paste(paste(c(sampID,'FLAG'),collapse='+'),'variable',sep='~')
		finalout <- dcast(finMetrics2,as.formula(form1),value.var='value')
}else {
	finalout <- finMetrics
}
finalout <- finalout[,names(finalout)!='SAMPID']
finalout$METRIC_TAXA_ROOT <- taxlevel
print("Done.")
	return(finalout)
}
