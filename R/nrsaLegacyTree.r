# nrsaLegacyTree.r
#

#' @export
#' @title Calculate NRSA Legacy Riparian Trees Metrics
#' @description This function calculates the legacy riparian trees 
#' portion of the physical habitat metrics for National Rivers and 
#' Streams Assessment (NRSA) data.  The function requires a data 
#' frame containing the invasive species/legacy riparian trees data file.
#' @param dbhClass A data frame containing dbh class values at each 
#' transect of all sites.  Expected to contain the columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE character values specifying the dbh class,
#'                  expected to be one of '0-0.1', '.1-.3',
#'                  '.3-.75', '.75-2' or '>2'
#' }
#' @param distance A dataframe containing distance to largest legacy tree 
#' at each transect of all sites.  Expected to contain the columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE numeric values specifying the distance value
#' }
#' @param heightClass A data frame containing height class values at 
#' each transect of all sites.  Expected to contain the columns:
#' \itemize{
#'    \item SITE integer or character specifying the site visit
#'    \item TRANSECT character value specifying the transect
#'                   for which the value was recorded.
#'    \item VALUE character values specifying the dbh class,
#'                expected to be one of '<5', '5-15', '15-30'
#'                or '>30'
#' }
#' @param species A data frame containing names of largest legacy tree 
#' taxa at each transect of all sites.  Expected to contain the columns:
#' \itemize{
#'        \item SITE integer or character specifying the site visit
#'        \item TRANSECT character value specifying the transect
#'                       for which the value was recorded.
#'        \item VALUE character values specifying the dbh class,
#'                    expected to be one of '0-0.1', '.1-.3',
#'                    '.3-.75', '.75-2' or '>2'
#' }
#' @param type dataframe containing types of largest legacy tree at each
#' transect of all sites.  Expected to contain the columns:
#' \itemize{
#'      \item SITE integer or character specifying the site visit
#'      \item TRANSECT character value specifying the transect
#'                     for which the value was recorded.
#'      \item VALUE character values, either Coniferous or Deciduous.
#' }
#' @return Either a data frame when metric calculation is successful 
#' or a character string containing an error message when metric 
#' calculation is not successful.  The data frame contains the following 
#' columns:
#' \itemize{
#'    \item SITE - universal ID value
#'    \item METRIC - metric name
#'    \item VALUE - metric value
#' }
#' Metrics calculated for wadeable sites include:
#' \itemize{
#'   \item ltmxcnt - Legacy number of largest trees
#'   \item ltmxdbh - Legacy largest tree dbh
#'   \item ltmxdist - Legacy largest tree distance
#'   \item ltmxht - Legacy largest tree height
#'   \item ltmxsize - Legacy largest tree size class (SMLX)
#'   \item ltmxspp - Legacy largest tree species
#'   \item ltsplist - Legacy tree species list comma delim.
#'   \item ltfracs - Legacy fraction of reach trees >= small
#'   \item ltfracm - Legacy fraction of reach trees >= medium
#'   \item ltfracl - Legacy fraction of reach trees >= large
#'   \item ltfracx - Legacy fraction of reach trees >= Xlarge
#'   \item ltmddist - Legacy mean dist of trees >= median size
#'   \item ltmddom - Legacy dominant sp.
#'   \item ltmddomn - Legacy dominant sp. Count
#'   \item ltmdsub - Legacy subdominant sp. >= median size
#'   \item ltmdsubn - Legacy subdominant sp. count
#'  } 
#' Descriptions for all metrics are included in 
#' \emph{NRSA_Physical_Habitat_Metric_Descriptions.pdf} in the package
#' documentation.
#' @author Curt Seeliger \email{Seeliger.Curt@epa.gov}\cr
#' Tom Kincaid \email{Kincaid.Tom@epa.gov}

#' 
nrsaLegacyTree <- function(dbhClass = NULL
                          ,distance = NULL
                          ,heightClass = NULL
                          ,species = NULL
                          ,type = NULL
                          ) {

################################################################################
# Function: nrsaLegacyTree
# Title: Calculate NRSA Legacy Riparian Trees Metrics
# Programmers: Suzanne San Romani
#              Curt Seeliger
#              Tom Kincaid
# Date: February 10, 2010
# Description:
#   This function calculates the legacy riparian trees portion of the physical
#   habitat metrics for National Rivers and Streams Assessment (NRSA) data.  The
#   function requires a data frame containing the invasive species/legacy
#   riparian trees data file.
# Metrics:
#   ltmxcnt - Legacy number of largest trees
#   ltmxdbh - Legacy largest tree dbh
#   ltmxdist - Legacy largest tree distance
#   ltmxht - Legacy largest tree height
#   ltmxsize - Legacy largest tree size class (SMLX)
#   ltmxspp - Legacy largest tree species
#   ltsplist - Legacy tree species list comma delim.
#   ltfracs - Legacy fraction of reach trees >= small
#   ltfracm - Legacy fraction of reach trees >= medium
#   ltfracl - Legacy fraction of reach trees >= large
#   ltfracx - Legacy fraction of reach trees >= Xlarge
#   ltmddist - Legacy mean dist of trees >= median size
#   ltmddom - Legacy dominant sp.
#   ltmddomn - Legacy dominant sp. Count
#   ltmdsub - Legacy subdominant sp. >= median size
#   ltmdsubn - Legacy subdominant sp. count
# Function Revisions:
#   02/10/10 ssr: Created.
#   02/18/10 cws: Removed source() of NRSAValidation.r.
#   03/22/10 cws: Added missing calls to checkEquals in unit test.
#   03/25/10 cws: Changed diff() calls to dfCompare().
#   09/16/10 cws: Removed hardcoding of NRSA database name, using NRSAdbName
#          instead.
#   01/24/11 cws: Removed source() of assignTaxCat() to NRSAvalidation.r.
#            Changed ltmxcnt, ltmxdbh, ltmxdist, ltmxht, ltmxsize to be max
#            values for trees with the largest sizen; formerly these values
#            were taken from whatever row was first in the subset of rows of
#            these largest trees (which relies on the accident of ordering
#            when there is more than one such tree at a site).  The value of
#            ltmxspp still relies on this ordering for now, until a better
#            definition comes along.  Until that time, this metric can vary
#            for each calculation.  Unit test updated to reflect this change,
#            as the old SAS mets were wrong.
#   02/11/11 cws: ltmxspp was misnamed ltmxdbh; now is fixed.  A bug in the unit
#            test prevented this from being seen.  Modified dataframe of
#            expected values for ltmxdbh at EPA01-0159.
#   07/31/12 tmk: Removed calls to the require() function.  Added argument tbl
#            to the function to identify name of the data file.  Added argument
#            NRSAdir to the function to identify the directory from which
#            metrics files are read and to which the output metrics file is
#            written.
#   12/20/12 tmk: Modified data input to use a data frame containing the data
#            file rather than a csv file.  Modified output to be a data frame
#            rather than a csv file.  Removed RUnit functions.
#   01/11/13 tmk: Inserted code to convert factors in the input data frame to
#            character variables.
#   01/24/13 cws: Changed use of reshape::cast to reshape2::dcast.  Requiring
#            reshape2 instead of reshape and calling dcast instead of cast; no
#            other changes made.
#   12/11/15 cws created from metsLegacyTree.r with updated calling interface.
#    3/01/16 cws Documenting arguments in comments at top.  Removed old code.
#            Fixed stupid code documentation error that confused DBH with HEIGHT
#            class values.
#    3/16/16 cws removed old UID name from comments
#
# ARGUMENTS:
# dbhClass      dataframe containing dbh class values at each transect of all 
#               sites.  Expected to contain the columns
#                   SITE        integer or character specifying the site visit
#                   TRANSECT    character value specifying the transect
#                               for which the value was recorded.
#                   VALUE       character values specifying the dbh class,
#                               expected to be one of '0-0.1', '.1-.3',
#                               '.3-.75', '.75-2' or '>2'
#
# distance      dataframe containing distance to largest legacy tree at each 
#               transect of all sites.  Expected to contain the columns
#                   SITE        integer or character specifying the site visit
#                   TRANSECT    character value specifying the transect
#                               for which the value was recorded.
#                   VALUE       numeric values specifying the distance value
#
# heightClass   dataframe containing height class values at each transect of all 
#               sites.  Expected to contain the columns
#                   SITE        integer or character specifying the site visit
#                   TRANSECT    character value specifying the transect
#                               for which the value was recorded.
#                   VALUE       character values specifying the dbh class,
#                               expected to be one of '<5', '5-15', '15-30'
#                               or '>30'
#
# species       dataframe containing names of largest legacy tree taxa at each 
#               transect of all sites.  Expected to contain the columns
#                   SITE        integer or character specifying the site visit
#                   TRANSECT    character value specifying the transect
#                               for which the value was recorded.
#                   VALUE       character values specifying the dbh class,
#                               expected to be one of '0-0.1', '.1-.3',
#                               '.3-.75', '.75-2' or '>2'
#
# type          dataframe containing types of largest legacy tree at each 
#               transect of all sites.  Expected to contain the columns
#                   SITE        integer or character specifying the site visit
#                   TRANSECT    character value specifying the transect
#                               for which the value was recorded.
#                   VALUE       character values, either Coniferous or Deciduous.
#
# Output:
#   Either a data frame when metric calculation is successful or a character
#   string containing an error message when metric calculation is not
#   successful.  The data frame contains the following columns:
#     SITE - universal ID value
#     METRIC - metric name
#     RESULT - metric value
# Other Functions Required:
#   intermediateMessage - print messages generated by the metric calculation
#      functions
################################################################################

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

        args <- list(...)
        pName <- args[[1]]
        rc <- df %>% select(SITE,TRANSECT,VALUE) %>% mutate(PARAMETER=pName)
        return(rc)
    }

    # Just recreate old argument from new arguments for now, rip out the  guts later.
    invasivelegacy <- rbind(absentAsNULL(dbhClass, ifdf, 'DBH')
                           ,absentAsNULL(distance, ifdf, 'DISTANCE')
                           ,absentAsNULL(heightClass, ifdf, 'HEIGHT')
                           ,absentAsNULL(species, ifdf, 'SPECIES')
                           ,absentAsNULL(type, ifdf, 'TREE_TYP')
                           )
    if(is.null(invasivelegacy)) return(NULL)

##  Renaming PARAMETER and RESULT to variable and value
df1 <-rename(invasivelegacy, c('PARAMETER', 'VALUE'), c('variable', 'value'))

# Casting (transforming) data using reshape2 package
lt <- dcast(df1, SITE + TRANSECT ~ variable)
lt$DBH      <- as.character(lt$DBH)
#lt$NOT_VIS  <- as.character(lt$NOT_VIS)
lt$SPECIES  <- as.character(lt$SPECIES)
lt$TREE_TYP <- as.character(lt$TREE_TYP)
lt$HEIGHT   <- as.character(lt$HEIGHT)
lt$DISTANCE <- as.character(lt$DISTANCE)
lt$DISTANCE <- as.numeric(lt$DISTANCE)

lt$METRIC <- ''
lt$numdbh  <- NA 
lt$numht   <- NA 
lt$size    <- ''
lt$sizenum <- NA
lt$sizen   <- NA 

#  Creating values of DBH and HEIGHT with simpler codes
#  newDBH: 0-0.1 = 1; .1-.3 = 2; .3-.75 = 3; .75-2=4; >2=5
#  newHEIGHT: <5 = 1; 5-15 = 2; 15-30 = 3; >30 = 4
## Matrices for tree size
##
##  Determining sizen
##                        HEIGHT
##              <5     5-15  15-30   >30
##   DBH
##     0-0.1     1      2      3      4
##
##   0.1-0.3     2      3      4      5
##
##  0.3-0.75     3      4      5      6
##
##    0.75-2     4      5      6      7
##
##        >2     5      6      7      8
##
##
##  Determining size
##                        HEIGHT
##              <5     5-15  15-30   >30
##   DBH
##     0-0.1     S      S      M      M
##
##   0.1-0.3     S      M      M      M
##
##  0.3-0.75     M      M      L      L
##
##    0.75-2     L      L      L      X
##
##        >2     L      X      X      X
##

lt$numdbh <- ifelse(lt$DBH=='0-0.1', 1, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='.1-.3', 2, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='.3-.75', 3, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='.75-2', 4, lt$numdbh)
lt$numdbh <- ifelse(lt$DBH=='>2', 5, lt$numdbh)

lt$numht <- ifelse(lt$HEIGHT=='<5', 1, lt$numht)
lt$numht <- ifelse(lt$HEIGHT=='5-15', 2, lt$numht)
lt$numht <- ifelse(lt$HEIGHT=='15-30', 3, lt$numht)
lt$numht <- ifelse(lt$HEIGHT=='>30', 4, lt$numht)

lt$size <- ifelse(lt$DBH=='0-0.1' & (lt$HEIGHT=='<5' | lt$HEIGHT=='5-15'), 'S', lt$size)
lt$size <- ifelse(lt$DBH=='.1-.3' & lt$HEIGHT=='<5', 'S', lt$size)
lt$size <- ifelse(lt$DBH=='0-0.1' & (lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'M', lt$size)
lt$size <- ifelse(lt$DBH=='.1-.3' & (lt$HEIGHT=='5-15' | lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'M', lt$size)
lt$size <- ifelse(lt$DBH=='.3-.75' & (lt$HEIGHT=='<5' | lt$HEIGHT=='5-15'), 'M', lt$size)
lt$size <- ifelse(lt$DBH=='.3-.75' & (lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'L', lt$size)
lt$size <- ifelse(lt$DBH=='.75-2' & (lt$HEIGHT=='<5' | lt$HEIGHT=='5-15' | lt$HEIGHT=='15-30'), 'L', lt$size)
lt$size <- ifelse(lt$DBH=='>2' & lt$HEIGHT=='5-15', 'L', lt$size)
lt$size <- ifelse(lt$DBH=='.75-2' & (lt$HEIGHT=='>30'), 'X', lt$size)
lt$size <- ifelse(lt$DBH=='>2' & (lt$HEIGHT=='5-15' | lt$HEIGHT=='15-30' | lt$HEIGHT=='>30'), 'X', lt$size)

lt$sizenum <- ifelse(lt$size=='S', 1, lt$sizenum)
lt$sizenum <- ifelse(lt$size=='M', 2, lt$sizenum)
lt$sizenum <- ifelse(lt$size=='L', 3, lt$sizenum)
lt$sizenum <- ifelse(lt$size=='X', 4, lt$sizenum)
 
lt$sizen <- ifelse(lt$DBH=='0-0.1' & lt$HEIGHT=='<5', 1, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='0-0.1' & lt$HEIGHT=='5-15')
                 | (lt$DBH=='.1-.3' & lt$HEIGHT=='<5'), 2, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='0-0.1' & lt$HEIGHT=='15-30')
                 | (lt$DBH=='.1-.3' & lt$HEIGHT=='5-15')
                 | (lt$DBH=='.3-.75' & lt$HEIGHT=='<5'), 3, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='0-0.1' & lt$HEIGHT=='>30')
                 | (lt$DBH=='.1-.3' & lt$HEIGHT=='15-30') 
                 | (lt$DBH=='.3-.75' & lt$HEIGHT=='5-15') 
                 | (lt$DBH=='.75-2' & lt$HEIGHT=='<5'), 4, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='.1-.3' & lt$HEIGHT=='>30')
                 | (lt$DBH=='.3-.75' & lt$HEIGHT=='15-30') 
                 | (lt$DBH=='.75-2' & lt$HEIGHT=='5-15') 
                 | (lt$DBH=='>2' & lt$HEIGHT=='<5'), 5, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='.3-.75' & lt$HEIGHT=='>30') 
                 | (lt$DBH=='.75-2' & lt$HEIGHT=='15-30') 
                 | (lt$DBH=='>2' & lt$HEIGHT=='5-15'), 6, lt$sizen)
lt$sizen <- ifelse((lt$DBH=='.75-2' & lt$HEIGHT=='>30')
                 | (lt$DBH=='>2' & lt$HEIGHT=='15-30'), 7, lt$sizen)
lt$sizen <- ifelse(lt$DBH=='>2' & lt$HEIGHT=='>30', 8, lt$sizen)

###  Assigning taxonomic category based on crew entry
lt <- assignTaxCat(lt)

###  Determining largest tree
#  Getting max of sizen
aa <- subset(lt, !(is.na(sizen)), select=c('SITE','sizen'))
##  This is where the warnings occur if there are no non-missing arguments to max 
bb <- aggregate(list(maxSizen=aa$sizen), list(SITE=aa$SITE), max)
cc <- subset(merge(bb, lt), maxSizen==sizen)


ltmxdbh  <- transform(aggregate(list(x=cc$numdbh), list(SITE=cc$SITE), max, na.rm=TRUE)
                     ,METRIC='ltmxdbh'
                     ,VALUE= ifelse(x==1, '0-0.1',
                              ifelse(x==2, '.1-.3',
                              ifelse(x==3, '.3-.75',
                              ifelse(x==4, '.75-2',
                              ifelse(x==5, '>2',    NA
                              )))))
                     ,x= NULL
                     ,stringsAsFactors=FALSE
                     )
ltmxht   <- transform(aggregate(list(x=cc$numht), list(SITE=cc$SITE), max, na.rm=TRUE)
                     ,METRIC='ltmxht'
                     ,VALUE= ifelse(x==1, '<5',
                              ifelse(x==2, '5-15',
                              ifelse(x==3, '15-30',
                              ifelse(x==4, '>30',   NA
                              ))))
                     ,x= NULL
                     ,stringsAsFactors=FALSE
                     )
ltmxsize <- transform(aggregate(list(x=cc$sizenum), list(SITE=cc$SITE), max, na.rm=TRUE)
                     ,METRIC='ltmxsize'
                     ,VALUE= ifelse(x==1, 'S',
                              ifelse(x==2, 'M',
                              ifelse(x==3, 'L',
                              ifelse(x==4, 'X',   NA
                              ))))
                     ,x= NULL
                     ,stringsAsFactors=FALSE
                     )
ltmxdist <- transform(aggregate(list(VALUE=cc$DISTANCE), list(SITE=cc$SITE)
                               ,function(x) {
                                    # max, or NA if largest trees have no distance
                                    if(all(is.na(x))) {
                                        return(NA)
                                    } else {
                                        return(max(x, na.rm=TRUE))
                                    }
                                }
                               )
                     ,METRIC='ltmxdist'
                     ,stringsAsFactors=FALSE
                     )
ltmxcnt <- transform(aggregate(list(VALUE=cc$sizen), list(SITE=cc$SITE), count)
                    ,METRIC='ltmxcnt'
                    ,stringsAsFactors=FALSE
                    )
ltmxspp <- transform(subset(first(cc, 'SITE', 'first.SITE')
                           ,first.SITE==TRUE
                           ,select=c(SITE,taxCat)
                           )
                    ,METRIC='ltmxspp'
                    ,VALUE=taxCat
                    ,taxCat=NULL
                    ,stringsAsFactors=FALSE
                    )

#### Determining median tree size and related metrics
# ltfracl - Legacy fraction of trees >= large
# ltfracm - Legacy fraction of trees >= medium
# ltfracs - Legacy fraction of trees >= small
# ltfracx - Legacy fraction of trees >= xlarge
# ltmddist - Legacy mean dist of trees >= median size

## NOTE:  SAS code used median DBH as median tree size.
## NOTE:  SAS code created counts for ltfracs, ltfracm, ltfracl, ltfracx.

## ltmddist
aa <- subset(aggregate(list(medianSize=lt$numdbh), list(SITE=lt$SITE),
             na.rm = T, median), !(is.na(medianSize)))
lt <- merge(lt, aa, all.x=T)
lt$medianDistance <- ifelse(lt$numdbh>=lt$medianSize, lt$DISTANCE, NA)
ltmddist <- aggregate(list(ltmddist=lt$medianDistance),
               list(SITE=lt$SITE,METRIC=lt$METRIC), mean, na.rm = T)
  ltmddist$METRIC <- 'ltmddist'
  ltmddist <- rename(ltmddist, 'ltmddist', 'VALUE')

## fractions of trees by size
# cntS - count of trees >= small -> s, m, l, x
# cntM - count of trees >= medium -> m, l, x
# cntL - count of trees >= large -> l, x
# cntX - count of trees >= xlarge  -> x

## Creating count of transects by SITE
tranCnt <- subset(aggregate(list(tranCnt=lt$TRANSECT), by=list(SITE=lt$SITE), count), select=c('SITE', 'tranCnt'))

sm <- aggregate(list(smCnt=subset(lt$size, lt$size %in% c('S','M','L','X'))),
        list(SITE=subset(lt$SITE,lt$size %in% c('S','M','L','X'))), count)
med <- aggregate(list(medCnt=subset(lt$size, lt$size %in% c('M','L','X'))),
        list(SITE=subset(lt$SITE,lt$size %in% c('M','L','X'))), count)
lg <- aggregate(list(lgCnt=subset(lt$size, lt$size %in% c('L','X'))),
        list(SITE=subset(lt$SITE,lt$size %in% c('L','X'))), count)
xl <- aggregate(list(xlCnt=subset(lt$size, lt$size %in% c('X'))),
        list(SITE=subset(lt$SITE,lt$size %in% c('X'))), count)

aa <- merge(sm, med, all=T)
bb <- merge(aa, lg, all=T)
cc <- merge(bb, xl, all=T)
treeCounts <- merge(tranCnt, cc, all=T)
treeCounts$ltfracs <- treeCounts$smCnt/treeCounts$tranCnt
treeCounts$ltfracm <- treeCounts$medCnt/treeCounts$tranCnt
treeCounts$ltfracl <- treeCounts$lgCnt/treeCounts$tranCnt
treeCounts$ltfracx <- treeCounts$xlCnt/treeCounts$tranCnt

treeCounts$ltfracs <- ifelse(is.na(treeCounts$ltfracs), 0, treeCounts$ltfracs)
treeCounts$ltfracm <- ifelse(is.na(treeCounts$ltfracm), 0, treeCounts$ltfracm)
treeCounts$ltfracl <- ifelse(is.na(treeCounts$ltfracl), 0, treeCounts$ltfracl)
treeCounts$ltfracx <- ifelse(is.na(treeCounts$ltfracx), 0, treeCounts$ltfracx)

treeCounts$METRIC <- ''

ltfracs <- subset(treeCounts, select=c('SITE','METRIC','ltfracs'))
  ltfracs$METRIC <- 'ltfracs'
  ltfracs <- rename(ltfracs,'ltfracs','VALUE')
ltfracm <- subset(treeCounts, select=c('SITE','METRIC','ltfracm'))
  ltfracm$METRIC <- 'ltfracm'
  ltfracm <- rename(ltfracm,'ltfracm','VALUE')
ltfracl <- subset(treeCounts, select=c('SITE','METRIC','ltfracl'))
  ltfracl$METRIC <- 'ltfracl'
  ltfracl <- rename(ltfracl,'ltfracl','VALUE')
ltfracx <- subset(treeCounts, select=c('SITE','METRIC','ltfracx'))
  ltfracx$METRIC <- 'ltfracx'
  ltfracx <- rename(ltfracx,'ltfracx','VALUE')

### And now for the piece de resistance - the list of dominant and subdominant species
# ltsplist - List of all speicies present
# ltmddom - Species occurring most commonly
# ltmddomn - Number of times dominant speices occurs
# ltmdsub - Second most commonly occurring species
# ltmdsubn - Number of tims subdominant species occurs

## NOTE:  SAS code miscounts ltmddomn and ltmdsubn by one tree in each metric.
                     
##  Creating ltmddom and ltmdsub
counts <- aggregate(list(count=lt$taxCat), list('SITE'=lt$SITE,'METRIC'=lt$METRIC, 'maxTaxCat'=lt$taxCat), count)
ltmddomn <- aggregate(list('ltmddomn'=counts$count), list('SITE'=counts$SITE, METRIC=counts$METRIC), max)
  ltmddomn$METRIC <- 'ltmddomn'
  ltmddomn <- rename(ltmddomn,'ltmddomn','VALUE')
  
aa <- subset(merge(counts, subset(ltmddomn,select=c('SITE','VALUE')), by='SITE'), count==VALUE)
ltmddom <- aggregate(list('ltmddom'=aa$maxTaxCat)
                        ,list('SITE'=aa$SITE,'METRIC'=aa$METRIC)
                        ,function(x) { paste(x, collapse=',') } )
  ltmddom$METRIC <- 'ltmddom'
  ltmddom <- rename(ltmddom,'ltmddom','VALUE')
                     
bb <- subset(merge(counts, subset(ltmddomn, select=c('SITE','VALUE')),
        by='SITE'), count!=VALUE, select=c('SITE','METRIC','maxTaxCat','count'))                      
ltmdsubn <- aggregate(list('ltmdsubn'=bb$count), list('SITE'=bb$SITE, METRIC=bb$METRIC), max)
  ltmdsubn$METRIC <- 'ltmdsubn'
  ltmdsubn <- rename(ltmdsubn,'ltmdsubn','VALUE')
cc <- subset(merge(counts, subset(ltmdsubn,select=c('SITE','VALUE')), by='SITE'), count==VALUE)
ltmdsub <- aggregate(list('ltmdsub'=cc$maxTaxCat)
                        ,list('SITE'=cc$SITE,'METRIC'=cc$METRIC)
                        ,function(x) { paste(x, collapse=',')})
  ltmdsub$METRIC <- 'ltmdsub'
  ltmdsub <- rename(ltmdsub,'ltmdsub','VALUE')

##  List of all trees
hasTrees <- subset(lt, !(is.na(SPECIES)))                   
ltsplist <- aggregate(list('ltsplist'=hasTrees$SPECIES)
                        ,list('SITE'=hasTrees$SITE,'METRIC'=hasTrees$METRIC) 
                        ,function(x) { paste(x, collapse=',')})
  ltsplist$METRIC <- 'ltsplist'
  ltsplist <- rename(ltsplist,'ltsplist','VALUE')


####  Woo hoo! Let's rbind these puppies and put 'em to bed
mhtrees <- rbind(ltmxdbh,ltmxht,ltmxspp,ltmxsize,ltmxdist,ltmxcnt,ltfracs,ltfracm,
                 ltfracl,ltfracx,ltmddist,ltsplist,ltmddom,ltmddomn,ltmdsub,
                 ltmdsubn)

}



# end of file
