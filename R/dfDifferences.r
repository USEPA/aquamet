# dfDifferences.r
#
#  6/09/23 cws Moved here from sharedSupport.r. Reorganized unit test for
#          readability. Using all_of() around variables specifying column names
#          as needed.
#  6/28/23 cws Comparing text values as numeric if both are numeric. This is to
#          facillitate comparison of 'long format' data where
#          values in a column may be text (e.g. taxa) and numbers (w.g. lengths, 
#          counts). Unit test NOT extended to test this.
#  7/10/23 cws Modified comparison of characters as numbers to account for values
#          such as NA, NaN, inf.
#  1/10/24 cws Added zeroish argument to replace hardcoded value. Modified unit
#          test to include 'zeroish' classification in output. Made classification
#          of numeric values stored in character columns the same as when stored 
#          in numeric columns. Prettyprinted. Corrected difference checking of
#          infinite numeric values when expressed in character columns. Added
#          sameABS value of type to indicate only difference in numbers is the 
#          sign.
#  2/19/24 cws Using all_of() within select() in compareText() when selecting 
#          columns using character strings instead of unquoted column names.
#  2/22/24 cws Using suppressWarnings() to address warnings due to attempting to
#          treat character values as if numeric.
#  4/06/24 cws Corrected classNumber to class number as zero if it was within 
#          zeroFudge of zero. Modified classCharacter to use classNumber to 
#          further classify numeric value instead of just 'number'. Also continued
#          change on 2/19/24 to use all_of() around thisVar in remaining calls 
#          to select(). No change to unit test.
#  4/24/24 cws Changed default class for numeric value to 'number' instead of 
#          'genl'. Updated unit test accordingly
#  4/26/24 cws Modifications based on extension of unit test which attempts to
#          use combinations of values to exercise each possible type and type2.
#          Using absentValue to fill in absent values when a tuple occurs in only
#          one of the dataframes being compared.
#  4/29/24 cws Removed unit test functions as duplicating those in the separate 
#          file in aquamet
#
require(plyr) # for pipe - %>%
require(tidyr)
require(Hmisc) # for %nin%

dfDifferences <- function(df1, df2, byVars
                         ,zeroFudge=1e-17
                         ,absentValue = '<absent>'
                         ,zeroish = 1e-6
                         )
# Used to find and categorize differences in data.frames. Returns data.frame
# of differences
#
# ARGUMENTS:
# df1, df2    Dataframes to compare.  These must share key variables.
# byVars      character vector of Key variable name(s) which uniquely identify 
#               specific rows in each dataframe.
# zeroFudge   Largest difference between numeric values that is regarded as zero,
#             but are not due to floating point issues.
# absentValue character value to place in columns 'first' or 'second' when a
#             column is absent in the first or second dataframe arguments
# zeroish     Small numeric value > 0 that is used to classify numbers with
#             absolute values which are suspiciously close to zero. If the
#             absolute value of a number is less than this value, it gets 
#             classed as 'zeroish'.
#
{
    # define constants
    altNumericStrings <- c('NA','inf','-inf','+inf','nan','NaN') # strings that can be numbers
    
    # sanity checks
    if(!all(byVars %in% names(df1))) return("df1 does not contain all of the by-variables")
    if(!all(byVars %in% names(df2))) return("df2 does not contain all of the by-variables")
    if(setequal(byVars, names(df1))) return("df1 contains only by-variables")
    if(setequal(byVars, names(df2))) return("df2 contains only by-variables")
    
    # setup comparisons
    only1Vars <- setdiff(names(df1), names(df2))
    only2Vars <- setdiff(names(df2), names(df1))
    compareVars <- intersect(names(df1), names(df2)) %>% setdiff(byVars)
    allDiffs <- NULL
    
    classCharacter <- function(cc) {
        # classify individual character strings
        rc <- ifelse(cc %in% '',         'blank'
             ,ifelse(trimws(cc) %in% '', 'spaces'
             ,ifelse(is.number(cc),      classNumber(suppressWarnings(as.numeric(cc))) # was 'number'
             ,ifelse(is.na(cc),          'na'
                                        ,'genl'
              ))))
        return(rc)
    }
    
    classNumber <- function(vv) {
        # classify individual numeric vvalues. Assumes vv is numeric type
        rc <- ifelse(is.infinite(vv) & sign(vv) == 1,  '+inf'
             ,ifelse(is.infinite(vv) & sign(vv) == -1, '-inf'
             ,ifelse(is.nan(vv),                       'nan'      
             ,ifelse(is.na(vv) & !is.nan(vv),          'na'
             ,ifelse(abs(vv) <= zeroFudge,             'zero'
             ,ifelse(abs(vv) < zeroish,                'zeroish'
                                                      ,'number' #'genl'       
              ))))))
        return(rc)
    }
    
    compareNumbers <- function(dd) {
        # compare values as numbers
        rc <- dd %>%
              mutate(first =    as.numeric(first)
                    ,second =   as.numeric(second)
                    ,classFirst = classNumber(first)
                    ,classSecond = classNumber(second)
                    ,diff =     second - first
                    ,type =     ifelse(inFirst & !inSecond,                   'only1'
                               ,ifelse(!inFirst & inSecond,                   'only2'
                               ,ifelse(inFirst & inSecond,  
                                       ifelse(is.na(first) & is.na(second),   'same'
                                      ,ifelse(is.nan(first) & is.nan(second), 'same'
                                      ,ifelse(is.infinite(first) &
                                              is.infinite(second) & 
                                              (sign(first) == sign(second)),  'same'
                                       ,ifelse(is.infinite(first) &
                                              is.infinite(second) & 
                                              (sign(first) != sign(second)),  'sameABS'
                                     ,ifelse(!is.na(diff) & 
                                              abs(diff) <= zeroFudge,         'same'
                                      ,ifelse(!is.na(diff) & 
                                              abs(abs(first) - abs(second)) <= zeroFudge, 'sameABS'
                                                                             ,'diff'
                                       ))))))
                               ,ifelse(!inFirst & !inSecond,                  'impossible'
                                                                             ,'beyondImpossible'
                                ))))
                    ,type2 =    ifelse(type == 'same',  paste('same', classFirst, sep=':')
                               ,ifelse(type == 'only1', paste('only1', classFirst, sep=':')
                               ,ifelse(type == 'only2', paste('only2', classSecond, sep=':')
                               ,ifelse(type == 'diff',  paste(classFirst, classSecond, sep=':')
                                                       ,type
                                ))))
                    ) %>%
              select(all_of(byVars), column, first, second, diff, type, type2)           
        return(rc)
    }
    
    compareText <- function(dd) {
        # compare character values. If both character strings that can be
        # interpreted as numeric values, then cast them to numeric and compare
        # them as such.
        rc <- dd %>%
              mutate(classFirst  = classCharacter(first)
                    ,classSecond = classCharacter(second)
                    ,compareAsNumbers = (classFirst %in% c('number','zero','zeroish')  | first %in% altNumericStrings) &
                                        (classSecond %in% c('number','zero','zeroish') | second %in% altNumericStrings)
                    ,diff =    ifelse(compareAsNumbers
                                     ,suppressWarnings( as.numeric(second) -  as.numeric(first) )
                                     ,suppressWarnings( as.numeric(second != first) )
                                     )
                    ,absDiff = ifelse(compareAsNumbers
                                     ,suppressWarnings( abs(as.numeric(second)) - abs(as.numeric(first)) )
                                     ,suppressWarnings( as.numeric(second != first) )
                                     )
                    ,sameAbs = as.logical(second == first) 
                    ,sameci =  !sameAbs & as.logical(toupper(second) == toupper(first))
                    ,samesi =  !sameAbs & as.logical(trimws(second) == trimws(first))
                    ,samecsi = !sameAbs & !sameci & !samesi & 
                               as.logical(trimws(toupper(second)) == trimws(toupper(first)))
                    ,type =     ifelse(inFirst & !inSecond,                        'only1'
                               ,ifelse(!inFirst & inSecond,                        'only2'
                               ,ifelse(inFirst & inSecond,  
                                       # compare as characters if appropriate
                                       ifelse(is.na(first) & is.na(second),        'same'
                                      ,ifelse(sameAbs %in% TRUE,                   'same'
                                      ,ifelse(samecsi %in% TRUE,                   'sameCSI'
                                      ,ifelse(sameci %in% TRUE,                    'sameCI'
                                      ,ifelse(samesi %in% TRUE,                    'sameSI'
                                      ,ifelse(compareAsNumbers,
                                              # compare as numeric values
                                              ifelse(first %in% c(NA,'NA') & 
                                                     second %in% c(NA,'NA'),       'same'
                                             ,ifelse(first %in% c('nan','NaN') & 
                                                     second %in% c('nan','NaN'),   'same'
                                             ,ifelse(first %in% 'inf' &
                                                     second %in% 'inf' | 
                                                     first %in% '+inf' &
                                                     second %in% '+inf' | 
                                                     first %in% '-inf' &
                                                     second %in% '-inf',           'same'
                                             ,ifelse(grepl('inf', first) &
                                                     grepl('inf', second),         'sameABS'
                                             ,ifelse(!is.na(diff) & 
                                                     compareAsNumbers &
                                                     suppressWarnings( abs(diff) <= zeroFudge )
                                                    ,                              'same'
                                             ,ifelse(!is.na(absDiff) & 
                                                     compareAsNumbers &
                                                     suppressWarnings( abs(absDiff) <= zeroFudge ),    'sameABS'
                                                                                  ,'diff'
                                              ))))) )
                                             ,'diff'
                                       ))))))
                               ,ifelse(!inFirst & !inSecond, 'absent', 'bigError'
                                ))))
                    ,type2 =    ifelse(type == 'same',    paste('same', classFirst, sep=':')
                               ,ifelse(type == 'sameCI',  paste('sameCI', classFirst, sep=':')
                               ,ifelse(type == 'sameSI',  paste('sameSI', classFirst, sep=':')
                               ,ifelse(type == 'sameCSI', paste('sameCSI', classFirst, sep=':')
                               ,ifelse(type == 'sameABS', paste('sameABS', classFirst, sep=':')
                               ,ifelse(type == 'only1',   paste('only1', classFirst, sep=':')
                               ,ifelse(type == 'only2',   paste('only2', classSecond, sep=':')
                               ,ifelse(type == 'diff',    paste(classFirst, classSecond, sep=':')
                                      ,type
                                ))))))))
                    ) %>%
              select(all_of(byVars), column, first, second, diff, type, type2)           
              
    }
    
    for(thisVar in sort(c(only1Vars, only2Vars, compareVars))) {
        if(thisVar %in% only1Vars)
            thisComparison <- df1[c(byVars, thisVar)] %>%
                              dplyr::rename(first=all_of(thisVar)) %>%
                              mutate(second = absentValue
                                    ,column = thisVar
                                    ,diff = absentValue
                                    ,type = absentValue
                                    ,type2 = absentValue
                                    ) %>%
                              select(all_of(byVars), column, first, second, diff, type, type2)
        else if (thisVar %in% only2Vars) {
            thisComparison <- df2[c(byVars, thisVar)] %>%
                              dplyr::rename(second=all_of(thisVar)) %>%
                              mutate(first = absentValue
                                    ,column = thisVar
                                    ,diff = absentValue
                                    ,type = absentValue
                                    ,type2 = absentValue
                                    ) %>%
                              select(all_of(byVars), column, first, second, diff, type, type2)
        } else {
            bothValues <- merge(df1 %>% select(all_of(byVars), all_of(thisVar)) %>%
                                dplyr::rename(first=all_of(thisVar)) %>%
                                mutate(inFirst=TRUE)
                               ,df2 %>% select(all_of(byVars), all_of(thisVar)) %>%
                                dplyr::rename(second=all_of(thisVar)) %>%
                                mutate(inSecond=TRUE)
                               ,by = byVars
                               ,all = TRUE
                               ) %>%
                          mutate(inFirst =  inFirst %in% TRUE
                                ,inSecond = inSecond %in% TRUE
                                ,column = thisVar
                                ) %>%
                          mutate(first = ifelse(!inFirst & inSecond, absentValue, first)
                                ,second = ifelse(inFirst & !inSecond, absentValue, second)
                                )
            if (all(is.number(df1[[thisVar]]) | is.na(df1[[thisVar]]) | is.nan(df1[[thisVar]]) | is.infinite(df1[[thisVar]])) & 
                all(is.number(df2[[thisVar]]) | is.na(df2[[thisVar]]) | is.nan(df2[[thisVar]]) | is.infinite(df2[[thisVar]]))
               ) {
                thisComparison <- compareNumbers(bothValues)
            }else {
                thisComparison <- compareText(bothValues)
            }
        }
        
        if(is.null(allDiffs))
            allDiffs <- thisComparison
        else
            allDiffs <- rbind(allDiffs
                             ,thisComparison %>% select(all_of(names(allDiffs)))
                             )
    }
    
    allDiffs <- allDiffs %>% 
                arrange(across(all_of(byVars)), column) %>% 
                data.frame()
    return(allDiffs)        
}

# end of file