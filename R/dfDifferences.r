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
# 12/19/23 cws Copied to aquamet for use with a variety of unit tests. Unit test 
#          separated to file in aquamet/unitTests/SharedCode/dfDifferencesTest.r
#

dfDifferences <- function(df1, df2, byVars, zeroFudge=1e-17
                         ,absentValue='<absent>'
                         )
# Used to find and categorize differences in data.frames. Returns data.frame
# of differences
#
# ARGUMENTS:
# df1, df2    Dataframes to compare.  These must share key variables.
# byVars      character vector of Key variable name(s) which uniquely identify 
#               specific rows in each dataframe.
# zeroFudge   Largest difference between numeric values that is regarded as zero.
# absentValue character value to place in columns 'first' or 'second' when a
#             column is absent in the first or second dataframe arguments
#
{
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
        rc <- ifelse(cc %in% '', 'blank'
             ,ifelse(trimws(cc) %in% '', 'spaces'
             ,ifelse(is.number(cc), 'number'
             ,ifelse(is.na(cc), 'na', 'genl'
              ))))
        return(rc)
    }
    classNumber <- function(vv) {
        rc <- ifelse(is.infinite(vv) & sign(vv) == 1,  '+inf'
             ,ifelse(is.infinite(vv) & sign(vv) == -1, '-inf'
             ,ifelse(is.nan(vv),                       'nan'      
             ,ifelse(is.na(vv) & !is.nan(vv),          'na'
             ,ifelse(vv == 0,                          'zero'
             ,ifelse(abs(vv) < 1e-6,                   'zeroish'
                                                      ,'genl'       
              ))))))
        return(rc)
    }
    compareNumbers <- function(dd) {
        rc <- dd %>%
              mutate(first =    as.numeric(first)
                    ,second =   as.numeric(second)
                    ,classFirst = classNumber(first)
                    ,classSecond = classNumber(second)
                    ,diff =     second - first
                    #,reldiff = diff / first
                    #,diffClass = cut(diff, breaks=c(-10^c(6,3,2,1,0,-1,-3,-6),0,10^c(-6,-3,-1,0,1,2,3,6)))
                    #,reldiffClass = cut(reldiff, breaks=c(-10^c(6,3,2,1,0,-1,-3,-6),0,10^c(-6,-3,-1,0,1,2,3,6)))
                    ,type =     ifelse(inFirst & !inSecond, 'only1'
                               ,ifelse(!inFirst & inSecond, 'only2'
                               ,ifelse(inFirst & inSecond,  
                                       ifelse(is.na(first) & is.na(second), 'same'
                                      ,ifelse(is.nan(first) & is.nan(second), 'same'
                                      ,ifelse(is.infinite(first) & is.infinite(second) & 
                                              (sign(first) == sign(second)) , 'same'
                                      ,ifelse(!is.na(diff) & abs(diff) <= zeroFudge, 'same', 'diff'
                                       ))))
                               ,ifelse(!inFirst & !inSecond, 'absent', 'bigError'
                                ))))
                    ,type2 =    ifelse(type == 'same', paste('same', classFirst, sep=':')
                               ,ifelse(type == 'only1', paste('only1', classFirst, sep=':')
                               ,ifelse(type == 'only2', paste('only2', classSecond, sep=':')
                               ,ifelse(type == 'diff', paste(classFirst, classSecond, sep=':')
                                      ,type
                                ))))
                    ) %>%
              select(all_of(byVars), column, first, second, diff, type, type2)           
        return(rc)
    }
    compareText <- function(dd) {
        rc <- dd %>%
              mutate(classFirst  = classCharacter(first)
                    ,classSecond = classCharacter(second)
                    ,compareAsNumbers = (classFirst=='number'  | first %in% c('NA','inf','-inf','+inf','nan','NaN')) &
                                        (classSecond=='number' | second %in% c('NA','inf','-inf','+inf','nan','NaN'))
                    ,diff =    ifelse(compareAsNumbers
                                     ,as.numeric(second) -  as.numeric(first)
                                     ,as.numeric(second != first)
                                     )
                    ,sameAbs = as.logical(second == first) #& is.na(second) == is.na(first)
                    ,sameci =  !sameAbs & as.logical(toupper(second) == toupper(first))
                    ,samesi =  !sameAbs & as.logical(trimws(second) == trimws(first))
                    ,samecsi = !sameAbs & !sameci & !samesi & as.logical(trimws(toupper(second)) == trimws(toupper(first)))
                    ,type =     ifelse(inFirst & !inSecond, 'only1'
                               ,ifelse(!inFirst & inSecond, 'only2'
                               ,ifelse(inFirst & inSecond,  
                                       ifelse(is.na(first) & is.na(second), 'same'
                                      ,ifelse(sameAbs %in% TRUE, 'same'
                                      ,ifelse(samecsi %in% TRUE, 'sameCSI'
                                      ,ifelse(sameci %in% TRUE, 'sameCI'
                                      ,ifelse(samesi %in% TRUE, 'sameSI'
                                      # ,ifelse(compareAsNumbers & diff < zeroFudge, 'sameNum'
                                      ,ifelse(compareAsNumbers,
                                              ifelse(first %in% c(NA,'NA') & second %in% c(NA,'NA'), 'sameNum'
                                             ,ifelse(first %in% c('nan','NaN') & second %in% c('nan','NaN'), 'sameNum'
                                             ,ifelse(first %in% c('-inf','+inf') & first %in% c('inf','+inf') | 
                                                     first %in% c('-inf') & first %in% c('inf'), 'sameNum'
                                             ,ifelse(!is.na(diff) & abs(diff) <= zeroFudge, 'sameNum', 'diff'
                                              ))))
                                             ,'diff'
                                       ))))))
                               ,ifelse(!inFirst & !inSecond, 'absent', 'bigError'
                                ))))
                    ,type2 =    ifelse(type == 'same', paste('same', classFirst, sep=':')
                               ,ifelse(type == 'sameCI', paste('sameCI', classFirst, sep=':')
                               ,ifelse(type == 'sameSI', paste('sameSI', classFirst, sep=':')
                               ,ifelse(type == 'sameCSI', paste('sameCSI', classFirst, sep=':')
                               ,ifelse(type == 'only1', paste('only1', classFirst, sep=':')
                               ,ifelse(type == 'only2', paste('only2', classSecond, sep=':')
                               ,ifelse(type == 'diff', paste(classFirst, classSecond, sep=':')
                                      ,type
                                )))))))
                    )%>%
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
            bothValues <- merge(df1 %>% select(all_of(byVars), thisVar) %>%
                                dplyr::rename(first=all_of(thisVar)) %>%
                                mutate(inFirst=TRUE)
                               ,df2 %>% select(all_of(byVars), thisVar) %>%
                                dplyr::rename(second=all_of(thisVar)) %>%
                                mutate(inSecond=TRUE)
                               ,by = byVars
                               #,sufixes = c('__1', '__2')
                               ,all = TRUE
                               ) %>%
                          mutate(inFirst =  inFirst %in% TRUE
                                ,inSecond = inSecond %in% TRUE
                                ,column = thisVar
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
            allDiffs <- rbind(allDiffs, thisComparison %>% select(names(allDiffs)))
    }
    
    allDiffs <- allDiffs %>% 
                arrange(across(all_of(byVars)), column) %>% 
                data.frame()
    return(allDiffs)        
}

# end of file