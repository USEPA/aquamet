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
             ,ifelse(is.number(cc),      'number'
             ,ifelse(is.na(cc),          'na'
                                        ,'genl'
              ))))
        return(rc)
    }
    
    classNumber <- function(vv) {
        # classify individual numeric values
        rc <- ifelse(is.infinite(vv) & sign(vv) == 1,  '+inf'
             ,ifelse(is.infinite(vv) & sign(vv) == -1, '-inf'
             ,ifelse(is.nan(vv),                       'nan'      
             ,ifelse(is.na(vv) & !is.nan(vv),          'na'
             ,ifelse(vv == 0,                          'zero'
             ,ifelse(abs(vv) < zeroish,                'zeroish'
                                                      ,'genl'       
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
        # interpretted as numeric values, then cast them to numeric and compare
        # them as such.
        rc <- dd %>%
              mutate(classFirst  = classCharacter(first)
                    ,classSecond = classCharacter(second)
                    ,compareAsNumbers = (classFirst=='number'  | first %in% altNumericStrings) &
                                        (classSecond=='number' | second %in% altNumericStrings)
                    ,diff =    ifelse(compareAsNumbers
                                     ,as.numeric(second) -  as.numeric(first)
                                     ,as.numeric(second != first)
                                     )
                    ,absdiff = ifelse(compareAsNumbers
                                     ,abs(as.numeric(second)) - abs(as.numeric(first))
                                     ,as.numeric(second != first)
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
                                                     abs(diff) <= zeroFudge,       'same'
                                             ,ifelse(!is.na(absDiff) & 
                                                     abs(absDiff) <= zeroFudge,    'sameABS'
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
            allDiffs <- rbind(allDiffs
                             ,thisComparison %>% select(all_of(names(allDiffs)))
                             )
    }
    
    allDiffs <- allDiffs %>% 
                arrange(across(all_of(byVars)), column) %>% 
                data.frame()
    return(allDiffs)        
}


dfDifferencesTest <- function()
# unit test for dfDifferences
{
    testData <- dfDifferencesTest.baseData()
    
    # test case with identical dataframes
    expected <- dfDifferencesTest.baseDifferences(testData)
    actual <- dfDifferences(testData, testData, c('aa','bb'))
# return(list(e=expected, a=actual))
    checkEquals(expected, actual, "Incorrect when dataframes are identical")
    
    # test case with dataframes with many differences
    tt <- dfDifferencesTest.createDifferencesAndResults(testData)
    altData <- tt$altData
    expected <- tt$expected
    actual <- dfDifferences(testData, altData, c('aa','bb'))
    dd <- dfCompare(expected, actual, c('aa','bb','column'))
#return(list(e=expected, a=actual, dd=dd, d1=testData, d2=altData))
    checkTrue(is.null(dd), "Incorrect values regardless of ordering")
    checkEquals(expected, actual, "Incorrect when dataframes are different")

    # test case with absent byVars
    expected <- "df1 does not contain all of the by-variables"
    actual <- dfDifferences(testData %>% dplyr::rename(aa1=aa), testData, c('aa','bb'))
    checkEquals(expected, actual, "Incorrect when byVars not all in dataframe 1")

    expected <- "df2 does not contain all of the by-variables"
    actual <- dfDifferences(testData, testData %>% dplyr::rename(aa2=aa), c('aa','bb'))
    checkEquals(expected, actual, "Incorrect when byVars not all in dataframe 2")

}

dfDifferencesTest.baseData <- function()
{
    rc <- data.frame(aa = rep(letters[1:5], each=3)
                    ,bb = rep(1:3, times=5)
                    ,cc = LETTERS[1:15]
                    ,rr = runif(15)
                    ,uu = runif(15)
                    ,stringsAsFactors = FALSE
                    ) %>%
          mutate(rr = ifelse(aa=='d' & bb==1,  1/0
                     ,ifelse(aa=='d' & bb==2, -1/0
                     ,ifelse(aa=='d' & bb==3,  0/0
                                            ,  rr
                      )))
                )
    return(rc)
}

dfDifferencesTest.baseDifferences <- function(baseData)
{
    rc <- baseData %>%
          mutate(rr = rr %>% as.character()
                ,uu = uu %>% as.character()
                ) %>%
          pivot_longer(-c('aa','bb'), names_to='column', values_to='value') %>%
          mutate(first = value
                ,second = value
                ,diff = ifelse(column=='cc'
                              ,as.numeric(FALSE)
                              ,suppressWarnings(as.numeric(second) - as.numeric(first))
                              )
                ,type = 'same'
                ,type2 = ifelse(value %in% c('Inf','+Inf'), 'same:+inf'
                        ,ifelse(value %in% '-Inf',          'same:-inf'
                        ,ifelse(value %in% 'NaN',           'same:nan'
                                                           ,'same:genl'
                         )))
                ) %>%
          select(-value) %>%
          arrange(aa, bb, column) %>%
          data.frame()
    return(rc)
}

dfDifferencesTest.createDifferencesAndResults <- function(testData)
{
    altData <- testData %>%
               dplyr::rename(newuu = uu) %>%               # check handling of different columns
               mutate(cc = ifelse (aa=='a' & bb==1, 'a'    # check case sensitivity
                          ,ifelse (aa=='a' & bb==2, '   B' # check whitespace sensitivity
                          ,ifelse (aa=='a' & bb==3, '   c' # check case and whitespace sensitivity
                          ,ifelse (aa=='b' & bb==1, 'DDD'  # check difference
                          ,ifelse (aa=='b' & bb==2, ''     # check blank
                          ,ifelse (aa=='b' & bb==3, NA     # check NA
                                  ,cc
                          ))))))
                     ,rr = ifelse (aa=='a' & bb==1, rr - 3e-18 # check allowance for rounding
                          ,ifelse (aa=='a' & bb==2, rr - 1     # check different number
                          ,ifelse (aa=='a' & bb==3, NA         # check difference with NA
                          ,ifelse (aa=='b' & bb==1, +Inf       # check difference with Inf
                          ,ifelse (aa=='b' & bb==2, -Inf       # check difference with -Inf
                          ,ifelse (aa=='b' & bb==3, NaN        # check difference with NaN
                          ,ifelse (aa=='c' & bb==2, 1e-7       # check difference with zeroish
                                  ,rr
                           )))))))
                     ,aa = ifelse(aa=='c' & bb == 1, 'cc', aa) # check absent rows in each
                     )
    expected <- dfDifferencesTest.baseDifferences(testData) %>%
                mutate(second = ifelse (aa=='a' & bb==1 & column=='cc', subset(altData, aa=='a' & bb==1)$cc
                               ,ifelse (aa=='a' & bb==2 & column=='cc', subset(altData, aa=='a' & bb==2)$cc
                               ,ifelse (aa=='a' & bb==3 & column=='cc', subset(altData, aa=='a' & bb==3)$cc
                               ,ifelse (aa=='b' & bb==1 & column=='cc', subset(altData, aa=='b' & bb==1)$cc
                               ,ifelse (aa=='b' & bb==2 & column=='cc', subset(altData, aa=='b' & bb==2)$cc   
                               ,ifelse (aa=='b' & bb==3 & column=='cc', subset(altData, aa=='b' & bb==3)$cc
                               ,ifelse (aa=='c' & bb==1 & column=='cc', NA
                               ,ifelse (aa=='a' & bb==1 & column=='rr', subset(altData, aa=='a' & bb==1)$rr
                               ,ifelse (aa=='a' & bb==2 & column=='rr', subset(altData, aa=='a' & bb==2)$rr
                               ,ifelse (aa=='a' & bb==3 & column=='rr', subset(altData, aa=='a' & bb==3)$rr
                               ,ifelse (aa=='b' & bb==1 & column=='rr', subset(altData, aa=='b' & bb==1)$rr
                               ,ifelse (aa=='b' & bb==2 & column=='rr', subset(altData, aa=='b' & bb==2)$rr
                               ,ifelse (aa=='b' & bb==3 & column=='rr', subset(altData, aa=='b' & bb==3)$rr
                               ,ifelse (aa=='c' & bb==1 & column=='rr', NA
                               ,ifelse (aa=='c' & bb==2 & column=='rr', subset(altData, aa=='c' & bb==2)$rr
                                       ,second
                                )))))))))))))))
                      ,diff = ifelse (aa=='a' & bb==1 & column=='cc', 1
                             ,ifelse (aa=='a' & bb==2 & column=='cc', 1
                             ,ifelse (aa=='a' & bb==3 & column=='cc', 1
                             ,ifelse (aa=='b' & bb==1 & column=='cc', 1
                             ,ifelse (aa=='b' & bb==2 & column=='cc', 1
                             ,ifelse (aa=='b' & bb==3 & column=='cc', NA
                             ,ifelse (aa=='c' & bb==1 & column=='cc', NA
                             ,ifelse (aa=='a' & bb==1 & column=='rr', subset(altData, aa=='a' & bb==1)$rr - subset(testData, aa=='a' & bb==1)$rr
                             ,ifelse (aa=='a' & bb==2 & column=='rr', -1
                             ,ifelse (aa=='a' & bb==3 & column=='rr', NA
                             ,ifelse (aa=='b' & bb==1 & column=='rr', +Inf
                             ,ifelse (aa=='b' & bb==2 & column=='rr', -Inf
                             ,ifelse (aa=='b' & bb==3 & column=='rr', NaN
                             ,ifelse (aa=='c' & bb==1 & column=='rr', NA
                             ,ifelse (aa=='c' & bb==2 & column=='rr', subset(altData, aa=='c' & bb==2)$rr - subset(testData, aa=='c' & bb==2)$rr
                             ,ifelse (aa=='d' & bb==1 & column=='rr', NaN 
                             ,ifelse (aa=='d' & bb==2 & column=='rr', NaN 
                             ,ifelse (aa=='d' & bb==3 & column=='rr', NaN 
                                     ,0
                              ))))))))))))))))))
                      ,type = ifelse (aa=='a' & bb==1 & column=='cc', 'sameCI'
                             ,ifelse (aa=='a' & bb==2 & column=='cc', 'sameSI'
                             ,ifelse (aa=='a' & bb==3 & column=='cc', 'sameCSI'
                             ,ifelse (aa=='b' & bb==1 & column=='cc', 'diff'
                             ,ifelse (aa=='b' & bb==2 & column=='cc', 'diff'
                             ,ifelse (aa=='b' & bb==3 & column=='cc', 'diff'
                             ,ifelse (aa=='c' & bb==1 & column=='cc', 'only1'
                             ,ifelse (aa=='a' & bb==1 & column=='rr', 'same'
                             ,ifelse (aa=='a' & bb==2 & column=='rr', 'diff'
                             ,ifelse (aa=='a' & bb==3 & column=='rr', 'diff'
                             ,ifelse (aa=='b' & bb==1 & column=='rr', 'diff'
                             ,ifelse (aa=='b' & bb==2 & column=='rr', 'diff'
                             ,ifelse (aa=='b' & bb==3 & column=='rr', 'diff'
                             ,ifelse (aa=='c' & bb==1 & column=='rr', 'only1'
                             ,ifelse (aa=='c' & bb==2 & column=='rr', 'diff'
                                                                    , 'same'
                              )))))))))))))))
                      ,type2= ifelse (aa=='a' & bb==1 & column=='cc', 'sameCI:genl'
                             ,ifelse (aa=='a' & bb==2 & column=='cc', 'sameSI:genl'
                             ,ifelse (aa=='a' & bb==3 & column=='cc', 'sameCSI:genl'
                             ,ifelse (aa=='b' & bb==1 & column=='cc', 'genl:genl'
                             ,ifelse (aa=='b' & bb==2 & column=='cc', 'genl:blank'
                             ,ifelse (aa=='b' & bb==3 & column=='cc', 'genl:na'
                             ,ifelse (aa=='c' & bb==1 & column=='cc', 'only1:genl'
                             ,ifelse (aa=='a' & bb==1 & column=='rr', 'same:genl'
                             ,ifelse (aa=='a' & bb==2 & column=='rr', 'genl:genl'
                             ,ifelse (aa=='a' & bb==3 & column=='rr', 'genl:na'
                             ,ifelse (aa=='b' & bb==1 & column=='rr', 'genl:+inf'
                             ,ifelse (aa=='b' & bb==2 & column=='rr', 'genl:-inf'
                             ,ifelse (aa=='b' & bb==3 & column=='rr', 'genl:nan'
                             ,ifelse (aa=='c' & bb==1 & column=='rr', 'only1:genl'
                             ,ifelse (aa=='c' & bb==2 & column=='rr', 'genl:zeroish'
                             ,ifelse (aa=='d' & bb==1 & column=='rr', 'same:+inf'
                             ,ifelse (aa=='d' & bb==2 & column=='rr', 'same:-inf'
                             ,ifelse (aa=='d' & bb==3 & column=='rr', 'same:nan'
                                                                    , 'same:genl'
                              ))))))))))))))))))
                      
                      ) %>%
                # Remove rows for the uu column which only occurs in the first 
                # dataframe, and add them below, just because it's easier.
                subset(column %nin% c('uu')) %>%
                rbind(# add this row as uniquely occuring in the second dataframe
                      dfDifferencesTest.baseDifferences(testData) %>%
                      subset(column %nin% c('uu')) %>%
                      subset(aa=='c' & bb == 1) %>%
                      mutate(aa = 'cc'
                            ,first = as.numeric(NA)
                            ,diff = as.numeric(NA)
                            ,type  = 'only2'
                            ,type2 = 'only2:genl'
                            )
                     ,# add rows for column uu occuring only in the first dataframe
                      testData %>%
                      mutate(column = 'uu'
                            ,first = uu
                            ,second =  '<absent>'
                            ,diff = '<absent>'
                            ,type = '<absent>'
                            ,type2 = '<absent>'
                            ) %>%
                      select(aa, bb, column, first, second, diff, type, type2)
                     ,# add rows for column newuu occuring only in the second dataframe
                      altData %>%
                      mutate(column = 'newuu'
                            ,first = '<absent>'
                            ,second = newuu
                            ,diff = '<absent>'
                            ,type = '<absent>'
                            ,type2 = '<absent>'
                            ) %>%
                      select(aa, bb, column, first, second, diff, type, type2)
                     ) %>%
              arrange(aa, bb, column)
    row.names(expected) <- NULL
    
    rc <- list(altData = altData, expected = expected)
    return(rc)

}

# end of file