# nrsaInvasiveSpecies.r
#
# 12/08/15 cws Modified unit test in metsInvasiveSpecies.r to reflect changes
#          to nrsaInvasiveSpecies: the new calling interface, as well as the
#          return of zero values for individual species (zero ip_score values
#          were previously returned, and still are).  Corrected test case where
#          NO_INVASIVE values were removed from the input data to no longer 
#          expect f_none values in response.
#  2/26/16 cws moved nrsaInvasiveSpecies.singleSpeciesTest and 
#          nrsaInvasiveSpecies.ip_scoreTest here from nrsaInvasiveSpecies.r.
#  6/28/17 cws Updated nrsaInvasiveSpecies.ip_scoreTest() so row names in actual
#          response match expected.
#

nrsaInvasiveSpeciesTest <- function()
{
    ## Creating test data, then convert 0809 organization to new arguments.  
    # Assume that transects where there is no row for a species do not have 
    # that species, because of the way we currently store the data.

    selectSpeciesData <- function(spName) {
        spData <- nrsaInvasiveSpecies.createData() %>%
                  dcast(SITE+TRANSECT~PARAMETER, value.var='VALUE') %>%
                  within(VALUE <- ifelse(is.na(get(spName)), '', get(spName))) %>%
                  select(SITE, TRANSECT, VALUE)
        return(spData)
    }

    
    # Test full data set
    expected <- nrsaInvasiveSpecies.createMetrics()
    actual <- nrsaInvasiveSpecies(butumb = selectSpeciesData('FLWR_RUSH')
                                 ,arudon = selectSpeciesData('G_REED')
                                 ,lytsal = selectSpeciesData('P_LSTRIFE')
                                 ,eiccra = selectSpeciesData('W_HYACINTH')
                                 ,none = selectSpeciesData('NO_INVASIVES')
                                 )
    errs <- dfCompare(expected, actual, c('SITE', 'METRIC'), zeroFudge=1e-9)
    checkEquals(NULL, errs
               ,"Error: nrsaInvasiveSpecies is broken when including 'no invasives' data."
               )

    # Test full data set and a NULL argument, to make sure they're accepted.
    expected <- nrsaInvasiveSpecies.createMetrics()
    actual <- nrsaInvasiveSpecies(butumb = selectSpeciesData('FLWR_RUSH')
                                 ,arudon = selectSpeciesData('G_REED')
                                 ,lytsal = selectSpeciesData('P_LSTRIFE')
                                 ,eiccra = selectSpeciesData('W_HYACINTH')
                                 ,none = selectSpeciesData('NO_INVASIVES')
                                 ,other = NULL
                                 )
    errs <- dfCompare(expected, actual, c('SITE', 'METRIC'), zeroFudge=1e-9)
    checkEquals(NULL, errs
               ,"Error: nrsaInvasiveSpecies is broken when including a NULL data."
               )
    
    # Test calculations without NO_INVASIVES parameter data
    expected <- nrsaInvasiveSpecies.createMetrics() %>% subset(METRIC %nin% 'f_none')
    actual <- nrsaInvasiveSpecies(butumb = selectSpeciesData('FLWR_RUSH')
                                 ,arudon = selectSpeciesData('G_REED')
                                 ,lytsal = selectSpeciesData('P_LSTRIFE')
                                 ,eiccra = selectSpeciesData('W_HYACINTH')
                                 )
    errs <- dfCompare(expected, actual, c('SITE', 'METRIC'), zeroFudge=1e-9)
    checkEquals(NULL, errs
               ,"Error: nrsaInvasiveSpecies is broken when 'no invasives' data is absent."
               )
    
    # Test calculations with no data at all
    actual <- nrsaInvasiveSpecies()
    checkEquals(NULL, actual
               ,"Error: nrsaInvasiveSpecies is broken when provided no arguments."
               )

}


nrsaInvasiveSpecies.createData <- function()
# creates dataframe of invasive species data for unit test
{
  fred <- textConnection(
              "SITE TRANSECT PARAMETER VALUE
              'EPA01-0124' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'E' 'P_LSTRIFE' 'Y'
              'EPA01-0124' 'F' 'P_LSTRIFE' 'Y'
              'EPA01-0124' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0124' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0144' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0147' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0159' 'A' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'B' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'C' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'C' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'D' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'D' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'E' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'E' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'F' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'F' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'G' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'G' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'H' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'H' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'I' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'I' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'J' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'J' 'P_LSTRIFE' 'Y'
              'EPA01-0159' 'K' 'W_HYACINTH' 'Y'
              'EPA01-0159' 'K' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'A' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'B' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'C' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'D' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'E' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'F' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'G' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'H' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'I' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'J' 'P_LSTRIFE' 'Y'
              'EPA01-0161' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0167' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0209' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0210' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0212' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0214' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0215' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0238' 'A' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'A' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'B' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'B' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'C' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'C' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'D' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'D' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'E' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'E' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'F' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'F' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'G' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'G' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'H' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'H' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'I' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'I' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'J' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'J' 'FLWR_RUSH' 'Y'
              'EPA01-0238' 'K' 'W_HYACINTH' 'Y'
              'EPA01-0238' 'K' 'FLWR_RUSH' 'Y'
              'EPA01-0239' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0239' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0240' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0242' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0246' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0248' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0302' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0303' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0305' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0306' 'H' 'FLWR_RUSH' 'Y'
              'EPA01-0306' 'I' 'FLWR_RUSH' 'Y'
              'EPA01-0306' 'J' 'FLWR_RUSH' 'Y'
              'EPA01-0306' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'G' 'W_HYACINTH' 'Y'
              'EPA01-0308' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0308' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0314' 'A' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'B' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'C' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'C' 'FLWR_RUSH' 'Y'
              'EPA01-0314' 'D' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'E' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'F' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'F' 'FLWR_RUSH' 'Y'
              'EPA01-0314' 'G' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'H' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'I' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'J' 'W_HYACINTH' 'Y'
              'EPA01-0314' 'K' 'W_HYACINTH' 'Y'
              'EPA01-0315' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0315' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0421' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0424' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0432' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0448' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0450' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0452' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0457' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0458' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0458' 'B' 'G_REED' 'Y'
              'EPA01-0458' 'C' 'G_REED' 'Y'
              'EPA01-0458' 'D' 'G_REED' 'Y'
              'EPA01-0458' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0458' 'F' 'G_REED' 'Y'
              'EPA01-0458' 'G' 'G_REED' 'Y'
              'EPA01-0458' 'H' 'G_REED' 'Y'
              'EPA01-0458' 'I' 'G_REED' 'Y'
              'EPA01-0458' 'J' 'G_REED' 'Y'
              'EPA01-0458' 'K' 'G_REED' 'Y'
              'EPA01-0537' 'A' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'B' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'C' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'D' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'E' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'F' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'G' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'H' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'I' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'J' 'P_LSTRIFE' 'Y'
              'EPA01-0537' 'K' 'P_LSTRIFE' 'Y'
              'EPA01-0555' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'B' 'P_LSTRIFE' 'Y'
              'EPA01-0555' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0555' 'K' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'A' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'B' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'C' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'D' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'E' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'F' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'G' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'H' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'I' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'J' 'NO_INVASIVES' 'X'
              'EPA01-0556' 'K' 'NO_INVASIVES' 'X'
              "
              )
  testData <-read.table(fred, header=TRUE, stringsAsFactors=FALSE)
  close(fred)

  return(testData)
}



nrsaInvasiveSpecies.createMetrics <- function()
# creates dataframe of expected metrics results for unit test
{
  bob <- textConnection(
              "SITE METRIC VALUE
              'EPA01-0124' 'f_lytsal' 0.181818182
              'EPA01-0124' 'f_none' 0.818181818
              'EPA01-0124' 'ip_score' 0.181818182
              'EPA01-0144' 'f_none' 1
              'EPA01-0144' 'ip_score' 0
              'EPA01-0147' 'f_none' 1
              'EPA01-0147' 'ip_score' 0
              'EPA01-0159' 'f_eiccra' 0.818181818
              'EPA01-0159' 'f_lytsal' 1
              'EPA01-0159' 'ip_score' 1.818181818
              'EPA01-0161' 'f_lytsal' 0.909090909
              'EPA01-0161' 'f_none' 0.090909091
              'EPA01-0161' 'ip_score' 0.909090909
              'EPA01-0167' 'f_none' 1
              'EPA01-0167' 'ip_score' 0
              'EPA01-0209' 'f_none' 1
              'EPA01-0209' 'ip_score' 0
              'EPA01-0210' 'f_none' 1
              'EPA01-0210' 'ip_score' 0
              'EPA01-0212' 'f_none' 1
              'EPA01-0212' 'ip_score' 0
              'EPA01-0214' 'f_none' 1
              'EPA01-0214' 'ip_score' 0
              'EPA01-0215' 'f_none' 1
              'EPA01-0215' 'ip_score' 0
              'EPA01-0238' 'f_eiccra' 1
              'EPA01-0238' 'f_butumb' 1
              'EPA01-0238' 'ip_score' 2
              'EPA01-0239' 'f_none' 1
              'EPA01-0239' 'ip_score' 0
              'EPA01-0240' 'f_none' 1
              'EPA01-0240' 'ip_score' 0
              'EPA01-0242' 'f_none' 1
              'EPA01-0242' 'ip_score' 0
              'EPA01-0246' 'f_none' 1
              'EPA01-0246' 'ip_score' 0
              'EPA01-0248' 'f_none' 1
              'EPA01-0248' 'ip_score' 0
              'EPA01-0302' 'f_none' 1
              'EPA01-0302' 'ip_score' 0
              'EPA01-0303' 'f_none' 1
              'EPA01-0303' 'ip_score' 0
              'EPA01-0305' 'f_none' 1
              'EPA01-0305' 'ip_score' 0
              'EPA01-0306' 'f_butumb' 0.272727273
              'EPA01-0306' 'f_none' 0.727272727
              'EPA01-0306' 'ip_score' 0.272727273
              'EPA01-0308' 'f_eiccra' 0.090909091
              'EPA01-0308' 'f_none' 0.909090909
              'EPA01-0308' 'ip_score' 0.090909091
              'EPA01-0314' 'f_eiccra' 1
              'EPA01-0314' 'f_butumb' 0.181818182
              'EPA01-0314' 'ip_score' 1.181818182
              'EPA01-0315' 'f_none' 1
              'EPA01-0315' 'ip_score' 0
              'EPA01-0421' 'f_none' 1
              'EPA01-0421' 'ip_score' 0
              'EPA01-0424' 'f_none' 1
              'EPA01-0424' 'ip_score' 0
              'EPA01-0432' 'f_none' 1
              'EPA01-0432' 'ip_score' 0
              'EPA01-0448' 'f_none' 1
              'EPA01-0448' 'ip_score' 0
              'EPA01-0450' 'f_none' 1
              'EPA01-0450' 'ip_score' 0
              'EPA01-0452' 'f_none' 1
              'EPA01-0452' 'ip_score' 0
              'EPA01-0457' 'f_none' 1
              'EPA01-0457' 'ip_score' 0
              'EPA01-0458' 'f_arudon' 0.818181818
              'EPA01-0458' 'f_none' 0.181818182
              'EPA01-0458' 'ip_score' 0.818181818
              'EPA01-0537' 'f_lytsal' 1
              'EPA01-0537' 'ip_score' 1
              'EPA01-0555' 'f_lytsal' 0.090909091
              'EPA01-0555' 'f_none' 0.909090909
              'EPA01-0555' 'ip_score' 0.090909091
              'EPA01-0556' 'f_none' 1
              'EPA01-0556' 'ip_score' 0
              "
              )

  testResults0809 <-read.table(bob, header=TRUE, stringsAsFactors=FALSE)
  close(bob)

  zerosNotIn0809 <- expand.grid(SITE=unique(testResults0809$SITE)
                               ,METRIC=setdiff(unique(testResults0809$METRIC), 'ip_score')
                               ,stringsAsFactors=FALSE
                               ) %>%
                    mutate(VALUE = 0) %>%
                    subset(paste(SITE, METRIC) %nin% with(testResults0809, paste(SITE, METRIC)))
  
  testResults <- rbind(testResults0809, zerosNotIn0809)
  
  return(testResults)

}


nrsaInvasiveSpecies.ip_scoreTest <- function()
# unit test for nrsaInvasiveSpecies.ip_score
{
    tc <- textConnection("SITE METRIC VALUE
                          1    f_a    .0    # site with f_none present = 0
                          1    f_b    .1
                          1    f_c    .2
                          1    f_d    .3
                          1    f_none .0
                          2    f_a    .1    # site with f_none absent
                          2    f_b    .2
                          2    f_c    .3
                          2    f_d    .4
                          3    f_a    .0    # site with f_none present = 1
                          3    f_b    .1
                          3    f_c    .2
                          3    f_d    .3
                          3    f_none  1
                          4    f_a    .0    # site with f_none present = NA
                          4    f_b    .1
                          4    f_c    .2
                          4    f_d    .3
                          4    f_none NA
                          5    f_a    .0    # site with NA value in data and f_none present = .7
                          5    f_b    .1
                          5    f_c    NA
                          5    f_d    .3
                          5    f_none .7
                          6    f_none  0    # site with only f_none, value is 0
                          7    f_none  1    # site with only f_none, value is 1
                          8    f_none NA    # site with only f_none, value is NA
                         ")
    testData <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    rm(tc)
    
    expected <- data.frame(SITE=1:8
                          ,VALUE=c(0.6, 1, 0.6, 0.6, 0.4, 0, 0, 0)
                          ,METRIC='ip_score'
                          ,stringsAsFactors=FALSE
                          )
    actual <- nrsaInvasiveSpecies.ip_score(testData)
    row.names(actual) <- NULL

    checkEquals(expected, actual, "Incorrect ip_score results")
}


nrsaInvasiveSpecies.singleSpeciesTest <- function()
# unit test for nrsaInvasiveSpecies.singleSpecies
{
    tc <- textConnection("SITE TRANSECT VALUE
                          a    A        0       # Site with 0/1 data
                          a    B        1
                          a    C        1
                          a    D        1
                          a    E        0
                          a    F        0
                          a    G        0
                          a    H        1
                          a    I        0
                          a    J        1
                          a    K        0
                          b    A        0       # Site with character data
                          b    B        X
                          b    C        X
                          b    D        Y
                          b    E        ''
                          b    F        ''
                          b    G        ''
                          b    H        X
                          b    I        ''
                          b    J        Y
                          b    K        ''
                          c    A        0       # Site with 0/1/NA/'' data
                          c    B        NA
                          c    C        1
                          c    D        1
                          c    E        0
                          c    F        ''
                          c    G        0
                          c    H        1
                          c    I        0
                          c    J        1
                          c    K        0
                          d    A        0       # Site with character and NA data
                          d    B        NA
                          d    C        X
                          d    D        Y
                          d    E        ''
                          d    F        NA
                          d    G        ''
                          d    H        X
                          d    I        ''
                          d    J        Y
                          d    K        ''
                          e    A        0       # Site with fractional and NA data
                          e    B        NA      # just to see if it works.
                          e    C        .3
                          e    D        .5
                          e    E        0
                          e    F        NA
                          e    G        .1
                          e    H        .1
                          e    I        0
                          e    J        .2
                          e    K        .7
                         ")
    testData <- read.table(tc, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    rm(tc)

    expected <- data.frame(SITE=c('a', 'b', 'c', 'd','e')
                          ,VALUE=c(5/11, 5/11, 4/10, 4/9, 1.9/9)
                          ,stringsAsFactors=FALSE
                          )
    actual <- nrsaInvasiveSpecies.singleSpecies(testData)
    checkEquals(expected, actual, "Incorrect ip_score results")
}

# end of file
