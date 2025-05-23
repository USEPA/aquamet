# sourceAquamet.r
#
# Source()s files and loads packages for aquamet development.  Not to be 
# included in aquamet package.
#
#  1/04/16 cws Used to be aquamet.r, but that's now used for something else.
# 10/01/20 cws No longer using reshape2 package.
# 12/19/23 cws Added tidyr requirement for dfDifferences which uses pivot_wider
#  2/21/24 cws Comment added
#  6/12/24 cws Removed require(plyr), as it is soon to be deprecated.
#  

require(foreach)
require(Hmisc)
#require(aquamet)
require(RUnit)
#require(plyr)
require(dplyr)
require(tidyr)
#require(reshape2)

# Get aquamet function definitions
# Definition of mainPath is also where unit test results go.
if(substr(getwd(),1,1) == 'C') {
    # local development
    mainPath <- sprintf('C:/Users/%s/local/aquamet', Sys.getenv('USERNAME'))
} else {
    # shared space on L drive
    mainPath <- getwd()
}

aquametPathList <- c(sprintf("%s/%s", mainPath, 'R') 
                    ,sprintf("%s/%s", mainPath, 'UnitTests/NLA_Physical_Habitat')
                    ,sprintf("%s/%s", mainPath, 'UnitTests/NRSA_Physical_Habitat')
                    ,sprintf("%s/%s", mainPath, 'UnitTests/SharedCode')
                    )
for(path in aquametPathList) {
    allsrcList <- grep('^.+\\.[rR]$', list.files(path), value=TRUE)
    srcList <- grep('^mets.+$', allsrcList, value = TRUE, invert = TRUE)
    for(src in srcList) 
        source(sprintf("%s/%s", path, src))
    
}
source(sprintf('%s/R/sharedSupport.r',mainPath)) # overwrite defs on L:drive
