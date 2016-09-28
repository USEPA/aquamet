# aquamet.r
#
# Source()s files and loads packages for aquamet.  Not to be included in aquamet
# package.
#

require(foreach)
require(Hmisc)
#require(aquamet)
require(RUnit)
require(plyr)
require(dplyr)
require(reshape2)

# Get aquamet function definitions
if(substr(getwd(),1,1) == 'C') {
    # local development
    mainPath <- sprintf('C:/Users/%s/local/aquamet', Sys.getenv('USERNAME'))
} else {
    # shared space on L drive
    mainPath <- getwd()
}

aquametPathList <- c(sprintf("%s/%s", mainPath, 'R')
                    ,sprintf("%s/%s", mainPath, 'UnitTests/NLA Physical Habitat')
                    ,sprintf("%s/%s", mainPath, 'UnitTests/NRSA Physical Habitat')
                    ,sprintf("%s/%s", mainPath, 'UnitTests/SharedCode')
                    )
for(path in aquametPathList) {
    allsrcList <- grep('^.+\\.[rR]$', list.files(path), value=TRUE)
    srcList <- grep('^mets.+$', allsrcList, value = TRUE, invert = TRUE)
    for(src in srcList) 
        source(sprintf("%s/%s", path, src))
}
source(sprintf('%s/R/sharedSupport.r',mainPath)) # overwrite defs on L:drive
