# This is a prototype to determine if using the object oriented class and methods capabilities in R will simplify using aquamet for users and programming it for those maintaining it.
# The existing functions in aquamet expect data sets in a certain format, with specific variables, and limits on those variables.  That is the requirements of the data set are built into the functions, not the data sets.
# These requirements aren't explicitly stated by the functions, which simply fail if they aren't met.
# This is an attempt to move the requirements of structure and limitations to the data sets by treating them as objects and coding the requirements and limitations into them explicitly.

# I'm using this thalweg data set from Curt Seeliger for the prototype. I hope to make something more generalized, as in the inheritence order of data.frame, long data frame, grouped long data frame, thalweg grouped long data frame.
# I believe enforcing the "long" and "grouped" parts is necessary. I'd like to have something more generalized than enforcing "thalweg" or other similar levels of data structure
# I might want to add another level after this for the summarized data. So the result of the calculations is a class that inherits from the input.

require(aquamet)
#### Example data set ####
thal <- read.csv("protoype code/jesseThalweg.csv",
         header = TRUE,
         colClasses = "character",
         stringsAsFactors = FALSE,
         na.strings = "NA")



#### Structure definition ####

# This is just for reference -- it doesn't do anything. It's actually used in the constructor function below
# This might need a sample type e.g. boatable, wadeable
# I've seen something similar done where the object itself is a list that includes a data frame and the meta data are also members of the list.  the "lm" class does this.  By using the attributes for the metadata the object can still be treated as a data frame easily.
structure(
  data.frame(),
  variables = character(),
  validVariableTypes = list(),
  parameters = character(),
  validParameters = character(),
  groups = character(),
  validResultTypes = list(),
  validResult = list(),
  summaryFunctions = list(),
  class = c("data.frame, GroupedLongDataFrame")
  )


# I'm going to treat this as including all possible valid values for a thalweg data set, and only valid values.
# The following code summarizes that for later use

thalwegStructure <- list() # At some point this option would need a structure definition for every kind of data set.  Either as a file or object that comes with the package. It shouldn't need to change much if ever. If new kinds of data sets are added it will only need to be added there.

thalwegStructure$ValidVariables <- c("FLAG", "PARAMETER", "RESULT", "SAMPLE_TYPE", "STATION", "TRANSECT", "UID")

thalwegStructure$ValidVariableTypes <- list(FLAG = "is.character", PARAMETER = "is.character", RESULT = "is.character", SAMPLE_TYPE = "is.character", STATION = "is.character", TRANSECT = "is.character", UID = "is.character")

thalwegStructure$ValidVariableTypesFunctional <- list(FLAG = function(x) is.character(x), PARAMETER = function(x) is.character(x), RESULT = function(x) is.character(x), SAMPLE_TYPE = function(x) is.character(x), STATIOn = function(x) is.character(x), TRANSECT = function(x) is.character(x), UID = function(x) is.character(x))

thalwegStructure$ValidParameters <- c("BACKWATER", "BAR_PRES", "BARWIDTH", "CHANUNCD", "DEPTH", "INCREMENT", "REACHLENGTH", "SEDIMENT", "SIDCHN", "WETWIDTH", "DEPTH_UNITS", "OFF_CHAN", "SNAG", "SONAR", "SIZE_CLS")

thalwegStructure$ValidResultTypes <- list(BACKWATER = "is.character", BAR_PRES = "is.character", BARWIDTH = "is.numeric", CHANUNCD = "is.character", DEPTH = "is.numeric", INCREMENT = "is.numeric", REACHLENGTH = "is.numeric", SEDIMENT = "is.character", SIDCHN = "is.character", WETWIDTH = "is.numeric", DEPTH_UNITS = "is.character", OFF_CHAN = "is.character", SNAG = "is.character", SONAR = "is.numeric", SIZE_CLS = "is.character")

thalwegStructure$Groups <- as.character(c("UID", "TRANSECT", "STATION"))

# I got the categorical ones from the field sheet explanation that Karen gave me.  The ranges I guessed.
# I can probably write a single function to generalize this that takes the parameter from $ValidParameters and the limits from $ValidResults and returns rows that don't pass.
thalwegStructure$ValidResults <- list(BACKWATER = as.character("Y"),
                            BAR_PRES = as.character("Y"),
                            BARWIDTH = as.numeric(c(0,50)),
                            CHANUNCD = as.character("PO","GL","RI","RA","CA","FA","DR"),
                            DEPTH = as.numeric(c(5,250)),
                            DEPTH_UNITS = as.character(c("M","FT")),
                            INCREMENT = as.numeric(c(1,10)),
                            OFF_CHAN = as.character("Y"),
                            REACHLENGTH = as.numeric(c(0,4400)),
                            SEDIMENT = as.character("Y"),
                            SIDCHN = as.character("Y"),
                            SIZE_CLS = as.character(c("RS","RR","RC","XB","SB","CB","GC","GF","SA","FN","HP","WD","OT")),
                            SNAG = as.character("Y"),
                            SONAR = as.numeric(c(1,50)),
                            WETWIDTH = as.numeric(c(1,90))
)

#This is in the wrong spot right now because $SummaryFunctions fails if the functions don't exist.
metsSimpleMetDepth <- function(tdf){ # trivial example and probably incorrect results.
  mythal <- subset(tdf, PARAMETER == "DEPTH")
  mythal$RESULT <- as.numeric(mythal$RESULT)
  mythal$RESULT <- ifelse(mythal$SAMPLE_TYPE == "THALW", mythal$RESULT * 
                            100, mythal$RESULT)
  depthx <- summaryby(mythal, "mean", "xdepth")
  depthsd <- summaryby(mythal, "sd", "sddepth")
  depthc <- aggregate(list(RESULT = mythal$RESULT), list(UID = mythal$UID), 
                      count)
  depthc$METRIC <- "n_d"
  mets <- rbind(depthx,depthsd, depthc)
  mets
}
metsSimpleMetBW <- function(tdf){ # trivial example and probably incorrect results.
  mythal <- subset(tdf, PARAMETER == "BARWIDTH")
  mythal$RESULT <- as.numeric(mythal$RESULT)
  mythal$RESULT <- ifelse(mythal$SAMPLE_TYPE == "THALW", mythal$RESULT * 
                            100, mythal$RESULT)
  bwx <- summaryby(mythal, "mean", "xbw")
  bwsd <- summaryby(mythal, "sd", "sdbw")
  bwc <- aggregate(list(RESULT = mythal$RESULT), list(UID = mythal$UID), 
                      count)
  bwc$METRIC <- "n_bw"
  mets <- rbind(bwx,bwsd, bwc)
  mets
}

thalwegStructure$SummaryFunctions <- list(metsSimpleMetDepth,metsSimpleMetBW) # metsDoMets() will look here to calculate metrics. Add another metric by writing the function and adding it to the list. The user doesn't need to do anything.

thalwegStructure$Class <- "thalweg"

#### Utility Functions ####

# Simple check to see that the correct column names exist
# I don't like that this works by sorting.  It should look at the name of the variable in the data frame and find the matching variable in $ValidVariableTypes
# If it did that it would not need matching lengths and could allow users to have additional columns if they needed
# I'd like it to also specifically say which variable did not pass
checkVariableTypes <- function(variables, validVariableTypes) {
  variables <- sort(names(variables))
  if (length(variables) != length(validVariableTypes)) {
    stop("variable and valid variable type lengths differ")
  }
  types <- logical(length(variables))
  for (i in seq_along(variables)) {
    types[i] <- get(validVariableTypes[[i]])(variables[i])
  }
  all(types)
}

# This is a really trivial and not well written example but the idea can be used for all the metrics that are univariate summaries and for multiple parameters within the same data set type.  For metrics that involve a parameter from two different data sets might need to be done slightly differently.
# The list of metrics and their functions are within the object, which would be in the same place as the limitations etc. The user would run this, and possibly select which metrics to use, and get all related metrics in a single data.frame.
# For any metric changes or additions the programmer would change the related function only. Or create the new function ad add it to list of functions within the object. This means there will only be two items to change.
metsDoMets <- function(gld){
  # if ("GroupedLongDataFrame" %in% class(gld)) stop("blah Needs class of ....") # This needs some checking, but not this. Could do summary.GroupedLongDataFrame() but I actually like that idea less.
  ph <- data.frame(UID = character(),METRIC= character(),RESULT=character())
  flist <- attr(testobj, "SummaryFunctions")
    for (i in seq_along(flist)){
    ph <- rbind(ph,flist[[1]](gl))
    }
  return(ph)
}

# This is the constructor / validation function that will combine the different validity-checking functions into a single function.
# Right now it has many nested ifs and stops if there's an issue. It would probably be more useful to only warned and gave a report listing all issues instead.
groupedLongDataFrame <- function(df, groupingStructure) {
  # Check validity -- make this a separate function later?
  if (!is.data.frame(df)) {
    stop("groupedLongDataFrame() requires an object of class data.frame")
  }else if (!all(groupingStructure$ValidVariables %in% names(df))) {# Note that this checks that the required names are in the data frame -- not that the data frame only has names within the required names. It will allow other columns then.
    stop(paste(
      c(
        "This structure requires columns with names of", paste(groupingStructure$ValidVariables, collapse = ", ")
      ), collapse = " "
    ))
  }else if (!checkVariableTypes(df, groupingStructure$ValidVariableTypes))  { # I might want to change the function to only pass the names
    stop("Invalid variable type")
  }else if (!all(groupingStructure$ValidParameters %in% unique(df$PARAMETER))) { # Like above this will not fail or warn if there are extra parameters
    stop(paste(
      c(
        "This structure requires these measurements within the PARAMETER variable:", paste(groupingStructure$ValidParameters, collapse = ", ")
      ), collapse = " "
    ))
  } # else if (){ placeholder for validating the types of the parameters }else if(){placeholder for validating the values of the parameters}
  
  # Should this have some of the data cleanup work in it? e.g. standardizing DEPTH, SONAR, and POLE
  
  
  return(structure(  df,
                     variables = names(df), #This is probably redundant since it is just the names. Possibly helpful if the names are changed unannounced
                     validVariableTypes = groupingStructure$ValidVariableTypes,
                     parameters = sort(unique(df$PARAMETER)),
                     validParameters = groupingStructure$ValidParameters,
                     groups = groupingStructure$Groups,
                     validResultTypes = groupingStructure$ValidResultTypes,
                     ValidResult = groupingStructure$ValidResults,
                     SummaryFunctions = groupingStructure$SummaryFunctions,
                     class = c("data.frame","GroupedLongDataFrame", groupingStructure$Class)
  )
  
  )
}

testobj <- groupedLongDataFrame(thal,thalwegStructure) # Make one

str(testobj) # Just a data frame with many attributes and additional classes

metsDoMets(testobj) # Calculate all available metrics from the object

#### Controlled failures ####

badthal <- setNames(thal,c("UID","QUACK","PARAMETER","TRANSECT","STATION","RESULT","FLAG"))

badtestobj <- groupedLongDataFrame(badthal,thalwegStructure)
str(badtestobj)

badthal2 <- thal
badthal2 <- within(badthal2, PARAMETER <- gsub("WETWIDTH", "WWIDTH", PARAMETER))

badtestobj2 <- groupedLongDataFrame(badthal2,thalwegStructure)
str(badtestobj2)
