#' aquamet internal functions
#' @name aquamet-internal
#' @aliases assignTaxCat 
#' calcSynCovers 
#' calcSynInfluence 
#' convert_to_char 
#' count
#' dfCompare 
#' dfLengthen 
#' dfWiden 
#' expandDataFrame
#' fillinDrawdownData 
#' fillinDrawdownData.expansion
#' first 
#' gmean 
#' idr 
#' iqr 
#' interpolatePercentile 
#' is.subset 
#' intermediateMessage 
#' lag 
#' last 
#' lead 
#' modalClass 
#' modalClasses 
#' modalCount 
#' modalvalue 
#' modalValues 
#' normalizedCover 
#' nWadeableStationsPerTransect 
#' protectedMean 
#' protectedSum 
#' rename 
#' summaryby 
#' trimws 
#' uidCreate 
#' uidSeparate 
#' aquametStandardizeArgument 
#' aquametStandardizeArgument.checkLegal
#' aquametStandardizeArgument.checkRange 
#' aquametStandardizeArgument.checkStructure
#' nlaStationInformation.islandStations 
#' nlaStationInformation.stationDepths 
#' fillinDDWithRiparianValues
#' fillinAbsentMissingWithDefaultValue.expansion
#' fillinAbsentMissingWithDefaultValue
#' nlaHumanImpact.calculateMets
#' nlaHumanImpact.weightedIndividualInfluence
#' nlaHumanImpact.overallInfluence
#' nlaHumanImpact.circaInfluence
#' nlaHumanImpact.anyPresence
#' nlaHumanImpact.weightedGroupInfluence 
#' nlaAquaticMacrophytes.individualCover
#' nlaAquaticMacrophytes.indices
#' nlaBankFeatures.standardizeData
#' nlaBankFeatures.bankAngle
#' nlaBankFeatures.distances
#' nlaBankFeatures.fillinDistances
#' nlaBottomSubstrate.setupForParticleCalculations
#' nlaBottomSubstrate.indivPresence
#' nlaBottomSubstrate.variety
#' nlaBottomSubstrate.indivCover
#' nlaBottomSubstrate.populationEstimates
#' nlaBottomSubstrate.modeCover
#' nlaBottomSubstrate.indivColor
#' nlaBottomSubstrate.modeColor
#' nlaBottomSubstrate.indivOdor
#' nlaBottomSubstrate.modeOdor
#' nlaFishCover.calculateMets
#' nlaFishCover.splitParameterNames
#' nlaFishCover.indivCovers
#' nlaFishCover.groupIndices
#' nlaHumanImpact.calculateMets
#' nlaHumanImpact.weightedIndividualInfluence
#' nlaHumanImpact.overallInfluence
#' nlaHumanImpact.circaInfluence
#' nlaHumanImpact.anyPresence
#' nlaHumanImpact.weightedGroupInfluence
#' nlaLittoralMacrohabitat.cover
#' nlaLittoralMacrohabitat.coverTypes
#' nlaLittoralMacrohabitat.humanDist
#' nlaLittoralMacrohabitat.substrate
#' nlaLittoralZone.prepareInput
#' nlaLittoralZone.modeFilm
#' nlaLittoralZone.fractionalPresences
#' nlaLittoralZone.filmVariety
#' nlaRiparianVegetation.calculateMets
#' nlaRiparianVegetation.canopyTypePresence
#' nlaRiparianVegetation.understoryTypePresence
#' nlaRiparianVegetation.componentPresence
#' nlaRiparianVegetation.componentCovers
#' nlaRiparianVegetation.compositeIndex
#' nlaStationInformation.islandStations
#' nlaStationInformation.stationDepths
#' 
#' @description These are internal functions to aquamet and not intended for use on their own.
#' @keywords internal
#' @usage
#' aquametStandardizeArgument.checkLegal(arg, expectedLegal)
#' aquametStandardizeArgument.checkRange(arg, expectedRange)
#' aquametStandardizeArgument.checkStructure(arg, expectedStruct)
#' assignTaxCat(lt)
#' calcSynCovers(coverData, maxDrawdown, assumptions=FALSE)
#' calcSynInfluence(influenceData)
#' convert_to_char(df)
#' count(x)
#' dfCompare(df1, df2, byVars, zeroFudge=1e-17, verbose=FALSE)
#' dfLengthen(df, keys, name, value, values)
#' expandDataFrame(df, cols)
#' first(df, v, first.v)
#' gmean(x)
#' idr(x, method=2)
#' iqr(x, method=2)
#' interpolatePercentile(df, classVar, percentile, pctlVar, classBounds)
#' is.subset(a, b)
#' intermediateMessage(text, loc='middle', intermediateMessages=TRUE)
#' lag(df, v, lag.v, offset=1)
#' last(df, v, last.v)
#' lead(df, v, lead.v, offset=1)
#' modalClass(x, values, classes)
#' modalClasses(df, classes, weights, delim = ', ')
#' modalCount(x)
#' modalvalue(x, na.rm=FALSE)
#' modalValues(x, delim='&', na.rm=FALSE)
#' normalizedCover(df, coverValue, coverNorm, allowTotalBelow100=FALSE)
#' nWadeableStationsPerTransect(thal)
#' protectedMean(x, na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE, ...)
#' protectedSum(x, na.rm=FALSE, inf.rm=FALSE, nan.rm=FALSE, ...)
#' summaryby(xxx,yyy,zzz)
#' trimws(text)
#' uidCreate(df, keys, sep='+')
#' uidSeparate(df, uidName, keyNames, sep='+')
#' nlaStationInformation.islandStations(df)
#' nlaStationInformation.stationDepths(df)
#' aquametStandardizeArgument(arg, ..., ifdf=NULL, 
#' struct=list(SITE, VALUE), 
#' rangeLimits=NULL, legalValues=NULL, stopOnError=TRUE, 
#' metsFuncName,
#' argSavePath = NULL))
#' fillinDDWithRiparianValues(hiData, horizDist, fillinMaxDrawdownDist)
#' fillinAbsentMissingWithDefaultValue(df)
#' fillinAbsentMissingWithDefaultValue.expansion(df)
#' nlaHumanImpact.calculateMets(hiData)
#' nlaHumanImpact.weightedIndividualInfluence(hiData)
#' nlaHumanImpact.overallInfluence(hiData)
#' nlaHumanImpact.circaInfluence(hiData)
#' nlaHumanImpact.anyPresence(hiData)  
#' nlaHumanImpact.weightedGroupInfluence(hiData)
#' nlaAquaticMacrophytes.individualCover(df, presenceWeights, coverWeights)
#' nlaAquaticMacrophytes.indices(df)
#' nlaBankFeatures.standardizeData(df)
#' nlaBankFeatures.bankAngle(df)
#' nlaBankFeatures.distances(df)
#' nlaBankFeatures.fillinDistances(df)
#' nlaBottomSubstrate.setupForParticleCalculations(bsData, substrateCovers, substrateSizes)
#' nlaBottomSubstrate.indivPresence(bsPresence)
#' nlaBottomSubstrate.variety(bsData)	
#' nlaBottomSubstrate.indivCover(bsData)
#' nlaBottomSubstrate.populationEstimates(bsData, substrateSizes)
#' nlaBottomSubstrate.modeCover(presences, covers)
#' nlaBottomSubstrate.indivColor(df)
#' nlaBottomSubstrate.modeColor(df)
#' nlaBottomSubstrate.indivOdor(df)
#' nlaBottomSubstrate.modeOdor(df)
#' nlaFishCover.calculateMets(fcDataCalcs)
#' nlaFishCover.splitParameterNames(parameters)
#' nlaFishCover.indivCovers(fcDataCalcs)
#' nlaFishCover.groupIndices(fcDataCalcs)
#' nlaHumanImpact.calculateMets(hiData)
#' nlaHumanImpact.weightedIndividualInfluence(hiData)
#' nlaHumanImpact.overallInfluence(hiData)
#' nlaHumanImpact.circaInfluence(hiData)	
#' nlaHumanImpact.anyPresence(hiData) 
#' nlaHumanImpact.weightedGroupInfluence(hiData)
#' nlaLittoralMacrohabitat.cover(df)
#' nlaLittoralMacrohabitat.coverTypes(df)
#' nlaLittoralMacrohabitat.humanDist(df, hdWeights)
#' nlaLittoralMacrohabitat.substrate(df)
#' nlaLittoralZone.prepareInput(lzData, data2007)
#' nlaLittoralZone.modeFilm(df, data2007)
#' nlaLittoralZone.fractionalPresences(df, data2007)
#' nlaLittoralZone.filmVariety(df, data2007)
#' nlaRiparianVegetation.calculateMets(rvData)
#' nlaRiparianVegetation.canopyTypePresence(rvData)
#' nlaRiparianVegetation.understoryTypePresence(rvData)
#' nlaRiparianVegetation.componentPresence(rvData)
#' nlaRiparianVegetation.componentCovers(allFractions)
#' nlaRiparianVegetation.compositeIndex(groupData, indexName)
#' nlaStationInformation.islandStations(df)
#' nlaStationInformation.stationDepths(df)
#' 
NULL