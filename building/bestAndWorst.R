# returns the best and the worst buildings in the given aspect,
# eg., energy consumption, water consumption, etc. Data is reported
# per region. Note that, higher the value, worser the building's rank.
# input: aspect must be one of: energy, water, co2, waste, non.recycled.waste
# output: data.frame(region, best.building, worst.building)
# the building is reported in the format, {<building-name>, <post-code>}
bestAndWorst <- function (aspect) {
  result <- data.frame(region = NA, best.building = NA, worst.building = NA)
  aspects <- pairlist(energy = 39, water = 40, waste = 41,
                      co2 = 42, non.recycled.waste = 44)
  if (!(aspect %in% names(aspects))) {
    stop("invalid aspect")
  }
  buildingData <- read.csv("building_data.csv", colClasses = "character")
  aspectCol <- aspects[[aspect]]
  buildingData[, aspectCol] <- as.numeric(gsub(",", "", buildingData[, aspectCol]))
  buildingData <- buildingData[complete.cases(buildingData[, aspectCol]), ]
  regionFactor <- factor(buildingData$Region)
  for (region in unique(regionFactor)) {
    buildingDataForRegion <- buildingData[buildingData$Region == region, ]
    incBuildingData <- buildingDataForRegion[order(buildingDataForRegion[, aspectCol], buildingDataForRegion$Building.Name), ]
    decBuildingData <- buildingDataForRegion[order(-buildingDataForRegion[, aspectCol], buildingDataForRegion$Building.Name), ]
    bestBuilding <- paste(incBuildingData$Building.Name[1], incBuildingData$Postcode[1], sep = ", ")
    worstBuilding <- paste(decBuildingData$Building.Name[1], decBuildingData$Postcode[1], sep = ", ")
    result <- rbind(result, c(region, bestBuilding, worstBuilding))
  }
  result[complete.cases(result), ]
}
