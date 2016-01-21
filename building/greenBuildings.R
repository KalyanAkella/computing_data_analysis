# returns the list of greenest buildings in every region
# greenest - least energy consumed
greenBuildings <- function () {
  result <- data.frame(building = NA, region = NA, kwh = NA, Max.kwh = NA)
  buildingData <- read.csv("building_data.csv", colClasses = "character")
  buildingData[, 39] <- as.numeric(gsub(",", "", buildingData[, 39]))
  buildingData <- buildingData[complete.cases(buildingData[, 39]), ]
  for (region in unique(factor(buildingData$Region))) {
    buildingDataForRegion <- buildingData[buildingData$Region == region, ]
    sortedBuildingData <- buildingDataForRegion[order(buildingDataForRegion[, 39], buildingDataForRegion$Building.Name), ]
    maxKwh <- max(sortedBuildingData[, 39])
    result <- rbind(result, c(sortedBuildingData$Building.Name[1], region, sortedBuildingData[1, 39], maxKwh))
  }
  result[complete.cases(result), ]
}
