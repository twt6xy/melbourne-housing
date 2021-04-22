library(tidyverse)

melbourne <- read.csv("melb_data_raw.csv", header=TRUE)
glimpse(melbourne)

# We don't need method, seller, or address.
melbourne <- subset(melbourne, select = -c(Address, SellerG, Method))

# telling R which predictors are categorical
melbourne$Type <- factor(melbourne$Type)
melbourne$Regionname <- factor(melbourne$Regionname)
melbourne$Postcode <- factor(melbourne$Postcode)
melbourne$CouncilArea <- factor(melbourne$CouncilArea)
melbourne$Suburb <- factor(melbourne$Suburb)

# telling R which predictor is a date column 
melbourne$Date <- as.Date(melbourne$Date)  # - the dates are in different formats so we'll have to standardize the formatting

# Checking for null columns
colSums(is.na(melbourne))

# We can drop the 62 instances where the cars predictor is NA
melbourne <- melbourne[!is.na(melbourne$Car),]

"Only building area and year built have NA values.
 We can two extra tables with year built, or building area
 dropped. Then filter the two tables to pull all rows where
 building area and year built contain values. We can then use the 
 non-null datasets to test each predictors significance separately.
 I suspect buildingArea will not be important because it's likely
 correlated to number of rooms, but we may have to do something about year built."

yearBuiltAnalysis = na.omit(subset(melbourne, select = -c(BuildingArea)))
buildingAreaAnalysis = na.omit(subset(melbourne, select = -c(YearBuilt)))


yearBuiltResult <- lm(Price~Rooms+Type+Distance+Bedroom2+Bathroom+Car+Landsize+YearBuilt+CouncilArea+Regionname+Propertycount, data=yearBuiltAnalysis)
summary(yearBuiltResult)
# in the prescense of the other predictors, year built is significant in estimating the price of a house, so we should keep it, < 2e-16 ***

buildingAreaResult <- lm(Price~Rooms+Type+Distance+Bedroom2+Bathroom+Car+Landsize+BuildingArea+CouncilArea+Regionname+Propertycount, data=buildingAreaAnalysis)
summary(buildingAreaResult)
# building area is less significant, but still significant, 0.012366 * 

### NEXT STEPS: refine dataset via multicollinearity tests, outlier detection, possible transforms, etc.