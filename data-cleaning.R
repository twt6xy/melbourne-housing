library(tidyverse)
library(data.table)
library(dplyr)
library(lubridate) # Datetime

###you need to read in your NA's, leaving stringasfactors as false, but true would save you some work later
melbourne <- read.csv("melb_data_raw.csv", stringsAsFactors = FALSE, na.strings = c(""," ","NA","NULL"), header=TRUE)
walkability <- read.csv("suburbWalkabilityPopulation.csv", stringsAsFactors = FALSE, na.strings = c(""," ","NA","NULL"), header=TRUE)
melbourne <- merge(melbourne, walkability, on="Suburb")

# view if any null columns
colSums(is.na(melbourne))

# =================================================================================================================
# BuildingArea: 6417 NA Values - fillna with median building area for type of building with same number of bedrooms
# YearBuilt: 5344 NA Values - fillna with the median year built for type of building in the respective suburb
# REASONING: Filling NA values strictly by suburb or number of bedrooms would introduce flawed assumptions,
#            an old suburb that used to consist of mainly houses may be be home to new builds that would
#            consist of townhomes and apartments, so we need to consider building type during the imputations.
# Car: 62 NA Values - fillna with with mode of Car since it's only 62 instances
# =================================================================================================================

# getting the median building area by number of rooms and type of building
roomsAndTypeGrouped <- melbourne %>%
        group_by(Rooms, Type) %>%
        summarise(medianArea = median(BuildingArea, na.rm=TRUE))

# getting the median year built by suburb and type of building 
suburbAndTypeGrouped <- melbourne %>%
        group_by(Suburb, Type) %>%
        summarise(medianYear = median(YearBuilt, na.rm=TRUE))

# getting the median number of car spots per building type to fill the 62 null values with
TypeGrouped <- melbourne %>%
        group_by(Type) %>%
        summarise(medianCar = median(Car, na.rm=TRUE))

# After grouping median year built by suburb and type of building, there are still 80 NA values, so I'll fill those by median year by building type
TypeGroupForRemainingNAyears <- melbourne %>%
        group_by(Type) %>%
        summarise(medianYearType = median(YearBuilt, na.rm=TRUE))

# filling in the blank council areas
suburbAndCouncilGrouped <- melbourne %>%
        group_by(Suburb, CouncilArea)

# median for 4 rooms of type U = 171.5, so I'm going to double it for the null value at 8 rooms of type u.
roomsAndTypeGrouped$medianArea[19]=394.45

# median for 5 rooms of type h = 260, so I'm doubling it for the null value at 10 rooms of type h
roomsAndTypeGrouped$medianArea[20]=520

# merging all of the grouped datasets into a copy of the melbourne data, and then filling null values with its respective median value
testMerge <- merge(melbourne, roomsAndTypeGrouped, by=c("Rooms", "Type"))
testMerge$BuildingArea <- ifelse(is.na(testMerge$BuildingArea), testMerge$medianArea, testMerge$BuildingArea)
testMerge <- merge(testMerge, suburbAndTypeGrouped, by=c("Suburb", "Type"))
testMerge$YearBuilt <- ifelse(is.na(testMerge$YearBuilt), testMerge$medianYear, testMerge$YearBuilt)
testMerge <- merge(testMerge, TypeGrouped, by="Type")
testMerge$Car <- ifelse(is.na(testMerge$Car), testMerge$medianCar, testMerge$Car)
testMerge <- merge(testMerge, TypeGroupForRemainingNAyears, by="Type")
testMerge$YearBuilt <- ifelse(is.na(testMerge$YearBuilt), testMerge$medianYearType, testMerge$YearBuilt)

councilSuburbs <- testMerge[!duplicated(testMerge[,c("Suburb", "CouncilArea")]),]
councilSuburbs <- subset(councilSuburbs, select = c( Suburb, CouncilArea))
councilSuburbs <- na.omit(councilSuburbs)
councilSuburbs <- councilSuburbs[!duplicated(councilSuburbs[,c("Suburb", "CouncilArea")]),]
councilSuburbs <- councilSuburbs[!duplicated(councilSuburbs$Suburb), ]
testMerge <- merge(testMerge, councilSuburbs, by="Suburb")
testMerge$CouncilArea <- ifelse(is.na(testMerge$CouncilArea.x), testMerge$CouncilArea.y, testMerge$CouncilArea.x)

# drop the median columns. Address, seller, and method of sale don't have much to do with the value of a house
testMerge <- subset(testMerge, select = -c(Address, medianArea, medianYear, medianCar, medianYearType, Bedroom2, CouncilArea.x, CouncilArea.y))
testMerge <- distinct(testMerge)
melbourne <- testMerge

factor.list.sub <- c("CouncilArea","Postcode","Regionname","SellerG","Suburb")
for (f in factor.list.sub) {
        x <- paste("agg_",f, sep="")
        eval(call("<-", as.name(x), aggregate(melbourne[,names(melbourne)%in% f], by = list(melbourne[,names(melbourne)%in% f]), NROW)))
        
}

##anything less than 100 frequency in councilarea, change to other
consolidate <- 100
for (i in 1:NROW(agg_CouncilArea)){
        if (agg_CouncilArea[i,2] <= consolidate){
                melbourne$CouncilArea <- as.character(melbourne$CouncilArea)
                melbourne$CouncilArea[melbourne$CouncilArea == agg_CouncilArea[i,1] & !is.na(melbourne$CouncilArea)] <- "Other"
                melbourne$CouncilArea <- as.factor(melbourne$CouncilArea)
        }
        
        
}
##anything less than 25 frequency in post code, change to other
consolidate <- 25
for (i in 1:NROW(agg_Postcode)){
        if (agg_Postcode[i,2] <= consolidate){
                melbourne$Postcode <- as.character(melbourne$Postcode)
                melbourne$Postcode[melbourne$Postcode == agg_Postcode[i,1] & !is.na(melbourne$Postcode)] <- "Other"
                melbourne$Postcode <- as.factor(melbourne$Postcode)
        }
        
        
}
##anything less than 20 frequency in seller, change to other
consolidate <- 20
for (i in 1:NROW(agg_SellerG)){
        if (agg_SellerG[i,2] <= consolidate){
                melbourne$SellerG <- as.character(melbourne$SellerG)
                melbourne$SellerG[melbourne$SellerG == agg_SellerG[i,1] & !is.na(melbourne$SellerG)] <- "Other"
                melbourne$SellerG <- as.factor(melbourne$SellerG)
        }
        
        
}
##anything less than 20 frequency in suburb, change to other
consolidate <- 20
for (i in 1:NROW(agg_Suburb)){
        if (agg_Suburb[i,2] <= consolidate){
                melbourne$Suburb <- as.character(melbourne$Suburb)
                melbourne$Suburb[melbourne$Suburb == agg_Suburb[i,1] & !is.na(melbourne$Suburb)] <- "Other"
                melbourne$Suburb <- as.factor(melbourne$Suburb)
        }

}


# view if any null columns
colSums(is.na(melbourne))

melbourne$YearBuilt<-round(as.numeric(melbourne$YearBuilt), 0)

# ===============================================================================
# There are multiple date formats in the date column {17/09/2016, 4/03/2017, etc.}
# We can standardize the date formatting with the lubridate package 
# ===============================================================================
mdy <- mdy(melbourne$Date)
dmy <- dmy(melbourne$Date)
mdy[is.na(mdy)] <- dmy[is.na(mdy)]
melbourne$Date <- mdy
melbourne$Month <- month(melbourne$Date)
melbourne$Year <- year(melbourne$Date)

# arrange by date for time series analysis purposes
melbourne <- arrange(melbourne, Date)

melbourneMassiveOutliersRemoved <- melbourne[!melbourne$BuildingArea > quantile(melbourne$BuildingArea, probs = c(.98)),]
melbourneMassiveOutliersRemoved <- melbourneMassiveOutliersRemoved[!melbourneMassiveOutliersRemoved$Landsize > quantile(melbourne$Landsize, probs = c(.98)),]
melbourneMassiveOutliersRemoved <- melbourneMassiveOutliersRemoved %>% filter(YearBuilt > 1750)

# The most expensive house in the data is 9M but every house on that street is 800-1.2M so it's supposed to be 900.
melbourneMassiveOutliersRemoved$Price[10707]=900000

colSums(is.na(melbourneMassiveOutliersRemoved))
melbourneMassiveOutliersRemoved = subset(melbourneMassiveOutliersRemoved, select = -c(X) )
# export to a cleaned csv file
write.csv(melbourneMassiveOutliersRemoved, file="./melbourne_cleaned.csv")