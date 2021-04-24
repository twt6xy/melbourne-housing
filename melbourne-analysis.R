library(lubridate)

melbourne <- read.csv("melbourne_cleaned.csv", header=TRUE)

# ==============================================================================
# The dataset contains multiple categorical variables, so create a list of them
# and tell R to turn them into a factor
# ==============================================================================

cats <- c('Type', 'Regionname', 'Postcode', 'CouncilArea', 'Suburb')
melbourne[,cats] <- lapply(melbourne[,cats], factor)

# ===============================================================================
# There are multiple date formats in the date column {17/09/2016, 4/03/2017, etc.}
# We can standardize the date formatting with the lubridate package 
# ===============================================================================

mdy <- mdy(melbourne$Date)
dmy <- dmy(melbourne$Date)
mdy[is.na(mdy)] <- dmy[is.na(mdy)]
melbourne$Date <- mdy