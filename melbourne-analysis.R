library(tidyverse)
library(dplyr)
library(stats)
library(corrplot)
library(car)
library(qpcR)
library(ggplot2)
library(pwr)
library(multcomp)

###setwd for me
setwd(dir = "C:/Users/MSachs.MSACHS-DELL/Documents/GitHub/melbourne-housing")

###define this operator for later
`%notin%` <- Negate(`%in%`)

###you need to read in your NA's, leaving stringasfactors as false, but true would save you some work later
melbourne <- read.csv("melb_data_raw.csv", stringsAsFactors = FALSE, na.strings = c(""," ","NA","NULL"), header=TRUE)
glimpse(melbourne)

# We don't need method, seller, or address.
## I would definitely not rule out the real estate agent or method yet, as both will likely have an impact on selling price (and agent especially will be relevant given our original scope)
melbourne <- subset(melbourne, select = -c(Address))


# telling R which predictors are categorical
## let's make this a little bit easier so we can also inspect for frequencies
factor.list <- c("Type","Regionname","Postcode","CouncilArea","Suburb","SellerG","Method")

for (f in factor.list) {
  x <- paste("varname_",f, sep="")
  eval(call("<-", as.name(x), unique(melbourne[,names(melbourne)%in% f])))
  melbourne[,names(melbourne)%in% f] <- as.factor(melbourne[,names(melbourne)%in% f])
}
## let's define our base classes
melbourne$Type <-relevel(melbourne$Type, ref = "u")
melbourne$Method <-relevel(melbourne$Method, ref = "S")
##let's look at frequency of our larger factor classes to explore recoding
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
##Ok, let's redefine our base class for these 4 as other
melbourne$CouncilArea <-relevel(melbourne$CouncilArea, ref = "Other")
melbourne$Postcode <-relevel(melbourne$Postcode, ref = "Other")
melbourne$SellerG <-relevel(melbourne$SellerG, ref = "Other")
melbourne$Suburb <-relevel(melbourne$Suburb, ref = "Other")

# melbourne$Type <- factor(melbourne$Type)
# melbourne$Regionname <- factor(melbourne$Regionname)
# melbourne$Postcode <- factor(melbourne$Postcode)
# melbourne$CouncilArea <- factor(melbourne$CouncilArea)
# melbourne$Suburb <- factor(melbourne$Suburb)
# melbourne$SellerG <- factor(melbourne$SellerG)
# melbourne$Method <- factor(melbourne$Method)

# telling R which predictor is a date column 
## dates are in one format.... dd/mm/yyyy
unique(melbourne$Date)
melbourne$Date <- as.Date(melbourne$Date,"%d/%m/%Y")  # - the dates are in different formats so we'll have to standardize the formatting

# Checking for null columns
colSums(is.na(melbourne))

# We can drop the 62 instances where the cars predictor is NA
## i am going to hold off on this, cars is one of those variables where imputing with the mean/median might make sense
#melbourne <- melbourne[!is.na(melbourne$Car),]

##need to conduct preliminary exploratory analysis
summary(melbourne)
##interesting....note there are building areas of 0, bathroom of 0, and a property built in year 1196. these are all a little fishy.
##let's inspect these individually
building_area_outliers <- melbourne[melbourne$BuildingArea == 0,]
building_area_outliers <- building_area_outliers[complete.cases(building_area_outliers$Price),]
summary(building_area_outliers)
bathroom_outliers <- melbourne[melbourne$Bathroom == 0,]
bathroom_outliers <- bathroom_outliers[complete.cases(bathroom_outliers$Price),]
summary(bathroom_outliers)
year_built_outliers <- melbourne[melbourne$YearBuilt <= 1900,]
year_built_outliers <- year_built_outliers[complete.cases(year_built_outliers$Price),]
summary(year_built_outliers)


###these aren't NA's but we need to decide what to with some of these



##lets take some initial scatter plots of our quantitative variables and inspect
pairs(melbourne[,names(melbourne) %notin%  cbind(factor.list,"Date")], lower.panel = NULL)

##let's look at box plot of our categoricals against price
for (f in factor.list){
  boxplot(Price~get(f),data=melbourne, main= paste0("Property Price By ",f),
          xlab=f, ylab="Price")
}

##let's take a look at the correlation plot
res1 <- cor.mtest(melbourne[,c(2,4,8,10:15,20)], conf.level = .95)
corrplot.mixed(cor(melbourne[,c(2,4,8,10:15,20)],use = "pairwise.complete.obs"), order = "original",tl.cex = 1,tl.pos = "lt",tl.col = "black", tl.srt = 50, number.cex = 1,p.mat = res1$p, sig.level = .05
)

##we have complete observations for price and rooms, so lets start our model with that
model.s.1 <- lm(Price~Rooms, data = melbourne)
summary(model.s.1)
anova(model.s.1)
###statistically significant, but does it meet our assumptions? lets check our plots
plot(model.s.1)
###hmmm, non-constant variance and non-0 mean of residuals
##box-cox?
bc <- boxcox(model.s.1,lambda = seq(-1, 1, 1/10))
bc$x[which.max(bc$y)]
###seems to advocate for a log transformation
melbourne$Rooms2 <- 1/melbourne$Rooms
plot(log(melbourne$Price)~melbourne$Rooms2)
plot(lm(log(melbourne$Price)~melbourne$Rooms2))