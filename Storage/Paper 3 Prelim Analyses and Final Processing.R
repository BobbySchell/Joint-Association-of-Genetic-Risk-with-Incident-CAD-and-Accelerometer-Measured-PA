# -----------------
# Preliminary analyses for paper 3
# AND creating TIME variable and INCIDENT CVD
# loess graphs by genetic risk
# -----------------


# -----------
# Left to do before finalizing results:
# TURN INTO INCIDENT CVD AND CALCULATE TIME W APPROPRIATE CENSORING
# Kinship coefficients screening (COULD potentially restrict to those used for PC calc...)
# PRS does NOT currently account for LD - could FURTHER improve performance (1.40 instead of 1.64) - LDPred
# MAKE WORKFLOW CLEAR - Make into an RMD file for Github
# -----------


dx download  PCsforCAD.csv
dx download SamplePGS.sscore
dx download FULLPHENODATAALLANCESTRIES.csv
dx download FinalCADDatasetPA.csv




PGS <- read.table("SamplePGS.sscore")
FINALDATA <- read.csv("FULLPHENODATAALLANCESTRIES.csv")
PCs <- read.csv("PCsforCAD.csv")
PAData <- read.csv("FinalCADDatasetPA.csv")


dim(PGS)
# 2 columns x 488377 rows - sounds right


# Renaming columns
colnames(PGS) <- c("eid", "PGS")


# Merging PGS w/ FINALDATA
PGSFINALDATA <- merge(PGS, FINALDATA, by = "eid", all = F)
# 486,432 x 92 - Worked!


# Restricting to only European ancestry
summary(as.factor(PGSFINALDATA$Genetic.Ethnic.Grouping))
# Either Caucasian or not... 408,168 Caucasian

PGSFINALDATAWHITE <- subset(PGSFINALDATA, Genetic.Ethnic.Grouping == "Caucasian")

dim(PGSFINALDATAWHITE)
# 408,168 x 92 variables



# Merging this dataset w/ PCs
PGSFINALDATAWHITEPCs <- merge(PGSFINALDATAWHITE, PCs, by = "eid", all = F)
# 408168 x 133

# NOW standardizing PGS to make interpretable on correct scale
PGSFINALDATAWHITEPCs$StandPGS <- scale(PGSFINALDATAWHITEPCs$PGS)



# ---------
# Merging the rest w/ PA data
# Have to drop wrong CAD, Status, and Time variables
# ---------

varnames <- c("CensorDate", "StatBinary", "TimeBMIYears", "Status", "BMIDate", "TimeBMI", "TimeAccel", "AgeBaselineBMI", "TimeAccelYears", "CADDate")

PAData <- PAData[ , -which(names(PAData) %in% varnames)]

PAData$eid <- PAData$ID


dat <- merge(PGSFINALDATAWHITEPCs, PAData, by = "eid", all = F)
# 76,477 x 265


dat$Time <- ifelse(is.na(dat$CADDate) == FALSE, as.Date(dat$CADDate) - as.Date(dat$AccelDate), NA)



# -------------
# GETTING FOLLOW UP TIME RIGHT!!!!!!!!
# -------------

# FIRST get summary of inds by region
summary(as.factor(dat$REGION))

# ORIGINALLY:
# dataset$CensorDate <- as.Date("2020-01-01")

install.packages('data.table')
library(data.table)
# Had to use data.table's fifelse command to get it to stop coercing dates to numeric...

# CORRECTING censoring by region
dat$date_cens <- fifelse(dat$REGION.x == "Scotland", as.Date("2021-07-31"),
                              fifelse(dat$REGION.x == "Wales", as.Date("2018-02-28"), as.Date("2021-09-30")))


summary(as.Date(dat$date_cens))




# Finishing Time Variable based on WHEN things happened
# IF not dead, CVD, or CENSORED (ignore) then time out is 2022
# o.w. time is CVD -> Dead -> Censored
dat$Time <- ifelse(is.na(dat$CADDate) == FALSE, dat$Time, 
                   ifelse(is.na(dat$date_of_death_nocad) == FALSE, dat$date_of_death_nocad - as.Date(dat$AccelDate), dat$date_cens - as.Date(dat$AccelDate)))



# Recreating TimeYear Variable
dat$TimeYear <- dat$Time/365.24


# Update Status Variable accordingly

dat$StatusThree <- ifelse(is.na(dat$CADDate) == FALSE, dat$Status, 
                     ifelse(is.na(dat$date_of_death_nocad) == FALSE, 2, 0))

summary(as.factor(dat$StatusThree))
# 75102 inds censored
# 1375 inds CAD event
# WOw! Very  few cases... Does make some sense




# Removing inds who are censored BEFORE accelerometer (negative Time)
datsub <- subset(dat, Time > 0)

# 76477 x 270
# 76475 x 270
# Only lost 2 of these cases as PREVALENT CAD


# -------------
# NOW can create plots w/ regressions
# -------------


# Just creating an above and a below 50% risk group - doing this VERY SIMPLY for now
# One graph for each group... Will eventually be interesting to as a factor
# Just creating an above and a below 50% risk group
datHIGH <- subset(dat, StandPGS > 0)
datLOW <- subset(dat, StandPGS <= 0)


# CAN DO THIS FOR FACET WRAPS IN THE FUTURE
# dat$GeneticRisk <- ifelse(dat$StandPGS <= 0, 0, 1)



model <- glm(Status ~ HoursMVPA, data = datHIGH, family = binomial)

summary(model)
exp(coef(model))

logoddsHighCAD <- -3.206033 - 0.051659*datHIGH$HoursMVPA
pHighCAD = exp(logoddsHighCAD)/(1+ exp(logoddsHighCAD))


datHIGH$pHighCAD <- pHighCAD



# Plot Predicted data and original data points
ggplot(datHIGH, aes(x=HoursMVPA, y=pHighCAD)) + geom_point()


logoddsLowCAD <- -3.347776 - 0.072404*datLOW$HoursMVPA
pLowCAD = exp(logoddsLowCAD)/(1+ exp(logoddsLowCAD))

model <- glm(Status ~ HoursMVPA, data = datLOW, family = binomial)

summary(model)
exp(coef(model))


# Plot Predicted data and original data points
ggplot(datLOW, aes(x=HoursMVPA, y=pLowCAD)) + geom_point()



# ----------
# NOW putting both on SAME GRAPH
# ----------


# JUST USING TO GET SIZING RIGHT FOR PRESENTATION
# Plot the spline and error bands
plotboth <- ggplot() +
  geom_line(data = datHIGH, aes(x = HoursMVPA, y = pHighCAD, color = "High Genetic Risk Group")) + 
  geom_line(data = datLOW, aes(x = HoursMVPA, y = pLowCAD, color  = "Low Genetic Risk Group")) + labs(color = "Genetic Groups", title = "Relationship between Hours of MVPA per week and CAD Probability", subtitle = "By Genetic Risk Group", y = "Predicted Probability of Coronary Artery Disease", x = "Hours of Moderate to Vigorous PA per Week") + theme_classic()



plotboth



# -------------
# REPEAT for Vigorous Activity
# -------------


model <- glm(Status ~ HoursVigorous, data = datLOW, family = binomial)

summary(model)
exp(coef(model))


logoddsLowCAD <- -4.07030 - 0.29968*datLOW$HoursVigorous
pLowCAD = exp(logoddsLowCAD)/(1+ exp(logoddsLowCAD))


datLOW$pLowCAD <- pLowCAD




model <- glm(Status ~ HoursVigorous, data = datHIGH, family = binomial)

summary(model)
exp(coef(model))




logoddsHighCAD <- -3.70343 - 0.28160*datHIGH$HoursVigorous
pHighCAD = exp(logoddsHighCAD)/(1+ exp(logoddsHighCAD))


datHIGH$pHighCAD <- pHighCAD




# JUST USING TO GET SIZING RIGHT FOR PRESENTATION
# Plot the spline and error bands
plotboth <- ggplot() +
  geom_line(data = datHIGH, aes(x = HoursVigorous, y = pHighCAD, color = "High Genetic Risk Group")) + 
  geom_line(data = datLOW, aes(x = HoursVigorous, y = pLowCAD, color  = "Low Genetic Risk Group")) + labs(color = "Genetic Groups", title = "Relationship between Hours of Vigorous PA per week and CAD Probability", subtitle = "By Genetic Risk Group", y = "Predicted Probability of Coronary Artery Disease", x = "Hours of Vigorous PA per Week") + theme_classic()





# -------------
# REPEAT for Overall Activity
# -------------


model <- glm(Status ~ Overall.Activity, data = datLOW, family = binomial)

summary(model)
exp(coef(model))


logoddsLowCAD <- -3.007721 - 0.044627*datLOW$Overall.Activity
pLowCAD = exp(logoddsLowCAD)/(1+ exp(logoddsLowCAD))


datLOW$pLowCAD <- pLowCAD



model <- glm(Status ~ Overall.Activity, data = datHIGH, family = binomial)

summary(model)
exp(coef(model))




logoddsHighCAD <- -2.883524 - 0.034959*datHIGH$Overall.Activity
pHighCAD = exp(logoddsHighCAD)/(1+ exp(logoddsHighCAD))


datHIGH$pHighCAD <- pHighCAD




# JUST USING TO GET SIZING RIGHT FOR PRESENTATION
# Plot the spline and error bands
plotboth <- ggplot() +
  geom_line(data = datHIGH, aes(x = Overall.Activity, y = pHighCAD, color = "High Genetic Risk Group")) + 
  geom_line(data = datLOW, aes(x = Overall.Activity, y = pLowCAD, color  = "Low Genetic Risk Group")) + labs(color = "Genetic Groups", title = "Relationship between Hours of Overall PA per week and CAD Prob", subtitle = "By Genetic Risk Group", y = "Predicted Probability of Coronary Artery Disease", x = "Hours of Overall PA per Week") + theme_classic()




# -------------
# Converting overall activity to PAEE
# AND converting %MVPA and %Vigorous to % of total PAEE
# -------------


dx download  PCsforCAD.csv
dx download SamplePGS.sscore
dx download FULLPHENODATAALLANCESTRIES.csv
dx download FinalCADDatasetPA.csv

# Adding THIS dataset to get PAEE
dx download PACOHORTprocessedPAVars.csv


ProcessedPAVars <- read.csv("PACOHORTprocessedPAVars.csv")

dat <- ProcessedPAVars
# p90091 to p90158 (<= 1 mg to <= 2000)

# Creating midpoints for each of these intervals and creating PAEE via x in formula



# interval variables
blankdf <- as.data.frame(matrix(NA, nrow = 96660, ncol = 67))
n <- 1:67
colnames(blankdf) <- paste("AccelInterval", n, sep = "")


# Merging w/ overall dat
dat <- cbind(dat, blankdf)

# CORRECTION:
# Does NOT stop at 90147
# After that comes:
# p90148 - <= 1000
# p90149 - <= 1100
# p90150 - <= 1200
# p90151 - <= 1300
# p90152 - <= 1400
# p90153 - <= 1500
# p90154 - <= 1600
# p90155 - <= 1700
# p90156 - <= 1800
# p90157 - <= 1900
# p90158 - <= 2000

# Getting indices for for loop
summary(match("p90092", colnames(dat)))
summary(match("p90158", colnames(dat)))
# 265 to 331

summary(match("AccelInterval1", colnames(dat)))
summary(match("AccelInterval67", colnames(dat)))
# 428 to 494


# Using a for loop to FIRST create accurate proportions
# All variables are <=. This code transforms these inequalities to intervals
# For instance, now p90093 is proportion of time spent between 1 and 2 mg (NOT <=2 as it was before), and so on...
# NOTE: Had to fix this - was originally 56 but only actually need to do this 55 times
# ACTUALLY INCLUDING ALL REQUIRES 67 TOTAL (so 66)
for(i in 1:66){
  
  # Start w p90093 - p90092 and continue - fill AccelInterval
  # START at AccelInterval2 for indexing!
  dat[ , i+428] <- dat[ , i+265] - dat[ , i+264]
  

  
}

# Then AccelInterval1 is simply the first one
dat$AccelInterval1 <- dat$p90092


# ---------
# Have AccelInterval1 to 67 - NOW have to create x variable
# X variable will give the midpoint of each interval
# Don't actually need this to be part of DF (at first) since it's static
# So CREATE this vector of values then add to dataframe
# ----------


# SO CREATE EMPTY MATRIX W NECESSARY DIMENSIONS THEN USE (transpose presumably)
# Can make same dimensions as dat...
object <- as.data.frame(matrix(NA, nrow = 96660, ncol = 67))

# Could also do as a series of sequences - turn these vals into columns
# OH! Misspecified this originally! Intervals are not all simply /2...
# Which means I was WAY understating PAEE (makes sense given original results)
# They are what's between the first and successive interval!!!

# INSTEAD start by creating raw intervals - THEN subtract by midway point of interval!
object[ , 1:20] <- rep(seq(from = 1, to = 20, by = 1) - 0.5, each = 96660)
object[ , 21:36] <- rep(seq(from = 25, to = 100, by = 5) - 2.5, each = 96660)
object[ , 37:52] <- rep(seq(from = 125, to = 500, by = 25) - 12.5, each = 96660)
object[ , 53:67] <- rep(seq(from = 600, to = 2000, by = 100) - 50, each = 96660)





# THEN NAME ALL OF THESE COLUMNS X# THEN MERGE...
n <- 1:67
colnames(object) <- paste("X", n, sep = "")

# Merging object w/ larger DF
dat <- cbind(dat, object)


# ------
# NOW have X# variables and AccelInterval# variables
# Conversion formula - to PAEE
# Using X variables
# ------


# Creating PAEE Midpoint variables
PAEEdf <- as.data.frame(matrix(NA, nrow = 96660, ncol = 67))
n <- 1:67
colnames(PAEEdf) <- paste("PAEEMidpoints", n, sep = "")


# Merging w/ overall dat
dat <- cbind(dat, PAEEdf)

# Getting column indices for for loop
summary(match("PAEEMidpoints1", colnames(dat)))
# 562

# Getting column indices for for loop
summary(match("X1", colnames(dat)))
# 495

for(i in 1:67){
  
  # Here the first dat should be indexed starting at FIRST PAEEdf variable
  # The dat in the equation should be indexed at the first X variable
  dat[ , i+561] <- (-10.58 + 1.1176*(1.5 + .8517*dat[ , i+494]) + 2.9418*sqrt((1.5 + .8517*dat[ , i+494])) - 0.00059277*((1.5 + .8517*dat[ , i+494])^2))
  
}
# So now have all PAEE midpoint calculations and can merge THESE w/ AccelInterval variables
# AccelInterval gives fraction of time spent in interval and PAEEMidpoints gives PAEE values at diff mg midpoints


# Now combine proportion in each and PAEE then SUM ALL TOGETHER FOR IND LEVEL PAEE

# Creating PAEE Interval variables
PAEEInts <- as.data.frame(matrix(NA, nrow = 96660, ncol = 67))

n <- 1:67
colnames(PAEEInts) <- paste("PAEEInterval", n, sep = "")


# Merging w/ overall dat
dat <- cbind(dat, PAEEInts)


# Getting col indices for for loop
summary(match("PAEEInterval1", colnames(dat)))
summary(match("AccelInterval1", colnames(dat)))
summary(match("PAEEMidpoints1", colnames(dat)))
# 629
# 428
# 562

for(i in 1:67){
  
  # Here first dat should get first indexed PAEEInterval, second dat should get first AccelInterval
  # Final dat should get first PAEEMidpoints variable
  dat[ , i + 628] <- dat[ , i + 427] * dat[ , i + 561]
  
}


# Getting col indices for rowSums
summary(match("PAEEInterval1", colnames(dat)))
summary(match("PAEEInterval67", colnames(dat)))
# 629 to 695



# NOW COMBINING all of these PAEEInterval variables to get overall PAEE
dat$OverallPAEE <- rowSums(dat[ , c(629:695)])
# sum over all rows in range of PAEEInterval variables

# Truncate to zero - to avoid negative values
dat$PAEEPOS <- ifelse(dat$OverallPAEE >= 0, dat$OverallPAEE, 0)

# --------
# Creating PA Intensity variables
# --------


# Getting col indices for rowSums for MVPA
summary(match("PAEEInterval37", colnames(dat)))
summary(match("PAEEInterval67", colnames(dat)))
# PAEEInterval37 corresponds to 112.5, so PAEEInterval38 is start of > 125 mg for MVPA
# 665 to 695

# Summarizing PAEE ABOVE MVPA threshold
dat$MVPAPAEE <- rowSums(dat[ , c(666:695)])
# sum over all rows ABOVE 125 mgs for PAEEInterval


# Getting col indices for rowSums for Vigorous
summary(match("PAEEInterval48", colnames(dat)))
summary(match("PAEEInterval67", colnames(dat)))
# By my calculation 48 should correspond to 400
# 676 to 695


# NOW REPEATING FOR VIGOROUS
dat$VigorousPAEE <- rowSums(dat[ , c(677:695)])
# sum over all rows AT OR ABOVE 400 MG!!!!!!!!! for PAEEInterval
# Same logic as above to choose 677 instead

# -----------
# NOW restricting to % from MVPA and Vigorous
# -----------

dat$PercentMVPA <- dat$MVPAPAEE/dat$PAEEPOS

dat$PercentVigorous <- dat$VigorousPAEE/dat$PAEEPOS

# --------
# Converting PAEE to correct units
# Formula is in J/min/kg but converting to jK/kg/days
# -------------

dat$OverallPAEETRANSFORM <- dat$PAEEPOS*1440/1000
# Just a linear transformation, so will have no effect on % MVPA or % vigorous (hence it's fine to do here)


# --------------
# Replicating Table 1 of Dempsey Article
# Making sure my Overall Acceleration corresponds to ENMO
# Making sure my overall PAEE is consistent w/ theirs
# Making sure my % MVPA is consistent w/ theirs
# All look to be approximately correct - v close despite slightly different cohort inclusion criteria
# --------------



quantile(datMALE$OverallPAEETRANSFORM, probs = c(0.33, 0.67))
quantile(datFEMALE$OverallPAEETRANSFORM, probs = c(0.33, 0.67))
quantile(dat$OverallPAEETRANSFORM, probs = c(0.33, 0.67))
# Getting the two cutpoints
# Male = 32.57 and 42.32
# Female = 34.97 and 44.48
# Overall = 33.89 and 43.56



datMALETert1 <- subset(datMALE, datMALE$OverallPAEETRANSFORM <= 32.57)
datMALETert2 <- subset(datMALE, datMALE$OverallPAEETRANSFORM > 32.57 & datMALE$OverallPAEETRANSFORM <= 42.32)
datMALETert3 <- subset(datMALE, datMALE$OverallPAEETRANSFORM > 42.32)

datFEMALETert1 <- subset(datFEMALE, datFEMALE$OverallPAEETRANSFORM <= 34.97)
datFEMALETert2 <- subset(datFEMALE, datFEMALE$OverallPAEETRANSFORM > 34.97 & datFEMALE$OverallPAEETRANSFORM <= 44.48)
datFEMALETert3 <- subset(datFEMALE, datFEMALE$OverallPAEETRANSFORM > 44.48)

# ------
# NOW comparing summary stats to those in their Table 1
# ------

summary(datMALETert1$OverallPAEETRANSFORM)
summary(datMALETert1$OverallPAEE)
summary(datMALETert1$p90012)# Overall accelerometer average (ENMO? Looks like it)
summary(datMALETert1$PercentMVPA)


# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4334 23.4061 27.2864 26.2494 30.1469 32.5692 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.301  16.254  18.949  18.229  20.935  22.618 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.55   17.26   19.82   19.22   21.68   89.96 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.2155  0.2783  0.2814  0.3444  0.7260 


sd(datMALETert1$p90012)
sd(datMALETert1$PercentMVPA)
# 3.57 - also almost exact same as ENMO
# 0.72 - v close to SD for their PercentMVPA too



summary(datMALETert2$OverallPAEETRANSFORM)
summary(datMALETert2$OverallPAEE)
summary(datMALETert2$p90012)# Overall accelerometer average (ENMO? Looks like it)
summary(datMALETert2$PercentMVPA)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 32.57   34.90   37.27   37.32   39.72   42.32
# Not SUPER close here... 3 points lower than in paper - but seems within margin to me given sample diffs

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 22.62   24.24   25.88   25.92   27.58   29.39 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.15   24.79   26.45   26.55   28.11   77.35 
# ALSO just about perfect

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.07116 0.30825 0.36328 0.36624 0.42225 0.80355
# This one is PERFECT!

summary(datMALETert3$OverallPAEETRANSFORM)
summary(datMALETert3$OverallPAEE)
summary(datMALETert3$p90012)# Overall accelerometer average (ENMO? Looks like it)
summary(datMALETert3$PercentMVPA)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 42.32   45.26   49.03   51.37   55.01  149.07 
# A bit off - 3 points lower than mean

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.39   31.43   34.05   35.67   38.20  103.52 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 23.70   31.94   34.81   36.75   39.38   97.21
# Pretty good - like 1.5 points off

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1269  0.3951  0.4546  0.4583  0.5185  0.8887 
# Almost EXACTLY right!

# --------
# REPEAT FOR FEMALE
# --------

summary(datFEALETert1$OverallPAEETRANSFORM)
summary(datFEMALETert1$p90012)# Overall accelerometer average (ENMO? Looks like it)
summary(datFEMALETert1$PercentMVPA)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.553  25.980  29.871  28.729  32.606  34.969 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.55   18.73   21.19   20.56   22.99   35.19 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.2024  0.2606  0.2634  0.3220  0.7920 
# VERY CLOSE in ALL of these! Same pattern as in paper and all within ~ 2 (or 0.02) points!


summary(datFEALETert2$OverallPAEETRANSFORM)
summary(datFEMALETert2$p90012)# Overall accelerometer average (ENMO? Looks like it)
summary(datFEMALETert2$PercentMVPA)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 34.97   37.25   39.52   39.59   41.89   44.48 
# VERY close to perfect

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.38   25.99   27.54   27.64   29.16   47.19 
# JUST about perfect

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.08672 0.28891 0.34346 0.34550 0.39867 0.87086 
# About 2 points off - not bad

summary(datFEMALETert3$OverallPAEETRANSFORM)
summary(datFEMALETert3$OverallPAEE)
summary(datFEMALETert3$p90012)# Overall accelerometer average (ENMO? Looks like it)
summary(datFEMALETert3$PercentMVPA)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 44.48   47.41   51.14   53.13   56.58  118.46 
# Almost exactly right

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 30.89   32.92   35.51   36.90   39.29   82.26 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 23.09   32.87   35.54   37.13   39.58   97.11 
# Almost exact

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1263  0.3786  0.4378  0.4390  0.4978  0.7991
# MUCH higher than for female in Table
# Could be skewed by some outliers...


# So got indices RIGHT
# Applied transformation - 1440/1000

write.csv(dat, "PACOHORTprocessedPAVarsPAEE.csv")

dx upload PACOHORTprocessedPAVarsPAEE.csv