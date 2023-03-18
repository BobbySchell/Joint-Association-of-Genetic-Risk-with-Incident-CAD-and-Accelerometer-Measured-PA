# ----------
# Final Data Processing for Analysis
# Adding time, getting incident CAD right, processing covariates, etc...
# 2/23/2023

# Rerun to create final dataset on 3/5/2023
# To recode '1' factors to NA
# AND making sure covariates are interpretable and on useful scale
# AND REMAKING log MVPA and Vigorous PA
# ----------



dx download FINALIZEDPADATASET.csv


# Reading in this full dataset
Dataset <- read.csv("FINALIZEDPADATASET.csv")


# FIRST need to standardize PRS 
Dataset$StandPGS <- scale(Dataset$PGS)

install.packages('lubridate')
library(lubridate)

# First recode variable that shows first date started wearing accelerometer
Dataset$AccelDate <- Dataset$p90010


# Age should be RELATIVE TO ACCELEROMETER DATE AND DOB... (DOB and AccelDate)
Dataset$AgeBaseline <- as.Date(Dataset$AccelDate) - as.Date(Dataset$DOB)

summary(as.numeric(Dataset$AgeBaseline))

# This gives us age in days - convert to years
summary(as.numeric(Dataset$AgeBaseline)/365.25)

Dataset$AgeBaseline <- Dataset$AgeBaseline/365.25

summary(as.numeric(Dataset$AgeBaseline))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    43.50   56.23   63.44   62.35   68.56   78.71 



# ----
# CENSORING DATE CODE
# Dates based on: https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates
# Wales = 31 March 2016
# England = 30 Sept 2021
# Scotland = 31 July 2021
# ----

summary(as.factor(Dataset$REGION))
# Already converted REGION variable - using this

ind_wales <- Dataset$REGION %in% c("Wales")
ind_scotland <- Dataset$REGION %in% c("Scotland")


# Note that if hospital and death records have different censoring dates, we use the earlier one
Dataset$date_cens <- "2021-09-30"
Dataset$date_cens[ind_wales] <- "2016-03-31" 
Dataset$date_cens[ind_scotland] <- "2021-07-31"
Dataset$date_cens <- as.Date(Dataset$date_cens)


# Now have all necessary variables for Time (CADDate, AccelDate, date_cens, date_of_death_nocad)
# NOTE: Have NO date_of_death_nocad records in Accel dataset (possibly just the case...)
# ********CONFIRM THIS IS CORRECT BY COMPARING TO FULL PHENO DATASET!!!!!*****
# Not implausible since by taking part in this had to be alive at least until 2013-2015ish...

Dataset$CADDate <- as.Date(Dataset$CADDate)
Dataset$AccelDate <- as.Date(Dataset$AccelDate)
Dataset$date_cens <- as.Date(Dataset$date_cens)
Dataset$date_of_death_nocad <- as.Date(Dataset$date_of_death_nocad)

# Creating Time Variable - and get CAD/Censoring/Death right (CADDate, AccelDate, date_cens, date_of_death_nocad)
# Time accumulates until CAD -> Dead -> Censored

# First define Time if CADDate exists
Dataset$Time <- ifelse(is.na(Dataset$CADDate) == FALSE, as.Date(Dataset$CADDate) - as.Date(Dataset$AccelDate), NA)

# Now Time goes until CAD or date of death, or date censored
Dataset$Time <- ifelse(is.na(Dataset$CADDate) == FALSE, Dataset$Time, 
                   ifelse(is.na(Dataset$date_of_death_nocad) == FALSE, Dataset$date_of_death_nocad - as.Date(Dataset$AccelDate), Dataset$date_cens - as.Date(Dataset$AccelDate)))


# Recreating TimeYear Variable
Dataset$TimeYear <- Dataset$Time/365.24
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -31.240   6.229   6.806   6.273   7.335   8.332


# Update Status Variable accordingly
# If they experienced CAD, keep as 1 (o.w. 2 for non-CAD death or 0 for censoring)
Dataset$StatusThree <- ifelse(is.na(Dataset$CADDate) == FALSE, Dataset$Status, 
                          ifelse(is.na(Dataset$date_of_death_nocad) == FALSE, 2, 0))

summary(as.factor(Dataset$StatusThree))
# CAD = 3609
# Censored = 75,845


dim(Dataset)
# 79454 x 909


# UPDATE status to include ONLY INCIDENT CAD - make note of change to Status variable and dataset
# Removing inds who are censored BEFORE accelerometer (negative Time)
datsub <- subset(Dataset, Time > 0)

dim(datsub)
# 77474 x 909

summary(as.factor(datsub$Status))
# Censored = 75845
# CAD cases = 1629


# --------
# Make sure covariates are from closest time to accelerometer start date
# Compare dates of instance vs accelerometer start date (most likely latest)
# Use Date Attending Assess Center Inst 0 to 3 to make this distinction
# Must do for all except time-invariant
# --------

# ---------
# FIRST - MAKE SURE RIGHT INSTANCE IS SELECTED
# ---------
# Covariates:
# Employment Status
# Townsend Index (time-invariant)
# Educational Attainment
# F & V Intake (combo of 4 variables)
# Processed Red Meat Intake
# Added Salt Intake
# Oily Fish Consumption
# Alcohol Intake
# Sleep Duration
# Smoking Status
# Parental History of CVD
# Mobility Issues (time-invariant)
# Meds for cholesterol/blood pressure/diabetes
# Body Mass Index

# Code to default to right instance
# This is the nearest NON-MISSING instance to AccelDate
# Closest date but BEFORE PA!!!!! (gets complex o.w. in my view)

# First days between dates attending assessment center and AccelDate
datsub$Inst0Time <- as.Date(datsub$AccelDate) - as.Date(datsub$Date.Attending.Assess.Center.Inst.0)
datsub$Inst1Time <- as.Date(datsub$AccelDate) - as.Date(datsub$Date.Attending.Assess.Center.Inst.1)
datsub$Inst2Time <- as.Date(datsub$AccelDate) - as.Date(datsub$Date.Attending.Assess.Center.Inst.2)
datsub$Inst3Time <- as.Date(datsub$AccelDate) - as.Date(datsub$Date.Attending.Assess.Center.Inst.3)

summary(as.numeric(datsub$Inst0Time))
summary(as.numeric(datsub$Inst1Time))
summary(as.numeric(datsub$Inst2Time))
summary(as.numeric(datsub$Inst3Time))

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1017    1792    2067    2076    2362    3528 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  183.0   667.0   779.0   787.1   905.0  1640.0   70569 

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-3260.0 -1656.0 -1228.0 -1146.2  -571.5   801.0   60215 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -3079   -2309   -2020   -2024   -1696   -1304   75661 

# ------
# Looks like instances 2 and 3 are almost entirely AFTER accelerometer wear time started (and largely missing)
# ------

# To keep temporal ordering right will restrict to ONLY those occurring BEFORE wear time started
datsub$Inst2Time <- ifelse(datsub$Inst2Time > 0, datsub$Inst2Time, NA)
datsub$Inst3Time <- ifelse(datsub$Inst3Time > 0, datsub$Inst3Time, NA)

# Convert from tim variable to numeric
datsub$Inst0Time <- as.numeric(datsub$Inst0Time)
datsub$Inst1Time <- as.numeric(datsub$Inst1Time)
datsub$Inst2Time <- as.numeric(datsub$Inst2Time)
datsub$Inst3Time <- as.numeric(datsub$Inst3Time)

# Now calculating nearest non-negative instance to accelerometer start date
datsub$NearestInstance <- apply(datsub[ , c("Inst0Time", "Inst1Time", "Inst2Time")], 1, FUN = min, na.rm = TRUE)
# REMOVING Inst3Time - all occurred AFTERWARD

summary(datsub$NearestInstance)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1    1660    1987    1918    2296    3134 


# Creating an index variable to know which instance to choose
datsub$InstanceChoice <- ifelse((datsub$NearestInstance == datsub$Inst0Time) == TRUE, 0,
                                ifelse((datsub$NearestInstance == datsub$Inst1Time) == TRUE, 1,
                                       ifelse((datsub$NearestInstance == datsub$Inst2Time) == TRUE, 2,NA)))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   0.000   0.000   0.098   0.000   2.000     507

# Not immediately clear to me how there are ANY missings with these values...
# Looks like large number of missing values for instance 1 might have driven this

# If we force InstanceChoice to equal 2, removes missingness - suggests all missing had an instance 2
# By nature of removing all negatives earlier, this is by default the most recent visit if it exists
# datsub$InstanceChoice <- ifelse(is.na(datsub$InstanceChoice) == TRUE, datsub$Inst2Time, datsub$InstanceChoice)
# Was used to CONFIRM that these were all available (no missingness)

datsub$InstanceChoice <- ifelse(is.na(datsub$InstanceChoice) == TRUE, 2, datsub$InstanceChoice)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.1105  0.0000  2.0000 


summary(as.factor(datsub$InstanceChoice))
# 0 70063
# 1 6264
# 2 1147
# These values all make sense!


# ----------
# Now using chosen instance to select applicable variable (surely much cleaner ways to do this)
# Technically instance 3 won't be chosen ever
# ----------

# Employment Status
datsub$EmploymentStatus_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$EmploymentStatus_Inst3,
                                             ifelse(datsub$InstanceChoice == 2, datsub$EmploymentStatus_Inst2,
                                                    ifelse(datsub$InstanceChoice == 1, datsub$EmploymentStatus_Inst1,
                                                           ifelse(datsub$InstanceChoice == 0, datsub$EmploymentStatus_Inst0, NA))))


# Cooked Veggies
datsub$CookedVeggie_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$CookedVeggie_Inst3,
                                             ifelse(datsub$InstanceChoice == 2, datsub$CookedVeggie_Inst2,
                                                    ifelse(datsub$InstanceChoice == 1, datsub$CookedVeggie_Inst1,
                                                           ifelse(datsub$InstanceChoice == 0, datsub$CookedVeggie_Inst0, NA))))



# Raw Veggies
datsub$RawVeggie_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$RawVeggie_Inst3,
                                         ifelse(datsub$InstanceChoice == 2, datsub$RawVeggie_Inst2,
                                                ifelse(datsub$InstanceChoice == 1, datsub$RawVeggie_Inst1,
                                                       ifelse(datsub$InstanceChoice == 0, datsub$RawVeggie_Inst0, NA))))


# Fresh Fruit
datsub$FreshFruit_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$FreshFruit_Inst3,
                                         ifelse(datsub$InstanceChoice == 2, datsub$FreshFruit_Inst2,
                                                ifelse(datsub$InstanceChoice == 1, datsub$FreshFruit_Inst1,
                                                       ifelse(datsub$InstanceChoice == 0, datsub$FreshFruit_Inst0, NA))))


# Dried Fruit
datsub$DriedFruit_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$DriedFruit_Inst3,
                                       ifelse(datsub$InstanceChoice == 2, datsub$DriedFruit_Inst2,
                                              ifelse(datsub$InstanceChoice == 1, datsub$DriedFruit_Inst1,
                                                     ifelse(datsub$InstanceChoice == 0, datsub$DriedFruit_Inst0, NA))))


# Processed Meat
datsub$ProcMeat_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$ProcMeat_Inst3,
                                       ifelse(datsub$InstanceChoice == 2, datsub$ProcMeat_Inst2,
                                              ifelse(datsub$InstanceChoice == 1, datsub$ProcMeat_Inst1,
                                                     ifelse(datsub$InstanceChoice == 0, datsub$ProcMeat_Inst0, NA))))


# Salt Intake
datsub$Salt_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$Salt_Inst3,
                                       ifelse(datsub$InstanceChoice == 2, datsub$Salt_Inst2,
                                              ifelse(datsub$InstanceChoice == 1, datsub$Salt_Inst1,
                                                     ifelse(datsub$InstanceChoice == 0, datsub$Salt_Inst0, NA))))


# Oily Fish Intake
datsub$OilyFish_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$OilyFish_Inst3,
                                       ifelse(datsub$InstanceChoice == 2, datsub$OilyFish_Inst2,
                                              ifelse(datsub$InstanceChoice == 1, datsub$OilyFish_Inst1,
                                                     ifelse(datsub$InstanceChoice == 0, datsub$OilyFish_Inst0, NA))))

# Alcohol Intake
datsub$AlcIntake_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$AlcIntake_Inst3,
                                       ifelse(datsub$InstanceChoice == 2, datsub$AlcIntake_Inst2,
                                              ifelse(datsub$InstanceChoice == 1, datsub$AlcIntake_Inst1,
                                                     ifelse(datsub$InstanceChoice == 0, datsub$AlcIntake_Inst0, NA))))

# Sleep Duration
datsub$SleepDur_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$SleepDur_Inst3,
                                       ifelse(datsub$InstanceChoice == 2, datsub$SleepDur_Inst2,
                                              ifelse(datsub$InstanceChoice == 1, datsub$SleepDur_Inst1,
                                                     ifelse(datsub$InstanceChoice == 0, datsub$SleepDur_Inst0, NA))))


# Smoking Status
datsub$SmokStat_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$SmokStat_Inst3,
                                       ifelse(datsub$InstanceChoice == 2, datsub$SmokStat_Inst2,
                                              ifelse(datsub$InstanceChoice == 1, datsub$SmokStat_Inst1,
                                                     ifelse(datsub$InstanceChoice == 0, datsub$SmokStat_Inst0, NA))))


# Pack Years - has missingness (makes sense for non-smokers)
datsub$PackYears_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$PackYears_Inst3,
                                       ifelse(datsub$InstanceChoice == 2, datsub$PackYears_Inst2,
                                              ifelse(datsub$InstanceChoice == 1, datsub$PackYears_Inst1,
                                                     ifelse(datsub$InstanceChoice == 0, datsub$PackYears_Inst0, NA))))

# Father Illness
datsub$FatherIll_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$FatherIll_Inst3,
                                       ifelse(datsub$InstanceChoice == 2, datsub$FatherIll_Inst2,
                                              ifelse(datsub$InstanceChoice == 1, datsub$FatherIll_Inst1,
                                                     ifelse(datsub$InstanceChoice == 0, datsub$FatherIll_Inst0, NA))))

# Mother Illness
datsub$MotherIll_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$MotherIll_Inst3,
                                      ifelse(datsub$InstanceChoice == 2, datsub$MotherIll_Inst2,
                                             ifelse(datsub$InstanceChoice == 1, datsub$MotherIll_Inst1,
                                                    ifelse(datsub$InstanceChoice == 0, datsub$MotherIll_Inst0, NA))))


# Medications
datsub$Meds_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$Meds_Inst3,
                                      ifelse(datsub$InstanceChoice == 2, datsub$Meds_Inst2,
                                             ifelse(datsub$InstanceChoice == 1, datsub$Meds_Inst1,
                                                    ifelse(datsub$InstanceChoice == 0, datsub$Meds_Inst0, NA))))


# Body  Mass Index
datsub$BMI_InstChosen <- ifelse(datsub$InstanceChoice == 3, datsub$Measured.BMI.Inst.3,
                                ifelse(datsub$InstanceChoice == 2, datsub$Measured.BMI.Inst.2,
                                       ifelse(datsub$InstanceChoice == 1, datsub$Measured.BMI.Inst.1,
                                              ifelse(datsub$InstanceChoice == 0, datsub$Measured.BMI.Inst.0, NA))))
# 148 had missing values


# ------------
# NOW check missingness of variables at chosen Instances
# ------------

summary(is.na(datsub$EmploymentStatus_InstChosen))
summary(is.na(datsub$CookedVeggie_InstChosen))
summary(is.na(datsub$RawVeggie_InstChosen))
summary(is.na(datsub$FreshFruit_InstChosen))
summary(is.na(datsub$DriedFruit_InstChosen))
summary(is.na(datsub$ProcMeat_InstChosen))
summary(is.na(datsub$Salt_InstChosen))
summary(is.na(datsub$OilyFish_InstChosen))
summary(is.na(datsub$AlcIntake_InstChosen))
summary(is.na(datsub$SleepDur_InstChosen))
summary(is.na(datsub$SmokStat_InstChosen))
summary(is.na(datsub$PackYears_InstChosen))
summary(is.na(datsub$FatherIll_InstChosen))
summary(is.na(datsub$MotherIll_InstChosen))
summary(is.na(datsub$Meds_InstChosen))
summary(is.na(datsub$BMI_InstChosen))
# So only pack years and BMI have missing values here
# pack years = 56,131 missing
# BMI = 148 missing


# Time-invariant variables
summary(is.na(datsub$Biological.Sex))
summary(is.na(datsub$Townsend))
summary(is.na(datsub$MobilProbs))
# Only Townsend Index has missing values - 89

# ----------
# Missingness is EXTREMELY low in this dataset... So far
# ----------

summary(is.na(datsub$Measured.BMI.Inst.0))
summary(is.na(datsub$Measured.BMI.Inst.1))
summary(is.na(datsub$Measured.BMI.Inst.2))
summary(is.na(datsub$Measured.BMI.Inst.3))
# Lowest missingness is instance 0 but still 152

# If missingness existed in a covariate at a given Instance, check if non-missing at other instances
# Going off assumption 3 would be closest
# Do this on ad hoc basis depending on number of missing for different variables
# ONLY APPLIES TO BMI (for now...)
datsub$BMI_Miss <- ifelse(is.na(datsub$BMI_InstChosen) == TRUE, datsub$BMI_Inst2)
# No dice - unsurprisingly can't add any more BMI vals

# Townsend index is time invariant, so naturally can't do anything but impute here

# -------
# NOT imputing for now - MOVE TO ANALYSIS FILE
# -------


# ---------
# SECOND - PROCESS COVARIATES SO THEY ARE IN FORMAT NEEDED FOR ANALYSIS (esp. F & V Intake and Meds)
# F & V should be merged into two variables
# Meds should be DISAGGREGATED BY CONDITION
# Father Illness and Mother Illness should isolate CVD history
# Smoking Status simply is never/current/ever (confirm this works)
# NOTE: COULD parameterize education and employment status, etc. as series of dummy variables - does it matter?
# ---------

# Fruit & Vegetable intake - AS TWO SEPARATE VARS

# FIRST have to turn variables from factor to numeric
summary(as.factor(datsub$FreshFruit_InstChosen))
# Making sure to REMOVE the indecipherable 1's
unique(as.character(datsub$FreshFruit_InstChosen))
# Have TWO 1 factors here (one with 40 and one with 19,710)
# CANT RECODE THESE FOR FRUIT N VEG BUT CAN FOR OTHERS


# Most are numeric but recoding less than 1 as 0.5 and Do Not Know to NA (same w/ prefer not to answer)
datsub$FreshFruit_InstChosen <- ifelse(datsub$FreshFruit_InstChosen == "Less than one", "0.5", datsub$FreshFruit_InstChosen)
datsub$FreshFruit_InstChosen <- ifelse(datsub$FreshFruit_InstChosen == "Do not know", NA, datsub$FreshFruit_InstChosen)
datsub$FreshFruit_InstChosen <- ifelse(datsub$FreshFruit_InstChosen == "Prefer not to answer", NA, datsub$FreshFruit_InstChosen)


datsub$FreshFruit_InstChosen <- as.numeric(datsub$FreshFruit_InstChosen)
# Now have 153 NAs

# Same logic for DriedFruit
summary(as.factor(datsub$DriedFruit_InstChosen))

# Most are numeric but recoding less than 1 as 0.5 and Do Not Know to NA (same w/ prefer not to answer)
datsub$DriedFruit_InstChosen <- ifelse(datsub$DriedFruit_InstChosen == "Less than one", "0.5", datsub$DriedFruit_InstChosen)
datsub$DriedFruit_InstChosen <- ifelse(datsub$DriedFruit_InstChosen == "Do not know", NA, datsub$DriedFruit_InstChosen)
datsub$DriedFruit_InstChosen <- ifelse(datsub$DriedFruit_InstChosen == "Prefer not to answer", NA, datsub$DriedFruit_InstChosen)


datsub$DriedFruit_InstChosen <- as.numeric(datsub$DriedFruit_InstChosen)
# Far more - 490 NAs here

# Fresh Fruit and Dried Fruit questions are easily combined - both ask how many pieces of fruit eaten per day
datsub$Fruit <- datsub$FreshFruit_InstChosen + datsub$DriedFruit_InstChosen

summary(datsub$Fruit)
# 549 NAs from this combination

# Same process for VEGGIES
summary(as.factor(datsub$CookedVeggie_InstChosen))
summary(as.factor(datsub$RawVeggie_InstChosen))


# Most are numeric but recoding less than 1 as 0.5 and Do Not Know to NA (same w/ prefer not to answer)
datsub$CookedVeggie_InstChosen <- ifelse(datsub$CookedVeggie_InstChosen == "Less than one", "0.5", datsub$CookedVeggie_InstChosen)
datsub$CookedVeggie_InstChosen <- ifelse(datsub$CookedVeggie_InstChosen == "Do not know", NA, datsub$CookedVeggie_InstChosen)
datsub$CookedVeggie_InstChosen <- ifelse(datsub$CookedVeggie_InstChosen == "Prefer not to answer", NA, datsub$CookedVeggie_InstChosen)


datsub$CookedVeggie_InstChosen <- as.numeric(datsub$CookedVeggie_InstChosen)
# Now have 153 NAs

# Same logic for DriedFruit
summary(as.factor(datsub$RawVeggie_InstChosen))

# Most are numeric but recoding less than 1 as 0.5 and Do Not Know to NA (same w/ prefer not to answer)
datsub$RawVeggie_InstChosen <- ifelse(datsub$RawVeggie_InstChosen == "Less than one", "0.5", datsub$RawVeggie_InstChosen)
datsub$RawVeggie_InstChosen <- ifelse(datsub$RawVeggie_InstChosen == "Do not know", NA, datsub$RawVeggie_InstChosen)
datsub$RawVeggie_InstChosen <- ifelse(datsub$RawVeggie_InstChosen == "Prefer not to answer", NA, datsub$RawVeggie_InstChosen)


datsub$RawVeggie_InstChosen <- as.numeric(datsub$RawVeggie_InstChosen)
# Far more - 490 NAs here

# Fresh Fruit and Dried Fruit questions are easily combined - both ask how many pieces of fruit eaten per day
datsub$Veggie <- datsub$CookedVeggie_InstChosen + datsub$RawVeggie_InstChosen

summary(datsub$Veggie)
# 665 NAs from this combination (note would be NA if missing EITHER category)





# Separating out medications
summary(as.factor(datsub$Meds_InstChosen))



datsub$CholMeds <- ifelse(grepl("Cholesterol lowering medication", datsub$Meds_InstChosen) == TRUE, 1, 0)
# Variable for whether cholesterol meds are taken (IRRESPECTIVE to other meds)

datsub$BPMeds <- ifelse(grepl("Blood pressure medication", datsub$Meds_InstChosen), 1, 0)
# Variable for whether BP meds are taken (IRRESPECTIVE to other meds)

summary(datsub$CholMeds)
summary(datsub$BPMeds)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.0813  0.0000  1.0000 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.0876  0.0000  1.0000 


# Father history of CVD
summary(as.factor(datsub$FatherIll_InstChosen))

datsub$FatherHeartDisease <- ifelse(grepl("Heart disease", datsub$FatherIll_InstChosen), 1, 0)
# Set as 1 if includes Heart Disease anywhere in set, 0 o.w.

# Mother history of CVD
datsub$MotherHeartDisease <- ifelse(grepl("Heart disease", datsub$MotherIll_InstChosen), 1, 0)
# Set as 1 if includes Heart Disease anywhere in set, 0 o.w.





# -------
# Coding in missingness for reporting of do not know or prefer not to answer in the rest of the vars
# NO such  category for:
# Mobility Problems, Biological Sex, BMI, Pack Years
# -------


summary(as.factor(datsub$EmploymentStatus_InstChosen))
# Will recode "Prefer not to answer" and "None of the above" to NAs

datsub$EmploymentStatus_InstChosen <- ifelse(grepl("None of the above", datsub$EmploymentStatus_InstChosen), NA, datsub$EmploymentStatus_InstChosen)
datsub$EmploymentStatus_InstChosen <- ifelse(grepl("Prefer not to answer", datsub$EmploymentStatus_InstChosen), NA, datsub$EmploymentStatus_InstChosen)
# Recodes if it contains EITHER statement (even if others are included)
# 504 NAs now
# Keeping as categorical factor

# datsub$EmploymentStatus_InstChosen <- ifelse(grepl("1", datsub$EmploymentStatus_InstChosen), NA, datsub$EmploymentStatus_InstChosen)

# In paid employment/self-employed, unemployed, retired as categories
datsub$EmploymentStatus_InstChosen <- ifelse(grepl("Unemployed", datsub$EmploymentStatus_InstChosen), "Unemployed", datsub$EmploymentStatus_InstChosen)
datsub$EmploymentStatus_InstChosen <- ifelse(grepl("In paid employment or self-employed", datsub$EmploymentStatus_InstChosen), "In paid employment or self-employed", datsub$EmploymentStatus_InstChosen)
datsub$EmploymentStatus_InstChosen <- ifelse(grepl("Retired", datsub$EmploymentStatus_InstChosen), "Retired", datsub$EmploymentStatus_InstChosen)
# 46830 employed, 25096 retired, 1007 unemployed

# Adding the rest of the factors as OTHER
# INCLUDES students, looking after home/family, volunteer work, disability
datsub$EmploymentStatus_InstChosen <- ifelse(datsub$EmploymentStatus_InstChosen == "Retired", "Retired", 
                                             ifelse(datsub$EmploymentStatus_InstChosen == "Unemployed", "Unemployed",
                                                    ifelse(datsub$EmploymentStatus_InstChosen == "In paid employment or self-employed", "In paid employment or self-employed", "Other")))


datsub$EmploymentStatus_InstChosen <- as.factor(datsub$EmploymentStatus_InstChosen)
# DONE THIS WAY DO NOT HAVE TO WORRY ABOUT 1

summary(as.factor(datsub$ProcMeat_InstChosen))
# Will recode Prefer not to answer and Do not know to NAs
# AND removing extraneous 1 - ONLY WAY is by recoding the rest



#datsub$ProcMeat_InstChosen <- ifelse(datsub$ProcMeat_InstChosen == "Do not know", NA, datsub$ProcMeat_InstChosen)
#datsub$ProcMeat_InstChosen <- ifelse(datsub$ProcMeat_InstChosen == "Prefer not to answer", NA, datsub$ProcMeat_InstChosen)
# Keeping as categorical factor variable here
# datsub$ProcMeat_InstChosen <- ifelse(datsub$ProcMeat_InstChosen == "1", NA, datsub$ProcMeat_InstChosen)

# RECODING to be MIDPOINT of categories
# INTERPRET AS PROCESSED MEAT CONSUMED PER WEEK
datsub$ProcMeat_InstChosen <- ifelse(datsub$ProcMeat_InstChosen == "2-4 times a week", 3,
                                     ifelse(datsub$ProcMeat_InstChosen == "5-6 times a week", 5.5,
                                            ifelse(datsub$ProcMeat_InstChosen == "Less than once a week", 0.5,
                                                   ifelse(datsub$ProcMeat_InstChosen == "Never", 0,
                                                          ifelse(datsub$ProcMeat_InstChosen == "Once a week", 1,
                                                                 ifelse(datsub$ProcMeat_InstChosen == "Once or more daily", 7, NA))))))


summary(as.factor(datsub$ProcMeat_InstChosen))
# 0 7934
# 0.5 25386
# 1 21896
# 3 19350
# 5.5 2288
# 7 513
# NA's 107



datsub$ProcMeat_InstChosen <- as.factor(datsub$ProcMeat_InstChosen)



summary(as.factor(datsub$Salt_InstChosen))
# JUST recoding Prefer not to answer to NA
# To remove 1 and prefer not to answers:
# ANSWERS ARE:
# Never/rarely, Sometimes, Usually, Always as ordered
# THEN THE REST ARE NA
# Understand simply as 'do they add salt'

#datsub$Salt_InstChosen <- ifelse(datsub$Salt_InstChosen == "Prefer not to answer", NA, datsub$Salt_InstChosen)
#datsub$Salt_InstChosen <- ifelse(datsub$Salt_InstChosen == "1", NA, datsub$Salt_InstChosen)

datsub$Salt_InstChosen <- ifelse(datsub$Salt_InstChosen == "Never/rarely", 0,
                                 ifelse(datsub$Salt_InstChosen == "Sometimes", 1,
                                        ifelse(datsub$Salt_InstChosen == "Usually", 2,
                                               ifelse(datsub$Salt_InstChosen == "Always", 3, NA))))


datsub$Salt_InstChosen <- as.factor(datsub$Salt_InstChosen)


summary(as.factor(datsub$OilyFish_InstChosen))
# Recoding to NA if Prefer not to answer
# USING EXACT SAME PROCESS AS FOR PROCESSED MEAT TO RECODE 1 AND PREFER
# UNDERSTAND AS WEEKLY OILY FISH CONSUMPTION
# datsub$OilyFish_InstChosen <- ifelse(datsub$OilyFish_InstChosen == "Prefer not to answer", NA, datsub$OilyFish_InstChosen)
#datsub$OilyFish_InstChosen <- ifelse(datsub$OilyFish_InstChosen == "1", NA, datsub$OilyFish_InstChosen)

datsub$OilyFish_InstChosen <- ifelse(datsub$OilyFish_InstChosen == "2-4 times a week", 3,
                                     ifelse(datsub$OilyFish_InstChosen == "5-6 times a week", 5.5,
                                            ifelse(datsub$OilyFish_InstChosen == "Less than once a week", 0.5,
                                                   ifelse(datsub$OilyFish_InstChosen == "Never", 0,
                                                          ifelse(datsub$OilyFish_InstChosen == "Once a week", 1,
                                                                 ifelse(datsub$OilyFish_InstChosen == "Once or more daily", 7, NA))))))


summary(as.factor(datsub$OilyFish_InstChosen))
# 0 7161
# 0.5 26446
# 1 30221
# 3 12804
# 5.5 509
# 7 129
# NA's 204




datsub$OilyFish_InstChosen <- as.factor(datsub$OilyFish_InstChosen)


summary(as.factor(datsub$AlcIntake_InstChosen))
# Recoding to NA if Prefer not to answer
# RECODING 1 AND TO MAKE ALC CONSUMPTION INTERPRETABLE

# Daily or almost daily, never, once or twice a week,
# one to three times a month, prefer not to answer,
# special occasions only, three or four times a week
# CODING AS YEARLY INTAKE TO START
datsub$AlcIntake_InstChosen <- ifelse(datsub$AlcIntake_InstChosen == "Daily or almost daily", 365,
                                      ifelse(datsub$AlcIntake_InstChosen == "Never", 0,
                                             ifelse(datsub$AlcIntake_InstChosen == "Once or twice a week", 78,
                                                    ifelse(datsub$AlcIntake_InstChosen == "One to three times a month", 24,
                                                           ifelse(datsub$AlcIntake_InstChosen == "Special occasions only", 12,
                                                                  ifelse(datsub$AlcIntake_InstChosen == "Three or four times a week", 182, NA))))))
# Will make VERY LITTLE difference but special occasion coding is arbitrary

# datsub$AlcIntake_InstChosen <- ifelse(datsub$AlcIntake_InstChosen == "Prefer not to answer", NA, datsub$AlcIntake_InstChosen)
#datsub$AlcIntake_InstChosen <- ifelse(datsub$AlcIntake_InstChosen == "1", NA, datsub$AlcIntake_InstChosen)

datsub$AlcIntake_InstChosen <- as.factor(datsub$AlcIntake_InstChosen)


summary(as.factor(datsub$SleepDur_InstChosen))
# Recoding Prefer not to answer and Do not know to NA
# LIKE FRUIT & VEGGIE CANNOT SEPARATE OUT 1
# AND readily interpreted

datsub$SleepDur_InstChosen <- ifelse(datsub$SleepDur_InstChosen == "Do not know", NA, datsub$SleepDur_InstChosen)
datsub$SleepDur_InstChosen <- ifelse(datsub$SleepDur_InstChosen == "Prefer not to answer", NA, datsub$SleepDur_InstChosen)
# Keeping as categorical factor variable here
# THEN convert to numeric

datsub$SleepDur_InstChosen <- as.numeric(datsub$SleepDur_InstChosen)


summary(as.factor(datsub$SmokStat_InstChosen))
# Convert Prefer not to answer to NA
# 1 to ignore
# MAKE AS Current, Never, Previous variable

# datsub$SmokStat_InstChosen <- ifelse(datsub$SmokStat_InstChosen == "Prefer not to answer", NA, datsub$SmokStat_InstChosen)
#datsub$SmokStat_InstChosen <- ifelse(datsub$SmokStat_InstChosen == "1", NA, datsub$SmokStat_InstChosen)

datsub$SmokStat_InstChosen <- ifelse(datsub$SmokStat_InstChosen == "Current", "Current",
                                     ifelse(datsub$SmokStat_InstChosen == "Never", "Never",
                                            ifelse(datsub$SmokStat_InstChosen == "Previous", "Previous", NA)))

datsub$SmokStat_InstChosen <- as.factor(datsub$SmokStat_InstChosen)


# -------------
# NOTED that all of these factor variables have a '1' category that is uninterpretable
# CONVERT this to NA as well
# Only about 40 obs per variable
# WOULD HAVE TO DO THIS BEFORE FACTOR CONVERTING FOR IT TO WORK...

# SOLVED
# --------------

#datsub$SmokStat_InstChosen <- ifelse(datsub$SmokStat_InstChosen == "1", NA, datsub$SmokStat_InstChosen)
#datsub$AlcIntake_InstChosen <- ifelse(datsub$AlcIntake_InstChosen == "1", NA, datsub$AlcIntake_InstChosen)
#datsub$OilyFish_InstChosen <- ifelse(datsub$OilyFish_InstChosen == "1", NA, datsub$OilyFish_InstChosen)
#datsub$Salt_InstChosen <- ifelse(datsub$Salt_InstChosen == "1", NA, datsub$Salt_InstChosen)
#datsub$ProcMeat_InstChosen <- ifelse(datsub$ProcMeat_InstChosen == "1", NA, datsub$ProcMeat_InstChosen)
#datsub$EmploymentStatus_InstChosen <- ifelse(grepl("1", datsub$EmploymentStatus_InstChosen), NA, datsub$EmploymentStatus_InstChosen)




# -------
# CREATE Season of Wear Covariate (based on AccelDate)
# -------

# First extract month and day from AccelDate variable
# SOLUTION was to normalize the years
datsub$YearZero <- datsub$AccelDate
year(datsub$YearZero) <- 0

datsub$YearZero <- as.Date(datsub$YearZero)

install.packages('data.table')
library(data.table)


# Making season variable
datsub$SeasonWear <- fifelse(datsub$YearZero >= '0-01-01' & datsub$YearZero <= '0-02-29' | datsub$YearZero >= '0-12-01', "Winter",
                             fifelse(datsub$YearZero >= '0-03-01' & datsub$YearZero <= '0-05-31', "Spring",
                                     fifelse(datsub$YearZero >= '0-06-01' & datsub$YearZero <= '0-08-31', "Summer", "Fall")))

summary(as.factor(datsub$SeasonWear))
# Fall 23014 Spring 17725 Summer 20322 Winter 16413


# -------
# LOG TRANSFORM MVPA AND VIGOROUS??? AT LEAST MAKE AN OPTION...
# -------

datsub$LogPercentMVPA <- log(datsub$PercentMVPA)
datsub$LogPercentVigorous <- log(datsub$PercentVigorous)



# SHOULD be write.csv
write.csv(datsub, "FINALANALYSISDATAPAPER3.csv")


# IN BASH - MAKE SURE IT'S ON BRAND NEW CELL WHEN YOU SWITCH!!!
dx upload FINALANALYSISDATAPAPER3.csv
