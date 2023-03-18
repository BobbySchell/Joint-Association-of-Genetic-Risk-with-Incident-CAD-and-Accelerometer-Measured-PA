# ------
# Uploading FINALDempseyDataset and formatting as previous Dempsey Dataset
# ------

dx download FINALDempseyData.csv

datsub <- read.csv("FINALDempseyData.csv")


# ADDED NEW VARIABLES
# START by restricting dataset to only what is needed - exposures, outcome, confounders
datsubrest <- datsub[ , c("SeasonWear", "PercentMVPA", "LogPercentMVPA", "LogPercentVigorous", "PercentVigorous", "OverallPAEETRANSFORM",
                          "SmokStat_InstChosen", "SleepDur_InstChosen", "AlcIntake_InstChosen",
                          "OilyFish_InstChosen", "Salt_InstChosen", "ProcMeat_InstChosen", "EmploymentStatus_InstChosen",
                          "MotherHeartDisease", "FatherHeartDisease", "CholMeds", "BPMeds", "Veggie", "Fruit",
                          "BMI_InstChosen", "MobilProbs", "Townsend", "Biological.Sex", "TimeYear", "Status",
                          "StatusThree", "AgeBaseline", "PGS", "StandPGS", "p22009_a1", "p22009_a2", "p22009_a3",
                          "p22009_a4", "p22009_a5", "p22009_a6", "p22009_a7", "p22009_a8", "p22009_a9", "p22009_a10", "REGION", "EA.Inst.0", "EA.Inst.1", "EA.Inst.2", "EA.Inst.3",
                          "Ethnicity_Inst0", "Ethnicity_Inst1", "Ethnicity_Inst2", "Black", "Asian",
                          "IPAQGroupInst1", "METVigorous", "METMVPA", "METTotal", "ManLabor_Inst0", "ManLabor_Inst1", "ManLabor_Inst2", "ManLabor_Inst3","WalkorStandWork_Inst0", "WalkorStandwork_Inst1", 
                          "WalkorStandWork_Inst2", "WalkorStandWork_Inst3","eid")]


# THEN look at extent of missingness
summary(is.na(datsubrest))
# Out of 77,474 inds total
# NO missingness for outcome variable or exposures (GOOD! Shouldn't be)

# RECODING PAEE TO ZERO IF NEGATIVE
datsubrest$PAEEPOS <- ifelse(datsubrest$OverallPAEETRANSFORM >= 0, datsubrest$OverallPAEETRANSFORM, 0)

# -------
# REMOVING outliers in confounder set - Like F or V >= 100...
# KEEP TRACK OF WHERE LINE IS DRAWN FOR EACH
# -------


summary(as.factor(datsubrest$SeasonWear))
summary(datsubrest$PercentMVPA)
summary(datsubrest$PercentVigorous)
summary(datsubrest$OverallPAEETRANSFORM)
summary(datsubrest$PAEEPOS)
summary(as.factor(datsubrest$SmokStat_InstChosen))
summary(datsubrest$SleepDur_InstChosen)
summary(datsubrest$AlcIntake_InstChosen)
summary(datsubrest$OilyFish_InstChosen)
summary(as.factor(datsubrest$Salt_InstChosen))
summary(datsubrest$ProcMeat_InstChosen)
summary(as.factor(datsubrest$EmploymentStatus_InstChosen))
summary(datsubrest$MotherHeartDisease)
summary(datsubrest$FatherHeartDisease)
summary(datsubrest$CholMeds)
summary(datsubrest$BPMeds)
summary(datsubrest$Veggie)
summary(datsubrest$Fruit)
# BOTH OF THESE HAVE 100+ (IMPLAUSIBLE)

summary(datsubrest$BMI_InstChosen)
# Goes from 12 to 67 (67 isn't impossible but pretty high...)

summary(as.factor(datsubrest$MobilProbs))
summary(datsubrest$Townsend)
summary(as.factor(datsubrest$Biological.Sex))
summary(datsubrest$TimeYear)
summary(datsubrest$AgeBaseline)
summary(datsubrest$StandPGS)
summary(as.factor(datsubrest$REGION))

# RECODING 100+ values to NA to be imputed
summary(datsubrest$Veggie)
summary(datsubrest$Fruit)
# Max of 100 and 102, respectively

datsubrest$Veggie <- ifelse(datsubrest$Veggie >= 100, NA, datsubrest$Veggie)
datsubrest$Fruit <- ifelse(datsubrest$Fruit >= 100, NA, datsubrest$Fruit)

summary(datsubrest$Veggie)
summary(datsubrest$Fruit)
# NOW maxes are 66 and 58 - still TOO HIGH

# RECODING 50+ values to NA to be imputed
datsubrest$Veggie <- ifelse(datsubrest$Veggie >= 50, NA, datsubrest$Veggie)
datsubrest$Fruit <- ifelse(datsubrest$Fruit >= 50, NA, datsubrest$Fruit)

summary(datsubrest$Veggie)
summary(datsubrest$Fruit)
# NOW maxes are 49 - still seems v high

# Ok what if we restricted to 30 or under? (aribtrary...)
# Marked increase in missingness, so keep at 50 for now...




# ---------
# REPEATING for Model 1
# From Model 0 - Season Wear & Sex
# Added - Educ level, employment status, Townsend, dietary, alcohol intake, 
# smoking status, sleep duration, parental history
# parental history of heart disease, BP meds and chol meds
# Mobility problems

# NOT INCLUDED BUT IN DEMPSEY - Prevalent cancer, parental history of cancer,
# Insulin meds/diagnosed diabetes, Ethnicity
# ---------

# BEFORE adding covariates in, comparing my results to Dempsey
# Dempsey Supp Table S2

# Education Level - COME BACK TO...
summary(as.factor(datsubrest$EA.Inst.0))
# They set as None/Degree Level/Any Other

# I will treat this as none/uni/other
datsubrest$NewEduc <- ifelse(grepl("None of the above", datsubrest$EA.Inst.0), "None",
                             ifelse(grepl("College or University degree", datsubrest$EA.Inst.0), "Uni", "Other"))

summary(as.factor(datsubrest$NewEduc))
# Matches up well



# Smoking Status - just current/never/previous
summary(as.factor(datsubrest$SmokStat_InstChosen))

# Alcohol Intake
# They changed to never/less than 2x per week/Usually
summary(datsubrest$AlcIntake_InstChosen)
# Ours is continuous over year - simple change...

datsubrest$NewAlc <- ifelse(datsubrest$AlcIntake_InstChosen < 12, "Never/Rarely",
                            ifelse(datsubrest$AlcIntake_InstChosen <= 78 & datsubrest$AlcIntake_InstChosen > 12, "Less than twice a week",
                                   "Usually/Always"))
# Percentages align well w Dempsey

# Salt intake
summary(as.factor(datsubrest$Salt_InstChosen))
# Simple - just keeping as FACTOR and as never/sometimes/usually/always

# Oily fish intake
summary(as.factor(datsubrest$OilyFish_InstChosen))
# We have coded as weekly intake.. They code as more than once a week or not

# Simple conversion
datsubrest$NewOilFish <- ifelse(datsubrest$OilyFish_InstChosen > 1, 1, 0)

# "Fruit & Vegetable Intake Score"
# I really have NO idea where they get this
# They say "a score from 0-4 taking into account fruit & veg categories"
# OKAY so I will take quintiles of sample and make it this way
datsubrest$FruitnVeg <- datsubrest$Fruit + datsubrest$Veggie

quantile(datsubrest$FruitnVeg, probs = c(0.20, 0.4, 0.5, 0.8), na.rm = TRUE)
# 20% 5
# 40% 6.5
# 50% 7
# 80% 11


datsubrest$FnVScore <- ifelse(datsubrest$FruitnVeg < 5, 0,
                              ifelse(datsubrest$FruitnVeg >= 5 & datsubrest$FruitnVeg < 6.5, 1,
                                     ifelse(datsubrest$FruitnVeg >= 6.5 & datsubrest$FruitnVeg < 7, 2,
                                            ifelse(datsubrest$FruitnVeg >= 7 & datsubrest$FruitnVeg < 11, 3, 4))))


# Weekly frequency of red meat consumption
summary(datsubrest$ProcMeat_InstChosen)
# Already on same scale - and similar median

# Sleep duration - average sleep per night
# No change
summary(datsubrest$SleepDur_InstChosen)


# Parental history - recode THIS as OVERALL - from EITHER mother or father
# THEIRS was parental history of CVD OR cancer... Unclear why...
datsubrest$ParentHist <- ifelse(datsubrest$MotherHeartDisease == 1 | datsubrest$FatherHeartDisease == 1, 1, 0)

summary(as.factor(datsubrest$ParentHist))

# Cholesterol meds & BP meds - PUT TOGETHER
datsubrest$Meds <- ifelse(datsubrest$CholMeds == 1 | datsubrest$BPMeds == 1, 1, 0)

# Mobility limitation - coded as Y/N
# Simple to code
summary(as.factor(datsubrest$MobilProbs))

datsubrest$MobilityDichot <- ifelse(grepl("I am unable to walk about", datsubrest$MobilProbs) | grepl("I have moderate problems in walking about", datsubrest$MobilProbs) | grepl("I have severe problems in walking about", datsubrest$MobilProbs) | grepl("I have slight problems in walking about", datsubrest$MobilProbs), 1, 0)

summary(as.factor(datsubrest$MobilityDichot))
# 19% - LOWER than theirs... LIKELY BECAUSE MISSING WERE CODED AS NO ISSUE...

# Employment Status
# THEY just code as 'in employment'
# So I just code as 1 if 'in paid employment or self-employed'
# Percent is about right
summary(as.factor(datsubrest$EmploymentStatus_InstChosen))

datsubrest$NewEmploy <- ifelse(grepl("In paid employment or self-employed", datsubrest$EmploymentStatus_InstChosen), 1, 0)
summary(as.factor(datsubrest$NewEmploy))

write.csv(datsubrest, "FINALDempseyDataset.csv")

dx upload FINALDempseyDataset.csv