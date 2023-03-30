# ---------
# Running Add'l Analyses
# ---------


# Looking at WHAT is missing
summary(is.na(data$PAEEPOS))
summary(is.na(data$StandPGS))
summary(is.na(data$p22009_a1))
summary(is.na(data$SeasonWear))
summary(is.na(data$Salt_InstChosen))
summary(is.na(data$NewAlc))
summary(is.na(data$NewOilFish))
summary(is.na(data$FnVScore))
summary(is.na(data$ProcMeat_InstChosen))
summary(is.na(data$ParentHist))
summary(is.na(data$MobilityDichot))
summary(is.na(data$NewEmploy))
summary(is.na(data$Townsend))
summary(is.na(data$NewEduc))
summary(is.na(data$SmokStat_InstChosen))
summary(is.na(data$Biological.Sex))

#Mode   FALSE 
#logical   77474 
#Mode   FALSE 
#logical   77474 
#Mode   FALSE 
#logical   77474 
#Mode   FALSE 
#logical   77474 
#Mode   FALSE    TRUE 
#logical   77431      43 
#Mode   FALSE    TRUE 
#logical   77406      68 
#Mode   FALSE    TRUE 
#logical   77270     204 
#Mode   FALSE    TRUE 
#logical   76410    1064 
#Mode   FALSE    TRUE 
#logical   77367     107 
#Mode   FALSE 
#logical   77474 
#Mode   FALSE 
#logical   77474 
#Mode   FALSE 
#logical   77474 
#Mode   FALSE    TRUE 
#logical   77385      89 
#Mode   FALSE 
#logical   77474 
#Mode   FALSE    TRUE 
#logical   77275     199 
#Mode   FALSE 
#logical   77474


# ------
# REDEFINING COVARIATES
# ------

# F&V Intake - I like split this way... LESS influence of outliers
quantile(whitedata$FruitnVeg, probs = c(0.20, 0.4, 0.6, 0.8), na.rm = TRUE)
# 20% 5
# 40% 6.5
# 60% 8
# 80% 10.5

whitedata$FnVScore <- ifelse(whitedata$FruitnVeg < 5, 0,
                             ifelse(whitedata$FruitnVeg >= 5 & whitedata$FruitnVeg < 6.5, 1,
                                    ifelse(whitedata$FruitnVeg >= 6.5 & whitedata$FruitnVeg < 8, 2,
                                           ifelse(whitedata$FruitnVeg >= 7 & whitedata$FruitnVeg < 10.5, 3, 4))))


# Weekly alcohol intake - transforming from yearly
whitedata$AlcIntake_Weekly <- whitedata$AlcIntake_InstChosen/52

# Oily fish consumption and Red Meat are ALREADY weekly so revert to:
# OilyFish_InstChosen
# ProcMeat_InstChosen

# The rest makes sense as is...


# Subset to only relevant variables
whitedatasub <- whitedata[ , c("AgeBaseline", "TimeYear", "Status", "PAEEPOS", "StandPGS", "SeasonWear", "Salt_InstChosen", "AlcIntake_Weekly", "OilyFish_InstChosen", "FnVScore", "ProcMeat_InstChosen", "ParentHist", "MobilityDichot", "NewEmploy", "Townsend", "NewEduc", "SmokStat_InstChosen", "Biological.Sex", "REGION")]


# Restrict to only relevant variables then restrict to complete cases
whitedatasub <- whitedatasub[complete.cases(whitedatasub), ]


# Computing median wear time
summary(FINAL$p90052)
FINALSUB <- FINAL[ , c("eid", "p90052")]

FINALSUBWHITE <- merge(whitedata, FINALSUB, by = "eid", all = F)
summary(FINALSUBWHITE$p90052)
# 7 - 0.07

# Adding table of quintiles
PAIntensity <- quantile(whitedatasub$PercentMVPA, probs = c(0.20, 0.40, 0.60, 0.80))
PAIntensity

PAVolume <- quantile(whitedatasub$PAEEPOS, probs = c(0.20, 0.40, 0.60, 0.80))
PAVolume

# (and for PGS)

# Standardized PAEE
whitedata$StandPAEEPOS <- scale(whitedata$PAEEPOS)

# Model 0
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + PercentMVPA100 + strata(Biological.Sex), data = whitedata)

summary(fit.lin)

# Model 2
# Sleep dur, BMI, Meds
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + PercentMVPA100 + BMI_InstChosen + SleepDur_InstChosen + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + OilyFish_InstChosen + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = whitedata)

summary(fit.lin)

# Model 3
# INCLUDING occupation variables
# NEED TO IMPUTE due to low levels of completeness in occ vars...





# --------
# REPEATING ANALYSIS FOR WHITE SPECIFICALLY
# Given most of sample is white unsurprisingly v similar
# ---------


# MODEL:
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = whitedata)

summary(fit.lin)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
48.6690567680804 - 41.5283167027557
k1 <- matrix(c(-7.11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.11140408168133
# Lower = 1.07120635703763
# Upper = 1.153110252439



# Change PA from 20th to 60th percentile w/ genetic risk at 20th
48.50 - 35.91
k1 <- matrix(c(-12.59, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.20566766052
# Lower = 1.12952989151296
# Upper = 1.28693761762842



# Change PA from 20th to 80th percentile w/ genetic risk at 20th
48.50 - 30.07
k1 <- matrix(c(-18.43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.31493979495329
# Lower = 1.19518430021643
# Upper = 1.44669459265712


# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.83 - -0.26
k1 <- matrix(c(0, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.25517486527203
# Lower = 1.21628777457933
# Upper = 1.29530525204494

# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-7.11, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.39500646848715
# Lower = 1.32968073623136
# Upper = 1.46354158114416


# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.59, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.51332374335604
# Lower = 1.40841191770701
# Upper = 1.62605039293735



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.43, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.65047937997132
# Lower = 1.49351925479814
# Upper = 1.82393509488346



# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.24 - -0.83
k1 <- matrix(c(0, 1.07, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53209923366796
# Lower = 1.44420759134503
# Upper = 1.62533978901178



# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(-7.11, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.70958435972041
# Lower = 1.59488510847928
# Upper = 1.83253242974186


# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.59, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.85458251361425
# Lower = 1.69924424572825
# Upper = 2.02412119885078



# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.43, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.02266713293453
# Lower = 1.80918279896173
# Upper = 2.26134270843249




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.81 - -0.83
k1 <- matrix(c(0, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.92305244920256
# Lower = 1.75657203730761
# Upper = 2.10531116506458


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-7.11, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.137288341331
# Lower = 1.93959861831807
# Upper = 2.35512719531146


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-12.59, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.31856214748731
# Lower = 2.07598195079157
# Upper = 2.58948803948474


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-18.43, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.52869819323882
# Lower = 2.21988893188627
# Upper = 2.88046598216784








# -----------
# REPEATING MVPA ANALYSES WITH WHITE ONLY
# ANALYSES including PA *AND* % MVPA
# Interaction was insig, so fitting separately in model
# Comparing quintiles for MVPA and genetic risk
# -----------




# MODEL:
fit.linMVPA <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = whitedata)

summary(fit.linMVPA)


# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
0.4546 - 0.3853
k1 <- matrix(c(0, -0.069, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.14585628243349
# Lower = 1.09258602511612
# Upper = 1.20172379090494


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
0.4546 - 0.3265
k1 <- matrix(c(0, -0.128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.28733244027232
# Lower = 1.17852283351586
# Upper = 1.40618812351181



# Change PA from 20th to 80th percentile w/ genetic risk at 20th
0.4546 - 0.2597
k1 <- matrix(c(0, -0.195, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.4692880494798
# Lower = 1.28433697673418
# Upper = 1.68087301965997


# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.25516823709337
# Lower = 1.21627594227799
# Upper = 1.2953041728815



# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.069, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.4382424099844
# Lower = 1.3584664005363
# Upper = 1.5227032697026



# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.61581878960971
# Lower = 1.47122381698253
# Upper = 1.77462486041768



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.84420369084791
# Lower = 1.60622107709453
# Upper = 2.11744653450148



# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53844998523622
# Lower = 1.44942866503642
# Upper = 1.63293883594737


# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.766409006087
# Lower = 1.63661085210499
# Upper = 1.90650133644911

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.98793583854993
# Lower = 1.78692498428352
# Upper = 2.21155836587945


# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.27337401962285
# Lower = 1.96216146720331
# Upper = 2.63394706270655




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.92302323137365
# Lower = 1.7565228714214
# Upper = 2.1053061184511


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.20350825093505
# Lower = 1.98922602602816
# Upper = 2.44087325844696


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.128, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.47557018914461
# Lower = 2.18144829007883
# Upper = 2.80934816986196


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.195, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.82547505272933
# Lower = 2.40250541903757
# Upper = 3.32290999651266


# -------
# 1 Year Exclusion Results
# -------

whitedataoneyear <- subset(whitedata, TimeYear > 1 | Status == 0)







# --------
# REPEATING ANALYSIS FOR WHITE SPECIFICALLY
# Given most of sample is white unsurprisingly v similar
# ---------


# MODEL:
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = whitedataoneyear)

summary(fit.lin)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
48.6690567680804 - 41.5283167027557
k1 <- matrix(c(-7.11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.08963691950934
# Lower = 1.0475551912767
# Upper = 1.13340912845917




# Change PA from 20th to 60th percentile w/ genetic risk at 20th
48.50 - 35.91
k1 <- matrix(c(-12.59, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.16417051102341
# Lower = 1.08574584776245
# Upper = 1.24825987732723



# Change PA from 20th to 80th percentile w/ genetic risk at 20th
48.50 - 30.07
k1 <- matrix(c(-18.43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.24922039282623
# Lower = 1.12797911035579
# Upper = 1.38349334267431


# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.83 - -0.26
k1 <- matrix(c(0, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.25588712445239
# Lower = 1.21419022496012
# Upper = 1.29901595066546

# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-7.11, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.36846097753974
# Lower = 1.2999789363054
# Upper = 1.44055060797467


# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.59, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.46206675546145
# Lower = 1.35391980247266
# Upper = 1.57885215470046



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.43, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.56887980695381
# Lower = 1.40987869879383
# Upper = 1.74581249491406



# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.24 - -0.83
k1 <- matrix(c(0, 1.07, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53373167548963
# Lower = 1.43953577451053
# Upper = 1.63409127723836



# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(-7.11, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.67790424309592
# Lower = 1.55756416511825
# Upper = 1.80754200183179


# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.59, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.79267662939773
# Lower = 1.6324154061
# Upper = 1.96867138448945



# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.43, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.92364278426703
# Lower = 1.70724138930277
# Upper = 2.1674741396551



# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.81 - -0.83
k1 <- matrix(c(0, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.92619386361221
# Lower = 1.74787026589108
# Upper = 2.12271063397593


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-7.11, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.0988519479242
# Lower = 1.89142117002397
# Upper = 2.32903150769396


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-12.59, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.24241809453158
# Lower = 1.99203379355917
# Upper = 2.52427389883699


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-18.43, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.40624065496112
# Lower = 2.09304061646578
# Upper = 2.76630756423852








# -----------
# REPEATING MVPA ANALYSES WITH WHITE ONLY
# ANALYSES including PA *AND* % MVPA
# Interaction was insig, so fitting separately in model
# Comparing quintiles for MVPA and genetic risk
# -----------




# MODEL:
fit.linMVPA <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = whitedataoneyear)

summary(fit.linMVPA)


# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
0.4546 - 0.3853
k1 <- matrix(c(0, -0.069, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.14342164470761
# Lower = 1.08646410749268
# Upper = 1.20336516279684


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
0.4546 - 0.3265
k1 <- matrix(c(0, -0.128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.28226298779125
# Lower = 1.16630233798833
# Upper = 1.40975312858868



# Change PA from 20th to 80th percentile w/ genetic risk at 20th
0.4546 - 0.2597
k1 <- matrix(c(0, -0.195, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.46048255356074
# Lower = 1.26410341786864
# Upper = 1.68736929202493


# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.25585180325758
# Lower = 1.21415212797183
# Upper = 1.29898364085551


# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.069, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.4359681343898
# Lower = 1.35067611370691
# Upper = 1.52664614562835



# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.61033228546809
# Lower = 1.45620171697685
# Upper = 1.7807766873153



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.83414964851549
# Lower = 1.58137752655478
# Upper = 2.12732562380509


# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53979256864632
# Lower = 1.44435968376302
# Upper = 1.6415309712061


# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.76063215135014
# Lower = 1.62224304842361
# Upper = 1.91082684889914

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.9744190196512
# Lower = 1.76109737865614
# Upper = 2.21358030078675

# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.24884018261044
# Lower = 1.92033116925768
# Upper = 2.63354688393579




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.92603800087426
# Lower = 1.74771247958501
# Upper = 2.12255872985044


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.20227353872901
# Lower = 1.97335729020841
# Upper = 2.4577448612328


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.128, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.46968724160052
# Lower = 2.15628299837806
# Upper = 2.82864312147908


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.195, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.81294489777187
# Lower = 2.36368625968467
# Upper = 3.34759275495233


# --------
# IMPUTED Results
# --------

install.packages('mice')
library(mice)

whitedatasub <- whitedata[ , c("AgeBaseline", "TimeYear", "Status", "PAEEPOS", "StandPGS", "SeasonWear", "Salt_InstChosen", "AlcIntake_Weekly", "OilyFish_InstChosen", "FnVScore", "ProcMeat_InstChosen", "ParentHist", "MobilityDichot", "NewEmploy", "Townsend", "NewEduc", "SmokStat_InstChosen", "Biological.Sex", "REGION", "p22009_a1",
                               "p22009_a2", "p22009_a3", "p22009_a4", "p22009_a5", "p22009_a6", "p22009_a7", "p22009_a8", "p22009_a9", "p22009_a10")]


imputed_Data <- mice(whitedatasub, m=5, method = 'pmm', seed = 500)
whitedataimputed <- complete(imputed_Data)



# --------
# REPEATING ANALYSIS FOR WHITE SPECIFICALLY
# Given most of sample is white unsurprisingly v similar
# ---------


# MODEL:
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = whitedataimputed)

summary(fit.lin)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
48.6690567680804 - 41.5283167027557
k1 <- matrix(c(-7.11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.11336962460528
# Lower = 1.073467645756
# Upper = 1.15475480411029




# Change PA from 20th to 60th percentile w/ genetic risk at 20th
48.50 - 35.91
k1 <- matrix(c(-12.59, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.20944590568166
# Lower = 1.13375550563869
# Upper = 1.29018945574698



# Change PA from 20th to 80th percentile w/ genetic risk at 20th
48.50 - 30.07
k1 <- matrix(c(-18.43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.32097626923202
# Lower = 1.20173523079148
# Upper = 1.4520488866128


# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.83 - -0.26
k1 <- matrix(c(0, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.25569301919307
# Lower = 1.21716379413172
# Upper = 1.29544188387153

# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-7.11, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.39805046539846
# Lower = 1.33321012258819
# Upper = 1.46604430215881


# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.59, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.5186927808561
# Lower = 1.41438760921256
# Upper = 1.63069002273607



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.43, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.65874067979436
# Lower = 1.50242454508884
# Upper = 1.83132034936367



# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.24 - -0.83
k1 <- matrix(c(0, 1.07, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53328672103123
# Lower = 1.44616082036998
# Upper = 1.62566163857849



# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(-7.11, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.71394760273871
# Lower = 1.60005969262853
# Upper = 1.83594174546571


# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.59, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.86184970819563
# Lower = 1.70737209288164
# Upper = 2.03030396851429



# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.43, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.03354219469353
# Lower = 1.82088349112395
# Upper = 2.27103704204956




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.81 - -0.83
k1 <- matrix(c(0, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.92533743202034
# Lower = 1.76021459104617
# Upper = 2.1059501756178


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-7.11, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.14361221392699
# Lower = 1.94721450004041
# Upper = 2.35981876860593


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-12.59, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.32859147421264
# Lower = 2.08725031806943
# Upper = 2.59783802969598


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-18.43, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.54332505796299
# Lower = 2.23559646495542
# Upper = 2.89341231830559









# -----------
# REPEATING MVPA ANALYSES WITH WHITE ONLY
# ANALYSES including PA *AND* % MVPA
# Interaction was insig, so fitting separately in model
# Comparing quintiles for MVPA and genetic risk
# -----------




# MODEL:
fit.linMVPA <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = whitedataimputed)

summary(fit.linMVPA)


# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
0.4546 - 0.3853
k1 <- matrix(c(0, -0.069, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.14578935599285
# Lower = 1.09302922437867
# Upper = 1.2010962003809


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
0.4546 - 0.3265
k1 <- matrix(c(0, -0.128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.28719296143273
# Lower = 1.17940982117672
# Upper = 1.40482611744651



# Change PA from 20th to 80th percentile w/ genetic risk at 20th
0.4546 - 0.2597
k1 <- matrix(c(0, -0.195, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.46904553544085
# Lower = 1.28580986137093
# Upper = 1.67839340017018


# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.2554727792154
# Lower = 1.21695378144695
# Upper = 1.29521097956303

# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.069, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.43850734716376
# Lower = 1.35954123607494
# Upper = 1.52206003976629



# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.61603572467645
# Lower = 1.47284710979171
# Upper = 1.7731449829846



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 0.57, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.8443466811739
# Lower = 1.60859979186839
# Upper = 2.11464324287036


# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53891216700791
# Lower = 1.45068108890854
# Upper = 1.63250949907039


# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.76326918076555
# Lower = 1.63507379915953
# Upper = 1.90151551901558

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.98087690963577
# Lower = 1.78268280983928
# Upper = 2.20110572081071

# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.26073204837857
# Lower = 1.95437853362024
# Upper = 2.61510721011587




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.92436598968963
# Lower = 1.75934089418507
# Upper = 2.10487033781448


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.20491806802103
# Lower = 1.99267180866703
# Upper = 2.43977139915364


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.128, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.47703035714903
# Lower = 2.18570576671592
# Upper = 2.8071845184619


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.195, 1.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.82698126570777
# Lower = 2.40793701683343
# Upper = 3.31895021372793






# -------
# Creating Quintile FIGURE
# -------

# ---------
# Physical Activity Volume & Genetic Risk Plots
# FIGURES to compare HR for different genetic and PA groups
# COMPARING DECILES from 20th to 80th
# ---------


# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.11, 1.21, 1.31)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.07, 1.13, 1.20)
RefUL <- c(1.00, 1.15, 1.29, 1.45)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th PA Risk", "40th PA Risk", "60th PA Risk", "80th PA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
FortyCoef <- c(1.26, 1.40, 1.51, 1.65)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
FortyLL <- c(1.22, 1.33, 1.41, 1.49)
FortyUL <- c(1.30, 1.46, 1.63, 1.82)

FortyvPA <- as.data.frame(cbind(FortyLL, FortyCoef, FortyUL))

colnames(FortyvPA) <- c("LL","Mean","UL")

FortyvPA$Model <- c("20th PA Risk", "40th PA Risk", "60th PA Risk", "80th PA Risk")
FortyvPA$Genetic.Risk <- "40th Percentile Genetic Risk"

# Merging FortyvPA and RefvPA20
PlotDF <- rbind(RefvPA20, FortyvPA)

# Creating PA Risk Colors ((For our purposes makes more sense to flip this)
# PlotDF$Physical.Activity <- as.factor(PlotDF$Model)



# ADDING HAZARD RATIOS VIA ANNOTATION
# From: https://www.khstats.com/blog/forest-plots/

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"

install.packages('ggforce')
library(ggforce)

# ---
# 20th vs 40th Genetic Risk
# ---


# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2, y = Model, hjust = 0, label = LABEL)) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 2.35, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3), ylim = c(0,5), expand = TRUE)

# scale_colour_manual(values=cbbPalette)





# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.11, 1.21, 1.31)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.07, 1.13, 1.20)
RefUL <- c(1.00, 1.15, 1.29, 1.45)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th PA Risk", "40th PA Risk", "60th PA Risk", "80th PA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
SixtyCoef <- c(1.53, 1.71, 1.85, 2.02)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
SixtyLL <- c(1.44, 1.59, 1.70, 1.81)
SixtyUL <- c(1.63, 1.83, 2.02, 2.26)

SixtyvPA <- as.data.frame(cbind(SixtyLL, SixtyCoef, SixtyUL))

colnames(SixtyvPA) <- c("LL","Mean","UL")

SixtyvPA$Model <- c("20th PA Risk", "40th PA Risk", "60th PA Risk", "80th PA Risk")
SixtyvPA$Genetic.Risk <- "60th Percentile Genetic Risk"

# Merging FortyvPA and RefvPA20
PlotDF <- rbind(RefvPA20, SixtyvPA)

# ADDING HAZARD RATIOS VIA ANNOTATION
# From: https://www.khstats.com/blog/forest-plots/

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"

# ---
# 20th vs 60th Genetic Risk
# ---


# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.5, y = Model, hjust = 0, label = LABEL)) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,4.7), ylim = c(0,5), expand = TRUE)







# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.11, 1.21, 1.31)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.07, 1.13, 1.20)
RefUL <- c(1.00, 1.15, 1.29, 1.45)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th PA Risk", "40th PA Risk", "60th PA Risk", "80th PA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
EightyCoef <- c(1.92, 2.14, 2.32, 2.53)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
EightyLL <- c(1.77, 1.94, 2.08, 2.22)
EightyUL <- c(2.10, 2.36, 2.59, 2.88)

EightyvPA <- as.data.frame(cbind(EightyLL, EightyCoef, EightyUL))

colnames(EightyvPA) <- c("LL","Mean","UL")

EightyvPA$Model <- c("20th PA Risk", "40th PA Risk", "60th PA Risk", "80th PA Risk")
EightyvPA$Genetic.Risk <- "80th Percentile Genetic Risk"

# Merging FortyvPA and RefvPA20
PlotDF <- rbind(RefvPA20, EightyvPA)

# ADDING HAZARD RATIOS VIA ANNOTATION
# From: https://www.khstats.com/blog/forest-plots/

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"

# ---
# 20th vs 80th Genetic Risk
# ---


# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 3.1, y = Model, hjust = 0, label = LABEL)) + geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3.6, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,4.5), ylim = c(0,5), expand = TRUE)

# xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 3.1, y = Model, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3.6, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,4.5), ylim = c(0,5), expand = TRUE)



# -------
# NOW use model with PA *AND* % MVPA TOGETHER
# Look at effect of PA intensity and PGS on risk
# FIGURES to compare HR for different genetic and PA groups
# COMPARING DECILES from 20th to 80th
# ---------


# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.15, 1.29, 1.47)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.09, 1.18, 1.28)
RefUL <- c(1.00, 1.20, 1.41, 1.68)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th % MVPA Risk", "40th % MVPA Risk", "60th % MVPA Risk", "80th % MVPA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
FortyCoef <- c(1.26, 1.44, 1.62, 1.84)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
FortyLL <- c(1.22, 1.36, 1.47, 1.61)
FortyUL <- c(1.30, 1.52, 1.77, 2.12)

FortyvPA <- as.data.frame(cbind(FortyLL, FortyCoef, FortyUL))

colnames(FortyvPA) <- c("LL","Mean","UL")

FortyvPA$Model <- c("20th % MVPA Risk", "40th % MVPA Risk", "60th % MVPA Risk", "80th % MVPA Risk")
FortyvPA$Genetic.Risk <- "40th Percentile Genetic Risk"

# Merging FortyvPA and RefvPA20
PlotDF <- rbind(RefvPA20, FortyvPA)

# Creating PA Risk Colors ((For our purposes makes more sense to flip this)
# PlotDF$Physical.Activity <- as.factor(PlotDF$Model)



# ADDING HAZARD RATIOS VIA ANNOTATION
# From: https://www.khstats.com/blog/forest-plots/

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"


# ---
# 20th vs 40th Genetic Risk
# ---


# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and % MVPA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.3, y = Model, hjust = 0, label = LABEL)) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 2.7, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3.3), ylim = c(0,5), expand = TRUE)







# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.15, 1.29, 1.47)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.09, 1.18, 1.28)
RefUL <- c(1.00, 1.20, 1.41, 1.68)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th % MVPA Risk", "40th % MVPA Risk", "60th % MVPA Risk", "80th % MVPA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
SixtyCoef <- c(1.54, 1.77, 1.99, 2.27)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
SixtyLL <- c(1.45, 1.64, 1.79, 1.96)
SixtyUL <- c(1.63, 1.91, 2.21, 2.63)

SixtyvPA <- as.data.frame(cbind(SixtyLL, SixtyCoef, SixtyUL))

colnames(SixtyvPA) <- c("LL","Mean","UL")

SixtyvPA$Model <- c("20th % MVPA Risk", "40th % MVPA Risk", "60th % MVPA Risk", "80th % MVPA Risk")
SixtyvPA$Genetic.Risk <- "60th Percentile Genetic Risk"

# Merging FortyvPA and RefvPA20
PlotDF <- rbind(RefvPA20, SixtyvPA)

# ADDING HAZARD RATIOS VIA ANNOTATION
# From: https://www.khstats.com/blog/forest-plots/

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"

# ---
# 20th vs 60th Genetic Risk
# ---


# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.8, y = Model, hjust = 0, label = LABEL)) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3.3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,4), ylim = c(0,5), expand = TRUE)







# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.15, 1.29, 1.47)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.09, 1.18, 1.28)
RefUL <- c(1.00, 1.20, 1.41, 1.68)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th % MVPA Risk", "40th % MVPA Risk", "60th % MVPA Risk", "80th % MVPA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
EightyCoef <- c(1.92, 2.20, 2.48, 2.83)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
EightyLL <- c(1.76, 1.99, 2.18, 2.40)
EightyUL <- c(2.11, 2.44, 2.81, 3.32)

EightyvPA <- as.data.frame(cbind(EightyLL, EightyCoef, EightyUL))

colnames(EightyvPA) <- c("LL","Mean","UL")

EightyvPA$Model <- c("20th % MVPA Risk", "40th % MVPA Risk", "60th % MVPA Risk", "80th % MVPA Risk")
EightyvPA$Genetic.Risk <- "80th Percentile Genetic Risk"

# Merging FortyvPA and RefvPA20
PlotDF <- rbind(RefvPA20, EightyvPA)

# ADDING HAZARD RATIOS VIA ANNOTATION
# From: https://www.khstats.com/blog/forest-plots/

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"

# ---
# 20th vs 80th Genetic Risk
# ---


# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 3.7, y = Model, hjust = 0, label = LABEL)) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 4.2, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,5), ylim = c(0,5), expand = TRUE)








# ------
# Black & Asian Plots
# ------

# -----
# Objective vs Subjective PA Correlations
# -----




