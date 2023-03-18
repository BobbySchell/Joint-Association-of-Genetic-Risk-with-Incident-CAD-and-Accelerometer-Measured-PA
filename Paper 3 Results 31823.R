# ------
# Paper 3 Results 
# 3/18/23
# ------

dx download FINALDempseyDataset.csv
dx download ImputedFINALDempseyDataset.csv
dx download FINALANALYSISDATAPAPER3.csv

data <- read.csv("FINALDempseyDataset.csv")
imputeddata <- read.csv("ImputedFINALDempseyDataset.csv")

# ONLY being used to read in Genetic.Ethnic.Grouping
FINAL <- read.csv("FINALANALYSISDATAPAPER3.csv")


# Reading in needed packages
install.packages("survival")
library(survival)

install.packages("ggplot2")
library(ggplot2)

install.packages("multcomp")
library(multcomp)


# ----
# Fitting linear model from before
# POOLED
# ----

fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = data)

summary(fit.lin)

# Fit same model w/ imputed dataset
fit.linIMPUTED <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = imputeddata)

summary(fit.linIMPUTED)

# -----
# Fitting linear model with PAEE *AND* MVPA
# POOLED
# -----


fit.linINTER <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = data)


summary(fit.linINTER)

# Fit same model w/ imputed dataset
fit.linINTERIMPUTED <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = imputeddata)


summary(fit.linINTERIMPUTED)


# -------
# Getting imputation WITH ETHNICITY
# -------

#DataSub <- Data[ , c("AgeBaseline", "TimeYear", "Status", "PAEEPOS", "StandPGS", "p22009_a1", "p22009_a2"
           #          ,"p22009_a3", "p22009_a4", "p22009_a5", "p22009_a6", "p22009_a7", "p22009_a8", "p22009_a9",
            #         "p22009_a10", "SeasonWear", "Salt_InstChosen", "NewAlc",
             #        "NewOilFish", "FnVScore", "ProcMeat_InstChosen", "ParentHist",
              #       "MobilityDichot", "NewEmploy", "Townsend", "NewEduc",
               #      "SmokStat_InstChosen", "Biological.Sex", "Genetic.")]
# Got rid of eid... Easier...


#imputed_Data <- mice(DataSub, m=5, method = 'fastpmm', seed = 500, predictorMatrix = as.matrix(DataSub[ , -29]))
#imputed_Data <- mice(DataSub, m=5, method = 'pmm', seed = 500)
# --------




# --------
# FITTING MODELS BY ETHNICITY
# -------

# -------
# WHITE results
# Use Genetic.Ethnic.Grouping
# -------

# Restricting other dataset to ONLY ethnic group
FINALGEN <- FINAL[ , c("eid", "Genetic.Ethnic.Grouping")]

# Merging with dataset
data <- merge(data, FINALGEN, by = "eid", all = F)
# 77474 x 74 

# RESTRICT to white dataset based on genetics
whitedata <- subset(data, Genetic.Ethnic.Grouping == "Caucasian")
dim(whitedata)
# 66,180 x 74

# ----------
# EXPLORATORY:
# BLACK AND ASIAN RESULTS
# ----------

blackdata <- subset(data, Black == 1)
dim(blackdata)

asiandata <- subset(data, Asian == 1)
dim(blackdata)

# --------
# Getting quintile levels for PA volume and PGS and % MVPA
# POOLED ANALYSES FIRST
# --------

PAVolume <- quantile(data$PAEEPOS, probs = c(0.20, 0.40, 0.60, 0.80))
PAVolume
# 20% 30.1581806907346
# 40% 36.0267497376078
# 60% 41.5283167027557
# 80% 48.6690567680804


#PercentMVPA <- quantile(data$PercentMVPA, probs = c(0.20, 0.40, 0.60, 0.80))
#PercentMVPA

StandPGS <- quantile(data$StandPGS, probs = c(0.20, 0.40, 0.60, 0.80))
StandPGS
# 20% -0.846868164911578
# 40% -0.266874315609926
# 60% 0.236609243902655
# 80% 0.821322441664612


# ------
# COMPARING 20th/20th to all the rest here
# ONLY CONSIDERING: PA VOLUME AND PGS FOR NOW
# ------

# MODEL:
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = data)

summary(fit.lin)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
48.6690567680804 - 41.5283167027557
k1 <- matrix(c(-7.14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.12337536052322
# Lower = 1.08550448075343
# Upper = 1.16256747254949


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
48.6690567680804 - 36.0267497376078
k1 <- matrix(c(-12.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.22869652614776
# Lower = 1.15632242237598
# Upper = 1.30560051777384


# Change PA from 20th to 80th percentile w/ genetic risk at 20th
48.6690567680804 - 30.1581806907346
k1 <- matrix(c(-18.51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.35201840457166
# Lower = 1.2370086325656
# Upper = 1.47772110733718

# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.26458771666326
# Lower = 1.22745994653166
# Upper = 1.30283851432733

# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-7.14, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.42060668211982
# Lower = 1.35803616255034
# Upper = 1.48606009245994

# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.64, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.55379453447328
# Lower = 1.45285617534813
# Upper = 1.66174566782602


# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.51, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.70974586712398
# Lower = 1.55747541080022
# Upper = 1.87690342324288


# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.5482318331584
# Lower = 1.46466325392075
# Upper = 1.63656854419501


# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(-7.14, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.73924549374783
# Lower = 1.63023316540078
# Upper = 1.85554738532047

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.64, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.9023070750731
# Lower = 1.75334455576876
# Upper = 2.06392531118135


# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.51, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.09323793297387
# Lower = 1.88650081486633
# Upper = 2.32263087803182




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.96581521667627
# Lower = 1.80417941986619
# Upper = 2.14193190741671


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-7.14, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.20834837775573
# Lower = 2.01454124876811
# Upper = 2.42080054728019


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-12.64, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.41539032777854
# Lower = 2.17625700323156
# Upper = 2.68080030385334


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-18.51, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.65781835293334
# Lower = 2.35140757365522
# Upper = 3.004157372092


# --------
# REPEATING ANALYSIS FOR WHITE SPECIFICALLY
# Given most of sample is white unsurprisingly v similar
# ---------


# MODEL:
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = whitedata)

summary(fit.lin)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
48.6690567680804 - 41.5283167027557
k1 <- matrix(c(-7.14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.11652155618926
# Lower = 1.07596845150906
# Upper = 1.15860310187245


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
48.6690567680804 - 36.0267497376078
k1 <- matrix(c(-12.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.21545682777667
# Lower = 1.13840024268355
# Upper = 1.29772925619412


# Change PA from 20th to 80th percentile w/ genetic risk at 20th
48.6690567680804 - 30.1581806907346
k1 <- matrix(c(-18.51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.33073773344321
# Lower = 1.20903339548728
# Upper = 1.46469313570603

# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.26015859525868
# Lower = 1.22046752845051
# Upper = 1.30114046313091

# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-7.14, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.4069942358235
# Lower = 1.34049264596968
# Upper = 1.47679495713199

# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.64, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53166836868863
# Lower = 1.42483195487425
# Upper = 1.64651556530281


# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.51, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.67694059283352
# Lower = 1.51662264737505
# Upper = 1.85420530067913


# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53814989243258
# Lower = 1.44916489826289
# Upper = 1.63259895021358


# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(-7.14, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.71737751155117
# Lower = 1.60208179701826
# Upper = 1.84097061877302

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.64, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.86955478890114
# Lower = 1.7126827187969
# Upper = 2.04079545519001


# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.51, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.04687410155165
# Lower = 1.83022812317304
# Upper = 2.28916468638853




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.94605600484178
# Lower = 1.77474461325982
# Upper = 2.13390363080162


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-7.14, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.1728134789574
# Lower = 1.96881707621819
# Upper = 2.39794670178681


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-12.64, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.36534705832074
# Lower = 2.11482861054949
# Upper = 2.6455414298811


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-18.51, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.5896901570367
# Lower = 2.2702843233521
# Upper = 2.95403313165223



# -------
# NOTHING is significant in black or Asian analyses
# Not surprising - just do in appendix with some graphs?
# -------


# -------
# Creating Quintile FIGURE
# -------


# ---------
# FIGURES to compare HR for different genetic and PA groups
# COMPARING DECILES from 20th to 80th
# ---------


# Selecting coefficients from No Highschool model (Obs)
NoHSObsCoeff <- 1.00

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLNoHSObs <- 1.00
ULNoHSObs <- 1.00


# Selecting coefficients from No Highschool model (IV)
NoHSIVCoeff <- 1.06

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLNoHSIV <- 0.97
ULNoHSIV <- 1.16



PointCompNoHSIV <- c(LLNoHSIV, NoHSIVCoeff, ULNoHSIV)
PointCompNoHSIV <- as.data.frame(PointCompNoHSIV)

PointCompNoHSObs <- c(LLNoHSObs, NoHSObsCoeff, ULNoHSObs)
PointCompNoHSObs <- as.data.frame(PointCompNoHSObs)

PointyNoHS <- cbind(PointCompNoHSIV, PointCompNoHSObs)

# Transpose
PointyTNoHS <- t(PointyNoHS)
PointyTNoHS <- as.data.frame(PointyTNoHS)

colnames(PointyTNoHS) <- c("LL","Mean","UL")

PointyTNoHS$Model <- c("40th Overall PA", "20th Overall PA")


# Selecting coefficients from Highschool model (Obs)
HSObsCoeff <- 1.10

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLHSObs <- 0.98
ULHSObs <- 1.24


# Selecting coefficients from No Highschool model (IV)
HSIVCoeff <- 1.12

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLHSIV <- 0.99
ULHSIV <- 1.26



PointCompHSIV <- c(LLHSIV, HSIVCoeff, ULHSIV)
PointCompHSIV <- as.data.frame(PointCompHSIV)

PointCompHSObs <- c(LLHSObs, HSObsCoeff, ULHSObs)
PointCompHSObs <- as.data.frame(PointCompHSObs)

PointyHS <- cbind(PointCompHSIV, PointCompHSObs)

# Transpose
PointyTHS <- t(PointyHS)
PointyTHS <- as.data.frame(PointyTHS)

colnames(PointyTHS) <- c("LL","Mean","UL")

PointyTHS$Model <- c("60th Overall PA", "80th Overall PA")


# Merging into one dataset then facet wrapping
PointyTHS$Gene <- "Low Genetic Risk"
PointyTNoHS$Gene <- "High Genetic Risk"
PlotDF <- rbind(PointyTHS, PointyTNoHS)

PlotDF$Genetic.Risk <- as.factor(PlotDF$Gene)

# Creating PA Risk Colors ((For our purposes makes more sense to flip this)
PlotDF$Physical.Activity <- as.factor(PlotDF$Model)

# For vertical forest plot
ggplot(PlotDF, aes(x = Model, y = Mean)) +
  geom_errorbar(aes(ymin = LL,
                    ymax = UL, color = Genetic.Risk),
                width = 0.05,
                size  = 0.5) + geom_point(shape = 15, size  = 4, aes(color = Genetic.Risk)) + theme_bw() + theme(axis.title  = element_text(face  = "bold")) + ylab("Adjusted Hazard Ratio") + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", subtitle = "High = 80th Percentile, Low = 20th Percentile", axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Model Type")


# For horizontal forest plot
ggplot(PlotDF, aes(x = Mean, y = Model, color = Genetic.Risk)) +
  geom_linerange(aes(xmin = LL,
                     xmax = UL)) + geom_point(shape = 15, size  = 4, aes(color = Genetic.Risk)) + theme_bw() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio")


# ADDING HAZARD RATIOS VIA ANNOTATION
# From: https://www.khstats.com/blog/forest-plots/

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# MODEL USED IN PRESENTATION
ggplot(PlotDF, aes(x = Mean, y = Model, color = Genetic.Risk)) +
  geom_linerange(aes(xmin = LL,
                     xmax = UL)) + geom_point(shape = 15, size  = 3, aes(color = Genetic.Risk)) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, subtitle = "High = 80th Percentile, Low = 20th Percentile", axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = Mean+0.32, y = Model, label = LABEL)) + expand_limits(x = 2) + scale_colour_manual(values=cbbPalette)




# ADDING HAZARD RATIOS VIA ANNOTATION
# From: https://www.khstats.com/blog/forest-plots/

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[4] <- "1 (Reference Group)"

install.packages('ggforce')
library(ggforce)



# MODEL WORKS
ggplot(PlotDF, aes(x = Mean, y = Model)) +
  geom_linerange(aes(xmin = LL,
                     xmax = UL)) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 1.6, y = Model, hjust = 0, label = LABEL)) + expand_limits(x = 2) + scale_colour_manual(values=cbbPalette) +   geom_vline(xintercept = 1, linetype="dashed") +   annotate("text", x = 1.7, y = 2.4, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Physical.Activity, scales = "free_y",space = "free", strip.position = "bottom")



# NOW splitting by PA PERCENTILE as variables and PGS as ind observations

# Adding column for PGS
PlotDF$PGS <- "20th Genetic Risk"

# Adding observations by other PGS groups
# 20th PA
D1 <- c(1.24, 1.28, 1.32, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.28 (1.24-1.32)", "40th Genetic Risk")

D2 <- c(1.50, 1.59, 1.68, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.59 (1.50-1.68)", "60th Genetic Risk")

D3 <- c(1.87, 2.04, 2.22, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "2.04 (1.87-2.22)", "80th Genetic Risk")

# 40th GS
D4 <- c(1.24, 1.36, 1.50, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.36 (1.24-1.50)", "40th Genetic Risk")

D5 <- c(1.25, 1.41, 1.59, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.41 (1.25-1.59)", "40th Genetic Risk")

D6 <- c(1.27, 1.43, 1.62, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "40th Genetic Risk")

# 60th GS
D7 <- c(1.52, 1.69, 1.88, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.69 (1.52-1.88)", "60th Genetic Risk")

D8 <- c(1.54, 1.75, 1.99, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.75 (1.54-1.99)", "60th Genetic Risk")

D9 <- c(1.56, 1.77, 2.02, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "60th Genetic Risk")

# 80th GS
D10 <- c(1.92, 2.17, 2.45, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "2.17 (1.92-2.45)", "80th Genetic Risk")

D11 <- c(1.95, 2.25, 2.60, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "2.25 (1.95-2.60)", "80th Genetic Risk")

D12 <- c(1.97, 2.28, 2.63, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "2.28 (1.97-2.63)", "80th Genetic Risk")

# Merging all with PlotDF
# PlotDF <- rbind(PlotDF, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12)

# Bunching up... What if we break it up?
# Do 20th vs 40th PA risk, etc...

PlotDF <- rbind(PlotDF, D1, D2, D3, D4, D7, D10)


# ---
# 20th vs 40th PA
# ---

# PlotDF20vs40 <- subset(PlotDF, Physical.Activity == "20th Overall PA" | Physical.Activity == "40th Overall PA")

# Remove top two rows here
PlotDF <- PlotDF[-c(1:2), ]

# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = PGS)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.6, y = PGS, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Physical.Activity, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3.5), ylim = c(0,5), expand = TRUE)



# --------
# REPEAT this for 20th PA vs 60th PA
# --------

# Saving earliest results here
#PlotDF20vs40 <- PlotDF


# Adding observations by other PGS groups
# 20th PA
D1 <- c(1.24, 1.28, 1.32, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.28 (1.24-1.32)", "40th Genetic Risk")

D2 <- c(1.50, 1.59, 1.68, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.59 (1.50-1.68)", "60th Genetic Risk")

D3 <- c(1.87, 2.04, 2.22, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "2.04 (1.87-2.22)", "80th Genetic Risk")

# 40th GS
D4 <- c(1.24, 1.36, 1.50, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.36 (1.24-1.50)", "40th Genetic Risk")

D5 <- c(1.25, 1.41, 1.59, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.41 (1.25-1.59)", "40th Genetic Risk")

D6 <- c(1.27, 1.43, 1.62, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "40th Genetic Risk")

# 60th GS
D7 <- c(1.52, 1.69, 1.88, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.69 (1.52-1.88)", "60th Genetic Risk")

D8 <- c(1.54, 1.75, 1.99, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.75 (1.54-1.99)", "60th Genetic Risk")

D9 <- c(1.56, 1.77, 2.02, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "60th Genetic Risk")

# 80th GS
D10 <- c(1.92, 2.17, 2.45, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "2.17 (1.92-2.45)", "80th Genetic Risk")

D11 <- c(1.95, 2.25, 2.60, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "2.25 (1.95-2.60)", "80th Genetic Risk")

D12 <- c(1.97, 2.28, 2.63, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "2.28 (1.97-2.63)", "80th Genetic Risk")

# Merging all with PlotDF
# PlotDF <- rbind(PlotDF, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12)

# Bunching up... What if we break it up?
# Do 20th vs 40th PA risk, etc...

PlotDF <- rbind(PlotDF, D1, D2, D3, D5, D8, D11)

# Drop all NOT 20th or 60th PA (2 and 3 rows)
PlotDF <- PlotDF[-c(2:3), ]


# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = PGS)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.6, y = PGS, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Physical.Activity, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3.5), ylim = c(0,5), expand = TRUE)


# Saving results for table
PlotDF20v60 <- PlotDF





# --------
# REPEAT this for 20th PA vs 80th PA
# --------

# Adding observations by other PGS groups
# 20th PA
D1 <- c(1.24, 1.28, 1.32, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.28 (1.24-1.32)", "40th Genetic Risk")

D2 <- c(1.50, 1.59, 1.68, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.59 (1.50-1.68)", "60th Genetic Risk")

D3 <- c(1.87, 2.04, 2.22, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "2.04 (1.87-2.22)", "80th Genetic Risk")

# 40th GS
D4 <- c(1.24, 1.36, 1.50, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.36 (1.24-1.50)", "40th Genetic Risk")

D5 <- c(1.25, 1.41, 1.59, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.41 (1.25-1.59)", "40th Genetic Risk")

D6 <- c(1.27, 1.43, 1.62, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "40th Genetic Risk")

# 60th GS
D7 <- c(1.52, 1.69, 1.88, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.69 (1.52-1.88)", "60th Genetic Risk")

D8 <- c(1.54, 1.75, 1.99, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.75 (1.54-1.99)", "60th Genetic Risk")

D9 <- c(1.56, 1.77, 2.02, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "60th Genetic Risk")

# 80th GS
D10 <- c(1.92, 2.17, 2.45, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "2.17 (1.92-2.45)", "80th Genetic Risk")

D11 <- c(1.95, 2.25, 2.60, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "2.25 (1.95-2.60)", "80th Genetic Risk")

D12 <- c(1.97, 2.28, 2.63, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "2.28 (1.97-2.63)", "80th Genetic Risk")

# Merging all with PlotDF
# PlotDF <- rbind(PlotDF, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12)

# Bunching up... What if we break it up?
# Do 20th vs 40th PA risk, etc...

PlotDF <- rbind(PlotDF, D1, D2, D3, D6, D9, D12)


# Drop all NOT 20th or 80th PA (2 and 3 rows)
PlotDF <- PlotDF[-c(1,3), ]

# Typo here...
PlotDF$LABEL[7] <- paste("1.77 (1.56-2.02)")
1.56
1.77
2.02

#PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = PGS)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.6, y = PGS, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Physical.Activity, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3.5), ylim = c(0,5), expand = TRUE)


# Saving results for table
PlotDF20v80 <- PlotDF















# -------
# WAIT looks like a much easier way to do this...
# forestploter package
# -------

install.packages('forestploter')
library(forestploter)

# Drop then re-add label
PlotDF <- PlotDF[-8]

# Adding space to allow for display
PlotDF$` ` <- paste(rep(" ", 20), collapse = " ")

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[4] <- "1 (Reference Group)"



p <- forest(data = PlotDF[ , c(1:3,8:9)],
            est = PlotDF$Mean,
            lower = PlotDF$LL,
            upper = PlotDF$UL,
            ci_column = 5,
            sizes = 0.4,
            ref_line = 1,
            xlim = c(0,2)
)

plot(p)

# -------
# NOW use model with PA *AND* % MVPA TOGETHER
# Look at effect of PA intensity and PGS on risk
# -------

