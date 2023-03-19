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
dim(asiandata)

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

# -----------
# THEN repeating analyses excluding FIRST YEAR
# COULD recalc percentiles but makes NO DIFF (only 200 diff...)
# -----------


# Exclude if ind had CAD within one year
dataoneyear <- subset(data, TimeYear > 1 | Status == 0)
# 77474 - 77257
# 217 cases EXCLUDED with this method



# MODEL:
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = dataoneyear)

summary(fit.lin)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
48.6690567680804 - 41.5283167027557
k1 <- matrix(c(-7.14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.10433280163195
# Lower = 1.06445321028489
# Upper = 1.14570647631742

# Change PA from 20th to 60th percentile w/ genetic risk at 20th
48.6690567680804 - 36.0267497376078
k1 <- matrix(c(-12.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.19206580624316
# Lower = 1.11692084964222
# Upper = 1.27226641607536


# Change PA from 20th to 80th percentile w/ genetic risk at 20th
48.6690567680804 - 30.1581806907346
k1 <- matrix(c(-18.51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.29340328961848
# Lower = 1.17577423420492
# Upper = 1.42280041604004

# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.26299857661125
# Lower = 1.22317266182402
# Upper = 1.30412120406886

# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-7.14, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.39477075656627
# Lower = 1.32894511623526
# Upper = 1.46385688890108


# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.64, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.50557741651205
# Lower = 1.40091319604472
# Upper = 1.61806124998394


# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.51, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.63356651377246
# Lower = 1.4780464098859
# Upper = 1.80545044937033


# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.54461099050421
# Lower = 1.45515164237164
# Upper = 1.6395700918826


# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(-7.14, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.70576458257501
# Lower = 1.59119937038654
# Upper = 1.82857840778335

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.64, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.84127794572744
# Lower = 1.68697662365225
# Upper = 2.00969262163358


# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.51, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.99780493629901
# Lower = 1.78689663580552
# Upper = 2.23360684861408




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.95871075052576
# Lower = 1.78609448508849
# Upper = 2.14800943413422


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-7.14, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.16306853071473
# Lower = 1.9597651280601
# Upper = 2.38746235534857


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-12.64, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.33491211002263
# Lower = 2.08759331023789
# Upper = 2.61153095997854


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-18.51, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.53340292814111
# Lower = 2.22127299125762
# Upper = 2.88939289388297



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



# -----------
# THEN repeating analyses excluding FIRST YEAR
# -----------


# Exclude if ind had CAD within one year
whitedataoneyear <- subset(whitedata, TimeYear > 1 | Status == 0)
# 66180 - 66000
# 180 cases EXCLUDED with this method



# MODEL:
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = whitedataoneyear)

summary(fit.lin)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
48.6690567680804 - 41.5283167027557
k1 <- matrix(c(-7.14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.0947827724586
# Lower = 1.0523184713274
# Upper = 1.1389606393208


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
48.6690567680804 - 36.0267497376078
k1 <- matrix(c(-12.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.17387701140841
# Lower = 1.09447880772426
# Upper = 1.25903510254197


# Change PA from 20th to 80th percentile w/ genetic risk at 20th
48.6690567680804 - 30.1581806907346
k1 <- matrix(c(-18.51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.26460601728347
# Lower = 1.1413404352174
# Upper = 1.40118437024002

# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.26076681589304
# Lower = 1.21821569878812
# Upper = 1.30480420309666

# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-7.14, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.38026579012718
# Lower = 1.31054829184935
# Upper = 1.45369206403453

# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.64, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.47998518192341
# Lower = 1.36983341784695
# Upper = 1.59899452749195


# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.51, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.59437330176965
# Lower = 1.4319238677497
# Upper = 1.77525235988329


# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53953256932428
# Lower = 1.44419007884385
# Upper = 1.6411693770308


# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(-7.14, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.68545373453515
# Lower = 1.56450296323668
# Upper = 1.81575513630315

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-12.64, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.80722189144429
# Lower = 1.64537442500525
# Upper = 1.98498950468679


# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(-18.51, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.94690215097137
# Lower = 1.72726466649513
# Upper = 2.19446854844097




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.94876168654
# Lower = 1.76533264482194
# Upper = 2.15125014657466


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-7.14, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.13347072205137
# Lower = 1.91947472702473
# Upper = 2.37132443463098


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-12.64, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.28760654454278
# Lower = 2.02905452646254
# Upper = 2.57910452103939


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(-18.51, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.46441575504997
# Lower = 2.14046156346347
# Upper = 2.83739970733754






# -----------
# ANALYSES including PA *AND* % MVPA
# Interaction was insig, so fitting separately in model
# Comparing quintiles for MVPA and genetic risk
# -----------

PAIntensity <- quantile(data$PercentMVPA, probs = c(0.20, 0.40, 0.60, 0.80))
PAIntensity
# 20% 0.260799896947289
# 40% 0.327906327735732
# 60% 0.387017057352891
# 80% 0.455997880283066


# MODEL:
fit.linMVPA <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = data)

summary(fit.linMVPA)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
0.455997880283066 - 0.387017057352891
k1 <- matrix(c(0, -0.069, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.13805326040757
# Lower = 1.08901382596577
# Upper = 1.18930099200136


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
0.455997880283066 - 0.327906327735732
k1 <- matrix(c(0, -0.128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.27111741623007
# Lower = 1.17138491948454
# Upper = 1.37934120455844


# Change PA from 20th to 80th percentile w/ genetic risk at 20th
0.455997880283066 - 0.260799896947289
k1 <- matrix(c(0, -0.195, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.44118706477043
# Lower = 1.27250527234834
# Upper = 1.63222911589873

# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.26440832680884
# Lower = 1.22728524150755
# Upper = 1.30265431607384


# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.069, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.43896401881128
# Lower = 1.36449456080398
# Upper = 1.51749776577597


# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.60721144543304
# Lower = 1.4734002658294
# Upper = 1.75317508095933



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.82224892518492
# Lower = 1.60339378755708
# Upper = 2.07097668152802


# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.54782289831424
# Lower = 1.46427509911445
# Upper = 1.63613772165817


# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.76150489596001
# Lower = 1.64112107632193
# Upper = 1.89071942543403

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.96746464328694
# Lower = 1.78256190694573
# Upper = 2.17154709045516


# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.23070233960595
# Lower = 1.94665600142989
# Upper = 2.55619530326283




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.96501238925604
# Lower = 1.80344014118515
# Upper = 2.14106007831912


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.2362887563341
# Lower = 2.0308097465192
# Upper = 2.46255830231166


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.128, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.49776147109121
# Lower = 2.21889273074178
# Upper = 2.81167822131811


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.195, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.83195043750943
# Lower = 2.43487709274321
# Upper = 3.29377745776661





# -----------
# REPEATING MVPA ANALYSES WITH WHITE ONLY
# ANALYSES including PA *AND* % MVPA
# Interaction was insig, so fitting separately in model
# Comparing quintiles for MVPA and genetic risk
# -----------

PAIntensity <- quantile(whitedata$PercentMVPA, probs = c(0.20, 0.40, 0.60, 0.80))
PAIntensity
# 20% 0.259328225722848
# 40% 0.326224308037015
# 60% 0.385086263660446
# 80% 0.454333631101671


# MODEL:
fit.linMVPA <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = whitedata)

summary(fit.linMVPA)


# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
0.454333631101671 - 0.385086263660446
k1 <- matrix(c(0, -0.069, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.14817447628353
# Lower = 1.09478715788506
# Upper = 1.20416522836795


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
0.454333631101671 - 0.326224308037015
k1 <- matrix(c(0, -0.128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.29216799871768
# Lower = 1.18293105438744
# Upper = 1.4114923525908


# Change PA from 20th to 80th percentile w/ genetic risk at 20th
0.454333631101671 - 0.259328225722848
k1 <- matrix(c(0, -0.195, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.47770420971715
# Lower = 1.29166275324668
# Upper = 1.69054168816677

# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.26029062347419
# Lower = 1.22058682131071
# Upper = 1.30128592893651


# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.069, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.44703352657253
# Lower = 1.36631693528161
# Upper = 1.53251853428381


# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.62850721273731
# Lower = 1.48244003738407
# Upper = 1.78896661926188



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.86233675977486
# Lower = 1.62172439496167
# Upper = 2.13864835331081


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
k1 <- matrix(c(0, 0, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.94664312651781
# Lower = 1.77524413276799
# Upper = 2.13459061324175


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.23508595230052
# Lower = 2.01476382786725
# Upper = 2.47950114304925


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.128, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.51538995301005
# Lower = 2.21380689914909
# upper = 2.85805714045604


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.195, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.87656274287232
# Lower = 2.44349651949823
# Upper = 3.3863822385883




# -----------
# DOING SENSITIVITY ANALYSIS - NO cases in first year
# ANALYSES including PA *AND* % MVPA
# Interaction was insig, so fitting separately in model
# Comparing quintiles for MVPA and genetic risk
# -----------

PAIntensity <- quantile(dataoneyear$PercentMVPA, probs = c(0.20, 0.40, 0.60, 0.80))
PAIntensity
# 20% 0.260925958199719
# 40% 0.327967752295678
# 60% 0.387097103895122
# 80% 0.456111507664084


# MODEL:
fit.linMVPA <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = dataoneyear)

summary(fit.linMVPA)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
0.456111507664084 - 0.387097103895122
k1 <- matrix(c(0, -0.069, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.13298768547527
# Lower = 1.08053914145918
# Upper = 1.18798204172884


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
0.456111507664084 - 0.327967752295678
k1 <- matrix(c(0, -0.128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.26064166259641
# Lower = 1.15453092815621
# Upper = 1.37650483215017


# Change PA from 20th to 80th percentile w/ genetic risk at 20th
0.456111507664084 - 0.260925958199719
k1 <- matrix(c(0, -0.195, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.42313172687506
# Lower = 1.24471805122214
# Upper = 1.62711861537624

# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.2628404164788
# Lower = 1.22301858703326
# Upper = 1.30395885590002


# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.069, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.43078264059093
# Lower = 1.35127673418718
# Upper = 1.51496648526829


# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.59198924222378
# Lower = 1.44983129579216
# Upper = 1.7480859702173



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.79718826267109
# Lower = 1.56602568797971
# Upper = 2.06247296980774


# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.5442508386965
# Lower = 1.45481035182747
# Upper = 1.63919004963036

# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.74961718352798
# Lower = 1.62134242550257
# Upper = 1.88804057720719

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.94674694476025
# Lower = 1.75063143296217
# Upper = 2.16483241165205


# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.1976723628024
# Lower = 1.89808255255837
# Upper = 2.54454886997174




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.95800459259776
# Lower = 1.78544676804829
# Upper = 2.147239589128


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.21839509151728
# Lower = 1.99996064848424
# Upper = 2.46068680691182


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.128, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.46834216498385
# Lower = 2.17319287634398
# Upper = 2.80357676014801


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.195, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.78649845709294
# Lower = 2.36849271433859
# Upper = 3.27827635034531




# -----------
# REPEATING WITH ONLY CASES AFTER FIRST YEAR
# REPEATING MVPA ANALYSES WITH WHITE ONLY
# ANALYSES including PA *AND* % MVPA
# Interaction was insig, so fitting separately in model
# Comparing quintiles for MVPA and genetic risk
# -----------

PAIntensity <- quantile(whitedataoneyear$PercentMVPA, probs = c(0.20, 0.40, 0.60, 0.80))
PAIntensity
# 20% 0.259485956939171
# 40% 0.32629579734654
# 60% 0.385148023897019
# 80% 0.454390754308548


# MODEL:
fit.linMVPA <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = whitedataoneyear)

summary(fit.linMVPA)

# NATURALLY:
# 20th/20th PA/Genetic Risk = 1 (REF)

# Change PA from 20th to 40th percentile w/ genetic risk at 20th
0.454390754308548 - 0.385148023897019
k1 <- matrix(c(0, -0.069, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.14488921512935
# Lower = 1.08783298486525
# Upper = 1.20493801268755


# Change PA from 20th to 60th percentile w/ genetic risk at 20th
0.454390754308548 - 0.32629579734654
k1 <- matrix(c(0, -0.128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.28531768916804
# Lower = 1.1690297770128
# Upper = 1.4131732095907


# Change PA from 20th to 80th percentile w/ genetic risk at 20th
0.454333631101671 - 0.259328225722848
k1 <- matrix(c(0, -0.195, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.4657863061748
# Lower = 1.26860968337226
# upper = 1.69360956607099

# --------
# REPEATING COMPARISON WITH 40th percentile genetic risk
# --------

# 20th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, 0, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.26029062347419
# Lower = 1.22058682131071
# Upper = 1.30128592893651


# 40th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.069, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.44356626039541
# Lower = 1.35732102478276
# Upper = 1.53529158548585



# 60th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.6206295119679
# Lower = 1.46511490708796
# Upper = 1.79265121278547



# 80th PA/40th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.84817852118949
# Lower = 1.59309886218588
# Upper = 2.14410036141726


# --------
# REPEATING COMPARISON WITH 60th percentile genetic risk
# --------

# 20th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.53978665237384
# Lower = 1.44441091566144
# Upper = 1.6414601337618


# 40th PA/60th genetic risk
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.76288513190293
# Lower = 1.62426874966004
# Upper = 1.91333114605257

# 60th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.128, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.97911502184094
# Lower = 1.76515096187917
# Upper = 2.21901489122865


# 80th PA/60th genetic risk
-0.846868164911578 - -0.266874315609926
k1 <- matrix(c(0, -0.195, 1.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.2569981894803
# Lower = 1.92707853079906
# Upper = 2.64340074672781




# --------
# REPEATING COMPARISON WITH 80th percentile genetic risk
# --------

# 20th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 0, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.94925903163336
# Lower = 1.76575007565993
# Upper = 2.15183950706284


# 40th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.069, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.23168564281051
# Lower = 1.99654281157327
# Upper = 2.49452242118566


# 60th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.128, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.50541711412893
# Lower = 2.18454021672264
# Upper = 2.87342611855752


# 80th PA/80th genetic risk
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, -0.195, 1.67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.85719719575573
# Lower = 2.39819719206226
# Upper = 3.40404694094999




# -------
# NOTHING is significant in black or Asian analyses
# Not surprising - just do in appendix with some graphs?
# -------


# -------
# Creating Quintile FIGURE
# -------

# ---------
# Physical Activity Volume & Genetic Risk Plots
# FIGURES to compare HR for different genetic and PA groups
# COMPARING DECILES from 20th to 80th
# ---------


# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.12, 1.22, 1.33)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.08, 1.14, 1.21)
RefUL <- c(1.00, 1.16, 1.30, 1.46)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th PA Risk", "40th PA Risk", "60th PA Risk", "80th PA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
FortyCoef <- c(1.26, 1.41, 1.53, 1.68)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
FortyLL <- c(1.22, 1.34, 1.42, 1.52)
FortyUL <- c(1.30, 1.48, 1.65, 1.85)

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
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2, y = Model, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 2.35, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3), ylim = c(0,5), expand = TRUE)







# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.12, 1.22, 1.33)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.08, 1.14, 1.21)
RefUL <- c(1.00, 1.16, 1.30, 1.46)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th PA Risk", "40th PA Risk", "60th PA Risk", "80th PA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
SixtyCoef <- c(1.54, 1.72, 1.87, 2.05)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
SixtyLL <- c(1.45, 1.60, 1.71, 1.83)
SixtyUL <- c(1.63, 1.84, 2.04, 2.29)

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
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.5, y = Model, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,4.7), ylim = c(0,5), expand = TRUE)







# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.12, 1.22, 1.33)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.08, 1.14, 1.21)
RefUL <- c(1.00, 1.16, 1.30, 1.46)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th PA Risk", "40th PA Risk", "60th PA Risk", "80th PA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
EightyCoef <- c(1.95, 2.17, 2.37, 2.59)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
EightyLL <- c(1.77, 1.97, 2.11, 2.27)
EightyUL <- c(2.13, 2.40, 2.65, 2.95)

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
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 3.1, y = Model, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3.6, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,4.5), ylim = c(0,5), expand = TRUE)




# -------
# NOW use model with PA *AND* % MVPA TOGETHER
# Look at effect of PA intensity and PGS on risk
# FIGURES to compare HR for different genetic and PA groups
# COMPARING DECILES from 20th to 80th
# ---------


# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.15, 1.29, 1.48)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.09, 1.18, 1.29)
RefUL <- c(1.00, 1.20, 1.41, 1.69)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th % MVPA Risk", "40th % MVPA Risk", "60th % MVPA Risk", "80th % MVPA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
FortyCoef <- c(1.26, 1.45, 1.63, 1.86)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
FortyLL <- c(1.22, 1.37, 1.48, 1.62)
FortyUL <- c(1.30, 1.53, 1.79, 2.14)

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
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and % MVPA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.3, y = Model, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 2.7, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3.3), ylim = c(0,5), expand = TRUE)







# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.15, 1.29, 1.48)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.09, 1.18, 1.29)
RefUL <- c(1.00, 1.20, 1.41, 1.69)

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
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.8, y = Model, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3.3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,4), ylim = c(0,5), expand = TRUE)







# Selecting coefficients from No Highschool model (Obs)
RefCoef <- c(1.00, 1.12, 1.22, 1.33)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
RefLL <- c(1.00, 1.08, 1.14, 1.21)
RefUL <- c(1.00, 1.16, 1.30, 1.46)

RefvPA20 <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA20) <- c("LL","Mean","UL")

RefvPA20$Model <- c("20th % MVPA Risk", "40th % MVPA Risk", "60th % MVPA Risk", "80th % MVPA Risk")
RefvPA20$Genetic.Risk <- "20th Percentile Genetic Risk"



# Selecting coefficients from No Highschool model (Obs)
EightyCoef <- c(1.95, 2.24, 2.52, 2.88)

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
EightyLL <- c(1.77, 2.01, 2.21, 2.44)
EightyUL <- c(2.13, 2.48, 2.86, 3.39)

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
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 3.7, y = Model, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 4.2, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,5), ylim = c(0,5), expand = TRUE)



