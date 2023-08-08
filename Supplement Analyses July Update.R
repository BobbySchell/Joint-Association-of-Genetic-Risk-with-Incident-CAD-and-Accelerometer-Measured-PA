# ----------
# Sensitivity Analyses for New Manuscript
# 7/20/23
# ----------



dx download FinalDatasetwNewExposuresFINAL.csv

dx download FINALANALYSISDATAPAPER3.csv
# Need this for mediation analysis

# Maybe this one...
dx download FINALIZEDPADATASET.csv

# Maybe this one...
dx download FINALCovarsforCAD.csv

dx download FINALCovarsforCADSUBPA.csv

# Reading in needed packages
install.packages("survival")
library(survival)

install.packages("ggplot2")
library(ggplot2)

install.packages("multcomp")
library(multcomp)


data <- read.csv("FinalDatasetwNewExposuresFINAL.csv")

newdata <- read.csv("FINALCovarsforCADSUBPA.csv")

otherdata <- read.csv("FINALANALYSISDATAPAPER3.csv")
otherotherdata <- read.csv("FINALIZEDPADATASET.csv")
otherotherotherdata <- read.csv("FINALCovarsforCAD.csv")

# ------
# Just getting remaining Model 1 standardized results
# ------

# ENMO
fit.linENMOSTAND <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedENMO + StandPGS + StandardizedENMO*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
# Coefficients:
# ENMO = 0.81 (0.76-0.87)
# PGS = 1.52 (1.43-1.61)
# Interaction term = 1.06 (1.00-1.12)
# Interaction is actually significant at 10% level (fringe for PA volume)



# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMO <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedENMO + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
# Coefficients:
# ENMO = 1.13 (0.98-1.30)
# MVPA Mins = 0.70 (0.60-0.81)
# PGS = 1.52 (1.43-1.61)
# Interaction term = 1.05 (0.99-1.12)

# ---------
# Getting Model 0 standardized results
# ---------

# ENMO
fit.linENMOSTAND <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedENMO + StandPGS + StandardizedENMO*StandPGS + strata(Biological.Sex), data = data)
# Coefficients:
# ENMO = 0.79 (0.74-0.84)
# PGS = 1.55 (1.46-1.64)
# Interaction term = 1.06 (1.00-1.13)
# Interaction is actually significant at 10% level (fringe for PA volume)



# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMO <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedENMO + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + strata(Biological.Sex), data = data)
# Coefficients:
# ENMO = 1.09 (0.95-1.26)
# MVPA Mins = 0.69 (0.59-0.80)
# PGS = 1.55 (1.46-1.64)
# Interaction term = 1.06 (1.00-1.13)


# ---------
# First Year Excluded
# ---------


dataoneyear <- subset(data, TimeYear > 1 | Status == 0)

# ENMO
fit.linENMO <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataoneyear)



# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMO <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataoneyear)





# Genetic Risk Diffs - 90th on down
1.23896364242179 - 0.811623092364168 # -0.4273406
1.23896364242179 - 0.50056964352169 # -0.738394
1.23896364242179 - 0.237628126974166 # -1.001336
1.23896364242179 - -0.0148077360175137 # -1.253771
1.23896364242179 - -0.260085293025043 # -1.499049
1.23896364242179 - -0.524242500848616 # -1.763206
1.23896364242179 - -0.83351688928712 # -2.072481
1.23896364242179 - -1.2687787156677 # -2.507742


# Change ENMO from 10th to 90th percentile w/ genetic risk at 90th
21.43 - 18.75 # 2.68
23.46 - 18.75 # 4.71
25.27 - 18.75 # 6.52
27.07 - 18.75 # 8.32
28.97 - 18.75 # 10.22
31.18 - 18.75 # 12.43
33.97 - 18.75 # 15.22
38.29 - 18.75 # 19.54


# -------
# Is this correct? That interaction is product of marginal increase in both
# COULD also be that it's product of LEVEL of both
# -------

# 90th
k1 <- matrix(c(2.68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k2 <- matrix(c(4.71, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k3 <- matrix(c(6.52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k4 <- matrix(c(8.32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k5 <- matrix(c(10.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k6 <- matrix(c(12.43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k7 <- matrix(c(15.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k8 <- matrix(c(19.54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)

delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825

# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k1 <- matrix(c(2.68, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-3.360106), nrow=1)
k2 <- matrix(c(4.71, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-5.905261), nrow=1)
k3 <- matrix(c(6.52, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-8.174587), nrow=1)
k4 <- matrix(c(8.32, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-10.431375), nrow=1)
k5 <- matrix(c(10.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-12.813540), nrow=1)
k6 <- matrix(c(12.43, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-15.584374), nrow=1)
k7 <- matrix(c(15.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-19.082395), nrow=1)
k8 <- matrix(c(19.54, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-24.498685), nrow=1)

delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.769487249751732
# Lower = 0.591476276533902
# Upper = 1.00107248764109


delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.708892155166331
# Lower = 0.562571147746568
# Upper = 0.893270281757763

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.666190440503722
# Lower = 0.540475298401312
# Upper = 0.821147061357478

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.630290489121154
# Lower = 0.520405814615279
# Upper = 0.763377521003049

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.596507663050134
# Lower = 0.499889349227525
# Upper = 0.711800306662626

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.562810445970893
# Lower = 0.47739845631035
# Upper = 0.663503607745303

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.526002613934141
# Lower = 0.449924879520322
# Upper = 0.614944321728827

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.482952676703008
# Lower = 0.413266293266569
# Upper = 0.564389817739506

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.423141126093645
# Lower = 0.354552919274139
# Upper = 0.50499771080254


# 10th
k0 <- matrix(c(0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k1 <- matrix(c(2.68, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-6.720749), nrow=1)
k2 <- matrix(c(4.71, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-11.811465), nrow=1)
k3 <- matrix(c(6.52, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-16.350478), nrow=1)
k4 <- matrix(c(8.32, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-20.864413), nrow=1)
k5 <- matrix(c(10.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-25.629123), nrow=1)
k6 <- matrix(c(12.43, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-31.171233), nrow=1)
k7 <- matrix(c(15.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-38.167833), nrow=1)
k8 <- matrix(c(19.54, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-49.001279), nrow=1)

delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.592085878507679
# Lower = 0.349814880944463
# Upper = 1.00214629686913


delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.531929241137851
# Lower = 0.332974250665395
# Upper = 0.849761556673116

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.490465533051865
# Lower = 0.320313179976265
# Upper = 0.751003874176125

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.456228925062577
# Lower = 0.309007718946706
# Upper = 0.673591044175994

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.424551875399178
# Lower = 0.297657372703111
# Upper = 0.605542853745263

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.393497958224027
# Lower = 0.285434326289834
# Upper = 0.542473798225832

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.360223386378164
# Lower = 0.270681076694238
# Upper = 0.479386626056354

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.322204217401425
# Lower = 0.25075160564013
# Upper = 0.414017519234783

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.27109798402828
# Lower = 0.215899169296501
# Upper = 0.340409447538291





# -------
# Repeat first year excluded analysis for MVPA
# -------

# Change MVPA Minutes from 10th to 90th percentile w/ genetic risk at 90th
41.76 - 31.68 # 10.08
51.84 - 31.68 # 20.16
59.04 - 31.68 # 27.36
67.68 - 31.68 # 36
76.32 - 31.68 # 44.64
87.84 - 31.68 # 56.16
100.80 - 31.68 # 69.12
120.96 - 31.68 # 89.28


# 90th
k1 <- matrix(c(0, 10.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k2 <- matrix(c(0, 20.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k3 <- matrix(c(0, 27.36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k4 <- matrix(c(0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k5 <- matrix(c(0, 44.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k6 <- matrix(c(0, 56.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k7 <- matrix(c(0, 69.12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k8 <- matrix(c(0, 89.28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.903177500655002
# Lower = 0.864233046442377
# Upper = 0.943876887197701

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.815729597689417
# Lower = 0.746898758563072
# Upper = 0.890903578186022

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.75849999949686
# Lower = 0.672973669374088
# Upper = 0.854895630273061

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.695099253580659
# Lower = 0.593856718652059
# Upper = 0.813601929814108

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.636997986353182
# Lower = 0.524041011316525
# Upper = 0.77430282335846

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.567013034477565
# Lower = 0.443550845925562
# Upper = 0.724840870490441

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.4974276227899
# Lower = 0.367678990483228
# Upper = 0.672962682989355

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.405766434618008
# Lower = 0.274618981541647
# Upper = 0.599544862260882

# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, 0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -12.63801), nrow=1)
k2 <- matrix(c(0, 20.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -25.27602), nrow=1)
k3 <- matrix(c(0, 27.36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -34.30317), nrow=1)
k4 <- matrix(c(0, 36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -45.13576), nrow=1)
k5 <- matrix(c(0, 44.64, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -55.96834), nrow=1)
k6 <- matrix(c(0, 56.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -70.41178), nrow=1)
k7 <- matrix(c(0, 69.12, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -86.66065), nrow=1)
k8 <- matrix(c(0, 89.28, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -111.93667), nrow=1)

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.695004387337457
# Lower = 0.593086998093315
# Upper = 0.814435487493718


delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.613607214162413
# Lower = 0.533650852347187
# Upper = 0.705543355952895

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.541743074046728
# Lower = 0.471112723802139
# Upper = 0.62296249591607

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.495624298260959
# Lower = 0.425871768852327
# Upper = 0.576801429427095

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.445434291966192
# Lower = 0.373501343347485
# Upper = 0.531220869732807

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.400326845805688
# Lower = 0.325193083034306
# Upper = 0.492819779490279

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.347207797550816
# Lower = 0.268575011144681
# Upper = 0.448862513925936

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.295825814915534
# Lower = 0.215494501374051
# Upper = 0.406102764629417

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.230590754928437
# Lower = 0.152196732307055
# Upper = 0.349364243584365


# 10th
k0 <- matrix(c(0, 0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -25.27804), nrow=1)
k2 <- matrix(c(0, 20.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -50.55608), nrow=1)
k3 <- matrix(c(0, 27.36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -68.61182), nrow=1)
k4 <- matrix(c(0, 36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -90.27871), nrow=1)
k5 <- matrix(c(0, 44.64, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -111.94560), nrow=1)
k6 <- matrix(c(0, 56.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -140.83479), nrow=1)
k7 <- matrix(c(0, 69.12, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -173.33513), nrow=1)
k8 <- matrix(c(0, 89.28, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -223.89121), nrow=1)

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.483003064663481
# Lower = 0.351722875307827
# Upper = 0.6632834451559

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.416851165549982
# Lower = 0.316728359041296
# Upper = 0.548624362991514

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.359759403061851
# Lower = 0.281650632155666
# upper = 0.459529691450813

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.323832258603012
# Lower = 0.256301709442093
# Upper = 0.409155802901974

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.28542345129332
# Lower = 0.225799620893486
# Upper = 0.360791334484213

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.251570201497624
# Lower = 0.196047527421034
# Upper = 0.322817467346262

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.21259444912359
# Lower = 0.159416843808444
# Upper = 0.283510818044239

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.175916361337113
# Lower = 0.12420822150372
# Upper = 0.249150706864946


delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.131029324187718
# Lower = 0.0826581281349287
# Upper = 0.207707144892811







# ---------
# MICE Imputation
# ---------


# mice library refuses to load, so switched to Amelia
install.packages("Amelia")
library(Amelia)

datasub <- data[ , c("AgeBaseline", "TimeYear", "Status", "p90012", "StandPGS", "SeasonWear", "Salt_InstChosen", "AlcIntake_InstChosen", "OilyFish_InstChosen", "FnVScore", "ProcMeat_InstChosen", "ParentHist", "MobilityDichot", "NewEmploy", "Townsend", "NewEduc", "SmokStat_InstChosen", "Biological.Sex", "REGION", "p22009_a1",
                     "p22009_a2", "p22009_a3", "p22009_a4", "p22009_a5", "p22009_a6", "p22009_a7", "p22009_a8", "p22009_a9", "p22009_a10", "Meds", "BMI_InstChosen", "SleepDur_InstChosen", "MVPAMins")]

# Converting characters to factors
datasub$SeasonWear <- as.factor(datasub$SeasonWear)
datasub$NewEduc <- as.factor(datasub$NewEduc)
datasub$SmokStat_InstChosen <- as.factor(datasub$SmokStat_InstChosen)
datasub$Biological.Sex <- as.factor(datasub$Biological.Sex)
datasub$REGION <- as.factor(datasub$REGION)
datasub$NewEduc <- as.factor(datasub$NewEduc)
datasub$Salt_InstChosen <- as.factor(datasub$Salt_InstChosen)
datasub$OilyFish_InstChosen <- as.factor(datasub$OilyFish_InstChosen)
datasub$AlcIntake_InstChosen <- as.factor(datasub$AlcIntake_InstChosen)

# Setting factor variables as 'nominal'
imputed_Data <- amelia(datasub, noms = c(6,16,17,18,19,7,8,9))

# Have 5 imputed datasets - taking average result
dataimpute1 <- imputed_Data$imputations$imp1
dataimpute2 <- imputed_Data$imputations$imp2
dataimpute3 <- imputed_Data$imputations$imp3
dataimpute4 <- imputed_Data$imputations$imp4
dataimpute5 <- imputed_Data$imputations$imp5

# ENMO
fit.linENMOimpute1 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute1)
fit.linENMOimpute2 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute2)
fit.linENMOimpute3 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute3)
fit.linENMOimpute4 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute4)
fit.linENMOimpute5 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute5)



# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMOimpute1 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute1)
fit.linMVPAMinsENMOimpute2 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute2)
fit.linMVPAMinsENMOimpute3 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute3)
fit.linMVPAMinsENMOimpute4 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute4)
fit.linMVPAMinsENMOimpute5 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = dataimpute5)




# Genetic Risk Diffs - 90th on down
1.23896364242179 - 0.811623092364168 # -0.4273406
1.23896364242179 - 0.50056964352169 # -0.738394
1.23896364242179 - 0.237628126974166 # -1.001336
1.23896364242179 - -0.0148077360175137 # -1.253771
1.23896364242179 - -0.260085293025043 # -1.499049
1.23896364242179 - -0.524242500848616 # -1.763206
1.23896364242179 - -0.83351688928712 # -2.072481
1.23896364242179 - -1.2687787156677 # -2.507742


# Change ENMO from 10th to 90th percentile w/ genetic risk at 90th
21.43 - 18.75 # 2.68
23.46 - 18.75 # 4.71
25.27 - 18.75 # 6.52
27.07 - 18.75 # 8.32
28.97 - 18.75 # 10.22
31.18 - 18.75 # 12.43
33.97 - 18.75 # 15.22
38.29 - 18.75 # 19.54


# -------
# Is this correct? That interaction is product of marginal increase in both
# COULD also be that it's product of LEVEL of both
# -------

# 90th
k1 <- matrix(c(2.68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k2 <- matrix(c(4.71, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k3 <- matrix(c(6.52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k4 <- matrix(c(8.32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k5 <- matrix(c(10.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k6 <- matrix(c(12.43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k7 <- matrix(c(15.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k8 <- matrix(c(19.54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)

delta.eta <- glht(fit.linENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.933376670245378lwr0.913434081113519upr0.953754656818064
#Estimate0.933241221030425lwr0.913306225422127upr0.953611343476624
#Estimate0.933055219798858lwr0.913119064828892upr0.953426641417278
#Estimate0.933292226460288lwr0.913351296723032upr0.953668520640791
#Estimate0.933104868383403lwr0.913164638876212upr0.953480520744122

delta.eta <- glht(fit.linENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.885882641502467lwr0.852887337243301upr0.920154421628508
#Estimate0.88565671978244lwr0.852677541047244upr0.919911440768592
#Estimate0.885346520593544lwr0.852370472578026upr0.919598328126438
#Estimate0.885741791073437lwr0.85275149522786upr0.920008378577332
#Estimate0.885429316273036lwr0.852445240086301upr0.919689661281193

delta.eta <- glht(fit.linENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.845577570828263lwr0.802295047993721upr0.891195115906314
# Estimate0.845279073358302lwr0.802021869683068upr0.890869362627
# Estimate0.844869272549382lwr0.801622078469797upr0.890449635644631
# Estimate0.845391469763797lwr0.802118163420296upr0.890999318731185
# Estimate0.844978647632826lwr0.801719417763145upr0.890572061915976

delta.eta <- glht(fit.linENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.807313928212051lwr0.754958855277667upr0.863299733659607
# Estimate0.806950277693864lwr0.754630842544437upr0.862897080212911
# Estimate0.806451087400634lwr0.754150857806823upr0.8623783287352
# Estimate0.807087202704131lwr0.75474646157156upr0.863057710018847
# Estimate0.806584313835069lwr0.754267716159006upr0.862529631571347

delta.eta <- glht(fit.linENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.768801385771112lwr0.708019448154907upr0.834801321212104
# Estimate0.768376021358795lwr0.707641598849642upr0.834323068568808
# Estimate0.767792186465829lwr0.707088755290957upr0.833706995319712
# Estimate0.768536178310276lwr0.707774780159294upr0.834513850915728
# Estimate0.767947995208322lwr0.707223344646184upr0.833886674993066

delta.eta <- glht(fit.linENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.726310473834679lwr0.657080470452192upr0.802834550901993
# Estimate0.725821749966079lwr0.656654001745095upr0.802275188034756
# Estimate0.725151046165725lwr0.656030110296182upr0.801554732781761
# Estimate0.726005755967506lwr0.656804314415771upr0.802498318189022
# Estimate0.725330026936213lwr0.656181986497057upr0.80176484390226

delta.eta <- glht(fit.linENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.676005520181751lwr0.597973276689949upr0.764220544847436
# Estimate0.675448588138962lwr0.597498092186433upr0.763568622536392
# Estimate0.67468441536767lwr0.596803058006753upr0.76272910172465
# Estimate0.675658264845879lwr0.597665567268317upr0.763828662476043
# Estimate0.674888323302786lwr0.596972239088855upr0.762973919232201

delta.eta <- glht(fit.linENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.604900327562475lwr0.516768208987353upr0.708062918580474
# Estimate0.604260600241159lwr0.516241055361061upr0.707287553386143
# Estimate0.603383066769776lwr0.515470222244004upr0.706289344279838
# Estimate0.604501430584444lwr0.516426832982003upr0.707596809926748
# Estimate0.603617195709924lwr0.515657830401424upr0.706580405601666

# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k1 <- matrix(c(2.68, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-3.360106), nrow=1)
k2 <- matrix(c(4.71, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-5.905261), nrow=1)
k3 <- matrix(c(6.52, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-8.174587), nrow=1)
k4 <- matrix(c(8.32, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-10.431375), nrow=1)
k5 <- matrix(c(10.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-12.813540), nrow=1)
k6 <- matrix(c(12.43, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-15.584374), nrow=1)
k7 <- matrix(c(15.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-19.082395), nrow=1)
k8 <- matrix(c(19.54, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-24.498685), nrow=1)

delta.eta <- glht(fit.linENMOimpute1, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.773941418497124lwr0.607050294676351upr0.986714485633665
# Estimate0.7738306752645lwr0.60697958525207upr0.986547041333578
# Estimate0.774349801317669lwr0.607351046861567upr0.987266948660391
# Estimate0.773752182304707lwr0.606916278280213upr0.986449797190776
# Estimate0.773422913859038lwr0.606626438739694upr0.986081326961234

delta.eta <- glht(fit.linENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.70413425537066lwr0.568957706040538upr0.871426899262471
# Estimate0.703945894534485lwr0.56881714487529upr0.871175960317792
# Estimate0.704232643191682lwr0.56902118390423upr0.871573202835651
# Estimate0.703912445309576lwr0.568791820380661upr0.871131955326117
# Estimate0.703495685965341lwr0.568427607845734upr0.870658239221481


delta.eta <- glht(fit.linENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.655480305511571lwr0.540629434362849upr0.794730001003193
# Estimate0.655243193404019lwr0.540442782573733upr0.794429412966989
# Estimate0.65537934888707lwr0.54053272984128upr0.794627350454362
# Estimate0.655238820111154lwr0.540442789454234upr0.794418798360182
# Estimate0.654768246751605lwr0.540029932159691upr0.793884619024033

delta.eta <- glht(fit.linENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.614941511574825lwr0.515537219868231upr0.733512631259844
# Estimate0.614667401882661lwr0.515314486852746upr0.733175613293294
# Estimate0.614685782070169lwr0.515311638296949upr0.73322351485779
# Estimate0.614685684105364lwr0.515334504774816upr0.733190746482582
# Estimate0.614175126348398lwr0.514883910712342upr0.732613853292092

delta.eta <- glht(fit.linENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.577113398257806lwr0.490539101912037upr0.678967024545982
# Estimate0.576807938306594lwr0.490285317714661upr0.678599553509646
# Estimate0.576723162364863lwr0.490198393632126upr0.678520391598301
# Estimate0.576845985118768lwr0.490322709142777upr0.678637322610216
# Estimate0.576302368354637lwr0.489840018820084upr0.678026308612304

delta.eta <- glht(fit.linENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.53970530779942lwr0.463895762885093upr0.627903599410579
# Estimate0.539372059767926lwr0.463614641415242upr0.627508695519665
# Estimate0.539192101620691lwr0.463447833228268upr0.627315744309331
# Estimate0.539428258120839lwr0.463667488158852upr0.627567929799712
# Estimate0.538856255121804lwr0.463157074003018upr0.626927839348952

delta.eta <- glht(fit.linENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.499233359476045lwr0.432376102447966upr0.576428590300575
# Estimate0.498873904951192lwr0.432070453118104upr0.57600599912631
# Estimate0.498599162083211lwr0.431822345705868upr0.575702315783844
# Estimate0.498948069022072lwr0.432137440362505upr0.576087957970084
# Estimate0.498350521848718lwr0.431603145283823upr0.575420372489575

delta.eta <- glht(fit.linENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.452445200239754lwr0.391855208366057upr0.522403823783712
# Estimate0.452060865378182lwr0.391529614394233upr0.521950367209518
# Estimate0.451688044978909lwr0.391197218942302upr0.521532567456622
# Estimate0.452153450888631lwr0.391608916930968upr0.522058447373242
# Estimate0.451533626436989lwr0.391057571169367upr0.521362149296008

delta.eta <- glht(fit.linENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.388500533590376lwr0.329742455857309upr0.457728939416042
# Estimate0.388092660390042lwr0.329406708383821upr0.45723389723176
# Estimate0.387608009416588lwr0.328984293634045upr0.456678242308471
# Estimate0.38820588545735lwr0.329495965058746upr0.457376798155499
# Estimate0.387569627568594lwr0.328944048216424upr0.456643666386782


# 10th
k0 <- matrix(c(0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k1 <- matrix(c(2.68, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-6.720749), nrow=1)
k2 <- matrix(c(4.71, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-11.811465), nrow=1)
k3 <- matrix(c(6.52, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-16.350478), nrow=1)
k4 <- matrix(c(8.32, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-20.864413), nrow=1)
k5 <- matrix(c(10.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-25.629123), nrow=1)
k6 <- matrix(c(12.43, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-31.171233), nrow=1)
k7 <- matrix(c(15.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-38.167833), nrow=1)
k8 <- matrix(c(19.54, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-49.001279), nrow=1)

delta.eta <- glht(fit.linENMOimpute1, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.598960834363624lwr0.368480719640996upr0.973603398981354
# Estimate0.598789422416517lwr0.36839487627647upr0.973272961942133
# Estimate0.599593154508513lwr0.368845953561356upr0.974694035442813
# Estimate0.598667943338477lwr0.368318028197015upr0.973081084669065
# Estimate0.598158487630283lwr0.36796629551237upr0.972354209305888

delta.eta <- glht(fit.linENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.531171145957813lwr0.344823064287268upr0.8182248101104
# Estimate0.530964041746728lwr0.344703902287432upr0.817869515712472
# Estimate0.53150268412525lwr0.345019013685759upr0.818781261399235
# Estimate0.530884559914289lwr0.344652506506745upr0.817746601677107
# Estimate0.530362550546035lwr0.344281416548609upr0.817018931319462

delta.eta <- glht(fit.linENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.484978216379916lwr0.327492188020008upr0.718196888252721
# Estimate0.484751024888509lwr0.327351209196412upr0.717833169785144
# Estimate0.485122458515675lwr0.327573477412235upr0.718445832719613
# Estimate0.484697988803758lwr0.327317034245136upr0.71775103575716
# Estimate0.484172826758629lwr0.326934383444091upr0.717034787537207

delta.eta <- glht(fit.linENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.44719002006385lwr0.312374157810849upr0.640190326389928
# Estimate0.446949210720127lwr0.312216165973995upr0.639824643096741
# Estimate0.447192781858318lwr0.312362636419673upr0.640221847396298
# Estimate0.446916364941738lwr0.312195888112528upr0.639772158628638
# Estimate0.446392214221591lwr0.311805169820214upr0.639072177772264

delta.eta <- glht(fit.linENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.412531026294012lwr0.297558766841463upr0.571926848137069
# Estimate0.412280149682351lwr0.297386143764401upr0.571563017935232
# Estimate0.412414125369675lwr0.297463505333501upr0.571785808190967
# Estimate0.412264579537625lwr0.297378329812044upr0.571534864859715
# Estimate0.411744446856631lwr0.296981723210129upr0.570854959304413

delta.eta <- glht(fit.linENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.378856474006527lwr0.282035127441144upr0.508916138209101
# Estimate0.378598226701567lwr0.28184959962598upr0.50855710794616
# Estimate0.378633357538559lwr0.28186059271651upr0.508631653893932
# Estimate0.378598205106968lwr0.28185344933901upr0.508550103773306
# Estimate0.378085050868659lwr0.281453043876814upr0.507893976634062

delta.eta <- glht(fit.linENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.343130180955964lwr0.263919574186205upr0.446114394682237
# Estimate0.342866950095374lwr0.263722503037564upr0.445763043023139
# Estimate0.342806196909885lwr0.263664646099711upr0.445702866797613
# Estimate0.342881969776372lwr0.26373790292249upr0.445776067432662
# Estimate0.342379836159183lwr0.263336462550542upr0.44514895914154

delta.eta <- glht(fit.linENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.302798635742878lwr0.240530449474504upr0.381186722961937
# Estimate0.302533665873753lwr0.240324643630402upr0.380845749334685
# Estimate0.302376975627385lwr0.240192744967779upr0.380660270991239
# Estimate0.302563672279599lwr0.240351288832593upr0.380879071745183
# Estimate0.302078936295096lwr0.239954431296064upr0.380287554017235

delta.eta <- glht(fit.linENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.249498961363271lwr0.202266014762567upr0.307761695875721
# Estimate0.249238944925031lwr0.202060791951263upr0.30743248636938
# Estimate0.248978418663387lwr0.201844181763355upr0.307119345321526
# Estimate0.249285038082077lwr0.202097227540355upr0.307490760600236
# Estimate0.248832539177109lwr0.201722380650418upr0.306944783983239




# -------
# Repeat MICE analysis for MVPA
# -------

# Change MVPA Minutes from 10th to 90th percentile w/ genetic risk at 90th
41.76 - 31.68 # 10.08
51.84 - 31.68 # 20.16
59.04 - 31.68 # 27.36
67.68 - 31.68 # 36
76.32 - 31.68 # 44.64
87.84 - 31.68 # 56.16
100.80 - 31.68 # 69.12
120.96 - 31.68 # 89.28


# 90th
k1 <- matrix(c(0, 10.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k2 <- matrix(c(0, 20.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k3 <- matrix(c(0, 27.36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k4 <- matrix(c(0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k5 <- matrix(c(0, 44.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k6 <- matrix(c(0, 56.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k7 <- matrix(c(0, 69.12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k8 <- matrix(c(0, 89.28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.905391693831415lwr0.868826865375419upr0.94349536360701
# Estimate0.905435644210159lwr0.868871354068638upr0.943538651570624
# Estimate0.905298715751391lwr0.868732152817852upr0.943404433786345
# Estimate0.905361622216274lwr0.868800620063705upr0.943461190119753
# Estimate0.905428278740439lwr0.868850625985583upr0.943545810320309

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.81973411925892lwr0.754860121998076upr0.890183501147925
# Estimate0.819813705806265lwr0.754937429921068upr0.890265187007712
# Estimate0.819565764741118lwr0.754695553339539upr0.890011925687735
# Estimate0.819679666982082lwr0.754814517423079upr0.890119017262181
# Estimate0.819800367942874lwr0.754901410275539upr0.890278696173008

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.763557846830697lwr0.68272745797082upr0.853958015969601
# Estimate0.763658456956413lwr0.682822351893485upr0.854064365737138
# Estimate0.763345031287091lwr0.682525464806553upr0.853734647037444
# Estimate0.763489012453044lwr0.682671480898643upr0.853874064534228
# Estimate0.763641595520374lwr0.682778138070268upr0.854081954142603

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.701204450649335lwr0.605207684558293upr0.812428021249086
# Estimate0.701326024410795lwr0.605318370229275upr0.812561152455279
# Estimate0.700947308447769lwr0.604972092929654upr0.812148419674797
# Estimate0.701121276506224lwr0.60514239435381upr0.812322932513482
# Estimate0.70130564928765lwr0.605266797969348upr0.81258317055032

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.643942935890562lwr0.536489835251456upr0.772917728234313
# Estimate0.644081379621157lwr0.536611504179615upr0.77307478565692
# Estimate0.643650130782585lwr0.536230883821189upr0.772587897034641
# Estimate0.643848223552464lwr0.536418068852407upr0.772793757411111
# Estimate0.644058176778102lwr0.536554813778125upr0.773100761418691

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.574801212773467lwr0.456847146599846upr0.723210020386192
# Estimate0.574956687050589lwr0.456977494704898upr0.723394906345801
# Estimate0.574472416891225lwr0.456569748428524upr0.722821778939017
# Estimate0.574694854440674lwr0.456770264366418upr0.723064090388433
# Estimate0.574930629326432lwr0.456916759380021upr0.723425485609668

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.505850000556561lwr0.381290963637457upr0.671099625918161
# Estimate0.50601840471392lwr0.381424863843372upr0.671310787999302
# Estimate0.505493895279169lwr0.381006035468566upr0.670656247873504
# Estimate0.505734803082196lwr0.381211990428719upr0.670932965043794
# Estimate0.505990179189134lwr0.381362472330524upr0.671345714409875

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.414662504683356lwr0.287821343328135upr0.597401814618891
# Estimate0.414840823574693lwr0.287951906417909upr0.597644624218493
# Estimate0.414285490856439lwr0.287543560763653upr0.596892058644409
# Estimate0.414540534971663lwr0.287744344591345upr0.597210191493583
# Estimate0.414810935074733lwr0.287891068188479upr0.59768478730616

# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, 0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -12.63801), nrow=1)
k2 <- matrix(c(0, 20.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -25.27602), nrow=1)
k3 <- matrix(c(0, 27.36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -34.30317), nrow=1)
k4 <- matrix(c(0, 36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -45.13576), nrow=1)
k5 <- matrix(c(0, 44.64, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -55.96834), nrow=1)
k6 <- matrix(c(0, 56.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -70.41178), nrow=1)
k7 <- matrix(c(0, 69.12, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -86.66065), nrow=1)
k8 <- matrix(c(0, 89.28, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -111.93667), nrow=1)

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.685473795986456lwr0.592223489711736upr0.793407105842412
# Estimate0.685368440611597lwr0.592142644360629upr0.793271526480866
# Estimate0.685621774005792lwr0.592343115895838upr0.793589398401166
# Estimate0.685305766173519lwr0.592088277332409upr0.793199276409601
# Estimate0.685186039751572lwr0.59197552365236upr0.793073176698006

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.608254398548203lwr0.534736925061012upr0.691879307401528
# Estimate0.608209150215837lwr0.534702609074005upr0.691820769393464
# Estimate0.608310676138151lwr0.534779774636037upr0.691951895442381
# Estimate0.608102146266415lwr0.534609182475625upr0.691698220710381
# Estimate0.608049671825967lwr0.53455866917648upr0.691644200583724

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.539733853459434lwr0.474233132189202upr0.61428148477392
# Estimate0.539736510301188lwr0.474237542433293upr0.614281819734004
# Estimate0.539717221262767lwr0.474211175606791upr0.614272066774613
# Estimate0.539595956354744lwr0.474115254493578upr0.61412028690238
# Estimate0.539597104958117lwr0.474110309075274upr0.614129307264132

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.495569113420804lwr0.430426675475635upr0.570570459893776
# Estimate0.495599629814059lwr0.430454110470823upr0.570604361991523
# Estimate0.495510187634142lwr0.430366917737793upr0.570513986855316
# Estimate0.495440666151298lwr0.430317365681803upr0.570419586222201
# Estimate0.495473014030228lwr0.430335867262672upr0.570469547876082

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.447315256057881lwr0.379615617275168upr0.527088268228675
# Estimate0.447373214657841lwr0.379665478294927upr0.527155626822142
# Estimate0.447214787826792lwr0.379520313451473upr0.526983825008185
# Estimate0.44719732944276lwr0.379517447885053upr0.526946659699576
# Estimate0.447260425136356lwr0.379556691550998upr0.527040867269959

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.403759911726403lwr0.332571214035323upr0.490186941735727
# Estimate0.403839680936245lwr0.332637959649547upr0.490282251822699
# Estimate0.403626554268659lwr0.332450241904315upr0.49004144011927
# Estimate0.403651674770954lwr0.332484145998276upr0.490052462670629
# Estimate0.403739225766142lwr0.332538159269946upr0.490185435500412

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.352210672788553lwr0.277106809385253upr0.447669829194631
# Estimate0.352312192061505lwr0.2771884595387upr0.447795990070261
# Estimate0.352044715825281lwr0.276964321753514upr0.447478148650496
# Estimate0.352114169427337lwr0.277033435693128upr0.447543048373562
# Estimate0.352226135041248lwr0.277100193143645upr0.447719825809657

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.302041585624074lwr0.224661743558847upr0.406073228139122
# Estimate0.302159456289704lwr0.224751911627566upr0.406227187854056
# Estimate0.301851398269512lwr0.224508189084547upr0.405839390575403
# Estimate0.301956816425509lwr0.224601703772869upr0.405953817153736
# Estimate0.302087174885179lwr0.22467574918663upr0.406170499310563

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.237823925390577lwr0.16133193037164upr0.350582921544993
# Estimate0.237954479413696lwr0.161423601634458upr0.35076862181073
# Estimate0.237615554355083lwr0.161179180131152upr0.350300526566339
# Estimate0.237754715018262lwr0.161288331822887upr0.35047361377318
# Estimate0.237899425201541lwr0.161363883996457upr0.350736082384247


# 10th
k0 <- matrix(c(0, 0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0, -25.27804), nrow=1)
k2 <- matrix(c(0, 20.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0, -50.55608), nrow=1)
k3 <- matrix(c(0, 27.36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0, -68.61182), nrow=1)
k4 <- matrix(c(0, 36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0, -90.27871), nrow=1)
k5 <- matrix(c(0, 44.64, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0, -111.94560), nrow=1)
k6 <- matrix(c(0, 56.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0, -140.83479), nrow=1)
k7 <- matrix(c(0, 69.12, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0, -173.33513), nrow=1)
k8 <- matrix(c(0, 89.28, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0, -223.89121), nrow=1)

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.469846019919124lwr0.350699353548567upr0.629471597823377
# Estimate0.46970159150473lwr0.350603603418574upr0.629256467734264
# Estimate0.470048915888808lwr0.350841058193644upr0.629760907876108
# Estimate0.469615683595311lwr0.350539220549878upr0.629141840199051
# Estimate0.469451596321694lwr0.350405713514602upr0.628941803141583

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.408607609864073lwr0.317219237337528upr0.526324255237969
# Estimate0.40852697998829lwr0.317164322578505upr0.526207651672558
# Estimate0.408725210911456lwr0.317303796981236upr0.52648691766047
# Estimate0.408416629137203lwr0.317078982194036upr0.526064962747107
# Estimate0.408316073523196lwr0.316995445149236upr0.525944515760818

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.355350842106889lwr0.283549697356016upr0.445333647552899
# Estimate0.355319837950074lwr0.283528901670865upr0.445288598435115
# Estimate0.355401943048268lwr0.283585288445561upr0.445405831222207
# Estimate0.355192871070163lwr0.283428353873145upr0.44512827998688
# Estimate0.355142078978368lwr0.283385077297109upr0.445068941046756

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.321615198590051lwr0.259141423283998upr0.39915014224013
# Estimate0.321612428135319lwr0.259141078448254upr0.399143796693543
# Estimate0.321628370707518lwr0.259147060375727upr0.399174154991348
# Estimate0.321477472764603lwr0.259033265639269upr0.398974877763534
# Estimate0.321455203756705lwr0.259012617144904upr0.39895140692878

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.285332749122526lwr0.229676016015384upr0.354476619432326
# Estimate0.285357218191469lwr0.229696051509902upr0.354506494294105
# Estimate0.285309224132184lwr0.229651599979165upr0.354455851308215
# Estimate0.285216147892501lwr0.229583429185814upr0.354329802055521
# Estimate0.285221626816294lwr0.229583475205681upr0.354343344314532

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.253143439982742lwr0.200816489306242upr0.319105275805179
# Estimate0.253189040131597lwr0.200852448760119upr0.319163099272545
# Estimate0.253091333938738lwr0.200768821182402upr0.319049655905956
# Estimate0.25304494999002lwr0.200738912379617upr0.318980241331395
# Estimate0.253072202450033lwr0.200753644698107upr0.319025538735409

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.215800982412954lwr0.165063954747801upr0.282133455978016
# Estimate0.21586701505706lwr0.165114499078217upr0.282219722978843
# Estimate0.215721064762881lwr0.164995534481101upr0.282041437841226
# Estimate0.215722655746759lwr0.165004537135973upr0.282030209654696
# Estimate0.215771343224316lwr0.165031883329434upr0.28211077530933

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.180333328621599lwr0.130361098310457upr0.249461763004625
# Estimate0.180414044408879lwr0.130420194167967upr0.249571990193858
# Estimate0.180233179774406lwr0.130280959342035upr0.249338040306496
# Estimate0.180273172443107lwr0.130318506981261upr0.249376834154302
# Estimate0.180337792748249lwr0.130353428717007upr0.249488792227431

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate0.136388513404145lwr0.088747551279838upr0.209603829292569
# Estimate0.136479607867445lwr0.0888084215542801upr0.20974005660338
# Estimate0.136273524155397lwr0.0886653494724769upr0.209444540581167
# Estimate0.136349248829971lwr0.0887234607872042upr0.209540041512656
# Estimate0.136426287900182lwr0.0887614567132683upr0.209687095270949


# --------
# Adding potential mediators - BMI, sleep duration, medication use
# --------

fit.linENMOMeds <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + BMI_InstChosen + SleepDur_InstChosen + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + OilyFish_InstChosen + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)

summary(fit.linENMOMeds)


fit.linENMOMVPAMeds <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + BMI_InstChosen + SleepDur_InstChosen + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + OilyFish_InstChosen + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)

summary(fit.linENMOMVPAMeds)






# 90th
k1 <- matrix(c(2.68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k2 <- matrix(c(4.71, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k3 <- matrix(c(6.52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k4 <- matrix(c(8.32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k5 <- matrix(c(10.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k6 <- matrix(c(12.43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k7 <- matrix(c(15.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k8 <- matrix(c(19.54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)

# ACTUAL RESULTS
Estimate0.956415790832258lwr0.935194401669757upr0.97811873480003
Estimate0.924671269759477lwr0.88891691856101upr0.961863745942328
Estimate0.897256660134058lwr0.849589421036567upr0.947598327169229
Estimate0.870799620420488lwr0.812204880647162upr0.933621549183823
Estimate0.843718622223789lwr0.774526610985609upr0.919091872881351
Estimate0.813276858746683lwr0.732894170635542upr0.902475794560231
Estimate0.776409431707972lwr0.683516244749944upr0.881927255241208
Estimate0.722594246701464lwr0.613542198226309upr0.851029394352205

delta.eta <- glht(fit.linENMOMeds, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMeds, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOMeds, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOMeds, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOMeds, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOMeds, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOMeds, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOMeds, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825

# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k1 <- matrix(c(2.68, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-3.360106), nrow=1)
k2 <- matrix(c(4.71, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-5.905261), nrow=1)
k3 <- matrix(c(6.52, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-8.174587), nrow=1)
k4 <- matrix(c(8.32, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-10.431375), nrow=1)
k5 <- matrix(c(10.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-12.813540), nrow=1)
k6 <- matrix(c(12.43, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-15.584374), nrow=1)
k7 <- matrix(c(15.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-19.082395), nrow=1)
k8 <- matrix(c(19.54, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-24.498685), nrow=1)

# ACTUAL RESULTS
# Estimate0.774508899736013lwr0.607230481277978upr0.987868781731467
# Estimate0.72348472085496lwr0.584140665844048upr0.896068655919126
# Estimate0.687085373437648lwr0.566023491944934upr0.83404013633745
# Estimate0.656177976281683lwr0.549173920312473upr0.784031288871358
# Estimate0.626820272329716lwr0.531537500802093upr0.739183318600487
# Estimate0.597255352534066lwr0.51172682073638upr0.697078874265916
# Estimate0.564616451198452lwr0.486909710600212upr0.654724541375362
# Estimate0.525946627194968lwr0.452946762185359upr0.610711628278649
# Estimate0.471230074201004lwr0.397096888001985upr0.559203029640396

delta.eta <- glht(fit.linENMOMeds, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMeds, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOMeds, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOMeds, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOMeds, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOMeds, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOMeds, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOMeds, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825


# 10th
k0 <- matrix(c(0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k1 <- matrix(c(2.68, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-6.720749), nrow=1)
k2 <- matrix(c(4.71, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-11.811465), nrow=1)
k3 <- matrix(c(6.52, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-16.350478), nrow=1)
k4 <- matrix(c(8.32, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-20.864413), nrow=1)
k5 <- matrix(c(10.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-25.629123), nrow=1)
k6 <- matrix(c(12.43, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-31.171233), nrow=1)
k7 <- matrix(c(15.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-38.167833), nrow=1)
k8 <- matrix(c(19.54, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,-49.001279), nrow=1)

# ACTUAL RESULTS
# Estimate0.599839585083422lwr0.36869951680181upr0.975882829883019
# Estimate0.547258669364494lwr0.354825329494937upr0.844054880808247
# Estimate0.510520828866012lwr0.344184915045766upr0.757242706791446
# Estimate0.479849372364566lwr0.334509788770791upr0.688336867524199
# Estimate0.451175038652286lwr0.324611377184903upr0.627084969319919
# Estimate0.422764522917145lwr0.313723757067396upr0.56970451810242
# Estimate0.391961456531621lwr0.300244252007435upr0.511696002102268
# Estimate0.356258772251997lwr0.281446167537834upr0.450957687279358
# Estimate0.307285367527128lwr0.247235485658459upr0.381920487040132

delta.eta <- glht(fit.linENMOMeds, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.592085878507679
# Lower = 0.349814880944463
# Upper = 1.00214629686913


delta.eta <- glht(fit.linENMOMeds, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMeds, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOMeds, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOMeds, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOMeds, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOMeds, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOMeds, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOMeds, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825





# -------
# Repeat mediators analysis for MVPA
# -------

# Change MVPA Minutes from 10th to 90th percentile w/ genetic risk at 90th
41.76 - 31.68 # 10.08
51.84 - 31.68 # 20.16
59.04 - 31.68 # 27.36
67.68 - 31.68 # 36
76.32 - 31.68 # 44.64
87.84 - 31.68 # 56.16
100.80 - 31.68 # 69.12
120.96 - 31.68 # 89.28


# 90th
k1 <- matrix(c(0, 10.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k2 <- matrix(c(0, 20.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k3 <- matrix(c(0, 27.36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k4 <- matrix(c(0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k5 <- matrix(c(0, 44.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k6 <- matrix(c(0, 56.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k7 <- matrix(c(0, 69.12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)
k8 <- matrix(c(0, 89.28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0), nrow=1)

# ACTUAL RESULTS:
# Estimate0.916562399799661lwr0.87929910282081upr0.955404856017137
# Estimate0.840086632726514lwr0.773166912221482upr0.912798438901126
# Estimate0.789399701918203lwr0.705295077797437upr0.883533586161675
# Estimate0.732595789880835lwr0.631666673674344upr0.849651586380539
# Estimate0.679879394490494lwr0.565724615400484upr0.817068903260534
# Estimate0.615443930270611lwr0.488383771638007upr0.775560641658021
# Estimate0.550224194798299lwr0.4139395199291upr0.73137898162826
# Estimate0.462235991052761lwr0.320044340470025upr0.667601592675372

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.903177500655002
# Lower = 0.864233046442377
# Upper = 0.943876887197701

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.815729597689417
# Lower = 0.746898758563072
# Upper = 0.890903578186022

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.75849999949686
# Lower = 0.672973669374088
# Upper = 0.854895630273061

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.695099253580659
# Lower = 0.593856718652059
# Upper = 0.813601929814108

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.636997986353182
# Lower = 0.524041011316525
# Upper = 0.77430282335846

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.567013034477565
# Lower = 0.443550845925562
# Upper = 0.724840870490441

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.4974276227899
# Lower = 0.367678990483228
# Upper = 0.672962682989355

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.405766434618008
# Lower = 0.274618981541647
# Upper = 0.599544862260882

# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, 0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -12.63801), nrow=1)
k2 <- matrix(c(0, 20.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -25.27602), nrow=1)
k3 <- matrix(c(0, 27.36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -34.30317), nrow=1)
k4 <- matrix(c(0, 36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -45.13576), nrow=1)
k5 <- matrix(c(0, 44.64, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -55.96834), nrow=1)
k6 <- matrix(c(0, 56.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -70.41178), nrow=1)
k7 <- matrix(c(0, 69.12, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -86.66065), nrow=1)
k8 <- matrix(c(0, 89.28, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -111.93667), nrow=1)

# ACTUAL RESULTS:
# Estimate0.690191635012387lwr0.595961284910974upr0.799321206095179
# Estimate0.621229159642078lwr0.545730418052095upr0.707172728555443
# Estimate0.559157267651724lwr0.490786117037571upr0.637053166570739
# Estimate0.518654751201582lwr0.449913950542729upr0.597898221692121
# Estimate0.473905357463324lwr0.401589414321147upr0.559243545331206
# Estimate0.433016935710075lwr0.3560742286056upr0.526585895716224
# Estimate0.383933405393924lwr0.301485322123466upr0.488928809996955
# Estimate0.335332615976089lwr0.24887598920284upr0.451823270286308
# Estimate0.271668997118937lwr0.18380590002205upr0.401532507861581

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.903177500655002
# Lower = 0.864233046442377
# Upper = 0.943876887197701


delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.903177500655002
# Lower = 0.864233046442377
# Upper = 0.943876887197701

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.815729597689417
# Lower = 0.746898758563072
# Upper = 0.890903578186022

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.75849999949686
# Lower = 0.672973669374088
# Upper = 0.854895630273061

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.695099253580659
# Lower = 0.593856718652059
# Upper = 0.813601929814108

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.636997986353182
# Lower = 0.524041011316525
# Upper = 0.77430282335846

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.567013034477565
# Lower = 0.443550845925562
# Upper = 0.724840870490441

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.4974276227899
# Lower = 0.367678990483228
# Upper = 0.672962682989355

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.405766434618008
# Lower = 0.274618981541647
# Upper = 0.599544862260882


# 10th
k0 <- matrix(c(0, 0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -25.27804), nrow=1)
k2 <- matrix(c(0, 20.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -50.55608), nrow=1)
k3 <- matrix(c(0, 27.36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -68.61182), nrow=1)
k4 <- matrix(c(0, 36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -90.27871), nrow=1)
k5 <- matrix(c(0, 44.64, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -111.94560), nrow=1)
k6 <- matrix(c(0, 56.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -140.83479), nrow=1)
k7 <- matrix(c(0, 69.12, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -173.33513), nrow=1)
k8 <- matrix(c(0, 89.28, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0, -223.89121), nrow=1)

# ACTUAL RESULTS:
# Estimate0.476336318190043lwr0.355140530202564upr0.638891561876728
# Estimate0.421031589267092lwr0.32639912175678upr0.543100723453743
# Estimate0.372147981145644lwr0.296419224730261upr0.467223811130359
# Estimate0.340745915809893lwr0.273986478041171upr0.423771931999052
# Estimate0.306541055106927lwr0.246167284916349upr0.381721797427305
# Estimate0.27576975718909lwr0.218213224351554upr0.348507562757106
# Estimate0.239491784354736lwr0.182703747553548upr0.313930696777881
# Estimate0.204351391761407lwr0.147330159122879upr0.283441567995559
# Estimate0.159653914648535lwr0.103603847133059upr0.246027277634444

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.903177500655002
# Lower = 0.864233046442377
# Upper = 0.943876887197701


delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.903177500655002
# Lower = 0.864233046442377
# Upper = 0.943876887197701

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.815729597689417
# Lower = 0.746898758563072
# Upper = 0.890903578186022

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.75849999949686
# Lower = 0.672973669374088
# Upper = 0.854895630273061

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.695099253580659
# Lower = 0.593856718652059
# Upper = 0.813601929814108

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.636997986353182
# Lower = 0.524041011316525
# Upper = 0.77430282335846

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.567013034477565
# Lower = 0.443550845925562
# Upper = 0.724840870490441

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.4974276227899
# Lower = 0.367678990483228
# Upper = 0.672962682989355

delta.eta <- glht(fit.linENMOMVPAMeds, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.405766434618008
# Lower = 0.274618981541647
# Upper = 0.599544862260882



# ---------
# Repeat mediation analyses but also w/ - phys active occ.
# Imputed to keep missingness down...
# ---------


# mice library refuses to load, so switched to Amelia
install.packages("Amelia")
library(Amelia)

# Getting colnames
otherotherotherdata <- otherotherotherdata[ , c(2,7,11)]

data <- merge(data, otherotherotherdata, by = "eid", all = F)

# Creating ManLabor_Inst0 and WalkorStandWork_Inst0
data$ManLabor_Inst0 <- data$p816_i0
data$WalkorStandWork_Inst0 <- data$p806_i0


datasub <- data[ , c("AgeBaseline", "TimeYear", "Status", "p90012", "StandPGS", "SeasonWear", "Salt_InstChosen", "AlcIntake_InstChosen", "OilyFish_InstChosen", "FnVScore", "ProcMeat_InstChosen", "ParentHist", "MobilityDichot", "NewEmploy", "Townsend", "NewEduc", "SmokStat_InstChosen", "Biological.Sex", "REGION", "p22009_a1",
                     "p22009_a2", "p22009_a3", "p22009_a4", "p22009_a5", "p22009_a6", "p22009_a7", "p22009_a8", "p22009_a9", "p22009_a10", "Meds", "BMI_InstChosen", "SleepDur_InstChosen", "MVPAMins", "ManLabor_Inst0", "WalkorStandWork_Inst0")]

# Converting characters to factors
datasub$SeasonWear <- as.factor(datasub$SeasonWear)
datasub$NewEduc <- as.factor(datasub$NewEduc)
datasub$SmokStat_InstChosen <- as.factor(datasub$SmokStat_InstChosen)
datasub$Biological.Sex <- as.factor(datasub$Biological.Sex)
datasub$REGION <- as.factor(datasub$REGION)
datasub$NewEduc <- as.factor(datasub$NewEduc)
datasub$Salt_InstChosen <- as.factor(datasub$Salt_InstChosen)
datasub$OilyFish_InstChosen <- as.factor(datasub$OilyFish_InstChosen)
datasub$AlcIntake_InstChosen <- as.factor(datasub$AlcIntake_InstChosen)
datasub$WalkorStandWork_Inst0 <- as.factor(datasub$WalkorStandWork_Inst0)
datasub$FnVScore <- as.factor(datasub$FnVScore)
datasub$ProcMeat_InstChosen <- as.factor(datasub$ProcMeat_InstChosen)

# JUST doing WalkorStandWork - collinear w/ manual labor

# Setting factor variables as 'nominal'
imputed_Data <- amelia(datasub, noms = c(6,16,17,18,19,7,8,9,34,10,11))

# Have 5 imputed datasets - taking average result
dataimpute1 <- imputed_Data$imputations$imp1
dataimpute2 <- imputed_Data$imputations$imp2
dataimpute3 <- imputed_Data$imputations$imp3
dataimpute4 <- imputed_Data$imputations$imp4
dataimpute5 <- imputed_Data$imputations$imp5


# ENMO
fit.linENMOimpute1 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore) + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute1)
fit.linENMOimpute2 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore) + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute2)
fit.linENMOimpute3 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore) + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute3)
fit.linENMOimpute4 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore) + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute4)
fit.linENMOimpute5 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore )+ ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute5)


# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMOimpute1 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore) + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute1)
fit.linMVPAMinsENMOimpute2 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore) + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute2)
fit.linMVPAMinsENMOimpute3 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore) + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute3)
fit.linMVPAMinsENMOimpute4 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore) + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute4)
fit.linMVPAMinsENMOimpute5 <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + as.factor(OilyFish_InstChosen) + as.numeric(FnVScore) + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + BMI_InstChosen + as.numeric(SleepDur_InstChosen) + Meds + WalkorStandWork_Inst0, data = dataimpute5)





# 90th
k1 <- matrix(c(2.68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k2 <- matrix(c(4.71, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k3 <- matrix(c(6.52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k4 <- matrix(c(8.32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k5 <- matrix(c(10.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k6 <- matrix(c(12.43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k7 <- matrix(c(15.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k8 <- matrix(c(19.54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)

delta.eta <- glht(fit.linENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.953330386561302lwr0.932294507260338upr0.974840910102384
#Estimate0.953443580688164lwr0.932405794513324upr0.97495604049732
#Estimate0.953601475980729lwr0.932560681604005upr0.975117000888921
#Estimate0.95379553360031lwr0.932751069365911upr0.975314797049053
#Estimate0.953409066594425lwr0.932374208048125upr0.974918482748866

delta.eta <- glht(fit.linENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.919435172127889lwr0.884078347612222upr0.956206017293659
#Estimate0.919627042188742lwr0.884263824071854upr0.956404495697307
#Estimate0.919894711922826lwr0.884521993938975upr0.956682012230394
#Estimate0.92022373131109lwr0.884839381757941upr0.957023085913869
#Estimate0.919568537137022lwr0.884211178910325upr0.956339746274666

delta.eta <- glht(fit.linENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.890230947059094lwr0.84319447686315upr0.939891283503218
#Estimate0.890488124511024lwr0.843439366265251upr0.940161357901076
#Estimate0.89084693624829lwr0.843780267528145upr0.940539017519148
#Estimate0.891288042274547lwr0.844199415734243upr0.941003226839086
#Estimate0.890409703733128lwr0.843369855480408upr0.940073249417359

delta.eta <- glht(fit.linENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.862108072578854lwr0.804411665068863upr0.923942753542741
#Estimate0.862425895313939lwr0.804709800600071upr0.924281553863783
#Estimate0.862869361162455lwr0.805124863571751upr0.924755361708627
#Estimate0.863414604488472lwr0.805635259253074upr0.925337825873142
#Estimate0.862328979364848lwr0.804625173705315upr0.924171021431104

delta.eta <- glht(fit.linENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.833386070552691lwr0.765407809641698upr0.907401693374905
#Estimate0.833763482231435lwr0.76575628671693upr0.907810430500145
#Estimate0.834290147071606lwr0.766241484060396upr0.908382101439315
#Estimate0.834937768668192lwr0.766838201062924upr0.909084962880476
#Estimate0.833648392035008lwr0.765657367187865upr0.90767707766603

delta.eta <- glht(fit.linENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.801179478957246lwr0.722413042986082upr0.888534009365291
#Estimate0.801620785543848lwr0.722813087676528upr0.889020819865827
#Estimate0.802236685699877lwr0.723370150056579upr0.889701765869638
#Estimate0.802994152147946lwr0.7240553544311upr0.890539106489209
#Estimate0.801486206269546lwr0.722699526039735upr0.888861989934433

delta.eta <- glht(fit.linENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.76229189889525lwr0.671566492108988upr0.865273872281021
#Estimate0.762806063602788lwr0.672021881029963upr0.865854382267112
#Estimate0.763523752857222lwr0.672656104507835upr0.866666513944328
#Estimate0.764406574486852lwr0.67343636983751upr0.867665361257086
#Estimate0.76264925904222lwr0.671892602943354upr0.865664973493812

delta.eta <- glht(fit.linENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.705769623750387lwr0.599805542998366upr0.830453748924624
#Estimate0.70638084070378lwr0.60032776553396upr0.831169105879299
#Estimate0.707234194446417lwr0.60105523603749upr0.832170116496704
#Estimate0.708284207768604lwr0.601950487383458upr0.83340163267419
#Estimate0.706194425770064lwr0.600179504173313upr0.830935684276047

# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k1 <- matrix(c(2.68, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-3.360106), nrow=1)
k2 <- matrix(c(4.71, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-5.905261), nrow=1)
k3 <- matrix(c(6.52, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-8.174587), nrow=1)
k4 <- matrix(c(8.32, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-10.431375), nrow=1)
k5 <- matrix(c(10.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-12.813540), nrow=1)
k6 <- matrix(c(12.43, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-15.584374), nrow=1)
k7 <- matrix(c(15.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-19.082395), nrow=1)
k8 <- matrix(c(19.54, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-24.498685), nrow=1)

delta.eta <- glht(fit.linENMOimpute1, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.796712303144321lwr0.626520322845196upr1.0131363195035
#Estimate0.797088858817612lwr0.626817753141236upr1.01361304089931
#Estimate0.797297383961945lwr0.626996013748654upr1.01385511954369
#Estimate0.796887805186632lwr0.626695897120659upr1.01329875777518
#Estimate0.797173385204004lwr0.626910734618498upr1.01367765933109

delta.eta <- glht(fit.linENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.739613941568266lwr0.598839097193385upr0.913482077449416
#Estimate0.740006353831566lwr0.59915704505702upr0.913966393667247
#Estimate0.740318159630178lwr0.599418939500522upr0.914337104421333
#Estimate0.740105773357518lwr0.599267569648085upr0.914043381454455
#Estimate0.740070476706318lwr0.599230263062946upr0.914013100227522

delta.eta <- glht(fit.linENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.699103931333989lwr0.577447231370259upr0.846391289550153
#Estimate0.699505526814361lwr0.57777839184066upr0.846878299628031
#Estimate0.699884921171567lwr0.578097636916412upr0.847329017804233
#Estimate0.699804406189261lwr0.578050665600167upr0.847202911553507
#Estimate0.69955586551495lwr0.577837455940059upr0.846913615491095

delta.eta <- glht(fit.linENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.664859069714237lwr0.557825282916103upr0.792430167866312
#Estimate0.665267006563665lwr0.558166364781038upr0.792918058034183
#Estimate0.665699624040758lwr0.558532165106803upr0.793429666424432
#Estimate0.665725057653267lwr0.558572017207902upr0.793433682200534
#Estimate0.665306168691912lwr0.558213421508984upr0.792944563932143

delta.eta <- glht(fit.linENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.632467131795471lwr0.537572639272743upr0.744112783237538
#Estimate0.632879803943078lwr0.537921674042632upr0.744600683643925
#Estimate0.633359288394234lwr0.538329247800037upr0.74516476642238
#Estimate0.633480024801493lwr0.538449227234821upr0.745282789026071
#Estimate0.632908816942148lwr0.53795742409562upr0.744619467305482

delta.eta <- glht(fit.linENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.599986161595554lwr0.515161281010011upr0.698778047527937
#Estimate0.600402284666615lwr0.515516434365384upr0.699265589615307
#Estimate0.600925193244533lwr0.515962915995285upr0.699877988671726
#Estimate0.601136445213331lwr0.516160212815192upr0.700102442597807
#Estimate0.600421554878629lwr0.515541068665159upr0.699277061469214

delta.eta <- glht(fit.linENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.564298277704644lwr0.487549625707481upr0.653128480528216
#Estimate0.564716610442178lwr0.487908703995611upr0.653615825046176
#Estimate0.565282874694591lwr0.488393362844748upr0.654277377075164
#Estimate0.565587419557617lwr0.488670377394615upr0.654611255274687
#Estimate0.564725705444874lwr0.487921747360907upr0.653619405396814

delta.eta <- glht(fit.linENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.522259909671224lwr0.450471488853899upr0.605488737908244
#Estimate0.522678580993263lwr0.450830436937869upr0.605977051781844
#Estimate0.52328968021179lwr0.451351991501179upr0.606692990332008
#Estimate0.523695298365613lwr0.451712861359761upr0.60714845422969
#Estimate0.522676448061953lwr0.450831413658045upr0.605970793255047

delta.eta <- glht(fit.linENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.463262093462031lwr0.390844815729983upr0.549097131653113
#Estimate0.463676740793068lwr0.391193422906872upr0.549590323771021
#Estimate0.46433836920614lwr0.391747682863321upr0.550380080211587
#Estimate0.464868297453203lwr0.392201794898191upr0.550998329911101
#Estimate0.463660356303126lwr0.39118164907713upr0.54956802425247


# 10th
k0 <- matrix(c(0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k1 <- matrix(c(2.68, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-6.720749), nrow=1)
k2 <- matrix(c(4.71, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-11.811465), nrow=1)
k3 <- matrix(c(6.52, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-16.350478), nrow=1)
k4 <- matrix(c(8.32, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-20.864413), nrow=1)
k5 <- matrix(c(10.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-25.629123), nrow=1)
k6 <- matrix(c(12.43, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-31.171233), nrow=1)
k7 <- matrix(c(15.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-38.167833), nrow=1)
k8 <- matrix(c(19.54, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-49.001279), nrow=1)

delta.eta <- glht(fit.linENMOimpute1, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.634727483108961lwr0.392498438629158upr1.02644733879945
#Estimate0.635327664110747lwr0.392871221285295upr1.02741361269448
#Estimate0.635660148228119lwr0.393094728066346upr1.02790445965278
#Estimate0.635007175354898lwr0.392718472301326upr1.02677653635506
#Estimate0.63546242721106lwr0.39298779542499upr1.02754462377203

delta.eta <- glht(fit.linENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.573784950704189lwr0.374109654927144upr0.880033875946677
#Estimate0.574325815953622lwr0.374462096798953upr0.880863899685657
#Estimate0.574714756325796lwr0.374729802198436upr0.881427228901619
#Estimate0.574268141062485lwr0.374463115338988upr0.880684597041877
#Estimate0.574446158856165lwr0.374569312638269upr0.880980844640845

delta.eta <- glht(fit.linENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.531549106603889lwr0.360253400043636upr0.784293646353287
#Estimate0.532048973217523lwr0.360590637655496upr0.785034552594979
#Estimate0.532471308382165lwr0.360888254569639upr0.785632922823493
#Estimate0.532158426481824lwr0.360698521011411upr0.78512268384558
#Estimate0.532159418489495lwr0.360690991912136upr0.785141999764895

delta.eta <- glht(fit.linENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.496519472032382lwr0.347855198027204upr0.708718994298412
#Estimate0.496985409154034lwr0.348178942365859upr0.70938953181283
#Estimate0.497431575499842lwr0.348500567118338upr0.710007947333509
#Estimate0.49722334764778lwr0.348375107354659upr0.709669124534707
#Estimate0.497087710386775lwr0.348273305191218upr0.709489323857018

delta.eta <- glht(fit.linENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.463973049231224lwr0.335381536769474upr0.641868936753331
#Estimate0.464407526663842lwr0.335691839083485upr0.642477193996933
#Estimate0.464872462256601lwr0.336034738819777upr0.643107337424474
#Estimate0.464756005156692lwr0.335969122656917upr0.642910701498539
#Estimate0.46450231776736lwr0.335780324282169upr0.642570119832084

delta.eta <- glht(fit.linENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.431930099573113lwr0.321918700576358upr0.579536418925705
#Estimate0.432333669087141lwr0.322214688425608upr0.580086532800951
#Estimate0.432813682397139lwr0.322577044907587upr0.580722300695133
#Estimate0.432782064042315lwr0.322570088466358upr0.580649978574379
#Estimate0.432421123303869lwr0.322297011534563upr0.580173011810031

delta.eta <- glht(fit.linENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.39743247737966lwr0.305631898083749upr0.516806573745951
#Estimate0.397802849021664lwr0.305910923804491upr0.517297992244596
#Estimate0.398295032527344lwr0.306291660145012upr0.517934222762877
#Estimate0.398348165804602lwr0.306346816477675upr0.51797914215131
#Estimate0.39788247245609lwr0.305986071641403upr0.517378000372845

delta.eta <- glht(fit.linENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.357788089475718lwr0.283598626950323upr0.451385531542463
#Estimate0.358120418934197lwr0.283855520749764upr0.451815184424983
#Estimate0.35862091761817lwr0.284252043813748upr0.452446922906094
#Estimate0.358762250210573lwr0.284375326781578upr0.452607311727199
#Estimate0.358191138413257lwr0.283921543523084upr0.451888539508991

delta.eta <- glht(fit.linENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.304061475835983lwr0.245207117966435upr0.377041995576211
#Estimate0.304342457921104lwr0.245427965572321upr0.37739925634581
#Estimate0.30484336083735lwr0.245830041668052upr0.378023263617613
#Estimate0.305086594267027lwr0.246032754800191upr0.378314790146719
#Estimate0.304401292843449lwr0.245480224064087upr0.37746481386856



# -------
# Repeat MICE analysis for MVPA
# -------

# Change MVPA Minutes from 10th to 90th percentile w/ genetic risk at 90th
41.76 - 31.68 # 10.08
51.84 - 31.68 # 20.16
59.04 - 31.68 # 27.36
67.68 - 31.68 # 36
76.32 - 31.68 # 44.64
87.84 - 31.68 # 56.16
100.80 - 31.68 # 69.12
120.96 - 31.68 # 89.28


# 90th
k1 <- matrix(c(0, 10.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k2 <- matrix(c(0, 20.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k3 <- matrix(c(0, 27.36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k4 <- matrix(c(0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k5 <- matrix(c(0, 44.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k6 <- matrix(c(0, 56.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k7 <- matrix(c(0, 69.12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)
k8 <- matrix(c(0, 89.28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0), nrow=1)

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.917238975947497lwr0.880085504415721upr0.955960909225247
#Estimate0.917094832131165lwr0.879958324233651upr0.95579859631894
#Estimate0.917235655417043lwr0.880089398182863upr0.955949758405707
#Estimate0.917117187100042lwr0.879987209746662upr0.955813818153604
#Estimate0.917185672457657lwr0.880037785865145upr0.955901634308362

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.841327338997214lwr0.774550495082675upr0.913861259966762
#Estimate0.84106293112169lwr0.774326652388095upr0.913550956725256
#Estimate0.841321247568332lwr0.774557348793874upr0.913839940595929
#Estimate0.841103934874294lwr0.774377489317716upr0.913580054973371
#Estimate0.841229557761604lwr0.774466504550426upr0.913747934473397

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.790982341089786lwr0.707008510442062upr0.884930032206663
#Estimate0.790644993693058lwr0.706731228886581upr0.884522263204273
#Estimate0.790974568853985lwr0.707017000818724upr0.884902014872707
#Estimate0.790697306254282lwr0.706794199807365upr0.884560499064896
#Estimate0.790857581522788lwr0.706904465159391upr0.884781105620895

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.734528973798079lwr0.633686606940712upr0.851418993930761
#Estimate0.734116803526138lwr0.633359620021578upr0.850902811267118
#Estimate0.734519477073441lwr0.633696619937467upr0.851383525216656
#Estimate0.734180715255323lwr0.63343387552562upr0.850951209714732
#Estimate0.734376536376686lwr0.633563905800876upr0.85123046348243

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.682104751675632lwr0.567968715913978upr0.819176970882569
#Estimate0.681630169694916lwr0.567605324170916upr0.818561187594523
#Estimate0.682093816217038lwr0.567979844409897upr0.819134655394146
#Estimate0.6817037548873lwr0.567687842900752upr0.818618921012706
#Estimate0.681929224402433lwr0.567832348665606upr0.818952051933829

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.617979306941892lwr0.490822275133189upr0.778078834553198
#Estimate0.61743843055938lwr0.490427234329105upr0.777343077313286
#Estimate0.617966842814666lwr0.490834373878404upr0.778028270108342
#Estimate0.617522288510075lwr0.49051693417832upr0.777412052950842
#Estimate0.617779248926658lwr0.49067402353492upr0.777810077767903

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.553015301764747lwr0.416484740898062upr0.734302830222554
#Estimate0.552419647859971lwr0.416072213236559upr0.733448323712573
#Estimate0.553001573972176lwr0.416497376415367upr0.734244098840908
#Estimate0.552511990681507lwr0.416165877008192upr0.733528423909758
#Estimate0.552794968749645lwr0.416329917816484upr0.733990675177997

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.465266892258475lwr0.322588462256973upr0.671050909624342
#Estimate0.464619688238319lwr0.322175804027171upr0.670042417836156
#Estimate0.465251974121522lwr0.322601103655891upr0.670981583667687
#Estimate0.464720009427444lwr0.322269486977309upr0.670136937840007
#Estimate0.465027467094103lwr0.322433576191099upr0.670682463366628

# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, 0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k1 <- matrix(c(0, 10.08, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -12.63801), nrow=1)
k2 <- matrix(c(0, 20.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -25.27602), nrow=1)
k3 <- matrix(c(0, 27.36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -34.30317), nrow=1)
k4 <- matrix(c(0, 36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -45.13576), nrow=1)
k5 <- matrix(c(0, 44.64, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -55.96834), nrow=1)
k6 <- matrix(c(0, 56.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -70.41178), nrow=1)
k7 <- matrix(c(0, 69.12, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -86.66065), nrow=1)
k8 <- matrix(c(0, 89.28, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -111.93667), nrow=1)

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.699963264995586lwr0.605793578200542upr0.808771485822995
#Estimate0.699915487730444lwr0.605734961915496upr0.808739334470324
#Estimate0.70011581870327lwr0.605923202950377upr0.808950964762267
#Estimate0.699890159571274lwr0.605723901550802upr0.808695569401467
#Estimate0.70021109470098lwr0.606019564204626upr0.809042489883698

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.629044545983764lwr0.553659746395971upr0.71469353408424
#Estimate0.628894574831211lwr0.553511573760276upr0.714544022205799
#Estimate0.629164006219473lwr0.553762079123265upr0.714832888790174
#Estimate0.628884064647906lwr0.553510228140508upr0.714521876310615
#Estimate0.629209547488939lwr0.553814872858287upr0.714868224119617

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.565311153628063lwr0.496905315687116upr0.643133994198439
#Estimate0.565080203518014lwr0.496693322037068upr0.642882886160749
#Estimate0.565402660741634lwr0.496985089767809upr0.643238952949422
#Estimate0.565081765130599lwr0.496702879279995upr0.642874069395341
#Estimate0.565407571584257lwr0.496997810099275upr0.643233663224692

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.52378021796205lwr0.454880773985397upr0.603115656713115
#Estimate0.523502595286498lwr0.454637347322905upr0.602799063661288
#Estimate0.523854513221523lwr0.454946930919843upr0.603199037891541
#Estimate0.523511324668612lwr0.454654587011138upr0.602796309299242
#Estimate0.523835229784178lwr0.454934775389544upr0.603170746241768

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.477950511171156lwr0.405387883689527upr0.563501526117937
#Estimate0.477627503866412lwr0.405118576074065upr0.563114223643876
#Estimate0.478006820260684lwr0.405439809486268upr0.563562124067812
#Estimate0.477643441782095lwr0.405143810752635upr0.563116728979827
#Estimate0.477963128794616lwr0.405403414504838upr0.563509690134622

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.436130817830281lwr0.358930300947948upr0.529936006402796
#Estimate0.435772502744733lwr0.358645076398487upr0.52948635474202
#Estimate0.436171719720843lwr0.358970009983939upr0.529976777427034
#Estimate0.435794318899776lwr0.35867649669134upr0.529492983614015
#Estimate0.436108041610478lwr0.358915481380641upr0.52990253645711

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.386007049739949lwr0.303373019660206upr0.491149287487164
#Estimate0.38561490897395lwr0.303078508047206upr0.490628184034175
#Estimate0.386030883473847lwr0.30339934852859upr0.491167313701586
#Estimate0.385642797650728lwr0.303115450184203upr0.490639349757669
#Estimate0.385946429589184lwr0.303328613389668upr0.491066915343347

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.336469733732843lwr0.249973595635929upr0.452895360528942
#Estimate0.336054379606999lwr0.249680792646855upr0.452307703992173
#Estimate0.336478381469018lwr0.249988422021024upr0.452891779070025
#Estimate0.336087099564266lwr0.249720912482133upr0.452323104904571
#Estimate0.336377219896945lwr0.249907688906775upr0.452765717455802

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.271742965452108lwr0.184113003719037upr0.401081063157248
#Estimate0.271315152401053lwr0.183839382253187upr0.400414269348595
#Estimate0.271734714574779lwr0.184115902723497upr0.401050392783986
#Estimate0.271352138420884lwr0.183880039709374upr0.40043488756019
#Estimate0.271618414043288lwr0.184031075674347upr0.400891874250319


# 10th
k0 <- matrix(c(0, 0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1)
k1 <- matrix(c(0, 10.08, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -25.27804), nrow=1)
k2 <- matrix(c(0, 20.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -50.55608), nrow=1)
k3 <- matrix(c(0, 27.36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -68.61182), nrow=1)
k4 <- matrix(c(0, 36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -90.27871), nrow=1)
k5 <- matrix(c(0, 44.64, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -111.94560), nrow=1)
k6 <- matrix(c(0, 56.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -140.83479), nrow=1)
k7 <- matrix(c(0, 69.12, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -173.33513), nrow=1)
k8 <- matrix(c(0, 89.28, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, -223.89121), nrow=1)

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.489920692751347lwr0.36695651881176upr0.654089171009073
#Estimate0.489853808844996lwr0.36688550352406upr0.654037163461307
#Estimate0.49013428489117lwr0.367113587268102upr0.654379531450952
#Estimate0.489818353535046lwr0.366872104350086upr0.653966373062909
#Estimate0.490267705489874lwr0.367230371575666upr0.654527625302311

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.431374202614588lwr0.335760728615198upr0.55421520988726
#Estimate0.431236301462568lwr0.335633376405158upr0.554071081043586
#Estimate0.431539636058019lwr0.335885520064762upr0.554434312777703
#Estimate0.431211373835502lwr0.335623819812146upr0.55402280156747
#Estimate0.431625640587132lwr0.335967700280088upr0.554519656076871

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.379824133649762lwr0.303537522559446upr0.475283488137908
#Estimate0.379633156548466lwr0.303367938066719upr0.475071078603091
#Estimate0.379949869310689lwr0.303633775560724upr0.475447446261914
#Estimate0.379616744826196lwr0.303362778165014upr0.475038084184636
#Estimate0.379997889981558lwr0.303684662089819upr0.475487946598134

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.346818961866826lwr0.279642901338816upr0.43013211397291
#Estimate0.346599221789041lwr0.279452373533944upr0.429880122417988
#Estimate0.34692077470848lwr0.279721197194991upr0.430264224274838
#Estimate0.346587846419735lwr0.279450694976156upr0.429854487555019
#Estimate0.346946554907094lwr0.279751823947923upr0.430281062204294

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.310976209864617lwr0.250262773279273upr0.386418650423273
#Estimate0.310730381748291lwr0.250056888801858upr0.386125615671186
#Estimate0.311053516763155lwr0.25032229431277upr0.386518869828809
#Estimate0.310724065476827lwr0.250059500571378upr0.38610588538262
#Estimate0.311057194851911lwr0.250331499496292upr0.386513797360019

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.278837704205159lwr0.221001193263537upr0.351810160561842
#Estimate0.278573534132763lwr0.220788911180013upr0.351481482944342
#Estimate0.27889448353743lwr0.221044888096111upr0.351883880317511
#Estimate0.278571351718784lwr0.220795471591454upr0.351465532508839
#Estimate0.278880355203554lwr0.221036631463296upr0.351861372495515

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.241092632430333lwr0.184158133667324upr0.315629053437249
#Estimate0.240813796950471lwr0.183947318889174upr0.315260288390729
#Estimate0.24112727202716lwr0.184184929932408upr0.315673824870456
#Estimate0.240815921706853lwr0.183957967581916upr0.315247601992
#Estimate0.241094969918939lwr0.184160068189469upr0.315631858152939

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.204701221278097lwr0.147763909744891upr0.283577972896682
#Estimate0.20441631960956lwr0.147563490526442upr0.283173240031416
#Estimate0.204716827062755lwr0.147776713268886upr0.283596639521863
#Estimate0.204421953983769lwr0.147576919315162upr0.28316308176416
#Estimate0.20467021872774lwr0.14774106129069upr0.283535924732799

delta.eta <- glht(fit.linMVPAMinsENMOimpute1, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute2, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute3, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute4, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
delta.eta <- glht(fit.linMVPAMinsENMOimpute5, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
#Estimate0.158700102239737lwr0.103154505048329upr0.244155332228127
#Estimate0.158421168238692lwr0.102979768505088upr0.243710652203223
#Estimate0.158695553618461lwr0.103153844269915upr0.244142900504727
#Estimate0.158430153101187lwr0.102994537735509upr0.243703345473748
#Estimate0.158636292759469lwr0.103112533718604upr0.244058335809546



# -------
# Correlations between PA Exposures
# -------


# Merging as one large dataset
Corrdata <- data[ , c("MVPAMins", "OverallPAEETRANSFORM", "PercentMVPA", "p90012")]




install.packages('corrplot')
install.packages('RColorBrewer')

library(corrplot)
library(RColorBrewer)

colnames(Corrdata) <- c("MVPA Minutes", "PAEE", "Percent MVPA", "ENMO")

M <-cor(Corrdata)

# Circles
corrplot(M, method = "color", type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


# Squares and fixed legend of corr
corrplot(M, method = "color", type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), addCoef.col = "dark grey", tl.col = "black", tl.cex = 1, cl.pos = "b")


# ---------
# Sex Stratified Analyses
# ---------

# Create male and female-specific datasets
datamale <- subset(data, Biological.Sex == "Male")
datafemale <- subset(data, Biological.Sex == "Female")


mean(datamale$Status)*nrow(datamale)
mean(datafemale$Status)*nrow(datafemale)
# 1026 cases among males vs only 372 among females

fit.linENMOmale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + OilyFish_InstChosen + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen)+ REGION, data = datamale)
fit.linENMOfemale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + OilyFish_InstChosen + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen)+ REGION, data = datafemale)



fit.linENMOMVPAmale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + OilyFish_InstChosen + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + REGION, data = datamale)
fit.linENMOMVPAfemale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + OilyFish_InstChosen + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + REGION, data = datafemale)





# 90th
k1 <- matrix(c(2.68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k2 <- matrix(c(4.71, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k3 <- matrix(c(6.52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k4 <- matrix(c(8.32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k5 <- matrix(c(10.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k6 <- matrix(c(12.43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k7 <- matrix(c(15.22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k8 <- matrix(c(19.54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# ACTUAL RESULTS MALE
Estimate0.937113738017224lwr0.913993233761215upr0.960819101873181
Estimate0.892125653611771lwr0.853805102521625upr0.932166110839178
Estimate0.853837664172166lwr0.803490385433906upr0.907339739187147
Estimate0.817390980730863lwr0.756394492470954upr0.883306293251229
Estimate0.780605933926251lwr0.709673649679664upr0.858627940259475
Estimate0.739896588562462lwr0.65894810392608upr0.830789190384834
Estimate0.691521283116898lwr0.600055066129411upr0.796929668618963
Estimate0.622782542519548lwr0.519079076591864upr0.747204256071515

delta.eta <- glht(fit.linENMOmale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOmale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOmale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOmale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOmale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOmale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOmale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOmale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825

# ACTUAL RESULTS - FEMALE
Estimate0.930100610818917lwr0.889633932086383upr0.972407992821169
Estimate0.880425323331327lwr0.814218233776512upr0.952015955682755
Estimate0.838375320572611lwr0.752383957474934upr0.934194796635659
Estimate0.79854955307273lwr0.695549014746631upr0.916802950177346
Estimate0.758561855229895lwr0.64020764128223upr0.898796032889202
Estimate0.71456202906289lwr0.581351984479013upr0.878295605778407
Estimate0.662640814543926lwr0.514713891322058upr0.85308140406239
Estimate0.589590269663145lwr0.426282594493692upr0.815460660537488

delta.eta <- glht(fit.linENMOfemale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOfemale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOfemale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOfemale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOfemale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOfemale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOfemale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOfemale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825


# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k1 <- matrix(c(2.68, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-3.360106), nrow=1)
k2 <- matrix(c(4.71, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-5.905261), nrow=1)
k3 <- matrix(c(6.52, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-8.174587), nrow=1)
k4 <- matrix(c(8.32, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-10.431375), nrow=1)
k5 <- matrix(c(10.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-12.813540), nrow=1)
k6 <- matrix(c(12.43, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-15.584374), nrow=1)
k7 <- matrix(c(15.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-19.082395), nrow=1)
k8 <- matrix(c(19.54, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-24.498685), nrow=1)

# ACTUAL RESULTS MALE
Estimate0.693704242369824lwr0.522430413500899upr0.921128562667539
Estimate0.640165340347819lwr0.498998736158305upr0.821267937746096
Estimate0.602379518309961lwr0.480857872669503upr0.754611923196559
Estimate0.570573633993529lwr0.464184521850921upr0.701346676770789
Estimate0.540609109568901lwr0.446929875093505upr0.653924084371702
Estimate0.510685404343359lwr0.427761682647031upr0.609684300369045
Estimate0.477956807023954lwr0.403998980582472upr0.565453677757236
Estimate0.439617250275881lwr0.371778384692984upr0.51983475827871
Estimate0.386230630172328lwr0.319333842347725upr0.467141529963105

delta.eta <- glht(fit.linENMOmale, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOmale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOmale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOmale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOmale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOmale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOmale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOmale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOmale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825

# ACTUAL RESULTS - FEMALE
Estimate0.869638677005476lwr0.562344633301618upr1.34485399834554
Estimate0.794205730519434lwr0.535718649447348upr1.17741419500816
Estimate0.732483362966333lwr0.510943166094569upr1.05008132533309
Estimate0.675859826768277lwr0.484985861667287upr0.941855302480164
Estimate0.620832333203621lwr0.455815755030544upr0.845588994450671
Estimate0.562437646535472lwr0.4193618684709upr0.754327300652782
Estimate0.496495857996607lwr0.370068897958254upr0.66611417054452
Estimate0.409314301929995lwr0.29248043228172upr0.572818483812498

delta.eta <- glht(fit.linENMOfemale, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOfemale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOfemale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOfemale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOfemale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOfemale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOfemale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOfemale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOfemale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825


# 10th
k0 <- matrix(c(0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k1 <- matrix(c(2.68, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-6.720749), nrow=1)
k2 <- matrix(c(4.71, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-11.811465), nrow=1)
k3 <- matrix(c(6.52, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-16.350478), nrow=1)
k4 <- matrix(c(8.32, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-20.864413), nrow=1)
k5 <- matrix(c(10.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-25.629123), nrow=1)
k6 <- matrix(c(12.43, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-31.171233), nrow=1)
k7 <- matrix(c(15.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-38.167833), nrow=1)
k8 <- matrix(c(19.54, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-49.001279), nrow=1)

# ACTUAL RESULTS MALE
Estimate0.481197503184945lwr0.272905270767691upr0.848466709419225
Estimate0.43728603386234lwr0.263986176456437upr0.724352608071547
Estimate0.406712159695382lwr0.257038100837794upr0.643541872994417
Estimate0.381259052400055lwr0.250626801296025upr0.579979731957314
Estimate0.357526501080404lwr0.243963765020367upr0.523951575202674
Estimate0.334076286204092lwr0.236500203803684upr0.471910650430398
Estimate0.308727990101408lwr0.227045736660664upr0.419796351492417
Estimate0.279455408787745lwr0.213440655053058upr0.365887770918396
Estimate0.23951013455897lwr0.187494397334475upr0.30595636654743

delta.eta <- glht(fit.linENMOmale, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOmale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOmale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOmale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOmale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOmale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOmale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOmale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOmale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825

# ACTUAL RESULTS - FEMALE
Estimate0.960999663418272lwr0.355553041637029upr2.59741935784878
Estimate0.813098389960344lwr0.335862948847516upr1.96844872000532
Estimate0.716417789077999lwr0.320829408758913upr1.59977369435323
Estimate0.639952426543127lwr0.307193764191562upr1.33316217963023
Estimate0.572005025110797lwr0.293262573116364upr1.1156887333938
Estimate0.508093664123078lwr0.277947598962901upr0.928805186608117
Estimate0.442682395783056lwr0.258989242189529upr0.756663488720572
Estimate0.371991559422373lwr0.232593462295667upr0.594933833976754
Estimate0.284143838508081lwr0.185363608410174upr0.435564033601726

delta.eta <- glht(fit.linENMOfemale, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOfemale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOfemale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOfemale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOfemale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOfemale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOfemale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOfemale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOfemale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825







# -------
# Repeat mediators analysis for MVPA
# -------

# Change MVPA Minutes from 10th to 90th percentile w/ genetic risk at 90th
41.76 - 31.68 # 10.08
51.84 - 31.68 # 20.16
59.04 - 31.68 # 27.36
67.68 - 31.68 # 36
76.32 - 31.68 # 44.64
87.84 - 31.68 # 56.16
100.80 - 31.68 # 69.12
120.96 - 31.68 # 89.28


# 90th
k1 <- matrix(c(0, 10.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k2 <- matrix(c(0, 20.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k3 <- matrix(c(0, 27.36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k4 <- matrix(c(0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k5 <- matrix(c(0, 44.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k6 <- matrix(c(0, 56.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k7 <- matrix(c(0, 69.12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k8 <- matrix(c(0, 89.28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# ACTUAL RESULTS MALE
#Estimate0.903669394180135lwr0.862034263035797upr0.947315447882588
#Estimate0.816618373977893lwr0.74310307064767upr0.897406557796989
#Estimate0.759621790257234lwr0.66833644968671upr0.863375421921238
#Estimate0.696452232014346lwr0.588478305414449upr0.824237201295229
#Estimate0.638535805184724lwr0.518162246134858upr0.78687318025248
#Estimate0.56873568926083lwr0.437300035763015upr0.739675869622575
#Estimate0.499288269154157lwr0.3613120762447upr0.689954175642118
#Estimate0.407727974502904lwr0.268492113319521upr0.619169401800652



delta.eta <- glht(fit.linENMOMVPAmale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825

# ACTUAL RESULTS - FEMALE
Estimate0.908229909936329lwr0.830993397402093upr0.992645154440158
Estimate0.824881569302952lwr0.690550026525873upr0.985344402633524
Estimate0.770072220969023lwr0.605013187910103upr0.98016247803887
Estimate0.70908661224783lwr0.516236224043647upr0.973980128187582
Estimate0.652930738153884lwr0.440485999876157upr0.967836773350427
Estimate0.584912477482704lwr0.356486938206957upr0.959705867585917
Estimate0.516823947450332lwr0.280975901287042upr0.950640220156355
Estimate0.426318548826176lwr0.194027916086898upr0.936708019849366



delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825


# 50th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, 0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0), nrow=1)
k1 <- matrix(c(0, 10.08, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -12.63801), nrow=1)
k2 <- matrix(c(0, 20.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -25.27602), nrow=1)
k3 <- matrix(c(0, 27.36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -34.30317), nrow=1)
k4 <- matrix(c(0, 36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -45.13576), nrow=1)
k5 <- matrix(c(0, 44.64, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -55.96834), nrow=1)
k6 <- matrix(c(0, 56.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -70.41178), nrow=1)
k7 <- matrix(c(0, 69.12, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -86.66065), nrow=1)
k8 <- matrix(c(0, 89.28, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -111.93667), nrow=1)

# ACTUAL RESULTS MALE
Estimate0.622936213353698lwr0.522943085233456upr0.74204925328386
Estimate0.559371146885271lwr0.479596515184988upr0.652415249195574
Estimate0.502292326662478lwr0.431217408323854upr0.58508208748967
Estimate0.465123536877408lwr0.394646502856248upr0.548186549206929
Estimate0.424133369346577lwr0.350953993566551upr0.512571785165247
Estimate0.386755564130948lwr0.309575359917592upr0.483177557884662
Estimate0.341991575280801lwr0.259940096202939upr0.449943041768104
Estimate0.297794449957927lwr0.212336686543823upr0.417645842879073
Estimate0.240120679983024lwr0.154091076714089upr0.374180920693368

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825

# ACTUAL RESULTS - FEMALE
Estimate0.833991022011374lwr0.630289374284428upr1.10352649619903
Estimate0.72125212412949lwr0.562729709816198upr0.924430712448459
Estimate0.623753269317817lwr0.481611674972438upr0.807846157398347
Estimate0.562289255337522lwr0.420671797987395upr0.751581656247606
Estimate0.496473540416902lwr0.351187832228564upr0.701863657319632
Estimate0.438361543540826lwr0.289626125479684upr0.663478968056611
Estimate0.371319337728567lwr0.221641852597912upr0.622075880322618
Estimate0.308071892773366lwr0.162765765010427upr0.583097379912072
Estimate0.230411173778432lwr0.0998772775232168upr0.531545415718946

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843


delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825


# 10th
k0 <- matrix(c(0, 0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -25.27804), nrow=1)
k2 <- matrix(c(0, 20.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -50.55608), nrow=1)
k3 <- matrix(c(0, 27.36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -68.61182), nrow=1)
k4 <- matrix(c(0, 36, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -90.27871), nrow=1)
k5 <- matrix(c(0, 44.64, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -111.94560), nrow=1)
k6 <- matrix(c(0, 56.16, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -140.83479), nrow=1)
k7 <- matrix(c(0, 69.12, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -173.33513), nrow=1)
k8 <- matrix(c(0, 89.28, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -223.89121), nrow=1)

# ACTUAL RESULTS MALE
Estimate0.388020228494353lwr0.273441191490092upr0.550610889677407
Estimate0.346224118664828lwr0.255715143428614upr0.46876825024131
Estimate0.308930131839715lwr0.236043643245105upr0.404322798303034
Estimate0.284777213118837lwr0.220500338649305upr0.367791095508084
Estimate0.258273111244512lwr0.200290743868856upr0.333040852030569
Estimate0.234235735582132lwr0.179073161174725upr0.306390859823869
Estimate0.205629347256008lwr0.151105474247372upr0.27982724427118
Estimate0.177601256380389lwr0.122468567505529upr0.257553484215194
Estimate0.141400822739042lwr0.0864274366371614upr0.231340803907182

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOMVPAmale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825

# ACTUAL RESULTS - FEMALE
Estimate0.69552088370578lwr0.397235445784839upr1.21778986443443
Estimate0.572746487409941lwr0.35274144874952upr0.92996879159887
Estimate0.471644412878899lwr0.304524454194409upr0.730478124616784
Estimate0.41055024255lwr0.268019233082167upr0.628878382045781
Estimate0.347590783437888lwr0.223721426457168upr0.540043726004475
Estimate0.294286399590303lwr0.181731733029661upr0.476551252442457
Estimate0.235707158213856lwr0.133455877980461upr0.416301366968535
Estimate0.183622405487653lwr0.0917619859607963upr0.36744178369763
Estimate0.124517442475919lwr0.0497352823274206upr0.311742343768615

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843


delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.944684042704023
# Lower = 0.922998342331267
# Upper = 0.9668792451843

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904830196208492
# Lower = 0.868644186072571
# Upper = 0.942523644430746

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87071555943549
# Lower = 0.822885773746337
# Upper = 0.921325425267074

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.838065068619308
# Lower = 0.779770926244581
# Upper = 0.900717166543327

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.804927992057586
# Lower = 0.736709125929489
# Upper = 0.879463888248168

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.768029108282854
# Lower = 0.689604035501284
# Upper = 0.855373055844969

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72385220674871
# Lower = 0.634413469245865
# Upper = 0.825899894335184

delta.eta <- glht(fit.linENMOMVPAfemale, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.660409549087899
# Lower = 0.557542986690105
# Upper = 0.782254970357825


# -----------
# Redoing Subjective and Objective MVPA Correlation
# -----------



# Keep dates to know which Subjective PAs are closest
SUBJPA <- newdata

# Getting dates of subjective PA and Accel Wear Date
FINAL <- data[ , c("eid", "AccelDate", "Date.Attending.Assess.Center.Inst.0", "Date.Attending.Assess.Center.Inst.1", "Date.Attending.Assess.Center.Inst.2", "Date.Attending.Assess.Center.Inst.3")]


# Getting SUBJPA - Percent MVPA
# BEST comparison anyway
# ADDING MVPA and VIG TOGETHER
# Note: ONLY visit 0 has MOST non-missing (as expected)
SUBJPA$SumMVPA.0 <- as.numeric(SUBJPA$p894_i0) + as.numeric(SUBJPA$p914_i0)
SUBJPA$SumMVPA.1 <- as.numeric(SUBJPA$p894_i1) + as.numeric(SUBJPA$p914_i1)
SUBJPA$SumMVPA.2 <- as.numeric(SUBJPA$p894_i2) + as.numeric(SUBJPA$p914_i2)
SUBJPA$SumMVPA.3 <- as.numeric(SUBJPA$p894_i3) + as.numeric(SUBJPA$p914_i3)

SUBJPA <- SUBJPA[ , c("eid", "SumMVPA.0", "SumMVPA.1", "SumMVPA.2", "SumMVPA.3")]



# Keeping ONLY eid and PercentMVPA
dataMVPAMins <- data[ , c("eid", "MVPAMins")]


# Source: http://www.sthda.com/english/wiki/correlation-analyses-in-r

# Merging as one large dataset
CorrData <- merge(FINAL, SUBJPA, by = "eid", all = T)
CorrData <- merge(CorrData, dataMVPAMins, by = "eid", all = T)

summary(is.na(CorrData))
# Checked missingness - NONE in visit 0
# About 5/6 in visit 1
# About 1/2 in visit 2
# ALMOST ALL in visit 3

# Least missingness so comparing Visit 0 and Visit 2 corr
# ALL FOR INDS WITHOUT MISSINGNESS IN EITHER

install.packages('corrplot')
install.packages('RColorBrewer')

library(corrplot)
library(RColorBrewer)

# CorrData visit 0 and visit 1 for comp
CorrDataSub <- subset(CorrData, is.na(SumMVPA.0) == FALSE & is.na(SumMVPA.2) == FALSE & is.na(MVPAMins) == FALSE)
dim(CorrDataSub)
# STILL only 6618 here

CorrDataSubVisit0 <- subset(CorrData, is.na(SumMVPA.0) == FALSE & is.na(MVPAMins) == FALSE)
dim(CorrDataSubVisit0)
# 36,898 inds here

# Getting which visit is CLOSER to accelerometer wear
CorrDataSub$Visit0Diff <- abs(as.Date(CorrDataSub$AccelDate) - as.Date(CorrDataSub$Date.Attending.Assess.Center.Inst.0))
CorrDataSub$Visit2Diff <- abs(as.Date(CorrDataSub$AccelDate) - as.Date(CorrDataSub$Date.Attending.Assess.Center.Inst.2))

# Closer and farther visits selecting SumMVPA
CorrDataSub$CloserVisit <- ifelse(CorrDataSub$Visit0Diff < CorrDataSub$Visit2Diff, CorrDataSub$SumMVPA.0, CorrDataSub$SumMVPA.2)
CorrDataSub$FartherVisit <- ifelse(CorrDataSub$Visit0Diff < CorrDataSub$Visit2Diff, CorrDataSub$SumMVPA.2, CorrDataSub$SumMVPA.0)


# Restrict ONLY to PA variables
CorrDataSubPA <- CorrDataSub[ , c("CloserVisit", "FartherVisit", "MVPAMins")]



M <-cor(CorrDataSubPA)

# Circles
corrplot(M, method = "color", type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

# Squares and fixed legend of corr
corrplot(M, method = "color", type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), addCoef.col = "dark grey", tl.col = "black", tl.cex = 1, cl.pos = "b")

