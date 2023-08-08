# ------------
# 7/6/2023
# New Analyses based on Circ Comments
# ------------

dx download FinalDatasetwNewExposures.csv

# Final Exposures:
# PAEEPOS in kJ/kg/day - NOTE OverallPAEETRANSFORM appears to be equiv but diff name so using
# PercentMVPA in %
# p90012 (ENMO) in mgs
# MVPAMins in mins/day
# IntensGrad unitless

# Reading in needed packages
install.packages("survival")
library(survival)

install.packages("ggplot2")
library(ggplot2)

install.packages("multcomp")
library(multcomp)


data <- read.csv("DatasetwNewExposures.csv")


# STILL PA volume alone and PA intensity CONTROLLING for PA volume in regs



# RANGE for each exposure (and units)
summary(data$OverallPAEETRANSFORM)
# Min = 1.38 and Max = 149.07 kJ/kg/day

summary(data$PercentMVPA)
# Min = 0 and Max = 88.87%

summary(data$MVPAMins)
# Min = 0 and Max = 470.88 mins/day

summary(data$p90012)
# Min = 3.99 mgs and Max = 97.11 mgs on average

summary(data$Intensity.Gradient)
# Min = -0.83 and Max = -0.37
# Makes sense that they are all negative - time spent decreases as activity level increases

summary(data$StandPGS)
# Min = -4.26 and Max = 3.99 SDs

# Deciles for each exposure (and units)
quantile(data$OverallPAEETRANSFORM, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# 0% 1.38025084922256
# 10% 25.8357508109679
# 20% 30.0209664478813
# 30% 33.1533444097311
# 40% 35.887713457484
# 50% 38.6116077499667
# 60% 41.3712369668165
# 70% 44.5428925296962
# 80% 48.474925153574
# 90% 54.3272821852545
# 100% 149.071716407243

quantile(data$PercentMVPA, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# 0% 0
# 10% 0.211577691492217
# 20% 0.259328225722848
# 30% 0.295122000635783
# 40% 0.326224308037015
# 50% 0.355539884843425
# 60% 0.385086263660446
# 70% 0.417018807736335
# 80% 0.454333631101671
# 90% 0.505831374916045
# 100% 0.88871206755059


quantile(data$MVPAMins, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# 0% 0
# 10% 31.68
# 20% 41.76
# 30% 51.84
# 40% 59.04
# 50% 67.6800000000001
# 60% 76.3200000000001
# 70% 87.8400000000001
# 80% 100.8
# 90% 120.96
# 100% 470.88

quantile(data$p90012, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# 0% 3.99
# 10% 18.75
# 20% 21.43
# 30% 23.46
# 40% 25.27
# 50% 27.065
# 60% 28.97
# 70% 31.18
# 80% 33.97
# 90% 38.29
# 100% 97.11


quantile(data$Intensity.Gradient, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# 0% -0.830429517869216
# 10% -0.740889735583082
# 20% -0.726301329963941
# 30% -0.715256096306618
# 40% -0.704953636723894
# 50% -0.694672411514131
# 60% -0.683537459631212
# 70% -0.670700002979227
# 80% -0.654399517770911
# 90% -0.629477492558648
# 100% -0.367917451105692


quantile(data$StandPGS, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
# 0% -4.26049335394351
# 10% -1.2687787156677
# 20% -0.83351688928712
# 30% -0.524242500848616
# 40% -0.260085293025043
# 50% -0.0148077360175137
# 60% 0.237628126974166
# 70% 0.50056964352169
# 80% 0.811623092364168
# 90% 1.23896364242179
# 100% 3.99225498695342


# Standard Deviation units for each exposure (and standardizing each)

# PAEE
sd(data$OverallPAEETRANSFORM)
data$StandardizedPAEE <- scale(data$OverallPAEETRANSFORM)
# 11.51 kJ/kg/day

# Percent MVPA
sd(data$PercentMVPA)
data$StandardizedPercentMVPA <- scale(data$PercentMVPA)
# 11.39% of PAEE

# MVPA in mins/day
sd(data$MVPAMins)
data$StandardizedMVPAMins <- scale(data$MVPAMins)
# 36.69 minutes of MVPA+ per day

# ENMO
sd(data$p90012)
data$StandardizedENMO <- scale(data$p90012)
# 8.20 miligravities average over week

# Intensity Gradient
sd(data$Intensity.Gradient)
data$StandardizedIntensity.Gradient <- scale(data$Intensity.Gradient)
# 0.045 units

sd(data$StandPGS)
# Already standardized so 1 (well 0.98)

# -----------
# Getting covariates formatted same way
# A few of these variables had to be re-added...
# ************FIXED SO NO NEED TO RUN AGAIN!!!!!!!!!*******
# -----------

# -----
# F&V Intake - split into quartiles... LESS influence of outliers
# -----

data$FruitnVeg <- data$Fruit + data$Veggie

quantile(data$FruitnVeg, probs = c(0.20, 0.4, 0.6, 0.8), na.rm = TRUE)
# 20% 5
# 40% 6.5
# 60% 8
# 80% 10.5

data$FnVScore <- ifelse(data$FruitnVeg < 5, 0,
                          ifelse(data$FruitnVeg >= 5 & data$FruitnVeg < 6.5, 1,
                                 ifelse(data$FruitnVeg >= 6.5 & data$FruitnVeg < 8, 2,
                                        ifelse(data$FruitnVeg >= 7 & data$FruitnVeg < 10.5, 3, 4))))


data$AlcIntake_Weekly <- data$AlcIntake_InstChosen/52

# Converting these separate variables into single ParentHist variable
data$ParentHist <- ifelse(data$MotherHeartDisease == 1 | data$FatherHeartDisease == 1, 1, 0)



# -------
# Coding in missingness for reporting of do not know or prefer not to answer in the rest of the vars
# NO such  category for:
# Mobility Problems, Biological Sex, BMI, Pack Years
# -------

data$EmploymentStatus_InstChosen <- ifelse(grepl("None of the above", data$EmploymentStatus_InstChosen), NA, data$EmploymentStatus_InstChosen)
data$EmploymentStatus_InstChosen <- ifelse(grepl("Prefer not to answer", data$EmploymentStatus_InstChosen), NA, data$EmploymentStatus_InstChosen)
# Recodes if it contains EITHER statement (even if others are included)
# 504 NAs now
# Keeping as categorical factor


# In paid employment/self-employed, unemployed, retired as categories
data$EmploymentStatus_InstChosen <- ifelse(grepl("Unemployed", data$EmploymentStatus_InstChosen), "Unemployed", data$EmploymentStatus_InstChosen)
data$EmploymentStatus_InstChosen <- ifelse(grepl("In paid employment or self-employed", data$EmploymentStatus_InstChosen), "In paid employment or self-employed", data$EmploymentStatus_InstChosen)
data$EmploymentStatus_InstChosen <- ifelse(grepl("Retired", data$EmploymentStatus_InstChosen), "Retired", data$EmploymentStatus_InstChosen)
# 46830 employed, 25096 retired, 1007 unemployed

# Adding the rest of the factors as OTHER
# INCLUDES students, looking after home/family, volunteer work, disability
data$EmploymentStatus_InstChosen <- ifelse(data$EmploymentStatus_InstChosen == "Retired", "Retired", 
                                             ifelse(data$EmploymentStatus_InstChosen == "Unemployed", "Unemployed",
                                                    ifelse(data$EmploymentStatus_InstChosen == "In paid employment or self-employed", "In paid employment or self-employed", "Other")))


data$EmploymentStatus_InstChosen <- as.factor(data$EmploymentStatus_InstChosen)

# ---
# Recoding Employment Status
# code as 1 if 'in paid employment or self-employed' and 0 o.w.
# ---

summary(as.factor(data$EmploymentStatus_InstChosen))

data$NewEmploy <- ifelse(grepl("In paid employment or self-employed", data$EmploymentStatus_InstChosen), 1, 0)
summary(as.factor(data$NewEmploy))



data$MobilityDichot <- ifelse(grepl("I am unable to walk about", data$MobilProbs) | grepl("I have moderate problems in walking about", data$MobilProbs) | grepl("I have severe problems in walking about", data$MobilProbs) | grepl("I have slight problems in walking about", data$MobilProbs), 1, 0)


# --------
# Education level - also time invariant
# --------

# treat this as none/uni/other
data$NewEduc <- ifelse(grepl("None of the above", data$EA.Inst.0), "None",
                             ifelse(grepl("College or University degree", data$EA.Inst.0), "Uni", "Other"))

summary(as.factor(data$NewEduc))
# Matches up well

# ----
# BP or Cholesterol Med Use
# ----

data$CholMeds <- ifelse(grepl("Cholesterol lowering medication", data$Meds_InstChosen) == TRUE, 1, 0)
# Variable for whether cholesterol meds are taken (IRRESPECTIVE to other meds)

data$BPMeds <- ifelse(grepl("Blood pressure medication", data$Meds_InstChosen), 1, 0)
# Variable for whether BP meds are taken (IRRESPECTIVE to other meds)

summary(data$CholMeds)
summary(data$BPMeds)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.0813  0.0000  1.0000 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.0876  0.0000  1.0000 

# Converting this into a single Meds variable
data$Meds <- ifelse(data$CholMeds == 1 | data$BPMeds == 1, 1, 0)


# Creating Base Models

# PAEE
fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandPGS + OverallPAEETRANSFORM*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
# Coefficients:
# PAEE = -0.0163787 => 0.9837547 (0.0028158 SE)
# PGS = 0.2487388 => 1.2824070 (0.0966613 SE)
# Interaction term = 0.0041923 => 1.0042011 (0.0025866 SE)

# ENMO
fit.linENMO <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandPGS + p90012*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
# Coefficients:
# ENMO = -0.0249830 => 0.9753265 (0.0041463 SE)
# PGS = 0.2248787 => 1.2521708 (0.1001134 SE)
# Interaction term = 0.0068336 => 1.0068570 (0.0037821 SE)
# Interaction is actually significant at 10% level (fringe for PA volume)

# Percent MVPA
fit.linMVPA <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + PercentMVPA + StandPGS + PercentMVPA*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
# Coefficients:
# Percent MVPA = -2.098 => 1.227 (0.3636 SE)
# PGS = 0.2888 => 1.335 (0.08412 SE)
# Interaction term = 0.3427 => 1.409 (0.2473 SE)

# MVPA in Mins
fit.linMVPAMins <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
# Coefficients:
# MVPA Mins = -0.01094 => 0.9891 (0.002141 SE)
# PGS = 0.3118 => 1.366 (0.06056 SE)
# Interaction term = 0.001436 => 1.001 (0.0008803 SE)

# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMO <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + MVPAMins + StandPGS + MVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
# Coefficients:
# MVPA Mins = -0.009874 => 0.9902 (0.002100 SE)
# PGS = 0.3118 => 1.366 (0.06040 SE)
# Interaction term = 0.001432 => 1.001 (0.0008773 SE)

# Intensity Gradient
fit.linIG <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + Intensity.Gradient + StandPGS + Intensity.Gradient*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
# Coefficients: 
# Int Gradient = -3.1457383 => 0.0430351 (1.1058382 SE)
# PGS = 0.8596385 => 2.3623067 (0.4766047 SE)
# Interaction term = 0.6574983 => 1.9299581 (0.6785162 SE)

# Intensity Gradient CONTROLLING FOR ENMO
fit.linIGENMO <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + Intensity.Gradient + StandPGS + Intensity.Gradient*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
# Coefficients: 
# Int Gradient = -2.8757250 => 0.0563753 (1.2363837 SE)
# PGS = 0.8607140 => 2.3648487 (0.4782309 SE)
# Interaction term = 0.6590621 => 1.9329785 (0.6808467 SE)

# Saving dataset w/ these covariates
write.csv(data, "FinalDatasetwNewExposures.csv")

dx upload FinalDatasetwNewExposures.csv


# -------
# Running across deciles results
# For PA variables more is better for PGS more is worse
# Reference group as high risk: 90th percentile genetic risk and 10th PA
# This is a change from prev manuscript but more in line w/ previous
# -------

# 10th PA and genetic risk at 90th as reference w/ HR = 1???

# Genetic Risk Diffs - 90th on down
1.23896364242179 - 0.811623092364168 # -0.4273406
1.23896364242179 - 0.50056964352169 # -0.738394
1.23896364242179 - 0.237628126974166 # -1.001336
1.23896364242179 - -0.0148077360175137 # -1.253771
1.23896364242179 - -0.260085293025043 # -1.499049
1.23896364242179 - -0.524242500848616 # -1.763206
1.23896364242179 - -0.83351688928712 # -2.072481
1.23896364242179 - -1.2687787156677 # -2.507742


# Change PAEE from 10th to 90th percentile w/ genetic risk at 90th
30.0209664478813 - 25.8357508109679 # 4.185216
33.1533444097311 - 25.8357508109679 # 7.317594
35.887713457484 - 25.8357508109679 # 10.05196
38.6116077499667 - 25.8357508109679 # 12.77586
41.3712369668165 - 25.8357508109679 # 15.53549
44.5428925296962 - 25.8357508109679 # 18.70714
48.474925153574 - 25.8357508109679 # 22.63917
54.3272821852545 - 25.8357508109679 # 28.49153


# -------
# Is this correct? That interaction is product of marginal increase in both
# COULD also be that it's product of LEVEL of both
# -------

# 90th
k1 <- matrix(c(4.185216, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k2 <- matrix(c(7.317594, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k3 <- matrix(c(10.05196, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k4 <- matrix(c(12.77586, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k5 <- matrix(c(15.53549, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k6 <- matrix(c(18.70714, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k7 <- matrix(c(22.63917, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k8 <- matrix(c(28.49153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)

delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.933748147890769
# Lower = 0.912428167998559
# Upper = 0.955566294716602

delta.eta <- glht(fit.lin, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.887050910563699
# Lower = 0.851941620025905
# Upper = 0.923607086959739

delta.eta <- glht(fit.lin, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.848200403275435
# Lower = 0.802427996008894
# Upper = 0.896583777852931

delta.eta <- glht(fit.lin, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.811190488673442
# Lower = 0.755965264716157
# Upper = 0.870450058523957

delta.eta <- glht(fit.lin, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.775341572511504
# Lower = 0.711635849064092
# Upper = 0.844750239683991

delta.eta <- glht(fit.lin, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.7360927056443
# Lower = 0.663889031079858
# Upper = 0.816149154357047

delta.eta <- glht(fit.lin, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.69018136358277
# Lower = 0.609118753648786
# Upper = 0.782031930199989

delta.eta <- glht(fit.lin, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.627096339315038
# Lower = 0.535853597440108
# Upper = 0.733875485134305

# 80th - INTERACTION TERM IS VERY LAST COEF HERE
k0 <- matrix(c(0, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0), nrow=1)
k1 <- matrix(c(4.185216, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-1.788513), nrow=1)
k2 <- matrix(c(7.317594, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-3.127105), nrow=1)
k3 <- matrix(c(10.05196, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-4.295611), nrow=1)
k4 <- matrix(c(12.77586, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-5.459644), nrow=1)
k5 <- matrix(c(15.53549, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-6.638946), nrow=1)
k6 <- matrix(c(18.70714, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-7.99432), nrow=1)
k7 <- matrix(c(22.63917, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-9.674636), nrow=1)
k8 <- matrix(c(28.49153, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-12.17559), nrow=1)

delta.eta <- glht(fit.lin, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.899158307400492
# Lower = 0.829230611594708
# Upper = 0.974982894339254


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.83331573442513
# Lower = 0.778095887855979
# Upper = 0.892454418637186

delta.eta <- glht(fit.lin, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.78721110185037
# Lower = 0.737203981332307
# Upper = 0.840610379988078

delta.eta <- glht(fit.lin, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.749054898671932
# Lower = 0.699627865493837
# Upper = 0.801973833372651

delta.eta <- glht(fit.lin, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.712883688183946
# Lower = 0.661237557578711
# Upper = 0.768563653189422

delta.eta <- glht(fit.lin, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.678018824835348
# Lower = 0.622417993861431
# Upper = 0.738586498727496

delta.eta <- glht(fit.lin, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.640049372196467
# Lower = 0.578993672649275
# Upper = 0.707543481390072

delta.eta <- glht(fit.lin, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.595915732438379
# Lower = 0.527989690835147
# Upper = 0.672580480891334

delta.eta <- glht(fit.lin, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.535799643209665
# Lower = 0.458948432177408
# Upper = 0.625519639105408


# 70th
k0 <- matrix(c(0, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(4.185216, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -3.090338), nrow=1)
k2 <- matrix(c(7.317594, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -5.403268), nrow=1)
k3 <- matrix(c(10.05196, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -7.422307), nrow=1)
k4 <- matrix(c(12.77586, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -9.433618), nrow=1)
k5 <- matrix(c(15.53549, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -11.471313), nrow=1)
k6 <- matrix(c(18.70714, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -13.813240), nrow=1)
k7 <- matrix(c(22.63917, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -16.716627), nrow=1)
k8 <- matrix(c(28.49153, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -21.037975), nrow=1)

delta.eta <- glht(fit.lin, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.832212716839797
# Lower = 0.723570053697588
# Upper = 0.957167868585308


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.767074500110636
# Lower = 0.680575616722269
# Upper = 0.864567102115413

delta.eta <- glht(fit.lin, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.721680887380396
# Lower = 0.647611584451399
# Upper = 0.804221721344519

delta.eta <- glht(fit.lin, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.684256738546701
# Lower = 0.61788766463674
# Upper = 0.757754703715974

delta.eta <- glht(fit.lin, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.648905535924485
# Lower = 0.587244898502874
# Upper = 0.71704053219865

delta.eta <- glht(fit.lin, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.614952659263887
# Lower = 0.555301852432251
# Upper = 0.681011186041134

delta.eta <- glht(fit.lin, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.578118940482738
# Lower = 0.518081921379195
# Upper = 0.645113244745438

delta.eta <- glht(fit.lin, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.5355027888853
# Lower = 0.472515362993854
# Upper = 0.60688658901375

delta.eta <- glht(fit.lin, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.477820664597211
# Lower = 0.408678881257958
# Upper = 0.558660106960628



# 60th
k0 <- matrix(c(0, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(4.185216, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-4.190807), nrow=1)
k2 <- matrix(c(7.317594, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-7.327370), nrow=1)
k3 <- matrix(c(10.05196, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-10.065389), nrow=1)
k4 <- matrix(c(12.77586, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-12.792929), nrow=1)
k5 <- matrix(c(15.53549, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-15.556245), nrow=1)
k6 <- matrix(c(18.70714, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-18.732133), nrow=1)
k7 <- matrix(c(22.63917, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-22.669416), nrow=1)
k8 <- matrix(c(28.49153, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,-28.529595), nrow=1)

delta.eta <- glht(fit.lin, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.779524573476422
# Lower = 0.64482466364807
# Upper = 0.942362466745291

delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.715203106882961
# Lower = 0.60726334898112
# Upper = 0.842328925256682


delta.eta <- glht(fit.lin, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.670559729021847
# Lower = 0.578918780712806
# Upper = 0.776707139526914


delta.eta <- glht(fit.lin, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.633873076417693
# Lower = 0.553690164799049
# Upper = 0.725667715540974


delta.eta <- glht(fit.lin, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.599322619394492
# Lower = 0.52785792756022
# Upper = 0.680462646034426


delta.eta <- glht(fit.lin, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.566238951167274
# Lower = 0.500830233535772
# Upper = 0.64019008508222


delta.eta <- glht(fit.lin, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.5304651635862
# Lower = 0.468793218173756
# Upper = 0.600250342517192


delta.eta <- glht(fit.lin, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.489236668926624
# Lower = 0.428386546507597
# Upper = 0.558730240652351


delta.eta <- glht(fit.lin, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.433730989833276
# Lower = 0.369733459134557
# Upper = 0.508805916516444





# 50th
k0 <- matrix(c(0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(4.185216, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -5.247302), nrow=1)
k2 <- matrix(c(7.317594, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -9.174587), nrow=1)
k3 <- matrix(c(10.05196, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -12.602856), nrow=1)
k4 <- matrix(c(12.77586, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -16.018003), nrow=1)
k5 <- matrix(c(15.53549, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -19.477947), nrow=1)
k6 <- matrix(c(18.70714, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -23.454470), nrow=1)
k7 <- matrix(c(22.63917, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -28.384335), nrow=1)
k8 <- matrix(c(28.49153, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -35.721854), nrow=1)

delta.eta <- glht(fit.lin, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.73208296373179
# Lower = 0.577300882516053
# Upper = 0.928364189312352

delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.668707709192316
# Lower = 0.544201089971535
# Upper = 0.821699935140932

delta.eta <- glht(fit.lin, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.624891682180723
# Lower = 0.519432061385778
# Upper = 0.751762633628887

delta.eta <- glht(fit.lin, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.588996682247568
# Lower = 0.497559504140783
# Upper = 0.697237393339962


delta.eta <- glht(fit.lin, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.555289281845674
# Lower = 0.475305161931148
# Upper = 0.648733090294843

delta.eta <- glht(fit.lin, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.523106380476058
# Lower = 0.452070894524585
# Upper = 0.605303921595157

delta.eta <- glht(fit.lin, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.488415496258834
# Lower = 0.424357969653597
# Upper = 0.562142610825694


delta.eta <- glht(fit.lin, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.448584620580917
# Lower = 0.388722537877016
# Upper = 0.517665281052959

delta.eta <- glht(fit.lin, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.395235595459373
# Lower = 0.335275467955771
# Upper = 0.465918896096305





# 40th
k0 <- matrix(c(0, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(4.185216, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -6.273844), nrow=1)
k2 <- matrix(c(7.317594, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -10.969432), nrow=1)
k3 <- matrix(c(10.05196, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -15.068381), nrow=1)
k4 <- matrix(c(12.77586, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -19.151640), nrow=1)
k5 <- matrix(c(15.53549, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -23.288461), nrow=1)
k6 <- matrix(c(18.70714, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -28.042920), nrow=1)
k7 <- matrix(c(22.63917, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -33.937225), nrow=1)
k8 <- matrix(c(28.49153, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -42.710200), nrow=1)

delta.eta <- glht(fit.lin, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.688753679347484
# Lower = 0.51847140513658
# Upper = 0.914961994267999


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.626427686133571
# Lower = 0.48915682006301
# Upper = 0.8022205351325

delta.eta <- glht(fit.lin, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.583499534936674
# Lower = 0.467326879065812
# Upper = 0.728551518269007

delta.eta <- glht(fit.lin, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.548437972565635
# Lower = 0.448144121047388
# Upper = 0.671177408394695

delta.eta <- glht(fit.lin, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.51560548947498
# Lower = 0.42871634034477
# Upper = 0.620104707375838

delta.eta <- glht(fit.lin, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.48434617458493
# Lower = 0.408493597070512
# Upper = 0.574283706078656

delta.eta <- glht(fit.lin, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.450753293879641
# Lower = 0.384333694240197
# Upper = 0.52865136465596

delta.eta <- glht(fit.lin, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.412323327121288
# Lower = 0.352899649819751
# Upper = 0.481753173105171

delta.eta <- glht(fit.lin, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.36110712064798
# Lower = 0.304433061735695
# Upper = 0.428331771323462





# 30th
k0 <- matrix(c(0, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(4.185216, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -7.379398), nrow=1)
k2 <- matrix(c(7.317594, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -12.902426), nrow=1)
k3 <- matrix(c(10.05196, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -17.723676), nrow=1)
k4 <- matrix(c(12.77586, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -22.526473), nrow=1)
k5 <- matrix(c(15.53549, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -27.392269), nrow=1)
k6 <- matrix(c(18.70714, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -32.984541), nrow=1)
k7 <- matrix(c(22.63917, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -39.917520), nrow=1)
k8 <- matrix(c(28.49153, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -50.236437), nrow=1)

delta.eta <- glht(fit.lin, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.644953112386684
# Lower = 0.461800762194558
# Upper = 0.900744544466609


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.583878227499786
# Lower = 0.436059012634748
# Upper = 0.781806532304948

delta.eta <- glht(fit.lin, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.541982591710982
# Lower = 0.416950117658997
# Upper = 0.704509046230785

delta.eta <- glht(fit.lin, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.5078754869646
# Lower = 0.400214589794422
# Upper = 0.644498018905369

delta.eta <- glht(fit.lin, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.476033164089937
# Lower = 0.38332341450652
# Upper = 0.591165487778004

delta.eta <- glht(fit.lin, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.445808466581279
# Lower = 0.365790129085048
# Upper = 0.543331197516656

delta.eta <- glht(fit.lin, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.413433769575361
# Lower = 0.344845673695517
# Upper = 0.495663697890014

delta.eta <- glht(fit.lin, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.376542357869913
# Lower = 0.317385124837357
# Upper = 0.446725873945387

delta.eta <- glht(fit.lin, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.327640288240014
# Lower = 0.273992618609778
# Upper = 0.391792154922558



# 20th
k0 <- matrix(c(0, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(4.185216, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -8.673781), nrow=1)
k2 <- matrix(c(7.317594, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -15.165575), nrow=1)
k3 <- matrix(c(10.05196, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -20.832496), nrow=1)
k4 <- matrix(c(12.77586, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -26.477727), nrow=1)
k5 <- matrix(c(15.53549, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -32.197008), nrow=1)
k6 <- matrix(c(18.70714, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -38.770192), nrow=1)
k7 <- matrix(c(22.63917, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -46.919250), nrow=1)
k8 <- matrix(c(28.49153, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -59.048155), nrow=1)

delta.eta <- glht(fit.lin, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.597198135062124
# Lower = 0.403272252196333
# Upper = 0.884379251434454


delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.537719650070872
# Lower = 0.381151587214113
# Upper = 0.758602172394772

delta.eta <- glht(fit.lin, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.497113020782465
# Lower = 0.364763804356437
# Upper = 0.677483216481609

delta.eta <- glht(fit.lin, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.46418098536461
# Lower = 0.350444120640915
# Upper = 0.614831222678257

delta.eta <- glht(fit.lin, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.43354431029498
# Lower = 0.336027431709366
# Upper = 0.559361085590534

delta.eta <- glht(fit.lin, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.404567190296791
# Lower = 0.32109722025241
# Upper = 0.509735373404908

delta.eta <- glht(fit.lin, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.373647736064063
# Lower = 0.30327472566842
# Upper = 0.460350365029898

delta.eta <- glht(fit.lin, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.33857597651199
# Lower = 0.279781864190551
# Upper = 0.40972524149376

delta.eta <- glht(fit.lin, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.292377648914687
# Lower = 0.241805654113859
# Upper = 0.353526429719578




# 10th
k0 <- matrix(c(0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(4.185216, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -10.49544), nrow=1)
k2 <- matrix(c(7.317594, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -18.35064), nrow=1)
k3 <- matrix(c(10.05196, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -25.20772), nrow=1)
k4 <- matrix(c(12.77586, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -32.03856), nrow=1)
k5 <- matrix(c(15.53549, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -38.95900), nrow=1)
k6 <- matrix(c(18.70714, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -46.91268), nrow=1)
k7 <- matrix(c(22.63917, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -56.77320), nrow=1)
k8 <- matrix(c(28.49153, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -71.44941), nrow=1)

delta.eta <- glht(fit.lin, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.535918804368309
# Lower = 0.333247102470173
# Upper = 0.861849848795805

delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.478872369440488
# Lower = 0.315368293006517
# Upper = 0.727145852321965

delta.eta <- glht(fit.lin, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.440186447614521
# Lower = 0.302135937461292
# Upper = 0.641314337816286

delta.eta <- glht(fit.lin, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.408979896973897
# Lower = 0.290588928473839
# Upper = 0.575605399033079

delta.eta <- glht(fit.lin, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.380092654399287
# Lower = 0.278982631402095
# Upper = 0.517847384269854

delta.eta <- glht(fit.lin, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.352906537452136
# Lower = 0.266982508464119
# Upper = 0.466483834064335

delta.eta <- glht(fit.lin, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.324054413374771
# Lower = 0.252665526240352
# Upper = 0.415613734054772

delta.eta <- glht(fit.lin, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.29153834154058
# Lower = 0.233705587684314
# Upper = 0.363682381026515

delta.eta <- glht(fit.lin, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.24908401029571
# Lower = 0.202314232229641
# Upper = 0.306665742203298





# Change ENMO from 10th to 90th percentile w/ genetic risk at 90th
21.43 - 18.75 # 2.68
23.46 - 18.75 # 4.71
25.27 - 18.75 # 6.52
27.07 - 18.75 # 8.32
28.97 - 18.75 # 10.22
31.18 - 18.75 # 12.43
33.97 - 18.75 # 15.22
38.29 - 18.75 # 19.54


# ----
# ENMO
# ----

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
# Estimate = 0.935237805685561
# Lower = 0.915089045151112
# Upper = 0.955830208894159


delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.888989426061393
# Lower = 0.855604946358811
# Upper = 0.923676520352349

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.849685353225155
# Lower = 0.805836014041786
# Upper = 0.895920741819711

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.812321912303313
# Lower = 0.759213378411461
# Upper = 0.869145497131232

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.774663701944539
# Lower = 0.712923774079496
# Upper = 0.841750370697416

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.733051947122395
# Lower = 0.662620313752849
# Upper = 0.81096994164952

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.683696423624828
# Lower = 0.60415221790656
# Upper = 0.773713620214958

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.613749845207114
# Lower = 0.523633719429072
# Upper = 0.719374743289007

# 80th
k0 <- matrix(c(0, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(2.68, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -1.145273), nrow=1)
k2 <- matrix(c(4.71, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -2.012774), nrow=1)
k3 <- matrix(c(6.52, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -2.786261), nrow=1)
k4 <- matrix(c(8.32, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -3.555474), nrow=1)
k5 <- matrix(c(10.22, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -4.367421), nrow=1)
k6 <- matrix(c(12.43, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -5.311844), nrow=1)
k7 <- matrix(c(15.22, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -6.504124), nrow=1)
k8 <- matrix(c(19.54, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -8.350235), nrow=1)

delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.908373374901182
# Lower = 0.835310350023158
# Upper = 0.987827085114518


delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.842922216797118
# Lower = 0.784704313885354
# Upper = 0.905459357107568

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.796503091359257
# Lower = 0.744393828795581
# Upper = 0.852260120924606

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.757274689863236
# Lower = 0.706847035053145
# Upper = 0.811299938273552

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.720179246470386
# Lower = 0.668558124673933
# Upper = 0.775786170124866

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.682992505375638
# Lower = 0.628092452484844
# Upper = 0.742691240045664

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.64214723536436
# Lower = 0.582178314782439
# Upper = 0.708293423880256

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.594052346962953
# Lower = 0.52732175915032
# Upper = 0.669227439999484

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.526591568698943
# Lower = 0.450729900447933
# Upper = 0.615221399665822


# 70th
k0 <- matrix(c(0, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(2.68, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -1.978896), nrow=1)
k2 <- matrix(c(4.71, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -3.477836), nrow=1)
k3 <- matrix(c(6.52, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -4.814329), nrow=1)
k4 <- matrix(c(8.32, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -6.143438), nrow=1)
k5 <- matrix(c(10.22, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -7.546387), nrow=1)
k6 <- matrix(c(12.43, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -9.178237), nrow=1)
k7 <- matrix(c(15.22, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -11.238357), nrow=1)
k8 <- matrix(c(19.54, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -14.428219), nrow=1)

delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.847004688371248
# Lower = 0.732761009759993
# Upper = 0.979059928908959


delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.781510623531257
# Lower = 0.689418573953655
# Upper = 0.885904264501685

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.735293728278793
# Lower = 0.656213210519617
# Upper = 0.823904271018886

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.696395481016624
# Lower = 0.625987200828715
# Upper = 0.774722974109295

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.659753102204451
# Lower = 0.59516969834722
# Upper = 0.731344618311631

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.623164632034107
# Lower = 0.56187058804657
# Upper = 0.691145197630487

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.583151402936323
# Lower = 0.522634692601132
# Upper = 0.650675440342678

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.536285236760966
# Lower = 0.473687889363344
# Upper = 0.607154756593658

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.471039180200156
# Lower = 0.402674558765273
# Upper = 0.55101049831403


# 60th
k0 <- matrix(c(0, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(2.68, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -2.683580), nrow=1)
k2 <- matrix(c(4.71, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -4.716293), nrow=1)
k3 <- matrix(c(6.52, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -6.528711), nrow=1)
k4 <- matrix(c(8.32, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -8.331116), nrow=1)
k5 <- matrix(c(10.22, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -10.233654), nrow=1)
k6 <- matrix(c(12.43, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -12.446606), nrow=1)
k7 <- matrix(c(15.22, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -15.240334), nrow=1)
k8 <- matrix(c(19.54, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -19.566105), nrow=1)

delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.798373223535987
# Lower = 0.655957142124543
# Upper = 0.971709526623652


delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.733100757187776
# Lower = 0.617549375045335
# Upper = 0.870273280010746

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.68723537774897
# Lower = 0.588545948213499
# Upper = 0.802473393731432

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.648766082646237
# Lower = 0.562482477897502
# Upper = 0.748285407156883

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.612645125259623
# Lower = 0.536150897888521
# Upper = 0.700053009297432

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.576696949336557
# Lower = 0.507738799991987
# Upper = 0.655020595982305

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.537528608219602
# Lower = 0.47388248211278
# Upper = 0.60972290717794

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.491857040751331
# Lower = 0.430513470975399
# Upper = 0.561941413792556

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.428675757673535
# Lower = 0.365217720511856
# Upper = 0.503159882164079




# 50th
k0 <- matrix(c(0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(2.68, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -3.360106), nrow=1)
k2 <- matrix(c(4.71, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -5.905261), nrow=1)
k3 <- matrix(c(6.52, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -8.174587), nrow=1)
k4 <- matrix(c(8.32, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -10.431375), nrow=1)
k5 <- matrix(c(10.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -12.813540), nrow=1)
k6 <- matrix(c(12.43, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -15.584374), nrow=1)
k7 <- matrix(c(15.22, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -19.082395), nrow=1)
k8 <- matrix(c(19.54, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -24.498685), nrow=1)


delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.754314163485706
# Lower = 0.58980724097701
# Upper = 0.964704767429803


delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.689449035945972
# Lower = 0.55550770496915
# Upper = 0.855685652808771

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.644055331261674
# Lower = 0.529807476493063
# Upper = 0.782939630207381

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.606107688809086
# Lower = 0.506888241621021
# Upper = 0.724748574278738

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.570587321372079
# Lower = 0.48389605513337
# Upper = 0.672809558699195

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.535349434732746
# Lower = 0.459203193820441
# Upper = 0.624122438879939

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.497090646254217
# Lower = 0.429730770357376
# Upper = 0.575009116493893

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.452671022688323
# Lower = 0.391431374824445
# Upper = 0.523491646201313

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.391594222328297
# Lower = 0.331913393114796
# Upper = 0.462006168301462





# 40th
k0 <- matrix(c(0, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(2.68, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -4.017451), nrow=1)
k2 <- matrix(c(4.71, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -7.060521), nrow=1)
k3 <- matrix(c(6.52, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -9.773799), nrow=1)
k4 <- matrix(c(8.32, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -12.472088), nrow=1)
k5 <- matrix(c(10.22, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -15.320281), nrow=1)
k6 <- matrix(c(12.43, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -18.633179), nrow=1)
k7 <- matrix(c(15.22, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -22.815526), nrow=1)
k8 <- matrix(c(19.54, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -29.291417), nrow=1)


delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.713834510241616
# Lower = 0.531928927170248
# Upper = 0.957946977470544


delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.649526048825748
# Lower = 0.501158970282702
# Upper = 0.841816894677561

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.604699859661977
# Lower = 0.478211955526254
# Upper = 0.764644037125374

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.567347202160989
# Lower = 0.457846604934785
# Upper = 0.703036441311497

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.532489381055475
# Lower = 0.437517243894813
# Upper = 0.648077178427765

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.49801577690223
# Lower = 0.415779337054267
# Upper = 0.596517652369916

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.460715265166906
# Lower = 0.389873961335022
# Upper = 0.544428652867682

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.417588718846545
# Lower = 0.355976020764749
# Upper = 0.489865406476748

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.358639109002542
# Lower = 0.301978012546267
# Upper = 0.425931707482942





# 30th
k0 <- matrix(c(0, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(2.68, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -4.725392), nrow=1)
k2 <- matrix(c(4.71, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -8.304700), nrow=1)
k3 <- matrix(c(6.52, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -11.496103), nrow=1)
k4 <- matrix(c(8.32, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -14.669874), nrow=1)
k5 <- matrix(c(10.22, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -18.019965), nrow=1)
k6 <- matrix(c(12.43, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -21.916651), nrow=1)
k7 <- matrix(c(15.22, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -26.835995), nrow=1)
k8 <- matrix(c(19.54, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -34.453045), nrow=1)

delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.672665303957697
# Lower = 0.475931578699317
# Upper = 0.950721976434276


delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.609111820481352
# Lower = 0.448535292110605
# Upper = 0.827175065989269

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.565000550943879
# Lower = 0.428171502777808
# Upper = 0.745555508705919

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.528370957372404
# Lower = 0.410161219812889
# Upper = 0.68064910847005

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.494299115001104
# Lower = 0.392250101544142
# Upper = 0.622897519029396

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.460715099393599
# Lower = 0.373170223762923
# Upper = 0.568797801359689

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.424511418673041
# Lower = 0.350489801437385
# Upper = 0.514166015229952

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.382840845544665
# Lower = 0.320718030879816
# Upper = 0.456996797514878

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.326242409255567
# Lower = 0.272322046724928
# Upper = 0.390839121829846




# 20th
k0 <- matrix(c(0, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(2.68, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -5.554249), nrow=1)
k2 <- matrix(c(4.71, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -9.761386), nrow=1)
k3 <- matrix(c(6.52, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -13.512576), nrow=1)
k4 <- matrix(c(8.32, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -17.243042), nrow=1)
k5 <- matrix(c(10.22, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -21.180756), nrow=1)
k6 <- matrix(c(12.43, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -25.760939), nrow=1)
k7 <- matrix(c(15.22, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -31.543161), nrow=1)
k8 <- matrix(c(19.54, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -40.496279), nrow=1)


delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.627471681307703
# Lower = 0.417815209929034
# Upper = 0.942332163805118



delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.564978917738505
# Lower = 0.393886338347367
# Upper = 0.810389054944754

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.521820108167335
# Lower = 0.376143296064483
# Upper = 0.723916199322848

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.486126769620302
# Lower = 0.360492000788306
# Upper = 0.655546407755788

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.453052225758863
# Lower = 0.344972880362254
# Upper = 0.594992623911541

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.420578366063099
# Lower = 0.328494415500072
# Upper = 0.538475400657967

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.38572285487524
# Lower = 0.308960777217813
# Upper = 0.481556662670539

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.345814665244626
# Lower = 0.28329374379982
# Upper = 0.422133510942463

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.292011774219745
# Lower = 0.240851418774144
# Upper = 0.354039335607673




# 10th
k0 <- matrix(c(0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, 0), nrow=1)
k1 <- matrix(c(2.68, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -6.720749), nrow=1)
k2 <- matrix(c(4.71, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -11.811465), nrow=1)
k3 <- matrix(c(6.52, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -16.350478), nrow=1)
k4 <- matrix(c(8.32, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -20.864413), nrow=1)
k5 <- matrix(c(10.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -25.629123), nrow=1)
k6 <- matrix(c(12.43, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -31.171233), nrow=1)
k7 <- matrix(c(15.22, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -38.167833), nrow=1)
k8 <- matrix(c(19.54, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -49.001279), nrow=1)


delta.eta <- glht(fit.linENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.568964267075929
# Lower = 0.347843285105916
# Upper = 0.93064995378789


delta.eta <- glht(fit.linENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.508231005784134
# Lower = 0.328050235702003
# Upper = 0.787375612419885

delta.eta <- glht(fit.linENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.466581411196047
# Lower = 0.313401646827873
# Upper = 0.694630087228801

delta.eta <- glht(fit.linENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.432332666876946
# Lower = 0.300507193469842
# Upper = 0.621986890532425

delta.eta <- glht(fit.linENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.400766671771865
# Lower = 0.287753198080139
# Upper = 0.55816556088585

delta.eta <- glht(fit.linENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.36994391993303
# Lower = 0.274249626520005
# Upper = 0.499028952680936

delta.eta <- glht(fit.linENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.33706179001897
# Lower = 0.258288339881227
# Upper = 0.439859772001459

delta.eta <- glht(fit.linENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.299690873097074
# Lower = 0.237319870509669
# Upper = 0.378453853125742

delta.eta <- glht(fit.linENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.249833125698116
# Lower = 0.202088543537716
# Upper = 0.308857640336459


# -----
# Percent MVPA
# -----

# Change Percent MVPA from 10th to 90th percentile w/ genetic risk at 90th
0.259328225722848 - 0.211577691492217 # 0.04775053
0.295122000635783 - 0.211577691492217 # 0.08354431
0.326224308037015 - 0.211577691492217 # 0.1146466
0.355539884843425 - 0.211577691492217 # 0.1439622
0.385086263660446 - 0.211577691492217 # 0.1735086
0.417018807736335 - 0.211577691492217 # 0.2054411
0.454333631101671 - 0.211577691492217 # 0.2427559
0.505831374916045 - 0.211577691492217 # 0.2942537



# 90th
k1 <- matrix(c(0, 0.04775053, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k2 <- matrix(c(0, 0.08354431, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k3 <- matrix(c(0, 0.1146466, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k4 <- matrix(c(0, 0.1439622, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k5 <- matrix(c(0, 0.1735086, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k6 <- matrix(c(0, 0.2054411, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k7 <- matrix(c(0, 0.2427559, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k8 <- matrix(c(0, 0.2942537, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.904669502068217
# Lower = 0.87440000345858
# Upper = 0.935986853539763

delta.eta <- glht(fit.linMVPA, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.839218080376575
# Lower = 0.790707941550717
# Upper = 0.890704328895083

delta.eta <- glht(fit.linMVPA, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.78620278296787
# Lower = 0.72451774868763
# Upper = 0.853139646428343

delta.eta <- glht(fit.linMVPA, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.739302789616817
# Lower = 0.667210694399156
# Upper = 0.819184433528018

delta.eta <- glht(fit.linMVPA, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.694863995527874
# Lower = 0.614037970058903
# Upper = 0.786329178038687

delta.eta <- glht(fit.linMVPA, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.649834958382933
# Lower = 0.561325413155662
# Upper = 0.752300649925222

delta.eta <- glht(fit.linMVPA, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.600899727319459
# Lower = 0.505433380579436
# Upper = 0.714397774596233

delta.eta <- glht(fit.linMVPA, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.539358419484876
# Lower = 0.43732038122053
# Upper = 0.66520454376565

# 80th
k0 <- matrix(c(0, 0, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 0.04775053, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -0.02040574), nrow=1)
k2 <- matrix(c(0, 0.08354431, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -0.03570188), nrow=1)
k3 <- matrix(c(0, 0.1146466, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -0.04899315), nrow=1)
k4 <- matrix(c(0, 0.1439622, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -0.06152089), nrow=1)
k5 <- matrix(c(0, 0.1735086, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -0.07414727), nrow=1)
k6 <- matrix(c(0, 0.2054411, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -0.08779332), nrow=1)
k7 <- matrix(c(0, 0.2427559, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -0.10373945), nrow=1)
k8 <- matrix(c(0, 0.2942537, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -0.12574655), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.883909992469233
# Lower = 0.823776579467823
# Upper = 0.948432978383161


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.794074436314981
# Lower = 0.745488849806061
# Upper = 0.845826480936628

delta.eta <- glht(fit.linMVPA, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.732773454945926
# Lower = 0.682168163880155
# Upper = 0.787132799072873

delta.eta <- glht(fit.linMVPA, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.683363013237273
# Lower = 0.627012944222932
# Upper = 0.744777300314699

delta.eta <- glht(fit.linMVPA, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.639845119017292
# Lower = 0.577061288557809
# Upper = 0.709459782605465

delta.eta <- glht(fit.linMVPA, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.598788250771774
# Lower = 0.529682478184438
# Upper = 0.676910005577856

delta.eta <- glht(fit.linMVPA, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.557372746169737
# Lower = 0.482199398193252
# Upper = 0.644265379294996

delta.eta <- glht(fit.linMVPA, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.512591694756355
# Lower = 0.4316254422259
# Upper = 0.60874596311604

delta.eta <- glht(fit.linMVPA, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.456637872915123
# Lower = 0.370022318408373
# Upper = 0.563528567350682


# 70th
k0 <- matrix(c(0, 0, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 0.04775053, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.03525870), nrow=1)
k2 <- matrix(c(0, 0.08354431, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.06168862), nrow=1)
k3 <- matrix(c(0, 0.11464660, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.08465436), nrow=1)
k4 <- matrix(c(0, 0.14396220, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.10630082), nrow=1)
k5 <- matrix(c(0, 0.17350860, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.12811771), nrow=1)
k6 <- matrix(c(0, 0.20544110, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.15169648), nrow=1)
k7 <- matrix(c(0, 0.24275590, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.17924950), nrow=1)
k8 <- matrix(c(0, 0.29425370, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.21727517), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.807977820912276
# Lower = 0.715366623146933
# Upper = 0.91257844294486


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.722174577863232
# Lower = 0.651019685575305
# Upper = 0.801106529445506

delta.eta <- glht(fit.linMVPA, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.663886416258903
# Lower = 0.600716214098112
# Upper = 0.733699479636661

delta.eta <- glht(fit.linMVPA, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.617071900578817
# Lower = 0.555832512648904
# Upper = 0.685058397662453

delta.eta <- glht(fit.linMVPA, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.575972988347045
# Lower = 0.513571941513558
# Upper = 0.645956012175692

delta.eta <- glht(fit.linMVPA, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.537319739456975
# Lower = 0.47214623027374
# Upper = 0.611489585001498

delta.eta <- glht(fit.linMVPA, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.498456276599187
# Lower = 0.429633671283694
# Upper = 0.578303508984202

delta.eta <- glht(fit.linMVPA, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.456589131975932
# Lower = 0.383633492524045
# Upper = 0.543418756446216

delta.eta <- glht(fit.linMVPA, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.404521913165553
# Lower = 0.327075537969474
# Upper = 0.500306379520169



# 60th
k0 <- matrix(c(0, 0, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 0.04775053, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.04781432), nrow=1)
k2 <- matrix(c(0, 0.08354431, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.08365593), nrow=1)
k3 <- matrix(c(0, 0.11464660, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.1147997), nrow=1)
k4 <- matrix(c(0, 0.14396220, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.14415453), nrow=1)
k5 <- matrix(c(0, 0.17350860, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.17374041), nrow=1)
k6 <- matrix(c(0, 0.20544110, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.20571557), nrow=1)
k7 <- matrix(c(0, 0.24275590, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.24308022), nrow=1)
k8 <- matrix(c(0, 0.29425370, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.29464682), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.748900969617589
# Lower = 0.634930740807611
# Upper = 0.883328883368884


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.666497653304514
# Lower = 0.579461060975936
# Upper = 0.766607373258635

delta.eta <- glht(fit.linMVPA, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.610730437915709
# Lower = 0.53696158880635
# upper = 0.694633797970285

delta.eta <- glht(fit.linMVPA, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.566075684173546
# Lower = 0.499091256425956
# Upper = 0.642050278554796

delta.eta <- glht(fit.linMVPA, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.526979459930429
# Lower = 0.462892444680389
# Upper = 0.599939261009787

delta.eta <- glht(fit.linMVPA, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.490307105797832
# Lower = 0.426608052579763
# Upper = 0.563517393875021

delta.eta <- glht(fit.linMVPA, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.453537197050114
# Lower = 0.388540554157875
# Upper = 0.529406742505685

delta.eta <- glht(fit.linMVPA, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.414048530554015
# Lower = 0.346597055834898
# Upper = 0.494626779910107

delta.eta <- glht(fit.linMVPA, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.365134249587308
# Lower = 0.294356372978822
# Upper = 0.452930639389551




# 50th
k0 <- matrix(c(0, 0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 0.04775053, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.05986823), nrow=1)
k2 <- matrix(c(0, 0.08354431, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.10474543), nrow=1)
k3 <- matrix(c(0, 0.11464660, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.14374058), nrow=1)
k4 <- matrix(c(0, 0.14396220, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.18049563), nrow=1)
k5 <- matrix(c(0, 0.17350860, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.21754005), nrow=1)
k6 <- matrix(c(0, 0.20544110, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.25757609), nrow=1)
k7 <- matrix(c(0, 0.24275590, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.30436031), nrow=1)
k8 <- matrix(c(0, 0.29425370, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.36892676), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.696252886644511
# Lower = 0.566231497561464
# Upper = 0.856130547750379


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.617088396125109
# Lower = 0.517872875229795
# Upper = 0.735311901522721

delta.eta <- glht(fit.linMVPA, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.563707282707298
# Lower = 0.481274486210761
# Upper = 0.660259186143703

delta.eta <- glht(fit.linMVPA, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.521086881411358
# Lower = 0.44881608514686
# Upper = 0.604995112619828

delta.eta <- glht(fit.linMVPA, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.483869255423711
# Lower = 0.417620152571538
# Upper = 0.560627773594307

delta.eta <- glht(fit.linMVPA, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.449047775284095
# Lower = 0.385902208838591
# Upper = 0.522525914258074


delta.eta <- glht(fit.linMVPA, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.41422628594271
# Lower = 0.351995313331514
# Upper = 0.487459376495425

delta.eta <- glht(fit.linMVPA, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.376941689007672
# Lower = 0.31393027680019
# Upper = 0.452600616799987

delta.eta <- glht(fit.linMVPA, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.330933597928433
# Lower = 0.26578451125443
# Upper = 0.412052025608894





# 40th
k0 <- matrix(c(0, 0, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 0.04775053, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.07158038), nrow=1)
k2 <- matrix(c(0, 0.08354431, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.12523701), nrow=1)
k3 <- matrix(c(0, 0.11464660, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.17186087), nrow=1)
k4 <- matrix(c(0, 0.14396220, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.21580639), nrow=1)
k5 <- matrix(c(0, 0.17350860, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.26009789), nrow=1)
k6 <- matrix(c(0, 0.20544110, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.30796628), nrow=1)
k7 <- matrix(c(0, 0.24275590, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.36390299), nrow=1)
k8 <- matrix(c(0, 0.29425370, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.44110071), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.648645142496382
# Lower = 0.506607596205905
# Upper = 0.830505748502726


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.572591031474354
# Lower = 0.464182854458459
# Upper = 0.706317534514204

delta.eta <- glht(fit.linMVPA, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.521487921573142
# Lower = 0.432322003692936
# Upper = 0.629044207844281

delta.eta <- glht(fit.linMVPA, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.480801091502869
# Lower = 0.404188102539815
# Upper = 0.571935908399428

delta.eta <- glht(fit.linMVPA, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.445362092836211
# Lower = 0.377107068677283
# Upper = 0.525971031068605

delta.eta <- glht(fit.linMVPA, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.412286639008562
# Lower = 0.349332768019886
# Upper = 0.486585537533371

delta.eta <- glht(fit.linMVPA, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.379296432334819
# Lower = 0.319199298879767
# Upper = 0.450708332025854

delta.eta <- glht(fit.linMVPA, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.344075067131085
# Lower = 0.284766904851502
# Upper = 0.415735290177054

delta.eta <- glht(fit.linMVPA, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.300773873486383
# Lower = 0.240485137645416
# Upper = 0.376176772742558





# 30th
k0 <- matrix(c(0, 0, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 0.04775053, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.08419402), nrow=1)
k2 <- matrix(c(0, 0.08354431, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.14730583), nrow=1)
k3 <- matrix(c(0, 0.11464660, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.20214557), nrow=1)
k4 <- matrix(c(0, 0.14396220, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.25383501), nrow=1)
k5 <- matrix(c(0, 0.17350860, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.30593140), nrow=1)
k6 <- matrix(c(0, 0.20544110, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.36223498), nrow=1)
k7 <- matrix(c(0, 0.24275590, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.42802866), nrow=1)
k8 <- matrix(c(0, 0.29425370, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.51882989), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.601007307780522
# Lower = 0.449396836304014
# Upper = 0.803765747387762


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.528250557944793
# Lower = 0.412499738095165
# Upper = 0.676482010043381

delta.eta <- glht(fit.linMVPA, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.47954852438161
# Lower = 0.384936637813956
# upper = 0.597414651259371

delta.eta <- glht(fit.linMVPA, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.440890828057111
# Lower = 0.360685597452967
# Upper = 0.538931201127965

delta.eta <- glht(fit.linMVPA, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.407311263297367
# Lower = 0.337340199346666
# Upper = 0.491795716994902

delta.eta <- glht(fit.linMVPA, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.376054603212835
# Lower = 0.313258193884076
# Upper = 0.451439315422649

delta.eta <- glht(fit.linMVPA, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.344965038705548
# Lower = 0.286805604414196
# Upper = 0.414918244614435

delta.eta <- glht(fit.linMVPA, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.311876474210687
# Lower = 0.256046924614618
# Upper = 0.379879333885725

delta.eta <- glht(fit.linMVPA, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.271359512058955
# Lower = 0.215742474216328
# Upper = 0.341314268561871




# 20th
k0 <- matrix(c(0, 0, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 0.04775053, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.09896207), nrow=1)
k2 <- matrix(c(0, 0.08354431, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.17314400), nrow=1)
k3 <- matrix(c(0, 0.11464660, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.23760290), nrow=1)
k4 <- matrix(c(0, 0.14396220, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.29835892), nrow=1)
k5 <- matrix(c(0, 0.17350860, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.35959328), nrow=1)
k6 <- matrix(c(0, 0.20544110, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.42577278), nrow=1)
k7 <- matrix(c(0, 0.24275590, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.50310699), nrow=1)
k8 <- matrix(c(0, 0.29425370, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.60983520), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.549660051678552
# Lower = 0.390570652249655
# Upper = 0.773550625657731


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.480680614474659
# Lower = 0.359206777870034
# Upper = 0.643233556175643

delta.eta <- glht(fit.linMVPA, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.434712120339067
# Lower = 0.335861707562202
# Upper = 0.562656067407414

delta.eta <- glht(fit.linMVPA, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.398353587532852
# Lower = 0.315379000926794
# Upper = 0.503158359415083

delta.eta <- glht(fit.linMVPA, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.366872197885471
# Lower = 0.295668444094787
# Upper = 0.455223451367595

delta.eta <- glht(fit.linMVPA, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.337659816425172
# Lower = 0.275250359609206
# Upper = 0.414219809885645

delta.eta <- glht(fit.linMVPA, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.308698003735181
# Lower = 0.252577784430839
# Upper = 0.37728756598615

delta.eta <- glht(fit.linMVPA, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.277986621051471
# Lower = 0.22573918891769
# upper = 0.342326743770623

delta.eta <- glht(fit.linMVPA, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.240555936147473
# Lower = 0.189793478106744
# Upper = 0.304895399952792




# 10th
k0 <- matrix(c(0, 0, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 0.04775053, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.1197460), nrow=1)
k2 <- matrix(c(0, 0.08354431, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.2095076), nrow=1)
k3 <- matrix(c(0, 0.11464660, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.2875041), nrow=1)
k4 <- matrix(c(0, 0.14396220, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.3610201), nrow=1)
k5 <- matrix(c(0, 0.17350860, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.4351148), nrow=1)
k6 <- matrix(c(0, 0.20544110, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.5151933), nrow=1)
k7 <- matrix(c(0, 0.24275590, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.6087692), nrow=1)
k8 <- matrix(c(0, 0.29425370, -2.507742, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -0.7379124), nrow=1)

delta.eta <- glht(fit.linMVPA, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.484740086377884
# Lower = 0.320589021539966
# Upper = 0.732941353427929


delta.eta <- glht(fit.linMVPA, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.420899401995957
# Lower = 0.295616756328815
# Upper = 0.599276944922238

delta.eta <- glht(fit.linMVPA, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.378621164220356
# Lower = 0.277067619569458
# Upper = 0.517397111284022

delta.eta <- glht(fit.linMVPA, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.345348208295998
# Lower = 0.260821775236839
# Upper = 0.457267744861245

delta.eta <- glht(fit.linMVPA, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.316668115939982
# Lower = 0.245189568498835
# Upper = 0.408984347364087

delta.eta <- glht(fit.linMVPA, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.290171692800923
# Lower = 0.228933186567695
# Upper = 0.367791199543085

delta.eta <- glht(fit.linMVPA, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.264022560040155
# Lower = 0.21068242615111
# Upper = 0.330867236929196

delta.eta <- glht(fit.linMVPA, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.236436230197164
# Lower = 0.188625255551774
# Upper = 0.296365885821172

delta.eta <- glht(fit.linMVPA, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.203034768835318
# Lower = 0.158205039182968
# Upper = 0.260567663134517



# -------
# MVPA Minutes
# for both fit.linMVPAMins and fit.linMVPAMinsENMO
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
k1 <- matrix(c(0, 10.08, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k2 <- matrix(c(0, 20.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k3 <- matrix(c(0, 27.36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k4 <- matrix(c(0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k5 <- matrix(c(0, 44.64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k6 <- matrix(c(0, 56.16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k7 <- matrix(c(0, 69.12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)
k8 <- matrix(c(0, 89.28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0), nrow=1)

delta.eta <- glht(fit.linMVPAMins, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.895603401495614
# Lower = 0.858513528218176
# Upper = 0.934295647542405

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.905264355026039
# Lower = 0.868469638059787
# Upper = 0.943617964942943

delta.eta <- glht(fit.linMVPAMins, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.802105452770515
# Lower = 0.73704547813362
# Upper = 0.872908357016681

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.81950355248071
# Lower = 0.754239512231697
# Upper = 0.890414863763062

delta.eta <- glht(fit.linMVPAMins, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.7413587569564
# Lower = 0.660953363256523
# Upper = 0.831545517535445

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.763266393237414
# Lower = 0.681965797736902
# Upper = 0.854259244347625

delta.eta <- glht(fit.linMVPAMins, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.674504366622275
# Lower = 0.579939451835898
# Upper = 0.784489034419497

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.700852296862016
# Lower = 0.604319448853696
# Upper = 0.812805119789683

delta.eta <- glht(fit.linMVPAMins, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.613678784156147
# Lower = 0.508855520665819
# Upper = 0.74009543933138

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.643541948615544
# Lower = 0.535513654020121
# Upper = 0.773362614601663

delta.eta <- glht(fit.linMVPAMins, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.541023630385249
# Lower = 0.427441750566939
# Upper = 0.684787034132726

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.574350947042688
# Lower = 0.45580160633554
# Upper = 0.723733760003449

delta.eta <- glht(fit.linMVPAMins, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.469516381945248
# Lower = 0.351313383765363
# Upper = 0.627489993555693

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.505362348185354
# Lower = 0.38021725060976
# Upper = 0.671697832104779

delta.eta <- glht(fit.linMVPAMins, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.376601650123367
# Lower = 0.258933940912082
# Upper = 0.547741259319108

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.414146239627892
# Lower = 0.286774873641982
# Upper = 0.598089733663521

# 80th
k0 <- matrix(c(0, 0, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -4.307593), nrow=1)
k2 <- matrix(c(0, 20.16, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -8.615186), nrow=1)
k3 <- matrix(c(0, 27.36, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -11.692039), nrow=1)
k4 <- matrix(c(0, 36, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -15.384262), nrow=1)
k5 <- matrix(c(0, 44.64, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -19.076484), nrow=1)
k6 <- matrix(c(0, 56.16, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -23.999448), nrow=1)
k7 <- matrix(c(0, 69.12, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -29.537782), nrow=1)
k8 <- matrix(c(0, 89.28, -0.4273406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, -38.152969), nrow=1)

delta.eta <- glht(fit.linMVPAMins, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.87525738674591
# Lower = 0.831965864159234
# Upper = 0.92080159301663
delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.875247940031877
# Lower = 0.832072622603678
# Upper = 0.92066357637502

delta.eta <- glht(fit.linMVPAMins, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.779050128193563
# Lower = 0.735838566950608
# Upper = 0.824799255567077

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.787457126445023
# Lower = 0.744261590481186
# Upper = 0.833159649671224

delta.eta <- glht(fit.linMVPAMins, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.693417857911317
# Lower = 0.634656438555416
# Upper = 0.757619865583914

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.708472077028218
# Lower = 0.649430753970187
# Upper = 0.772880989790204

delta.eta <- glht(fit.linMVPAMins, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.638077338166243
# Lower = 0.568074160978773
# Upper = 0.716706932735374

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.656952603149406
# Lower = 0.586131607244105
# Upper = 0.736330744581463


delta.eta <- glht(fit.linMVPAMins, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.577467143805781
# Lower = 0.496326048959512
# Upper = 0.671873464780425

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.600050218090746
# Lower = 0.517203900042016
# Upper = 0.696166955047132

delta.eta <- glht(fit.linMVPAMins, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.522614239227428
# Lower = 0.433192002727019
# Upper = 0.630495580075092

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.548076471605974
# Lower = 0.455904409917817
# Upper = 0.658883336491969

delta.eta <- glht(fit.linMVPAMins, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.457495173346324
# Lower = 0.361041111244218
# Upper = 0.57971745354397

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.485712453452336
# Lower = 0.385014340996836
# Upper = 0.61274753254094

delta.eta <- glht(fit.linMVPAMins, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.393883197450322
# Lower = 0.293968745627791
# Upper = 0.527756693666086

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.423993871414201
# Lower = 0.318171243281529
# Upper = 0.565012730700286

delta.eta <- glht(fit.linMVPAMins, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.312051799604392
# Lower = 0.213406993043873
# Upper = 0.456293977284617

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.343203113722097
# Lower = 0.236364458947807
# Upper = 0.498333708007059

# 70th
k0 <- matrix(c(0, 0, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -7.443012), nrow=1)
k2 <- matrix(c(0, 20.16, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -14.886023), nrow=1)
k3 <- matrix(c(0, 27.36, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -20.202460), nrow=1)
k4 <- matrix(c(0, 36.00, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -26.582184), nrow=1)
k5 <- matrix(c(0, 44.64, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -32.961908), nrow=1)
k6 <- matrix(c(0, 56.16, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -41.468207), nrow=1)
k7 <- matrix(c(0, 69.12, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -51.037793), nrow=1)
k8 <- matrix(c(0, 89.28, -0.738394, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -65.923816), nrow=1)

delta.eta <- glht(fit.linMVPAMins, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.794360209361403
# Lower = 0.727698984334429
# Upper = 0.867127968845287
delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.79434539527875
# Lower = 0.727860339466616
# Upper = 0.866903405484279


delta.eta <- glht(fit.linMVPAMins, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.703869140978546
# Lower = 0.648191839354837
# Upper = 0.764328918603777

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.711467015546241
# Lower = 0.655550244795298
# Upper = 0.772153344810873


delta.eta <- glht(fit.linMVPAMins, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.623686537284541
# Lower = 0.563843978002783
# Upper = 0.689880378199342

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.637235789297531
# Lower = 0.576893242389952
# Upper = 0.703890115750678

delta.eta <- glht(fit.linMVPAMins, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.572068605981235
# Lower = 0.505693353721951
# Upper = 0.647156003812648

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.589004067242822
# Lower = 0.521722688548629
# Upper = 0.664962055213075

delta.eta <- glht(fit.linMVPAMins, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.515734530478681
# Lower = 0.441531099330773
# Upper = 0.602408542300221

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.535920111758357
# Lower = 0.460085511488651
# Upper = 0.624254315807061

delta.eta <- glht(fit.linMVPAMins, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.464947915594568
# Lower = 0.384397020469287
# Upper = 0.562378355461279

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.4876203445106
# Lower = 0.404546348632914
# Upper = 0.5877536682364

delta.eta <- glht(fit.linMVPAMins, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.404925472955023
# Lower = 0.31881446956802
# Upper = 0.514294846372607

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.429923151121077
# Lower = 0.339986418811563
# Upper = 0.543650880279192

delta.eta <- glht(fit.linMVPAMins, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.346610919576589
# Lower = 0.257884304751003
# Upper = 0.465864449120808

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.373132840895128
# Lower = 0.279120196940684
# Upper = 0.498810614496865

delta.eta <- glht(fit.linMVPAMins, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.272139215519771
# Lower = 0.185095311931847
# Upper = 0.400116847102997

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.299332761030481
# Lower = 0.205009713364167
# Upper = 0.437052958885761


# 60th
k0 <- matrix(c(0, 0, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -10.09347), nrow=1)
k2 <- matrix(c(0, 20.16, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -20.18693), nrow=1)
k3 <- matrix(c(0, 27.36, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -27.39655), nrow=1)
k4 <- matrix(c(0, 36, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -36.04810), nrow=1)
k5 <- matrix(c(0, 44.64, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -44.69964), nrow=1)
k6 <- matrix(c(0, 56.16, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -56.23503), nrow=1)
k7 <- matrix(c(0, 69.12, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -69.21234), nrow=1)
k8 <- matrix(c(0, 89.28, -1.001336, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -89.39928), nrow=1)

delta.eta <- glht(fit.linMVPAMins, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.731835918427173
# Lower = 0.649819609674668
# Upper = 0.82420383061121
delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.731817410352985
# Lower = 0.650015013311905
# Upper = 0.823914388326237

delta.eta <- glht(fit.linMVPAMins, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.646004286893898
# Lower = 0.580587331735191
# Upper = 0.718792016074572

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.652979234169146
# Lower = 0.5871677567158
# Upper = 0.726167054269129

delta.eta <- glht(fit.linMVPAMins, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.570239221893136
# Lower = 0.508008567823072
# Upper = 0.640093082639779

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.58263424773916
# Lower = 0.519707467187536
# Upper = 0.653180275580074

delta.eta <- glht(fit.linMVPAMins, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.521624865486566
# Lower = 0.456787598661872
# Upper = 0.595665252495808

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.537076885943519
# Lower = 0.471219278250922
# Upper = 0.6121387530779

delta.eta <- glht(fit.linMVPAMins, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.468726731829467
# Lower = 0.399064122802117
# Upper = 0.550549990785511

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.487085276360462
# Lower = 0.415807549999416
# Upper = 0.570581429912663

delta.eta <- glht(fit.linMVPAMins, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.421193019779073
# Lower = 0.346951141580665
# Upper = 0.511321447459106

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.441746938017349
# Lower = 0.365125471675157
# Upper = 0.534447395171912

delta.eta <- glht(fit.linMVPAMins, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.365227177443528
# Lower = 0.286721061841197
# upper = 0.465228784682884

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.387791489464857
# Lower = 0.305758538755009
# upper = 0.491833326760719

delta.eta <- glht(fit.linMVPAMins, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.311103753734937
# Lower = 0.230674988837198
# upper = 0.419575377789553

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.334927719748755
# Lower = 0.249670102790578
# Upper = 0.449299200033547

delta.eta <- glht(fit.linMVPAMins, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.242408927252276
# Lower = 0.163950293802485
# Upper = 0.358414045188546

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.266651700939418
# Lower = 0.18158925862144
# Upper = 0.391560217568341



# 50th
k0 <- matrix(c(0, 0, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -12.63801), nrow=1)
k2 <- matrix(c(0, 20.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -25.27602), nrow=1)
k3 <- matrix(c(0, 27.36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -34.30317), nrow=1)
k4 <- matrix(c(0, 36, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -45.13576), nrow=1)
k5 <- matrix(c(0, 44.64, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -55.96834), nrow=1)
k6 <- matrix(c(0, 56.16, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -70.41178), nrow=1)
k7 <- matrix(c(0, 69.12, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -86.66065), nrow=1)
k8 <- matrix(c(0, 89.28, -1.253771, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -111.93667), nrow=1)

delta.eta <- glht(fit.linMVPAMins, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.676445269923062
# Lower = 0.582905587984482
# Upper = 0.78499539656749
delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.676423850035795
# Lower = 0.583125066947181
# Upper = 0.784650242001502


delta.eta <- glht(fit.linMVPAMins, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.594932407921862
# Lower = 0.521767864831303
# Upper = 0.678356399181734

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.601357424564364
# Lower = 0.527688243297836
# Upper = 0.685311368353861

delta.eta <- glht(fit.linMVPAMins, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.523241991234504
# Lower = 0.458502419956232
# Upper = 0.597122652956081

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.534621527699753
# Lower = 0.469021269900567
# Upper = 0.609397049179906

delta.eta <- glht(fit.linMVPAMins, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.477386815256015
# Lower = 0.41332788922889
# Upper = 0.551373805927905

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.491537068384794
# Lower = 0.426343628112471
# Upper = 0.566699426624432

delta.eta <- glht(fit.linMVPAMins, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.427633589545263
# Lower = 0.361515659885222
# Upper = 0.505843887828888

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.444393878109858
# Lower = 0.37665451229947
# Upper = 0.524315818482753

delta.eta <- glht(fit.linMVPAMins, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.383065647581871
# Lower = 0.314062312751719
# Upper = 0.46722985980595

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.401772184505159
# Lower = 0.330497840303289
# Upper = 0.488417376930257

delta.eta <- glht(fit.linMVPAMins, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.330781898937473
# Lower = 0.258748498756953
# Upper = 0.42286879031309

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.351233230470714
# Lower = 0.275922347235516
# Upper = 0.447099640253475

delta.eta <- glht(fit.linMVPAMins, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.280442500644971
# Lower = 0.207120872990884
# Upper = 0.379720281361821

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.301934887000705
# Lower = 0.224173909210584
# Upper = 0.406669430484394

delta.eta <- glht(fit.linMVPAMins, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.216927073022401
# Lower = 0.145807274604753
# Upper = 0.322736675091328

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.238638673881217
# Lower = 0.161492347496429
# Upper = 0.352638484452306




# 40th
k0 <- matrix(c(0, 0, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -15.11041), nrow=1)
k2 <- matrix(c(0, 20.16, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -30.22083), nrow=1)
k3 <- matrix(c(0, 27.36, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -41.01398), nrow=1)
k4 <- matrix(c(0, 36, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -53.96576), nrow=1)
k5 <- matrix(c(0, 44.64, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -66.91755), nrow=1)
k6 <- matrix(c(0, 56.16, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -84.18659), nrow=1)
k7 <- matrix(c(0, 69.12, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -103.61427), nrow=1)
k8 <- matrix(c(0, 89.28, -1.499049, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -133.83509), nrow=1)

delta.eta <- glht(fit.linMVPAMins, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.626643737902943
# Lower = 0.524495393900196
# Upper = 0.74868603007731
delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.626620013161715
# Lower = 0.524731523244367
# Upper = 0.748292457192305

delta.eta <- glht(fit.linMVPAMins, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.549179016731269
# Lower = 0.470080210554314
# Upper = 0.641587511336166

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.555111257614892
# Lower = 0.475427615683355
# Upper = 0.648150208708155

delta.eta <- glht(fit.linMVPAMins, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.481290356617559
# Lower = 0.414395307635199
# Upper = 0.558984146550653

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.491762946354344
# Lower = 0.42387725565448
# Upper = 0.570520810402321

delta.eta <- glht(fit.linMVPAMins, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.437999640096011
# Lower = 0.374423282025817
# Upper = 0.51237114232391

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.450990162786139
# Lower = 0.386178988878817
# Upper = 0.526678386932367

delta.eta <- glht(fit.linMVPAMins, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.391159305165568
# Lower = 0.327949913797843
# Upper = 0.466551737262924

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.40650003664652
# Lower = 0.341654764115422
# Upper = 0.48365278974362

delta.eta <- glht(fit.linMVPAMins, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.349328140514353
# Lower = 0.284797310210256
# Upper = 0.428480696201537

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.366398850932638
# Lower = 0.299683752842258
# Upper = 0.447965953080614

delta.eta <- glht(fit.linMVPAMins, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.300427803056688
# Lower = 0.234018757283777
# Upper = 0.385682181621111

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.319015753591725
# Lower = 0.249543068671824
# Upper = 0.407829604650476

delta.eta <- glht(fit.linMVPAMins, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.253547880354027
# Lower = 0.186429356057232
# Upper = 0.344830497683449

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.272993561145666
# Lower = 0.201774869606749
# Upper = 0.369349684488655

delta.eta <- glht(fit.linMVPAMins, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.194736090263576
# Lower = 0.130008732831167
# Upper = 0.291689212142314

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.214241672892471
# Lower = 0.14399153539345
# Upper = 0.31876522656937




# 30th
k0 <- matrix(c(0, 0, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -17.77312), nrow=1)
k2 <- matrix(c(0, 20.16, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -35.54623), nrow=1)
k3 <- matrix(c(0, 27.36, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -48.24132), nrow=1)
k4 <- matrix(c(0, 36, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -63.47542), nrow=1)
k5 <- matrix(c(0, 44.64, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -78.70952), nrow=1)
k6 <- matrix(c(0, 56.16, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -99.02165), nrow=1)
k7 <- matrix(c(0, 69.12, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -121.87280), nrow=1)
k8 <- matrix(c(0, 89.28, -1.763206, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -157.41903), nrow=1)

delta.eta <- glht(fit.linMVPAMins, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.577101795834603
# Lower = 0.468118246457453
# Upper = 0.711458024283175
delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.577076096671358
# Lower = 0.468366141762901
# Upper = 0.711018136571522

delta.eta <- glht(fit.linMVPAMins, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.503831425497363
# Lower = 0.419982072760946
# Upper = 0.604421287913388

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.50927514630525
# Lower = 0.424777269653151
# Upper = 0.610581575742067

delta.eta <- glht(fit.linMVPAMins, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.439863661481793
# Lower = 0.371204630282857
# Upper = 0.521222056267827

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.449440168905339
# Lower = 0.379683004518645
# Upper = 0.532013450751496

delta.eta <- glht(fit.linMVPAMins, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.399207482394352
# Lower = 0.336121121804938
# Upper = 0.474134482069599

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.411055082918609
# Lower = 0.346645313176317
# Upper = 0.487432758415176

delta.eta <- glht(fit.linMVPAMins, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.355349233467242
# Lower = 0.294877017746048
# Upper = 0.428222852669057

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.36929531083299
# Lower = 0.307171708564
# Upper = 0.443983032294198

delta.eta <- glht(fit.linMVPAMins, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.316309396227748
# Lower = 0.25605159674977
# Upper = 0.390747940696263

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.331777983706963
# Lower = 0.269416097273952
# Upper = 0.408574808953335

delta.eta <- glht(fit.linMVPAMins, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.270845136968602
# Lower = 0.209865939556658
# Upper = 0.349542609794175

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.287615702390202
# Lower = 0.223778007261181
# Upper = 0.369664531710929

delta.eta <- glht(fit.linMVPAMins, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.227460561851128
# Lower = 0.166346406562674
# Upper = 0.311027501385414

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.244919434587813
# Lower = 0.180033626420638
# Upper = 0.333190696823833

delta.eta <- glht(fit.linMVPAMins, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.173369128810019
# Lower = 0.114815341746221
# Upper = 0.261784308326847

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.190748902413879
# Lower = 0.127160586671813
# Upper = 0.286135387736181




# 20th
k0 <- matrix(c(0, 0, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0, 0), nrow=1)
k1 <- matrix(c(0, 10.08, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -20.89061), nrow=1)
k2 <- matrix(c(0, 20.16, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -41.78122), nrow=1)
k3 <- matrix(c(0, 27.36, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -56.70308), nrow=1)
k4 <- matrix(c(0, 36, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -74.60932), nrow=1)
k5 <- matrix(c(0, 44.64, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -92.51555), nrow=1)
k6 <- matrix(c(0, 56.16, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -116.39053), nrow=1)
k7 <- matrix(c(0, 69.12, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -143.24989), nrow=1)
k8 <- matrix(c(0, 89.28, -2.072481, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0, -185.03110), nrow=1)

delta.eta <- glht(fit.linMVPAMins, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.524052657423134
# Lower = 0.409764486231192
# Upper = 0.670217153951453
delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.524025227329413
# Lower = 0.410019553491958
# Upper = 0.669730105647338

delta.eta <- glht(fit.linMVPAMins, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.45547417641528
# Lower = 0.367967870946503
# Upper = 0.563790324539881

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.460396816991011
# Lower = 0.372190753813159
# Upper = 0.569506971690816

delta.eta <- glht(fit.linMVPAMins, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.395869999784529
# Lower = 0.325991050773175
# Upper = 0.480728094706023

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.404494322106765
# Lower = 0.333429144057568
# Upper = 0.4907059251796

delta.eta <- glht(fit.linMVPAMins, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.358133227111254
# Lower = 0.295799972841709
# Upper = 0.43360182602098

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.368769821799343
# Lower = 0.305038126413707
# Upper = 0.445816996939857

delta.eta <- glht(fit.linMVPAMins, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.31756676243141
# Lower = 0.259978096457315
# Upper = 0.387912097116711

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.330040245001594
# Lower = 0.270789958588587
# Upper = 0.402254809921535

delta.eta <- glht(fit.linMVPAMins, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.281595346130403
# Lower = 0.225778678000846
# Upper = 0.351210927729874

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.295378196484305
# Lower = 0.237541570697041
# Upper = 0.367296884929655

delta.eta <- glht(fit.linMVPAMins, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.23989031292872
# Lower = 0.184566691522045
# Upper = 0.311797116600347

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.254757595855469
# Lower = 0.196789473122998
# Upper = 0.329801343619096

delta.eta <- glht(fit.linMVPAMins, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.20030802502564
# Lower = 0.145451067562106
# Upper = 0.275854316934046

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.215697162951983
# Lower = 0.157412293921339
# Upper = 0.295563103405276

delta.eta <- glht(fit.linMVPAMins, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.151312922163614
# Lower = 0.0991737821267534
# Upper = 0.230863439133833

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.166496333397847
# Lower = 0.10983298722469
# Upper = 0.252392561974272




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

delta.eta <- glht(fit.linMVPAMins, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.45754967112443
# Lower = 0.339749671793008
# Upper = 0.616193977292851
delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k0)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.457520692316247
# Lower = 0.340005589376567
# Upper = 0.615652184663658

delta.eta <- glht(fit.linMVPAMins, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.395176562350631
# Lower = 0.305397381093358
# upper = 0.511348574346562

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.399449235646554
# Lower = 0.308930581424573
# Upper = 0.516490439770766

delta.eta <- glht(fit.linMVPAMins, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.341306147259351
# Lower = 0.271202990918118
# Upper = 0.42953024139838

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k2)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.348748580202631
# Lower = 0.277391157724303
# Upper = 0.438462325876421

delta.eta <- glht(fit.linMVPAMins, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.307384468966684
# Lower = 0.246648275134393
# Upper = 0.383076718093598

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k3)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.316523469109285
# Lower = 0.254332676624088
# Upper = 0.393921488291715

delta.eta <- glht(fit.linMVPAMins, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.271098610153184
# Lower = 0.217269092570244
# Upper = 0.33826466322276

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k4)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.281759182278749
# Lower = 0.226276039116528
# Upper = 0.350846855497174

delta.eta <- glht(fit.linMVPAMins, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.239096193356972
# Lower = 0.188763702457556
# Upper = 0.302849482890646

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k5)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.250813113548239
# Lower = 0.198572408191784
# Upper = 0.316797376335413

delta.eta <- glht(fit.linMVPAMins, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.202224213015739
# Lower = 0.153805327588118
# Upper = 0.265885668403818

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k6)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.214773050550568
# Lower = 0.163974750978379
# Upper = 0.281308329285884

delta.eta <- glht(fit.linMVPAMins, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.167494753071437
# Lower = 0.120252525875824
# Upper = 0.23329649088146

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k7)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.180379851267512
# Lower = 0.130132248209155
# Upper = 0.250029421538886

delta.eta <- glht(fit.linMVPAMins, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.124941601895332
# Lower = 0.0805732123032607
# Upper = 0.193741858341421

delta.eta <- glht(fit.linMVPAMinsENMO, linfct=k8)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.137495894903096
# Lower = 0.0892277538789959
# Upper = 0.211874896468211

# NOT bothering w/ Intensity Gradient decile until we figure out it's worthwhile

# ---------
# Running as 1 SD increase for each exposure
# ---------


# PAEE (1 sd = 11.51 kJ/kg/day)
fit.linstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedPAEE + StandPGS + StandardizedPAEE*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linstand)
# Coefficients:
# PAEE = 0.8282093 (95% CI: 0.7772, 0.8825)
# PGS = 1.5136 (95% CI: 1.4277, 1.6047)
# PAEEXPGS = 1.0494 (95% CI: 0.99, 1.1125)

# ENMO (1 sd = 8.20 mgs avg over week)
fit.linENMOstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedENMO + StandPGS + StandardizedENMO*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linENMOstand)
# Coefficients:
# ENMO = 0.8148 (95% CI: 0.7623, 0.8709)
# PGS = 1.5166 (95% CI: 1.4303, 1.6080)
# ENMOXPGS = 1.0576 (95% CI: 0.9953, 1.1239)

# Percent MVPA (1 sd = 11.39% of PAEE)
fit.linMVPAstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedPercentMVPA + StandPGS + StandardizedPercentMVPA*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linMVPAstand)
# Coefficients:
# PAEE (not stand) = 0.9987 (95% CI: 0.9918, 1.0058)
# Percent MVPA = 0.7874 (95% CI: 0.7260, 0.8540)
# PGS = 1.5088 (95% CI: 1.4237, 1.5990)

# MVPA in Mins (1 sd = 36.69 mins/day)
fit.linMVPAMinsstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linMVPAMinsstand)
# Coefficients:
# PAEE (not stand) = 1.0136 (95% CI: 1.0012, 1.0262)
# MVPA Mins = 0.6694 (95% CI: 0.5739, 0.7809)
# PGS = 1.5174 (95% CI: 1.4302, 1.6100)

# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMOstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linMVPAMinsENMOstand)
# Coefficients:
# ENMO (not stand) = 1.0145 (95% CI: 0.9972, 1.0321)
# MVPA Mins = 0.6961 (95% CI: 0.5985, 0.8096)
# PGS = 1.5171 (95% CI: 1.4299, 1.6096)
# MVPA MinsXPGS = 1.054 (95% CI: 0.9895, 1.1226)

# Intensity Gradient (1 sd = 0.045 units)
fit.linIGstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedIntensity.Gradient + StandPGS + StandardizedIntensity.Gradient*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linIGstand)
# Coefficients: 
# PAEE (not stand) = 0.9939 (95% CI: 0.9858, 1.0021)
# IG = 0.8670 (95% CI: 0.7858, 0.9566)
# PGS = 1.5019 (95% CI: 1.4178, 1.5909)
# IGXPGS = 1.0303 (95% CI: 0.9700, 1.0943)

# Intensity Gradient CONTROLLING FOR ENMO
fit.linIGENMOstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandardizedIntensity.Gradient + StandPGS + StandardizedIntensity.Gradient*StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linIGENMOstand)
# Coefficients: 
# ENMO (not stand) = 0.9898358 (95% CI: 0.9766, 1.0032)
# IG = 0.8777 (95% CI: 0.7863, 0.9797)
# PGS = 1.5019 (95% CI: 1.4178, 1.5909)
# IGXPGS = 1.0303 (95% CI: 0.9698, 1.0946)

# ---------
# Running as 1 SD increase ALSO CONTROLLING FOR MEDICATION USE!
# If no real diff, then not bothering incorporating 
# ---------


# PAEE (1 sd = 11.51 kJ/kg/day)
fit.linstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedPAEE + StandPGS + StandardizedPAEE*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linstand)
# Coefficients:
# PAEE = 0.8475 (95% CI: 0.7952, 0.9034)
# PGS = 1.4948 (95% CI: 1.4099, 1.5847)
# PAEEXPGS = 1.0508 (95% CI: 0.9915, 1.1137)

# ENMO (1 sd = 8.20 mgs avg over week)
fit.linENMOstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedENMO + StandPGS + StandardizedENMO*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linENMOstand)
# Coefficients:
# ENMO = 0.8355 (95% CI: 0.7815, 0.8933)
# PGS = 1.4977 (95% CI: 1.4125, 1.5880)
# ENMOXPGS = 1.0587 (95% CI: 0.9966, 1.1247)

# Percent MVPA (1 sd = 11.39% of PAEE)
fit.linMVPAstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedPercentMVPA + StandPGS + StandardizedPercentMVPA*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linMVPAstand)
# Coefficients:
# PAEE (not stand) = 0.9996 (95% CI: 0.9926, 1.0066)
# Percent MVPA = 0.8022 (95% CI: 0.7396, 0.8702)
# PGS = 1.4901 (95% CI: 1.4059, 1.5793)
# PGSXPercent MVPA = 1.0381 (95% CI: 0.9824, 1.0970)

# MVPA in Mins (1 sd = 36.69 mins/day)
fit.linMVPAMinsstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linMVPAMinsstand)
# PAEE (not stand) = 1.0141 (95% CI: 1.0017, 1.0267)
# MVPA Mins = 0.6836 (95% CI: 0.5862, 0.7972)
# PGS = 1.4987 (95% CI: 1.4126, 1.5900)
# PGSXMVPA Mins = 1.0536 (95% CI: 0.9895, 1.1220)

# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMOstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linMVPAMinsENMOstand)
# Coefficients:
# ENMO (not stand) = 1.0162 (95% CI: 0.9988, 1.0338)
# MVPA Mins = 0.7057 (95% CI: 0.6069, 0.8205)
# PGS = 1.4983 (95% CI: 1.4122, 1.5896)
# PGSXMVPA Mins = 1.0535 (95% CI: 0.9895, 1.1216)

# Intensity Gradient (1 sd = 0.045 units)
fit.linIGstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedIntensity.Gradient + StandPGS + StandardizedIntensity.Gradient*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linIGstand)
# Coefficients: 
# PAEE (not stand) = 0.9950 (95% CI: 0.9869, 1.0032)
# IG = 0.8782 (95% CI: 0.7960, 0.9689)
# PGS = 1.4830 (95% CI: 1.40, 1.5709)
# IGXPGS = 1.0305 (95% CI: 0.9703, 1.0944)

# Intensity Gradient CONTROLLING FOR ENMO
fit.linIGENMOstand <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandardizedIntensity.Gradient + StandPGS + StandardizedIntensity.Gradient*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = data)
summary(fit.linIGENMOstand)
# Coefficients: 
# ENMO (not stand) = 0.9920 (95% CI: 0.9788, 1.0054)
# IG = 0.8856 (95% CI: 0.7935, 0.9884)
# PGS = 1.4830 (95% CI: 1.40, 1.5710)
# PGSXIG = 1.0305 (95% CI: 0.9704, 0.9701)


# --------
# STRATIFYING by SEX for 1 SD increase for each exposure
# Same logic as above
# --------

maledata <- subset(data, Biological.Sex == "Male")
femaledata <- subset(data, Biological.Sex == "Female")

# male
# PAEE (1 sd = 11.51 kJ/kg/day)
fit.linstandmale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedPAEE + StandPGS + StandardizedPAEE*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = maledata)
summary(fit.linstandmale)
# Coefficients:
# PAEE = 0.8488 (95% CI: 0.7881, 0.9143)
# PGS = 1.4958 (95% CI: 1.3957, 1.6031)
# PAEEXPGS = 1.0367 (95% CI: 0.9687, 1.1095)

# ENMO (1 sd = 8.20 mgs avg over week)
fit.linENMOstandmale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedENMO + StandPGS + StandardizedENMO*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = maledata)
summary(fit.linENMOstandmale)
# Coefficients:
# ENMO = 0.8481 (95% CI: 0.7855, 0.9157)
# PGS = 1.4953 (95% CI: 1.3960, 1.6017)
# ENMOXPGS = 1.0393 (95% CI: 0.9693, 1.1143)

# Percent MVPA (1 sd = 11.39% of PAEE)
fit.linMVPAstandmale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedPercentMVPA + StandPGS + StandardizedPercentMVPA*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = maledata)
summary(fit.linMVPAstandmale)
# Coefficients:
# PAEE (not stand) = 0.9980 (95% CI: 0.99, 1.0061)
# Percent MVPA = 0.8281 (95% CI: 0.7545, 0.9088)
# PGS = 1.4803 (95% CI: 1.3847, 1.5824)
# Percent MVPAXPGS = 1.0043 (95% CI: 0.9418, 1.0709)

# MVPA in Mins (1 sd = 36.69 mins/day)
fit.linMVPAMinsstandmale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = maledata)
summary(fit.linMVPAMinsstandmale)
# Coefficients:
# PAEE (not stand) = 1.0115 (95% CI: 0.9971, 1.0260)
# MVPA Mins = 0.7127 (95% CI: 0.5958, 0.8527)
# PGS = 1.4876 (95% CI: 1.3882, 1.5941)
# MVPA MinsXPGS = 1.0184 (95% CI: 0.9461, 1.0961)

# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMOstandmale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = maledata)
summary(fit.linMVPAMinsENMOstandmale)
# Coefficients:
# ENMO (not stand) = 1.0171 (95% CI: 0.9979, 1.0366)
# MVPA Mins = 0.7086 (95% CI: 0.5972, 0.8408)
# PGS = 1.4878 (95% CI: 1.3884, 1.5943)
# MVPA Mins X PGS = 1.0186 (95% CI: 0.9464, 1.0962)

# Intensity Gradient (1 sd = 0.045 units)
fit.linIGstandmale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedIntensity.Gradient + StandPGS + StandardizedIntensity.Gradient*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = maledata)
summary(fit.linIGstandmale)
# Coefficients: 
# PAEE (not stand) = 0.9896 (95% CI: 0.9803, 0.999)
# IG = 0.9599 (95% CI: 0.8585, 1.0733)
# PGS = 1.4769 (95% CI: 1.3818, 1.5786)
# IGXPGS = 1.0013 (95% CI: 0.9343, 1.0732)

# Intensity Gradient CONTROLLING FOR ENMO
fit.linIGENMOstandmale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandardizedIntensity.Gradient + StandPGS + StandardizedIntensity.Gradient*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = maledata)
summary(fit.linIGENMOstandmale)
# Coefficients: 
# ENMO (not stand) = 0.9852 (95% CI: 0.9702, 1.0004)
# IG = 0.9660 (95% CI: 0.8517, 1.0957)
# PGS = 1.4769 (95% CI: 1.3817, 1.5786)
# IGXPGS = 1.0010 (95% CI: 0.9337, 1.0732)

# female
# PAEE (1 sd = 11.51 kJ/kg/day)
fit.linstandfemale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedPAEE + StandPGS + StandardizedPAEE*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = femaledata)
summary(fit.linstandfemale)
# Coefficients:
# PAEE = 0.8471 (95% CI: 0.74741, 0.9601)
# PGS = 1.4912 (95% CI: 1.3363, 1.6641)
# PGSXPAEE = 1.0964 (95% CI: 0.97787, 1.2293)

# ENMO (1 sd = 8.20 mgs avg over week)
fit.linENMOstandfemale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ StandardizedENMO + StandPGS + StandardizedENMO*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = femaledata)
summary(fit.linENMOstandfemale)
# Coefficients:
# ENMO = 0.8005 (95% CI: 0.69869, 0.9171)
# PGS = 1.5087 (95% CI: 1.34865, 1.6878)
# PGSXENMO = 1.1248 (95% CI: 0.99564, 1.2708)

# Percent MVPA (1 sd = 11.39% of PAEE)
fit.linMVPAstandfemale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedPercentMVPA + StandPGS + StandardizedPercentMVPA*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = femaledata)
summary(fit.linMVPAstandfemale)
# Coefficients:
# PAEE (not stand) = 1.0058 (95% CI: 0.9915, 1.0202)
# Percent MVPA = 0.72 (95% CI: 0.6089, 0.8514)
# PGS = 1.5667 (95% CI: 1.3874, 1.7693)
# PGS X Percent MVPA = 1.1453 (95% CI: 1.0266, 1.2776)

# MVPA in Mins (1 sd = 36.69 mins/day)
fit.linMVPAMinsstandfemale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = femaledata)
summary(fit.linMVPAMinsstandfemale)
# Coefficients:
# PAEE (not stand) = 1.0233 (0.9987, 1.0485)
# MVPA Mins = 0.5997 (95% CI: 0.44471, 0.8088)
# PGS = 1.5398 (95% CI: 1.37216, 1.7279)
# PGS X MVPA Mins = 1.1586 (95% CI: 1.02619, 1.3081)

# MVPA in Mins CONTROLLING FOR ENMO
fit.linMVPAMinsENMOstandfemale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandardizedMVPAMins + StandPGS + StandardizedMVPAMins*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = femaledata)
summary(fit.linMVPAMinsENMOstandfemale)
# Coefficients:
# ENMO (not stand) = 1.0128 (95% CI: 0.97369, 1.0535)
# MVPA Mins = 0.7013 (95% CI: 0.50766, 0.9687)
# PGS = 1.5367 (95% CI: 1.36972, 1.7240)
# PGS X MVPA Mins = 1.1552 (95% CI: 1.02465, 1.3024)

# Intensity Gradient (1 sd = 0.045 units)
fit.linIGstandfemale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ OverallPAEETRANSFORM + StandardizedIntensity.Gradient + StandPGS + StandardizedIntensity.Gradient*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = femaledata)
summary(fit.linIGstandfemale)
# Coefficients: 
# PAEE (not stand) = 1.0129 (95% CI: 0.9873, 0.99603)
# IG = 0.6611 (95% CI: 0.53835, 0.8118)
# PGS = 1.5313 (95% CI: 1.36191, 1.7217)
# PGSXIG = 1.1292 (95% CI: 1.00081, 1.2742)

# Intensity Gradient CONTROLLING FOR ENMO
fit.linIGENMOstandfemale <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ p90012 + StandardizedIntensity.Gradient + StandPGS + StandardizedIntensity.Gradient*StandPGS + Meds + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_Weekly + as.factor(OilyFish_InstChosen) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION, data = femaledata)
summary(fit.linIGENMOstandfemale)
# Coefficients: 
# ENMO (not stand) = 1.0132 (95% CI: 0.9855, 1.0417)
# IG = 0.6836 (95% CI: 0.54638, 0.8553)
# PGS = 1.5303 (95% CI: 1.36115, 1.7204)
# PGSXIG = 1.1290 (95% CI: 1.00094, 1.2733)

# ----------
# Correlations of exposures
# ----------

# Correlation of two PA volume measures
cor(data$p90012, data$OverallPAEETRANSFORM)
# ENMO and PAEE are correlated at 0.973720069989151 - makes sense given results

# Correlation of PA intensity measures
cor(data$Intensity.Gradient, data$MVPAMins)
cor(data$Intensity.Gradient, data$PercentMVPA)
cor(data$PercentMVPA, data$MVPAMins)
# 0.858014143013049
# 0.85157584075231
# 0.864168208259047
# Virtually identical for all - IG correctly specified?

# PA volume and intensity correlations
# For ENMO
cor(data$p90012, data$MVPAMins)
cor(data$p90012, data$PercentMVPA)
cor(data$p90012, data$Intensity.Gradient)
# 0.916180683944619
# 0.747075709024377
# 0.868134340272766
# So correlation of IG and ENMO is almost as high as MVPA mins is...

# For PAEE
cor(data$OverallPAEETRANSFORM, data$MVPAMins)
cor(data$OverallPAEETRANSFORM, data$PercentMVPA)
cor(data$OverallPAEETRANSFORM, data$Intensity.Gradient)
# 0.922788627621323
# 0.693885849475599
# 0.815015071661016
# Surprisingly PercentMVPA was LEAST correlated of these


# SAVING AT THE END!! To save Meds variable
# Saving dataset w/ these covariates
write.csv(data, "FinalDatasetwNewExposures.csv")

dx upload FinalDatasetwNewExposuresFINAL.csv


# --------
# RERUNNING PA Intensity analysis using Intensity Gradient
# WHEN I know how to interpert, make figure for this too
# Less correlated w/ volume but also WAY less interpretable...
# NOT EVEN LESS CORRELATED AS CONSTRUCTED!
# --------



# -----------
# FIGURES TO CREATE - OFFLINE
# Forest plot by decile
# Figure across full range of PA w/ different lines as diff genetic risks (hist of PA?)
# w/ Reference group as highest or lowest genetic risk group
# W/ DIFF LINES representing DIFF GENETIC RISKS

# TABLES? Unclear how we'll do this w/ margins Patrick mentioned
# STILL seems like when DESCRIBING results will want to pick some comps
# -----------


# Forest Plot Deciles Example - PAEE
# STILL WORKING ON THIS - Compresses everything like hell...

# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.93, 0.89, 0.85, 0.81, 0.78, 0.74, 0.69, 0.63)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.91, 0.85, 0.80, 0.76, 0.71, 0.66, 0.61, 0.54)
RefUL <- c(1.00, 0.96, 0.92, 0.90, 0.87, 0.84, 0.82, 0.78, 0.73)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.73, 0.67, 0.62, 0.59, 0.56, 0.52, 0.49, 0.45, 0.40)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.58, 0.54, 0.52, 0.50, 0.48, 0.45, 0.42, 0.39, 0.34)
FiftyUL <- c(0.93, 0.82, 0.75, 0.70, 0.65, 0.61, 0.56, 0.52, 0.47)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.54, 0.48, 0.44, 0.41, 0.38, 0.35, 0.32, 0.29, 0.25)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.33, 0.32, 0.30, 0.29, 0.28, 0.27, 0.25, 0.23, 0.20)
TenUL <- c(0.86, 0.73, 0.64, 0.58, 0.52, 0.46, 0.42, 0.36, 0.31)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"

install.packages('ggforce')
library(ggforce)



# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2, y = Model, hjust = 0, label = LABEL)) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 2.35, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3), ylim = c(0,5), expand = TRUE)

# Try using pre-made program to make comprehensible
library(grid)
library(forestploter)



# -----
# Full range of PA w/ different lines as diff genetic risks Example - PAEE
# Far easier to code well than foret plots here
# -----


# 50th genetic risk vs 90th vs 10th
PlotDF$Percentiles <- rep(c(10,20,30,40,50,60,70,80,90), times = 3)

# ALSO make option to represent percentiles in raw units


p <- ggplot(data=PlotDF, aes(x=Percentiles, y=Mean, colour=Genetic.Risk)) + geom_point() + geom_line()


p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), linetype=1, alpha=0.1) + theme_bw() + xlab("PA Volume Percentile (PAEE)") + ylab("Hazard Ratio")

p + scale_x_continuous(breaks=seq(10,90,10)) + ggtitle("Association of CAD and PA Volume by Genetic Risk") + guides(color=guide_legend(title="Genetic Risk Percentile")) + scale_y_continuous(breaks=seq(0,1.0,0.20))

# Percentile vs raw and get tick marks right
# Get as RAW this time
PlotDF$Raw <- rep(c(25.84,30.02,33.15,35.89,38.61,41.37,44.54,48.47,54.33), times = 3)

p <- ggplot(data=PlotDF, aes(x=Raw, y=Mean, colour=Genetic.Risk)) + geom_point() + geom_line()


p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), linetype=1, alpha=0.1) + theme_bw() + xlab("PA Volume (in kJ/kg/day)") + ylab("Hazard Ratio")

p + scale_x_continuous(breaks = unique(PlotDF$Raw)) + ggtitle("Association of CAD and PA Volume by Genetic Risk") + theme(axis.text.x=element_text(angle=45, vjust = 0.75))



# CAN ALSO SIMPLY FIT AS REGRESSION LINE
# So each point is just a one unit increase

