# -------
# Exploring Sensitivity of Our Results
# Using DempseyDataset:
# ACCOUNTING for PH violations
# Running restricted cubic spline


# ANALYSES

# AFTER ALL OF THIS, DECIDE FIGURES TO CREATE...
# -------



# -----
# FIRST just creating baseline analysis
# cubic spline without considering PH violation (used to create original Figures)
# -----

dx download DempseyDataset

# Reading in needed packages
install.packages("survival")
library(survival)

install.packages("ggplot2")
library(ggplot2)

install.packages("multcomp")
library(multcomp)


datsubrest <- read.csv("DempseyDataset")


datsubrest$TimeAge <-datsubrest$TimeYear + datsubrest$AgeBaseline


# Getting 10th to 90th(?) percentile
PGSQUINTILE <- quantile(datsubrest$StandPGS, probs = seq(0.2, 0.8, 0.2))
# 20% -0.846868164911578
# 40% -0.266874315609926
# 60% 0.236609243902655
# 80% 0.821322441664612



PAEEQUINTILE <- quantile(datsubrest$PAEEPOS, probs = seq(0.2, 0.8, 0.2))
# 20% 30.1581806907346
# 40% 36.0267497376078
# 60% 41.5283167027557
# 80% 48.6690567680804


# Goes from 20th to 80th %tile
# FLIPPING PA (10th = LEAST RISK/HIGHEST ACTIVITY)
PAEEQUINTILE <- sort(PAEEQUINTILE, decreasing = TRUE)




knots <- quantile(datsubrest$PAEEPOS, probs = c(0.25, 0.50, 0.75))
# Using these four knots
# 25% 31.7921775924108
# 50% 38.7595302926387
# 75% 46.5649390159472




datsubrest$ls.1 <- (datsubrest$PAEEPOS - 31.79)*as.integer(datsubrest$PAEEPOS > 31.79)
datsubrest$ls.2 <- (datsubrest$PAEEPOS - 38.76)*as.integer(datsubrest$PAEEPOS > 38.76)
datsubrest$ls.3 <- (datsubrest$PAEEPOS - 46.56)*as.integer(datsubrest$PAEEPOS > 46.56)



# Unrestricted quadratic spline
datsubrest$qs.1 <- datsubrest$ls.1^3
datsubrest$qs.2 <- datsubrest$ls.2^3
datsubrest$qs.3 <- datsubrest$ls.3^3




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3




fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)






# ------
# REPEATING their analysis anyway to see what happens
# ------

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(20 - 31.79)*as.integer(20 > 31.79)
(20 - 38.76)*as.integer(20 > 38.76)
(20 - 46.56)*as.integer(20 > 46.56)
#0
#0
#0
#0


# Creating spline codes to add
qs.1 <- ls.1^3
qs.2 <- ls.2^3
qs.3 <- ls.3^3


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.960408042228368
# Lower = 0.918095731833569
# Upper = 1.00467040156563




# -----
# In Dempsey:
# Estimate = 0.88
# Lower = 0.80
# Upper = 0.96
# -----













# --------------------------------------------------------
# EVALUATING proportional hazards assumption
# 3 ways:
# Graphic eval with log-log plot (ONLY FOR CATEGORICAL)
# Analytically w/ interaction of follow up time and covariate
# Analytically with schoenfeld residuals
# -------

# Analytically w/ interaction
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + tt(p22009_a1) + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + tt(PAEEPOS) + tt(rqs.1) + tt(rqs.2) + tt(StandPGS) + tt(p22009_a1) + tt(p22009_a2) + tt(p22009_a3) + tt(p22009_a4) + tt(p22009_a5) + tt(p22009_a6) + tt(p22009_a7) + tt(p22009_a8) + tt(p22009_a9) + tt(p22009_a10) + tt(SeasonWear) + tt(Biological.Sex) + tt(as.factor(Salt_InstChosen)) + tt(as.factor(NewAlc)) + tt(as.factor(NewOilFish)) + tt(FnVScore) + tt(ProcMeat_InstChosen) + tt(ParentHist) + tt(MobilityDichot) + tt(NewEmploy) + tt(Townsend) + tt(as.factor(NewEduc)) + tt(as.factor(SmokStat_InstChosen))
                 , data = datsubrest, tt=function(x,t,...) x*t)


# Schoenfeld resids
ph <- cox.zph(fit.rqs, transform="km", global=TRUE)
plot(ph, var = 1)
abline(h = coef(fit.rqs)[1], col = "red", lwd = 2)

ph <- cox.zph(fit.rqs, transform="km", global=TRUE)
plot(ph, var = 1, ylim = c(-0.2, 0.2), xlab = "Age", ylab = "Effect of PA on CAD Risk")
abline(h = coef(fit.rqs)[1], col = "red", lwd = 2)


# Significant differences by time
# PAEE - p-value of 0.0175
# ODD THING THOUGH - PA REALLY does not look nonlinear graphically
plot(ph, var = 1, ylim = c(-0.5, 0.5))
plot(ph, var = 1, ylim = c(-0.2, 0.2))
# Zooming in a lot makes this clearer...
# DISCONTINUITY apparent before around 70...

# p22009_a1 - p-value of 0.0114
# Biological Sex - p-value of 0.0057
# NewEmploy - HUGE
# NewEduc - 0.0139 p-value
# SmokStat - 0.0167 p-value

# Doing interactions based on testing - just won't run...
#fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + tt(PAEEPOS) + tt(p22009_a1) + tt(Biological.Sex) + tt(NewEmploy) + tt(as.factor(NewEduc)) + tt(as.factor(SmokStat_InstChosen))
#                 , data = datsubrest)


# Can first STRATIFY on all NON-EXPOSURE variables
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + tt(p22009_a1) + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + Townsend + strata(Biological.Sex) + strata(NewEmploy) + strata(NewEduc) + strata(SmokStat_InstChosen), data = datsubrest,  tt=function(x,t,...) x*t)

summary(fit.rqs)

# Worth noting: stratifying and adjusting for PH violations of confounders
# has almost NO effect on exposure or CI

# Okay NOW trying w/ interaction of pre- and post-70 for PA????
# datsubrest$Age70 <- ifelse(datsubrest$TimeAge >= 70, 1, 0)

fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + tt(PAEEPOS) + rqs.1 + rqs.2 + StandPGS + p22009_a1 + tt(p22009_a1) + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + Townsend + strata(Biological.Sex) + strata(NewEmploy) + strata(NewEduc) + strata(SmokStat_InstChosen), data = datsubrest,  tt=function(x,t,...) x*t)

summary(fit.rqs)

# NOW testing how much influence this has
# 10 point increase at age 65 vs 75 (10*10)

k.30 <- matrix(c(10,100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

delta.eta <- glht(fit.rqs, linfct=k.30)
exp(confint(delta.eta)$confint)[,1:3]
# This gets wonky - CI is MASSIVE
# Oh! Was really just looking at this a bit wrong
# Need to do PAEE DIFF then PAEE DIFF*AGE

# ---------------------------------------------------------








# ------
# prop hazards adjusted results
# ------


# MODEL
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + tt(PAEEPOS) + rqs.1 + rqs.2 + StandPGS + p22009_a1 + tt(p22009_a1) + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + Townsend + strata(Biological.Sex) + strata(NewEmploy) + strata(NewEduc) + strata(SmokStat_InstChosen), data = datsubrest,  tt=function(x,t,...) x*t)

summary(fit.rqs)


# At 65 vs at 75


# ------
# REPEATING their analysis anyway to see what happens
#  11 more zeros
# ------

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(20 - 31.79)*as.integer(20 > 31.79)
(20 - 38.76)*as.integer(20 > 38.76)
(20 - 46.56)*as.integer(20 > 46.56)
#0
#0
#0
#0


# Creating spline codes to add
qs.1 <- ls.1^2
qs.2 <- ls.2^2
qs.3 <- ls.3^2
qs.4 <- ls.4^2


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4

65*5
75*5

# For age 65
k1 <- matrix(c(5,325,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.977559516502724
# Lower = 0.929129673220318
# Upper = 1.0285137111087



# For age 75
k1 <- matrix(c(5,375,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.945138298754053
# Lower = 0.902242613396589
# Upper = 0.990073391023766



# -----
# In Dempsey:
# Estimate = 0.88
# Lower = 0.80
# Upper = 0.96
# -----

# GENETICS do not further attenuate this difference





# --------
# FIRST new analysis
# EASIEST - trim 5th and 95th percentiles of PAEEPOS
# THIS sort of thing is fairly common - based on belief tails have too much influence on results
# Seems Dempsey may have done a 1st and 99th trim????

# WHAT REMAINS UNCLEAR: DO THEY RECALCULATE SPINE PERCENTILES AFTER THIS? DOES IT MATTER MUCH???
# Should at least test to see how much knots change...

# Logic from "Accelerometer-measured dose-response for physical activity, sedentary time, and mortality in US adults":
# "Our preliminary evaluation of spline results did reveal a strong influence of sparse data (few deaths) in the tails of the exposure distributions, 
# so we trimmed the exposures to minimize this influence as described in figure footnotes"
# --------

# Getting levels - treat this most conservatively first
# COULD consider 5th and 95th...
PAEE1stn99th <- quantile(datsubrest$PAEEPOS, probs = c(0.01, 0.99))
# 1% 16.4558641876511
# 99%71.9728913204384

dim(datsubrest)
# 77,474 beforehand

# Creating this trimmed dataset
datsubTRIM <- subset(datsubrest, PAEEPOS > 16.4558641876511 & PAEEPOS < 71.9728913204384)
dim(datsubTRIM)
# 75,924 NOW


knots <- quantile(datsubTRIM$PAEEPOS, probs = c(0.25, 0.50, 0.75))
# 25% 31.9485438564947
# 50% 38.7595302926387
# 75% 46.3640350624997
# AH! So in terms of knots, this makes VERY little difference
# SO just rerunning models as is... (at least as first test)


# MODEL
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + tt(PAEEPOS) + rqs.1 + rqs.2 + StandPGS + p22009_a1 + tt(p22009_a1) + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + Townsend + strata(Biological.Sex) + strata(NewEmploy) + strata(NewEduc) + strata(SmokStat_InstChosen), data = datsubTRIM,  tt=function(x,t,...) x*t)

summary(fit.rqs)








# ------
# REPEATING their analysis anyway to see what happens
#  11 more zeros
# ------

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(20 - 31.79)*as.integer(20 > 31.79)
(20 - 38.76)*as.integer(20 > 38.76)
(20 - 46.56)*as.integer(20 > 46.56)
#0
#0
#0
#0


# Creating spline codes to add
qs.1 <- ls.1^2
qs.2 <- ls.2^2
qs.3 <- ls.3^2
qs.4 <- ls.4^2


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4

65*5
75*5

# For age 65
k1 <- matrix(c(5,325,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.01062306265588
# Lower = 0.952496620438273
# Upper = 1.07229669151162



# For age 75
k1 <- matrix(c(5,375,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.972087164825597
# Lower = 0.919858980348939
# Upper = 1.02728078564848


# -------
# Strange - this trimming actually ATTENUATES results
# What about 5th and 95th?
# -------

PAEE5thn95th <- quantile(datsubrest$PAEEPOS, probs = c(0.05, 0.95))
# 5% 22.5342097428332
# 95% 59.9099177265671


# Creating this trimmed dataset
datsubTRIM <- subset(datsubrest, PAEEPOS > 22.5342097428332 & PAEEPOS < 59.9099177265671)
dim(datsubrest)
# 69,726 NOW


knots <- quantile(datsubTRIM$PAEEPOS, probs = c(0.25, 0.50, 0.75))
# 25% 32.5449785993642
# 50% 38.7595302926387
# 75% 45.6176720584177
# DOES make more of a difference now.. but keep same for now...


# MODEL
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + tt(PAEEPOS) + rqs.1 + rqs.2 + StandPGS + p22009_a1 + tt(p22009_a1) + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + Townsend + strata(Biological.Sex) + strata(NewEmploy) + strata(NewEduc) + strata(SmokStat_InstChosen), data = datsubTRIM,  tt=function(x,t,...) x*t)

summary(fit.rqs)








# ------
# REPEATING their analysis anyway to see what happens
#  11 more zeros
# ------

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(20 - 31.79)*as.integer(20 > 31.79)
(20 - 38.76)*as.integer(20 > 38.76)
(20 - 46.56)*as.integer(20 > 46.56)
#0
#0
#0
#0


# Creating spline codes to add
qs.1 <- ls.1^2
qs.2 <- ls.2^2
qs.3 <- ls.3^2
qs.4 <- ls.4^2


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4

65*5
75*5

# For age 65
k1 <- matrix(c(5,325,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.05843875393693
# Lower = 0.976689148759078
# Upper = 1.14703086161952




# For age 75
k1 <- matrix(c(5,375,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.01447808207689
# Lower = 0.939355172076961
# Upper = 1.09560878526795


# ---------
# Okay so this trimming issue in others does not appear to exist for us
# Trimming just causes an attenuation of results mostly - as you might expect
# ---------




# --------
# FORMALLY stratify by sex and compare results for Males and Females
# COMMONLY DONE - Ref in Dempsey explains why
# --------

# STRATIFY by gender and run on two separate datasets
datsubrestMALE <- subset(datsubrest, Biological.Sex == "Male")
datsubrestFEMALE <- subset(datsubrest, Biological.Sex == "Female")
# 33,346 for male
# 44,128 for female


fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + tt(PAEEPOS) + rqs.1 + rqs.2 + StandPGS + p22009_a1 + tt(p22009_a1) + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + Townsend + strata(NewEmploy) + strata(NewEduc) + strata(SmokStat_InstChosen), data = datsubrestMALE,  tt=function(x,t,...) x*t)

summary(fit.rqs)



# ------
# REPEATING their analysis anyway to see what happens
#  11 more zeros
# ------

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(20 - 31.79)*as.integer(20 > 31.79)
(20 - 38.76)*as.integer(20 > 38.76)
(20 - 46.56)*as.integer(20 > 46.56)
#0
#0
#0
#0


# Creating spline codes to add
qs.1 <- ls.1^2
qs.2 <- ls.2^2
qs.3 <- ls.3^2
qs.4 <- ls.4^2


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4

65*5
75*5

# For age 65
k1 <- matrix(c(5,325,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.986455510159519
# Lower = 0.930235308920608
# Upper = 1.04607346570536





# For age 75
k1 <- matrix(c(5,375,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.950238371211676
# Lower = 0.900428654768643
# Upper = 1.00280345071318




# -------
# REPEAT FOR FEMALE
# -------



fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + tt(PAEEPOS) + rqs.1 + rqs.2 + StandPGS + p22009_a1 + tt(p22009_a1) + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + Townsend + strata(NewEmploy) + strata(NewEduc) + strata(SmokStat_InstChosen), data = datsubrestFEMALE,  tt=function(x,t,...) x*t)

summary(fit.rqs)



# ------
# REPEATING their analysis anyway to see what happens
#  11 more zeros
# ------

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(20 - 31.79)*as.integer(20 > 31.79)
(20 - 38.76)*as.integer(20 > 38.76)
(20 - 46.56)*as.integer(20 > 46.56)
#0
#0
#0
#0


# Creating spline codes to add
qs.1 <- ls.1^2
qs.2 <- ls.2^2
qs.3 <- ls.3^2
qs.4 <- ls.4^2


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4

65*5
75*5

# For age 65
k1 <- matrix(c(5,325,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.950781467113565
# Lower = 0.857928718761793
# Upper = 1.05368357351564





# For age 75
k1 <- matrix(c(5,375,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.92958580753878
# Lower = 0.847941056636844
# Upper = 1.01909179513596


# -------
# Gender stratified results do NOT appear to differ too much
# Slightly more protective for women and wider CIs...
# -------




# -------
# *************DISCUSS W PATRICK (look at figure)**********
# What about POOLING HR after doing age-specific HRs?
# -------


# -------
# ********DISCUSS W PATRICK***************
# SOME CONFUSION FOR ME:
# They all talk about restricted cubic spline w/ three evenly spaced knots
# IS THAT DOING IT W 25/50/75? OR IS THAT WIH THREE TERMS FOR RCS????
# ALSO DUMB QUESTION: IMPLEMENTATION MATCHES RESTRICTED QUAD SPLINE???
# -------

# CAN try running w/ package...
install.packages('rms')
library(rms)

# Comparing to NO adjustment for PH - just trying to see if results are the same
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)

# ------
# Figuring out rcs...
# OKAY so knots command specifies WHERE knots should be...
# SEEMS TO WORK AND SIMILAR TO WHAT I GOT... BUT
# Why the narrower CIs???? SHOULD BREAK DOWN W PATRICK
# (Obv I have a bias toward this)
# WHAT is rcs doing under the hood that distinguishes it from mine
# 3 evenly spaced knots in restricted cubic spline

# QUESTION FOR PATRICK REALLY: How do we define knots w/ restricted?
# IS HIS EXAMPLE 2 KNOTS OR THREE?
# IF THREE THIS GETS WAY MORE CONFUSING...

# OR NOT... Appears that 0.10/0.50/0.90 is typical
# OH! Is the difference here just knot selection???
# -------

# Create the restricted cubic spline with three knots
rcs <- rcs(datsubrest$PAEEPOS, 3)
# Why does this only yield TWO terms???



# Create the restricted cubic spline with three knots
rcs <- rcs(datsubrest$PAEEPOS, 3)
dim(rcs)
rcs <- as.data.frame(cbind(rcs[ ,1], rcs[ , 2]))
head(rcs)

# MERGING these results w/ data frame
colnames(rcs) <- c("rq.1", "rq.2")

datsubrest <- cbind(datsubrest, rcs)

summary(match(datsubrest$PAEEPOS, datsubrest$rq.1))
# BUT this yields rq.1, which is IDENTICAL to PAEEPOS
# The REST in rq.2...


# PRESUMABLY should get same results as before...
# Fit a Cox proportional hazards regression model using the restricted cubic spline
model <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rq.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)

# Print the model summary
summary(model)

# TRY PUTTING IN DIRECTLY - THIS seems to work...
model <- coxph(Surv(TimeAge, Status) ~ rcs(PAEEPOS, 3) + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)
# So when I do this there are 4 variables?
# base then three others... Why though?
# Weird thing is... IF I increase # knots, nothing else changes...

# BUT THIS *SHOULD* WORK!!!

# Print the model summary
summary(model)

# WEIRD - so comparing values of rqs.1 and rqs.2 vs knots = 2...
# 
rcs(datsubrest$PAEEPOS, knots = 2)

dim(rcs)
rcs <- as.data.frame(cbind(rcs[ ,1], rcs[ , 2], rcs[ ,3]))
head(rcs)

# COMPARING COEFFICIENTS BEFORE DOING MODEL PREDICTION STUFF (& CIs)


# -------
# ANOTHER WAY CHAT GPT SUGGESTED
# WORTH A SHOT
# --------

# Create a restricted cubic spline with three evenly spaced knots for the predictor variable x
spline <- rcs(datsubrest$PAEE, 3)

# Transform the predictor variable x using the spline
x_transformed <- predict(spline, datsubrest$PAEE)

datsubrest$x_transformed <- x_transformed

# Fit the Cox proportional hazards model with the transformed predictor variable x
# GET SAME MODEL IN OTHERWISE
model <- coxph(Surv(TimeAge, Status) ~ PAEEPOS? + x_transformed + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)

#fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)

# Print the summary of the model
summary(model)



# IF THIS FAILS I WILL USE cph AND EXCLUDE MISSINGNESS BEFOREHAND...
datsubrestnona <- as.data.frame(na.omit(datsubrest))
# 75,737 - looks good

# Weird necessary precursor for cph
dd <- datadist(datsubrestnona)
options(datadist="dd")


# as.factor is NOT compatible w/ cph
model <- cph(Surv(TimeAge, Status) ~ rcs(PAEEPOS, 3) + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + Salt_InstChosen + NewAlc + NewOilFish + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + NewEduc + SmokStat_InstChosen, data = datsubrestnona)

summary(model)

coef(model)
# WAIT! This is true but in coef(model) shows TWO OF THEM

# Okay this appears to work.. .BUT ONLY produces 1 coeff for PAEEPOS
# Is this implied to be full thing? It does DIFFER from original
# AND have to see if I can adjust for PH violations...


# Try out some predictions


k1 <- matrix(c(5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(model, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.970496344822111
# Lower = 0.947651698545199
# Upper = 0.99389169750763
# Whoa! This CI is considerably narrower...

# WAIT! This time it adds 31 and yields (extra coeff)
# Estimate = 0.955818063847439
# Lower = 0.913972809669121
# Upper = 0.999579157620462


# Trouble with this: Gives me only one linear and one nonlinear part
# SO hard to tell value to put into nonlinear part
# BECAUSE first knot is BEFORE 30

# ATTEMPTED SOLUTION:
# Ok try to get coeff value by using predict?
datsubrest$RCS <- rcs(datsubrest$PAEE, 3)

# See approximate value in this situation
PAEE30 <- subset(datsubrest, PAEEPOS >= 30 & PAEEPOS < 31)

k1 <- matrix(c(15,0.12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(model, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.914074749553961
# Lower = 0.851032677242581
# Upper = 0.981786798691836
# AGAIN point estimate is actually HIGHER but CI is NARROWER

# AGAIN giving me something different...
# Estimate = 0.873899906102514
# Lower = 0.765324433988939
# Upper = 0.997878823632358



# ATTEMPTED SOLUTION:
# Ok try to get coeff value by using predict?
# datsubrest$RCS <- rcs(datsubrest$PAEE, 3)

# See approximate value in this situation
PAEE40 <- subset(datsubrest, PAEEPOS >= 40 & PAEEPOS < 41)



k1 <- matrix(c(25,3.48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(model, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.860933327806904
# Lower = 0.764264569827214
# Upper = 0.969829328993026

# NEW ONE:
# Estimate = 0.812215286377234
# Lower = 0.673485262618012
# Upper = 0.979522059414414


# ATTEMPTED SOLUTION:
# Ok try to get coeff value by using predict?
# datsubrest$RCS <- rcs(datsubrest$PAEE, 3)

# See approximate value in this situation
PAEE50 <- subset(datsubrest, PAEEPOS >= 50 & PAEEPOS < 51)


k1 <- matrix(c(35,13.906,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(model, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.810881380642398
# Lower = 0.686343013978866
# Upper = 0.958017492828694

# NEW:
# Estimate = 0.783024291995658
# Lower = 0.649696525390103
# Upper = 0.943712976588811

# ATTEMPTED SOLUTION:
# Ok try to get coeff value by using predict?
# datsubrest$RCS <- rcs(datsubrest$PAEE, 3)

# See approximate value in this situation
PAEE60 <- subset(datsubrest, PAEEPOS >= 60 & PAEEPOS < 61)

k1 <- matrix(c(45,27.307,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(model, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.763739295756473
# Lower = 0.616366048401397
# Upper = 0.946349516485262
# THIS one is practically IDENTICAL

# NEW ONE:
# Estimate = 0.766554666824572
# Lower = 0.620426384667808
# Upper = 0.947100367991521




# Does NOT match here - CAN try to specify knot locations for rcs n see


# -------
# TRY THIS
# 10/50/90 as knots and do it manually then compare...
# -------

knots <- quantile(datsubrest$PAEEPOS, probs = c(0.10, 0.50, 0.90))
knots
# 10% 25.9491318778412
# 50% 38.7595302926387
# 90% 54.5613992585524

# CONFIRMED via rcs object that knots are:
# 25.94913, 38.75953, 54.56140





datsubrest$ls.1 <- (datsubrest$PAEEPOS - 25.9491318778412)*as.integer(datsubrest$PAEEPOS > 25.9491318778412)
datsubrest$ls.2 <- (datsubrest$PAEEPOS - 38.7595302926387)*as.integer(datsubrest$PAEEPOS > 38.7595302926387)
datsubrest$ls.3 <- (datsubrest$PAEEPOS - 54.5613992585524)*as.integer(datsubrest$PAEEPOS > 54.5613992585524)



# Unrestricted quadratic spline
datsubrest$qs.1 <- datsubrest$ls.1^3
datsubrest$qs.2 <- datsubrest$ls.2^3
datsubrest$qs.3 <- datsubrest$ls.3^3




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3




fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)






# ------
# REPEATING their analysis anyway to see what happens
# ------

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(20 - 31.79)*as.integer(20 > 31.79)
(20 - 38.76)*as.integer(20 > 38.76)
(20 - 46.56)*as.integer(20 > 46.56)
#0
#0
#0
#0


# Creating spline codes to add
qs.1 <- ls.1^3
qs.2 <- ls.2^3
qs.3 <- ls.3^3


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.954739251719029
# Lower = 0.909414995908
# Upper = 1.00232241921951


k1 <- matrix(c(15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.870270642919808
# Lower = 0.752118608421359
# Upper = 1.00698345107791














# ---------
# What is this difference in PAEE by percentile in practical terms?
# Dempsey vs our current approach...
# Point: In many of these studies, take a HUGE difference in PA...
# OURS appears to be a v small diff, which could be driving equiv results...
# ---------

# Dempsey:
# PAEE 15 vs 20/30/40/50/60

# Strain:
# PAEE 15 vs 20/30

# Us:
# PAEE 49 vs 30/36/41
# FULL RANGE OF COMPARISON IS JUST OVER 1.5 SDs...

# Interpretation of PAEE here...:

# "what is the conversion between physical activity energy expenditure as kj/kg/day and METs"
# For example, if an individual has a PAEE of 15 kJ/kg/day and weighs 70 kilograms, the conversion to METs would be:
  
#  15 kJ/kg/day / 1440 = 0.0104 kJ/kg/min

# 0.0104 kJ/kg/min / 20.1 = 0.00052 METs

# Therefore, a PAEE of 15 kJ/kg/day for an individual weighing 70 kilograms is equivalent to approximately 0.00052 METs. 
# This value is very low and suggests that the individual is not engaging in much physical activity.
# However, it's important to note that the conversion between PAEE and METs is only an approximation and may vary depending on the specific activity and individual characteristics.

# ME: THIS CALCULATION APPEARS OFF.. Should NOT divide by 1440 for the day here
# INSTEAD JUST DO
# 15 kJ/kg/day / 20.1 = 0.75 - FAR MORE REASONABLE

# FOR CONTEXT:
# While it's difficult to estimate average METs per day,
# Typically 1.6 for men and 1.3 for women in US
# SO would translate to about 30-35 kJ/kg/day...

# IF THIS CONVERSION IS CORRECT:
# Makes sense that 15 would be V sedentary person...
# AND w/ 60th percentile at 38 makes sense too - generally more active than avg

# WHAT IF WE TOOK LOWER BASELINE???? (Like 10th Percentile or something???)





# ---------
# DO FAVORED ANALYSES WITH TERTILE SPLITS INSTEAD OF QUINTILES (for comp or modeling???)
# MORE in line w/ lit using subjective PA...
# ---------





# ---------
# When I've settled on way to present it, get things right:
# GET imputation to work
# What age groups? Pooled???
# What percentile comps????
# ADD ALL CONFOUNDERS FROM LIA/ME IN PRES
# From Lia: Read in subjective PA over time for cmoparison, ADD OCCUPATION, URBANICITY??
# ----------





# ------------
# HAVE TO REPEAT ALL ANALYSES WITH MVPA AND VIGOROUS PA TO ACCOUNT FOR INTENSITY
# Dempsey approach - interact log(MVPA) or log(Vigorous) w/ knots of the spline
# THEN set PA volume at some level and increase %PA intensity and interpret
# SHOULD ULTIMATELY BE IDENTICAL MODEL WITH THIS ADDITION...

# WAIT: WHY log transform %PAEE from MVPA? Isn't part of advantage of log
# That it can be interpreted as % change?

# ------------



# -------
# MAKE interaction w/ rcs and manually
# NOTE: JUST using %MVPA here
# -------

# ---------
# THERE is actually an update to rms package that explicitly allows for interact
# https://cran.r-project.org/web/packages/interactionRCS/vignettes/vignette.html#Introduction
# ---------


# Create a restricted cubic spline with 3 knots interacted with z using rcs()
# spline <- rcs(x, 3, interact = "logMVPA")



datsubrestnona <- as.data.frame(na.omit(datsubrest))
# 75,737 - looks good

# Weird necessary precursor for cph
dd <- datadist(datsubrestnona)
options(datadist="dd")

# BASED ON CODE IN VIGNETTE FOR RCS INTERACTION


# FIRST just run this interaction model
model <- cph(Surv(TimeAge, Status) ~ rcs(PAEEPOS, 3)*PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + Salt_InstChosen + NewAlc + NewOilFish + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + NewEduc + SmokStat_InstChosen, data = datsubrestnona, x = TRUE, y = TRUE)

summary(model)
coef(model)

install.packages('interactionRCS')
library(interactionRCS)

# Effect of PercentMVPA when PAEE = 15
HR_rcs_delta <- intEST( var2values = c(15,20,30,40,50), model = model , data = datsubrest , var1 ="PercentMVPA", var2="PAEEPOS" ,ci.method = "delta")

HR_rcs_delta <- rcsHR( var2values = c(15,20,30,40,50), model = model , data = datsubrest , var1 ="PercentMVPA", var2="PAEEPOS" ,ci.method = "delta")
HR_rcs_delta
# This gives an implausibly wide range...
# It almost looks like HR and CI_L are on different scale than CI_U
# PLUS would be in the wrong direction...


# ----------
# What if I do this part manually?
# ----------


# PAEE at 15 and MVPA increases from 10 to 20
k1 <- matrix(c(0,0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,300,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(model, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Preposterous results...
# Estimate = 0.00951587309144118
# Lower = 8.21655976066918e-06
# Upper = 11.0206513833035




# THIS is treating MVPA LINEARLY (seems Strain did but Dempsey didn't)
# Given collinearity, unsurprisingly pushes PAEE to insig (AND MVPA though..)



# Getting predictions from this approach
# Seems we just have to insert value for PAEE and MVPA
# SHOULD have to set PAEE at some value





k1 <- matrix(c(5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(model, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]





datsubrest$ls.1 <- (datsubrest$PAEEPOS - 25.9491318778412)*as.integer(datsubrest$PAEEPOS > 25.9491318778412)
datsubrest$ls.2 <- (datsubrest$PAEEPOS - 38.7595302926387)*as.integer(datsubrest$PAEEPOS > 38.7595302926387)
datsubrest$ls.3 <- (datsubrest$PAEEPOS - 54.5613992585524)*as.integer(datsubrest$PAEEPOS > 54.5613992585524)



# Unrestricted quadratic spline
datsubrest$qs.1 <- datsubrest$ls.1^3
datsubrest$qs.2 <- datsubrest$ls.2^3
datsubrest$qs.3 <- datsubrest$ls.3^3




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3


# Creating spline regression for logPercentMVPA also
knots <- quantile(datsubrest$PercentMVPA, probs = c(0.10, 0.50, 0.90))
knots
# 10% 0.213109755634595
# 50% 0.35725720776084
# 90% 0.507174876692717

datsubrest$ls.1MVPA <- (datsubrest$PercentMVPA - 0.213109755634595)*as.integer(datsubrest$PercentMVPA > 0.213109755634595)
datsubrest$ls.2MVPA <- (datsubrest$PercentMVPA - 0.35725720776084)*as.integer(datsubrest$PercentMVPA > 0.35725720776084)
datsubrest$ls.3MVPA <- (datsubrest$PercentMVPA - 0.507174876692717)*as.integer(datsubrest$PercentMVPA > 0.507174876692717)



# Unrestricted quadratic spline
datsubrest$qs.1MVPA <- datsubrest$ls.1MVPA^3
datsubrest$qs.2MVPA <- datsubrest$ls.2MVPA^3
datsubrest$qs.3MVPA <- datsubrest$ls.3MVPA^3



# Doing the restricting
datsubrest$rqs.1MVPA <- datsubrest$qs.1MVPA - datsubrest$qs.3MVPA
datsubrest$rqs.2MVPA <- datsubrest$qs.2MVPA - datsubrest$qs.3MVPA




fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + PercentMVPA + rqs.1 + rqs.2 + rqs.1MVPA + rqs.2MVPA + rqs.1MVPA*rqs.1 + rqs.2MVPA*rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)

# --------
# THIS is fitting manually based on Dempsey (I think...)
# INTERACTIONS are between orthogonal spline variables, NOT main effects...
# --------

# Getting predictions from this approach
length(coef(fit.rqs))


# 10 to 20 MVPA
k1 <- matrix(c(0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]

# ALSO yielded something absurd
# Estimate = 0.000740123156929698
# Lower = 9.67653980860083e-09
# Upper = 56.6093147197819

# ----------
# I THINK THE COLLINEARITY ISSUE IS JUST KILLING US...
# IS *THIS* why they took the log???
# TRY STANDARDIZING BOTH BEFOREHAND AND SEE IF RESULTS ARE WACKY STILL

# Sources:
# https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/

# SHOULD COMPARE VIF/correlation matrix before and after
# -----------






# -----------
# OR just fit spline to PAEE and interact MVPA w/ it
# LINEAR MVPA interacted w/ spline variables???
# -----------




