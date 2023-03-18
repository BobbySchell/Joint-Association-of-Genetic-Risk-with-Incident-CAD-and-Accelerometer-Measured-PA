
dx download FINALANALYSISDATAPAPER3.csv


datsub <- read.csv("FINALANALYSISDATAPAPER3.csv")




# List of confounders
# SeasonWear, PercentMVPA, PercentVigorous, PAEETRANSFORM
# SmokStat_InstChosen, SleepDur_InstChosen, AlcIntake_InstChosen, OilyFish_InstChosen
# Salt_InstChosen, ProcMeat_InstChosen, EmploymentStatus_InstChosen, MotherHeartDisease,
# FatherHeartDisease, CholMeds, BPMeds, Veggie, Fruit, BMI_InstChosen
# MobilProbs, Townsend, Biological.Sex
# TimeYear, Status, StatusThree
# AgeBaseline
# PGS, StandPGS

# ALSO ADDED (on 3/1):
# 10 principal components
# Educational attainment
# REGION


# NOTE: SHOULD BE USING TRANSFORM VERSION OF PAEE (shouldn't affect MVPA or vig since they're %s and transform is linear)


# START by restricting dataset to only what is needed - exposures, outcome, confounders
datsubrest <- datsub[ , c("SeasonWear", "PercentMVPA", "LogPercentMVPA", "LogPercentVigorous", "PercentVigorous", "OverallPAEETRANSFORM",
                          "SmokStat_InstChosen", "SleepDur_InstChosen", "AlcIntake_InstChosen",
                          "OilyFish_InstChosen", "Salt_InstChosen", "ProcMeat_InstChosen", "EmploymentStatus_InstChosen",
                          "MotherHeartDisease", "FatherHeartDisease", "CholMeds", "BPMeds", "Veggie", "Fruit",
                          "BMI_InstChosen", "MobilProbs", "Townsend", "Biological.Sex", "TimeYear", "Status",
                          "StatusThree", "AgeBaseline", "PGS", "StandPGS", "p22009_a1", "p22009_a2", "p22009_a3",
                          "p22009_a4", "p22009_a5", "p22009_a6", "p22009_a7", "p22009_a8", "p22009_a9", "p22009_a10")]



# THEN look at extent of missingness
summary(is.na(datsubrest))
# Out of 77,474 inds total
# NO missingness for outcome variable or exposures (GOOD! Shouldn't be)


install.packages('survival')
library(survival)


install.packages('ggplot2')
library(ggplot2)

datsubrest$TimeAge <-datsubrest$TimeYear + datsubrest$AgeBaseline


# GETTING FULL CONFOUNDER LIST HERE
fit.linear <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10, data = datsubrest)


summary(fit.linear)


# -----------------
# Clearly NO STRONG PRS and PA interaction (at least linearly... Suspect this is true of spline too)
# CAN DO LIKE NEJM - Lifestyle factors for inds in genetic risk categories and how it changes
# LOOK AT DIFF IN PREDICTION FROM DIFF VALUES
# -----------------

# Getting 10th to 100th(?) percentile
PGSDECILE <- quantile(datsubrest$StandPGS, probs = seq(0, 1, 0.1))
# 0% -4.35013757268161
# 10% -1.2874939740801
# 20% -0.846868164911578
# 30% -0.533799378613378
# 40% -0.266874315609926
# 50% -0.0168291390930716
# 60% 0.236609243902655
# 70% 0.504869885863517
# 80% 0.821322441664612
# 90% 1.26151998986052
# 100% 4.47222889518045

PAEEDECILE <- quantile(datsubrest$OverallPAEETRANSFORM, probs = seq(0, 1, 0.1))
# 0% -1.55325339731384
# 10% 25.9491318778412
# 20% 30.1581806907346
# 30% 33.2842529902615
# 40% 36.0267497376078
# 50% 38.7595302926387
# 60% 41.5283167027557
# 70% 44.7140637230322
# 80% 48.6690567680804
# 90% 54.5613992585524
# 100% 149.071716407243


# --------
# getting mean values for confounders and setting there
# NOT changing between comparisons so could technically set these at whatever
# MANY BEING TREATED AS NUMERIC THAT ARE ACTUALLY FACTORS - Will have to figure that out...
# --------

summary(datsubrest$SeasonWear)
summary(datsubrest$SmokStat_InstChosen)
summary(datsubrest$SleepDur_InstChosen)
summary(datsubrest$AlcIntake_InstChosen)
summary(datsubrest$OilyFish_InstChosen)
summary(datsubrest$Salt_InstChosen)
summary(datsubrest$ProcMeat_InstChosen)
summary(datsubrest$EmploymentStatus_InstChosen)
summary(datsubrest$MotherHeartDisease)
summary(datsubrest$FatherHeartDisease)
summary(datsubrest$Veggie)
summary(datsubrest$Fruit)
summary(datsubrest$MobilProbs)
summary(datsubrest$Townsend)
summary(datsubrest$Biological.Sex)
summary(datsubrest$REGION)


# ------
# RUN AGAIN W PA SPLINES
# OVERALL PA AS NONLINEAR AND SEE WHAT HAPPENS TO THIS ESTIMATE (now all relative to PAEE value)
# LINEAR SPLINES
# BIG QUESTION: Doesn't REFERENCE LEVEL OF PAEE MATTER W SPLINES?? Probably not w/o interaction??
# ------



knots <- quantile(datsubrest$OverallPAEETRANSFORM, probs = c(0.20, 0.40, 0.60, 0.80))
# Using these four knots
# 20% 30.1581806907346
# 40% 36.0267497376078
# 60% 41.5283167027557
# 80% 48.6690567680804



datsubrest$ls.1 <- (datsubrest$OverallPAEETRANSFORM - 30.16)*as.integer(datsubrest$OverallPAEETRANSFORM > 30.16)
datsubrest$ls.2 <- (datsubrest$OverallPAEETRANSFORM - 36.03)*as.integer(datsubrest$OverallPAEETRANSFORM > 36.03)
datsubrest$ls.3 <- (datsubrest$OverallPAEETRANSFORM - 41.53)*as.integer(datsubrest$OverallPAEETRANSFORM > 41.53)
datsubrest$ls.4 <- (datsubrest$OverallPAEETRANSFORM - 48.67)*as.integer(datsubrest$OverallPAEETRANSFORM > 48.67)

fit.ls <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + StandPGS + ls.1 + ls.2 + ls.3 + ls.4 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10, data = datsubrest)


summary(fit.ls)


# NOW MIXING THESE UP...
# NOW should be from 0 to 100 (have to hold constant at each and let PGS vary and vice versa)
OverallPAEETRANSFORM <- c(rep(-1.55325339731384, times = 11), rep(25.9491318778412, times = 11), rep(30.1581806907346, times = 11), rep(33.2842529902615, times = 11),
                          rep(36.0267497376078, times = 11), rep(38.7595302926387, times = 11), rep(41.5283167027557, times = 11), rep(44.7140637230322, times = 11),
                          rep(48.6690567680804, times = 11), rep(54.5613992585524, times = 11), rep(149.071716407243, times = 11))
# Creates 121 values as it should


# NOW should be from 0 to 100
StandPGS <- rep(c(-4.35013757268161, -1.2874939740801, -0.846868164911578, -0.533799378613378, -0.266874315609926, -0.0168291390930716, 
                  0.236609243902655, 0.504869885863517, 0.821322441664612, 1.26151998986052, 4.47222889518045), times = 11)
# Creating reps this way allows for EACH combo of PA and PGS
# Creates 121 values as it should

# HOLDING CONFOUNDERS ALL CONSTANT AT MEAN
SeasonWear <- rep("Summer", times = 121)

SmokStat_InstChosen <- rep(mean(datsubrest$SmokStat_InstChosen), times = 121)
SleepDur_InstChosen <- rep(mean(datsubrest$SleepDur_InstChosen), times = 121)
AlcIntake_InstChosen <- rep(mean(datsubrest$AlcIntake_InstChosen), times = 121)
OilyFish_InstChosen <- rep(mean(datsubrest$OilyFish_InstChosen), times = 121)
Salt_InstChosen <- rep(mean(datsubrest$Salt_InstChosen), times = 121)
ProcMeat_InstChosen <- rep(mean(datsubrest$ProcMeat_InstChosen), times = 121)
EmploymentStatus_InstChosen <- rep(mean(datsubrest$EmploymentStatus_InstChosen), times = 121)
MotherHeartDisease <- rep(mean(datsubrest$MotherHeartDisease), times = 121)
FatherHeartDisease <- rep(mean(datsubrest$FatherHeartDisease), times = 121)
Veggie <- rep(mean(datsubrest$Veggie), times = 121)
Fruit <- rep(mean(datsubrest$Fruit), times = 121)
MobilProbs <- rep("I have no problems in walking about", times = 121)
Townsend <- rep(mean(datsubrest$Townsend), times = 121)
Biological.Sex <- rep(mean(datsubrest$Biological.Sex), times = 121)
# REGION <- rep(mean(datsubrest$REGION), times = 121)

p22009_a1 <- rep(mean(datsubrest$p22009_a1), times = 121)
p22009_a2 <- rep(mean(datsubrest$p22009_a2), times = 121)
p22009_a3 <- rep(mean(datsubrest$p22009_a3), times = 121)
p22009_a4 <- rep(mean(datsubrest$p22009_a4), times = 121)
p22009_a5 <- rep(mean(datsubrest$p22009_a5), times = 121)
p22009_a6 <- rep(mean(datsubrest$p22009_a6), times = 121)
p22009_a7 <- rep(mean(datsubrest$p22009_a7), times = 121)
p22009_a8 <- rep(mean(datsubrest$p22009_a8), times = 121)
p22009_a9 <- rep(mean(datsubrest$p22009_a9), times = 121)
p22009_a10 <- rep(mean(datsubrest$p22009_a10), times = 121)




# Creating spline codes to add
ls.1 <- (OverallPAEETRANSFORM - 30.16)*as.integer(OverallPAEETRANSFORM > 30.16)
ls.2 <- (OverallPAEETRANSFORM - 36.03)*as.integer(OverallPAEETRANSFORM > 36.03)
ls.3 <- (OverallPAEETRANSFORM - 41.53)*as.integer(OverallPAEETRANSFORM > 41.53)
ls.4 <- (OverallPAEETRANSFORM - 48.67)*as.integer(OverallPAEETRANSFORM > 48.67)


val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit, ls.1, ls.2, ls.3, ls.4, as.factor(SeasonWear), SmokStat_InstChosen, SleepDur_InstChosen, AlcIntake_InstChosen, OilyFish_InstChosen, Salt_InstChosen, ProcMeat_InstChosen,
                           EmploymentStatus_InstChosen, MotherHeartDisease, FatherHeartDisease, Veggie, Fruit, as.factor(MobilProbs), Townsend, as.factor(Biological.Sex), p22009_a1, p22009_a2, p22009_a3, p22009_a4, p22009_a5, p22009_a6, p22009_a7, p22009_a8, p22009_a9, p22009_a10))


pred1 <- predict(fit.ls, newdata = val, se = TRUE)


exp(0.350378969830493)
exp(0.167270451397939)
# 1.419605
# 1.182074


# Compute error bands (2*SE)
se_bands = with(pred1, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)
# ADDING CIs

exp(se_bands)
# upper	lower
#	1.603145	1.257078
#	1.383297	1.010122

write.csv(val, "PredValsLinearSpline.csv")
write.csv(pred1, "PredMatrixLinearSpline.csv")
write.csv(se_bands, "SEBandsMatrixLinearSpline.csv")


# ------
# Restricted quadratic spline
# ------



# Unrestricted quadratic spline
datsubrest$qs.1 <- datsubrest$ls.1^2
datsubrest$qs.2 <- datsubrest$ls.2^2
datsubrest$qs.3 <- datsubrest$ls.3^2
datsubrest$qs.4 <- datsubrest$ls.4^2




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.4
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.4
datsubrest$rqs.3 <- datsubrest$qs.3 - datsubrest$qs.4


fit.rqs <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + StandPGS + rqs.1 + rqs.2 + rqs.3 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10, data = datsubrest)


summary(fit.rqs)


# Creating spline codes to add
qs.1 <- ls.1^2
qs.2 <- ls.2^2
qs.3 <- ls.3^2
qs.4 <- ls.4^2


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit, rqs.1, rqs.2, rqs.3, as.factor(SeasonWear), SmokStat_InstChosen, SleepDur_InstChosen, AlcIntake_InstChosen, OilyFish_InstChosen, Salt_InstChosen, ProcMeat_InstChosen,
                           EmploymentStatus_InstChosen, MotherHeartDisease, FatherHeartDisease, Veggie, Fruit, as.factor(MobilProbs), Townsend, as.factor(Biological.Sex), p22009_a1, p22009_a2, p22009_a3, p22009_a4, p22009_a5, p22009_a6, p22009_a7, p22009_a8, p22009_a9, p22009_a10))


pred2 <- predict(fit.rqs, newdata = val, se = TRUE, interval = "confidence")
# DOES adding interval = confidence change SE value??? (SHOULD only affect interval calc)

# Compute error bands (2*SE)
se_bands = with(pred2, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)
# ADDING CIs

exp(se_bands)
# upper	lower
#	0.8117284	0.6725787
#	0.6988033	0.5605129

# USE POINT ESTIMATES AND CONFIDENCE INTERVAL DATA FRAME
# CAN ADD AND SUBTRACT WAY TO ESTIMATES HERE

write.csv(val, "PredValsQuadSpline.csv")
write.csv(pred2, "PredMatrixQuadSpline.csv")
write.csv(se_bands, "SEBandsMatrixQuadSpline.csv")


# Uploading these values to take offline and use to fit figures
dx upload PredValsLinearSpline.csv
dx upload PredMatrixLinearSpline.csv
dx upload SEBandsMatrixLinearSpline.csv

dx upload PredValsQuadSpline.csv
dx upload PredMatrixQuadSpline.csv
dx upload SEBandsMatrixQuadSpline.csv



# THIS PART DONE OFFLINE!!!
# Using Quadratic Spline Values for these figures
# WAS HAVING SERIOUS TROUBLE WITH CONVERTING SE WITH NEW COMPARISON GROUP:
# Chat GPT gave me answer via:
# how to interpret standard errors in continuous cox model comparing two different values
# DELTA METHOD?????
# SE(diff) = sqrt(SE1^2 + SE2^2 - 2rSE1*SE2)
# where SE1 and SE2 are the standard errors of the hazard ratios for each value, and r is the correlation between the two estimates.
# CI(diff) = diff +/- z*SE(diff)


# CURRENT SOLUTION:
# Right now... It appears that MULTIPLYING the standard errors (in log form) then calculating CIs and exp creates reasonable values
# No clue WHY this works but it's weirdly reliable...

# ---------
# FIGURES to compare HR for different genetic and PA groups
# DO LINEAR VS RESTRICTED QUADRATIC SPLINE
# COMPARING LOW/HIGH GENETIC RISK AND LOW/HIGH PA
# 80th/20th percentile chosen arbitrarily....
# ---------

# Uploading prediction vals, predictions, CIs for figures



# Selecting coefficients from No Highschool model (Obs)
NoHSObsCoeff <- 1.49

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLNoHSObs <- 1.41
ULNoHSObs <- 1.63


# Selecting coefficients from No Highschool model (IV)
NoHSIVCoeff <- 1.26

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLNoHSIV <- 1.13
ULNoHSIV <- 1.36



PointCompNoHSIV <- c(LLNoHSIV, NoHSIVCoeff, ULNoHSIV)
PointCompNoHSIV <- as.data.frame(PointCompNoHSIV)

PointCompNoHSObs <- c(LLNoHSObs, NoHSObsCoeff, ULNoHSObs)
PointCompNoHSObs <- as.data.frame(PointCompNoHSObs)

PointyNoHS <- cbind(PointCompNoHSIV, PointCompNoHSObs)

# Transpose
PointyTNoHS <- t(PointyNoHS)
PointyTNoHS <- as.data.frame(PointyTNoHS)

colnames(PointyTNoHS) <- c("LL","Mean","UL")

PointyTNoHS$Model <- c("Low Genetic Risk/High Overall PA", "Low Genetic Risk/Low Overall PA")


# Selecting coefficients from Highschool model (Obs)
HSObsCoeff <- 0.63

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLHSObs <- 0.56
ULHSObs <- 0.67


# Selecting coefficients from No Highschool model (IV)
HSIVCoeff <- 0.74

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLHSIV <- 0.70
ULHSIV <- 0.81



PointCompHSIV <- c(LLHSIV, HSIVCoeff, ULHSIV)
PointCompHSIV <- as.data.frame(PointCompHSIV)

PointCompHSObs <- c(LLHSObs, HSObsCoeff, ULHSObs)
PointCompHSObs <- as.data.frame(PointCompHSObs)

PointyHS <- cbind(PointCompHSIV, PointCompHSObs)

# Transpose
PointyTHS <- t(PointyHS)
PointyTHS <- as.data.frame(PointyTHS)

colnames(PointyTHS) <- c("LL","Mean","UL")

PointyTHS$Model <- c("High Genetic Risk/High Overall PA", "High Genetic Risk/Low Overall PA")


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
                     xmax = UL)) + geom_point(shape = 15, size  = 4, aes(color = Genetic.Risk)) + theme_bw() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, subtitle = "High = 80th Percentile, Low = 20th Percentile", axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio")


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


