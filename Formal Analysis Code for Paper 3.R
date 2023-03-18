# -------------
# Formal Analysis Code for Paper 3
# "Genetic Risk Affects the Relationship Between Objectively Measured Physical Activity Intensity and Incident Coronary Artery Disease"
# Or maybe "Genetic Risk Affects the Relationship Between Physical Activity Intensity and Incident Coronary Artery Disease"
# -------------

dx download FINALANALYSISDATAPAPER3.csv



datsub <- read.csv("FINALANALYSISDATAPAPER3.csv")

# ---------------------------------------------------------------------------------
# -----------------
# ONE persisting issues after dataset creation:

# 1. MICE (Imputation) - To run smoothly, will want to subset to ONLY variables needed for analysis
# THIS is straightforward but wanted to keep dataset intact for processing part so doing here instead
# -----------------


# -------
# AFTER this step - consider imputation
# First get scope of problem
# applies to MANY of the variables
# Performing MICE
# -------

install.packages('mice')
library(mice)

colnames(datsub)

# List of confounders
# SeasonWear, PercentMVPA, PercentVigorous, PAEETRANSFORM
# SmokStat_InstChosen, SleepDur_InstChosen, AlcIntake_InstChosen, OilyFish_InstChosen
# Salt_InstChosen, ProcMeat_InstChosen, EmploymentStatus_InstChosen, MotherHeartDisease,
# FatherHeartDisease, CholMeds, BPMeds, Veggie, Fruit, BMI_InstChosen
# MobilProbs, Townsend, Biological.Sex
# TimeYear, Status, StatusThree
# AgeBaseline, Genetic PCs, REGION, Educ Attain
# PGS, StandPGS


# Need to add: Urbanicity & Job Code & ETHNICITY& Geno Array

# NOTE: SHOULD BE USING TRANSFORM VERSION OF PAEE (shouldn't affect MVPA or vig since they're %s and transform is linear)


# START by restricting dataset to only what is needed - exposures, outcome, confounders
datsubrest <- datsub[ , c("SeasonWear", "PercentMVPA", "LogPercentMVPA", "LogPercentVigorous", "PercentVigorous", "OverallPAEETRANSFORM",
                       "SmokStat_InstChosen", "SleepDur_InstChosen", "AlcIntake_InstChosen",
                       "OilyFish_InstChosen", "Salt_InstChosen", "ProcMeat_InstChosen", "EmploymentStatus_InstChosen",
                       "MotherHeartDisease", "FatherHeartDisease", "CholMeds", "BPMeds", "Veggie", "Fruit",
                       "BMI_InstChosen", "MobilProbs", "Townsend", "Biological.Sex", "TimeYear", "Status",
                       "StatusThree", "AgeBaseline", "PGS", "StandPGS", "p22009_a1", "p22009_a2", "p22009_a3",
                       "p22009_a4", "p22009_a5", "p22009_a6", "p22009_a7", "p22009_a8", "p22009_a9", "p22009_a10", "REGION", "EA.Inst.0", "EA.Inst.1", "EA.Inst.2", "EA.Inst.3")]


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



# SHOULD MAKE SURE NOT TO DO ANYTHING W DATE MISSINGNESS...
# Just using defaults here to start - should make rel little diff but can mess w/...
imputed_Data <- mice(datsubrest, m=5, maxit = 50, method = 'fastpmm', seed = 500)
# Note: This takes a while to run
# fastpmm didn't work so did default and timed out at: 

# Confirming no missingness now
summary(is.na(imputed_Data))
# WEIRD result - gotta look into this package more
# Use COMPLETE CASE ANALYSIS FOR NOW
# Took 12 mins


# CONFIRM that imputation worked (NOT done for outcome or exposure)
# SHOULD WE EXCLUDE OUTCOME WHEN IMPUTING? Probably though there are arguments for inclusion from:
# https://d1wqtxts1xzle7.cloudfront.net/74617414/j.jclinepi.2006.01.00920211113-24443-1fd7xz0-libre.pdf?1636821915=&response-content-disposition=inline%3B+filename%3DUsing_the_outcome_for_imputation_of_miss.pdf&Expires=1677462351&Signature=R0dwfK7437k~1WK3UeRcULA77Xgd95hXHbYT8dWS4I2XBGoFp1rI5KVuiJ3oajfI2PzV82EM23CTv1dONqGx5TshXrewyMKUPyS3Ndrdv7WTyInxz~1KXNNN1n8hibfo-HdbQgBkSlfWvfg8MwE6Wezr9q53ag-aU1~m8iYF1t9PsYaWLn4fxkq8h1YEEtGYVZmtIRkSIZTpZGCh5QfohHpxiklpg6gu-aIET9LaGqw49JH94iAVv2oWOPVUFYzc9mJigoFy2Q13HzwyMPnGhUYbv0aAJnGg4-p~G-Bh6Rl8XzWyiSRxVYnBK7IWWP6H4ILb1ToHB1Z3e5nrTJq~nA__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA


# ----------------------
# 3/6/2023
# AFTER creating both imputed and not imputed dataset...
# Refer to: "Code for Creating Results in Paper 3.R"
# The rest of this page is just a reference to past work
# That R file shows how to create results following Patrick's advice
# ----------------------











# NOW that we have a dataset w/ NO MISSINGNESS, break genetic risk score into tertiles
# To use to make groups later
Tertile <- quantile(datsubrest$StandPGS, c(0:3/3))
# 0% -4.3501375726816
# 33.33333% -0.442665039268912
# 66.66667% 0.413328397665102
# 100% 4.47222889518045
# SO cutoffs would be at -0.44 and 0.41


# CREATE a Table 1 OVERALL (that contains genetic risk groups) and BY GENETIC RISK GROUP
install.packages('stargazer')
library(stargazer)

stargazer(datsubrest, summary=TRUE, title = "Descriptive Statistics for Accelerometer Data Cohort", header = FALSE, type = 'latex')
# Only summarizes numeric results, so manually inserting factors into latex table


Tertile <- quantile(datsubrest$StandPGS, c(0:3/3))
# 0% -4.3501375726816
# 33.33333% -0.442665039268912
# 66.66667% 0.413328397665102
# 100% 4.47222889518045
# SO cutoffs would be at -0.44 and 0.41

datsubrestGRS1 <- subset(datsubrest, StandPGS <= -0.442)
datsubrestGRS2 <- subset(datsubrest, StandPGS > -0.442 & StandPGS <= 0.413)
datsubrestGRS3 <- subset(datsubrest, StandPGS > 0.413)

stargazer(datsubrestGRS1, summary=TRUE, title = "Descriptive Statistics for Lowest Genetic Risk Group", header = FALSE, type = 'latex')
# Only summarizes numeric results, so manually inserting factors into latex table

stargazer(datsubrestGRS3, summary=TRUE, title = "Descriptive Statistics for Highest Genetic Risk Group", header = FALSE, type = 'latex')
# Only summarizes numeric results, so manually inserting factors into latex table



install.packages('flextable')
library(flextable)

install.packages('survival')
library(survival)

# CREATING Figures for person-years
pyearsum <- summary(pyears(Surv(TimeYear, Status) ~ Status, data = datsubrest, scale = 1))
# 10,771 individuals (7,293 censored, 2,198 CVD, 1,220 death)

# Total follow up time in person years of 68,934, 18,718, and 16,561 (scale = 1 because in years)



Summed <- rbind(pyearsum$pyears, pyearsum$n)
Summed <- as.data.frame(Summed)
AvgPYears <- c(6.69, 3.58)
Summed <- rbind(Summed, AvgPYears)

rownames(Summed) <- c("Total Person-Years", "Number of Individuals", "Average Person-Years")

# flextable can't add... SO adding rownames manually
Summed$Category <- c("Total Person-Years", "Number of Individuals", "Average Person-Years")

install.packages('kableExtra')
library(kableExtra)

# FLEXTABLE DID NOT WORK

Summed %>%
  kbl(caption = "Person-Years Contributed by Status") %>%
  kable_classic(full_width = F, html_font = "Cambria")



# ------
# Creating LINEAR then SPLINE cox models
# PAEE overall then PAEE by genetic tertile
# %MVPA overall then PAEE by genetic tertile
# %Vigorous overall then PAEE by genetic tertile
# Restricted quadratic spline comparison
# ------


datsubrest$PGSTertile <- ifelse(datsubrest$StandPGS <= -0.442, 1,
                                ifelse(datsubrest$StandPGS > -0.442 & datsubrest$StandPGS <= 0.413, 2, 3))



install.packages("survival")
library(survival)

install.packages('ggplot2')
library(ggplot2)

datsubrest$TimeAge <-datsubrest$TimeYear + datsubrest$AgeBaseline

# Using PARTIAL set of confounders
fit.linear <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(fit.linear)


# not specifying type... just to see
phat.linear <- predict(fit.linear)

# Convert from log HR to HR
phat.linear <- exp(phat.linear)

# Creating df w x and y preds in it to simplify
plotlin <- cbind(datsubrest$OverallPAEETRANSFORM, phat.linear)
plotlin <- as.data.frame(plotlin)

# Remove duplicate observations
plotlin <- plotlin[!duplicated(plotlin), ]

colnames(plotlin) <- c("Overall Physical Activity (in kJ/kg/day)","Hazard Ratio for CAD")


plotlin %>%
  ggplot(aes(x = `Overall Physical Activity (in kJ/kg/day)`, y = `Hazard Ratio for CAD`)) + geom_line() 
# This ONLY works WITHOUT controlling for anything... USING AGE AT LEAST

# --------
# LINEAR SPLINE
# --------

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

fit.ls <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + ls.1 + ls.2 + ls.3 + ls.4 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(fit.ls)


# not specifying type... just to see
phat.ls <- predict(fit.ls)

# Convert from log HR to HR
phat.ls <- exp(phat.ls)

# Creating df w x and y preds in it to simplify
plotls <- cbind(datsubrest$OverallPAEETRANSFORM, phat.ls)
plotls <- as.data.frame(plotls)

# Remove duplicate observations
plotls <- plotls[!duplicated(plotls), ]

colnames(plotls) <- c("PAEE","Hazard Ratio for CAD")


plotls %>%
  ggplot(aes(x = PAEE, y = `Hazard Ratio for CAD`)) + geom_line() 


# ----------
# RESTRICTED quadratic spline
# ----------

# Unrestricted quadratic spline
datsubrest$qs.1 <- datsubrest$ls.1^2
datsubrest$qs.2 <- datsubrest$ls.2^2
datsubrest$qs.3 <- datsubrest$ls.3^2
datsubrest$qs.4 <- datsubrest$ls.4^2




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.4
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.4
datsubrest$rqs.3 <- datsubrest$qs.3 - datsubrest$qs.4


fit.rqs <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + rqs.1 + rqs.2 + rqs.3 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(fit.rqs)




# Adding 95% Confidence Interval to these graphs
# Source: http://www.science.smith.edu/~jcrouser/SDS293/labs/lab13-r.html

# Log odds interpretation
phat.rqs <- predict(fit.rqs, se = TRUE)
phat.rqs <- exp(phat.rqs)

phat.df <- as.data.frame(phat.rqs)

# Compute error bands (2*SE)
se_bands = with(phat.rqs, cbind("upper" = fit+2*se.fit, 
                                       "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)

# Creating df w x and y preds in it to simplify
plotrqs <- cbind(datsubrest$OverallPAEETRANSFORM, phat.df)
plotrqs <- cbind(plotrqs, se_bands)



colnames(plotrqs) <- c("PAEE","HR of CAD","SE","Upper","Lower")


# Plot the spline and error bands
plot <- ggplot() +
  geom_line(data = plotrqs, aes(x = PAEE, y = `HR of CAD`), color = "#0000FF") + 
  geom_ribbon(data = plotrqs, aes(x = PAEE, 
                                  ymin = Lower, 
                                  ymax = Upper), 
              alpha = 0.3) + labs(title = "Relationship of CAD Hazard and PAEE", subtitle = "Restricted Quadratic Spline", y = "HR of CAD", x = "PAEE", caption = "Gray represents 95% confidence intervals and dashed lines represent knots")



# REPLACE THESE WITH THE KNOTS
plot + geom_vline(xintercept = 30.16, linetype = "dotdash") + geom_vline(xintercept = 36.03, linetype = "dotdash") + geom_vline(xintercept = 41.53, linetype = "dotdash") + geom_vline(xintercept = 48.67, linetype = "dotdash")



# ---------
# REPEAT THIS PROCESS W PAEE FOR GENETIC RISK GROUPS
# AND LAYER THEM ON TOP OF EACH OTHER
# ---------

# -----
# TRY MAKING HIGH GENETIC RISK AND LAYERING ONTO THIS TO BE ABLE TO DO FULL
# Unrestricted quadratic spline

datsubrestGRS1 <- subset(datsubrest, StandPGS <= -0.442)
datsubrestGRS2 <- subset(datsubrest, StandPGS > -0.442 & StandPGS <= 0.413)
datsubrestGRS3 <- subset(datsubrest, StandPGS > 0.413)


fit.rqs <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + rqs.1 + rqs.2 + rqs.3 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrestGRS3)


summary(fit.rqs)



# Log odds interpretation
phat.rqs <- predict(fit.rqs, se = TRUE)
phat.rqs <- exp(phat.rqs)

phat.df <- as.data.frame(phat.rqs)

# Compute error bands (2*SE)
se_bands = with(phat.rqs, cbind("upper" = fit+2*se.fit, 
                                "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)

# Creating df w x and y preds in it to simplify
plotrqs <- cbind(datsubrestGRS3$OverallPAEETRANSFORM, phat.df)
plotrqs <- cbind(plotrqs, se_bands)



colnames(plotrqs) <- c("PAEE","HR of CAD","SE","Upper","Lower")


# Plot the spline and error bands
plot <- ggplot() +
  geom_line(data = plotrqs, aes(x = PAEE, y = `HR of CAD`), color = "#0000FF") + 
  geom_ribbon(data = plotrqs, aes(x = PAEE, 
                                  ymin = Lower, 
                                  ymax = Upper), 
              alpha = 0.3) + labs(title = "Relationship of CAD Hazard and PAEE", subtitle = "Restricted Quadratic Spline", y = "HR of CAD", x = "PAEE", caption = "Gray represents 95% confidence intervals and dashed lines represent knots")



# REPLACE THESE WITH THE KNOTS
plot + geom_vline(xintercept = 30.16, linetype = "dotdash") + geom_vline(xintercept = 36.03, linetype = "dotdash") + geom_vline(xintercept = 41.53, linetype = "dotdash") + geom_vline(xintercept = 48.67, linetype = "dotdash")



# ----
# PUTTING ONTO SAME GRAPH
# LIKELY WORKS - Trouble here is that overlap is nearly IDENTICAL
# ----


p = ggplot() + 
  geom_line(data = plotrqs, aes(x = PAEE, y = `HR of CAD`), color = "blue") + geom_line(data = plotrqsGENE, aes(x = PAEE, y = `HR of CAD`), color = "red") + geom_ribbon(data = plotrqs, aes(x = PAEE,
                                                                                                                                                                                             ymin = Lower,
                                                                                                                                                                                             ymax = Upper), alpha = 0.3) +  geom_ribbon(data = plotrqsGENE, aes(x = PAEE, ymin = Lower, ymax = Upper), alpha = 0.3)+ labs(title = "Relationship of CAD Hazard and PAEE", subtitle = "Restricted Quadratic Spline", y = "HR of CAD", x = "PAEE", caption = "Gray represents 95% confidence intervals and dashed lines represent knots")



# ---------
# DO ALL OF THIS OVER AGAIN FOR %MVPA AND %VIGOROUS
# GETS WEIRD - Strong effect of MVPA w/ confounders - STAT INSIG IF THEY ARE REMOVED
# ---------

# FITTING OVERALL THEN FIT FOR GENETIC RISK GROUP THEN LAYER ON SAME GRAPH

# Using PARTIAL set of confounders
fit.linear <- coxph(Surv(TimeAge, Status) ~ PercentMVPA + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(fit.linear)


# not specifying type... just to see
phat.linear <- predict(fit.linear)

# Convert from log HR to HR
phat.linear <- exp(phat.linear)

# Creating df w x and y preds in it to simplify
plotlin <- cbind(datsubrest$PercentMVPA, phat.linear)
plotlin <- as.data.frame(plotlin)

# Remove duplicate observations
plotlin <- plotlin[!duplicated(plotlin), ]

colnames(plotlin) <- c("Percent MVPA","Hazard Ratio for CAD")


ggplot(data = plotlin, aes(x = `Percent MVPA`, y = `Hazard Ratio for CAD`)) + geom_line() 
# This ONLY works WITHOUT controlling for anything... USING AGE AT LEAST


# --------
# LINEAR SPLINE
# --------

knots <- quantile(datsubrest$PercentMVPA, probs = c(0.20, 0.40, 0.60, 0.80))
# Using these four knots
# 20% 0.260799896947289
# 40% 0.327906327735732
# 60% 0.387017057352891
# 80% 0.455997880283066




datsubrest$ls.1 <- (datsubrest$PercentMVPA - 0.26)*as.integer(datsubrest$PercentMVPA > 0.26)
datsubrest$ls.2 <- (datsubrest$PercentMVPA - 0.33)*as.integer(datsubrest$PercentMVPA > 0.33)
datsubrest$ls.3 <- (datsubrest$PercentMVPA - 0.39)*as.integer(datsubrest$PercentMVPA > 0.39)
datsubrest$ls.4 <- (datsubrest$PercentMVPA - 0.46)*as.integer(datsubrest$PercentMVPA > 0.46)

fit.ls <- coxph(Surv(TimeAge, Status) ~ PercentMVPA + ls.1 + ls.2 + ls.3 + ls.4, data = datsubrest)


summary(fit.ls)


# not specifying type... just to see
phat.ls <- predict(fit.ls)

# Convert from log HR to HR
phat.ls <- exp(phat.ls)

# Creating df w x and y preds in it to simplify
plotls <- cbind(datsubrest$OverallPAEETRANSFORM, phat.ls)
plotls <- as.data.frame(plotls)

# Remove duplicate observations
plotls <- plotls[!duplicated(plotls), ]

colnames(plotls) <- c("Percent MVPA","Hazard Ratio for CAD")


ggplot(data = plotls, aes(x = `Percent MVPA`, y = `Hazard Ratio for CAD`)) + geom_line() 
# This ONLY works WITHOUT controlling for anything... USING AGE AT LEAST


# ----------
# RESTRICTED quadratic spline
# ----------

# Unrestricted quadratic spline
datsubrest$qs.1 <- datsubrest$ls.1^2
datsubrest$qs.2 <- datsubrest$ls.2^2
datsubrest$qs.3 <- datsubrest$ls.3^2
datsubrest$qs.4 <- datsubrest$ls.4^2




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.4
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.4
datsubrest$rqs.3 <- datsubrest$qs.3 - datsubrest$qs.4


fit.rqs <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + rqs.1 + rqs.2 + rqs.3 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(fit.rqs)




# Adding 95% Confidence Interval to these graphs
# Source: http://www.science.smith.edu/~jcrouser/SDS293/labs/lab13-r.html

# Log odds interpretation
phat.rqs <- predict(fit.rqs, se = TRUE)
phat.rqs <- exp(phat.rqs)

phat.df <- as.data.frame(phat.rqs)

# Compute error bands (2*SE)
se_bands = with(phat.rqs, cbind("upper" = fit+2*se.fit, 
                                "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)

# Creating df w x and y preds in it to simplify
plotrqs <- cbind(datsubrest$OverallPAEETRANSFORM, phat.df)
plotrqs <- cbind(plotrqs, se_bands)



colnames(plotrqs) <- c("PAEE","HR of CAD","SE","Upper","Lower")


# Plot the spline and error bands
plot <- ggplot() +
  geom_line(data = plotrqs, aes(x = PAEE, y = `HR of CAD`), color = "#0000FF") + 
  geom_ribbon(data = plotrqs, aes(x = PAEE, 
                                  ymin = Lower, 
                                  ymax = Upper), 
              alpha = 0.3) + labs(title = "Relationship of CAD Hazard and PAEE", subtitle = "Restricted Quadratic Spline", y = "HR of CAD", x = "PAEE", caption = "Gray represents 95% confidence intervals and dashed lines represent knots")





# --------
# TESTING STUFF OUT:
# What if GRS is cut in half?
# What about PA ATTENUATING genetic risk of CAD???
# ----------

datsubrest$TimeAge <-datsubrest$TimeYear + datsubrest$AgeBaseline


HALF <- quantile(datsubrest$StandPGS, c(0.5))
# 50%: -0.0168291390930716


datsubrestGRS1 <- subset(datsubrest, StandPGS <= -0.0168)
datsubrestGRS2 <- subset(datsubrest, StandPGS > -0.168)


datsubrest$PGSHALF <- ifelse(datsubrest$StandPGS <= -0.0168, 0, 1)



# -------
# Is there an interaction effect between overall PAEE and genetic risk?
# -------

# Fit broken into halves
fit.PGS <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM*PGSHALF + OverallPAEETRANSFORM + PGSHALF + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex, data = datsubrest)


summary(fit.PGS)

# Fit CONTINUOUS (best powered)
fit.PGScont <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM*StandPGS + OverallPAEETRANSFORM + StandPGS + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex, data = datsubrest)


summary(fit.PGScont)

# Try cont for Percent MVPA and Percent Vigorous too
fit.PGScont <- coxph(Surv(TimeAge, Status) ~ PercentMVPA*StandPGS + PercentMVPA + StandPGS + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                     + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex, data = datsubrest)


summary(fit.PGScont)

fit.PGScont <- coxph(Surv(TimeAge, Status) ~ PercentVigorous*StandPGS + PercentVigorous + StandPGS + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                     + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex, data = datsubrest)


summary(fit.PGScont)


# -------
# DO PA VARS ATTENUATE GENETIC RISK?
# Do before w controls then after w controls & PA
# -------


fit.PGSonly <- coxph(Surv(TimeAge, Status) ~ StandPGS + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                     + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(fit.PGSonly)




fit.PGSPAEE <- coxph(Surv(TimeAge, Status) ~ StandPGS + OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                     + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(fit.PGSPAEE)


# Just throwing others in haphazardly for now
fit.PGSPA <- coxph(Surv(TimeAge, Status) ~ StandPGS + OverallPAEETRANSFORM + PercentMVPA + PercentVigorous + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                     + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(fit.PGSPA)


# ------
# Try a HUGE split
# ------


datsubrest$TimeAge <-datsubrest$TimeYear + datsubrest$AgeBaseline


HIGHLOW <- quantile(datsubrest$StandPGS, c(0.10, 0.90))
#  10% -1.2874939740801
# 90% 1.26151998986052


datsubrest$HIGH <- ifelse(datsubrest$StandPGS >= 1.26, 1, 0)
datsubrest$LOW <- ifelse(datsubrest$StandPGS <= -1.29, 1, 0)


datsubHIGH <- subset(datsubrest, HIGH == 1)
datsubLOW <- subset(datsubrest, LOW == 1)

fit.PGS <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex, data = datsubHIGH)


summary(fit.PGS)


fit.PGS <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex, data = datsubLOW)


summary(fit.PGS)



HIGHLOW <- quantile(datsubrest$StandPGS, c(0.20, 0.80))
# 20% -0.846868164911578
# 80% 0.821322441664612

datsubrest$HIGH <- ifelse(datsubrest$StandPGS >= 0.82, 1, 0)
datsubrest$LOW <- ifelse(datsubrest$StandPGS <= -0.85, 1, 0)


datsubHIGH <- subset(datsubrest, HIGH == 1)
datsubLOW <- subset(datsubrest, LOW == 1)

fit.PGS <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex, data = datsubrest)


summary(fit.PGS)




# -----------------
# Clearly NO STRONG PRS and PA interaction (at least linearly... Suspect this is true of spline too)
# CAN DO LIKE NEJM - Lifestyle factors for inds in genetic risk categories and how it changes
# LOOK AT DIFF IN PREDICTION FROM DIFF VALUES
# -----------------

HIGHLOW <- quantile(datsubrest$StandPGS, probs = seq(0, 1, 0.1))
# 20% -0.846868164911578
# 80% 0.821322441664612

HIGHLOW <- quantile(datsubrest$OverallPAEETRANSFORM, c(0.20, 0.80))
# 20% 30.1581806907346
# 80% 48.6690567680804

HIGHLOW <- quantile(datsubrest$Veggie, c(0.20, 0.80), na.rm = TRUE)
HIGHLOW
# 20% 3
# 80% 6

HIGHLOW <- quantile(datsubrest$Fruit, c(0.20, 0.80), na.rm = TRUE)
HIGHLOW
# 20% 1
# 80% 5


fit.PGS <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + StandPGS + Veggie + Fruit, data = datsubrest)


summary(fit.PGS)

OverallPAEETRANSFORM <- c(30.16, 48.67)
StandPGS <- c(-0.85, 0.82)
Veggie <- c(3, 6)
Fruit <- c(1, 5)



val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit))

pred1 <- predict(fit.PGS, newdata = val, se = TRUE)
# Comparing 20th and 80th percentile for lifestyle and genetic risk and PAEE
# 1 -0.112512405535192
# 2 0.150083345479898

exp(-0.112512405535192)
exp(0.150083345479898)
# HRs:
# 0.89358626162394
# 1.16193108039624

# Compute error bands (2*SE)
se_bands = with(pred1, cbind("upper" = fit+2*se.fit, 
                                "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)
# ADDING CIs

exp(se_bands)
# upper	lower
#	0.9687814	0.8242276
#	1.2481357	1.0816803



# NOW MIXING THESE UP...
OverallPAEETRANSFORM <- c(30.16, 48.67)
StandPGS <- c(0.82, -0.85)
Veggie <- c(3, 6)
Fruit <- c(1, 5)


val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit))

pred2 <- predict(fit.PGS, newdata = val, se = TRUE)
# Comparing 20th and 80th percentile for lifestyle and VICE VERSA for genetic risk and PAEE
# 1 0.588167707897946
# 2 -0.55059676795324

exp(0.588167707897946)
exp(-0.55059676795324)
# 1.80068600808348
# 0.576605607937733
# MAKES SENSE - FLIPPING GENETIC RISK *IS* THE APPLES TO APPLES COMPARISON

# Compute error bands (2*SE)
se_bands = with(pred2, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)
# ADDING CIs

exp(se_bands)
# upper	lower
#	1.9510724	1.6618913
#	0.6189054	0.5371969



# -----
# NOW what if we hold veggie and fruit at mean and compare high and low PA by genetic risk
# One HIGH/HIGH and one LOW/LOW
# -----

# NOW MIXING THESE UP...
OverallPAEETRANSFORM <- c(30.16, 48.67)
StandPGS <- c(0.82, 0.82)
Veggie <- c(4.9, 4.9)
Fruit <- c(3.2, 3.2)


val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit))

pred1 <- predict(fit.PGS, newdata = val, se = TRUE)
# Comparing 20th and 80th percentile for lifestyle and VICE VERSA for genetic risk and PAEE
# 0.449000564664036
# 0.259137143123151

exp(0.449000564664036)
exp(0.259137143123151)
# 1.57
# 1.30


# Compute error bands (2*SE)
se_bands = with(pred1, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)
# ADDING CIs

exp(se_bands)
# upper	lower
#	1.667293	1.472262
#	1.377709	1.218782



# NOW MIXING THESE UP...
OverallPAEETRANSFORM <- c(30.16, 48.67)
StandPGS <- c(-0.85, -0.85)
Veggie <- c(4.9, 4.9)
Fruit <- c(3.2, 3.2)


val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit))

pred2 <- predict(fit.PGS, newdata = val, se = TRUE)
# Comparing 20th and 80th percentile for lifestyle and VICE VERSA for genetic risk and PAEE
# -0.251679548769102
# -0.441542970309988

exp(-0.251679548769102)
exp(-0.441542970309988)
# 0.78
# 0.64


# Compute error bands (2*SE)
se_bands = with(pred1, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)
# ADDING CIs

exp(se_bands)
# upper	lower
#	0.8283905	0.7297243
#	0.6829392	0.605478


# So similar % RISK RED from PA in both genetic risk groups
# BUT *ABSOLUTE* RISK RED FROM HIGHER PA IS HIGHEST IN HIGH GENETIC RISK GROUP

# NOTE: THIS IS TREATING PA REL W CAD AS LINEAR... Which probably isn't the case....

# SO:
# NO CLEAR INTERACTION of PGS and PA
# BUT diffs if we look at all in reg together
# BOTH experience 18% reduction from 80th vs 20th percentile (but risk red of 27 pp vs 14 pp for high vs low gen)

# IMPORTANT NOTE: SHOULD PROBABLY CHANGE REFERENCE GROUPS FROM DEFAULT (if no one there can cause instability in HR)
# For instance, 0 PAEE is basically impossible...



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

# ------
# IF REF VALUE MATTERS - USE PAEE = 15??
# ------

new_reference_value <- 15
datsubrest$PAEECENTERED <- datsubrest$OverallPAEETRANSFORM - new_reference_value
# TRY COX MODEL W OverallPAEETRANSFORM AND THEN W CENTERED VARIABLE - Does it make a difference??
# NOT SURE IF THIS IS THINKING OF THINGS THE RIGHT WAY...


datsubrest$ls.1 <- (datsubrest$OverallPAEETRANSFORM - 30.16)*as.integer(datsubrest$OverallPAEETRANSFORM > 30.16)
datsubrest$ls.2 <- (datsubrest$OverallPAEETRANSFORM - 36.03)*as.integer(datsubrest$OverallPAEETRANSFORM > 36.03)
datsubrest$ls.3 <- (datsubrest$OverallPAEETRANSFORM - 41.53)*as.integer(datsubrest$OverallPAEETRANSFORM > 41.53)
datsubrest$ls.4 <- (datsubrest$OverallPAEETRANSFORM - 48.67)*as.integer(datsubrest$OverallPAEETRANSFORM > 48.67)

fit.ls <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + StandPGS + ls.1 + ls.2 + ls.3 + ls.4 + Veggie + Fruit, data = datsubrest)


summary(fit.ls)


# NOW MIXING THESE UP...
OverallPAEETRANSFORM <- c(30.16, 48.67)
StandPGS <- c(0.82, 0.82)
Veggie <- c(4.9, 4.9)
Fruit <- c(3.2, 3.2)

# Creating spline codes to add
ls.1LOW <- (30.16 - 30.16)*as.integer(30.16 > 30.16)
ls.2LOW <- (30.16 - 36.03)*as.integer(30.16 > 36.03)
ls.3LOW <- (30.16 - 41.53)*as.integer(30.16 > 41.53)
ls.4LOW <- (30.16 - 48.67)*as.integer(30.16 > 48.67)

ls.1HIGH <- (48.67 - 30.16)*as.integer(48.67 > 30.16)
ls.2HIGH <- (48.67 - 36.03)*as.integer(48.67 > 36.03)
ls.3HIGH <- (48.67 - 41.53)*as.integer(48.67 > 41.53)
ls.4HIGH <- (48.67 - 48.67)*as.integer(48.67 > 48.67)

ls.1 <- c(ls.1LOW, ls.1HIGH)
ls.2 <- c(ls.2LOW, ls.2HIGH)
ls.3 <- c(ls.3LOW, ls.3HIGH)
ls.4 <- c(ls.4LOW, ls.4HIGH)


val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit, ls.1, ls.2, ls.3, ls.4))

pred1 <- predict(fit.ls, newdata = val, se = TRUE)
# Comparing 20th and 80th percentile for lifestyle and VICE VERSA for genetic risk and PAEE
# 0.350378969830493
# 0.167270451397939

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


# NOW MIXING THESE UP...
OverallPAEETRANSFORM <- c(30.16, 48.67)
StandPGS <- c(-0.85, -0.85)
Veggie <- c(4.9, 4.9)
Fruit <- c(3.2, 3.2)

# Creating spline codes to add
ls.1LOW <- (30.16 - 30.16)*as.integer(30.16 > 30.16)
ls.2LOW <- (30.16 - 36.03)*as.integer(30.16 > 36.03)
ls.3LOW <- (30.16 - 41.53)*as.integer(30.16 > 41.53)
ls.4LOW <- (30.16 - 48.67)*as.integer(30.16 > 48.67)

ls.1HIGH <- (48.67 - 30.16)*as.integer(48.67 > 30.16)
ls.2HIGH <- (48.67 - 36.03)*as.integer(48.67 > 36.03)
ls.3HIGH <- (48.67 - 41.53)*as.integer(48.67 > 41.53)
ls.4HIGH <- (48.67 - 48.67)*as.integer(48.67 > 48.67)

ls.1 <- c(ls.1LOW, ls.1HIGH)
ls.2 <- c(ls.2LOW, ls.2HIGH)
ls.3 <- c(ls.3LOW, ls.3HIGH)
ls.4 <- c(ls.4LOW, ls.4HIGH)


val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit, ls.1, ls.2, ls.3, ls.4))

pred2 <- predict(fit.ls, newdata = val, se = TRUE)
# Comparing 20th and 80th percentile for lifestyle and VICE VERSA for genetic risk and PAEE
# -0.34937170671553
# -0.532480225148084

exp(-0.34937170671553)
exp(-0.532480225148084)
# 0.705131
# 0.5871469


# Compute error bands (2*SE)
se_bands = with(pred2, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)
# ADDING CIs

exp(se_bands)
# upper	lower
#	0.7960819	0.6245711
#	0.6870524	0.5017688


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


fit.rqs <- coxph(Surv(TimeAge, Status) ~ OverallPAEETRANSFORM + StandPGS + rqs.1 + rqs.2 + rqs.3 +  Veggie + Fruit, data = datsubrest)


summary(fit.rqs)


# Creating spline codes to add
qs.1LOW <- ls.1LOW^2
qs.2LOW <- ls.2LOW^2
qs.3LOW <- ls.3LOW^2
qs.4LOW <- ls.4LOW^2

qs.1HIGH <- ls.1HIGH^2
qs.2HIGH <- ls.2HIGH^2
qs.3HIGH <- ls.3HIGH^2
qs.4HIGH <- ls.4HIGH^2

rqs.1LOW <- qs.1LOW - qs.4LOW
rqs.2LOW <- qs.2LOW - qs.4LOW
rqs.3LOW <- qs.3LOW - qs.4LOW

rqs.1HIGH <- qs.1HIGH - qs.4HIGH
rqs.2HIGH <- qs.2HIGH - qs.4HIGH
rqs.3HIGH <- qs.3HIGH - qs.4HIGH


rqs.1 <- c(rqs.1LOW, rqs.1HIGH)
rqs.2 <- c(rqs.2LOW, rqs.2HIGH)
rqs.3 <- c(rqs.3LOW, rqs.3HIGH)


# NOW MIXING THESE UP...
OverallPAEETRANSFORM <- c(30.16, 48.67)
StandPGS <- c(-0.85, -0.85)
Veggie <- c(4.9, 4.9)
Fruit <- c(3.2, 3.2)



val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit, rqs.1, rqs.2, rqs.3))

pred2 <- predict(fit.rqs, newdata = val, se = TRUE)
# Comparing 20th and 80th percentile for lifestyle and VICE VERSA for genetic risk and PAEE
# -0.302612795826811
# -0.468644482933133

exp(-0.302612795826811)
exp(-0.468644482933133)
# HR = 0.7388851
# HR = 0.62585


# Compute error bands (2*SE)
se_bands = with(pred2, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)
# ADDING CIs

exp(se_bands)
# upper	lower
#	0.8117284	0.6725787
#	0.6988033	0.5605129


# NOW MIXING THESE UP...
OverallPAEETRANSFORM <- c(30.16, 48.67)
StandPGS <- c(0.82, 0.82)
Veggie <- c(4.9, 4.9)
Fruit <- c(3.2, 3.2)




val <- as.data.frame(cbind(OverallPAEETRANSFORM, StandPGS, Veggie, Fruit, rqs.1, rqs.2, rqs.3))

pred2 <- predict(fit.rqs, newdata = val, se = TRUE)
# Comparing 20th and 80th percentile for lifestyle and VICE VERSA for genetic risk and PAEE
# 0.397512734698546
# 0.231481047592225

exp(0.397512734698546)
exp(0.231481047592225)
# HR = 1.488119
# HR = 1.260465


# Compute error bands (2*SE)
se_bands = with(pred2, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

se_bands <- as.data.frame(se_bands)
# ADDING CIs

exp(se_bands)
# upper	lower
#	1.633914	1.355333
#	1.408301	1.128149


# THIS PART DONE OFFLINE!!!

# ---------
# FIGURES to compare HR for different genetic and PA groups
# DO LINEAR VS RESTRICTED QUADRATIC SPLINE
# COMPARING LOW/HIGH GENETIC RISK AND LOW/HIGH PA
# 80th/20th percentile chosen arbitrarily....
# ---------


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




datsubrest$SDPAEE <- scale(datsubrest$OverallPAEETRANSFORM)


modlin <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM, data = datsubrest)
summary(modlin)
# HR = 0.97 (95% CI: 0.96 to 0.97)
# Makes sense these effects are relatively small - v small change and modeled linearly

# Model w/ confounders
modlinfull <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + CholMeds + BPMeds + Veggie + Fruit + BMI_InstChosen + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(modlinfull)
# HR = 0.99 (95% CI: 0.985 to 0.995)


# Model w/ confounders - WITHOUT BMI OR MEDS AS POSS MEDIATORS
modlinpart <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(modlinpart)
# HR = 0.98 (95% CI: 0.9795 to 0.989)





# ----------------------------------------
# COMING BACK TO THIS SECTION *AFTER* PRESENTATION
# -----
# FIT Cox Model w/ Restricted Cubic Spline w/ 3 knots by PAEE
# FIT an OVERALL model and one for each genetic risk group
# WILL TRY PATRICK TECHNIQUE AND DEFINE 3 KNOTS BASED ON 25th/50th/75th %tile of PAEE
# -----

install.packages("survival")
library(survival)

# GETTING SD of PAEE
datsubrest$SDPAEE <- scale(datsubrest$OverallPAEETRANSFORM)

# FIRST fitting PAEE linearly - likely not a reasonable assumption
# FIRST fit on RAW scale THEN do SD version
modlin <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM, data = datsubrest)
summary(modlin)
# HR = 0.97 (95% CI: 0.96 to 0.97)
# Makes sense these effects are relatively small - v small change and modeled linearly

# Model w/ confounders
modlinfull <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + CholMeds + BPMeds + Veggie + Fruit + BMI_InstChosen + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(modlinfull)
# HR = 0.99 (95% CI: 0.985 to 0.995)


# Model w/ confounders - WITHOUT BMI OR MEDS AS POSS MEDIATORS
modlinpart <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(modlinpart)
# HR = 0.98 (95% CI: 0.9795 to 0.989)

# REPEATING process measuring PAEE as standard deviations
modlinsd <- coxph(Surv(TimeYear, Status) ~ SDPAEE, data = datsubrest)

summary(modlinsd)
# HR = 0.67 (95% CI: 0.63 to 0.70)


# Model w/ confounders
modlinfullsd <- coxph(Surv(TimeYear, Status) ~ SDPAEE + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                      + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + CholMeds + BPMeds + Veggie + Fruit + BMI_InstChosen + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(modlinfullsd)
# HR = 0.89 (95% CI: 0.84 to 0.95)
# SIGNIFICANT attenuation here

# Model w/ confounders - WITHOUT BMI OR MEDS AS POSS MEDIATORS
modlinpartsd <- coxph(Surv(TimeYear, Status) ~ SDPAEE + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                      + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(modlinpartsd)
# HR = 0.83 (95% CI: 0.79 to 0.88)

# --------------------------
# NOW JUST FIT THESE LINEAR RELATIONSHIPS ON THE THREE GENETIC RISK TERTILES
# INITIAL RESULTS
# SHOULD RECENTER THESE TOO INSTEAD OF USING 0 PAEE
# --------------------------

Tertile <- quantile(datsubrest$StandPGS, c(0:3/3))
# 0% -4.3501375726816
# 33.33333% -0.442665039268912
# 66.66667% 0.413328397665102
# 100% 4.47222889518045
# SO cutoffs would be at -0.44 and 0.41

datsubrestGRS1 <- subset(datsubrest, StandPGS <= -0.442)
datsubrestGRS2 <- subset(datsubrest, StandPGS > -0.442 & StandPGS <= 0.413)
datsubrestGRS3 <- subset(datsubrest, StandPGS > 0.413)


# FIRST fitting PAEE linearly - likely not a reasonable assumption
# FIRST fit on RAW scale THEN do SD version
modlin <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM, data = datsubrestGRS1)
summary(modlin)
# HR = 0.96 (95% CI: 0.95 to 0.97)
# Makes sense these effects are relatively small - v small change and modeled linearly

# Model w/ confounders
modlinfull <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + CholMeds + BPMeds + Veggie + Fruit + BMI_InstChosen + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrestGRS1)


summary(modlinfull)
# HR = 0.985 (95% CI: 0.974 to 0.9965)


# Model w/ confounders - WITHOUT BMI OR MEDS AS POSS MEDIATORS
modlinpart <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrestGRS1)


summary(modlinpart)
# HR = 0.979 (95% CI: 0.9689 to 0.9901)




# FIRST fitting PAEE linearly - likely not a reasonable assumption
# FIRST fit on RAW scale THEN do SD version
modlin <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM, data = datsubrestGRS2)
summary(modlin)
# HR = 0.97 (95% CI: 0.96 to 0.97)
# Makes sense these effects are relatively small - v small change and modeled linearly

# Model w/ confounders
modlinfull <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + CholMeds + BPMeds + Veggie + Fruit + BMI_InstChosen + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrestGRS2)


summary(modlinfull)
# HR = 0.97 (95% CI: 0.962 to 0.978)


# Model w/ confounders - WITHOUT BMI OR MEDS AS POSS MEDIATORS
modlinpart <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrestGRS2)


summary(modlinpart)
# HR = 0.9907 (95% CI: 0.9823 to 0.9992)



# FIRST fitting PAEE linearly - likely not a reasonable assumption
# FIRST fit on RAW scale THEN do SD version
modlin <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM, data = datsubrestGRS3)
summary(modlin)
# HR = 0.966 (95% CI: 0.9596 to 0.972)
# Makes sense these effects are relatively small - v small change and modeled linearly

# Model w/ confounders
modlinfull <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + CholMeds + BPMeds + Veggie + Fruit + BMI_InstChosen + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrestGRS3)


summary(modlinfull)
# HR = 0.9886 (95% CI: 0.9815 to 0.9957)


# Model w/ confounders - WITHOUT BMI OR MEDS AS POSS MEDIATORS
modlinpart <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrestGRS3)


summary(modlinpart)
# HR = 0.983 (95% CI: 0.976 to 0.9899)


# -------------------------
# NOW FULL MODEL BUT *INTERACT* GENETIC RISK AND THESE LINEAR RELATIONSHIPS
# -------------------------

datsubrest$PGSTertile <- ifelse(datsubrest$StandPGS <= -0.442, 1,
                                ifelse(datsubrest$StandPGS > -0.442 & datsubrest$StandPGS <= 0.413, 2, 3))



# FIRST fitting PAEE linearly - likely not a reasonable assumption
# FIRST fit on RAW scale THEN do SD version
modlin <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM*PGSTertile + OverallPAEETRANSFORM + PGSTertile, data = datsubrest)
summary(modlin)
# Insig interaction

# Model w/ confounders
modlinfull <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM*PGSTertile + OverallPAEETRANSFORM + PGSTertile + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + CholMeds + BPMeds + Veggie + Fruit + BMI_InstChosen + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(modlinfull)
# Insig interaction

# Model w/ confounders - WITHOUT BMI OR MEDS AS POSS MEDIATORS
modlinpart <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM*PGSTertile + OverallPAEETRANSFORM + PGSTertile + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                    + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + AgeBaseline, data = datsubrest)


summary(modlinpart)
# Still insig




# --------
# WORKING THRU SPLINE FITTING AND INTERP A BIT TO GET DEMPSEY RESULTS IN TABLE 2 BEFORE DOING INTERACT
# MAKE SURE FOLLOWING IS IN THE VICINITY OF WHAT DEMPSEY ET AL GOT OVERALL BEFORE PROCEEDING
# BOTH in terms of knot values AND in terms of results
# ---------

# DEMPSEY split by gender, so starting here
datsubrestMALE <- subset(datsubrest, Biological.Sex == "Male")
datsubrestFEMALE <- subset(datsubrest, Biological.Sex == "Female")


# NOTE IN DEMPSEY - Split as 15/20/30/40/50/60 w/ 15 PAEE as ref group
# BUT THEY SAY THEY FIT A CUBIC SPLINE WITH 3 KNOTS (so just fit that model then look at different PAEE levels manually)
knotsMALE <- quantile(datsubrestMALE$OverallPAEETRANSFORM, probs = c(0.25, 0.50, 0.75))
# 25% 30.5261123419424
# 50% 37.5167400366435
# 75% 45.3506325755487

knotsFEMALE <- quantile(datsubrestFEMALE$OverallPAEETRANSFORM, probs = c(0.25, 0.50, 0.75))
# 25% 32.8005638715756 
# 50% 39.5953956298191 
# 75% 47.4057492917517


# FIRST fitting w/ software
install.packages("splines")
library(splines)

# Presumably 3 evenly spaced knots arrived at this way
PAEEmod1 <- coxph(Surv(TimeYear, Status) ~ ns(OverallPAEETRANSFORM, 3), data = datsubrestMALE)
summary(PAEEmod1)

# LOOKING at where the cutoffs were created
ns(datsubrestMALE$OverallPAEETRANSFORM, 3)

max(ns(datsubrestMALE$OverallPAEETRANSFORM, 3)[ , 1])
max(ns(datsubrestMALE$OverallPAEETRANSFORM, 3)[ , 2])
# 0.44 and 0.66 (is this STANDARDIZED?? Must be...)



# PREDICTION to interpret
val <- data.frame(OverallPAEETRANSFORM=c(15, 20, 30, 40, 50, 60))
predict(PAEEmod1, newdata = val)
# 0.774770596741312
# 2 0.571508366165329
# 3 0.211914807204536
# 4 -0.0655394291665201
# 5 -0.301309521923998
# 6 -0.523679544571026


# WAIT initial results are NOT stratified by sex in Table 2
# Presumably 3 evenly spaced knots arrived at this way
PAEEmod1 <- coxph(Surv(TimeYear, Status) ~ ns(OverallPAEETRANSFORM, 3), data = datsubrest)
summary(PAEEmod1)

# LOOKING at where the cutoffs were created
ns(datsubrestMALE$OverallPAEETRANSFORM, 3)

max(ns(datsubrestMALE$OverallPAEETRANSFORM, 3)[ , 1])
max(ns(datsubrestMALE$OverallPAEETRANSFORM, 3)[ , 2])
# 0.44 and 0.66 (is this STANDARDIZED?? Must be...)
# SAME %tiles - so makes sense to me


# PREDICTION to interpret
val <- data.frame(OverallPAEETRANSFORM=c(15, 20, 30, 40, 50, 60))
predict(PAEEmod1, newdata = val)
# HRs in theory (BUT PRESUMABLY RELATIVE TO PAEE = 0...)
# 1 0.990120862453371
# 2 0.752463036962013
# 3 0.315466443922535
# 4 -0.0534144698281092
# 5 -0.354186913852178
# 6 -0.59711946323724



max(ns(datsubrest$OverallPAEETRANSFORM, 3)[ , 1])
min(ns(datsubrest$OverallPAEETRANSFORM, 3)[ , 1])
# -0.211673814909916 to 0.445714228602017


max(ns(datsubrest$OverallPAEETRANSFORM, 3)[ , 2])
min(ns(datsubrest$OverallPAEETRANSFORM, 3)[ , 2])
# 0 to 0.648445195965263


max(ns(datsubrest$OverallPAEETRANSFORM, 3)[ , 3])
min(ns(datsubrest$OverallPAEETRANSFORM, 3)[ , 3])
# -0.338098739918575 to 0.818872576946599


# Implausible
# --------
# fit to model 0 in Table 2
# NOT completely sure ns(x, 3) is WHAT WE WANT HERE...
# DOING BY HAND FOLLOWING PATRICK
# --------


# FIRST centering PAEE around 15 so that it is treated as ref value
# To match Dempsey but ALSO makes sense because no one would have PAEE = 0 so unstable
new_reference_value <- 15
datsubrest$PAEECENTERED <- datsubrest$OverallPAEETRANSFORM - new_reference_value


install.packages('splines')
library(splines)


# WORKS FOR FITTING CUBIC SPLINE??? (thru df = 4)
# NOW KNOW what knots are so can try to apply the following:
cox_model <- coxph(Surv(TimeYear, Status) ~ ns(PAEECENTERED, df=4, knots=c(25, 50, 75)) + as.factor(SeasonWear) + as.factor(Biological.Sex), data=datsubrest)

summary(cox_model)

# COMPUTE AS:
# Log HR x PAEE level (beyond 15) + Log HR x (PAEE level - min of interval?)
# Works because cox model is linear to log hazard
# Exp(sum)




# ------
# TRYING THIS OUT - INCREDIBLY HARD TO UNDERSTAND OUTPUT OF PACKAGES
# ------

new_reference_value <- 15
datsubrest$PAEECENTERED <- datsubrest$OverallPAEETRANSFORM - new_reference_value


knots <- quantile(datsubrest$PAEECENTERED, probs = c(0.25, 0.50, 0.75))
# 25% 16.7921775924108
# 50% 23.7595302926387
# 75% 31.5649390159472


# HAND CODING OR RCS IF NOT...
datsubrest$ls.1 <- (datsubrest$PAEECENTERED - knots[1])*as.integer(datsubrest$PAEECENTERED > knots[1])
datsubrest$ls.2 <- (datsubrest$PAEECENTERED - knots[2])*as.integer(datsubrest$PAEECENTERED > knots[2])
datsubrest$ls.3 <- (datsubrest$PAEECENTERED - knots[3])*as.integer(datsubrest$PAEECENTERED > knots[3])

# Cube the linear spline terms: (DOING QUADRATIC FOR NOW)
datsubrest$qs.1 <- datsubrest$ls.1^2
datsubrest$qs.2 <- datsubrest$ls.2^2
datsubrest$qs.3 <- datsubrest$ls.3^2

# RESTRICTED cubic w/ 3 knots (DOING QUADRATIC FOR NOW)
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3




# USING AGE as time here - simply TimeYear + AgeBaseline
datsubrest$TimeAge <-datsubrest$TimeYear + datsubrest$AgeBaseline

PAEEmod1 <- coxph(Surv(TimeAge, Status) ~ PAEECENTERED + rq.1 + rqs.2 + as.factor(SeasonWear) + as.factor(Biological.Sex), data = datsubrest)
summary(PAEEmod1)

phat.qs <- predict(PAEEmod1)
expphat <- exp(phat.qs)

plot(datsubrest$PAEECENTERED, expphat, main="CHD risk by quadratic spline age", xlab="PAEE",ylab="CHD risk")


# RESULTS: (they likely DID mean restricted cubic so start here...)
# COMPUTE AS:
# Log HR x PAEE level (beyond 15) + Log HR x (PAEE level - min of interval?)
# Works because cox model is linear to log hazard
# Exp(sum)

# 0, 5, 15, 25, 35, 45





install.packages('rms')
library(rms)

# USING rms PACKAGE
#PAEEmod1 <- coxph(Surv(TimeYear, Status) ~ PAEECENTERED + rcs(PAEECENTERED, 4) + as.factor(SeasonWear) + as.factor(Biological.Sex), data = datsubrest)
#summary(PAEEmod1)








# PREDICTION to interpret - REMEMBER 0 is 15 (so instead of 20, etc... do 5, 15, 25, 35, 45)
val <- data.frame(PAEECENTERED=c(0, 5, 15, 25, 35, 45))

# SHOULD be able to simply predict here w/ only variable of interest
predict(PAEEmod1, newdata = val)









coxph(Surv(TimeYear, Status) ~ ns(OverallPAEETRANSFORM, 3), data = datsubrestFEMALE)




wcgs$ls.1 <- (wcgs$age - q.age[2])*as.integer(wcgs$age > q.age[2])
wcgs$ls.2 <- (wcgs$age - q.age[3])*as.integer(wcgs$age > q.age[3])
wcgs$ls.3 <- (wcgs$age - q.age[4])*as.integer(wcgs$age > q.age[4])
wcgs$ls.4 <- (wcgs$age - q.age[5])*as.integer(wcgs$age > q.age[5])


# Cube the linear spline terms:
# NOTE: DEMPSEY ET AL *DOES NOT* USE restricted cubic spline, so just fit normal cubic spline
datsubrest$qs.1 <- datsubrest$ls.1^3
datsubrest$qs.2 <- datsubrest$ls.2^3
datsubrest$qs.3 <- datsubrest$ls.3^3


# Implementing restricting
#datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
#datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3


# LOOKS LIKE THIS IS JUST CUBED (don't need others) - make sure to get interp right
# ADD ALL CONFOUNDERS BUT NOT MVPA OR VIG YET
mod <- coxph(Surv(TimeYear, Status) ~ OverallPAEETRANSFORM + qs.1 + qs.2 + qs.3, data = datsubrest)


# MAKE SURE THIS IS IN THE VICINITY OF WHAT DEMPSEY ET AL GOT OVERALL BEFORE PROCEEDING
# BOTH in terms of knot values AND in terms of results
summary(mod)

# INTERPRET results at different points here for PAEE - how overall PAEE effect changes over time

#phat.rqs <- predict(fit.rqs, type="response")
#phat.rqs <- phat.rqs[order(wcgs$PAEE)]

 
#plot(wcgs$PAEE, phat.rqs, type="o", main="CHD risk by restricted quadratic spline age", xlab="age",ylab="CHD risk")


# Can ALSO do via spline package
install.packages("splines")
library(splines)

# Restricted cubic spline
# NOTE: DEFAULT is to EXCLUDE the intercept - SHOULD I BE DOING THE SAME ABOVE???
# NO - THIS only really comes into play when making splines of many vars as per:
# https://stats.stackexchange.com/questions/586858/whether-to-use-an-intercept-for-spline-regression
rcs <- ns(datsubrest$OverallPAEETRANSFORM, knots = knots, degree = 3, intercept = FALSE)

# OR as ns(x, 3)

model <- coxph(Surv(TimeYear, Status) ~ rcs, data = datsubrest)

# COULD also use library rms - another way to fit cubic spline
# Courtesy: https://discourse.datamethods.org/t/calculate-linear-predictor-for-a-prediction-model-using-restricted-cubic-splines/3975/5
install.packages("rms")
library(rms)

S <- Surv(time = df$time, event = df$event)
a <- cph(S ~ rcs(OverallPAEETRANSFORM, 3) + sex, df)




# -------
# ADD Interactions by %MVPA to restricted cubic spline model
# -------

# Just using existing model by hand and adding %MVPA by itself then interacted w/ rqs.1 and rqs.2
# ALONG W LIMITED SET OF CONFOUNDERS THEN ALL OF THEM
coxph(Surv(TimeYear, Status) ~ PAEE + MVPA + MVPA*rqs.1 + MVPA*rqs.2, data = datsubrest)



# ---------
# REPEAT the process for %Vigorous
# ---------


# -------
# SPLIT by genetic risk group and get PAEE linear, PAEE spline, spline interaction model
# -------


