# --------
# Installing required packages
# --------

install.packages('survival')
library(survival)


install.packages('ggplot2')
library(ggplot2)

# -------
# Summary and interpretation of each confounder
# -------

summary(datsubrest$SeasonWear)
# Factor variable w/ 4 seasons

summary(datsubrest$SmokStat_InstChosen)
# Factor variable w/ 3 levels (never/current/previous)

summary(datsubrest$SleepDur_InstChosen)
# Numeric variable w/ answer to "About how many hours sleep do you get in 24 hours?"

summary(datsubrest$AlcIntake_InstChosen)
# Has been transformed to numeric that measures ANNUAL intake of alcohol

summary(datsubrest$OilyFish_InstChosen)
# Has been transformed to numeric that measures weekly oily fish intake

summary(datsubrest$Salt_InstChosen)
# Coded as 0/1/2/3 for if a person never/sometimes/usually/always adds salt
# FACTOR VARIABLE

summary(datsubrest$ProcMeat_InstChosen)
# Has been transformed to numeric that measures weekly processed/red meat intake

summary(datsubrest$EmploymentStatus_InstChosen)
# FACTOR variable for if someone is retired/unemployed/employed/other

summary(datsubrest$MotherHeartDisease)
summary(datsubrest$FatherHeartDisease)
# Binary variables for whether mother and father have history of heart disease

summary(datsubrest$Veggie)
summary(datsubrest$Fruit)
# Counts number of tablespoons of raw + cooked veggies and number of pieces of fresh and dried fruit in day

summary(datsubrest$MobilProbs)
# Factor - unable to walk, have problems walking, moderate problems, slight problems, no problems

summary(datsubrest$Townsend)
# Townsend Index - measure of area deprivation


# -------
# Using AGE as time scale
# -------

datsubrest$TimeAge <-datsubrest$TimeYear + datsubrest$AgeBaseline


# -----------------
# Creating DECILES of PA and PGS
# Will be important for figure creation
# AND for creating a sensible reference level
# -----------------

# Getting 10th to 90th(?) percentile
PGSDECILE <- quantile(datsubrest$StandPGS, probs = seq(0.1, 0.9, 0.1))
# 10% -1.2874939740801
# 20% -0.846868164911578
# 30% -0.533799378613378
# 40% -0.266874315609926
# 50% -0.0168291390930716
# 60% 0.236609243902655
# 70% 0.504869885863517
# 80% 0.821322441664612
# 90% 1.26151998986052


PAEEDECILE <- quantile(datsubrest$PAEEPOS, probs = seq(0.1, 0.9, 0.1))
# 10% 25.9491318778412
# 20% 30.1581806907346
# 30% 33.2842529902615
# 40% 36.0267497376078
# 50% 38.7595302926387
# 60% 41.5283167027557
# 70% 44.7140637230322
# 80% 48.6690567680804
# 90% 54.5613992585524


# Goes from 10th to 90th %tile
# FLIPPING PA (10th = LEAST RISK/HIGHEST ACTIVITY)
PAEEDECILE <- sort(PAEEDECILE, decreasing = TRUE)


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



knots <- quantile(datsubrest$PAEEPOS, probs = c(0.20, 0.40, 0.60, 0.80))
# Using these four knots
# 20% 30.1581806907346
# 40% 36.0267497376078
# 60% 41.5283167027557
# 80% 48.6690567680804



datsubrest$ls.1 <- (datsubrest$PAEEPOS - 30.16)*as.integer(datsubrest$PAEEPOS > 30.16)
datsubrest$ls.2 <- (datsubrest$PAEEPOS - 36.03)*as.integer(datsubrest$PAEEPOS > 36.03)
datsubrest$ls.3 <- (datsubrest$PAEEPOS - 41.53)*as.integer(datsubrest$PAEEPOS > 41.53)
datsubrest$ls.4 <- (datsubrest$PAEEPOS - 48.67)*as.integer(datsubrest$PAEEPOS > 48.67)

fit.ls <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + StandPGS + ls.1 + ls.2 + ls.3 + ls.4 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + Townsend + Biological.Sex + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + REGION, data = datsubrest)
# REMOVED Mobility Problems for now...

summary(fit.ls)

# ----
# Package needed to compare to ref group
# ----

install.packages('multcomp')
library(multcomp)

# PAEE20th - PAEE10th
# StandPGS10th - StandPGS10th (THEN change StandPGS to 20th)
# ls.120th - ls.110th
# ls.220th - ls.210th
# ls.320th - ls.310th
# ls.420th - ls.410th
# DO WE HAVE TO SPECIFY OTHER COVARIATES??? IF SO TAKE MEANS OF OTHERS...

# -------------
# 10th/20th genetic/PA vs 10th/10th
# Doing by hand this time for comparison
# --------------

# 10% -1.2874939740801

# 80% 48.6690567680804
# 90% 54.5613992585524

(54.56 - 30.16)*as.integer(54.56 > 30.16)
(54.56 - 36.03)*as.integer(54.56 > 36.03)
(54.56 - 41.53)*as.integer(54.56 > 41.53)
(54.56 - 48.67)*as.integer(54.56 > 48.67)
#24.4
#18.53
#13.03
#5.89


(48.67 - 30.16)*as.integer(48.67 > 30.16)
(48.67 - 36.03)*as.integer(48.67 > 36.03)
(48.67 - 41.53)*as.integer(48.67 > 41.53)
(48.67 - 48.67)*as.integer(48.67 > 48.67)
#18.51
#12.64
#7.14
#0

18.51 - 24.40
12.64 - 18.53
7.14 - 13.03
0 - 5.89
# -5.89
# -5.89
# -5.89
# -5.89
# CONSTANT here - likely because both are above all splines???


k1 <- matrix(c(-5.89, 0, -5.89, -5.89, -5.89, -5.89, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# 10th percentile/10th percentile FIRST as reference to subtract out
# DO need ALL vars in model - TRY SETTING ALL REST TO ZERO (shouldn't matter...)
delta.eta <- glht(fit.ls, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate 0.978463950156886
# lwr 0.879798915724622
# upr 1.08819377319656
# NOT significant here... STRANGE... (due to collinearity?)
# ***SEEMS MOBILITY PROBLEMS IS CAUSING ISSUES HERE...***

# REMOVED MOBILITY PROBLEMS - STILL INSIG...
# Estimate 0.971040354028552
# lwr 0.873237986044181
# upr 1.07979655514458

# ----------
# TESTING 10th vs 90th in PA
# ----------

# 10% 25.9491318778412
# ALL SPLINES VALUES ARE 0 HERE

# -24.4
# -18.53
# -13.03
# -5.89

k1 <- matrix(c(-5.89, 0, -24.40, -18.53, -13.03, -5.89, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# 10th percentile/10th percentile FIRST as reference to subtract out
# DO need ALL vars in model - TRY SETTING ALL REST TO ZERO (shouldn't matter...)
delta.eta <- glht(fit.ls, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate 0.790809093918874
# lwr 0.519818431349616
# upr 1.20307204460047

# CRAZY range... Something about how things are entering model here...
# COMPARING TO DEMPSEY - POINT ESTIMATE LOOKS REASONABLE...
# BUT MY CI IS OVER TWICE THE SIZE...
# JUST DO SAME BASIC OBS THEY DID AND COMPARE TO TABLE 2 RESULTS...

# TimeAge is behaving strangely here... CONVERT to TimeYear and use AgeBaseline



# -----
# REPEATING this process for ALL OTHERS vs 10/10 and 20/20
# AUTOMATING this process
# -----

# -----
# To create comparisons to 10th/10th percentile
# -----



# Creating empty matrix objects
PGSDIFF <- matrix(NA, nrow = 10, ncol = 1)
PADIFF <- matrix(NA, nrow = 10, ncol = 1)

ls.110 <- matrix(NA, nrow = 10, ncol = 1)
ls.210 <- matrix(NA, nrow = 10, ncol = 1)
ls.310 <- matrix(NA, nrow = 10, ncol = 1)
ls.410 <- matrix(NA, nrow = 10, ncol = 1)

ls.1 <- matrix(NA, nrow = 10, ncol = 1)
ls.2 <- matrix(NA, nrow = 10, ncol = 1)
ls.3 <- matrix(NA, nrow = 10, ncol = 1)
ls.4 <- matrix(NA, nrow = 10, ncol = 1)

for(i in 1:10){

PGSDIFF <- (PGSDECILE[i] - PGSDECILE[1])

PADIFF <- (PAEEDECILE[i] - PAEEDECILE[1])

# Spline values at 10th percentile PA (reference)
ls.110[[i]] <- (PAEEDECILE[1] - 30.16)*as.integer(PAEEDECILE[1] > 30.16)
ls.210[[i]] <- (PAEEDECILE[1] - 36.03)*as.integer(PAEEDECILE[1] > 36.03)
ls.310[[i]] <- (PAEEDECILE[1] - 41.53)*as.integer(PAEEDECILE[1] > 41.53)
ls.410[[i]] <- (PAEEDECILE[1] - 48.67)*as.integer(PAEEDECILE[1] > 48.67)


# Spline values at PA decile i
ls.1[[i]] <- (PAEEDECILE[i] - 30.16)*as.integer(PAEEDECILE[i] > 30.16)
ls.2[[i]] <- (PAEEDECILE[i] - 36.03)*as.integer(PAEEDECILE[i] > 36.03)
ls.3[[i]] <- (PAEEDECILE[i] - 41.53)*as.integer(PAEEDECILE[i] > 41.53)
ls.4[[i]] <- (PAEEDECILE[i] - 48.67)*as.integer(PAEEDECILE[i] > 48.67)

mat <- c(PADIFF, PGSDIFF, ls.110, ls.210, ls.310, ls.410, ls.1, ls.2, ls.3, ls.4)

}

dim(mat)

ls.1DIFF <- ls.1 - ls.110
ls.2DIFF <- ls.2 - ls.210
ls.3DIFF <- ls.3 - ls.310
ls.4DIFF <- ls.4 - ls.410


# Now HAVE full collection of differences
# BUT need every PGS and PA comparison available- use rep
PAMAT <- c(rep(PADIFF, times = 10), rep(ls.1DIFF, times = 10), rep(ls.2DIFF, times = 10), rep(ls.3DIFF, times = 10), rep(ls.4DIFF, times = 10))
# 10th to 100th percentile for all of these variables

PGSMAT <- c(rep(PGSDIFF[1], times = 10), rep(PGSDIFF[2], times = 10), rep(PGSDIFF[3], times = 10), rep(PGSDIFF[4], times = 10), rep(PGSDIFF[5], times = 10),
            rep(PGSDIFF[6], times = 10), rep(PGSDIFF[7], times = 10), rep(PGSDIFF[8], times = 10), rep(PGSDIFF[9], times = 10), rep(PGSDIFF[10], times = 10))
# 10th x 10, 20th x 10, ...

# NOW merging these together
# Have values to throw in matrix for all comparisons
FULLMAT <- c(PAMAT, PGSMAT)
# Maybe cbind here???

# NOW getting all of these comparisons ready


k1 <- FULLMAT[1, ]

delta.eta <- glht(fit.ls, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Difference between 10th/10th and 10th/10th...

# NOW comparing 20th PA and 10th PGS to 10th/10th
k1 <- FULLMAT[2, ]

delta.eta <- glht(fit.ls, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]


RESULTS <- matrix(NA, nrow = 100, ncol = 3)


# NOW can automate remainder of comparisons
for(i in 1:100){
  
  k1 <- FULLMAT[i, ]
  
  delta.eta <- glht(fit.ls, linfct=k1)
  
  RESULTS[[i]] <- exp(confint(delta.eta)$confint)[,1:3]
  
  
}





# --------
# Should repeat process w/ QUINTILES using 20th/20th as ref
# AND NEED TO REPEAT USING RESTRICTED QUADRATIC SPLINE
# --------









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
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + REGION, data = datsubrest)


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


