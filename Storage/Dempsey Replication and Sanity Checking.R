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




# ---------
# Trying out removing variables and getting reasonable results
# ---------


fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + StandPGS + rqs.1 + rqs.2 + rqs.3 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + MobilProbs + Townsend + Biological.Sex + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + REGION, data = datsubrest)


summary(fit.rqs)
# ORIGINAL
# Warning - Loglik converged before variable 23; coefficient may be infinite

# REMOVING Mobility Problems
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + StandPGS + rqs.1 + rqs.2 + rqs.3 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                 + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + Townsend + Biological.Sex + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + REGION, data = datsubrest)


summary(fit.rqs)



# --------
# Dempsey Replication
# --------

# Model 0 - Adjusted for sex and season of wear
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + rqs.3 + SeasonWear + Biological.Sex, data = datsubrest)


summary(fit.rqs)


# -------
# PA, RQs values, SeasonWear, Biological.Sex
# -------

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(20 - 30.16)*as.integer(20 > 30.16)
(20 - 36.03)*as.integer(20 > 36.03)
(20 - 41.53)*as.integer(20 > 41.53)
(20 - 48.67)*as.integer(20 > 48.67)
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


k1 <- matrix(c(5, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.91
# Lower = 0.85
# Upper = 0.98

# -----
# In Dempsey:
# Estimate = 0.80
# Lower = 0.73
# Upper = 0.87
# -----

# Our relationship is markedly weaker... CAD (so 1600 vs 4000 events...)


# -----
# SAME MODEL W 30 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(30 - 30.16)*as.integer(30 > 30.16)
(30 - 36.03)*as.integer(30 > 36.03)
(30 - 41.53)*as.integer(30 > 41.53)
(30 - 48.67)*as.integer(30 > 48.67)
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


k1 <- matrix(c(15, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate 0.764148044782545
# Lower = 0.622963958239052
# Upper = 0.937329080795579

# -----
# In Dempsey:
# Estimate = 0.57
# Lower = 0.47
# Upper = 0.69
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est



# -----
# SAME MODEL W 40 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(40 - 30.16)*as.integer(40 > 30.16)
(40 - 36.03)*as.integer(40 > 36.03)
(40 - 41.53)*as.integer(40 > 41.53)
(40 - 48.67)*as.integer(40 > 48.67)
#9.84
#3.97
#0
#0


# Creating spline codes to add
qs.1 <- 9.84^2
qs.2 <- 3.97^2
# 96.8256
# 15.7609


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(25, 96.83, 15.76, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.749629044832456
# Lower = 0.618425720050417
# Upper = 0.908668068997208


# -----
# In Dempsey:
# Estimate = 0.57
# Lower = 0.47
# Upper = 0.69
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est





# -----
# SAME MODEL W 50 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(50 - 30.16)*as.integer(50 > 30.16)
(50 - 36.03)*as.integer(50 > 36.03)
(50 - 41.53)*as.integer(50 > 41.53)
(50 - 48.67)*as.integer(50 > 48.67)
#19.84
#13.97
#8.47
#1.33


# Creating spline codes to add
qs.1 <- 19.84^2
qs.2 <- 13.97^2
qs.3 <- 8.47^2
qs.4 <- 1.33^2
# 393.6256
# 195.1609
# 71.7409
# 1.7689


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(35, 391.8567, 193.392, 69.972, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.671417154944408
# Lower = 0.544065233201793
# Upper = 0.828578943191619



# -----
# In Dempsey:
# Estimate = 0.44
# Lower = 0.37
# Upper = 0.52
# -----




# -----
# SAME MODEL W 60 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(60 - 30.16)*as.integer(60 > 30.16)
(60 - 36.03)*as.integer(60 > 36.03)
(60 - 41.53)*as.integer(60 > 41.53)
(60 - 48.67)*as.integer(60 > 48.67)
#29.84
#23.97
#18.47
#11.33


# Creating spline codes to add
qs.1 <- 29.84^2
qs.2 <- 23.97^2
qs.3 <- 18.47^2
qs.4 <- 11.33^2
# 890.4256
# 574.5609
# 341.1409
# 128.3689


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(45, 762.0567, 446.192, 212.772, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.680821869344472
# Lower = 0.535138335857973
# Upper = 0.866165600030419



# -----
# In Dempsey:
# Estimate = 0.40
# Lower = 0.33
# Upper = 0.49
# -----

# WEIRD turn here... restricted quadratic vs cubic here???

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

quantile(datsubrest$FruitnVeg, probs = c(0.20, 0.4, 0.6, 0.8), na.rm = TRUE)
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


fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + rqs.3 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + Meds + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + SleepDur_InstChosen, data = datsubrest)


summary(fit.rqs)

# So now PA is INSIGNIFICANT
# This isn't SUPER surprising given collinearity of spline but ALSO - sleep duration is HIGHLY sig (measure similar thing), meds

# ------
# REPEATING their analysis anyway to see what happens
# 26 total - so add 22 0's after PA
# ------

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(20 - 30.16)*as.integer(20 > 30.16)
(20 - 36.03)*as.integer(20 > 36.03)
(20 - 41.53)*as.integer(20 > 41.53)
(20 - 48.67)*as.integer(20 > 48.67)
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


k1 <- matrix(c(5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.940895058136217
# Lower = 0.877492792181234
# Upper = 1.00887838431648


# -----
# In Dempsey:
# Estimate = 0.88
# Lower = 0.80
# Upper = 0.96
# -----

# ACTUALLY while our result is insig it is CLOSER to Dempsey


# -----
# SAME MODEL W 30 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(30 - 30.16)*as.integer(30 > 30.16)
(30 - 36.03)*as.integer(30 > 36.03)
(30 - 41.53)*as.integer(30 > 41.53)
(30 - 48.67)*as.integer(30 > 48.67)
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


k1 <- matrix(c(15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.832958880008511
# Lower = 0.675663834315268
# Upper = 1.02687232991857


# -----
# In Dempsey:
# Estimate = 0.73
# Lower = 0.60
# Upper = 0.88
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est
# THIS comparison brings it back to normal...



# -----
# SAME MODEL W 40 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(40 - 30.16)*as.integer(40 > 30.16)
(40 - 36.03)*as.integer(40 > 36.03)
(40 - 41.53)*as.integer(40 > 41.53)
(40 - 48.67)*as.integer(40 > 48.67)
#9.84
#3.97
#0
#0


# Creating spline codes to add
qs.1 <- 9.84^2
qs.2 <- 3.97^2
# 96.8256
# 15.7609


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(25, 96.83, 15.76, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.83670474649304
# Lower = 0.685458370202501
# Upper = 1.02132363282275


# -----
# In Dempsey:
# Estimate = 0.69
# Lower = 0.58
# Upper = 0.82
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est





# -----
# SAME MODEL W 50 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(50 - 30.16)*as.integer(50 > 30.16)
(50 - 36.03)*as.integer(50 > 36.03)
(50 - 41.53)*as.integer(50 > 41.53)
(50 - 48.67)*as.integer(50 > 48.67)
#19.84
#13.97
#8.47
#1.33


# Creating spline codes to add
qs.1 <- 19.84^2
qs.2 <- 13.97^2
qs.3 <- 8.47^2
qs.4 <- 1.33^2
# 393.6256
# 195.1609
# 71.7409
# 1.7689


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(35, 391.8567, 193.392, 69.972, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.751403864160155
# Lower = 0.603733216573875
# Upper = 0.93519414134426
# INTERESTING! So becomes SIGNIFICANT at this point - not totally crazy...


# -----
# In Dempsey:
# Estimate = 0.64
# Lower = 0.53
# Upper = 0.76
# -----




# -----
# SAME MODEL W 60 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(60 - 30.16)*as.integer(60 > 30.16)
(60 - 36.03)*as.integer(60 > 36.03)
(60 - 41.53)*as.integer(60 > 41.53)
(60 - 48.67)*as.integer(60 > 48.67)
#29.84
#23.97
#18.47
#11.33


# Creating spline codes to add
qs.1 <- 29.84^2
qs.2 <- 23.97^2
qs.3 <- 18.47^2
qs.4 <- 11.33^2
# 890.4256
# 574.5609
# 341.1409
# 128.3689


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(45, 762.0567, 446.192, 212.772, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.752375293719124
# Lower = 0.586261640467688
# Upper = 0.965556235518598




# -----
# In Dempsey:
# Estimate = 0.60
# Lower = 0.49
# Upper = 0.73
# -----

# WEIRD turn here... restricted quadratic vs cubic here???



# --------
# REPEATING this analysis WITHOUT likely mediators/comp vars
# Sleep Duration & Meds excluded
# ---------

fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + rqs.3 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)




# ------
# REPEATING their analysis anyway to see what happens
# 26 total - so add 22 0's after PA
# ------

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(20 - 30.16)*as.integer(20 > 30.16)
(20 - 36.03)*as.integer(20 > 36.03)
(20 - 41.53)*as.integer(20 > 41.53)
(20 - 48.67)*as.integer(20 > 48.67)
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


k1 <- matrix(c(5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.936619827457634
# Lower = 0.873770247456504
# Upper = 1.00399012639811



# -----
# In Dempsey:
# Estimate = 0.88
# Lower = 0.80
# Upper = 0.96
# -----

# ACTUALLY while our result is insig it is CLOSER to Dempsey


# -----
# SAME MODEL W 30 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(30 - 30.16)*as.integer(30 > 30.16)
(30 - 36.03)*as.integer(30 > 36.03)
(30 - 41.53)*as.integer(30 > 41.53)
(30 - 48.67)*as.integer(30 > 48.67)
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


k1 <- matrix(c(15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.821656020101603
# Lower = 0.667101255031625
# Upper = 1.0120182060476



# -----
# In Dempsey:
# Estimate = 0.73
# Lower = 0.60
# Upper = 0.88
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est
# THIS comparison brings it back to normal...



# -----
# SAME MODEL W 40 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(40 - 30.16)*as.integer(40 > 30.16)
(40 - 36.03)*as.integer(40 > 36.03)
(40 - 41.53)*as.integer(40 > 41.53)
(40 - 48.67)*as.integer(40 > 48.67)
#9.84
#3.97
#0
#0


# Creating spline codes to add
qs.1 <- 9.84^2
qs.2 <- 3.97^2
# 96.8256
# 15.7609


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(25, 96.83, 15.76, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.812950877968357
# Lower = 0.666926668083278
# Upper = 0.990947223461453



# -----
# In Dempsey:
# Estimate = 0.69
# Lower = 0.58
# Upper = 0.82
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est
# JUST BARELY MOVES NEEDLE BUT SIG HERE INSTEAD OF AT NEXT LEVEL!!!




# -----
# SAME MODEL W 50 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(50 - 30.16)*as.integer(50 > 30.16)
(50 - 36.03)*as.integer(50 > 36.03)
(50 - 41.53)*as.integer(50 > 41.53)
(50 - 48.67)*as.integer(50 > 48.67)
#19.84
#13.97
#8.47
#1.33


# Creating spline codes to add
qs.1 <- 19.84^2
qs.2 <- 13.97^2
qs.3 <- 8.47^2
qs.4 <- 1.33^2
# 393.6256
# 195.1609
# 71.7409
# 1.7689


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(35, 391.8567, 193.392, 69.972, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.72572554503335
# Lower = 0.584284526378529
# Upper = 0.901405981052362


# -----
# In Dempsey:
# Estimate = 0.64
# Lower = 0.53
# Upper = 0.76
# -----




# -----
# SAME MODEL W 60 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(60 - 30.16)*as.integer(60 > 30.16)
(60 - 36.03)*as.integer(60 > 36.03)
(60 - 41.53)*as.integer(60 > 41.53)
(60 - 48.67)*as.integer(60 > 48.67)
#29.84
#23.97
#18.47
#11.33


# Creating spline codes to add
qs.1 <- 29.84^2
qs.2 <- 23.97^2
qs.3 <- 18.47^2
qs.4 <- 11.33^2
# 890.4256
# 574.5609
# 341.1409
# 128.3689


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(45, 762.0567, 446.192, 212.772, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.723828103037269
# Lower = 0.565219527208259
# Upper = 0.926944483560785


# --------
# SO what happens when we add genetic data?
# PGS and 10 PCs
# --------


fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + rqs.3 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)




# ------
# REPEATING their analysis anyway to see what happens
#  11 more zeros
# ------

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(20 - 30.16)*as.integer(20 > 30.16)
(20 - 36.03)*as.integer(20 > 36.03)
(20 - 41.53)*as.integer(20 > 41.53)
(20 - 48.67)*as.integer(20 > 48.67)
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


k1 <- matrix(c(5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.938523419155307
# Lower = 0.87567102936553
# Upper = 1.00588711829507



# -----
# In Dempsey:
# Estimate = 0.88
# Lower = 0.80
# Upper = 0.96
# -----

# GENETICS do not further attenuate this difference


# -----
# SAME MODEL W 30 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(30 - 30.16)*as.integer(30 > 30.16)
(30 - 36.03)*as.integer(30 > 36.03)
(30 - 41.53)*as.integer(30 > 41.53)
(30 - 48.67)*as.integer(30 > 48.67)
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


k1 <- matrix(c(15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.826676024698107
# Lower = 0.671464327862177
# Upper = 1.01776553340736




# -----
# In Dempsey:
# Estimate = 0.73
# Lower = 0.60
# Upper = 0.88
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est
# THIS comparison brings it back to normal...



# -----
# SAME MODEL W 40 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(40 - 30.16)*as.integer(40 > 30.16)
(40 - 36.03)*as.integer(40 > 36.03)
(40 - 41.53)*as.integer(40 > 41.53)
(40 - 48.67)*as.integer(40 > 48.67)
#9.84
#3.97
#0
#0


# Creating spline codes to add
qs.1 <- 9.84^2
qs.2 <- 3.97^2
# 96.8256
# 15.7609


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(25, 96.83, 15.76, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.828386193194976
# Lower = 0.6797719833777
# Upper = 1.00949097911671




# -----
# In Dempsey:
# Estimate = 0.69
# Lower = 0.58
# Upper = 0.82
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est
# JUST BARELY MOVES NEEDLE BUT SIG HERE INSTEAD OF AT NEXT LEVEL!!!




# -----
# SAME MODEL W 50 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(50 - 30.16)*as.integer(50 > 30.16)
(50 - 36.03)*as.integer(50 > 36.03)
(50 - 41.53)*as.integer(50 > 41.53)
(50 - 48.67)*as.integer(50 > 48.67)
#19.84
#13.97
#8.47
#1.33


# Creating spline codes to add
qs.1 <- 19.84^2
qs.2 <- 13.97^2
qs.3 <- 8.47^2
qs.4 <- 1.33^2
# 393.6256
# 195.1609
# 71.7409
# 1.7689


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(35, 391.8567, 193.392, 69.972, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.737478052272948
# Lower = 0.594013158706042
# Upper = 0.915592305680633



# -----
# In Dempsey:
# Estimate = 0.64
# Lower = 0.53
# Upper = 0.76
# -----

# ADDING GENE HAD V LITTLE EFFECT BUT DID PUSH FIRST SIG OUT TO 35 AGAIN


# -----
# SAME MODEL W 60 vs 15
# -----

(15 - 30.16)*as.integer(15 > 30.16)
(15 - 36.03)*as.integer(15 > 36.03)
(15 - 41.53)*as.integer(15 > 41.53)
(15 - 48.67)*as.integer(15 > 48.67)
#0
#0
#0
#0


(60 - 30.16)*as.integer(60 > 30.16)
(60 - 36.03)*as.integer(60 > 36.03)
(60 - 41.53)*as.integer(60 > 41.53)
(60 - 48.67)*as.integer(60 > 48.67)
#29.84
#23.97
#18.47
#11.33


# Creating spline codes to add
qs.1 <- 29.84^2
qs.2 <- 23.97^2
qs.3 <- 18.47^2
qs.4 <- 11.33^2
# 890.4256
# 574.5609
# 341.1409
# 128.3689


rqs.1 <- qs.1 - qs.4
rqs.2 <- qs.2 - qs.4
rqs.3 <- qs.3 - qs.4


k1 <- matrix(c(45, 762.0567, 446.192, 212.772, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.747086160293097
# Lower = 0.583649918891165
# Upper = 0.956288543587651

# NOW SAVE this dataset as the DempseyDataset

write.csv(datsubrest, "DempseyDataset.csv")
# FORGOT .csv before - MIGHT WORK..

dx upload DempseyDataset.csv




# ---------------
# REPEATING ANALYSIS
# NOW with THREE evenly spaced knots (instead of 4)
# To match process of Dempsey
# STILL using restricted quadratic spline and NOT cubic...
# ---------------

dx download DempseyDataset.csv


datsubrest <- read.csv("DempseyDataset.csv")



knots <- quantile(datsubrest$PAEEPOS, probs = c(0.25, 0.50, 0.75))
# Using these four knots
# 25% 31.7921775924108
# 50% 38.7595302926387
# 75% 46.5649390159472




datsubrest$ls.1 <- (datsubrest$PAEEPOS - 31.79)*as.integer(datsubrest$PAEEPOS > 31.79)
datsubrest$ls.2 <- (datsubrest$PAEEPOS - 38.76)*as.integer(datsubrest$PAEEPOS > 38.76)
datsubrest$ls.3 <- (datsubrest$PAEEPOS - 46.56)*as.integer(datsubrest$PAEEPOS > 46.56)

fit.ls <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + StandPGS + ls.1 + ls.2 + ls.3 + SeasonWear + SmokStat_InstChosen + SleepDur_InstChosen + AlcIntake_InstChosen + OilyFish_InstChosen + Salt_InstChosen + ProcMeat_InstChosen
                + EmploymentStatus_InstChosen + MotherHeartDisease + FatherHeartDisease + Veggie + Fruit + Townsend + Biological.Sex + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + REGION, data = datsubrest)

summary(fit.ls)

# ----
# Package needed to compare to ref group
# ----

install.packages('multcomp')
library(multcomp)


# Unrestricted quadratic spline
datsubrest$qs.1 <- datsubrest$ls.1^2
datsubrest$qs.2 <- datsubrest$ls.2^2
datsubrest$qs.3 <- datsubrest$ls.3^2




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3




fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)


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


k1 <- matrix(c(5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.958037568405077
# Lower = 0.904153736562359
# Upper = 1.01513265428198



# -----
# In Dempsey:
# Estimate = 0.88
# Lower = 0.80
# Upper = 0.96
# -----

# GENETICS do not further attenuate this difference


# -----
# SAME MODEL W 30 vs 15
# -----

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(30 - 31.79)*as.integer(30 > 31.79)
(30 - 38.76)*as.integer(30 > 38.76)
(30 - 46.56)*as.integer(30 > 46.56)
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


k1 <- matrix(c(15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.879321352845526
# Lower = 0.739140236037199
# Upper = 1.04608841985863





# -----
# In Dempsey:
# Estimate = 0.73
# Lower = 0.60
# Upper = 0.88
# -----


# -----
# SAME MODEL W 40 vs 15
# -----

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(40 - 31.79)*as.integer(40 > 31.79)
(40 - 38.76)*as.integer(40 > 38.76)
(40 - 46.56)*as.integer(40 > 46.56)
#8.21
#1.24
#0


# Creating spline codes to add
qs.1 <- 8.21^2
qs.2 <- 1.24^2
# 67.40
# 1.54



k1 <- matrix(c(25, 67.40, 1.54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.81762785249449
# Lower = 0.674384278418427
# Upper = 0.991297286381825





# -----
# In Dempsey:
# Estimate = 0.69
# Lower = 0.58
# Upper = 0.82
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est
# JUST BARELY MOVES NEEDLE BUT SIG HERE INSTEAD OF AT NEXT LEVEL!!!




# -----
# SAME MODEL W 50 vs 15
# -----

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(50 - 31.79)*as.integer(50 > 31.79)
(50 - 38.76)*as.integer(50 > 38.76)
(50 - 46.56)*as.integer(50 > 46.56)
#18.21
#11.24
#3.44



# Creating spline codes to add
qs.1 <- 18.21^2
qs.2 <- 11.24^2
qs.3 <- 3.44^2
# 331.6041
# 126.3376
# 11.8336


rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3


k1 <- matrix(c(35, 319.7705, 114.504, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.787487471297623
# Lower = 0.653093372644449
# Upper = 0.94953729960499



# -----
# In Dempsey:
# Estimate = 0.64
# Lower = 0.53
# Upper = 0.76
# -----

# ADDING GENE HAD V LITTLE EFFECT BUT DID PUSH FIRST SIG OUT TO 35 AGAIN


# -----
# SAME MODEL W 60 vs 15
# -----


(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(60 - 31.79)*as.integer(60 > 31.79)
(60 - 38.76)*as.integer(60 > 38.76)
(60 - 46.56)*as.integer(60 > 46.56)
#28.21
#21.24
#13.44


# Creating spline codes to add
qs.1 <- 28.21^2
qs.2 <- 21.24^2
qs.3 <- 13.44^2
# 795.8041
# 451.1376
# 180.6336


rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3


k1 <- matrix(c(45, 615.1705, 270.504, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.722784336905611
# Lower = 0.543560479097182
# Upper = 0.961102246697156

# UPDATED AFTER FIXING SPLINE
# Estimate = 0.764633304262427
# Lower = 0.599198166353276
# Upper = 0.975744124094283


# ---------
# NOW REPEATING the analysis as RESTRICTED cubic spline
# ---------




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

# GENETICS do not further attenuate this difference


# -----
# SAME MODEL W 30 vs 15
# -----

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(30 - 31.79)*as.integer(30 > 31.79)
(30 - 38.76)*as.integer(30 > 38.76)
(30 - 46.56)*as.integer(30 > 46.56)
#0
#0
#0
#0


# Creating spline codes to add
qs.1 <- ls.1^3
qs.2 <- ls.2^3
qs.3 <- ls.3^3



k1 <- matrix(c(15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.885864634736496
# Lower = 0.773862683781279
# Upper = 1.0140767445231






# -----
# In Dempsey:
# Estimate = 0.73
# Lower = 0.60
# Upper = 0.88
# -----


# -----
# SAME MODEL W 40 vs 15
# -----

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(40 - 31.79)*as.integer(40 > 31.79)
(40 - 38.76)*as.integer(40 > 38.76)
(40 - 46.56)*as.integer(40 > 46.56)
#8.21
#1.24
#0


# Creating spline codes to add
qs.1 <- 8.21^3
qs.2 <- 1.24^3
# 67.40
# 1.54



k1 <- matrix(c(25, 553.3877, 553.3877, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.813024826449318
# Lower = 0.625591927714882
# Upper = 1.0566142866284





# -----
# In Dempsey:
# Estimate = 0.69
# Lower = 0.58
# Upper = 0.82
# -----

# Our CIs are somewhat wider (as expected) but MAIN diff is in point est
# JUST BARELY MOVES NEEDLE BUT SIG HERE INSTEAD OF AT NEXT LEVEL!!!




# -----
# SAME MODEL W 50 vs 15
# -----

(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(50 - 31.79)*as.integer(50 > 31.79)
(50 - 38.76)*as.integer(50 > 38.76)
(50 - 46.56)*as.integer(50 > 46.56)
#18.21
#11.24
#3.44



# Creating spline codes to add
qs.1 <- 18.21^3
qs.2 <- 11.24^3
qs.3 <- 3.44^3
# 6038.511
# 1420.035
# 40.70758


rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3


k1 <- matrix(c(35, 5997.803, 1379.327, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.788471556227299
# Lower = 0.654798576246446
# Upper = 0.94943302800572




# -----
# In Dempsey:
# Estimate = 0.64
# Lower = 0.53
# Upper = 0.76
# -----

# ADDING GENE HAD V LITTLE EFFECT BUT DID PUSH FIRST SIG OUT TO 35 AGAIN


# -----
# SAME MODEL W 60 vs 15
# -----


(15 - 31.79)*as.integer(15 > 31.79)
(15 - 38.76)*as.integer(15 > 38.76)
(15 - 46.56)*as.integer(15 > 46.56)
#0
#0
#0
#0


(60 - 31.79)*as.integer(60 > 31.79)
(60 - 38.76)*as.integer(60 > 38.76)
(60 - 46.56)*as.integer(60 > 46.56)
#28.21
#21.24
#13.44


# Creating spline codes to add
qs.1 <- 28.21^3
qs.2 <- 21.24^3
qs.3 <- 13.44^3
# 22449.63
# 9582.163
# 2427.716


rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3


k1 <- matrix(c(45, 20021.92, 7154.447, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.765033921718912
# Lower = 0.61906001987103
# Upper = 0.94542836331532

