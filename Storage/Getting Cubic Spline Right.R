# ------
# FITTING restricted cubic spline properly
# Based on Table 1 of:
# "Dose-response analysis using restricted cubic spline functions in public
# health research"
# Desquilbet and Mariotti, 2009
# ------


datsubrest$TimeAge <-datsubrest$TimeYear + datsubrest$AgeBaseline



# GETTING AGE AS TIME SCALE RIGHT!!!!
Surv(AgeBaseline, AgeBaseline + TimeYear)
# RIGHT NOW would suffer from IMMORTAL TIME BIAS

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

# So these are:
# (v-ti)^3 where i = 1:3
# BUT max value of i here is K - 2
# Since k = 3, i = 1 ONLY
# SO qs.2 = tk-1 and qs.3 = tk

# v if i = 0
# o.w.:
# (v-t1)^3 - tK-t1/(tk - tk-1)*(v - tk-1)^3 + tk-1-ti/(tk-tk-1)*(v-tk)^3

# Funky cubic restricting
# THIS would explain why there's only one nonlinear part!
datsubrest$rqs.1 <- datsubrest$qs.1 - (((54.56 - 25.95)/(54.56 - 38.76))*datsubrest$qs.2 + (38.76 - 25.95)/(54.56 - 38.76)*datsubrest$qs.3)*as.integer(datsubrest$PAEEPOS > 25.9491318778412)
# as.integer I think is not necessary... But ALSO shouldn't change anything... (all zeros until knot 1)

# Doing the restricting
#datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
#datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3



# fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)

fit.rqs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)



# NOW get value of rqs.1 when PAEEPOS = 15 or 20 or 30, etc...

# See approximate value in this situation
PAEE15 <- subset(datsubrest, PAEEPOS >= 15 & PAEEPOS < 16)
# PAEE20 <- subset(datsubrest, PAEEPOS >= 20 & PAEEPOS < 21)
PAEE30 <- subset(datsubrest, PAEEPOS >= 30 & PAEEPOS < 31)
PAEE30 <- subset(datsubrest, PAEEPOS >= 30.00 & PAEEPOS <= 30.01)
PAEE40 <- subset(datsubrest, PAEEPOS >= 40.00 & PAEEPOS <= 40.01)
PAEE50 <- subset(datsubrest, PAEEPOS >= 50.00 & PAEEPOS <= 50.01)
PAEE60 <- subset(datsubrest, PAEEPOS >= 60.00 & PAEEPOS <= 60.01)

length(coef(fit.rqs))
# 33

k1 <- matrix(c(5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.95363629870403
# Lower = 0.913236792563804
# Upper = 0.995822986558423

# AFTER fixing immortal time bias issue:
# Estimate = 0.913275228545814
# Lower = 0.874768690596737
# Upper = 0.953476789968824



k1 <- matrix(c(15,66.60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 15
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.867722677884766
# Lower = 0.762755236383689
# Upper = 0.987135334901532

# AFTER fixing immortal time bias issue:
# Estimate = 0.761981471834741
# Lower = 0.670214335289654
# Upper = 0.866313555004023



k1 <- matrix(c(25,2772,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 25
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.806484824621122
# Lower = 0.673922670095223
# Upper = 0.965122262843985

# AFTER fixing immortal time bias issue:
# Estimate = 0.64388674454069
# Lower = 0.5382005980101
# Upper = 0.770326419792322



k1 <- matrix(c(35,11348,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 35
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.785808820788288
# Lower = 0.658006647846223
# Upper = 0.938433532320466

# AFTER fixing immortal time bias issue:
# Estimate = 0.559706353488656
# Lower = 0.467872219479654
# Upper = 0.669565725624774



k1 <- matrix(c(45,22002.53,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 45
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.778570074412896
# Lower = 0.62769655179706
# Upper = 0.965707648123712

# AFTER fixing immortal time bias issue:
# Estimate = 0.491429050571053
# Lower = 0.395038019518809
# Upper = 0.611339921254511




# ---------
# REEVALUATE PH assumption question in light of new model
# ---------


# --------------------------------------------------------
# EVALUATING proportional hazards assumption
# 3 ways:
# Graphic eval with log-log plot (ONLY FOR CATEGORICAL)
# Analytically w/ interaction of follow up time and covariate
# Analytically with schoenfeld residuals
# -------



# fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)
# PAEE violates and so does p22009_a1, biological sex, NewEmploy
# ALSO NewEduc and SmokStat



# Schoenfeld resids
ph <- cox.zph(fit.rqs, transform="km", global=TRUE)
plot(ph, var = 1)
abline(h = coef(fit.rqs)[1], col = "red", lwd = 2)


# Wow! There are NO violations after fixing this EXCEPT BIOLOGICAL SEX!!!!


# ---------
# NOW applying PH adjustment
# VERY little difference
# ---------


# fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + tt(PAEEPOS) + rqs.1 + StandPGS + p22009_a1 + tt(p22009_a1) + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + Townsend + strata(Biological.Sex) + strata(NewEmploy) + strata(NewEduc) + strata(SmokStat_InstChosen), data = datsubrest,  tt=function(x,t,...) x*t)


fit.rqs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)

summary(fit.rqs)
# Very small 





# ---------
# CHECK model fit by functional form
# Comparing linear to restricted quadratic spline to restricted cubic spline
# 3/17/2023
# ----------

# ------
# Restricted cubic spline from before
# ------

fit.rcs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)

summary(fit.rcs)

# -------
# Linear model
# -------

fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)

summary(fit.lin)

# ----------
# Restricted Quadratic Model
# ----------


# Unrestricted quadratic spline (using same knots as for cubic)
datsubrest$qs.1 <- datsubrest$ls.1^2
datsubrest$qs.2 <- datsubrest$ls.2^2
datsubrest$qs.3 <- datsubrest$ls.3^2




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3




fit.rqs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)


summary(fit.rqs)




# -------
# looking for smallest AIC/BIC for best model fit
# -------

BIC(fit.lin, fit.rqs, fit.rcs)
# fit.lin	31	28092.32
# fit.rqs	33	28106.49
# fit.rcs	32	28099.27

# Linear is actually PREFERRED here

AIC(fit.lin, fit.rqs, fit.rcs)
# fit.lin	31	27925.87
# fit.rqs	33	27929.30
# fit.rcs	32	27927.44

# SAME conclusion - linear



# ------------
# NOW try same percentiles but defining knots based on CASE dist
# Point here is to not have sparsity of outcomes anywhere
# ------------

# Creating dataset of ONLY individuals who have an event
Cases <- subset(datsubrest, Status == 1)





knots <- quantile(Cases$PAEEPOS, probs = c(0.10, 0.50, 0.90))
knots
# 10% 22.255515024469
# 50% 34.813962747853
# 90% 49.5899839638447



datsubrest$ls.1 <- (datsubrest$PAEEPOS - 22.255515024469)*as.integer(datsubrest$PAEEPOS > 22.255515024469)
datsubrest$ls.2 <- (datsubrest$PAEEPOS - 34.813962747853)*as.integer(datsubrest$PAEEPOS > 34.813962747853)
datsubrest$ls.3 <- (datsubrest$PAEEPOS - 49.5899839638447)*as.integer(datsubrest$PAEEPOS > 49.5899839638447)



# Unrestricted cubic spline
datsubrest$qs.1 <- datsubrest$ls.1^3
datsubrest$qs.2 <- datsubrest$ls.2^3
datsubrest$qs.3 <- datsubrest$ls.3^3

# So these are:
# (v-ti)^3 where i = 1:3
# BUT max value of i here is K - 2
# Since k = 3, i = 1 ONLY
# SO qs.2 = tk-1 and qs.3 = tk

# v if i = 0
# o.w.:
# (v-t1)^3 - tK-t1/(tk - tk-1)*(v - tk-1)^3 + tk-1-ti/(tk-tk-1)*(v-tk)^3

# Funky cubic restricting
# THIS would explain why there's only one nonlinear part!
datsubrest$rqs.1 <- datsubrest$qs.1 - (((49.59 - 22.26)/(49.59 - 34.81))*datsubrest$qs.2 + (34.81 - 22.26)/(49.59 - 34.81)*datsubrest$qs.3)*as.integer(datsubrest$PAEEPOS > 22.26)
# as.integer I think is not necessary... But ALSO shouldn't change anything... (all zeros until knot 1)

# Doing the restricting
#datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
#datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3

# -------
# Refitting restricted cubic spline w/ case-based knots
# -------

fit.rcs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)

summary(fit.rcs)


# ----------
# Refitting restricted quadratic spline w/ case-based knots
# ----------



# Unrestricted quadratic spline (using same knots as for cubic)
datsubrest$qs.1 <- datsubrest$ls.1^2
datsubrest$qs.2 <- datsubrest$ls.2^2
datsubrest$qs.3 <- datsubrest$ls.3^2




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3




fit.rqs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)


summary(fit.rqs)


# -------
# NOW checking model fits w/ case-defined knots
# VIRTUALLY identical results
# -------

BIC(fit.lin, fit.rqs, fit.rcs)
# fit.lin	31	28092.32
# fit.rqs	33	28105.68
# fit.rcs	32	28099.48

# Linear is actually PREFERRED here

AIC(fit.lin, fit.rqs, fit.rcs)
# fit.lin	31	27925.87
# fit.rqs	33	27928.48
# fit.rcs	32	27927.65

# SAME conclusion - linear


#  ---------
# RESULT:
# Should just model OVERALL PA as LINEAR
# TESTED model fit w/ AIC/BIC for restricted cubic/quadratic splines and linear
# CONCLUDED that LINEAR is best specification
# ----------



# --------
# NOW evaluating Dempsey replication w/ LINEAR model
# --------


k1 <- matrix(c(5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.921761196272421
# Lower = 0.899889031208217
# Upper = 0.944164973110972




k1 <- matrix(c(15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 15
# 

delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.783168596039808
# Lower = 0.728730379082598
# Upper = 0.84167350151522




k1 <- matrix(c(25,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 25
# 

delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.665414265976207
# Lower = 0.590126056637079
# Upper = 0.750307735753747




k1 <- matrix(c(35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 35
# 

delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.565365040942153
# Lower = 0.477884239106981
# Upper = 0.668859952604478




k1 <- matrix(c(45,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 45
# 

delta.eta <- glht(fit.lin, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.480358846906585
# Lower = 0.38699078513543
# Upper = 0.596253530224688


# -------
# Conclusions:
# Unsurprisingly, results are virtually identical between spline and linear
# LINEAR MODEL IS THE WAY TO GO!
# -------


# ----------
# Doing FIRST interaction test suggested by Patrick
# Put PA and MVPA into quartiles then compare interactions of them in reg
# -----------

# FIRST splitting PAEEPOS into quartiles
quarterpa <- quantile(datsubrest$PAEEPOS, probs = c(0.25, 0.50, 0.75))
quarterpa

datsubrest$QuarterPA <- ifelse(datsubrest$PAEEPOS < 31.79, 1,
                               ifelse(datsubrest$PAEEPOS >= 31.79 & datsubrest$PAEEPOS < 38.76, 2,
                                      ifelse(datsubrest$PAEEPOS >= 38.76 & datsubrest$PAEEPOS < 46.56, 3, 4)))


# NOW splitting MVPA into quartiles
quartermvpa <- quantile(datsubrest$PercentMVPA, probs = c(0.25, 0.50, 0.75))
quartermvpa

datsubrest$QuarterMVPA <- ifelse(datsubrest$PercentMVPA < 0.28, 1,
                               ifelse(datsubrest$PercentMVPA >= 0.28 & datsubrest$PercentMVPA < 0.36, 2,
                                      ifelse(datsubrest$PercentMVPA >= 0.36 & datsubrest$PercentMVPA < 0.44, 3, 4)))




fit.test <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ as.factor(QuarterMVPA)*as.factor(QuarterPA) + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)

summary(fit.test)
# ONLY close to significant interaction is between MVPA 3 & PA 4 and 2 & 2 and 4 & 4






# ---------
# Should go through SAME process of model selection for % MVPA
# Cubic vs Quadratic vs Linear
# ---------


knots <- quantile(datsubrest$PercentMVPA, probs = c(0.10, 0.50, 0.90))
knots
# 10% 0.213109755634595
# 50% 0.35725720776084
# 90% 0.507174876692717



datsubrest$ls.1 <- (datsubrest$PercentMVPA - 0.213109755634595)*as.integer(datsubrest$PercentMVPA > 0.213109755634595)
datsubrest$ls.2 <- (datsubrest$PercentMVPA - 0.35725720776084)*as.integer(datsubrest$PercentMVPA > 0.35725720776084)
datsubrest$ls.3 <- (datsubrest$PercentMVPA - 0.507174876692717)*as.integer(datsubrest$PercentMVPA > 0.507174876692717)



# Unrestricted quadratic spline
datsubrest$qs.1 <- datsubrest$ls.1^3
datsubrest$qs.2 <- datsubrest$ls.2^3
datsubrest$qs.3 <- datsubrest$ls.3^3

# So these are:
# (v-ti)^3 where i = 1:3
# BUT max value of i here is K - 2
# Since k = 3, i = 1 ONLY
# SO qs.2 = tk-1 and qs.3 = tk

# v if i = 0
# o.w.:
# (v-t1)^3 - tK-t1/(tk - tk-1)*(v - tk-1)^3 + tk-1-ti/(tk-tk-1)*(v-tk)^3

# Funky cubic restricting
# THIS would explain why there's only one nonlinear part!
datsubrest$rqs.1 <- datsubrest$qs.1 - (((0.51 - 0.21)/(0.51 - 0.36))*datsubrest$qs.2 + (0.36 - 0.21)/(0.51 - 0.36)*datsubrest$qs.3)*as.integer(datsubrest$PercentMVPA > 0.21)
# as.integer I think is not necessary... But ALSO shouldn't change anything... (all zeros until knot 1)


fit.rcs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PercentMVPA + rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rcs)



# ------
# Create restricted quadratic spline
# ------

# Unrestricted quadratic spline (using same knots as for cubic)
datsubrest$qs.1 <- datsubrest$ls.1^2
datsubrest$qs.2 <- datsubrest$ls.2^2
datsubrest$qs.3 <- datsubrest$ls.3^2




# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3




fit.rqs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PercentMVPA + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)


summary(fit.rqs)


# -----
# Creating linear model
# -----

fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)


summary(fit.lin)

# ------
# Model fit comparison
# ------



# -------
# looking for smallest AIC/BIC for best model fit
# -------

BIC(fit.lin, fit.rqs, fit.rcs)
# fit.lin	31	28059.79
# fit.rqs	33	28072.93
# fit.rcs	33	29975.62

# Linear is actually PREFERRED here

AIC(fit.lin, fit.rqs, fit.rcs)
# fit.lin	31	27893.34
# fit.rqs	33	27895.73
# fit.rcs	33	29798.42

# SAME conclusion - linear



# --------
# NOW check if model WITHOUT interaction produces similar results to Dempsey
# BOTH exposures entering the model LINEARLY
# --------

fit.linINTER <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)


summary(fit.linINTER)


# PA diff - from PAEE 15 to 20
# MVPA from 10 to 20
k1 <- matrix(c(5,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days


delta.eta <- glht(fit.linINTER, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.814945791593442
# Lower = 0.776540117821535
# Upper = 0.855250911052729


# PA diff - from PAEE 15 to 20
# MVPA from 10 to 30
k1 <- matrix(c(5,0.20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days


delta.eta <- glht(fit.linINTER, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.675668492077708
# Lower = 0.606223640749944
# Upper = 0.75306847258844


# PA diff - from PAEE 15 to 30
# MVPA 10
k1 <- matrix(c(15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days


delta.eta <- glht(fit.linINTER, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.949666955185829
# Lower = 0.861757137347674
# Upper = 1.04654465473614


# PA diff - from PAEE 15 to 30
# MVPA 10 to 20
k1 <- matrix(c(15,0.1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days


delta.eta <- glht(fit.linINTER, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.787365302337247
# Lower = 0.732935782655808
# Upper = 0.845836885024556

# PA diff - from PAEE 15 to 30
# MVPA 10 to 30
k1 <- matrix(c(15,0.2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days


delta.eta <- glht(fit.linINTER, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.652801612122341
# Lower = 0.59378987303289
# Upper = 0.717678027435679

# PA diff - from PAEE 15 to 30
# MVPA 10 to 40
k1 <- matrix(c(15,0.3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days


delta.eta <- glht(fit.linINTER, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.541235362448061
# Lower = 0.468264993266534
# Upper = 0.625576803255815

# ... PA diff - from PAEE 15 to 60
# MVPA from 10 to 40
k1 <- matrix(c(45,0.3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days


delta.eta <- glht(fit.linINTER, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.488122488944231
# Lower = 0.393729336250189
# Upper = 0.605145571529651

# -----
# These results are VERY similar to interaction results in Dempsey
# SUGGESTS to me that they ALSO did not find sig interaction
# -----



# --------
# REPEATING RESULTS FOR LINEAR MODELS
# EXCLUDING those w/ cases WITHIN FIRST year of follow up (avoid reverse causation)
# Do w/ JUST PA and then MVPA/PA
# ---------

# Exclude if ind had CAD within one year
datsubrestONEYEAR <- subset(datsubrest, TimeYear > 1 | Status == 0)
# 77474 - 77257
# 217 cases EXCLUDED with this method

# NOW linear PA model
fit.linONEYEAR <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrestONEYEAR)


summary(fit.linONEYEAR)

k1 <- matrix(c(5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.linONEYEAR, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.932863168986052
# Lower = 0.909142782319282
# Upper = 0.957202442756766



k1 <- matrix(c(15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 15
# 

delta.eta <- glht(fit.linONEYEAR, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.811808959724849
# Lower = 0.751443419550366
# Upper = 0.877023831659716




k1 <- matrix(c(25,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 25
# 

delta.eta <- glht(fit.linONEYEAR, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.706463508261193
# Lower = 0.621098493841687
# Upper = 0.803561260336798




k1 <- matrix(c(35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 35
# 

delta.eta <- glht(fit.linONEYEAR, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.614788347093229
# Lower = 0.513363120916327
# Upper = 0.736252169900668




k1 <- matrix(c(45,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 45
# 

delta.eta <- glht(fit.linONEYEAR, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.535009533120688
# Lower = 0.424315461283546
# Upper = 0.674581123356101

# ----------
# Conclusion:
# Slight attenuation from original analysis - as expected
# ----------


# REPEATING for "interaction" model
fit.linINTERONEYEAR <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrestONEYEAR)


summary(fit.linINTERONEYEAR)



k1 <- matrix(c(0,0.1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# MVPA Diff = 0.1
# 

delta.eta <- glht(fit.linINTERONEYEAR, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.834473921344515
# Lower = 0.779076115868823
# Upper = 0.893810901425889




k1 <- matrix(c(0,0.2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# MVPA Diff = 20
# 

delta.eta <- glht(fit.linINTERONEYEAR, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.696346725404092
# Lower = 0.606959594317251
# Upper = 0.798897927507759




k1 <- matrix(c(0,0.3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# MVPA Diff = 30
# 

delta.eta <- glht(fit.linINTERONEYEAR, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.581083182563366
# Lower = 0.472867723230001
# Upper = 0.714063676732985










# -------
# TRY OUT INTERACTIONS
# Try NORMALLY then try w/ standardized variables...
# -------




# ------------
# HAVE TO REPEAT ALL ANALYSES WITH MVPA AND VIGOROUS PA TO ACCOUNT FOR INTENSITY
# Dempsey approach - interact log(MVPA) or log(Vigorous) w/ knots of the spline
# THEN set PA volume at some level and increase %PA intensity and interpret
# SHOULD ULTIMATELY BE IDENTICAL MODEL WITH THIS ADDITION...

# WAIT: WHY log transform %PAEE from MVPA? Isn't part of advantage of log
# That it can be interpreted as % change?

# WAIT... WHAT ABOUT just doing a COMPOSITIONAL data analysis
# ------------



# -------
# MAKE interaction w/ rcs and manually
# NOTE: JUST using %MVPA here
# -------

# ---------
# THERE is actually an update to rms package that explicitly allows for interact
# https://cran.r-project.org/web/packages/interactionRCS/vignettes/vignette.html#Introduction
# ---------

# ------
# START by fitting WIHOUT interaction
# Interpret as effect of change in MVPA at some constant PA
# ------



# -------
# ALTHOUGH in Dempsey MVPA is ALSO modeled as a spline
# So ALSO try this WITHOUT interaction
# -------


# fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + PercentMVPA + rqs.1MVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)
fit.rqs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + PercentMVPA + rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)


summary(fit.rqs)

# FORMALLY testing multicollinearity question
# Source: https://stackoverflow.com/questions/23518075/testing-multicollinearity-in-cox-proportional-hazards-using-r
install.packages('rms')
library(rms)

cvif <- vif(fit.rqs) 
cvif

# --------
# Strange:
# VIF actually is NOT high for PA volume or percent MVPA
# Both have highest overall but 2.02 for PA and 4.73 for percent MVPA

# Yet it seems self-evident that these are explaining part of same variation
# --------



# 10% increase in MVPA - from 10 to 20
k1 <- matrix(c(0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.947351023568257
# Lower = 0.889617061206014
# Upper = 1.00883177829251

MVPA50 <- subset(datsubrest, PercentMVPA >= 0.50 & PercentMVPA < 0.51)

# 40% increase in MVPA - from 10 to 50
k1 <- matrix(c(0,0.40,0.018,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.798793086502127
# Lower = 0.621200515026526
# Upper = 1.02715689959843



# Makes sense it's fringe significant here... Only sig at 9% level...
# DEF collinearity issue there...


# -------
# TRY OUT ADDING BMI
# No real change...
# -------


fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + PercentMVPA + BMI_InstChosen + rqs.1MVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)

# 10% increase in MVPA - from 10 to 20
k1 <- matrix(c(0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.979528490143869
# Lower = 0.888896923351593
# Upper = 1.07940081442268






# ----------
# COMPOSITIONAL data analysis
# Substituting OTHER activities for increase in MVPA
# ----------







# ----------
# What if I do this part manually?
# Applying PH violating model from earlier
# ----------


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

# So these are:
# (v-ti)^3 where i = 1:3
# BUT max value of i here is K - 2
# Since k = 3, i = 1 ONLY
# SO qs.2 = tk-1 and qs.3 = tk

# v if i = 0
# o.w.:
# (v-t1)^3 - tK-t1/(tk - tk-1)*(v - tk-1)^3 + tk-1-tj/(tk-tk-1)*(v-tk)^3

# Funky cubic restricting
# THIS would explain why there's only one nonlinear part!
datsubrest$rqs.1MVPA <- datsubrest$qs.1MVPA - (((0.51 - 0.21)/(0.51 - 0.36))*datsubrest$qs.2MVPA + (0.36 - 0.21)/(0.51 - 0.36)*datsubrest$qs.3MVPA)*as.integer(datsubrest$PercentMVPA > 0.26)




fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + PercentMVPA + rqs.1 + rqs.1MVPA + rqs.1MVPA*rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)

# --------
# OTHER POINT ON DEMPSEY THOUGH - log(PercentMVPA) in INTERACTION ANALYSES
# THIS is for COLLINEARITY ONLY IN INTERACTION I THINK...

# But ALSO looks like they model MVPA AS cubic

# THIS is fitting manually based on Dempsey (I think...)
# INTERACTIONS are between orthogonal spline variables, NOT main effects...
# --------

# Getting predictions from this approach
length(coef(fit.rqs))
# 36


# 10 to 20 MVPA
k1 <- matrix(c(0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.938996941324149
# Lower = 0.846548036993949
# Upper = 1.04154190581675

# So interpret this as (for instance):
# Person at PAEE = 15 (below first knot really...)
# Increases their composition of MVPA by 10%


# What if we interacted with MAIN EFFECT too???
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + PercentMVPA + PAEEPOS*PercentMVPA + rqs.1 + rqs.1MVPA + rqs.1MVPA*rqs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)



# Getting predictions from this approach
length(coef(fit.rqs))
# 37

# 10 to 20 MVPA
k1 <- matrix(c(0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1.5,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Hardly any change here (at PAEE = 15 and 10% increase)
# Estimate = 0.935002396109114
# Lower = 0.838684032056804
# Upper = 1.04238240781312


# ------
# NOW try replicating Dempsey and see
# ------


# 10 to 30 MVPA - DEMPSEY NEVER DOES THIS COMPARISON
k1 <- matrix(c(0,0.20,0,0.00066,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Hardly any change here (at PAEE = 15 and 10% increase)
# Estimate = 0.935002396109114
# Lower = 0.838684032056804
# Upper = 1.04238240781312


# 10 to 20 MVPA and PAEE from 15 to 20

k1 <- matrix(c(5,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.901320367583809
# Lower = 0.790931941710467
# Upper = 1.02711543456516


# 10 to 30 MVPA and PAEE from 15 to 20

k1 <- matrix(c(5,0.20,0.00066,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.0033), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.830619588855184
# lower = 0.605920476412791
# upper = 1.13864595808763




# 10 to 30 MVPA and PAEE from 15 to 30

k1 <- matrix(c(15,0.20,69,0.00066,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0.046), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 0.778098654106452
# Lower = 0.536510492362766
# Upper = 1.12847283350593






# ----------
# I THINK THE COLLINEARITY ISSUE IS JUST KILLING US...
# IS *THIS* why they took the log???
# TRY STANDARDIZING BOTH BEFOREHAND AND SEE IF RESULTS ARE WACKY STILL

# Sources:
# https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/

# SHOULD COMPARE VIF/correlation matrix before and after

# WAIT ALSO - doesn't cox standardize automatically???
# -----------

cor(datsubrest$PAEEPOS, datsubrest$PercentMVPA)
# Just as base test strongly correlated = 0.69 (matches strongly Strain)


# WAIT... Can I just do this? Holding PA constant?
# Only trouble is THEN MVPA does NOT depend on PA
# BUT this is how Strain et al. appear to model it

# Let's start there... Simpler anyhow
fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + PercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)

length(coef(fit.rqs))
# 34

# NOW what happens with a 10% increase in MVPA?


# 10 to 20 MVPA
k1 <- matrix(c(0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]






# Ah - for interaction analyses:
# log transformed and centered PAEE w/ interaction terms between activity exposures
datsubrest$logPAEEPOS <- ifelse(datsubrest$PAEEPOS == 0, log(0.01), log(datsubrest$PAEEPOS))
datsubrest$standlogPAEEPOS <- scale(datsubrest$logPAEEPOS)




# 10 to 20 MVPA
k1 <- matrix(c(0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow=1) # Calc log-HR @ 30 days

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]








# -----------
# OR just fit spline to PAEE and interact MVPA w/ it
# LINEAR MVPA interacted w/ spline variables???
# -----------









# ---
# SHOULD:
# GET INTERACTION TO WORK
# Read in rest of the poss variables
# Get imputation figured out
# update figures n tables - FINALIZE
# ---


