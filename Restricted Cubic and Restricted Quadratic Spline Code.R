# --------
# CONFIRMING spline code is CORRECT
# --------

# -------
# Restricted Cubic Spline Code
# -------

# First step is checking the knot locations for the spline
knots <- quantile(datsubrest$PAEEPOS, probs = c(0.10, 0.50, 0.90))
knots
# 10% 25.9491318778412
# 50% 38.7595302926387
# 90% 54.5613992585524

# ----
# Splines placed at 10th/50th/90th percentiles
# ----


# Coding linear spline terms
datsubrest$ls.1 <- (datsubrest$PAEEPOS - 25.9491318778412)*as.integer(datsubrest$PAEEPOS > 25.9491318778412)
datsubrest$ls.2 <- (datsubrest$PAEEPOS - 38.7595302926387)*as.integer(datsubrest$PAEEPOS > 38.7595302926387)
datsubrest$ls.3 <- (datsubrest$PAEEPOS - 54.5613992585524)*as.integer(datsubrest$PAEEPOS > 54.5613992585524)



# Converting linear spline terms to unrestricted cubic spline
datsubrest$cs.1 <- datsubrest$ls.1^3
datsubrest$cs.2 <- datsubrest$ls.2^3
datsubrest$cs.3 <- datsubrest$ls.3^3

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
datsubrest$rcs.1 <- datsubrest$cs.1 - (((54.56 - 25.95)/(54.56 - 38.76))*datsubrest$cs.2 + (38.76 - 25.95)/(54.56 - 38.76)*datsubrest$cs.3)*as.integer(datsubrest$PAEEPOS > 25.9491318778412)


# Fitting model with linear term and restricted cubic term
fit.rqs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + rcs.1 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)





# ---------
# Restricted Quadratic Spline
# ---------


# Unrestricted quadratic spline (using same linear knot terms as cubic)
datsubrest$qs.1 <- datsubrest$ls.1^2
datsubrest$qs.2 <- datsubrest$ls.2^2
datsubrest$qs.3 <- datsubrest$ls.3^2


# Doing the restricting
datsubrest$rqs.1 <- datsubrest$qs.1 - datsubrest$qs.3
datsubrest$rqs.2 <- datsubrest$qs.2 - datsubrest$qs.3




fit.rqs <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex), data = datsubrest)


summary(fit.rqs)



