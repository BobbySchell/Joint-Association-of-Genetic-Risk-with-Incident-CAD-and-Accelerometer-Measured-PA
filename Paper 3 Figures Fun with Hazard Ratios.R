# FIXING THIS BASED ON PATRICK FEEDBACK!!!!
# Use library(multicomp)
# Then use glht with difference in covariates
# Slides in presentation lay it out well - will do w/ PA spline AND genetic risk
# https://mail.google.com/mail/u/0/#inbox/QgrcJHsblSFTzDKJnTdJxMcSSZKPxWXfrGB?projector=1&messagePartId=0.4

# ----------
# Fun with Hazard Ratios
# Making new reference groups and figures (by 10th of a percentile)
# Have SINGLE REFERENCE GROUP in each graph (all at mean and PA/genetic risk at some %tile level)
# ----------



# Simple process to get new reference group
# CoefficientXunit at new %tile - CoefficientXunit at old %tile (for genetic risk and PA)
# Old %tile will now be 1.00 HR
# Then EXPONENTIATE this difference (in log hazard before scaling)


# ANOTHER POTENTIALLY INTERESTING KIND OF FIGURE: HOW PA ACROSS PA SPECTRUM AFFECTS HR FOR GEN RISK GROUP
# HAVE IND LINES FOR DIFF GENETIC RISK GROUPS

# -------
# TESTING OUT FIGURE W THIS ARBITRARY 20th/80th PERCENTILE
# 20th/20th as reference group
# -------


HIGHLOW <- quantile(datsubrest$StandPGS, c(0.20, 0.80))
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


# RECALCULATING ALL HRs w/ 80th/80th percentile as reference group (low genetic risk/high overall PA)
# Point Estimates

# 80th/80th original point estimate (WILL be 1.00 HR) - HIGH/HIGH here
# -0.468644482933133
# HR = 1.00 (REF)
# 95% CI = NA

# 80th genetic/20th PA
# -0.302612795826811
#	Original CI (exponentiated) = log(0.6725787) log(0.8117284)
# Converting back to log hazard = -0.3966361 -0.2085895
# HR = exp(-0.302612795826811 - -0.468644482933133) = 1.18
# 95% CI = 


# 20th genetic/80th PA
# 0.231481047592225
# Original CI (exponentiated) = log(1.128149) log(1.408301)
# Converting back to log hazard = 0.1205782 0.342384
# HR = exp(0.231481047592225 - -0.468644482933133) = 2.01
# 95% CI = 


# 20th genetic/20th PA
# 0.397512734698546
# Original CI (exponentiated) = log(1.355333) log(1.633914)
# Converting back to log hazard = 0.3040472 0.4909784
# HR = exp(0.397512734698546 - -0.468644482933133) = 2.38



# ---------
# FIGURES to compare HR for different genetic and PA groups
# DO LINEAR VS RESTRICTED QUADRATIC SPLINE
# COMPARING LOW/HIGH GENETIC RISK AND LOW/HIGH PA
# 80th/20th percentile chosen arbitrarily....
# ---------


# Selecting coefficients from No Highschool model (Obs)
NoHSObsCoeff <- 1.00

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLNoHSObs <- 1.00
ULNoHSObs <- 1.00


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



