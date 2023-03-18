# NOTE SHOULD GET RESULTS BY AGE AND REPEAT THESE FIGURES!!!!


# --------
# Taking data and model from Dempsey replication to create figure
# --------


fit.rqs <- coxph(Surv(TimeAge, Status) ~ PAEEPOS + rqs.1 + rqs.2 + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + Biological.Sex + as.factor(Salt_InstChosen) + as.factor(NewAlc) + as.factor(NewOilFish) + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen), data = datsubrest)


summary(fit.rqs)

# 20th percentile of PA and of PGS

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


# ------
# COMPARING 20th/20th to all the rest here
# ------

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(41.53 - 31.79)*as.integer(41.53 > 31.79)
(41.53 - 38.76)*as.integer(41.53 > 38.76)
(41.53 - 46.56)*as.integer(41.53 > 46.56)
#9.74
#2.77
#0


# Creating spline codes to add
qs.1 <- 7.14^3
qs.2 <- 7.14^3
qs.3 <- 2.11^3
# 363.9943
# 363.9943
# 9.393931

rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3
# 354.6004
# 354.6004

48.67 - 41.53
# 7.14

k1 <- matrix(c(-7.14, -354.6004, -354.6004, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.06278867716369
# Lower = 0.971858196642359
# Upper = 1.162226934145


# THIS shows:
# Difference between PA 20th/genetic 20th and PA 40th/genetic 20th



# -----
# SAME MODEL W PA 20th/20th vs PA 60th/20th
# -----

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(36.03 - 31.79)*as.integer(36.03 > 31.79)
(36.03 - 38.76)*as.integer(36.03 > 38.76)
(36.03 - 46.56)*as.integer(36.03 > 46.56)
#4.24
#0
#0


# Creating spline codes to add
qs.1 <- 12.64^3
qs.2 <- 9.91^3
qs.3 <- 2.11^3
# 2019.488
# 973.2423
# 9.393931

rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3

48.67 - 36.03
# 12.64

k1 <- matrix(c(-12.64, -2010.094, -963.8483, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.10273970248323
# Lower = 0.982847117637892
# Upper = 1.23725738175368


# THIS shows:
# Difference between PA 20th/genetic 20th and PA 60th/genetic 20th



# -----
# PA 20th vs PA 80th
# -----

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(30.16 - 31.79)*as.integer(30.16 > 31.79)
(30.16 - 38.76)*as.integer(30.16 > 38.76)
(30.16 - 46.56)*as.integer(30.16 > 46.56)
#0
#0
#0


# Creating spline codes to add
qs.1 <- 16.88^3
qs.2 <- 9.91^3
qs.3 <- 2.11^3
# 4809.693
# 973.2423
# 9.393931


rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3

48.67 - 30.16
# 18.51

k1 <- matrix(c(-18.51, -4800.299, -963.8483, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.11675038716192
# Lower = 0.99262052705168
# Upper = 1.25640301931955


# ----------
# REPEATING this process w/ changing PGS
# PGS 40th vs 20th
# ----------

# FIRST difference between HR for PGS changes ONLY w/ same PA
# 20% -0.846868164911578
# 40% -0.266874315609926
# 60% 0.236609243902655
# 80% 0.821322441664612

# 20th vs 40th
-0.266874315609926 - -0.846868164911578
k1 <- matrix(c(0, 0, 0, 0.5799938, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.2814500611353
# Lower = 1.24379204256757
# Upper = 1.32024824326246

# 20th vs 60th
0.236609243902655 - -0.846868164911578
k1 <- matrix(c(0, 0, 0, 1.083477, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.58926299430888
# Lower = 1.50313081479609
# Upper = 1.68033070722608

# 20th vs 80th
0.821322441664612 - -0.846868164911578
k1 <- matrix(c(0, 0, 0, 1.668191, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.04067560401943
# Lower = 1.872904546309
# Upper = 2.22347525881493


# ------
# COMPARING 20th/20th to all the rest here (AT 40th PGS)
# ------

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(41.53 - 31.79)*as.integer(41.53 > 31.79)
(41.53 - 38.76)*as.integer(41.53 > 38.76)
(41.53 - 46.56)*as.integer(41.53 > 46.56)
#9.74
#2.77
#0


# Creating spline codes to add
qs.1 <- 7.14^3
qs.2 <- 7.14^3
qs.3 <- 2.11^3
# 363.9943
# 363.9943
# 9.393931

rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3
# 354.6004
# 354.6004

48.67 - 41.53
# 7.14

k1 <- matrix(c(-7.14, -354.6004, -354.6004, 0.5799938, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.36191061532531
# Lower = 1.23956354859757
# Upper = 1.49633354920308


# THIS shows:
# Difference between PA 20th/genetic 20th and PA 40th/genetic 40th



# -----
# SAME MODEL W PA 20th/20th vs PA 60th/40th
# -----

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(36.03 - 31.79)*as.integer(36.03 > 31.79)
(36.03 - 38.76)*as.integer(36.03 > 38.76)
(36.03 - 46.56)*as.integer(36.03 > 46.56)
#4.24
#0
#0


# Creating spline codes to add
qs.1 <- 12.64^3
qs.2 <- 9.91^3
qs.3 <- 2.11^3
# 2019.488
# 973.2423
# 9.393931

rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3

48.67 - 36.03
# 12.64

k1 <- matrix(c(-12.64, -2010.094, -963.8483, 0.5799938, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.41310585916346
# Lower = 1.25498367459405
# Upper = 1.59115071345294



# THIS shows:
# Difference between PA 20th/genetic 20th and PA 60th/genetic 40th



# -----
# PA 20th vs PA 80th
# -----

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(30.16 - 31.79)*as.integer(30.16 > 31.79)
(30.16 - 38.76)*as.integer(30.16 > 38.76)
(30.16 - 46.56)*as.integer(30.16 > 46.56)
#0
#0
#0


# Creating spline codes to add
qs.1 <- 16.88^3
qs.2 <- 9.91^3
qs.3 <- 2.11^3
# 4809.693
# 973.2423
# 9.393931


rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3

48.67 - 30.16
# 18.51

k1 <- matrix(c(-18.51, -4800.299, -963.8483, 0.5799938, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.43105985190151
# Lower = 1.26778927838488
# Upper = 1.61535701132712





# ------
# COMPARING 20th/20th to all the rest here (AT 60th PGS)
# ------

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(41.53 - 31.79)*as.integer(41.53 > 31.79)
(41.53 - 38.76)*as.integer(41.53 > 38.76)
(41.53 - 46.56)*as.integer(41.53 > 46.56)
#9.74
#2.77
#0


# Creating spline codes to add
qs.1 <- 7.14^3
qs.2 <- 7.14^3
qs.3 <- 2.11^3
# 363.9943
# 363.9943
# 9.393931

rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3
# 354.6004
# 354.6004

48.67 - 41.53
# 7.14

k1 <- matrix(c(-7.14, -354.6004, -354.6004, 1.083477, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.68905071538673
# Lower = 1.52051247230028
# Upper = 1.87627025172144


# THIS shows:
# Difference between PA 20th/genetic 20th and PA 40th/genetic 60th



# -----
# SAME MODEL W PA 20th/20th vs PA 60th/60th
# -----

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(36.03 - 31.79)*as.integer(36.03 > 31.79)
(36.03 - 38.76)*as.integer(36.03 > 38.76)
(36.03 - 46.56)*as.integer(36.03 > 46.56)
#4.24
#0
#0


# Creating spline codes to add
qs.1 <- 12.64^3
qs.2 <- 9.91^3
qs.3 <- 2.11^3
# 2019.488
# 973.2423
# 9.393931

rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3

48.67 - 36.03
# 12.64

k1 <- matrix(c(-12.64, -2010.094, -963.8483, 1.083477, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.75254340151178
# Lower = 1.54279765269265
# Upper = 1.99080441224548




# THIS shows:
# Difference between PA 20th/genetic 20th and PA 60th/genetic 60th



# -----
# PA 20th vs PA 80th
# -----

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(30.16 - 31.79)*as.integer(30.16 > 31.79)
(30.16 - 38.76)*as.integer(30.16 > 38.76)
(30.16 - 46.56)*as.integer(30.16 > 46.56)
#0
#0
#0


# Creating spline codes to add
qs.1 <- 16.88^3
qs.2 <- 9.91^3
qs.3 <- 2.11^3
# 4809.693
# 973.2423
# 9.393931


rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3

48.67 - 30.16
# 18.51

k1 <- matrix(c(-18.51, -4800.299, -963.8483, 1.083477, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 1.77481006419654
# Lower = 1.55902519820797
# Upper = 2.02046173954986







# ------
# COMPARING 20th/20th to all the rest here (AT 80th PGS)
# ------

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(41.53 - 31.79)*as.integer(41.53 > 31.79)
(41.53 - 38.76)*as.integer(41.53 > 38.76)
(41.53 - 46.56)*as.integer(41.53 > 46.56)
#9.74
#2.77
#0


# Creating spline codes to add
qs.1 <- 7.14^3
qs.2 <- 7.14^3
qs.3 <- 2.11^3
# 363.9943
# 363.9943
# 9.393931

rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3
# 354.6004
# 354.6004

48.67 - 41.53
# 7.14

k1 <- matrix(c(-7.14, -354.6004, -354.6004, 1.668191, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)
# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.16880692571602
# Lower = 1.91665503615692
# Upper = 2.454131490696


# THIS shows:
# Difference between PA 20th/genetic 20th and PA 40th/genetic 80th



# -----
# SAME MODEL W PA 20th/20th vs PA 60th/60th
# -----

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(36.03 - 31.79)*as.integer(36.03 > 31.79)
(36.03 - 38.76)*as.integer(36.03 > 38.76)
(36.03 - 46.56)*as.integer(36.03 > 46.56)
#4.24
#0
#0


# Creating spline codes to add
qs.1 <- 12.64^3
qs.2 <- 9.91^3
qs.3 <- 2.11^3
# 2019.488
# 973.2423
# 9.393931

rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3

48.67 - 36.03
# 12.64

k1 <- matrix(c(-12.64, -2010.094, -963.8483, 1.668191, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.25033400844118
# Lower = 1.95049303787508
# Upper = 2.59626825177689





# THIS shows:
# Difference between PA 20th/genetic 20th and PA 60th/genetic 80th



# -----
# PA 20th vs PA 80th
# -----

(48.67 - 31.79)*as.integer(48.67 > 31.79)
(48.67 - 38.76)*as.integer(48.67 > 38.76)
(48.67 - 46.56)*as.integer(48.67 > 46.56)
#16.88
#9.91
#2.11


(30.16 - 31.79)*as.integer(30.16 > 31.79)
(30.16 - 38.76)*as.integer(30.16 > 38.76)
(30.16 - 46.56)*as.integer(30.16 > 46.56)
#0
#0
#0


# Creating spline codes to add
qs.1 <- 16.88^3
qs.2 <- 9.91^3
qs.3 <- 2.11^3
# 4809.693
# 973.2423
# 9.393931


rqs.1 <- qs.1 - qs.3
rqs.2 <- qs.2 - qs.3

48.67 - 30.16
# 18.51

k1 <- matrix(c(-18.51, -4800.299, -963.8483, 1.668191, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1)

# PA Diff = 5
# 

delta.eta <- glht(fit.rqs, linfct=k1)
exp(confint(delta.eta)$confint)[,1:3]
# Estimate = 2.27892527086058
# Lower = 1.97175938596177
# Upper = 2.63394226858654


# -------
# Creating Quintile FIGURE
# -------


# ---------
# FIGURES to compare HR for different genetic and PA groups
# COMPARING DECILES from 20th to 80th
# ---------


# Selecting coefficients from No Highschool model (Obs)
NoHSObsCoeff <- 1.00

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLNoHSObs <- 1.00
ULNoHSObs <- 1.00


# Selecting coefficients from No Highschool model (IV)
NoHSIVCoeff <- 1.06

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLNoHSIV <- 0.97
ULNoHSIV <- 1.16



PointCompNoHSIV <- c(LLNoHSIV, NoHSIVCoeff, ULNoHSIV)
PointCompNoHSIV <- as.data.frame(PointCompNoHSIV)

PointCompNoHSObs <- c(LLNoHSObs, NoHSObsCoeff, ULNoHSObs)
PointCompNoHSObs <- as.data.frame(PointCompNoHSObs)

PointyNoHS <- cbind(PointCompNoHSIV, PointCompNoHSObs)

# Transpose
PointyTNoHS <- t(PointyNoHS)
PointyTNoHS <- as.data.frame(PointyTNoHS)

colnames(PointyTNoHS) <- c("LL","Mean","UL")

PointyTNoHS$Model <- c("40th Overall PA", "20th Overall PA")


# Selecting coefficients from Highschool model (Obs)
HSObsCoeff <- 1.10

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLHSObs <- 0.98
ULHSObs <- 1.24


# Selecting coefficients from No Highschool model (IV)
HSIVCoeff <- 1.12

# Creating lower and upper bound confidence interval objects from No Highschool model (Obs)
LLHSIV <- 0.99
ULHSIV <- 1.26



PointCompHSIV <- c(LLHSIV, HSIVCoeff, ULHSIV)
PointCompHSIV <- as.data.frame(PointCompHSIV)

PointCompHSObs <- c(LLHSObs, HSObsCoeff, ULHSObs)
PointCompHSObs <- as.data.frame(PointCompHSObs)

PointyHS <- cbind(PointCompHSIV, PointCompHSObs)

# Transpose
PointyTHS <- t(PointyHS)
PointyTHS <- as.data.frame(PointyTHS)

colnames(PointyTHS) <- c("LL","Mean","UL")

PointyTHS$Model <- c("60th Overall PA", "80th Overall PA")


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
                     xmax = UL)) + geom_point(shape = 15, size  = 4, aes(color = Genetic.Risk)) + theme_bw() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio")


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




# ADDING HAZARD RATIOS VIA ANNOTATION
# From: https://www.khstats.com/blog/forest-plots/

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[4] <- "1 (Reference Group)"

install.packages('ggforce')
library(ggforce)



# MODEL WORKS
ggplot(PlotDF, aes(x = Mean, y = Model)) +
  geom_linerange(aes(xmin = LL,
                     xmax = UL)) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 1.6, y = Model, hjust = 0, label = LABEL)) + expand_limits(x = 2) + scale_colour_manual(values=cbbPalette) +   geom_vline(xintercept = 1, linetype="dashed") +   annotate("text", x = 1.7, y = 2.4, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Physical.Activity, scales = "free_y",space = "free", strip.position = "bottom")



# NOW splitting by PA PERCENTILE as variables and PGS as ind observations

# Adding column for PGS
PlotDF$PGS <- "20th Genetic Risk"

# Adding observations by other PGS groups
# 20th PA
D1 <- c(1.24, 1.28, 1.32, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.28 (1.24-1.32)", "40th Genetic Risk")

D2 <- c(1.50, 1.59, 1.68, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.59 (1.50-1.68)", "60th Genetic Risk")

D3 <- c(1.87, 2.04, 2.22, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "2.04 (1.87-2.22)", "80th Genetic Risk")

# 40th GS
D4 <- c(1.24, 1.36, 1.50, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.36 (1.24-1.50)", "40th Genetic Risk")

D5 <- c(1.25, 1.41, 1.59, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.41 (1.25-1.59)", "40th Genetic Risk")

D6 <- c(1.27, 1.43, 1.62, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "40th Genetic Risk")

# 60th GS
D7 <- c(1.52, 1.69, 1.88, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.69 (1.52-1.88)", "60th Genetic Risk")

D8 <- c(1.54, 1.75, 1.99, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.75 (1.54-1.99)", "60th Genetic Risk")

D9 <- c(1.56, 1.77, 2.02, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "60th Genetic Risk")

# 80th GS
D10 <- c(1.92, 2.17, 2.45, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "2.17 (1.92-2.45)", "80th Genetic Risk")

D11 <- c(1.95, 2.25, 2.60, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "2.25 (1.95-2.60)", "80th Genetic Risk")

D12 <- c(1.97, 2.28, 2.63, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "2.28 (1.97-2.63)", "80th Genetic Risk")

# Merging all with PlotDF
# PlotDF <- rbind(PlotDF, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12)

# Bunching up... What if we break it up?
# Do 20th vs 40th PA risk, etc...

PlotDF <- rbind(PlotDF, D1, D2, D3, D4, D7, D10)


# ---
# 20th vs 40th PA
# ---

# PlotDF20vs40 <- subset(PlotDF, Physical.Activity == "20th Overall PA" | Physical.Activity == "40th Overall PA")

# Remove top two rows here
PlotDF <- PlotDF[-c(1:2), ]

# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = PGS)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.6, y = PGS, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Physical.Activity, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3.5), ylim = c(0,5), expand = TRUE)



# --------
# REPEAT this for 20th PA vs 60th PA
# --------

# Saving earliest results here
#PlotDF20vs40 <- PlotDF


# Adding observations by other PGS groups
# 20th PA
D1 <- c(1.24, 1.28, 1.32, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.28 (1.24-1.32)", "40th Genetic Risk")

D2 <- c(1.50, 1.59, 1.68, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.59 (1.50-1.68)", "60th Genetic Risk")

D3 <- c(1.87, 2.04, 2.22, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "2.04 (1.87-2.22)", "80th Genetic Risk")

# 40th GS
D4 <- c(1.24, 1.36, 1.50, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.36 (1.24-1.50)", "40th Genetic Risk")

D5 <- c(1.25, 1.41, 1.59, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.41 (1.25-1.59)", "40th Genetic Risk")

D6 <- c(1.27, 1.43, 1.62, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "40th Genetic Risk")

# 60th GS
D7 <- c(1.52, 1.69, 1.88, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.69 (1.52-1.88)", "60th Genetic Risk")

D8 <- c(1.54, 1.75, 1.99, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.75 (1.54-1.99)", "60th Genetic Risk")

D9 <- c(1.56, 1.77, 2.02, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "60th Genetic Risk")

# 80th GS
D10 <- c(1.92, 2.17, 2.45, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "2.17 (1.92-2.45)", "80th Genetic Risk")

D11 <- c(1.95, 2.25, 2.60, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "2.25 (1.95-2.60)", "80th Genetic Risk")

D12 <- c(1.97, 2.28, 2.63, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "2.28 (1.97-2.63)", "80th Genetic Risk")

# Merging all with PlotDF
# PlotDF <- rbind(PlotDF, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12)

# Bunching up... What if we break it up?
# Do 20th vs 40th PA risk, etc...

PlotDF <- rbind(PlotDF, D1, D2, D3, D5, D8, D11)

# Drop all NOT 20th or 60th PA (2 and 3 rows)
PlotDF <- PlotDF[-c(2:3), ]


# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = PGS)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.6, y = PGS, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Physical.Activity, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3.5), ylim = c(0,5), expand = TRUE)


# Saving results for table
PlotDF20v60 <- PlotDF





# --------
# REPEAT this for 20th PA vs 80th PA
# --------

# Adding observations by other PGS groups
# 20th PA
D1 <- c(1.24, 1.28, 1.32, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.28 (1.24-1.32)", "40th Genetic Risk")

D2 <- c(1.50, 1.59, 1.68, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "1.59 (1.50-1.68)", "60th Genetic Risk")

D3 <- c(1.87, 2.04, 2.22, "20th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "20th Overall PA", "2.04 (1.87-2.22)", "80th Genetic Risk")

# 40th GS
D4 <- c(1.24, 1.36, 1.50, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.36 (1.24-1.50)", "40th Genetic Risk")

D5 <- c(1.25, 1.41, 1.59, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.41 (1.25-1.59)", "40th Genetic Risk")

D6 <- c(1.27, 1.43, 1.62, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "40th Genetic Risk")

# 60th GS
D7 <- c(1.52, 1.69, 1.88, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "1.69 (1.52-1.88)", "60th Genetic Risk")

D8 <- c(1.54, 1.75, 1.99, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "1.75 (1.54-1.99)", "60th Genetic Risk")

D9 <- c(1.56, 1.77, 2.02, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "1.43 (1.27-1.62)", "60th Genetic Risk")

# 80th GS
D10 <- c(1.92, 2.17, 2.45, "40th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "40th Overall PA", "2.17 (1.92-2.45)", "80th Genetic Risk")

D11 <- c(1.95, 2.25, 2.60, "60th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "60th Overall PA", "2.25 (1.95-2.60)", "80th Genetic Risk")

D12 <- c(1.97, 2.28, 2.63, "80th Overall PA", "Low Genetic Risk", "Low Genetic Risk", "80th Overall PA", "2.28 (1.97-2.63)", "80th Genetic Risk")

# Merging all with PlotDF
# PlotDF <- rbind(PlotDF, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12)

# Bunching up... What if we break it up?
# Do 20th vs 40th PA risk, etc...

PlotDF <- rbind(PlotDF, D1, D2, D3, D6, D9, D12)


# Drop all NOT 20th or 80th PA (2 and 3 rows)
PlotDF <- PlotDF[-c(1,3), ]

# Typo here...
PlotDF$LABEL[7] <- paste("1.77 (1.56-2.02)")
  1.56
1.77
2.02

#PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = PGS)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2.6, y = PGS, hjust = 0, label = LABEL)) + scale_colour_manual(values=cbbPalette) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 3, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Physical.Activity, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3.5), ylim = c(0,5), expand = TRUE)


# Saving results for table
PlotDF20v80 <- PlotDF















# -------
# WAIT looks like a much easier way to do this...
# forestploter package
# -------

install.packages('forestploter')
library(forestploter)

# Drop then re-add label
PlotDF <- PlotDF[-8]

# Adding space to allow for display
PlotDF$` ` <- paste(rep(" ", 20), collapse = " ")

# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[4] <- "1 (Reference Group)"



p <- forest(data = PlotDF[ , c(1:3,8:9)],
  est = PlotDF$Mean,
  lower = PlotDF$LL,
  upper = PlotDF$UL,
  ci_column = 5,
  sizes = 0.4,
  ref_line = 1,
  xlim = c(0,2)
)

plot(p)

# -------
# HAVE results for quintiles (from 20th to 80th)
# CONSIDER doing for deciles - GREATER risk differences and more of range
# CONSIDER doing for tertiles - what others have done (by default)
# -------


