# -----
# Further sensitivity analyses & appendix work
# -----

# Base KM Curve
#### FOR THE KM SURVIVAL FUNCTION
survmod1 <- survfit(Surv(AgeBaseline, AgeBaseline + TimeYear, Status)~1, data=whitedata,
                    conf.type="log-log")

survmod1.summary <- summary(survmod1)


plot(survmod1,
     main="Kaplan-Meier survival estimates",
     xlab="Age",
     ylab="Survival Probability",
     bty="l", ylim = c(1.0,0.85), xlim = c(60,90))



# MODEL 3 - BMI, Sleep Dur, Meds


install.packages('mice')
library(mice)

whitedatasub <- whitedata[ , c("AgeBaseline", "TimeYear", "Status", "PAEEPOS", "StandPGS", "SeasonWear", "Salt_InstChosen", "AlcIntake_InstChosen", "OilyFish_InstChosen", "FnVScore", "ProcMeat_InstChosen", "ParentHist", "MobilityDichot", "NewEmploy", "Townsend", "NewEduc", "SmokStat_InstChosen", "Biological.Sex", "REGION", "p22009_a1",
                               "p22009_a2", "p22009_a3", "p22009_a4", "p22009_a5", "p22009_a6", "p22009_a7", "p22009_a8", "p22009_a9", "p22009_a10", "Meds", "BMI_InstChosen", "SleepDur_InstChosen", "ManLabor_Inst0", "WalkorStandWork_Inst0", "PercentMVPA")]


imputed_Data <- mice(whitedatasub, m=5, method = 'pmm', seed = 500)
whitedataimputed <- complete(imputed_Data)

whitedataimputed$ScalePAEEPOS <- scale(whitedataimputed$PAEEPOS)

fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ ScalePAEEPOS + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + OilyFish_InstChosen + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + Meds + BMI_InstChosen + SleepDur_InstChosen + ManLabor_Inst0 + WalkorStandWork_Inst0, data = whitedataimputed)

summary(fit.lin)

whitedataimputed$ScalePercentMVPA <- scale(whitedataimputed$PercentMVPA)

fit.lin <- coxph(Surv(AgeBaseline, AgeBaseline + TimeYear, Status) ~ ScalePAEEPOS + ScalePercentMVPA + StandPGS + p22009_a1 + p22009_a2 + p22009_a3 + p22009_a4 + p22009_a5 + p22009_a6 + p22009_a7 + p22009_a8 + p22009_a9 + p22009_a10 + SeasonWear + as.factor(Salt_InstChosen) + AlcIntake_InstChosen + OilyFish_InstChosen + FnVScore + ProcMeat_InstChosen + ParentHist + MobilityDichot + NewEmploy + Townsend + as.factor(NewEduc) + as.factor(SmokStat_InstChosen) + strata(Biological.Sex) + REGION + Meds + BMI_InstChosen + SleepDur_InstChosen + ManLabor_Inst0 + WalkorStandWork_Inst0, data = whitedataimputed)

summary(fit.lin)




# GRAPHING relationship of PGS and of PA volume for Black and Asian

# ----
# Black
# Plotting PAEE and % MVPA and incident CAD
# RAW DATA
# AND show regression results
# ----

# RESTRICT to black dataset based on self-report
blackdata <- subset(data, Black == 1)
dim(blackdata)
# 421 x 74

summary(as.factor(blackdata$Status))
# Only 5 cases here

blackdataStat1 <- subset(blackdata, Status == 1)

ggplot(data = blackdataStat1, aes(x = PAEEPOS, y = StandPGS)) + geom_point() + theme_classic() + ggtitle("Cases of Incident CAD by Polygenic Risk Score and Physical Activity Volume") + xlab("Physical Activity Volume (PAEE)") + ylab("Standardized Polygenic Risk Score")

# ----
# Asian
# Plotting StandPGS and incident CAD
# RAW DATA
# AND show regression results
# ----

# RESTRICT to asian dataset based on self-report
asiandata <- subset(data, Asian == 1)
dim(asiandata)
# 1090 x 74

summary(as.factor(asiandata$Status))
# Only 33 cases here


asiandataStat1 <- subset(asiandata, Status == 1)

ggplot(data = asiandataStat1, aes(x = PAEEPOS, y = StandPGS)) + geom_point() + geom_smooth() + theme_classic() + ggtitle("CAD Events by Polygenic Risk Score and Physical Activity Volume") + xlab("Physical Activity Volume (PAEE)") + ylab("Standardized Polygenic Risk Score")


# ------
# CHECKING relationship of subjective PA OVER TIME and objective PA
# ------

# Keep dates to know which Subjective PAs are closest


whitedatasub <- whitedata[ , c("AgeBaseline", "TimeYear", "Status", "PAEEPOS", "StandPGS", "SeasonWear", "Salt_InstChosen", "AlcIntake_Weekly", "OilyFish_InstChosen", "FnVScore", "ProcMeat_InstChosen", "ParentHist", "MobilityDichot", "NewEmploy", "Townsend", "NewEduc", "SmokStat_InstChosen", "Biological.Sex", "REGION")]

# Getting dates of subjective PA and Accel Wear Date
FINAL <- FINAL[ , c("eid", "AccelDate", "Date.Attending.Assess.Center.Inst.0", "Date.Attending.Assess.Center.Inst.1", "Date.Attending.Assess.Center.Inst.2", "Date.Attending.Assess.Center.Inst.3")]

# Getting SUBJPA - Percent MVPA
# BEST comparison anyway
# ADDING MVPA and VIG TOGETHER
# Note: ONLY visit 0 has MOST non-missing (as expected)
SUBJPA$SumMVPA.0 <- as.numeric(SUBJPA$p894_i0) + as.numeric(SUBJPA$p914_i0)
SUBJPA$SumMVPA.1 <- as.numeric(SUBJPA$p894_i1) + as.numeric(SUBJPA$p914_i1)
SUBJPA$SumMVPA.2 <- as.numeric(SUBJPA$p894_i2) + as.numeric(SUBJPA$p914_i2)
SUBJPA$SumMVPA.3 <- as.numeric(SUBJPA$p894_i3) + as.numeric(SUBJPA$p914_i3)

SUBJPA <- SUBJPA[ , c("eid", "SumMVPA.0", "SumMVPA.1", "SumMVPA.2", "SumMVPA.3")]

# Keeping ONLY eid and PercentMVPA
whitedata$MVPAVOL <- whitedata$PercentMVPA*whitedata$PAEEPOS

whitedataPercentMVPA <- whitedata[ , c("eid", "PercentMVPA", "MVPAVOL")]

# Source: http://www.sthda.com/english/wiki/correlation-analyses-in-r

# Merging as one large dataset
CorrData <- merge(FINAL, SUBJPA, by = "eid", all = T)
CorrData <- merge(CorrData, whitedataPercentMVPA, by = "eid", all = T)

summary(is.na(CorrData))
# Checked missingness - NONE in visit 0
# About 5/6 in visit 1
# About 1/2 in visit 2
# ALMOST ALL in visit 3

# Least missingness so comparing Visit 0 and Visit 2 corr
# ALL FOR INDS WITHOUT MISSINGNESS IN EITHER

install.packages('corrplot')
install.packages('RColorBrewer')

library(corrplot)
library(RColorBrewer)

# CorrData visit 0 and visit 1 for comp
CorrDataSub <- subset(CorrData, is.na(SumMVPA.0) == FALSE & is.na(SumMVPA.2) == FALSE & is.na(PercentMVPA) == FALSE)
dim(CorrDataSub)
# STILL only 6618 here

CorrDataSubVisit0 <- subset(CorrData, is.na(SumMVPA.0) == FALSE & is.na(PercentMVPA) == FALSE)
dim(CorrDataSubVisit0)
# 36,898 inds here

# Getting which visit is CLOSER to accelerometer wear
CorrDataSub$Visit0Diff <- abs(as.Date(CorrDataSub$AccelDate) - as.Date(CorrDataSub$Date.Attending.Assess.Center.Inst.0))
CorrDataSub$Visit2Diff <- abs(as.Date(CorrDataSub$AccelDate) - as.Date(CorrDataSub$Date.Attending.Assess.Center.Inst.2))

# Closer and farther visits selecting SumMVPA
CorrDataSub$CloserVisit <- ifelse(CorrDataSub$Visit0Diff < CorrDataSub$Visit2Diff, CorrDataSub$SumMVPA.0, CorrDataSub$SumMVPA.2)
CorrDataSub$FartherVisit <- ifelse(CorrDataSub$Visit0Diff < CorrDataSub$Visit2Diff, CorrDataSub$SumMVPA.2, CorrDataSub$SumMVPA.0)

  
# Restrict ONLY to PA variables
CorrDataSubPA <- CorrDataSub[ , c("CloserVisit", "FartherVisit", "MVPAVOL")]



M <-cor(CorrDataSubPA)

# Circles
corrplot(M, method = "color", type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

# Squares and fixed legend of corr
corrplot(M, method = "color", type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), addCoef.col = "dark grey", tl.col = "black", tl.cex = 1, cl.pos = "b")

