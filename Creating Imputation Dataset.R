# --------
# Creating IMPUTATION dataset
# 3/17/2023
# --------

dx download FINALDempseyDataset.csv

Data <- read.csv("FINALDempseyDataset.csv")

install.packages("mice")
library(mice)

# Restrict to ONLY variables that will be used in regression
# AgeBaseline, TimeYear, Status, PAEEPOS, StandPGS, p22009_a1, p22009_a2
# p22009_a3, p22009_a4, p22009_a5, p22009_a6, p22009_a7, p22009_a8, p22009_a9,
# p22009_a10, SeasonWear, Salt_InstChosen, NewAlc,
# NewOilFish, FnVScore, ProcMeat_InstChosen, ParentHist,
# MobilityDichot, NewEmploy, Townsend, NewEduc
# SmokStat_InstChosen, Biological.Sex, eid

# NOT including manual labor here because only answered by about half
# Will use in sensitivity analyses

# Subsetting data accordingly
# KEEPING eid to stay flexible
DataSub <- Data[ , c("AgeBaseline", "TimeYear", "Status", "PAEEPOS", "StandPGS", "p22009_a1", "p22009_a2"
                 ,"p22009_a3", "p22009_a4", "p22009_a5", "p22009_a6", "p22009_a7", "p22009_a8", "p22009_a9",
                 "p22009_a10", "SeasonWear", "Salt_InstChosen", "NewAlc",
                 "NewOilFish", "FnVScore", "ProcMeat_InstChosen", "ParentHist",
                 "MobilityDichot", "NewEmploy", "Townsend", "NewEduc",
                 "SmokStat_InstChosen", "Biological.Sex")]
# Got rid of eid... Easier...


#imputed_Data <- mice(DataSub, m=5, method = 'fastpmm', seed = 500, predictorMatrix = as.matrix(DataSub[ , -29]))
imputed_Data <- mice(DataSub, m=5, method = 'pmm', seed = 500)


# Confirming no missingness now
summary(is.na(imputed_Data))

imputed_Data <- complete(imputed_Data)

# CONFIRMING that this works
summary(is.na(imputed_Data))
# Weirdly 199 smoking status coded as NA

write.csv(imputed_Data, "ImputedFINALDempseyDataset.csv")

dx upload ImputedFINALDempseyDataset.csv
