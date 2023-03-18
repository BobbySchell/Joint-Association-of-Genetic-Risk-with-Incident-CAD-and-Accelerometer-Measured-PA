# ---------
# MERGING FINAL COVARIATES WITH PA DATASET
# AND GETTING NAMES RIGHT FOR ADDED COVARIATES
# 3/17/2023
# ---------



dx download FINALCovarsforCAD.csv
dx download FINALANALYSISDATAPAPER3.csv


# Couldn't do DempseyDataset because EID is not attached
# INSTEAD using dataset from a step back...







PADATA <- read.csv("FINALANALYSISDATAPAPER3.csv")
COVARS <- read.csv("FINALCovarsforCAD.csv")


# FIRST getting covariate names right - if order remains the same as in Spark JL




field_names <- c("X", "eid", "p22032_i0", "p22039_i0", "p22038_i0", "p22040_i0", "p816_i0", "p816_i1", "p816_i2", "p816_i3", "p806_i0", "p806_i1", 
                 "p806_i2", "p806_i3", "p21000_i0", "p21000_i1", "p21000_i2")



# Making sure order is same between original field_names and COVARS dataset BEFORE renaming
MATCH <- match(field_names, colnames(COVARS))


# Provided this works then:
covarvector <- c("X", "eid", "IPAQGroupInst1", "METVigorous", "METMVPA", "METTotal", "ManLabor_Inst0", "ManLabor_Inst1", "ManLabor_Inst2", "ManLabor_Inst3","WalkorStandWork_Inst0", "WalkorStandwork_Inst1", 
                 "WalkorStandWork_Inst2", "WalkorStandWork_Inst3", "Ethnicity_Inst0", "Ethnicity_Inst1", "Ethnicity_Inst2")


colnames(COVARS) <- covarvector

# Dropping unnecessary x variable
COVARS <- COVARS[ , -1]


colnames(PADATA)
dim(PADATA)
# 77474 x 67

# ALSO NOTE there are some unprocessed redundancies in PADATA (un-renamed covars that overlap w accelerometer)
# UsedinPCA merging to get ONLY those that make kinship criteria
FINALDATAMERGE <- merge(PADATA, COVARS, by = "eid", all = F)

dim(FINALDATAMERGE)
# 77474 x 957

# --------
# GET RIGHT INSTANCE AND VARIABLES TO USE IN REGRESSION AND FORMAT
# JUST INSTANCE 0 FOR ALL OF THESE
# --------

# ----
# Occupation
# ONLY instance 0 has significant NON missing for these two variables
# AND EVEN THEN... Only about half answer
# ----



# Ethnicity - White already shown thru Genetic.Ethnic.Grouping
summary(as.factor(FINALDATAMERGE$Ethnicity_Inst0))
summary(as.factor(FINALDATAMERGE$Ethnicity_Inst1))
summary(as.factor(FINALDATAMERGE$Ethnicity_Inst2))
# Same point here - ONLY 0 HAS SIG NON-MISSINGNESS
# Only 31 not classified


# Creating Black
FINALDATAMERGE$Black <- ifelse(grepl("African", FINALDATAMERGE$Ethnicity_Inst0) | grepl("Any other Black background", FINALDATAMERGE$Ethnicity_Inst0) | grepl("Black or Black British", FINALDATAMERGE$Ethnicity_Inst0) | grepl("White and Black African", FINALDATAMERGE$Ethnicity_Inst0) | grepl("White and Black Caribbean", FINALDATAMERGE$Ethnicity_Inst0) == TRUE, 1, 0)

summary(as.factor(FINALDATAMERGE$Black))
# Only 421 here defined liberally (grows to about twice that if we include generic "Caribbean")


# Creating Asian
FINALDATAMERGE$Asian <- ifelse(grepl("Any other Asian background", FINALDATAMERGE$Ethnicity_Inst0) | grepl("Asian or Asian British", FINALDATAMERGE$Ethnicity_Inst0) | grepl("Bangladeshi", FINALDATAMERGE$Ethnicity_Inst0) | grepl("Chinese", FINALDATAMERGE$Ethnicity_Inst0) | grepl("Indian", FINALDATAMERGE$Ethnicity_Inst0) | grepl("Pakistani", FINALDATAMERGE$Ethnicity_Inst0) | grepl("White and Asian", FINALDATAMERGE$Ethnicity_Inst0) == TRUE, 1, 0)

summary(as.factor(FINALDATAMERGE$Asian))
# Only 1090 Asian explicitly here defined liberally
# Just won't have enough juice to do much with these

write.csv(FINALDATAMERGE, "FINALDempseyData.csv")


dx upload FINALDempseyData.csv


