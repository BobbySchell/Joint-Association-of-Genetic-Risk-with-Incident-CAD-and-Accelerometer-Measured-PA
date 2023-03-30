# -------
# Converting days worn to hours worn
# Originally did more with this but before using PAEE
# 3/28/2023
# -------


dx download PACOHORT.csv

PACOHORT <- read.csv("PACOHORT.csv")

summary(PACOHORT$p90051)
# Wear time (IN DAYS)

# Converting to wear time in hours
PACOHORT$HoursWorn <- PACOHORT$p90051*24


# Creating OVERALL AVERAGE ACTIVITY QUARTERS
summary(PACOHORT$p90012)
# Overall average activity


write.csv(PACOHORT, "PACOHORTprocessedPAVars.csv")

dx upload PACOHORTprocessedPAVars.csv




dx download PACOHORTprocessedPAVars.csv
