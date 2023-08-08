# -------
# CURRENT ISSUE: Firstly, need some way a la file.choose() to get to relevant file
# This R script as currently written would likely try to process EVERY file in folder 10
# --------


# --------
# Use this file to interact w/ bash script
# Derives total and MVPA step counts
# --------

install.packages('GGIR')
library(GGIR)

install.packages('GGIRread')
library(GGIRread)

install.packages('data.table')
library(data.table)



source("verisense_count_steps.R") # update to local path to verisense_count_steps.R file
myfun =  list(FUN=verisense_count_steps,
              parameters= c(3, 5, 15, -0.5, 3, 4, 0.001, 1.2),
              expected_sample_rate= 15,
              expected_unit="g",
              colnames = c("step_count"),
              outputres = 1,
              minlength = 1,
              outputtype="numeric",
              aggfunction = sum,
              timestamp=F,
              reporttype="event")


# NOW do it w/ step counts
# CANT make outputdir sub directory...
GGIR(datadir="/Bulk/Activity/Raw/10/",
     outputdir= "/Bulk/Activity/Raw/",
     myfun=myfun,
     do.report = 2,
     mode = c(1,2),
     IVIS.activity.metric = 2,
     epochvalues2csv= TRUE
)



# General summary of steps
IndSteps <- mean(summary(summary$step_count_sum_0.24hr))
A <- IndSteps


# Rolling window for 12 5-second epochs
# >= 100 steps in a minute
IMP$metashort$VECT <- frollsum(IMP$metashort$step_count, n = 12)

IMP$metashort$MVPA <- ifelse(IMP$metashort$VECT >= 100, 1, 0)


VECTSUB <- subset(IMP$metashort, MVPA == 1)

# step counts at or above MVPA
B <- sum(VECTSUB$step_count)
# 6199

# Total step counts
C <- sum(IMP$metashort$step_count)
# = 40984.1669 vs only 32501 in original summary
# BUT have 6.99 days of wear time and original summary only shows for 6
# Makes sense that it's off by a day-ish
# Wear time on first day was insufficient to count on summary I believe


# Create matrix storing all of these values
MAT <- c(A,B,C)

# write csv w/ these values
write.csv(MAT, "MatrixFile.csv")


dx upload MatrixFile.csv
