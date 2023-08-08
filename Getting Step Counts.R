# ------------
# "Fun" with step counts
# ------------


# FIRST adding p90001 as a variable to dataset to get raw accelerometer data
# Using Spark Cluster...
# Using "Adding Further Covariates to Overall Dataset"

# "The values held within this field are coded links to files held within a bulk repository, not the actual data itself."

# STILL should get vars so I can try to comprehend bulk activity folder...

dx download BULKPATEST.csv


bulk <- read.csv("BULKPATEST.csv")


bulknona <- subset(bulk, p90001_i0 != "")
# Keep only accelerometer inds

bulknona <- bulknona[c(2:3)]


# Add a column of first two nums of EID to add to file path



write.csv(bulknona, "BULKREADYPA.csv")



#All Projects > PROJECT > Bulk > Activity > Raw > first two nums in eid
# THEN can use IDs from csv


# ACTUALLY let's just try the workflow w/ 1 person first

dx download /Bulk/Activity/Raw/10/1000011_90001_0_0.cwa
dx download myscript.R
dx download verisense_count_steps.R


install.packages('GGIR')

library(GGIR)

install.packages('GGIRread')
library(GGIRread)



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


wd <- getwd()
getwd()

# Had to create INPUT folder in this env manually

GGIR(datadir="/opt/notebooks/INPUT/",
     outputdir= wd)
# Ran in a couple minutes


# NOW do it w/ step counts
GGIR(datadir="/opt/notebooks/INPUT/",
     outputdir= wd,
     myfun=myfun)
# Took 5 mins... Already too long w/o parallel if scales linearly

# Gives summary including step counts per day
summary <- read.csv('output_INPUT/results/part2_daysummary.csv')

summary$step_count_sum_0.24hr

IndSteps <- mean(summary(summary$step_count_sum_0.24hr))
IndSteps

# So person 100011 averaged 4614 steps/day over the week

# THEN use a JL w/ only 1 eid selected who have accel data and run this...


# Try out GGIR and Verisense together for 1 person sample timed


# -----
# NOW for the scaleup
# FIRST limit bulknona to ONLY eids used in our study
# THEN create column that shows which folders to use
# THEN try to do a folder, see how long and how many ppl
# 51 total folders (ignoring that some may be irrel.)
# -------


# Two major questions:
# Does this scale? Does it give a PA intensity metric?




# NOW do it w/ step counts
GGIR(datadir="/opt/notebooks/INPUT/",
     outputdir= wd,
     myfun=myfun,
     do.report = 2,
     mode = c(1,2),
     IVIS.activity.metric = 2,
)
# Took 5 mins... Already too long w/o parallel if scales linearly




GGIR(datadir="/opt/notebooks/INPUT/",
     outputdir= wd,
     myfun=myfun,
     do.report = 2,
     mode = c(1,2),
     IVIS.activity.metric = 2,
     epochvalues2csv= TRUE
)


# TRYING to get epoch level
ls()

# C, I, IMP, M, SUM
# THIS is how to do it
# NOT I - inspect
# NOT C - calibration error


unique(IMP$metashort$step_count)
# 120780 x 4 - 120,780 5-seccond epochs

# MVPA defined as >= 100 steps in a minute

install.packages('data.table')
library(data.table)

# Rolling window for 12 5-second epochs
# >= 100 steps in a minute
IMP$metashort$VECT <- frollsum(IMP$metashort$step_count, n = 12)


summary(IMP$metashort$VECT)


IMP$metashort$MVPA <- ifelse(IMP$metashort$VECT >= 100, 1, 0)


VECTSUB <- subset(IMP$metashort, MVPA == 1)


sum(VECTSUB$step_count)
# 6199

sum(IMP$metashort$step_count)
# = 40984.1669 vs only 32501 in original summary
# BUT have 6.99 days of wear time and original summary only shows for 6
# Makes sense that it's off by a day-ish
# Wear time on first day was insufficient to count on summary I believe


6199/40984.1669
# So roughly 15% of steps taken as MVPA

#  If we wanted to make it daily then
#886 MVPA steps / 5854 steps



# ---------
# NOW need to see if we can do this data extraction quickly
# Do BY FOLDER and time it out
# ---------


# FIRST creating well-suited CSVs

dx download BULKREADYPA.csv
dx download FinalDatasetwNewExposuresFINAL.csv


bulk <- read.csv("BULKREADYPA.csv")
data <- read.csv("FinalDatasetwNewExposuresFINAL.csv")


bulknona <- subset(bulk, p90001_i0 != "")
# Keep only accelerometer inds

# NOW restrict to only eids that match those in our actual analysis
bulknonasub <- subset(bulknona, eid %in% data$eid)
# now 66,180 inds



# Add a column of first two nums of EID to add to file path
bulknonasub$firstnums <- substr(bulknonasub$eid, start = 1, stop = 2)

# How many folders to do?
unique(bulknonasub$firstnums)
# so 50 folders

# NOW create a column to make transfer easy
bulknonasub$char <- paste("dx download /Bulk/Activity/Raw/", bulknonasub$firstnums, "/", bulknonasub$p90001_i0, sep = "")

# TEST this w/ a single folder
bulknonasub10 <- subset(bulknonasub, firstnums == 10)
# 1326 inds here

write.csv(bulknonasub, "TheFinalBulk.csv")
write.csv(bulknonasub10, "TheFinalBulkFolder10.csv")

dx upload TheFinalBulk.csv
dx upload TheFinalBulkFolder10.csv



# ------------
# NOW use the char variable in TheFinalBulkFolder10.csv in workflow
# ------------



#dx download /Bulk/Activity/Raw/10/1000011_90001_0_0.cwa
dx download myscript.R
dx download verisense_count_steps.R
# Took TWO HOURS to read everything in...
# May need to bash script it all
# Have to put all in a folder... Impractical
# Yup... Gonna have to bash it
install.packages('GGIR')

library(GGIR)

install.packages('GGIRread')
library(GGIRread)



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


wd <- getwd()
getwd()

# Had to create INPUT folder in this env manually

#GGIR(datadir="/opt/notebooks/INPUT/",
#     outputdir= wd)
# Ran in a couple minutes


# NOW do it w/ step counts
#GGIR(datadir="/opt/notebooks/INPUT/",
#     outputdir= wd,
#     myfun=myfun)
# Took 5 mins... Already too long w/o parallel if scales linearly

# Gives summary including step counts per day
#summary <- read.csv('output_INPUT/results/part2_daysummary.csv')

#summary$step_count_sum_0.24hr

#IndSteps <- mean(summary(summary$step_count_sum_0.24hr))
#IndSteps

# So person 100011 averaged 4614 steps/day over the week

# THEN use a JL w/ only 1 eid selected who have accel data and run this...


# Try out GGIR and Verisense together for 1 person sample timed



# NOW do it w/ step counts
# CANT make outputdir sub directory...
GGIR(datadir="/opt/notebooks/INPUT/",
     outputdir= wd,
     myfun=myfun,
     do.report = 2,
     mode = c(1,2),
     IVIS.activity.metric = 2,
     epochvalues2csv= TRUE
)


# TRYING to get epoch level
ls()

# C, I, IMP, M, SUM
# THIS is how to do it
# NOT I - inspect
# NOT C - calibration error


# NOW need to cycle through all of these inds and get their step counts
install.packages('data.table')
library(data.table)

# HOW are they labeled when they come out? Can then make a for loop w/ empty matrix to store


# Rolling window for 12 5-second epochs
# >= 100 steps in a minute
IMP$metashort$VECT <- frollsum(IMP$metashort$step_count, n = 12)


summary(IMP$metashort$VECT)


IMP$metashort$MVPA <- ifelse(IMP$metashort$VECT >= 100, 1, 0)


VECTSUB <- subset(IMP$metashort, MVPA == 1)


sum(VECTSUB$step_count)
# 6199

sum(IMP$metashort$step_count)
# = 40984.1669 vs only 32501 in original summary
# BUT have 6.99 days of wear time and original summary only shows for 6
# Makes sense that it's off by a day-ish
# Wear time on first day was insufficient to count on summary I believe


6199/40984.1669
# So roughly 15% of steps taken as MVPA

#  If we wanted to make it daily then
#886 MVPA steps / 5854 steps


