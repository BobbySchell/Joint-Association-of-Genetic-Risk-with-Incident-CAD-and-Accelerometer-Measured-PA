# ------------
# Updated Figures for Manuscript
# 7/14/23
# ------------

# -------
# ENMO (raw and percentiles)
# -------



# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.93, 0.89, 0.85, 0.81, 0.77, 0.73, 0.68, 0.61)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.92, 0.86, 0.81, 0.76, 0.71, 0.66, 0.60, 0.52)
RefUL <- c(1.00, 0.96, 0.92, 0.90, 0.87, 0.84, 0.81, 0.77, 0.72)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.75, 0.69, 0.64, 0.61, 0.57, 0.54, 0.50, 0.45, 0.39)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.59, 0.56, 0.53, 0.51, 0.48, 0.46, 0.43, 0.39, 0.33)
FiftyUL <- c(0.96, 0.86, 0.78, 0.72, 0.67, 0.62, 0.58, 0.52, 0.46)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.57, 0.51, 0.47, 0.43, 0.40, 0.37, 0.34, 0.30, 0.25)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.35, 0.33, 0.31, 0.30, 0.29, 0.27, 0.26, 0.24, 0.20)
TenUL <- c(0.93, 0.79, 0.69, 0.62, 0.56, 0.50, 0.44, 0.38, 0.31)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"

install.packages('ggforce')
library(ggforce)



# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2, y = Model, hjust = 0, label = LABEL)) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 2.35, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3), ylim = c(0,5), expand = TRUE)

# Try using pre-made program to make comprehensible
library(grid)
library(forestploter)



# -----
# Full range of PA w/ different lines as diff genetic risks Example - PAEE
# Far easier to code well than foret plots here
# -----


# 50th genetic risk vs 90th vs 10th
PlotDF$Percentiles <- rep(c(10,20,30,40,50,60,70,80,90), times = 3)

# ALSO make option to represent percentiles in raw units


p <- ggplot(data=PlotDF, aes(x=Percentiles, y=Mean, colour=Genetic.Risk)) + geom_point() + geom_line()


p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), linetype=1, alpha=0.1) + theme_bw() + xlab("PA Volume Percentile (ENMO)") + ylab("Hazard Ratio")

p + scale_x_continuous(breaks=seq(10,90,10)) + ggtitle("Association of CAD and PA Volume by Genetic Risk") + guides(color=guide_legend(title="Genetic Risk Percentile")) + scale_y_continuous(breaks=seq(0,1.0,0.20))


# Percentile vs raw and get tick marks right
# Get as RAW this time
PlotDF$Raw <- rep(c(18.75,21.43,23.46,25.27,27.07,28.97,31.18,33.97,38.29), times = 3)

p <- ggplot(data=PlotDF, aes(x=Raw, y=Mean, colour=Genetic.Risk)) + geom_point() + geom_line()


p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), linetype=1, alpha=0.1) + theme_bw() + xlab("PA Volume (in mgs)") + ylab("Hazard Ratio")

p + scale_x_continuous(breaks = unique(PlotDF$Raw)) + ggtitle("Association of CAD and PA Volume by Genetic Risk") + theme(axis.text.x=element_text(angle=45, vjust = 0.75))



# CAN ALSO SIMPLY FIT AS REGRESSION LINE
# So each point is just a one unit increase




# ------------
# MVPA Minutes controlling for ENMO
# Raw and percentiles
# ------------




# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.91, 0.82, 0.76, 0.70, 0.64, 0.57, 0.51, 0.41)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.87, 0.75, 0.68, 0.60, 0.54, 0.46, 0.38, 0.29)
RefUL <- c(1.00, 0.94, 0.89, 0.85, 0.81, 0.77, 0.72, 0.67, 0.60)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.68, 0.60, 0.53, 0.49, 0.44, 0.40, 0.35, 0.30, 0.24)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.58, 0.53, 0.47, 0.43, 0.38, 0.33, 0.28, 0.22, 0.16)
FiftyUL <- c(0.78, 0.69, 0.61, 0.57, 0.52, 0.49, 0.45, 0.41, 0.35)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.46, 0.40, 0.35, 0.32, 0.28, 0.25, 0.21, 0.18, 0.14)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.34, 0.31, 0.28, 0.25, 0.23, 0.20, 0.16, 0.13, 0.09)
TenUL <- c(0.62, 0.52, 0.44, 0.39, 0.35, 0.32, 0.28, 0.25, 0.21)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"

install.packages('ggforce')
library(ggforce)



# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2, y = Model, hjust = 0, label = LABEL)) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 2.35, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3), ylim = c(0,5), expand = TRUE)

# Try using pre-made program to make comprehensible
library(grid)
library(forestploter)



# -----
# Full range of PA w/ different lines as diff genetic risks Example - PAEE
# Far easier to code well than foret plots here
# -----


# 50th genetic risk vs 90th vs 10th
PlotDF$Percentiles <- rep(c(10,20,30,40,50,60,70,80,90), times = 3)

# ALSO make option to represent percentiles in raw units


p <- ggplot(data=PlotDF, aes(x=Percentiles, y=Mean, colour=Genetic.Risk)) + geom_point() + geom_line()


p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), linetype=1, alpha=0.1) + theme_bw() + xlab("PA Intensity Percentile (Mins/Day)") + ylab("Hazard Ratio")

p + scale_x_continuous(breaks=seq(10,90,10)) + ggtitle("Association of CAD and PA Intensity by Genetic Risk") + guides(color=guide_legend(title="Genetic Risk Percentile")) + scale_y_continuous(breaks=seq(0,1.0,0.20))


# Percentile vs raw and get tick marks right
# Get as RAW this time
PlotDF$Raw <- rep(c(31.68,41.76,51.84,59.04,67.68,76.32,87.84,100.80,120.96), times = 3)

p <- ggplot(data=PlotDF, aes(x=Raw, y=Mean, colour=Genetic.Risk)) + geom_point() + geom_line()


p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), linetype=1, alpha=0.1) + theme_bw() + xlab("PA Intensity (MVPA mins/day)") + ylab("Hazard Ratio")

p + scale_x_continuous(breaks = unique(PlotDF$Raw)) + ggtitle("Association of CAD and PA Intensity by Genetic Risk") + theme(axis.text.x=element_text(angle=45, vjust = 0.75))



# CAN ALSO SIMPLY FIT AS REGRESSION LINE
# So each point is just a one unit increase







# ------------
# Percent MVPA controlling for PAEE
# Raw and percentiles
# ------------




# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.90, 0.84, 0.79, 0.74, 0.69, 0.65, 0.60, 0.54)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.87, 0.79, 0.72, 0.67, 0.61, 0.56, 0.51, 0.44)
RefUL <- c(1.00, 0.94, 0.89, 0.85, 0.82, 0.79, 0.75, 0.71, 0.67)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.70, 0.62, 0.56, 0.52, 0.48, 0.45, 0.41, 0.38, 0.33)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.57, 0.52, 0.48, 0.45, 0.42, 0.39, 0.35, 0.31, 0.27)
FiftyUL <- c(0.86, 0.74, 0.66, 0.60, 0.56, 0.52, 0.49, 0.45, 0.41)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.48, 0.42, 0.38, 0.35, 0.32, 0.29, 0.26, 0.24, 0.20)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.32, 0.30, 0.28, 0.26, 0.25, 0.23, 0.21, 0.19, 0.16)
TenUL <- c(0.73, 0.60, 0.52, 0.46, 0.41, 0.37, 0.33, 0.30, 0.26)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")

# FIXING figure - get reference at bottom and correctly annotated
PlotDF$LABEL[1] <- "1 (Reference Group)"

install.packages('ggforce')
library(ggforce)



# MODEL WORKS
ggplot(PlotDF, aes(x = as.numeric(Mean), y = Model)) +
  geom_linerange(aes(xmin = as.numeric(LL),
                     xmax = as.numeric(UL))) + geom_point(shape = 15, size  = 3) + theme_classic() + theme(axis.title  = element_text(face  = "bold")) + ggtitle("Comparison of Combined Genetic and PA Risk") + labs(caption = "Error Bars represent 95% Confidence Intervals", y = NULL, axis.text.y = element_text(hjust = 0, size = 18)) + xlab("Adjusted Hazard Ratio") + geom_text(aes(x = 2, y = Model, hjust = 0, label = LABEL)) +  geom_vline(xintercept = 1, linetype="dashed") + annotate("text", x = 2.35, y = 5, label = "Hazard Ratio (95% CI)", fontface = "bold") + ggforce::facet_col(facets = ~Genetic.Risk, scales = "free_y",space = "free", strip.position = "bottom") + coord_cartesian(xlim = c(0,3), ylim = c(0,5), expand = TRUE)

# Try using pre-made program to make comprehensible
library(grid)
library(forestploter)



# -----
# Full range of PA w/ different lines as diff genetic risks Example - PAEE
# Far easier to code well than foret plots here
# -----


# 50th genetic risk vs 90th vs 10th
PlotDF$Percentiles <- rep(c(10,20,30,40,50,60,70,80,90), times = 3)

# ALSO make option to represent percentiles in raw units


p <- ggplot(data=PlotDF, aes(x=Percentiles, y=Mean, colour=Genetic.Risk)) + geom_point() + geom_line()


p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), linetype=1, alpha=0.1) + theme_bw() + xlab("PA Intensity Percentile (% PAEE)") + ylab("Hazard Ratio")

p + scale_x_continuous(breaks=seq(10,90,10)) + ggtitle("Association of CAD and PA Intensity by Genetic Risk") + guides(color=guide_legend(title="Genetic Risk Percentile")) + scale_y_continuous(breaks=seq(0,1.0,0.20))


# Percentile vs raw and get tick marks right
# Get as RAW this time
PlotDF$Raw <- rep(c(21.16,25.93,29.51,32.62,35.56,38.51,41.70,45.43,50.58), times = 3)

p <- ggplot(data=PlotDF, aes(x=Raw, y=Mean, colour=Genetic.Risk)) + geom_point() + geom_line()


p <- p + geom_ribbon(aes(ymin=LL, ymax=UL), linetype=1, alpha=0.1) + theme_bw() + xlab("PA Intensity (PAEE)") + ylab("Hazard Ratio")

p + scale_x_continuous(breaks = unique(PlotDF$Raw)) + ggtitle("Association of CAD and PA Intensity by Genetic Risk") + theme(axis.text.x=element_text(angle=45, vjust = 0.75))



# CAN ALSO SIMPLY FIT AS REGRESSION LINE
# So each point is just a one unit increase


# ------
# Creating Correlation matrix between PA exposures - volume and intensity
# ------

# First create correlation matrix object

