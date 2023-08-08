# ------
# Supplement Figures
# ------

# -------
# First year excluded
# ENMO (raw and percentiles)
# -------


# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.94, 0.90, 0.87, 0.84, 0.80, 0.77, 0.72, 0.66)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.92, 0.87, 0.82, 0.78, 0.74, 0.69, 0.63, 0.56)
RefUL <- c(1.00, 0.97, 0.94, 0.92, 0.90, 0.88, 0.86, 0.83, 0.78)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.77, 0.71, 0.67, 0.63, 0.60, 0.56, 0.53, 0.48, 0.42)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.59, 0.56, 0.54, 0.52, 0.50, 0.48, 0.45, 0.41, 0.35)
FiftyUL <- c(1.00, 0.89, 0.82, 0.76, 0.71, 0.66, 0.61, 0.56, 0.50)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.59, 0.53, 0.49, 0.46, 0.42, 0.39, 0.36, 0.32, 0.27)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.35, 0.33, 0.32, 0.31, 0.30, 0.29, 0.27, 0.25, 0.22)
TenUL <- c(1.00, 0.85, 0.75, 0.67, 0.61, 0.54, 0.48, 0.41, 0.34)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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


# ------------
# MVPA Minutes controlling for ENMO
# Raw and percentiles
# ------------




# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.90, 0.82, 0.76, 0.70, 0.64, 0.57, 0.50, 0.41)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.86, 0.75, 0.67, 0.59, 0.52, 0.44, 0.37, 0.27)
RefUL <- c(1.00, 0.94, 0.89, 0.85, 0.81, 0.77, 0.72, 0.67, 0.60)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.70, 0.61, 0.54, 0.50, 0.45, 0.40, 0.35, 0.30, 0.23)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.59, 0.53, 0.47, 0.43, 0.37, 0.33, 0.27, 0.22, 0.15)
FiftyUL <- c(0.81, 0.71, 0.62, 0.58, 0.53, 0.49, 0.45, 0.41, 0.35)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.48, 0.42, 0.36, 0.32, 0.29, 0.25, 0.21, 0.18, 0.13)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.35, 0.32, 0.28, 0.26, 0.23, 0.20, 0.16, 0.12, 0.08)
TenUL <- c(0.66, 0.55, 0.46, 0.41, 0.36, 0.32, 0.28, 0.25, 0.21)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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


# ------
# MICE Imputation
# ENMO (raw and percentiles)
# -------


# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.93, 0.89, 0.85, 0.81, 0.77, 0.73, 0.68, 0.60)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.91, 0.85, 0.80, 0.75, 0.71, 0.66, 0.60, 0.52)
RefUL <- c(1.00, 0.95, 0.92, 0.89, 0.86, 0.83, 0.80, 0.76, 0.71)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.77, 0.70, 0.66, 0.61, 0.58, 0.54, 0.50, 0.45, 0.39)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.61, 0.57, 0.54, 0.52, 0.49, 0.46, 0.43, 0.39, 0.33)
FiftyUL <- c(0.99, 0.87, 0.79, 0.73, 0.68, 0.63, 0.58, 0.52, 0.46)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.60, 0.53, 0.48, 0.45, 0.41, 0.38, 0.34, 0.30, 0.25)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.37, 0.34, 0.33, 0.31, 0.30, 0.28, 0.26, 0.24, 0.20)
TenUL <- c(0.97, 0.82, 0.72, 0.64, 0.57, 0.51, 0.45, 0.38, 0.31)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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


# ------------
# MVPA Minutes controlling for ENMO
# Raw and percentiles
# ------------


# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.91, 0.82, 0.76, 0.70, 0.64, 0.57, 0.51, 0.41)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.87, 0.75, 0.68, 0.61, 0.54, 0.46, 0.38, 0.29)
RefUL <- c(1.00, 0.94, 0.89, 0.85, 0.81, 0.77, 0.72, 0.67, 0.60)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.69, 0.61, 0.54, 0.50, 0.45, 0.40, 0.35, 0.30, 0.24)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.59, 0.53, 0.47, 0.43, 0.38, 0.33, 0.28, 0.22, 0.16)
FiftyUL <- c(0.79, 0.69, 0.61, 0.57, 0.53, 0.49, 0.45, 0.41, 0.35)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.47, 0.41, 0.36, 0.32, 0.29, 0.25, 0.22, 0.18, 0.14)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.35, 0.32, 0.28, 0.26, 0.23, 0.20, 0.17, 0.13, 0.09)
TenUL <- c(0.63, 0.53, 0.45, 0.40, 0.35, 0.32, 0.28, 0.25, 0.21)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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




# --------
# Mediator Analysis 1
# ENMO
# --------

# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.96, 0.92, 0.90, 0.87, 0.84, 0.81, 0.78, 0.72)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.94, 0.89, 0.85, 0.81, 0.77, 0.73, 0.68, 0.61)
RefUL <- c(1.00, 0.98, 0.96, 0.95, 0.93, 0.92, 0.90, 0.88, 0.85)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.77, 0.72, 0.69, 0.66, 0.63, 0.60, 0.56, 0.53, 0.47)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.61, 0.58, 0.57, 0.55, 0.53, 0.51, 0.49, 0.45, 0.40)
FiftyUL <- c(0.99, 0.90, 0.83, 0.78, 0.74, 0.70, 0.65, 0.61, 0.56)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.60, 0.55, 0.51, 0.48, 0.45, 0.42, 0.39, 0.36, 0.31)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.37, 0.35, 0.34, 0.33, 0.32, 0.31, 0.30, 0.28, 0.25)
TenUL <- c(0.98, 0.84, 0.76, 0.69, 0.63, 0.57, 0.51, 0.45, 0.38)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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


# ------------
# MVPA Minutes controlling for ENMO
# Raw and percentiles
# ------------


# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.92, 0.84, 0.79, 0.73, 0.68, 0.62, 0.55, 0.46)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.88, 0.77, 0.71, 0.63, 0.57, 0.49, 0.41, 0.32)
RefUL <- c(1.00, 0.96, 0.91, 0.88, 0.85, 0.82, 0.78, 0.73, 0.67)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.69, 0.62, 0.56, 0.52, 0.47, 0.43, 0.38, 0.34, 0.27)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.60, 0.55, 0.49, 0.45, 0.40, 0.36, 0.30, 0.25, 0.18)
FiftyUL <- c(0.80, 0.71, 0.64, 0.60, 0.56, 0.53, 0.49, 0.45, 0.40)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.48, 0.42, 0.37, 0.34, 0.31, 0.28, 0.24, 0.20, 0.16)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.36, 0.33, 0.30, 0.27, 0.25, 0.22, 0.18, 0.15, 0.10)
TenUL <- c(0.64, 0.54, 0.47, 0.42, 0.38, 0.35, 0.31, 0.28, 0.25)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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



# ----------
# Second Mediation Results
# ENMO
# --------

# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.95, 0.92, 0.89, 0.86, 0.83, 0.80, 0.76, 0.71)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.93, 0.88, 0.84, 0.80, 0.77, 0.72, 0.67, 0.60)
RefUL <- c(1.00, 0.97, 0.96, 0.94, 0.92, 0.91, 0.89, 0.87, 0.83)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.80, 0.74, 0.70, 0.67, 0.63, 0.60, 0.56, 0.52, 0.46)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.63, 0.60, 0.58, 0.56, 0.54, 0.52, 0.49, 0.45, 0.39)
FiftyUL <- c(1.01, 0.91, 0.85, 0.79, 0.75, 0.70, 0.65, 0.61, 0.55)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.64, 0.57, 0.53, 0.50, 0.46, 0.43, 0.40, 0.36, 0.30)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.39, 0.37, 0.36, 0.35, 0.34, 0.32, 0.31, 0.28, 0.25)
TenUL <- c(1.03, 0.88, 0.79, 0.71, 0.64, 0.58, 0.52, 0.45, 0.38)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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


# ------------
# MVPA Minutes controlling for ENMO
# Raw and percentiles
# ------------


# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.92, 0.84, 0.79, 0.73, 0.68, 0.62, 0.55, 0.47)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.88, 0.77, 0.71, 0.63, 0.57, 0.49, 0.42, 0.32)
RefUL <- c(1.00, 0.96, 0.91, 0.88, 0.85, 0.82, 0.78, 0.73, 0.67)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.70, 0.63, 0.57, 0.52, 0.48, 0.44, 0.39, 0.34, 0.27)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.61, 0.55, 0.50, 0.45, 0.41, 0.36, 0.30, 0.25, 0.18)
FiftyUL <- c(0.81, 0.71, 0.64, 0.60, 0.56, 0.53, 0.49, 0.45, 0.40)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.49, 0.43, 0.38, 0.35, 0.31, 0.28, 0.24, 0.20, 0.16)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.37, 0.34, 0.30, 0.28, 0.25, 0.22, 0.18, 0.15, 0.10)
TenUL <- c(0.65, 0.55, 0.48, 0.43, 0.39, 0.35, 0.32, 0.28, 0.24)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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


# --------
# Sex stratified analyses
# ENMO
# --------


# ------
# Male
# ------


# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.94, 0.89, 0.85, 0.82, 0.78, 0.74, 0.69, 0.62)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.91, 0.85, 0.80, 0.76, 0.71, 0.66, 0.60, 0.52)
RefUL <- c(1.00, 0.96, 0.93, 0.91, 0.88, 0.86, 0.83, 0.80, 0.75)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.69, 0.64, 0.60, 0.57, 0.54, 0.51, 0.48, 0.44, 0.39)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.52, 0.50, 0.48, 0.46, 0.45, 0.43, 0.40, 0.37, 0.32)
FiftyUL <- c(0.92, 0.82, 0.75, 0.70, 0.65, 0.61, 0.57, 0.52, 0.47)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.48, 0.44, 0.41, 0.38, 0.36, 0.33, 0.31, 0.28, 0.24)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.27, 0.26, 0.26, 0.25, 0.24, 0.24, 0.23, 0.21, 0.19)
TenUL <- c(0.85, 0.72, 0.64, 0.58, 0.52, 0.47, 0.42, 0.37, 0.31)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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


# ------------
# MVPA Minutes controlling for ENMO
# Raw and percentiles
# ------------


# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.90, 0.82, 0.76, 0.70, 0.64, 0.57, 0.50, 0.41)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.86, 0.74, 0.67, 0.59, 0.52, 0.44, 0.36, 0.27)
RefUL <- c(1.00, 0.95, 0.90, 0.86, 0.82, 0.79, 0.74, 0.69, 0.62)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.62, 0.56, 0.50, 0.47, 0.42, 0.39, 0.34, 0.30, 0.24)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.52, 0.48, 0.43, 0.39, 0.35, 0.31, 0.26, 0.21, 0.15)
FiftyUL <- c(0.74, 0.65, 0.59, 0.55, 0.51, 0.48, 0.45, 0.42, 0.37)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.39, 0.35, 0.31, 0.28, 0.26, 0.23, 0.21, 0.18, 0.14)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.27, 0.26, 0.24, 0.22, 0.20, 0.18, 0.15, 0.12, 0.09)
TenUL <- c(0.55, 0.47, 0.40, 0.37, 0.33, 0.31, 0.28, 0.26, 0.23)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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





# ------
# Female
# ------


# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.93, 0.88, 0.84, 0.80, 0.76, 0.71, 0.66, 0.59)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.89, 0.81, 0.75, 0.70, 0.64, 0.58, 0.51, 0.43)
RefUL <- c(1.00, 0.97, 0.95, 0.93, 0.92, 0.90, 0.88, 0.85, 0.82)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.98, 0.87, 0.79, 0.73, 0.68, 0.62, 0.56, 0.50, 0.41)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.60, 0.56, 0.54, 0.51, 0.48, 0.46, 0.42, 0.37, 0.29)
FiftyUL <- c(1.61, 1.34, 1.18, 1.05, 0.94, 0.85, 0.75, 0.67, 0.57)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.96, 0.81, 0.72, 0.64, 0.57, 0.51, 0.44, 0.37, 0.28)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.36, 0.34, 0.32, 0.31, 0.29, 0.28, 0.26, 0.23, 0.19)
TenUL <- c(2.60, 1.97, 1.60, 1.33, 1.12, 0.93, 0.76, 0.59, 0.44)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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


# ------------
# MVPA Minutes controlling for ENMO
# Raw and percentiles
# ------------


# Selecting coefficients from 10th and 90th %tile model
RefCoef <- c(1.00, 0.91, 0.82, 0.77, 0.71, 0.65, 0.58, 0.52, 0.43)

# Creating lower and upper bound confidence interval objects from 20th %tile model
RefLL <- c(1.00, 0.83, 0.69, 0.61, 0.52, 0.44, 0.36, 0.28, 0.19)
RefUL <- c(1.00, 0.99, 0.99, 0.98, 0.97, 0.97, 0.96, 0.95, 0.94)

RefvPA <- as.data.frame(cbind(RefLL, RefCoef, RefUL))

colnames(RefvPA) <- c("LL","Mean","UL")

RefvPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
RefvPA$Genetic.Risk <- "90th Percentile Genetic Risk"



# Selecting coefficients from 50th %tile model
FiftyCoef <- c(0.83, 0.72, 0.62, 0.56, 0.50, 0.44, 0.37, 0.31, 0.23)

# Creating lower and upper bound confidence interval objects from 40th %tile model
FiftyLL <- c(0.63, 0.56, 0.48, 0.42, 0.35, 0.29, 0.22, 0.16, 0.10)
FiftyUL <- c(1.10, 0.92, 0.81, 0.75, 0.70, 0.66, 0.62, 0.58, 0.53)

FiftyPA <- as.data.frame(cbind(FiftyLL, FiftyCoef, FiftyUL))

colnames(FiftyPA) <- c("LL","Mean","UL")

FiftyPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
FiftyPA$Genetic.Risk <- "50th Percentile Genetic Risk"

# Selecting coefficients from 10th %tile model
TenCoef <- c(0.70, 0.57, 0.47, 0.41, 0.35, 0.29, 0.24, 0.18, 0.12)

# Creating lower and upper bound confidence interval objects from 10th %tile model
TenLL <- c(0.40, 0.35, 0.30, 0.27, 0.22, 0.18, 0.13, 0.09, 0.05)
TenUL <- c(1.22, 0.93, 0.73, 0.63, 0.54, 0.48, 0.42, 0.37, 0.31)

TenPA <- as.data.frame(cbind(TenLL, TenCoef, TenUL))

colnames(TenPA) <- c("LL","Mean","UL")

TenPA$Model <- c("10th PA Risk", "20th PA Risk", "30th PA Risk", "40th PA Risk", "50th PA Risk", "60th PA Risk", "70th PA Risk", "80th PA Risk", "90th PA Risk")
TenPA$Genetic.Risk <- "10th Percentile Genetic Risk"


# Merging PlotDF and TenPA
#PlotDF <- PlotDF[c(1:5)]
PlotDF <- rbind(RefvPA, FiftyPA, TenPA)


# Putting CIs w/ Mean in new variable FOR LABELING in PlotDF
PlotDF$LABEL <- paste(as.character(PlotDF$Mean), " (", as.character(PlotDF$LL), "-", as.character(PlotDF$UL), ")", sep = "")


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
