library("readxl")
library("GGally")
library("ggplot2")
library("MASS")
library("ggstatsplot")

data = read_excel('~/git/RegressionCaseStudy/data/BrownFat.xls')

data = subset(data, select = -c(1, 21, 23))

s = as.numeric(data$Sex == 2) # Male
d = as.numeric(data$Diabetes == 1) #Yes
se1 = as.numeric(data$Season == 2) #Spring
se2 = as.numeric(data$Season == 3) #Fall
se3 = as.numeric(data$Season == 4) #Winter
# Summer is the base category
cs = as.numeric(data$Cancer_Status == 1) #Yes
ct1 = as.numeric(data$Cancer_Type == 1)
ct2 = as.numeric(data$Cancer_Type == 2)
ct3 = as.numeric(data$Cancer_Type == 3)
ct4 = as.numeric(data$Cancer_Type == 4)
ct5 = as.numeric(data$Cancer_Type == 5)
ct6 = as.numeric(data$Cancer_Type == 6)
ct7 = as.numeric(data$Cancer_Type == 7)
ct8 = as.numeric(data$Cancer_Type == 8)
ct9 = as.numeric(data$Cancer_Type == 9)
ct10 = as.numeric(data$Cancer_Type == 10)
ct11 = as.numeric(data$Cancer_Type == 11)
ct12 = as.numeric(data$Cancer_Type == 12)
ct13 = as.numeric(data$Cancer_Type == 13)
ct14 = as.numeric(data$Cancer_Type == 14)
ct15 = as.numeric(data$Cancer_Type == 15)
ct16 = as.numeric(data$Cancer_Type == 16)
# Other is the base category

b = as.numeric(data$BrownFat == 1)
# No brown fat is the base category

# Excluding some visible outliers
#data = subset(data, data$Age > 17)
#data = subset(data, data$Weigth > 25)
#data = subset(data, data$Weigth < 175)
#data = subset(data, data$Size > 125)

#ggpairs(subset(data, select = c(6, 7, 8, 9, 10, 20)))
# Removing highly correlated temparatures
#data = subset(data, select = -c(6, 7, 8, 10))
# Removin size and weight, check for colinearrity of size and weight and each
# in relation to the response variable

#ggpairs(subset(data, select = c(9, 10, 11, 13, 2, 12, 16)))
# Removing size and weight
#data = subset(data, select = -c(9, 10))

# Removing cancer status : redundant 
data = subset(data, select = -c(18))

# Reomveing day and month since it is not related for prediction
#data = subset(data, select = -c(4, 5))

#ggpairs(subset(data, select = c(4, 5, 6, 11)))

Y = data$BrownFat
X1 = data$Sex
X2 = data$Diabetes 
X3 = data$Age 
X4 = data$`7D_Temp`
X5 = data$Season 
X6 = data$Duration_Sunshine
X7 = data$BMI 
X8 = data$Glycemy 
X9 = data$LBW 
X10 = data$Cancer_Type 
X11 = data$Month
X12 = data$Ext_Temp
X13 = data$`2D_Temp`
X14 = data$`3D_Temp`
X15 = data$`1M_Temp`
X16 = data$Weigth
X17 = data$Size

fit0 <- lm(Y ~ X1 + X2 +X3 +X4 +X5 +X6 +X7 +X8 +X9 +X10 +X11 +X12 +X13 +X14 +X15 +X16 +X17) 

# Model selection 
fit1 = lm(Y ~ X1 + X2 +X3 +X4 +X5 +X6 +X7 +X8 +X9 +X10 +X11 +X12 +X13 +X14 +X15 +X16 +X17) 
stepAIC(fit1, direction = "backward")
fit2 <- lm(Y ~ 1)
stepAIC(fit2, direction = "forward", scope = list(upper = fit1, lower = fit2))

stepAIC(fit2, direction = "both", scope = list(upper = fit1, lower = fit2))

# Final model : Y ~ X3 + X9 + X12 + X2 + X1 + X16 + X5
fit = lm(Y ~ X3 + X9 + X12 + X2 + X1 + X16 + X5)

ed = data.frame(cbind(Sex = X1, Diabetes = X2, Age = X3, Season = X5, LBW = X9, Ext_Temp = X12, Weight = X16, BrownFat = Y))


dffits <- as.data.frame(dffits(fit))

p <- length(fit$coefficients)-1

n <- nrow(data)

thresh <- 2*sqrt(p/n)

#sort observations by DFFITS, descending
dffits[order(-dffits['dffits(fit)']), ]

#plot DFFITS values for each observation
plot(dffits(fit), type = 'h')

#add horizontal lines at absolute values for threshold
abline(h = thresh, lty = 2)
abline(h = -thresh, lty = 2)

boxplot(X3)$out
# or
ggbetweenstats(data = ed, Age, BrownFat, outlier.tagging = TRUE)

Q <- quantile(X3, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(X3)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
X3<- subset(X3, X3 > (Q[1] - 1.5*iqr) & X3 < (Q[2]+1.5*iqr))
boxplot(X3)$out

boxplot(X9)$out
# or
ggbetweenstats(data = ed, LBW, BrownFat, outlier.tagging = TRUE)

Q <- quantile(X9, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(X9)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
X9<- subset(X9, X9 > (Q[1] - 1.5*iqr) & X9 < (Q[2]+1.5*iqr))
boxplot(X9)$out


boxplot(X12)$out
# or
ggbetweenstats(data = ed, Ext_Temp, BrownFat, outlier.tagging = TRUE)

Q <- quantile(X12, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(X12)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
X12<- subset(X12, X12 > (Q[1] - 1.5*iqr) & X12 < (Q[2]+1.5*iqr))
boxplot(X12)$out



boxplot(X16)$out
# or
ggbetweenstats(data = ed, Weight, BrownFat, outlier.tagging = TRUE)

Q <- quantile(X16, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(X16)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
X16<- subset(X16, X16 > (Q[1] - 1.5*iqr) & X16 < (Q[2]+1.5*iqr))
boxplot(X16)$out

# fit the model without outliers
fit = lm(Y ~ X3 + X9 + X12 + X2 + X1 + X16 + X5)



# NO colinearrity since the significant t values and corr matrix
summary(fit)
ggpairs(subset(data, select = c(1, 2, 3, 11, 17, 6, 13)))



# Check for interaction terms
fit.inter =  lm(Y ~ (X3 + X9 + X12 + X2 + X1 + X16 + X5)^2)
anova(lm.fit2)






# check for normality of data
qqPlot(data)



# if not normal : box cox transf


# Checking for unequal variance
ed$resid = rstandard(fit)
ggplot(data=ed, aes(X1, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X2, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X3, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X5, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X9, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X12, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X16, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)


# if we have unequal variances : use WLS (lec 21)