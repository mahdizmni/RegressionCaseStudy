library("readxl")
library("GGally")
library("ggplot2")
library("MASS")
library("ggstatsplot")
library("car")
library("ggpubr")
library("olsrr")

data = read_excel('~/git/RegressionCaseStudy/data/BrownFat.xls')

data = subset(data, select = -c(1, 21, 23))

# Removing cancer status : redundant 
data = subset(data, select = -c(18))

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

fit0 <- lm(Y ~ factor(X1) + factor(X2) +X3 +X4 +factor(X5) +X6 +X7 +X8 +X9 +factor(X10) +factor(X11) +X12 +X13 +X14 +X15 +X16 +X17)

## Model selection 
fit1 <- lm(Y ~ factor(X1) + factor(X2) +X3 +X4 +factor(X5) +X6 +X7 +X8 +X9 +factor(X10) +factor(X11) +X12 +X13 +X14 +X15 +X16 +X17)
fit2 <- lm(Y ~ 1)

stepAIC(fit2, direction = "both", scope = list(upper = fit1, lower = fit2))

# Final model 
fit = lm(Y ~X3 + X9 + X12 + factor(X2) + factor(X1) + X16 + X13 + X14 + X15)

# Effective data
ed = data.frame(cbind(Sex = X1, Diabetes = X2, Age = X3, LBW = X9, Ext_Temp = X12, Weight = X16, BrownFat = Y))



## Multicolinearity
summary(fit)
ggpairs(subset(data, select = c(1, 2, 3, 17, 6, 7, 8, 10, 13, 19)))

# by looking at the correlation matrix, ext_temp, 2d_temp, 3_d temp and 1m_temp are highly
# correlated and we decide to only keep one, which is ext_temp, since is has the highest correlation with brown fat

fit = lm(Y+1 ~X3 + X9 + X12 + factor(X2) + factor(X1) + X16, na.action = na.exclude)

# VIF Test
VIFbar = mean(vif(fit))
# which is not significantly > 1


## Normality of errors
res = resid(fit)

#produce residual vs. fitted plot
plot(fitted(fit), res)

#add a horizontal line at 0 
abline(0,0)


#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

# box cox suggest to use lambda = -2 
result = boxcox(fit)
lambda = result$x[which.max(result$y)]
fit = lm(((Y+1) ^ lambda - 1)/lambda ~X3 + X9 + X12 + factor(X2) + factor(X1) + X16)

# still not good but it is what it is



# Removing outliers

## Outliers
# Graphical representations
ols_plot_cooksd_char(fit)
ols_plot_dfbetas(fit)
ols_plot_dffits(fit)
p1 -> ols_plot_cooksd_char(fit)
p2 -> ols_plot_dffits(fit)
ggarrange(p1, p2, ncol=2, nrow=1)

boxplot(ed)
# a lot in age and weight 


# Removing outliers from Age 
quartiles <- quantile(ed$Age, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(ed$Age)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

ed = subset(ed, ed$Age > Lower & ed$Age < Upper)
# Removing outliers from Weight 
quartiles <- quantile(ed$Weight , probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(ed$Weight)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

ed = subset(ed, ed$Weight > Lower & ed$Weight < Upper)
# After
boxplot(ed)
X1 = ed$Sex
X2 = ed$Diabetes 
X3 = ed$Age 
X9 = ed$LBW 
X12 = ed$Ext_Temp
X16 = ed$Weight
Y = ed$BrownFat
# fit the model without outliers
fit = lm(Y ~X3 + X9 + X12 + factor(X2) + factor(X1) + X16)

## Check for interaction terms
fit.inter = lm(Y ~ (X3 + X9 + X12 + factor(X2) + factor(X1) + X16)^2)
anova(fit.inter)
# interaction terms are not significant and seem to complicate the model, so we decide not to add them


## Checking for unequal variance
ed$resid = rstandard(fit)
ggplot(data=ed, aes(X1, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X2, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X3, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X5, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X9, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X12, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X16, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)


# if we have unequal variances : use WLS (lec 21)