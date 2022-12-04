library("readxl")
library("GGally")
library("ggplot2")
library("MASS")
library("ggstatsplot")
library("car")
library("ggpubr")
library("olsrr")
library("caret")
library("Fgmutils")
install.packages()
data = read_excel('/Users/nikhillakhwani/Desktop/c67/project/RegressionCaseStudy/data/BrownFat.xls')

data = subset(data, select = -c(1, 21, 23))

# Removing cancer status : redundant 
data = subset(data, select = -c(18))


# Removing outliers----------------------

boxplot(data$Sex)

boxplot(data$Diabetes)
data <- data[-which(data$Diabetes %in% boxplot.stats(data$Diabetes)$out), ]
boxplot(data$Diabetes)

boxplot(data$Age)
data <- data[-which(data$Age %in% boxplot.stats(data$Age)$out), ]
boxplot(data$Age)

boxplot(data$`7D_Temp`)
data <- data[-which(data$`7D_Temp` %in% boxplot.stats(data$`7D_Temp`)$out), ]
boxplot(data$`7D_Temp`)


boxplot(data$Season, data = data)

boxplot(data$Duration_Sunshine, data = data)

boxplot(data$BMI, data = data)
data <- data[-which(data$BMI %in% boxplot.stats(data$BMI)$out), ]
boxplot(data$BMI)

boxplot(data$Glycemy, data = data)
data <- data[-which(data$Glycemy%in% boxplot.stats(data$Glycemy)$out), ]
boxplot(data$Glycemy)

boxplot(data$LBW, data = data)

boxplot(data$Ext_Temp, data = data)

boxplot(data$`2D_Temp`, data = data)

boxplot(data$`3D_Temp`, data = data)

boxplot(data$`1M_Temp`, data = data)

boxplot(data$Weigth, data = data)
data <- data[-which(data$Weigth %in% boxplot.stats(data$Weigth)$out), ]
boxplot(data$Weigth)

boxplot(data$Size, data = data)
data <- data[-which(data$Size %in% boxplot.stats(data$Size)$out), ]
boxplot(data$Size)

ggplot(data = data, aes(Size, BrownFat)) + geom_point(color = "red")

ggplot(data = data, aes(Sex, BrownFat)) + geom_point(color = "red")
ggplot(data = data, aes(Diabetes, BrownFat)) + geom_point(color = "red")
ggplot(data = data, aes(Age, BrownFat)) + geom_point(color = "red")
ggplot(data = data, aes(Ext_Temp, BrownFat)) + geom_point(color = "red")
ggplot(data = data, aes(Size, BrownFat)) + geom_point(color = "red")
ggplot(data = data, aes(Size, BrownFat)) + geom_point(color = "red")
ggplot(data = data, aes(Size, BrownFat)) + geom_point(color = "red")



# split into train and test set--------------

# Randomly Split the data into training and test set
set.seed(1212)
training.samples <- data$BrownFat %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
test.data = na.omit(test.data)
train.data = na.omit(train.data)
Y = train.data$BrownFat
X1 = train.data$Sex
X2 = train.data$Diabetes 
X3 = train.data$Age 
X4 = train.data$`7D_Temp`
X5 = train.data$Season 
X6 = train.data$Duration_Sunshine
X7 = train.data$BMI 
X8 = train.data$Glycemy 
X9 = train.data$LBW 
X10 = train.data$Cancer_Type 
X11 = train.data$Month
X12 = train.data$Ext_Temp
X13 = train.data$`2D_Temp`
X14 = train.data$`3D_Temp`
X15 = train.data$`1M_Temp`
X16 = train.data$Weigth
X17 = train.data$Size

# Model Selection--------------------------------

sapply(lapply(train.data, unique), length)
# Since diabetes only has one level in our training set, we drop it

fit0 <- lm(Y ~ factor(X1) + X3 +X4 +factor(X5) +X6 +X7 +X8 +X9 +factor(X10) +factor(X11) +X12 +X13 +X14 +X15 +X16 +X17)
summary(fit0)
fit1 <- lm(Y ~ factor(X1) + X3 + X4 +factor(X5) +X6 +X7 +X8 +X9 +factor(X10) +factor(X11) +X12 +X13 +X14 +X15 +X16 +X17)
fit2 <- lm(Y ~ 1)

stepAIC(fit2, direction = "both", scope = list(upper = fit1, lower = fit2))

# Final model 
fit = lm(Y ~ factor(X1) + X3 + X16 + X12 + X15)
summary(fit)


## Multicolinearity --------------------------------
summary(fit)
ggpairs(subset(data, select = c(1, 2, 3, 10, 6, 13, 19)))

# by looking at the correlation matrix, ext_temp and season are highly
# correlated and we decide to only keep one, which is ext_temp, since is has the higher correlation with brown fat
fit = lm(Y ~ factor(X1) + X3 + X16 + X12) 
summary(fit)

# VIF test


# Effective data
ed = data.frame(cbind(Age = X3, Sex = X1, Weigth = X16, Ext_Temp = X12, BrownFat = Y))

# Check for interaction terms
fit.inter = lm(Y ~ (factor(X1) + X3 + X16 + X12)^2)
anova(fit.inter)
# interaction terms are not significant and seem to complicate the model, so we decide not to add them


# Checking Assumptions ------------------------------------

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
fit = lm(Y + 1 ~(factor(X1) + X3 + X16 + X12))        # shift resoponse since 1/0 is undefined
result = boxcox(fit)
lambda = result$x[which.max(result$y)]
fit = lm(((Y+1) ^ lambda - 1)/lambda ~(factor(X1) + X3 + X16 + X12))
summary(fit)
# still not good but it is what it is

hist(fit$residuals)
# Looks like a left-tail normal


# Try Polynomial model--------------------
pm = lm(Y ~ polym(X3 , X12 , X16, degree=2, raw=TRUE))        # shift resoponse since 1/0 is undefined
summary(pm)
# Multiple R-squared keep increasing as we increase the degree (danger of overfitting)

# box cox suggest to use lambda = -2 
result = boxcox(pm)
lambda = result$x[which.max(result$y)]
fit = lm(((Y+1) ^ lambda - 1)/lambda ~polym(Age , Weigth , Sex , Ext_Temp, degree=2, raw=TRUE), data = train.data)
summary(fit)
#
## Normality of errors (Still bad)
res = resid(pm)

#produce residual vs. fitted plot
plot(fitted(pm), res)

#add a horizontal line at 0 
abline(0,0)


# Linearity Assumption
plot(pm ,1)

# Homoscedasticity Assumption 
ols_test_score(pm)

  # Autocorrelation Assumption 
durbinWatsonTest(pm)

# Normality Assumption
shapiro.test(pm$residuals)

# Multicolinearity Assumption
vif(pm)


# Graphical representations of Influencial points
ols_plot_cooksd_char(fit)
ols_plot_dfbetas(fit)
ols_plot_dffits(fit)
p1 -> ols_plot_cooksd_char(fit)
p2 -> ols_plot_dffits(fit)
ggarrange(p1, p2, ncol=2, nrow=1)



## Checking for unequal variance
ed$resid = rstandard(fit)
ggplot(data=ed, aes(X1, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X2, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X3, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X5, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X9, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X12, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)
ggplot(data=ed, aes(X16, resid, col="red")) + geom_point() + geom_smooth(method = "lm", se=FALSE)

# Seem to have equal variance, no need to do WLS to complicate model more


# Prediction on test set----------------------
prediction = predict(fit, test.data[, c(1, 13, 3, 6)]) 

# Checking performance by calculating R2 , RMSE and MAE
data.frame( R2 = R2(prediction, test.data$BrownFat),
            RMSE = RMSE(prediction, test.data$BrownFat),
            MAE = MAE(prediction, test.data$BrownFat)
            # ,MSPR = mspr(test.data$BrownFat ,prediction, dim(test.data)[1] )
            )

# All in all, there seem to be other factors effecting brown fat