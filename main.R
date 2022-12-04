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
library("tidyverse")
library("lmtest")
library("dplyr")

data = read_excel('~/git/RegressionCaseStudy/data/BrownFat.xls')

### Data cleaning------------------

# Remove id, TSH(have lots of NA, so it would effect negatively if we let NA = 0), and total volume of brown fat : bc we 
# just want to predict its existance
data = subset(data, select = -c(1, 21, 23))

# Removing cancer status : redundant 
data = subset(data, select = -c(18))


# Descriptive statistics (For all variables)
plot(data$Age, data$BrownFat)

ggplot(data, aes(x=Age)) + 
  geom_histogram(color="black", fill="red")

summary(data$Age)


### Removing outliers----------------------

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


### Train-test split--------------

# Randomly Split the data into training and test set
set.seed(1212)
training.samples <- data$BrownFat %>%
  createDataPartition(p = 0.70, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]
test.data = na.omit(test.data)
train.data = na.omit(train.data)

# Naming variables
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


### Model Selection--------------------------------

# Check if a categorical variables has only one type, if yes, we remove since it's not effective and the lm() returns an error
sapply(lapply(train.data, unique), length)
# Since diabetes only has one level in our training set, we drop it

# Full model (Description of the data and significance of t values)
fit0 <- lm(Y ~ factor(X1) + X3 +X4 +factor(X5) +X6 +X7 +X8 +X9 +factor(X10) +factor(X11) +X12 +X13 +X14 +X15 +X16 +X17)
summary(fit0)

# Models to use for stepwise regression based on AIC(add explanation from lecture notes)
fit1 <- lm(Y ~ factor(X1) + X3 + X4 +factor(X5) +X6 +X7 +X8 +X9 +factor(X10) +factor(X11) +X12 +X13 +X14 +X15 +X16 +X17)
fit2 <- lm(Y ~ 1)

stepAIC(fit2, direction = "both", scope = list(upper = fit1, lower = fit2))

# Final model obtainded by stepAIC()
fit = lm(Y ~ X3 + X12 + X15 + X8 + X7 + factor(X1) + X17)
summary(fit)        # explanation of the data by p-value and R-squared
qf(1 - 0.05, 7, 2703)
# f-test : reject the null hypo

## Multicolinearity --------------------------------

summary(fit)

# VIF test : if one of the variables had a vif number close or more than 10, then that variable is problematic (multicolinearity)

vif(fit) # remove whatever is the hightest predictor

# using correlation matrix as well
ggpairs(subset(data, select = c(1, 3, 15, 16, 6, 10, 14, 19)))
# by looking at the correlation matrix, ext_temp and 1m_temp are highly
# correlated and we decide to only keep one, which is ext_temp, since is has the higher correlation with brown fat

fit = lm(Y ~ X3 + X12 + X8 + X7 + factor(X1) + X17) 
summary(fit) # explanation of the data


# Effective data
ed = data.frame(cbind(Age = X3, Sex = X1, Size = X17, Ext_Temp = X12, Glycemy = X8, BMI = X7, BrownFat = Y))

# Check for interaction terms
fit.inter = lm(Y ~ (X3 + X12 + X8 + X7 + factor(X1) + X17)^2)
anova(fit.inter)
summary(fit.inter)
qf(1 - 0.05, 21, 2689)
# F-test : reject the null
# we got slightly better R-squared in this one
fit = fit.inter

# Checking Assumptions ------------------------------------

## Normality of errors
res = resid(fit)

#produce residual vs. fitted plot
plot(fitted(fit), res)

#add a horizontal line at 0 
abline(0,0)

## result : errors are not randomly spreaded (lec 6), violoates

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

# plotting his of residuals
hist(fit$residuals)
# Looks like a left-tail normal

## result : errors are not normally distributed

# remedy: use box cox transformation

# adding 1 to Y, to not get errors in box-cox, since we have 1/0 in calcluations,
# we shift the response variables form {0, 1} to {1, 2}
fit = lm(Y + 1 ~ (X3 + X12 + X8 + X7 + factor(X1) + X17)^2)  
result = boxcox(fit)
lambda = result$x[which.max(result$y)]
# lambda = -2

fit = lm(((Y+1) ^ lambda - 1)/lambda ~ (X3 + X12 + X8 + X7 + factor(X1) + X17)^2)
summary(fit)

# rechecking assumptions to see if it got any better
## Normality of errors
res = resid(fit)

#produce residual vs. fitted plot
plot(fitted(fit), res)

#add a horizontal line at 0 
abline(0,0)

## result : errors are not randomly spreaded (lec 6), violoates

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

# result : it did not get any better and box-cox did not work out


# best model so far:
summary(fit)


# Try Polynomial model--------------------
# have to remove factor(X1), since it's categorical and can't be added to polynomial
pm = lm(Y ~ polym(X3 , X12 , X8 , X7 , X17, degree=3, raw=TRUE))        # shift resoponse since 1/0 is undefined
summary(pm)
# Multiple R-squared keep increasing as we increase the degree (danger of overfitting)
# on degree2, our interaction model worked better, check r-squares

### Graphical representations of Influencial points
ols_plot_cooksd_char(fit)         # Cooks distance
ols_plot_dfbetas(fit)           #DFBETAS
ols_plot_dffits(fit)            #DFFITS
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

# Test of equal variances
bptest(fit)
# result : p-value < 0.05, so we have unequal variances

# remedy: Weighted Least Square Regression

#define weights to use
wt <- 1 / lm(abs(fit$residuals) ~ fit$fitted.values)$fitted.values^2

wls_fit <- lm(Y ~ (Age + Ext_Temp + Glycemy + BMI + Sex + Size)^2,  data = train.data, weights=wt)

summary(wls_fit)

# Result : wow! we got 0.602 R-squared

# Final Model : is the weighted least squares model with interaction terms
# lm(Y ~ (X3 + X12 + X8 + X7 + factor(X1) + X17)^2,  weights=wt

fit = wls_fit

# Prediction on test set----------------------

# Selecting chosen predictors from test data
test = data.frame(test.data[, c(3, 6, 16, 15, 1, 14)])
# Predictoin
prediction = predict(fit, newdata = test) 

# changing the type of prediction (it's a technical thing, no need to mention)
prediction = unname(prediction)

# if the predicted value is < 1/2, we label it as 0, otherwise 1
prediction[prediction < 0.5] = 0
prediction[prediction >= 0.5] = 1

# Testing on the testing set
data.frame( R2 = R2(prediction, test.data$BrownFat),
            RMSE = RMSE(prediction, test.data$BrownFat),
            MAE = MAE(prediction, test.data$BrownFat))
            
# As we can see our prediction is not good, one reason could be the model that we selected.
# Since our target(response) varaible is binary, the linear regression loss function penalizes us, even if we make a high confidential decision.
# the better approach would be to use a logistic regression model.
# Implemening logistic regression model
lfit = glm(BrownFat ~ Age + Ext_Temp + Glycemy + BMI + Sex + Size,  data = train.data)

lprediction = predict(fit, newdata = test)
lprediction[lprediction < 0.5] = 0
lprediction[lprediction >= 0.5] = 1

data.frame( R2 = R2(lprediction, test.data$BrownFat),
            RMSE = RMSE(lprediction, test.data$BrownFat),
            MAE = MAE(lprediction, test.data$BrownFat))

# Interestingly, as we can see, the prediction remained the same. Therefore we can conclude that,
# Linear regression and logistic regression are not good models for predicion on this data. 
# Therefore, this data is not linearly separable.
# Take outs:
# One can try to map the features to another space and apply logistic regression and try to see if this made a difference.
# Linear and polynomial regression does not seem to be good models, according to our analysis. One can try different models.
# There could be other factors which affects the existance of brown fat!