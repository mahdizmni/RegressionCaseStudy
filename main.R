library("readxl")
library("GGally")
library("ggplot2")

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
data = subset(data, data$Age > 17)
data = subset(data, data$Weigth > 25)
data = subset(data, data$Weigth < 175)
data = subset(data, data$Size > 125)

ggpairs(subset(data, select = c(6, 7, 8, 9, 10, 20)))
# Removing highly correlated temparatures
data = subset(data, select = -c(6, 7, 8, 10))
# Removin size and weight, check for colinearrity of size and weight and each
# in relation to the response variable

ggpairs(subset(data, select = c(9, 10, 11, 13, 2, 12, 16)))
# Removing size and weight
data = subset(data, select = -c(9, 10))

# Removing cancer status : redundant 
data = subset(data, select = -c(12))

# Reomveing day and month since it is not related for prediction
data = subset(data, select = -c(4, 5))

ggpairs(subset(data, select = c(4, 5, 6)))
