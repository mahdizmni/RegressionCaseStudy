library("readxl")

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
ct17 = as.numeric(data$Cancer_Type == 17)
# No is the base category
b = as.numeric(data$BrownFat == 1)
# No brown fat is the base category
