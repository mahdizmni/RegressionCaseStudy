library("readxl")

data = read_excel('~/git/RegressionCaseStudy/data/BrownFat.xls')

data = subset(data, select = -c(21))
