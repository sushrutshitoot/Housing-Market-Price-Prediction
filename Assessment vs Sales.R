#- Set the working directory -#
setwd("D:/Freelance/Fiverr/Housing Market Price Prediction/Assignment")
getwd()

#- Import data -#
Data <- read.csv(file = "Assessments_vs_Sales.csv", header = TRUE)

#- Import the libraries required for the analysis -#
library(ggplot2)

#- Calculate the absolute difference and the ration between the sale price and the market value -#

Data$Assessment_to_Sales_Ratio <- Data$MARKET_VAL / Data$SALE_PRICE

#- Preliminary plot between Sale price and absolute difference -#
plot(Data$SALE_PRICE, Data$Assessment_to_Sales_Ratio)

#In the initial plot, 2 outliers are observed that might skew the relationship. Consequently, the plot will be limited to a range such that these two datapoints do not make an appearance and the rest of the data is easily visible.

```
summary(Data$Assessment_to_Sales_Ratio)
#- The ratio goes from 0.01 to 269 indicating large disparities around the assessed value vs the actual sale value -#

plot(Data$SALE_PRICE, Data$Assessment_to_Sales_Ratio, xlim = c(0, 500000), ylim = c(0, 50))
abline(a = NULL, b = NULL, h = 1)

#- It is observed that the assessment to sales ratio is higher for properties with a lower sale price and follows a decreasing trend as the sale price increases -#

#- Calculating the coefficient of concentration to understand how well the assessments are spread compared to the median ratio -#
Median <- median(Data$Assessment_to_Sales_Ratio)

CoC = (length(Data$Assessment_to_Sales_Ratio[Data$Assessment_to_Sales_Ratio < 0.85 * Median]) + length(Data$Assessment_to_Sales_Ratio[Data$Assessment_to_Sales_Ratio > 1.15 * Median])) / nrow(Data)
CoC

#- Coefficient of concentration comes out to be 44.49% indicating that 44.49% of the houses are valued in the +/- 15% of the ideal valuation (Ideal valuation is CoC = median ratio) -#
#- Conversely, it also implies that 100 % - 44.49% = 55.51% houses fall beyond the 15% threshold for the fair valuation bracket -#

#- calculating a second measure of quality of assessment -#
#- Coefficient of dispersion -#

Data$CODData <- abs(Data$Assessment_to_Sales_Ratio - Median)
summary(Data$CODData)

COD = mean(Data$CODData) / Median
COD
#- Coefficient of dispersion comes out to be 26% indicating that on an average, the ratio between of assessed value and the actual sale price deviate by 26% from the median ratio -#
#- Typically, a coeffcient of dispersion of 20% is considered to be a benchmark in the real estate industry -#
#- COD values greater than 20% indicate that the properties are not assessed fairly -#
