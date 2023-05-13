#- Project: Sustainability Index -#
#- Date: 9th May 2023 -#
#- Author: Sushrut Shitoot -#

#- Commnents -#
#- 1. There in one instance in the dataset where an entire row is duplicated. The dataset has be deduped for processing -#
#- 2. There is an instance in the dataset where the same house has been sold twice. In the housing market, renovations etc. can be accounted for in the latest sale and hence the latest sale entry is only retained. The older entry is excluded from the analysis -#
#- 3. The year in which the house was built is available. That can be used to calculate the age of the house -#


#- Delete R objects to free up space -#
rm(list = ls(all = TRUE)) 

#- Check free space -#
gc()

#- Set working directory -#
setwd("D:/Freelance/Fiverr/Housing Market Price Prediction/Assignment")
getwd()

#- Install the required packages for the analysis -#
#install.packages("readxl") #- Package to import xlsx files in R -#
#install.packages("openxlsx")

#- Load packages in the environment for use -#
#library(readxl)
library(openxlsx)
library(dplyr)
library(data.table)
library(funModeling)
library (caret)
library(corrplot)
library(MASS)
library (Boruta)
library(caret)
library(randomForest)

#- Import data infrom xlsx files in R -#
Data <- read.csv(file = "Input.csv", header = TRUE)
Data_Backup <- Data

summary(Data) #- Different variables seem to be in different ranges. Standardization, as a best practice, needs to be done -#
str(Data) #- A mix of character, integer and numeric variables exist in the dataset -#
nrow(Data) #- 14471 rows
dim(Data) #- 14471 rows x 146 columns

#- Check for duplicate records -#
length(duplicated(Data)[duplicated(Data) == TRUE])

#- Retain only unique records for the purpose of the analysis -#
Data <- Data[!duplicated(Data),]

#- Consider only the latest entries for sale of the houses in the dataset -#
#- var23: PRINT_KEY -#
#- var30: SALE_DATE -#
Data <- Data %>% 
  group_by(var23) %>%
  slice(which.max(var30))

#- Add a variable that represents the number of times the property has been sold -#
#- Calculate the number of instances for each PRINT KEY -#
PRINTKEY <- Data_Backup %>% count(var23)
PRINTKEY$n <- as.character(PRINTKEY$n)
#View(PRINTKEY)

stringr::str_trim(Data$var23)
stringr::str_trim(PRINTKEY$var23)

Data <- merge(Data, PRINTKEY, by.x = "var23", by.y = "var23", all.x = TRUE)
#unique(Data$n)

setnames(Data, old = "n", new = "var145")
names(Data)

#- Adding age of the property to the dataset -#
Data$var146 <- 2023 - Data$var7

#- Introduce an ID variable against each unique observation -#
ID <- c(1:nrow(Data))
Data$ID <- paste("ID", ID, sep = "")

head(Data$ID) #- ID1, ID2, .. 
tail(Data$ID) #- .. ID10749, ID10750

#- Bring the ID column to the beginning and removing the PRINT_KEY variable that serves as the key in the dataset -#
Data <- Data[, c(148, 1:144, 146:147, 145)]
names(Data)

#- Drop the PRINT_KEY and use the ID variable as the primary key instead -#
Data$var23 <- NULL

#- Convert the variables to the right variable class to process the data appropriately -#
str(Data)

Columns <- as.data.frame(colnames(Data))
Columns$Position <- row.names(Columns)

write.csv(Columns, "Columns.csv", row.names = TRUE)

Data$ID <- as.factor(Data$ID)

#- Change the date variables to the appropriate date format -#
Data <- Data %>% mutate_at(c('var30', 'var32'), convertToDateTime)
Data$var30 <- as.Date(substr(Data$var30, 1, 10))
Data$var32 <- as.Date(substr(Data$var32, 1, 10))

#- Change the variables that should have variable class as numeric -#
Data <- Data %>% 
  mutate_at(c(2:25, 27:29, 31, 33, 35:37, 141:147), as.numeric)

#- Change the variables that should have variable class as factor -#
Data <- Data %>% 
  mutate_at(c(1, 26, 34, 38:140), as.factor)

str(Data)


#- 1. Univariate analysis -#
Univariate1 <- df_status(Data)

#- 1.1 Checking min, max, median, sd of variables -#
Univariate2 <- do.call (data.frame,
                        list (mean = vapply(Data, function(x) mean(x[!(is.na(x))]), numeric(1)),
                              median = apply(Data, 2, median, na.rm = TRUE),
                              min = apply(Data, 2, min, na.rm = TRUE),
                              max = apply(Data, 2, max, na.rm = TRUE),
                              sd = apply(Data, 2, sd, na.rm = TRUE)
                        ))


#- Writing the Univariate analysis (1.1 + 1.2) for future analysis and reference
Univariate <- cbind (
  var = Univariate1$variable,
  type = Univariate1$type,
  q_zeros = Univariate1$q_zeros,
  p_zeros = Univariate1$p_zeros,
  q_na = Univariate1$q_na,
  p_na = Univariate1$p_na,
  mean = Univariate2$mean,
  median = Univariate2$median,
  min = Univariate2$min,
  max = Univariate2$max,
  sd = Univariate2$sd
)

write.csv (Univariate, file = "Univariate Analysis.csv", row.names = TRUE)

nzv_details = nearZeroVar (Data, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)
write.csv (nzv_details, file = "Univariate_NearZerovariance.csv", row.names = TRUE)

#- Feature Elimination -#
#- Multiple criteria are used for feature elimination -#
Data.ID <- Data$ID
Data.Y <- Data$Y

Data$ID <- NULL
Data$Y <- NULL

#- 2. Eliminating features (variables) -#
#- 2.1 Removing constant features -#
#- 2.2 Removing features with a lot of NAs -#
#- 2.3 Removing identical features -#
#- 2.4 Removing features with near zero variance -#
#- 2.5 Removing features with too many unique values -#
#- 2.6 Removing highly correlated features -#

#- 2.1 Removing constant features -#
toRemove <- c()
feature.names <- names(Data)

for (f in feature.names)
{
  if (class(Data[[f]]) == "numeric")
  {
    if (sd(Data[[f]], na.rm = TRUE) == 0)
    {
      toRemove <- c(toRemove, f)
      cat (f, " is constant\n")
    }
  }
  else if (class(Data[[f]]) == "integer")
  {
    u <- unique(Data[[f]])
    if (length(u) == 1)
    {
      toRemove <- c(toRemove, f)
      cat(f, "is constant\n")
    }
  }
  else if (class(Data[[f]]) == "character")
  {
    u <- unique (Data[[f]])
    if (length(u) == 1)
    {
      toRemove <- c(toRemove, f)
    }
  }
}

toRemove
feature.names <- setdiff (names(Data), toRemove)

Data <- Data[, feature.names]
write.csv (toRemove, "1. FE_Constant_Features.csv", row.names = FALSE)

#- 2.2 Removing features with a lot of NAs -#
toRemove <- c()
feature.names <- names(Data)

for (f in feature.names)
{
  if (!class(Data[[f]]) == "character")
  {
    if (sum(is.na(Data[[f]]))/nrow(Data) > 0.70)
    {
      toRemove <- c(toRemove, f)
    }
  }
}

toRemove
feature.names <- setdiff(names(Data), toRemove)

Data <- Data[, feature.names]
write.csv (toRemove, "2. FE_Lot_of_NAs.csv", row.names = FALSE)

#- 2.3 Removing Identical Features -#
feature.pairs <- combn(names(Data), 2, simplify = FALSE)
toRemove <- c()

for (pair in feature.pairs)
{
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(class(Data[[f1]]) == "factor") & !(class(Data[[f2]]) %in% "factor"))
  {
    if (!(f1 %in% toRemove) & !(f2 %in% toRemove))
    {
      if (all(Data[[f1]] == Data[[f2]], na.rm = TRUE))
      {
        cat (f1, "and", f2, "are equals.\n")
        toRemove <- c(toRemove, f2)
      }
    }
  }  
}

toRemove
feature.names <- setdiff(names(Data), toRemove)

Data <- Data[, feature.names]
write.csv (toRemove, "3. FE_Identical_Features.csv", row.names = FALSE)

#- 2.4 Removing features with near zero variance -#
toRemove <- c()
feature.names <- names(Data)

nzv_cols <- nearZeroVar (Data, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = TRUE)

toRemove <- nzv_cols

#if (length(nzv_cols) > 0) Data <- Data[,-c(nzv_cols)]
feature.names <- setdiff(names(Data), toRemove)
Data <- Data[, feature.names]

write.csv (toRemove, "4. FE_NZV.csv", row.names = FALSE)

#- 2.5 Removing features with too many unique values -#
toRemove <- c()
feature.names <- names(Data)

for (f in feature.names)
{
  if(class(Data[[f]]) %in% c("character", "factor"))
  {
    u <- unique (Data[[f]])
    if (length(u)/nrow(Data) > 0.20)
    {
      toRemove <- c (toRemove,f)
      cat (f, " has too many categories\n")
    }
    else if (length(u) > 40)
    {
      toRemove <- c(toRemove, f)
      cat (f, " has too many categories\n")
    }
  }
}

toRemove
feature.names <- setdiff(names(Data), toRemove)

Data <- Data[,feature.names]
write.csv (toRemove, "5. FE_Too_Many_Categories.csv", row.names = FALSE)

#- 2.6 Highly correlation features -#
#- Removing variables that have high missing values and other issues -#
Data$var36 <- NULL
Data$var37 <- NULL
Data$var142 <- NULL
Data$var143 <- NULL
Data$var144 <- NULL

#- Reintroducing the dependent variable to understand the correlation betweeen independent variables as well as the dependent variable -#
Data$Y <- Data.Y

#- Create a subset of the dataset to include only the numeric variables for correlation -#
Numeric_variables <- unlist(lapply(Data, is.numeric))
Data_for_correlation <- Data[ , Numeric_variables]
Data_for_correlation$Y <- Data.Y
Data_for_correlation$Y_AL <- 10^Data$Y

Data.Y_AL <- Data_for_correlation$Y_AL

CorrelationMatrix <- cor(Data_for_correlation)

#- Plot the correlation matrix with aesthetic tweaking for best look -#
corrplot(CorrelationMatrix, method = "square", main = "Correlation Matrix", mar = c(0,0,2,0), outline = T, addgrid.col = "darkgrey", addrect = 2, rect.col = "black", cl.pos = "r", tl.col = "indianred4", tl.cex = 0.8, cl.cex = 0.75, tl.srt = 45, col = colorRampPalette(c("darkred", "white","midnightblue"))(100))
#mtext("Correlation Matrix", at=11, line=0.0005, cex=1.2)

write.csv(CorrelationMatrix, "Correlation Matrix.csv", row.names = TRUE)

#- Only var7 was eliminated since it had a complete inverse correlation with var146 (Year built vs house age) -#
#- Threshold considered for decision making (R = 0.8) -#


#- 3. Variable treatment -#

#- 3.1 Missing Value Treatment - Replacing NAs with mean / median / mode -#
getMode <- function (x, na.rm) {
  xtab <- table (x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

#- MVT on Data -#
for (var in 1:ncol(Data)) {
  if (class(Data[,var]) == "numeric")
  {Data[is.na(Data[,var]),var] <- mean(Data[,var], na.rm = TRUE)}
  else if (class(Data[,var]) %in% c("character", "factor"))
  {Data[is.na(Data[,var]),var] <- getMode(Data[,var], na.rm = TRUE)}
  else if (class(Data[,var]) %in% c("integer"))
  {Data[is.na(Data[,var]), var] <- as.numeric(round(mean(Data[,var], na.rm = TRUE),0))}
}

#- 3.2 Outlier Treatment -#
#- Applicable ONLY for numeric variables; not for integers, characters or factors -#
fun <- function (x)
{
  quantiles <- quantile(x, c(0.05, 0.95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}  

feature.names <- names(Data)
for (f in feature.names)
{
  if (class(Data[[f]]) == "numeric")
  {
    Data[[f]] <- fun(Data[[f]])
  }
}

#- 4. Bivariate Analysis -#
Data$Y <- Data.Y

#- Bivariate plot with Y -#
BivariateData <- Data

for (i in seq(1,44))
{plot(BivariateData[,i], BivariateData$Y, xlab = colnames(BivariateData)[i])}

#- 5. Data standardization using z-transformation -#
Data$Y <- NULL

Scaled <- sapply(Data, is.numeric)
Data[Scaled] <- lapply(Data[Scaled], scale)

Data %>% 
  rename_at(.vars = vars(ends_with(".Scaled")),
            .funs = funs(sub("[.]Scaled", "", .)))



summary(Data)
str(Data)

#- Reintroducing the ID and dependent variable -#
Data$Y <- Data.Y
Data$Y_AL <- Data.Y_AL
Data$ID <- Data.ID

#- Split the dataset into train and test -#
TrainFlag <- createDataPartition(y=Data$Y_AL,p=0.7,list=FALSE)
Train <- Data[TrainFlag,]
Test <- Data[-TrainFlag,]

Data$ID <- NULL
Data$Y <- NULL
Data$Y_AL <- NULL

feature.names <- names(Data)
feature.names

Data$ID <- Data.ID
Data$Y_AL <- Data.Y_AL

Train$Y <- NULL
Test$Y <- NULL

#- 5. Feature selection using Boruta -#
#- Implement the Boruta algorithm to identify the most important variables -#
set.seed(123)
#Boruta.train <- Boruta(Y_AL ~. -ID, data = Train, doTrace = 1, maxRuns = 100)
print(Boruta.train)

#- Plot the variable importance and decision together on one chart -#
plot(Boruta.train, main = "Variable Importance")

Boruta.Dataframe <- attStats(Boruta.train)
print(Boruta.Dataframe)
write.csv(Boruta.Dataframe, "Boruta Output.csv", row.names = TRUE)

Boruta_ConfirmedVariables <- getSelectedAttributes(Boruta.train, withTentative = FALSE)
write.csv(Boruta_ConfirmedVariables, "Boruta_ConfirmedVariables.csv", row.names = TRUE)

#- Extracting the list of final predictors from Boruta that can be used as input for building the model -#
Predictor.names_Boruta <- getSelectedAttributes(Boruta.train, withTentative = FALSE)
Predictor.names_Boruta

#- Feature Formula -#
Predictor.names_Boruta
Predictors.formula.Boruta <- formula(paste('Y_AL ~', paste(Predictor.names_Boruta, collapse = '+'), sep = ''))
Predictors.formula.Boruta

#- 6. Model Building -#
SearchGrid <- expand.grid(mtry = c(1:3))

#- Define the parameters for cross-validation -#
fitControl <- trainControl(
  method = "repeatedcv", number = 8, repeats = 3,
  verboseIter = TRUE,
  classProbs = FALSE,
  savePredictions = TRUE)

#- Model Building -#
set.seed(1134)

Model.fit <- train(
  Y_AL ~ var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + 
    var9 + var10 + var11 + var24 + var25 + var27 + var28 + var30 + 
    var66 + var69 + var73 + var74 + var76 + var80 + var95 + var97 + 
    var98 + var107 + var108 + var109 + var112 + var113 + var116 + 
    var120 + var127 + var129 + var138 + var139 + var141 + var145 + 
    var146, #Individual variable names can also be used for better iteration purposes at a later step
  data = Train,
  method = "rf",
  trControl = fitControl,
  verbose = TRUE,
  tuneGrid = SearchGrid,
  metric = "R Squared")

Model.fit

summary(Model.fit)
VariableImportance <- varImp(Model.fit)
plot(varImp(Model.fit))
plot(Model.fit)

write.csv(VariableImportance$importance, file = "Variable Importance RF.csv", row.names = TRUE)

#- write a function to calculate the RSquared value -#
rsq <- function (x, y) cor(x, y) ^ 2

#- Apply the model on the train set -#
Predictions_Train <- predict(Model.fit, newdata = Train)
rsq(Train$Y_AL, Predictions_Train) #- Train R squared = 0.9097402

plot(Train$Y_AL, Predictions_Train)

#- Apply the model on the test set -#
Predictions_Test <- predict(Model.fit, newdata = Test)
rsq(Test$Y_AL, Predictions_Test) #- Test R Squared = 0.6601749

plot(Test$Y_AL, Predictions_Test)

