# Load necessary libraries
library(dplyr)
library(caret)

# Load the data
train_data = read.csv("train.csv")
test_data = read.csv("test.csv")
train_data
# Create the target variable in test_data (since we will predict SalePrice)
test_data$SalePrice = NA

# Combine train and test data for consistent processing
merged_data = rbind(train_data, test_data)

# Select relevant columns for the model
model_data = merged_data %>%
  select(Id, SalePrice, YearBuilt, LotArea, GarageCars, YearRemodAdd, 
         TotalBsmtSF, GrLivArea, TotRmsAbvGrd, Fireplaces, GarageArea, 
         OpenPorchSF, PoolArea, WoodDeckSF, MiscVal, YrSold)
colSums(is.na(merged_data))

# Split the merged data back into train and test sets
train_set = model_data[!is.na(model_data$SalePrice), ]
test_set = model_data[is.na(model_data$SalePrice), ]

# Handle missing values (replace NA with mean for numeric columns)
train_set = train_set %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

test_set = test_set %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Build a linear regression model using the training data
linear_model = lm(SalePrice ~ . -Id, data = train_set)

# Summarize the model (optional)
summary(linear_model)

# Predict SalePrice for the test set
test_set$SalePrice = predict(linear_model, newdata = test_set)

# Create a submission file with Id and predicted SalePrice
submission_data = test_set %>%
  select(Id, SalePrice)

# Ensure submission has 1459 rows
if (nrow(submission_data) == 1459) {
  # Write the submission file as CSV
  write.csv(submission_data, "submission_Linear.csv", row.names = FALSE)
  cat("Submission file created: submission_Linear.csv\n")
} else {
  cat("Error: The number of rows in the submission file is not 1459\n")
}
################################ DECISION TREE #########################################

# Load necessary libraries
library(dplyr)
library(caret)
library(dplyr)
library(rpart)  # For Decision Tree
library(rpart.plot)  # For plotting the Decision Tree
library(caret)

# Load the data
train_data = read.csv("train.csv")
test_data = read.csv("test.csv")

# Create the target variable in test_data (since we will predict SalePrice)
test_data$SalePrice = NA

# Combine train and test data for consistent processing
merged_data = rbind(train_data, test_data)

# Select relevant columns for the model
model_data = merged_data %>%
  select(Id, SalePrice, YearBuilt, LotArea, GarageCars, YearRemodAdd, 
         TotalBsmtSF, GrLivArea, TotRmsAbvGrd, Fireplaces, GarageArea, 
         OpenPorchSF, PoolArea, WoodDeckSF, MiscVal, YrSold)

# Split the merged data back into train and test sets
train_set = model_data[!is.na(model_data$SalePrice), ]
test_set = model_data[is.na(model_data$SalePrice), ]

# Build a linear regression model using the training data
decision_tree_model = rpart(SalePrice ~ ., data = train_set, method = "anova")

# Plot the decision tree (optional)
rpart.plot(decision_tree_model)


# Summarize the model
summary(decision_tree_model)

# Predict SalePrice for the test set
test_set$SalePrice = predict(decision_tree_model, newdata = test_set)

# Create a submission file with Id and predicted SalePrice
submission_data = data.frame(Id = test_set$Id, SalePrice = test_set$SalePrice)

# Ensure submission has 1459 rows
if (nrow(submission_data) == 1459) {
  # Write the submission file as CSV
  write.csv(submission_data, "submission_decisiontree.csv", row.names = FALSE)
  cat("Submission file created: submission_decisiontree.csv\n")
} else {
  cat("Error: The number of rows in the submission file is not 1459\n")
}
  
  ########################### RANDOM FOREST#################################
getwd() 
train=read.csv("train.csv")
test=read.csv("test.csv")
str(train)
str(test)
summary(train)
summary(test)


test$SalePrice = NA

str(test)
combined = rbind(train, test)
str(combined)
colSums(is.na(combined))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$MSZoning[is.na(combined$MSZoning)] = Mode(combined$MSZoning)
table(is.na(combined$MSZoning))

mean_val = mean(combined$LotFrontage, na.rm = TRUE)
combined$LotFrontage[is.na(combined$LotFrontage)] = mean_val
table(is.na(combined$LotFrontage))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$Utilities[is.na(combined$Utilities)] = Mode(combined$Utilities)
table(is.na(combined$Utilities))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$Exterior1st[is.na(combined$Exterior1st)] = Mode(combined$Exterior1st)
table(is.na(combined$Exterior1st))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$Exterior2nd[is.na(combined$Exterior2nd)] = Mode(combined$Exterior2nd)
table(is.na(combined$Exterior2nd))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$MasVnrType[is.na(combined$MasVnrType)] = Mode(combined$MasVnrType)
table(is.na(combined$MasVnrType))

mean_val = mean(combined$MasVnrArea, na.rm = TRUE)
combined$MasVnrArea[is.na(combined$MasVnrArea)] = mean_val
table(is.na(combined$MasVnrArea))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$BsmtQual[is.na(combined$BsmtQual)] = Mode(combined$BsmtQual)
table(is.na(combined$BsmtQual))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$BsmtCond[is.na(combined$BsmtCond)] = Mode(combined$BsmtCond)
table(is.na(combined$BsmtCond))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$BsmtExposure[is.na(combined$BsmtExposure)] = Mode(combined$BsmtExposure)
table(is.na(combined$BsmtExposure))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$BsmtFinType1[is.na(combined$BsmtFinType1)] = Mode(combined$BsmtFinType1)
table(is.na(combined$BsmtFinType1))

mean_val = mean(combined$BsmtFinSF1, na.rm = TRUE)
combined$BsmtFinSF1[is.na(combined$BsmtFinSF1)] = mean_val
table(is.na(combined$BsmtFinSF1))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$BsmtFinType2[is.na(combined$BsmtFinType2)] = Mode(combined$BsmtFinType2)
table(is.na(combined$BsmtFinType2))

mean_val = mean(combined$BsmtFinSF2, na.rm = TRUE)
combined$BsmtFinSF2[is.na(combined$BsmtFinSF2)] = mean_val
table(is.na(combined$BsmtFinSF2))

mean_val = mean(combined$BsmtUnfSF, na.rm = TRUE)
combined$BsmtUnfSF[is.na(combined$BsmtUnfSF)] = mean_val
table(is.na(combined$BsmtUnfSF))

mean_val = mean(combined$TotalBsmtSF, na.rm = TRUE)
combined$TotalBsmtSF[is.na(combined$TotalBsmtSF)] = mean_val
table(is.na(combined$TotalBsmtSF))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$Electrical[is.na(combined$Electrical)] = Mode(combined$Electrical)
table(is.na(combined$Electrical))

mean_val = mean(combined$BsmtFullBath, na.rm = TRUE)
combined$BsmtFullBath[is.na(combined$BsmtFullBath)] = mean_val
table(is.na(combined$BsmtFullBath))

mean_val = mean(combined$BsmtHalfBath, na.rm = TRUE)
combined$BsmtHalfBath[is.na(combined$BsmtHalfBath)] = mean_val
table(is.na(combined$BsmtHalfBath))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$KitchenQual[is.na(combined$KitchenQual)] = Mode(combined$KitchenQual)
table(is.na(combined$KitchenQual))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$Functional[is.na(combined$Functional)] = Mode(combined$Functional)
table(is.na(combined$Functional))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$FireplaceQu[is.na(combined$FireplaceQu)] = Mode(combined$FireplaceQu)
table(is.na(combined$FireplaceQu))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$GarageType[is.na(combined$GarageType)] = Mode(combined$GarageType)
table(is.na(combined$GarageType))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$GarageFinish[is.na(combined$GarageFinish)] = Mode(combined$GarageFinish)
table(is.na(combined$GarageFinish))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$GarageQual[is.na(combined$GarageQual)] = Mode(combined$GarageQual)
table(is.na(combined$GarageQual))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$GarageCond[is.na(combined$GarageCond)] = Mode(combined$GarageCond)
table(is.na(combined$GarageCond))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$ PoolQC[is.na(combined$ PoolQC)] = Mode(combined$ PoolQC)
table(is.na(combined$ PoolQC))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$Fence[is.na(combined$Fence)] = Mode(combined$Fence)
table(is.na(combined$Fence))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$MiscFeature[is.na(combined$MiscFeature)] = Mode(combined$MiscFeature)
table(is.na(combined$MiscFeature))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
combined$SaleType[is.na(combined$SaleType)] = Mode(combined$SaleType)
table(is.na(combined$SaleType))

mean_val = mean(combined$GarageCars, na.rm = TRUE)
combined$GarageCars[is.na(combined$GarageCars)] = mean_val
table(is.na(combined$GarageCars))

mean_val = mean(combined$GarageYrBlt, na.rm = TRUE)
combined$GarageYrBlt[is.na(combined$GarageYrBlt)] = mean_val
table(is.na(combined$GarageYrBlt))

mean_val = mean(combined$GarageArea, na.rm = TRUE)
combined$GarageArea[is.na(combined$GarageArea)] = mean_val
table(is.na(combined$GarageArea))

colSums(is.na(combined))
str(combined)

category_mapping = c(
  "RL" = 1, 
  "RM" = 2, 
  "FV" = 3, 
  "RH" = 4, 
  "C (all)" = 5
)
combined$MSZoning1 = category_mapping[combined$MSZoning]

category_mapping = c("Pave" = 1, "Grvl" = 2)
combined$Street1 = category_mapping[combined$Street]

category_mapping = c("Reg" = 1, "IR1" = 2, "IR2" = 3, "IR3" = 4)
combined$LotShape1 = category_mapping[combined$LotShape]

category_mapping = c("Lvl" = 1, "Bnk" = 2, "HLS" = 3, "Low" = 4)
combined$LandContour1 = category_mapping[combined$LandContour]

category_mapping = c("AllPub" = 1, "NoSewr" = 2, "NoSeWa" = 3, "ELO" = 4)
combined$Utilities1 = category_mapping[combined$Utilities]

category_mapping = c(
  "Inside" = 1, 
  "Corner" = 2, 
  "CulDSac" = 3, 
  "FR2" = 4, 
  "FR3" = 5
)
combined$LotConfig1 = category_mapping[combined$LotConfig]

category_mapping = c("Gtl" = 1, "Mod" = 2, "Sev" = 3)
combined$LandSlope1 = category_mapping[combined$LandSlope]

category_mapping = c(
  "CollgCr" = 1, 
  "Veenker" = 2, 
  "Crawfor" = 3 
)
combined$Neighborhood1 = category_mapping[combined$Neighborhood]

category_mapping = c(
  "Norm" = 1, 
  "Feedr" = 2 
)
combined$Condition11 = category_mapping[combined$Condition1]

category_mapping = c(
  "Norm" = 1, 
  "Feedr" = 2
)
combined$Condition21 = category_mapping[combined$Condition2]

category_mapping = c(
  "1Fam" = 1, 
  "2FmCon" = 2, 
  "Duplex" = 3, 
  "TwnhsE" = 4, 
  "Twnhs" = 5
)
combined$BldgType1 = category_mapping[combined$BldgType]

category_mapping = c(
  "2Story" = 1, 
  "1Story" = 2, 
  "1.5Fin" = 3, 
  "1.5Unf" = 4, 
  "SFoyer" = 5, 
  "SLvl" = 6, 
  "2.5Unf" = 7, 
  "2.5Fin" = 8
)
combined$HouseStyle1 = category_mapping[combined$HouseStyle]

category_mapping = c(
  "Gable" = 1, 
  "Hip" = 2, 
  "Gambrel" = 3, 
  "Mansard" = 4, 
  "Flat" = 5, 
  "Shed" = 6
)
combined$RoofStyle1 = category_mapping[combined$RoofStyle]

category_mapping = c(
  "CompShg" = 1, 
  "Tar&Grv" = 2, 
  "WdShake" = 3, 
  "WdShngl" = 4, 
  "ClyTile" = 5, 
  "Metal" = 6, 
  "Membran" = 7, 
  "Roll" = 8
)
combined$RoofMatl1 = category_mapping[combined$RoofMatl]

category_mapping = c(
  "VinylSd" = 1, 
  "MetalSd" = 2, 
  "Wd Sdng" = 3, 
  "HdBoard" = 4, 
  "BrkFace" = 5, 
  "WdShing" = 6, 
  "CemntBd" = 7, 
  "Plywood" = 8,
  "AsbShng" = 9,
  "Stucco" = 10,
  "BrkComm" = 11,
  "AsphShn" = 12,
  "Stone" = 13,
  "ImStucc" = 14
)
combined$Exterior1st1 = category_mapping[combined$Exterior1st]

category_mapping = c(
  "VinylSd" = 1, 
  "MetalSd" = 2, 
  "Wd Shng" = 3, 
  "HdBoard" = 4, 
  "Plywood" = 5, 
  "Wd Sdng" = 6, 
  "CmentBd" = 7, 
  "Stucco" = 8,
  "BrkFace" = 9,
  "AsbShng" = 10,
  "Brk Cmn" = 11,
  "ImStucc" = 12,
  "AsphShn" = 13,
  "Stone" = 14,
  "Other" = 15
)
combined$Exterior2nd1 = category_mapping[combined$Exterior2nd]

category_mapping = c(
  "BrkFace" = 1, 
  "None" = 2, 
  "Stone" = 3, 
  "BrkCmn" = 4
)
combined$MasVnrType1 = category_mapping[combined$MasVnrType]

category_mapping = c("Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)
combined$ExterQual1 = category_mapping[combined$ExterQual]

category_mapping = c("Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)
combined$ExterCond1 = category_mapping[combined$ExterCond]

category_mapping = c(
  "BrkTil" = 1, 
  "CBlock" = 2, 
  "PConc" = 3, 
  "Slab" = 4, 
  "Stone" = 5, 
  "Wood" = 6
)
combined$Foundation1 = category_mapping[combined$Foundation]

category_mapping = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "NA" = 0)
combined$BsmtQual1 = category_mapping[combined$BsmtQual]

category_mapping = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "NA" = 0)
combined$BsmtCond1 = category_mapping[combined$BsmtCond]

category_mapping = c("Gd" = 4, "Av" = 3, "Mn" = 2, "No" = 1, "NA" = 0)
combined$BsmtExposure1 = category_mapping[combined$BsmtExposure]

category_mapping = c(
  "GLQ" = 6, 
  "ALQ" = 5, 
  "BLQ" = 4, 
  "Rec" = 3, 
  "LwQ" = 2, 
  "Unf" = 1, 
  "NA" = 0
)
combined$BsmtFinType11 = category_mapping[combined$BsmtFinType1]

category_mapping = c(
  "GLQ" = 6, 
  "ALQ" = 5, 
  "BLQ" = 4, 
  "Rec" = 3, 
  "LwQ" = 2, 
  "Unf" = 1, 
  "NA" = 0
)
combined$BsmtFinType21 = category_mapping[combined$BsmtFinType2]

category_mapping = c("GasA" = 1, "GasW" = 2, "Grav" = 3, "Wall" = 4, "OthW" = 5, "Floor" = 6)
combined$Heating1 = category_mapping[combined$Heating]

category_mapping = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1)
combined$HeatingQC1 = category_mapping[combined$HeatingQC]

category_mapping = c("Y" = 1, "N" = 0)
combined$CentralAir1 = category_mapping[combined$CentralAir]

category_mapping = c("SBrkr" = 1, "FuseA" = 2, "FuseF" = 3, "FuseP" = 4, "Mix" = 5)
combined$Electrical1 = category_mapping[combined$Electrical]

category_mapping = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1)
combined$KitchenQual1 = category_mapping[combined$KitchenQual]

category_mapping = c(
  "Typ" = 1,
  "Min1" = 2,
  "Min2" = 3,
  "Mod" = 4,
  "Maj1" = 5,
  "Maj2" = 6,
  "Sev" = 7,
  "Sal" = 8
)
combined$Functional1 = category_mapping[combined$Functional]




category_mapping = c(
  "Attchd" = 1,
  "Detchd" = 2,
  "BuiltIn" = 3,
  "Basment" = 4,
  "CarPort" = 5,
  "2Types" = 6
)
combined$GarageType1 = category_mapping[combined$GarageType]



category_mapping = c(
  "Unf" = 1,
  "RFn" = 2,
  "Fin" = 3
)
combined$GarageFinish1 = category_mapping[combined$GarageFinish]


category_mapping = c(
  "Ex" = 5,
  "Gd" = 4,
  "TA" = 3,
  "Fa" = 2,
  "Po" = 1,
  "NA" = 0  
)
combined$GarageQual1 = category_mapping[combined$GarageQual]


category_mapping = c(
  "Ex" = 5,
  "Gd" = 4,
  "TA" = 3,
  "Fa" = 2,
  "Po" = 1,
  "NA" = 0  
)
combined$GarageCond1 = category_mapping[combined$GarageCond]


category_mapping = c(
  "N" = 0,
  "P" = 1,
  "Y" = 2
)
combined$PavedDrive1 = category_mapping[combined$PavedDrive]


category_mapping = c(
  "WD" = 1,
  "CWD" = 2,
  "VWD" = 3,
  "New" = 4,
  "COD" = 5,
  "Con" = 6,
  "ConLw" = 7,
  "ConLI" = 8,
  "ConLD" = 9,
  "Oth" = 10
)
combined$SaleType1 = category_mapping[combined$SaleType]


category_mapping = c(
  "Normal" = 1,
  "Abnorml" = 2,
  "AdjLand" = 3,
  "Alloca" = 4,
  "Family" = 5,
  "Partial" = 6
)
combined$SaleCondition1 = category_mapping[combined$SaleCondition]

str(combined)

colSums(is.na(combined))
str(combined)

combined = subset(combined,select = -MSZoning)
combined = subset(combined,select = -Street)
combined= subset(combined,select = -LotShape)
combined= subset(combined,select = -Alley)
combined= subset(combined,select = -LandContour)
combined= subset(combined,select = -Utilities)
combined= subset(combined,select = -LotConfig)
combined= subset(combined,select = -LandSlope)
combined= subset(combined,select = -Neighborhood)
combined= subset(combined,select = -Condition1)
combined= subset(combined,select = -Condition2)
combined= subset(combined,select = -BldgType)
combined= subset(combined,select = -HouseStyle)
combined= subset(combined,select = -RoofStyle)
combined= subset(combined,select = -FireplaceQu)
combined= subset(combined,select = -RoofMatl)
combined= subset(combined,select = -Exterior1st)
combined= subset(combined,select = -Exterior2nd)
combined= subset(combined,select = -MasVnrType)
combined= subset(combined,select = -ExterQual)
combined= subset(combined,select = -ExterCond)
combined= subset(combined,select = -Foundation)
combined= subset(combined,select = -BsmtQual)
combined = subset(combined,select = -BsmtCond)
combined= subset(combined,select = -BsmtExposure)
combined = subset(combined,select = -BsmtFinType1)
combined= subset(combined,select = -BsmtFinType2)
combined= subset(combined,select = -Heating)
combined= subset(combined,select = -HeatingQC)
combined= subset(combined,select = -CentralAir)
combined= subset(combined,select = -Electrical)
combined= subset(combined,select = -KitchenQual)
combined= subset(combined,select = -Functional)
combined= subset(combined,select = -GarageType)
combined= subset(combined,select = -GarageFinish)
combined= subset(combined,select = -GarageQual)
combined= subset(combined,select = -GarageCond)
combined= subset(combined,select = -PavedDrive)
combined= subset(combined,select = -SaleType)
combined= subset(combined,select = -SaleCondition)
combined= subset(combined, select = -PoolQC)
combined= subset(combined, select = -Fence)
combined= subset(combined, select=-MiscFeature)
combined= subset(combined, select=-Neighborhood1)
combined= subset(combined, select=-Condition11)
combined= subset(combined, select=-Condition21)
combined= subset(combined, select=-BldgType1)
combined= subset(combined, select=-Exterior1st1)
combined= subset(combined, select=-Exterior2nd1)

str(combined)

colSums(is.na(combined))

library(randomForest)
target_variable = "SalePrice"  
train_data = combined[!is.na(combined$SalePrice), ]
test_data = combined[is.na(combined$SalePrice), ]
test_data = test_data[, !(names(test_data) %in% target_variable)]
set.seed(44)
rf_model = randomForest(SalePrice ~ ., data = train_data, ntree = 1000)
predictions = predict(rf_model, test_data)
test_data$Predicted_SalePrice=predictions
MySubFin=data.frame(Id=test_data$Id,SalePrice=predictions)
write.csv(MySubFin,"Random_forest.csv",row.names=FALSE)

############################ SVM ##############################

library(e1071)
getwd() 
data=read.csv("train.csv")
hp=read.csv("test.csv")
str(data)
str(hp)
summary(data)
summary(hp)


hp$SalePrice = NA

str(hp)
mergeddata = rbind(data, hp)
str(mergeddata)
colSums(is.na(mergeddata))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$MSZoning[is.na(mergeddata$MSZoning)] = Mode(mergeddata$MSZoning)
table(is.na(mergeddata$MSZoning))

mean_val = mean(mergeddata$LotFrontage, na.rm = TRUE)
mergeddata$LotFrontage[is.na(mergeddata$LotFrontage)] = mean_val
table(is.na(mergeddata$LotFrontage))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$Utilities[is.na(mergeddata$Utilities)] = Mode(mergeddata$Utilities)
table(is.na(mergeddata$Utilities))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$Exterior1st[is.na(mergeddata$Exterior1st)] = Mode(mergeddata$Exterior1st)
table(is.na(mergeddata$Exterior1st))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$Exterior2nd[is.na(mergeddata$Exterior2nd)] = Mode(mergeddata$Exterior2nd)
table(is.na(mergeddata$Exterior2nd))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$MasVnrType[is.na(mergeddata$MasVnrType)] = Mode(mergeddata$MasVnrType)
table(is.na(mergeddata$MasVnrType))

mean_val = mean(mergeddata$MasVnrArea, na.rm = TRUE)
mergeddata$MasVnrArea[is.na(mergeddata$MasVnrArea)] = mean_val
table(is.na(mergeddata$MasVnrArea))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$BsmtQual[is.na(mergeddata$BsmtQual)] = Mode(mergeddata$BsmtQual)
table(is.na(mergeddata$BsmtQual))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$BsmtCond[is.na(mergeddata$BsmtCond)] = Mode(mergeddata$BsmtCond)
table(is.na(mergeddata$BsmtCond))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$BsmtExposure[is.na(mergeddata$BsmtExposure)] = Mode(mergeddata$BsmtExposure)
table(is.na(mergeddata$BsmtExposure))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$BsmtFinType1[is.na(mergeddata$BsmtFinType1)] = Mode(mergeddata$BsmtFinType1)
table(is.na(mergeddata$BsmtFinType1))

mean_val = mean(mergeddata$BsmtFinSF1, na.rm = TRUE)
mergeddata$BsmtFinSF1[is.na(mergeddata$BsmtFinSF1)] = mean_val
table(is.na(mergeddata$BsmtFinSF1))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$BsmtFinType2[is.na(mergeddata$BsmtFinType2)] = Mode(mergeddata$BsmtFinType2)
table(is.na(mergeddata$BsmtFinType2))

mean_val = mean(mergeddata$BsmtFinSF2, na.rm = TRUE)
mergeddata$BsmtFinSF2[is.na(mergeddata$BsmtFinSF2)] = mean_val
table(is.na(mergeddata$BsmtFinSF2))

mean_val = mean(mergeddata$BsmtUnfSF, na.rm = TRUE)
mergeddata$BsmtUnfSF[is.na(mergeddata$BsmtUnfSF)] = mean_val
table(is.na(mergeddata$BsmtUnfSF))

mean_val = mean(mergeddata$TotalBsmtSF, na.rm = TRUE)
mergeddata$TotalBsmtSF[is.na(mergeddata$TotalBsmtSF)] = mean_val
table(is.na(mergeddata$TotalBsmtSF))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$Electrical[is.na(mergeddata$Electrical)] = Mode(mergeddata$Electrical)
table(is.na(mergeddata$Electrical))

mean_val = mean(mergeddata$BsmtFullBath, na.rm = TRUE)
mergeddata$BsmtFullBath[is.na(mergeddata$BsmtFullBath)] = mean_val
table(is.na(mergeddata$BsmtFullBath))

mean_val = mean(mergeddata$BsmtHalfBath, na.rm = TRUE)
mergeddata$BsmtHalfBath[is.na(mergeddata$BsmtHalfBath)] = mean_val
table(is.na(mergeddata$BsmtHalfBath))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$KitchenQual[is.na(mergeddata$KitchenQual)] = Mode(mergeddata$KitchenQual)
table(is.na(mergeddata$KitchenQual))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$Functional[is.na(mergeddata$Functional)] = Mode(mergeddata$Functional)
table(is.na(mergeddata$Functional))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$FireplaceQu[is.na(mergeddata$FireplaceQu)] = Mode(mergeddata$FireplaceQu)
table(is.na(mergeddata$FireplaceQu))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$GarageType[is.na(mergeddata$GarageType)] = Mode(mergeddata$GarageType)
table(is.na(mergeddata$GarageType))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$GarageFinish[is.na(mergeddata$GarageFinish)] = Mode(mergeddata$GarageFinish)
table(is.na(mergeddata$GarageFinish))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$GarageQual[is.na(mergeddata$GarageQual)] = Mode(mergeddata$GarageQual)
table(is.na(mergeddata$GarageQual))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$GarageCond[is.na(mergeddata$GarageCond)] = Mode(mergeddata$GarageCond)
table(is.na(mergeddata$GarageCond))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$ PoolQC[is.na(mergeddata$ PoolQC)] = Mode(mergeddata$ PoolQC)
table(is.na(mergeddata$ PoolQC))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$Fence[is.na(mergeddata$Fence)] = Mode(mergeddata$Fence)
table(is.na(mergeddata$Fence))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$MiscFeature[is.na(mergeddata$MiscFeature)] = Mode(mergeddata$MiscFeature)
table(is.na(mergeddata$MiscFeature))

Mode = function(x) {
  uniq_x = unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mergeddata$SaleType[is.na(mergeddata$SaleType)] = Mode(mergeddata$SaleType)
table(is.na(mergeddata$SaleType))

mean_val = mean(mergeddata$GarageCars, na.rm = TRUE)
mergeddata$GarageCars[is.na(mergeddata$GarageCars)] = mean_val
table(is.na(mergeddata$GarageCars))

mean_val = mean(mergeddata$GarageYrBlt, na.rm = TRUE)
mergeddata$GarageYrBlt[is.na(mergeddata$GarageYrBlt)] = mean_val
table(is.na(mergeddata$GarageYrBlt))

mean_val = mean(mergeddata$GarageArea, na.rm = TRUE)
mergeddata$GarageArea[is.na(mergeddata$GarageArea)] = mean_val
table(is.na(mergeddata$GarageArea))

colSums(is.na(mergeddata))
str(mergeddata)

category_mapping = c(
  "RL" = 1, 
  "RM" = 2, 
  "FV" = 3, 
  "RH" = 4, 
  "C (all)" = 5
)
mergeddata$MSZoning1 = category_mapping[mergeddata$MSZoning]

category_mapping = c("Pave" = 1, "Grvl" = 2)
mergeddata$Street1 = category_mapping[mergeddata$Street]

category_mapping = c("Reg" = 1, "IR1" = 2, "IR2" = 3, "IR3" = 4)
mergeddata$LotShape1 = category_mapping[mergeddata$LotShape]

category_mapping = c("Lvl" = 1, "Bnk" = 2, "HLS" = 3, "Low" = 4)
mergeddata$LandContour1 = category_mapping[mergeddata$LandContour]

category_mapping = c("AllPub" = 1, "NoSewr" = 2, "NoSeWa" = 3, "ELO" = 4)
mergeddata$Utilities1 = category_mapping[mergeddata$Utilities]

category_mapping = c(
  "Inside" = 1, 
  "Corner" = 2, 
  "CulDSac" = 3, 
  "FR2" = 4, 
  "FR3" = 5
)
mergeddata$LotConfig1 = category_mapping[mergeddata$LotConfig]

category_mapping = c("Gtl" = 1, "Mod" = 2, "Sev" = 3)
mergeddata$LandSlope1 = category_mapping[mergeddata$LandSlope]

category_mapping = c(
  "CollgCr" = 1, 
  "Veenker" = 2, 
  "Crawfor" = 3 
)
mergeddata$Neighborhood1 = category_mapping[mergeddata$Neighborhood]

category_mapping = c(
  "Norm" = 1, 
  "Feedr" = 2 
)
mergeddata$Condition11 = category_mapping[mergeddata$Condition1]

category_mapping = c(
  "Norm" = 1, 
  "Feedr" = 2
)
mergeddata$Condition21 = category_mapping[mergeddata$Condition2]

category_mapping = c(
  "1Fam" = 1, 
  "2FmCon" = 2, 
  "Duplex" = 3, 
  "TwnhsE" = 4, 
  "Twnhs" = 5
)
mergeddata$BldgType1 = category_mapping[mergeddata$BldgType]

category_mapping = c(
  "2Story" = 1, 
  "1Story" = 2, 
  "1.5Fin" = 3, 
  "1.5Unf" = 4, 
  "SFoyer" = 5, 
  "SLvl" = 6, 
  "2.5Unf" = 7, 
  "2.5Fin" = 8
)
mergeddata$HouseStyle1 = category_mapping[mergeddata$HouseStyle]

category_mapping = c(
  "Gable" = 1, 
  "Hip" = 2, 
  "Gambrel" = 3, 
  "Mansard" = 4, 
  "Flat" = 5, 
  "Shed" = 6
)
mergeddata$RoofStyle1 = category_mapping[mergeddata$RoofStyle]

category_mapping = c(
  "CompShg" = 1, 
  "Tar&Grv" = 2, 
  "WdShake" = 3, 
  "WdShngl" = 4, 
  "ClyTile" = 5, 
  "Metal" = 6, 
  "Membran" = 7, 
  "Roll" = 8
)
mergeddata$RoofMatl1 = category_mapping[mergeddata$RoofMatl]

category_mapping = c(
  "VinylSd" = 1, 
  "MetalSd" = 2, 
  "Wd Sdng" = 3, 
  "HdBoard" = 4, 
  "BrkFace" = 5, 
  "WdShing" = 6, 
  "CemntBd" = 7, 
  "Plywood" = 8,
  "AsbShng" = 9,
  "Stucco" = 10,
  "BrkComm" = 11,
  "AsphShn" = 12,
  "Stone" = 13,
  "ImStucc" = 14
)
mergeddata$Exterior1st1 = category_mapping[mergeddata$Exterior1st]

category_mapping = c(
  "VinylSd" = 1, 
  "MetalSd" = 2, 
  "Wd Shng" = 3, 
  "HdBoard" = 4, 
  "Plywood" = 5, 
  "Wd Sdng" = 6, 
  "CmentBd" = 7, 
  "Stucco" = 8,
  "BrkFace" = 9,
  "AsbShng" = 10,
  "Brk Cmn" = 11,
  "ImStucc" = 12,
  "AsphShn" = 13,
  "Stone" = 14,
  "Other" = 15
)
mergeddata$Exterior2nd1 = category_mapping[mergeddata$Exterior2nd]

category_mapping = c(
  "BrkFace" = 1, 
  "None" = 2, 
  "Stone" = 3, 
  "BrkCmn" = 4
)
mergeddata$MasVnrType1 = category_mapping[mergeddata$MasVnrType]

category_mapping = c("Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)
mergeddata$ExterQual1 = category_mapping[mergeddata$ExterQual]

category_mapping = c("Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)
mergeddata$ExterCond1 = category_mapping[mergeddata$ExterCond]

category_mapping = c(
  "BrkTil" = 1, 
  "CBlock" = 2, 
  "PConc" = 3, 
  "Slab" = 4, 
  "Stone" = 5, 
  "Wood" = 6
)
mergeddata$Foundation1 = category_mapping[mergeddata$Foundation]

category_mapping = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "NA" = 0)
mergeddata$BsmtQual1 = category_mapping[mergeddata$BsmtQual]

category_mapping = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "NA" = 0)
mergeddata$BsmtCond1 = category_mapping[mergeddata$BsmtCond]

category_mapping = c("Gd" = 4, "Av" = 3, "Mn" = 2, "No" = 1, "NA" = 0)
mergeddata$BsmtExposure1 = category_mapping[mergeddata$BsmtExposure]

category_mapping = c(
  "GLQ" = 6, 
  "ALQ" = 5, 
  "BLQ" = 4, 
  "Rec" = 3, 
  "LwQ" = 2, 
  "Unf" = 1, 
  "NA" = 0
)
mergeddata$BsmtFinType11 = category_mapping[mergeddata$BsmtFinType1]

category_mapping = c(
  "GLQ" = 6, 
  "ALQ" = 5, 
  "BLQ" = 4, 
  "Rec" = 3, 
  "LwQ" = 2, 
  "Unf" = 1, 
  "NA" = 0
)
mergeddata$BsmtFinType21 = category_mapping[mergeddata$BsmtFinType2]

category_mapping = c("GasA" = 1, "GasW" = 2, "Grav" = 3, "Wall" = 4, "OthW" = 5, "Floor" = 6)
mergeddata$Heating1 = category_mapping[mergeddata$Heating]

category_mapping = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1)
mergeddata$HeatingQC1 = category_mapping[mergeddata$HeatingQC]

category_mapping = c("Y" = 1, "N" = 0)
mergeddata$CentralAir1 = category_mapping[mergeddata$CentralAir]

category_mapping = c("SBrkr" = 1, "FuseA" = 2, "FuseF" = 3, "FuseP" = 4, "Mix" = 5)
mergeddata$Electrical1 = category_mapping[mergeddata$Electrical]

category_mapping = c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1)
mergeddata$KitchenQual1 = category_mapping[mergeddata$KitchenQual]

category_mapping = c(
  "Typ" = 1,
  "Min1" = 2,
  "Min2" = 3,
  "Mod" = 4,
  "Maj1" = 5,
  "Maj2" = 6,
  "Sev" = 7,
  "Sal" = 8
)
mergeddata$Functional1 = category_mapping[mergeddata$Functional]




category_mapping = c(
  "Attchd" = 1,
  "Detchd" = 2,
  "BuiltIn" = 3,
  "Basment" = 4,
  "CarPort" = 5,
  "2Types" = 6
)
mergeddata$GarageType1 = category_mapping[mergeddata$GarageType]



category_mapping = c(
  "Unf" = 1,
  "RFn" = 2,
  "Fin" = 3
)
mergeddata$GarageFinish1 = category_mapping[mergeddata$GarageFinish]


category_mapping = c(
  "Ex" = 5,
  "Gd" = 4,
  "TA" = 3,
  "Fa" = 2,
  "Po" = 1,
  "NA" = 0  
)
mergeddata$GarageQual1 = category_mapping[mergeddata$GarageQual]


category_mapping = c(
  "Ex" = 5,
  "Gd" = 4,
  "TA" = 3,
  "Fa" = 2,
  "Po" = 1,
  "NA" = 0  
)
mergeddata$GarageCond1 = category_mapping[mergeddata$GarageCond]


category_mapping = c(
  "N" = 0,
  "P" = 1,
  "Y" = 2
)
mergeddata$PavedDrive1 = category_mapping[mergeddata$PavedDrive]


category_mapping = c(
  "WD" = 1,
  "CWD" = 2,
  "VWD" = 3,
  "New" = 4,
  "COD" = 5,
  "Con" = 6,
  "ConLw" = 7,
  "ConLI" = 8,
  "ConLD" = 9,
  "Oth" = 10
)
mergeddata$SaleType1 = category_mapping[mergeddata$SaleType]


category_mapping = c(
  "Normal" = 1,
  "Abnorml" = 2,
  "AdjLand" = 3,
  "Alloca" = 4,
  "Family" = 5,
  "Partial" = 6
)
mergeddata$SaleCondition1 = category_mapping[mergeddata$SaleCondition]

str(mergeddata)

colSums(is.na(mergeddata))
str(mergeddata)

mergeddata = subset(mergeddata,select = -MSZoning)
mergeddata = subset(mergeddata,select = -Street)
mergeddata= subset(mergeddata,select = -LotShape)
mergeddata= subset(mergeddata,select = -Alley)
mergeddata= subset(mergeddata,select = -LandContour)
mergeddata= subset(mergeddata,select = -Utilities)
mergeddata= subset(mergeddata,select = -LotConfig)
mergeddata= subset(mergeddata,select = -LandSlope)
mergeddata= subset(mergeddata,select = -Neighborhood)
mergeddata= subset(mergeddata,select = -Condition1)
mergeddata= subset(mergeddata,select = -Condition2)
mergeddata= subset(mergeddata,select = -BldgType)
mergeddata= subset(mergeddata,select = -HouseStyle)
mergeddata= subset(mergeddata,select = -RoofStyle)
mergeddata= subset(mergeddata,select = -FireplaceQu)
mergeddata= subset(mergeddata,select = -RoofMatl)
mergeddata= subset(mergeddata,select = -Exterior1st)
mergeddata= subset(mergeddata,select = -Exterior2nd)
mergeddata= subset(mergeddata,select = -MasVnrType)
mergeddata= subset(mergeddata,select = -ExterQual)
mergeddata= subset(mergeddata,select = -ExterCond)
mergeddata= subset(mergeddata,select = -Foundation)
mergeddata= subset(mergeddata,select = -BsmtQual)
mergeddata = subset(mergeddata,select = -BsmtCond)
mergeddata= subset(mergeddata,select = -BsmtExposure)
mergeddata = subset(mergeddata,select = -BsmtFinType1)
mergeddata= subset(mergeddata,select = -BsmtFinType2)
mergeddata= subset(mergeddata,select = -Heating)
mergeddata= subset(mergeddata,select = -HeatingQC)
mergeddata= subset(mergeddata,select = -CentralAir)
mergeddata= subset(mergeddata,select = -Electrical)
mergeddata= subset(mergeddata,select = -KitchenQual)
mergeddata= subset(mergeddata,select = -Functional)
mergeddata= subset(mergeddata,select = -GarageType)
mergeddata= subset(mergeddata,select = -GarageFinish)
mergeddata= subset(mergeddata,select = -GarageQual)
mergeddata= subset(mergeddata,select = -GarageCond)
mergeddata= subset(mergeddata,select = -PavedDrive)
mergeddata= subset(mergeddata,select = -SaleType)
mergeddata= subset(mergeddata,select = -SaleCondition)
mergeddata= subset(mergeddata, select = -PoolQC)
mergeddata= subset(mergeddata, select = -Fence)
mergeddata= subset(mergeddata, select=-MiscFeature)
mergeddata= subset(mergeddata, select=-Neighborhood1)
mergeddata= subset(mergeddata, select=-Condition11)
mergeddata= subset(mergeddata, select=-Condition21)
mergeddata= subset(mergeddata, select=-BldgType1)
mergeddata= subset(mergeddata, select=-Exterior1st1)
mergeddata= subset(mergeddata, select=-Exterior2nd1)

str(mergeddata)

colSums(is.na(mergeddata))

# Split the data into training and testing sets based on availability of SalePrice
train_data = mergeddata[!is.na(mergeddata$SalePrice), ]
test_data = mergeddata[is.na(mergeddata$SalePrice), ]
test_data = test_data[, !(names(test_data) %in% "SalePrice")]

# Tune SVM to find the best cost and gamma
set.seed(44)
tune_result = tune(svm, SalePrice ~ ., data = train_data,
                    kernel = "radial",
                    ranges = list(cost = 10^(-1:2), gamma = 10^(-2:1)))

# Get the best model parameters
best_model = tune_result$best.model

# Predict using the tuned model
svm_predictions = predict(best_model, test_data)

# Calculate RMSE
actuals = test_data$SalePrice
rmse = sqrt(mean((svm_predictions - actuals) ^ 2, na.rm = TRUE))
print(paste("RMSE:", rmse))

# Save the predictions to a CSV file
test_data$Predicted_SalePrice = svm_predictions
MySubFin = data.frame(Id = test_data$Id, SalePrice = svm_predictions)
write.csv(MySubFin, "Submission_svm.csv", row.names = FALSE)



####################### XG BOOST ############################
library(dplyr)
library(Matrix)
library(randomForest)
library(xgboost)
library(lightgbm)

# Load datasets
X <- read.csv("train.csv")
X_test <- read.csv("test.csv")

# Define target variable
y <- X$SalePrice  # Replace 'SalePrice' with the actual target column
X <- X[, !(names(X) %in% c("SalePrice"))]  # Remove target column from features

# Ensure all categorical columns are factors in both X and X_test
categorical_features <- intersect(names(X)[sapply(X, is.character)], names(X_test)[sapply(X_test, is.character)])
X[categorical_features] <- lapply(X[categorical_features], as.factor)
X_test[categorical_features] <- lapply(X_test[categorical_features], as.factor)

# Handle single-level columns in X_test
single_level_cols <- names(which(sapply(X_test, function(col) is.factor(col) && length(unique(col)) <= 1)))
X_test <- X_test[, !(names(X_test) %in% single_level_cols)]

# Convert character columns to factors
categorical_features <- names(which(sapply(X_test, is.character)))
X_test[categorical_features] <- lapply(X_test[categorical_features], as.factor)

# Replace missing values
factor_cols <- names(which(sapply(X_test, is.factor)))
for (col in factor_cols) {
  X_test[[col]][is.na(X_test[[col]])] <- "Missing"
}

numeric_cols <- names(which(sapply(X_test, is.numeric)))
for (col in numeric_cols) {
  X_test[[col]][is.na(X_test[[col]])] <- 0
}

# Recreate sparse model matrix for X and X_test using caret
library(caret)
dummies <- dummyVars("~ .", data = X, fullRank = TRUE)
X_encoded <- predict(dummies, newdata = X)
X_test_encoded <- predict(dummies, newdata = X_test)

# Convert encoded data to sparse matrices
X_matrix <- Matrix(as.matrix(X_encoded), sparse = TRUE)
X_test_matrix <- Matrix(as.matrix(X_test_encoded), sparse = TRUE)

# Train XGBoost model
xgb_train <- xgb.DMatrix(data = X_matrix, label = y)
xgb_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)
xgb_model <- xgboost(params = xgb_params, data = xgb_train, nrounds = 200, verbose = 0)

# Predict with XGBoost
xgb_predictions <- predict(xgb_model, xgb.DMatrix(data = X_test_matrix))

# Train LightGBM model
lgb_train <- lgb.Dataset(data = as.matrix(X_encoded), label = y)
lgb_params <- list(
  objective = "regression",
  metric = "rmse",
  num_leaves = 31,
  learning_rate = 0.05,
  feature_fraction = 0.8
)
lgb_model <- lightgbm(params = lgb_params, data = lgb_train, nrounds = 200, verbose = -1)

# Predict with LightGBM
lgb_predictions <- predict(lgb_model, as.matrix(X_test_encoded))

# Ensemble predictions
ensemble_predictions <- (xgb_predictions + lgb_predictions) / 2

# Ensure 'Id' column is correctly sourced from the test dataset
if ("Id" %in% names(X_test)) {
  test_ids <- X_test$Id
} else {
  stop("The test dataset does not contain an 'Id' column.")
}

# Create submission file
submission <- data.frame(
  Id = test_ids,
  SalePrice = ensemble_predictions
)

# Save submission file
write.csv(submission, "submission_ensemble.csv", row.names = FALSE)

