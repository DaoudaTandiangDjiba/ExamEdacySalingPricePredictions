#
# Import packages 
library(plyr)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)
library(reshape)
library(scales)
library(xgboost)
library(gbm)
library(randomForest)

# Load the train and test dataset
train <- read.csv("train.csv", header=TRUE, stringsAsFactors = FALSE)
test <- read.csv("test.csv",header=TRUE, stringsAsFactors = FALSE)

# Check the variables
View(train)
View(test)
str(train)
str(test)
names(train)
names(test)

# Add a "SalePrice" variable to the test set to allow for combining data sets 
summary(train$SalePrice)
test.SalePrice <- data.frame(SalePrice = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.SalePrice)
data.combined$SalePrice <- as.integer(data.combined$SalePrice) 
dim(data.combined)
summary(data.combined$SalePrice)

# Take a look to the saling price
summary(data.combined$SalePrice)
ggplot(data.combined[!is.na(data.combined$SalePrice),], aes(x = SalePrice)) +
  geom_histogram(binwidth = 10000) +
  xlab("SalePrice") +
  ylab("Total Count") + 
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
# Check outliers
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(data.combined$SalePrice)$out
scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

# Missing data
missing_data <- data.combined %>% summarise_all(funs(sum(is.na(.))/n()))
View(missing_data)
# Transform it as a dataframe with 2 columns 
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
# visualize the missing data percent for each variables
# Plot dimension
options(repr.plot.width = 12, repr.plot.height = 8)
# Missing data barplot
Missingdata_barplot <- ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.1)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()
Missingdata_barplot

nacolumn <- which(colSums(is.na(data.combined)) > 0)
sort(colSums(sapply(data.combined[nacolumn], is.na)), decreasing = TRUE)
names(nacolumn)
# 34 variables with NAs, NAs in SalePrice is from the test set.
# Imputing missing values
# Only Fence variable and PoolQC are needed for our Work
table(data.combined$Fence)
# The NAs value on the Fence means None
data.combined$Fence[is.na(data.combined$Fence)] <- "No Fence" 
#The NAs value on the PoolQC means None
table(data.combined$PoolQC)
data.combined$PoolQC[is.na(data.combined$PoolQC)] <- "No Pool" 


#======================================New Dataset============================================================= 
data.combined_full <- data.combined[c(18,20,29,53,62,73,74,52,80,81)]
str(data.combined_full)

data.combined_full$ExterCond <- as.factor(data.combined_full$ExterCond)
data.combined_full$PoolQC <- as.factor(data.combined_full$PoolQC)
data.combined_full$Fence <- as.factor(data.combined_full$Fence)
data.combined_full$SaleCondition <- as.factor(data.combined_full$SaleCondition)

#====================================Distribution of variables=================================================
# Distribution of the Bedroom variable
ggplot(data.combined_full, aes(x = BedroomAbvGr )) +
  geom_bar() +
  xlab("Bedroom") +
  ylab("Total Count") +
  theme_bw()

# Distribution of the Fence variable
ggplot(data.combined_full, aes(x = Fence)) +
  geom_bar(fill = 'Blue') +
  xlab("Fence") +
  ylab("Total Count")+
  theme_bw()

# Distribution of the Sale condition varibale
ggplot(data.combined_full, aes(x = SaleCondition )) +
  geom_bar() +
  xlab("Sale condition") +
  ylab("Total Count") +
  theme_bw()

# Distribution of the Extern condition variable
ggplot(data.combined_full, aes(x = ExterCond )) +
  geom_bar() +
  xlab("Extern condition") +
  ylab("Total Count") +
  theme_bw()

# Distribution of the Pool quality variable
ggplot(data.combined_full, aes(x = PoolQC )) +
  geom_bar() +
  xlab("Pool quality") +
  ylab("Total Count") +
  theme_bw()
# =======================================Correlation with the SalePrice Variable====================================
options(repr.plot.width =6, repr.plot.height = 4)
data.combined_full$BedroomAbvGr <- as.integer(data.combined_full$BedroomAbvGr)
correlation <- round(cor(data.combined_full[1:1460,][,c("YearBuilt","OverallQual","KitchenAbvGr", "GarageCars","BedroomAbvGr","SalePrice")]), 1)

ggcorrplot(correlation,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

# Feature engineering
data.combined_full$SalePrice2[data.combined_full$SalePrice >=34900 & data.combined_full$SalePrice <=129975 ] <- 'Cheap'
data.combined_full$SalePrice2[data.combined_full$SalePrice > 129975 & data.combined_full$SalePrice <= 180921] <- 'Medium'
data.combined_full$SalePrice2[data.combined_full$SalePrice > 180921 & data.combined_full$SalePrice <= 755000] <- 'Expensive'
data.combined_full$SalePrice2 <- as.factor(data.combined_full$SalePrice2)

# Distribution of factor variables by Saleprice
ggplot(data.combined_full[1:1460,], aes(x = PoolQC, fill = SalePrice2)) +
geom_bar() +
  facet_grid(~SalePrice2)+
  xlab("Pool quality") +
  ylab("Total Count") +
  theme_bw()

ggplot(data.combined_full[1:1460,], aes(x = BedroomAbvGr, fill = SalePrice2 )) +
  geom_bar() +
  facet_grid(~SalePrice2)+
  xlab("Pool quality") +
  ylab("Total Count") +
  theme_bw()

ggplot(data.combined_full[1:1460,], aes(x = Fence, fill = SalePrice2 )) +
  geom_bar() +
  facet_grid(~SalePrice2)+
  xlab("Fence") +
  ylab("Total Count") +
  theme_bw()

ggplot(data.combined_full[1:1460,], aes(x = SaleCondition, fill = SalePrice2 )) +
  geom_bar() +
  facet_grid(~SalePrice2)+
  xlab("Fence") +
  ylab("Total Count") +
  theme_bw()

ggplot(data.combined_full[1:1460,], aes(x = ExterCond, fill = SalePrice2 )) +
  geom_bar() +
  facet_grid(~SalePrice2)+
  xlab("Extern condition") +
  ylab("Total Count") +
  theme_bw()

# Data Preparation
data.combined_full$SalePrice[is.na(data.combined$SalePrice)] <- "None" 

# ====================================Exploratory modeling==============================================
# Fisrt Model: Random Forest
set.seed(2018)
data.combined_full$SalePrice <- as.factor(data.combined_full$SalePrice)
RFModel <- randomForest(formula = SalePrice ~ OverallQual+YearBuilt+ExterCond+KitchenAbvGr+
                          GarageCars+PoolQC+Fence+BedroomAbvGr+SaleCondition, 
             data = data.combined_full[0:1461,],
             ntree=500)

Prediction <- predict(RFModel, data.combined_full[1461:2919,])

Prediction2 <- Prediction
PreComp_RF <- Prediction2
Compl_testRF <- data.combined_full$SalePrice[1461:2919,]
(M_RF_test <- as.matrix(table(Compl_testRF, PreComp_RF)))

perf(Test_set$Cible, Test_set$Pred.rf)

# Second Model: Gradient Boosting
gbm.fit <- gbm(
  formula = SalePrice ~ OverallQual+YearBuilt+ExterCond+KitchenAbvGr+
             GarageCars+PoolQC+Fence+BedroomAbvGr+SaleCondition,
  distribution = "gaussian",
  data = data.combined_full[1:1460,],
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 





formula("y ~ a+b+c")
model.rf <- randomForest(formula(SalePrice ~ OverallQual+YearBuilt+ExterCond+KitchenAbvGr+GarageCars+PoolQC+Fence+
                           BedroomAbvGr+SaleCondition, data=data.combined_full[1:1460,],
                      ntree=150))



#Splitting the data
set.seed(123)
indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
test = telco_final[!(indices),]












# Take a look to the saling price
summary(data.combined$SalePrice)
ggplot(data.combined[!is.na(data.combined$SalePrice),], aes(x = SalePrice)) +
  geom_histogram(binwidth = 10000) +
  xlab("SalePrice") +
  ylab("Total Count") + 
scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
# Check outliers
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(data.combined$SalePrice)$out
scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)









  