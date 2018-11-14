
## Importing packages
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

#========================================WHAT'S ABOUT TELCO DATASET===================================
list.files(path = "../input")
# Load and view the dataset
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
View(telco)
# See the variables types
glimpse(telco)
str(telco)
# Uniforming the variables names (tenure, gender, customerID) by putting the first letter to uppercase 
telco <- rename(telco,c("tenure"= "Tenure","gender"="Gender", "customerID"="CustomerID"))
# Change the value of SeniorCitizen to "YES" or "NO" and set up in factor
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1, 'YES', 'NO'))
View(telco)

#===========================Take a look to missing data in telco dataset================================= 
# Take the percent of the count missing data in each column
missing_data <- telco %>% summarise_all(funs(sum(is.na(.))/n()))
View(missing_data)

# Transform it as a dataframe with 2 columns 
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")

# visualize the missing data percent for each variables
# Plot dimension
options(repr.plot.width = 6, repr.plot.height = 4) 
# Missing data barplot
Missingdata_barplot <- ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()
Missingdata_barplot
# Create a complete dataset without NAs
telco_full <- telco[complete.cases(telco),]
glimpse(telco_full)
str(telco_full)

#============================================== DATA ANALYSIS and VISUALIZATION =====================================================
# Take a look to the Customer Churn Rate (CCR)
options(repr.plot.width = 6, repr.plot.height = 4)
telco_full %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Customer Churn Rate")

# Visualize descriptive variables by Customer Churn Rate 
theme1 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
theme2 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")
# First plotgrid
options(repr.plot.width = 12, repr.plot.height = 8)
First_plotgrid <- plot_grid(ggplot(telco_full, aes(x=Gender,fill=Churn))+ geom_bar()+ theme1, 
          ggplot(telco_full, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco_full, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco_full, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco_full, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco_full, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")
First_plotgrid

# Second plotgrid
options(repr.plot.width = 12, repr.plot.height = 8)
Second_plotgrid <- plot_grid(ggplot(telco_full, aes(x=InternetService,fill=Churn))+ geom_bar()+ theme1, 
          ggplot(telco_full, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco_full, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco_full, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco_full, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco_full, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")
Second_plotgrid

# Third plotgrid
options(repr.plot.width = 12, repr.plot.height = 8)
Third_plotgrid <- plot_grid(ggplot(telco_full, aes(x=StreamingMovies,fill=Churn))+ geom_bar()+ theme1, 
          ggplot(telco_full, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill')+ theme1,
          ggplot(telco_full, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')+ theme1,
          ggplot(telco_full, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')+ theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")
Third_plotgrid

# Visualize continuous variables
# Tenure, MonthlyCharges, TotalCharges
options(repr.plot.width =12, repr.plot.height = 8)
plot_grid(ggplot(telco_full, aes(y= Tenure, x = Churn, fill = Churn)) + 
            geom_boxplot()+ 
            theme1+
            xlab("Churn"),
          ggplot(telco_full, aes(y= MonthlyCharges, x = Churn, fill = Churn)) + 
            geom_boxplot()+ 
            theme1+
            xlab("Churn"),
          ggplot(telco_full, aes(y= TotalCharges, x = Churn, fill = Churn)) + 
            geom_boxplot()+ 
            theme_bw()+
            xlab("Churn"))

# Checking the correlation between countinuous variables
options(repr.plot.width =6, repr.plot.height = 4)
telco_full_cor <- round(cor(telco_full[,c("Tenure", "MonthlyCharges", "TotalCharges")]), 1)
ggcorrplot(telco_full_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

# Checking outliers
# Tenure outliers
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(telco$Tenure)$out
# MonthlyCharges
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(telco$MonthlyCharges)$out
# TotalCharges
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(telco$TotalCharges)$out

#================================ DATA PREPARATION =================================
# Cleaning data
telco_full <- data.frame(lapply(telco_full, function(x) {
  gsub("No internet service", "No", x)}))

telco_full <- data.frame(lapply(telco_full, function(x) {
  gsub("No phone service", "No", x)}))


num_columns <- c("Tenure", "MonthlyCharges", "TotalCharges")
telco_full[num_columns] <- sapply(telco_full[num_columns], as.numeric)

telco_int <- telco_full[,c("Tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

telco_full <- mutate(telco_full,Tenure_bin = Tenure)

telco_full$Tenure_bin[telco_full$Tenure_bin >=0 & telco_full$Tenure_bin <= 12] <- '0-1 year'
telco_full$Tenure_bin[telco_full$Tenure_bin > 12 & telco_full$Tenure_bin <= 24] <- '1-2 years'
telco_full$Tenure_bin[telco_full$Tenure_bin > 24 & telco_full$Tenure_bin <= 36] <- '2-3 years'
telco_full$Tenure_bin[telco_full$Tenure_bin > 36 & telco_full$Tenure_bin <= 48] <- '3-4 years'
telco_full$Tenure_bin[telco_full$Tenure_bin > 48 & telco_full$Tenure_bin <= 60] <- '4-5 years'
telco_full$Tenure_bin[telco_full$Tenure_bin > 60 & telco_full$Tenure_bin <= 72] <- '5-6 years'


telco_full$Tenure_bin <- as.factor(telco_full$Tenure_bin)

ggplot(telco_full, aes(x = Tenure_bin, fill = Tenure_bin))+
  geom_bar()+ theme1

# The Tenure by the churn rate
ggplot(telco_full, aes(x = Tenure_bin, fill = Tenure_bin))+
  geom_bar()+ 
  facet_wrap(~Churn)+
  theme_bw() +
  ggtitle("Distribution of Tenure_bin by the churn rate")+
  ylab("Count")
 
telco_cat <- telco_full[,-c(1,6,19,20)]
#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))
head(dummy)

# Create the final dataset by combining the data
# Combining the data
telco_final <- cbind(telco_int,dummy)
head(telco_final)

#Splitting the data
set.seed(123)
indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
test = telco_final[!(indices),]

#=============================== Exploratory Modeling ======================================
# First Model building-logistic regression
model_1 = glm(Churn ~ ., data = train, family = "binomial")
summary(model_1)
#
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
vif(model_2)
#Removing DeviceProtection due to high p-value 
model_3 <-glm(formula = Churn ~ Tenure + MonthlyCharges + SeniorCitizen + 
                Partner + InternetService.xFiber.optic + InternetService.xNo + 
                OnlineSecurity + OnlineBackup + TechSupport + 
                StreamingTV + Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                PaymentMethod.xElectronic.check + Tenure_bin.x1.2.years + 
                Tenure_bin.x5.6.years, family = "binomial", data = train)
summary(model_3)
vif(model_3)

#Removing StreamingTV  as it has high p-value 
model_4 <- glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
                 Partner + InternetService.xFiber.optic + InternetService.xNo + 
                 OnlineSecurity + OnlineBackup + TechSupport +  
                 Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                 PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                 tenure_bin.x5.6.years, family = "binomial", data = train)
summary(model_4)
vif(model_4)

#Model_3 all has significant variables, so let's just use it for prediction first
final_model <- model_3
#Prediction
pred <- predict(final_model, type = "response", newdata = validation[,-24])
summary(pred)
validation$prob <- pred
# Using probability cutoff of 50%.
pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(validation$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, actual_churn, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

options(repr.plot.width =8, repr.plot.height =6)
summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.32, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))

#cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

cutoff_churn <- factor(ifelse(pred >=0.32, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity


# Second model building-Decision Tree
set.seed(123)
telco_final$Churn <- as.factor(telco_final$Churn)
indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]

# Training the decision tree using all variables
options(repr.plot.width = 10, repr.plot.height = 8)
library(rpart)
library(rpart.plot)
# Training 
Dtree = rpart(Churn ~., data = train, method = "class")
summary(Dtree)
# Predicting 
DTPred <- predict(Dtree,type = "class", newdata = validation[,-24])
#Checking the confusion matrix
confusionMatrix(validation$Churn, DTPred)
 
# Third model building-Random forest
library(randomForest)
set.seed(123)
telco_final$Churn <- as.factor(telco_final$Churn)

indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]
#Training the RandomForest Model
model.rf <- randomForest(Churn ~ ., data=train, proximity=FALSE,importance = FALSE,
                         ntree=500,mtry=4, do.trace=FALSE)
model.rf
#Predicting on the validation set and checking the Confusion Matrix.
testPred <- predict(model.rf, newdata=validation[,-24])
table(testPred, validation$Churn)

confusionMatrix(validation$Churn, testPred)
#Checking the variable Importance Plot
varImpPlot(model.rf)
options(repr.plot.width =10, repr.plot.height = 8)

glm.roc <- roc(response = validation$Churn, predictor = as.numeric(pred))
DT.roc <- roc(response = validation$Churn, predictor = as.numeric(DTPred))
rf.roc <- roc(response = validation$Churn, predictor = as.numeric(testPred))

plot(glm.roc,      legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
plot(DT.roc, col = "blue", add = TRUE, print.auc.y = 0.65, print.auc = TRUE)
plot(rf.roc, col = "red" , add = TRUE, print.auc.y = 0.85, print.auc = TRUE)
legend("bottom", c("Random Forest", "Decision Tree", "Logistic"),
       lty = c(1,1), lwd = c(2, 2), col = c("red", "blue", "black"), cex = 0.75)

#Summary of all models


