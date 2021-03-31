library(caret)
library(glmnet)
library(randomForest)
#install.packages('caret')
library(caret)
#data is loaded from excel sheet into RStudio Global environment as given_data
required_data <- given_data[-nrow(given_data),]
View(required_data)
#replacing the ? in the dataset with NA
required_data$rbc <- gsub("?",NA,required_data$rbc, fixed = TRUE)
required_data$pc <- gsub("?",NA,required_data$pc, fixed = TRUE)
required_data$pcc <- gsub("?",NA,required_data$pcc, fixed = TRUE)
required_data$ba <- gsub("?",NA,required_data$ba, fixed = TRUE)
required_data$htn <- gsub("?",NA,required_data$htn, fixed = TRUE)
required_data$dm <- gsub("?",NA,required_data$dm, fixed = TRUE)
required_data$cad <- gsub("?",NA,required_data$cad, fixed = TRUE)
required_data$appet <- gsub("?",NA,required_data$appet, fixed = TRUE)
required_data$pe <- gsub("?",NA,required_data$pe, fixed = TRUE)
required_data$ane <- gsub("?",NA,required_data$ane, fixed = TRUE)
#To find the sum of na's in each variable
sapply(required_data,function(x) sum(is.na(x)))
#To find count of unique values in each variable
sapply(required_data, function(x) length(unique(x)))
#To plot a graph between the missing values and present values
missing.values <- required_data %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)
levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot
#plot a graph across rows for missing values
row.plot <- required_data %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha=0.8) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'),
                    labels = c("Present", "Missing")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Variable",
       y = "Row Number", title = "Missing values in rows") +
  coord_flip()

row.plot
#Taking care of missing values for each variable
#replacing the NA values in continuous variables with mean
required_data$age[is.na(required_data$age)] <- mean(required_data$age,na.rm=T)
sapply(required_data, typeof)
as.integer(required_data$age)
is.factor(required_data$bp)
required_data$bp<-factor(required_data$bp)
levels(required_data$bp)
#writing the mode function
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
#replacing the NA values in categorical variable with mode
required_data$bp[is.na(required_data$bp)] <- Mode(required_data$bp,na.rm=T)
required_data$sg<-factor(required_data$sg)
is.factor(required_data$sg)
required_data$sg[is.na(required_data$sg)] <- Mode(required_data$sg,na.rm=T)
levels(required_data$sg)

required_data$al<-factor(required_data$al)
is.factor(required_data$al)
required_data$al[is.na(required_data$al)] <- Mode(required_data$al,na.rm=T)
levels(required_data$al)

required_data$al<-factor(required_data$al)
is.factor(required_data$al)
required_data$al[is.na(required_data$al)] <- Mode(required_data$al,na.rm=T)
levels(required_data$al)

required_data$su<-factor(required_data$su)
is.factor(required_data$su)
required_data$su[is.na(required_data$su)] <- Mode(required_data$su,na.rm=T)
levels(required_data$su)

cols <- c("rbc", "pc", "pcc", "ba","htn","dm","cad","appet","pe","ane","class")
required_data[cols] <- lapply(required_data[cols], factor)
is.factor(required_data$class)
levels(required_data$class)

#replacing with modes
required_data$rbc[is.na(required_data$rbc)] <- Mode(required_data$rbc,na.rm=T)
required_data$pc[is.na(required_data$pc)] <- Mode(required_data$pc,na.rm=T)
required_data$pcc[is.na(required_data$pcc)] <- Mode(required_data$pcc,na.rm=T)
required_data$ba[is.na(required_data$ba)] <- Mode(required_data$ba,na.rm=T)
required_data$htn[is.na(required_data$htn)] <- Mode(required_data$htn,na.rm=T)
required_data$dm[is.na(required_data$dm)] <- Mode(required_data$dm,na.rm=T)
required_data$cad[is.na(required_data$cad)] <- Mode(required_data$cad,na.rm=T)
required_data$appet[is.na(required_data$appet)] <- Mode(required_data$appet,na.rm=T)
required_data$pe[is.na(required_data$pe)] <- Mode(required_data$pe,na.rm=T)
required_data$ane[is.na(required_data$ane)] <- Mode(required_data$ane,na.rm=T)

#replacing with means
required_data$bgr[is.na(required_data$bgr)] <- mean(required_data$bgr,na.rm=T)
required_data$bu[is.na(required_data$bu)] <- mean(required_data$bu,na.rm=T)
required_data$sc[is.na(required_data$sc)] <- mean(required_data$sc,na.rm=T)
required_data$sod[is.na(required_data$sod)] <- mean(required_data$sod,na.rm=T)
required_data$pot[is.na(required_data$pot)] <- mean(required_data$pot,na.rm=T)
required_data$hemo[is.na(required_data$hemo)] <- mean(required_data$hemo,na.rm=T)
required_data$pcv[is.na(required_data$pcv)] <- mean(required_data$pcv,na.rm=T)
required_data$wbcc[is.na(required_data$wbcc)] <- mean(required_data$wbcc,na.rm=T)
required_data$rbcc[is.na(required_data$rbcc)] <- mean(required_data$rbcc,na.rm=T)

#converting the values in columns from double to int
intcols <- c("age","bgr","bu","sc","sod","pot","hemo","pcv","wbcc","rbcc")
required_data[intcols] <- lapply(required_data[intcols], as.integer)

str(required_data)
features = subset(required_data,select = -class)
class = required_data$class
#one hot encoding
one_hot_enc = dummyVars(~.,data = features)
one_hot_data = predict(one_hot_enc,newdata = features)
final_data = cbind(one_hot_data,as.data.frame(class))
#splitting the data into train and test
smp_size <- floor(0.80 * nrow(final_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(final_data)), size = smp_size)
train <- final_data[train_ind, ]
test <- final_data[-train_ind, ]
#Random Forest Model
rf = randomForest(class ~. , data=train,importance=T)
rf
#plotting the variable importance plot to find importance of the predictors
varImpPlot(rf,pch=10,col="red",cex=1,main="Variable Importance using randomForest")
round(importance(rf), 2)
rank(importance(rf))
importance(rf)[rank(importance(rf)),]
test$rf_pred <- predict(rf, test)
test$rf_pred <- as.factor(test$rf_pred)
#Confusion Matrix for randomForest model
confusionMatrix(test$rf_pred, test$class)
#Logistic Regression
#using glm for significant variables found from random Forest
glm_model<-glm(class~hemo+pcv+bgr+sc,family="binomial",data=required_data)
summary(glm_model)
mod_fit <- train(class~.,  data=train, method="glm", family="binomial")
pred = predict(mod_fit, newdata=test)
#Confusion Matrix for Logistic Regression
#when all the  predictors are used
confusionMatrix(data=pred, test$class)
mod_fit <- train(class~hemo+pcv+bgr+sc,  data=train, method="glm", family="binomial")
pred = predict(mod_fit, newdata=test)
#Confusion Matrix for Logistic Regression
#when significant predictors are used
confusionMatrix(data=pred, test$class)
#Support Vector Machines model
#install.packages('e1071') 
library(e1071) 
classifier = svm(formula = class ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear')  
prediction <- predict(classifier, test)
#Confusion Matrix for Support Vector Machines
confusionMatrix(prediction, test$class, dnn = c("Prediction", "Reference"))