title: "Project_2"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

**Problem Statement:**
A company would like to identify the employees that are likely to leave the company.
The company has demographic, educational, and other information about the
employees. The data set is split into two parts: HRRetention_train.csv and
HRRetention_test.csv.


#Modules used in the code for cleaning the data and training model
```{r}
library(tidyr)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(corrplot)
library(MASS)
library(e1071)
library(rpart)
library(ada)
library(caret)
library(klaR)
```

**loading the Training dataset**
```{r}

# filename of training file
trainFile <- "HRRetention_train.csv"

#loading the file for reading
train_df <- read.csv(trainFile,na.strings=c("",".","NA"))
#train_df <- read.csv(trainFile)

# name of each columns
colnames(train_df)

```
#Checking the dimension of the training dataset
```{r}
dim(train_df)
```


#Understading the dataset by visualizing the columns and rows the datasets
```{r}
glimpse(train_df)
```


#Checking the number of rows which has na values
```{r}
cbind(
  lapply(
    lapply(train_df,is.na)
  ,sum)
)
```


#Plotting the Missing Values Gender
```{r}
train_df %>% group_by(gender)%>% summarise(n=n()) %>% ggplot(aes(gender,n,fill=gender))+
  geom_col()+ geom_text(aes(label=n),position =position_dodge(width = 1),vjust=0.25) +theme_base()
```

#Plotting the Employee education level with Na values
```{r}
train_df %>% group_by(education_level)%>% summarise(n=n()) %>% ggplot(aes(education_level,n,fill=education_level))+
 geom_col()+ geom_text(aes(label=n),position =position_dodge(width = 1),vjust=0.25) +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

#Plotting the Missing Values enrolled_university
```{r}
train_df %>% group_by(enrolled_university)%>% summarise(n=n()) %>% ggplot(aes(enrolled_university,n,fill=enrolled_university))+
 geom_col()+ geom_text(aes(label=n),position =position_dodge(width = 1),vjust=0.25) +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

#Plotting the Missing Values Company Type
```{r}
train_df %>% group_by(company_type)%>% summarise(n=n()) %>% ggplot(aes(company_type,n,fill=company_type))+
  geom_col()+ geom_text(aes(label=n),position =position_dodge(width = 1),vjust=0.25) +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

**Cleaning the training data**

While training these missing values may influence the training model,
which reduce the accuracy of the model. So removing all na values from the training dataset.

#Removing the all the rows which has na values
```{r}

#train_df <- na.omit(train_df)
```

#Imputing data for training hours which is the integer values
```{r}
train_df$training_hours <-  mean(train_df$training_hours, na.rm = TRUE)
for (i in 1:nrow(train_df)){
  if ( is.na(train_df[i,"company_size"]) == TRUE)
      train_df[i,"company_size" ] = "50-99"
  if ( is.na(train_df[i,"gender"]) == TRUE)  
      train_df[i,"gender" ] = "Male"
  if ( is.na(train_df[i,"enrolled_university"]) == TRUE)  
      train_df[i,"enrolled_university" ] = "no_enrollment"
  if ( is.na(train_df[i,"education_level"]) == TRUE)  
      train_df[i,"education_level" ] = "Graduate"
  if ( is.na(train_df[i,"major_discipline"]) == TRUE)  
      train_df[i,"major_discipline" ] = "STEM"
  if ( is.na(train_df[i,"experience"]) == TRUE)  
      train_df[i,"experience" ] = ">20"
  if ( is.na(train_df[i,"company_type"]) == TRUE)  
      train_df[i,"company_type" ] = "Pvt Ltd"
  if ( is.na(train_df[i,"last_new_job"]) == TRUE)  
      train_df[i,"last_new_job" ] = "1"
}
```

#Updated dimension of the training dataset
```{r}
dim(train_df)
```

#Plotting the Gender After removing the Na values
```{r}
train_df %>% group_by(gender)%>% summarise(n=n()) %>% ggplot(aes(gender,n,fill=gender))+
  geom_col()+ geom_text(aes(label=n),position =position_dodge(width = 1),vjust=0.25) +theme_base()
```

#Plotting the Employee education level After removing the Na values
```{r}
train_df %>% group_by(education_level)%>% summarise(n=n()) %>% ggplot(aes(education_level,n,fill=education_level))+
 geom_col()+ geom_text(aes(label=n),position =position_dodge(width = 1),vjust=0.25) +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

#Plotting the Employee enrolled_university After removing the Na values
```{r}
train_df %>% group_by(enrolled_university)%>% summarise(n=n()) %>% ggplot(aes(enrolled_university,n,fill=enrolled_university))+
 geom_col()+ geom_text(aes(label=n),position =position_dodge(width = 1),vjust=0.25) +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

#Plotting the Employee company_type After removing the Na values
```{r}
train_df %>% group_by(company_type)%>% summarise(n=n()) %>% ggplot(aes(company_type,n,fill=company_type))+
  geom_col()+ geom_text(aes(label=n),position =position_dodge(width = 1),vjust=0.25) +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

#loading the Testing dataset
``` {r test}
# filename of testing file
testFile = "HRRetention_test.csv"

#loading the file for reading
test_df = read.csv(testFile,na.strings=c("",".","NA"))
```

#Checking the dimension of the testing dataset
```{r}
dim(test_df)
```
#Understading the dataset by visualizing the columns and rows the datasets
```{r}
glimpse(test_df)
```


#Checking the number of rows which has na values
```{r}
cbind(
  lapply(
    lapply(test_df,is.na)
  ,sum)
)
```

#Imputing data for training hours which is the integer values
```{r}
test_df$training_hours <-  mean(test_df$training_hours, na.rm = TRUE)
for (i in 1:nrow(test_df)){
  if ( is.na(test_df[i,"company_size"]) == TRUE)
      test_df[i,"company_size" ] = "50-99"
  if ( is.na(test_df[i,"gender"]) == TRUE)  
      test_df[i,"gender" ] = "Male"
  if ( is.na(test_df[i,"enrolled_university"]) == TRUE)  
      test_df[i,"enrolled_university" ] = "no_enrollment"
  if ( is.na(test_df[i,"education_level"]) == TRUE)  
      test_df[i,"education_level" ] = "Graduate"
  if ( is.na(test_df[i,"major_discipline"]) == TRUE)  
      test_df[i,"major_discipline" ] = "STEM"
  if ( is.na(test_df[i,"experience"]) == TRUE)  
      test_df[i,"experience" ] = ">20"
  if ( is.na(test_df[i,"company_type"]) == TRUE)  
      test_df[i,"company_type" ] = "Pvt Ltd"
  if ( is.na(test_df[i,"last_new_job"]) == TRUE)  
      test_df[i,"last_new_job" ] = "1"
}
```

#Removing the all the rows which has na values
```{r}
#test_df <- na.omit(test_df)
```

#Updated dimension of the testing dataset
```{r}
dim(test_df)
```

#Configure the data to select the column on which training and testing need to be done.
```{r}
colnumber <- c(3,5,6,7,8,9,11,12,13)

selectedData <- train_df[,colnumber]
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(train_df$target, p=split, list=FALSE)
trainX <- selectedData[ trainIndex,]
testX <- selectedData[-trainIndex,]
trainY <- train_df[trainIndex,14]
testY  <- train_df[-trainIndex,14]

#For Final Prediction after selecting the best model for prediciton
test_data <- test_df[,colnumber]

#column used for training and testing
colnames(test_data)

```
#Naive Bayes Classifer for prediction
```{r}
learn_nb <- naiveBayes(trainX, trainY)    
pred_nb <- predict(learn_nb, testX) 
cmnb <- confusionMatrix(factor(pred_nb), factor(testY))
cmnb
```

#AdaBoost Classfier for prediction
```{r}
control <- rpart.control(cp = -1, maxdepth = 30,maxcompete = 1,xval = 10)
learn_ada <- ada(trainY~., data = trainX, test.x = trainX, test.y = trainY, type = "gentle", control = control, iter = 70)
pred_ada <- predict(learn_ada, testX)
cmada <- confusionMatrix(factor(pred_ada), factor(testY))
cmada
```

#Logestic Regression for Prediction
```{r}
train <- train_df[trainIndex,]
testX  <- train_df[-trainIndex,-14]
testY <- train_df[-trainIndex,14]
logistic_model <- glm(target ~ city_development_index + 
                        relevent_experience +    enrolled_university 
                      + education_level +  major_discipline 
                      +    experience + company_type 
                      + last_new_job + training_hours, 
                      data = train, 
                      family = "binomial")
# Predict test data based on model
pred_prob <- predict(logistic_model, 
                       testX, type = "response")
pred_reg <- ifelse(pred_prob >0.7, 1, 0)
cmlr <- confusionMatrix(factor(pred_reg), factor(testY))
cmlr
```
**Conclusion**
We have used three training models (Naive Bayes Classifier, Ada Boost Classifier, Logestic Regression) for predicting the outcomes for target (0  Not looking for job change, 1 – Looking for a job change). 

Accuracy of Naive Bayes Classifier  : 

```{r}
cmnb$overall["Accuracy"]
``` 
Accuracy of Ada Boost Classifier    : 
```{r}
cmada$overall["Accuracy"]
```
Accuracy of Logistic Regression     : 
```{r}
cmlr$overall["Accuracy"] 
```

As from the above result, I will go with the Ada Boost Classifier for predicting the outcomes for test_df dataframe.

#AdaBoost Classfier for prediction
```{r}
test_df$target <- predict(learn_ada, test_data)
cat("These many employee may leave the company : " , sum(test_df$target == 1), " out of  ", dim(test_df)[1], "Employees")
```

