rm(list=ls(all=T))
#SETTING THE WORKING DIRECTORY
setwd("D:/Project_R/Inputfiles")
getwd()

#Loading Libraries in R
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

install.packages(x)
lapply("unbalanced", require, character.only = TRUE)
rm(x)


## Read the data
santander_data_train = read.csv("train.csv", header = T, na.strings = c(" ", "", "NA"))

#Get the names of the Variables
names('santander_data_train')

#MISSING VALUE ANALYSIS
#------------------------------------------------------------------------------------------------------------------------------
#CREATE A DATAFRAME WITH MISSING PERCENTAGE
missing_values = data.frame(apply(santander_data_train,2,function(x){sum(is.na(x))}))

#CONVERT THE ROW INTO COLUMNS
missing_values$Columns = row.names(missing_values)

#RENAME THE COLUMN AS MISSING_PERCENTAGE
names(missing_valUes)[1] =  "Missing_percentage"

#CALUCLATING THE MISSING PERCENTAGES
missing_values$Missing_percentage = (missing_val$Missing_percentage/nrow(santander_data_train)) * 100

#WRITING THE MISSING VALUES INTO A FILE Missing_percentages.csv
write.csv(missing_values, "Missing_percentages.csv", row.names = F)

#Plot the missing values using bargraph
ggplot(data = missing_values[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()

#we can observe that there are no missing values in the dataset


#-----------------------------------------EXPLORATORY DATA ABALYSIS ----------------------------------------------------------------------
#OUTLIER ANALYSIS
#CHECK FOR THE OUTLIERS USING BOX-PLOT METHOD
#WE CAN REMOVE OUTLIER VALUES 
  
  
#sTORE THE NUMERIC VARIBALES IN ONE ARRAY
numeric_index = sapply(santander_data_train,is.numeric)

numeric_data = santander_data_train[,numeric_index]

cnames = colnames(numeric_data)

#plotting box plot for the variables
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "target"), data = subset(santander_data_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="target")+
           ggtitle(paste("Box plot of target for",cnames[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)

#Remove outliers using boxplot method
 df = santander_data_train
 #santander_data_train = df
 
val = santander_data_train$previous[santander_data_train$previous %in% boxplot.stats(santander_data_train$previous)$out]
# 
santander_data_train = santander_data_train[which(!santander_data_train$previous %in% val),]
#                                   
#loop to remove from all variables
 for(i in cnames){   print(i)
   val = trans[,i][santander_data_train[,i] %in% boxplot.stats(santander_data_train[,i])$out]
   #print(length(val))
   santander_data_train = santander_data_train[which(!santander_data_train[,i] %in% val),]
 }
#AFTER REMOVING OUTLIER WE GET 175073 OBSERVATION FROM 200000 OBSERVATIONS SO 24927 OBSERVATION ARE EXIST AS AN OUTLIER IN TRANS DATASET¶
------------------------------------------------Feature Selection------------------------------------------------------
## Correlation Plot 

#DATA SELECTION
#FOR DATA SELECTION WE HAVE SHOWN EARLIER THAT NO VARIABLE IS DEPENDENT WITH EACH OTHER CORRELATION VALUE IS COMES OUT TO BE 1
#WE HAVE FETCH OUT INITIAL TO 10 VARIABLES TO GET HEAT MAP TO GET FOW CORELATION FORM BUT IT SHOWS NO TWO ARE DEPENDENT ON EACH OTHER 

corrgram(trans[,numeric_index], order = F,upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#------------------------------------------------FeatureScaling--------------------------------------------------------
#FeatureScaling will be done based on the model

#-------------------------------------------------MODEL DEVELOPMENT--------------------------------------------------------
#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(santander_data_train$target, p = .80, list = FALSE)
train = santander_data_train[train.index,]
test  = santander_data_train[-train.index,]

########Logistic Regression###########
logit_model = glm(target ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "target")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model
ConfMatrix_RF = table(test$target, logit_Predictions)

#False Negative rate
FNR = FN/FN+TP 

#recall
Recall=(TP*100)/(TP+FN) 

#precision
precision= (TP*100)/(TP+FP))

#LOGISTIC REGRESSION RESULTS
recall = 26.34356068008599
precision = 69.05737704918033
AUC =62.53%
Accuracy =92%
#------------------------------------------------------------------------------------------------------------------
######DECISIONTREE CLASSIFICATION######
#Clean the environment
rmExcept("santander_data_train")

#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(santander_data_train$target, p = .80, list = FALSE)
train = santander_data_train[train.index,]
test  = santander_data_train[-train.index,]

##Decision tree for classification
#Develop Model on training data
C50_model = C5.0(responded ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-202], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$responded, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#recall
Recall=(TP*100)/(TP+FN) 

#precision
precision= (TP*100)/(TP+FP))

#DECISION TREE RESULTS
recall = 19.53488372093023%
precision = 19.174955160606554%
AUC =54.89%
accuracy =84%¶
#------------------------------------NAIVE BAYES MODEL-----------------------------------
library(e1071)

#Develop model
NB_model = naiveBayes(target ~ ., data = train)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,2:202], type = 'class')

#Look at confusion matrix
Conf_matrix = table(observed = test[,202], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)

#NAIVE BAYES RESULTS
recall = 35.86085597029509%
precision = 72.04554377699255%
AUC =67.08%
Accuracy =92%¶

#statical way
mean(NB_Predictions == test$target)

#------------------------------RANDOMFOREST PREDICTION--------------------------------------------------------------
RF_model = randomForest(target ~ ., train, importance = TRUE, ntree = 500)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
 treeList = RF2List(RF_model)  
 
#Extract rules
 exec = extractRules(treeList, train[,-202])  # R-executable conditions
 
#Visualize some rules
 exec[1:2,]

#Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
 
#Get rule metrics
 ruleMetric = getRuleMetric(exec, train[,-17], train$target)  # get rule metrics
 
#evaulate few rules
 ruleMetric[1:2,]

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-17])

#Evaluate the performance of classification model
ConfMatrix_RF = table(test$target, RF_Predictions)
confusionMatrix(ConfMatrix_RF)

#recall
Recall=(TP*100)/(TP+FN) 

#precision
precision= (TP*100)/(TP+FP))

#RANDOM FOREST RESULTS
recall = 0.019542700801250732
precision = 100.0
AUC =50%
accuracy =90%
#------------------------------------------------------SUMMARY-------------------------------------------------------------------
LOGISTIC REGRESSION:Results
recall = 26.34356068008599
precision = 69.05737704918033
AUC =62.53%
accuracy =92%

DECISION TREES:Results
recall = 19.53488372093023%
precision = 19.174955160606554%
AUC =54.89%
accuracy =84%

NAIVE BAYES:Results
recall =  35.86085597029509%
precision =  72.04554377699255%
AUC =67.08%
accuracy =92%

RANDOM FORREST:Results
n_estimators=100
recall = 0.019542700801250732
precision = 100.0
AUC =50%
accuracy =90%

# PRECISION CRITERION:
   # * RF > NAÏVE BAYES > LOGISTIC REGRESSION > DECISION TREE

# RECALL CRITERION AS FOLLOW:
   # * NAÏVE BAYES > LOGISTIC REGRESSION > DECISION TREE > RF

# AUC CRITERION AS FOLLOW:
   # * NAÏVE BAYES > LOGISTIC REGRESSION > DECISION TREE > RF

#* For the most Accurate Model the value of AUC must be high.

#* According to the question we have to predict the result based on recall , precision and AUC. 

#* According to the results which were obtained using all four machine learning algorithm we can infer that "Naive Bayes" is giving all the three parameters equally good. In Random Forest precision is high but recal is very low.    

#* So the accurate Model which can be selected from the results for this Santander problem is Naive Bayes.




