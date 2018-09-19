# Loan-Data (R script for logistic regression, Decision tree, random forest/ Used Boruta package for selecting important variables)

install.packages("data.table")
install.packages("dummies")
library("data.table")
library("dummies")
setwd("C:/Users/Mahua/Desktop/KDD_PROJECT")


data <- read.csv("Loan payments data.csv",
                 strip.white = T,
                 na.strings = c("NA","NaN","","?"))

#View(data)
str(data)
data$Loan_ID<-NULL
data$due_date<-NULL
data$paid_off_time<-NULL
str(data)

data$past_due_days[is.na(data$past_due_days)] <- 0
#View(data)


#categorical read as factors:
data$loan_status <- as.factor(data$loan_status)
data$education <- as.factor(data$education)
data$Gender <- as.factor(data$Gender)
#data$terms <- as.factor(data$terms)
data$effective_date <- as.Date(data$effective_date)
summary(data)
str(data)
#View(sub_data)
##############################
#create dummies for factors:
 
 dummy_data <- dummy.data.frame(data,
                                names = c("education",
                                          "Gender"
                                         ))
 View(dummy_data)
########################Boruta
install.packages("Boruta", repos = "https://cran.r-project.org")
 library(Boruta)
 boruta <- Boruta(loan_status~., data = dummy_data, doTrace = 2)
 print(boruta)
 plot(boruta, xlab = "", xaxt = "n")
 lz<-lapply(1:ncol(boruta$ImpHistory),function(i)
   boruta$ImpHistory[is.finite(boruta$ImpHistory[,i]),i])
 names(lz) <- colnames(boruta$ImpHistory)
 Labels <- sort(sapply(lz,median))
 axis(side = 1,las=2,labels = names(Labels),
      at = 1:ncol(boruta$ImpHistory), cex.axis = 0.7)
#########################

names(dummy_data)
names(dummy_data)[9]<-paste("educationHighSchoolorBelow")
names(dummy_data)[10]<-paste("educationMasterorAbove")
dummy_data$past_due_days<-NULL
#View(dummy_data)
n = nrow(dummy_data) # n will be ther number of obs. in data
trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) # We create an index for 70% of obs. by random
train = dummy_data[trainIndex,] # We use the index to create training data
test= dummy_data[-trainIndex,] # We take the remaining 30% as the testing data
#View(train)
#######################################crossvalidation
#Randomly shuffle the data
yourData<-yourData[sample(nrow(yourData)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- yourData[testIndexes, ]
  trainData <- yourData[-testIndexes, ]
  #Use the test and train data partitions however you desire...
}
##############################################################

#######################
# Decision Tree

library(rpart)
cartfit <- rpart(loan_status ~ .,
                  data = train, method= "class")
print(cartfit)
library("rpart.plot")
rpart.plot(cartfit)

##decision tree test evaluation
library(ROCR)
library(ggplot2)
predicted_values <- predict(cartfit, test,type= "prob")[,2] 
pred <- prediction(predicted_values, test$loan_status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

summary(test$loan_status)
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="rf")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

##########################
library(randomForest)
str(train)
rf <- randomForest(loan_status ~.,data = train, ntree=80)
print(rf)

##RF test evaluation

library(ROCR)
library(ggplot2)
predicted_values <- predict(rf, test,type= "prob")[,2]
pred <- prediction(predicted_values, test$loan_status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

summary(test$loan_status)
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="rf")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

##################Logistic Regression
logit <- glm(loan_status ~.,family=binomial(link='logit'),data=train)
summary(logit)

##Logit test evaluation
library(ROCR)
library(ggplot2)
predicted_values <- predict(logit, test,type= "link")
  pred <- prediction(predicted_values, test$loan_status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

summary(test$loan_status)
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="rf")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))


# test$past_due_days[is.na(test$past_due_days)] <- 0
# test$effective_date <- NULL
# test$due_date <- NULL
# test$paid_off_time <- NULL
# test$Loan_ID <- NULL
# 
# summary(test$loan_status)
# test$loan_status <- factor(test$loan_status)
# 
# library(plyr)
# revalue(test$loan_status, c("COLLECTION_PAIDOFF"="PAIDOFFF"))
# 
# 
# 
# library(caret)
# predicted_values <- predict(rf, test,type= "prob")
# head(predicted_values)
# 
# threshold <- 0.60
# pred <- factor( ifelse(predicted_values[,2] > threshold,"Collection", "Paidoff") )
# 
# confusionMatrix(pred, test_data$loan_status, 
#                 positive = levels(test_data$loan_status)[2])
# #install.packages("ROCR")
# 
# 
# ######################################
#Associasion rule mining
# ########################################
data$loan_status <- as.factor(data$loan_status)
data$education <- as.factor(data$education)
data$Gender <- as.factor(data$Gender)

header <- c("education", "Gender", "loan_status")
subset<- data[,header]
str(subset)
library(arules)
# find association rules with default settings
rules <- apriori(subset)
inspect(rules)

############change loan_status as rhs

rules <- apriori(subset,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.3),
                 appearance = list(rhs=c("loan_status=1",
                                          "loan_status=0"),
                                    default="lhs"))
                 


## order rules by lift

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
library(arulesViz)
plot(rules)



############################
boxplot(dummy_data$ag)
dummy_data <-  dummy_data[!dummy_data$age > 48,]
dummy_data$agegrp <- 0

dummy_data$agegrp[dummy_data$age>=40 & dummy_data$age<=48] <- 3
dummy_data$agegrp[dummy_data$age>=29 & dummy_data$age<=39] <- 2
dummy_data$agegrp[dummy_data$age>=18 & dummy_data$age<=28] <- 1
dummy_data$agegrp <- as.factor(dummy_data$agegrp)
View(dummy_data$agegrp)
tab <- table(dummy_data$agegrp)
barplot(tab, height=, ylab="Count", xlab="age")  

