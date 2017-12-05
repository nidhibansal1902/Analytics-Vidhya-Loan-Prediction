#load libraries
library(ggplot2)
library(caret)
library(catools)
library(randomForest)



train_loan<-read.csv("train.csv")
test_loan<-read.csv("test.csv")
test_loan$Loan_Status<-"NA"
str(train_loan)

str(test_loan)

full_loan<-rbind(train_loan,test_loan)

#Data exploration
colSums(train_loan=="")
colSums(is.na(train_loan))

colSums(full_loan=="")
colSums(is.na(full_loan))

full_loan$Credit_History<-as.factor(full_loan$Credit_History)

#Dependents value which are empty fill with 0
full_loan$Dependents[full_loan$Dependents==""]='0'

#Married value which are empty fill with No
full_loan$Married[full_loan$Married==""]="No"

# Self employed value which are empty fill with No
full_loan$Self_Employed[full_loan$Self_Employed==""]="No"

# to fill missing credit history values


ggplot(full_loan, aes(x=Loan_Status, fill=factor(Credit_History)))+
  geom_bar(aes(fill=factor(Credit_History))) +
  ggtitle("Loan Status vs Credit_history") +
  xlab("Loan Status") +
  ylab("Credit_history")
## where loan status is y, fill with value 1 in credit historyand where loan status is N fill with value 0
full_loan$Credit_History[full_loan$Loan_Status=="Y" & is.na(full_loan$Credit_History) ]<- "1"
full_loan$Credit_History[full_loan$Loan_Status=="N" & is.na(full_loan$Credit_History) ]<- "0"
full_loan$Credit_History[full_loan$Self_Employed=="Yes" & is.na(full_loan$Credit_History) ]<- "1"
full_loan$Credit_History[full_loan$Gender=="Female" & is.na(full_loan$Credit_History) ]<- "1"
full_loan$Credit_History[full_loan$Gender=="" & is.na(full_loan$Credit_History) ]<- "1"
full_loan$Credit_History[full_loan$Education=="Not Graduate" & is.na(full_loan$Credit_History) ]<- "1"
full_loan$Credit_History[is.na(full_loan$Loan_Status) & is.na(full_loan$Credit_History) ]<- "1"
#Exploratory Analysis
plot( full_loan$Dependents, full_loan$Gender, col=c("Red","Green"))


ggplot(full_loan[1:614,], aes(x=Dependents, fill=factor(Loan_Status)))+
  geom_bar(aes(fill=factor(Loan_Status))) +
ggtitle("Dependents vs Loan Status") +
  xlab("Dependents") +
  ylab("Loan Status")

ggplot(full_loan[1:614,], aes(x=Gender, fill=factor(Loan_Status)))+
  geom_bar(aes(fill=factor(Loan_Status))) +
  ggtitle("Gender vs Loan Status") +
  xlab("Gender") +
  ylab("Loan Status")



ggplot(full_loan[1:614,], aes(x=Education, fill=factor(Loan_Status)))+
  geom_bar(aes(fill=factor(Loan_Status))) +
  ggtitle("Education vs Loan Status") +
  xlab("Education") +
  ylab("Loan Status")



ggplot(full_loan[1:614,], aes(x=Self_Employed, fill=factor(Loan_Status)))+
  geom_bar(aes(fill=factor(Loan_Status))) +
  facet_wrap(~Education)
  ggtitle("Self_Employed vs Loan Status") +
  xlab("Self_Employed") +
  ylab("Loan Status")

  
  
  ggplot(full_loan[1:614,], aes(x=Credit_History, fill=factor(Loan_Status)))+
    geom_bar(aes(fill=factor(Loan_Status))) +
  ggtitle("Credit_History vs Loan Status") +
    xlab("Credit_History") +
    ylab("Loan Status")
  
  ## So if credit history is 0, then loan status ~ N
  
  ggplot(full_loan[1:614,], aes(x=Property_Area, fill=factor(Loan_Status)))+
    geom_bar(aes(fill=factor(Loan_Status))) +
    ggtitle("Property_Area vs Loan Status") +
    xlab("Property_Area") +
    ylab("Loan Status")
  
  ggplot(full_loan[1:614,], aes(x=LoanAmount, fill=factor(Loan_Status)))+
    geom_bar(aes(fill=factor(Loan_Status))) +
    ggtitle("LoanAmount vs Loan Status") +
    xlab("LoanAmount") +
    ylab("Loan Status")
  

full_data<- full_loan[, 2:13]  


#Predective Modeling(LOgistic)

logic<-glm(Loan_Status~., data= full_data[1:614,], family=binomial)
prdicted<- predict(logic, full_data[1:614,]) #on training data set
y_prdicted<-ifelse(prdicted>0.5,1,0)

cm_train<-table(full_data[1:614,]$Loan_Status,y_prdicted>0.5)

predict_test<-predict(logic, full_data[615:981,-13]) #on test data set
y_pred<-ifelse(predict_test>0.5,1,0)

summary(logic) #Credit history1 and propery area semi urban plays most important role

#It is 81.47 result
full_data<-droplevels(full_data)

#Random Forest
data<-full_data[1:614,]
data<-droplevels(data)

set.seed(101)
rf<- randomForest(Loan_Status~ .,data=data, ntree=500,na.action=na.exclude)
predict<-predict(rf,full_data[615:981,-12])

#82.18 % accuracy rate
# Making the Confusion Matrix
cm = table(full_data[615:981, 12], predict)



full_loan[615:981,13]<- data.frame(predict)

solution <- data.frame('Loan_ID' = test_loan$Loan_ID, 'Loan_Status'=predict)

head(solution)

# Write it to file
write.csv(solution, 'D:/Data Science/Analytics Vidya/Practice_projects/Loan Prediction/Submission.csv', row.names = F)

