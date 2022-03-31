library(readr)
library(caret)
library(gains)
bank.df <- read_csv("UniversalBank.csv")
bank.df$Education<-factor(bank.df$Education,levels=c(1,2,3),
                          labels=c("Undergrad","Graduate","Advanced/Professional"))

#split data
View(bank.df)

set.seed(1)

#randomize data
train.index<-sample(c(1:dim(bank.df)[1]),dim(bank.df)[1]*0.6)


train.df<-bank.df[train.index,]

valid.df<-bank.df[-train.index,]

#Removing ID columns and ZIP code 

train.df<-subset(train.df,select=-c(ID))
test.df<-subset(valid.df,select=-c(ID))
train.df<-subset(train.df,select=-c(`ZIP Code`))
valid.df<-subset(valid.df,select=-c(`ZIP Code`))


rg<-glm(`Personal Loan`~.,data=train.df,family="binomial")

options(scipen = 999)
print(summary(rg))


prediction<-predict(rg,valid.df[,-8],type="response")

print(data.frame(actual=valid.df$`Personal Loan`[1:5],predicted=prediction[1:5]))


