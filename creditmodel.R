libr##Credit Card Default Model

suppressWarnings(suppressMessages(library(rpart)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(rpart)))
suppressWarnings(suppressMessages(library(caret)))

datafile<-"credit-data.csv"

##Proceed only if the file exists
if (!file.exists(datafile)){
  error()
  
}
##Read the file
data <-read.table(datafile, header=TRUE, sep=",", na.strings=c("NA","#DIV/0!",""))
print(dim(data))
print(names(data))

features <- subset(data, 
                   select=-c(ID))
features <-mutate(features, IS_MALE=ifelse (SEX==1, TRUE, FALSE),IS_MARRIED=ifelse (MARRIAGE==1, TRUE, FALSE),
                  HS_GRADUATED=ifelse (EDUCATION>3, TRUE, FALSE),
                  TOT_PAY_STATUS=PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6,
                  TOT_BILL_AMT=BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6,
                  TOT_PAY_AMT=PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6)

features <- subset(features, 
                   select=c(LIMIT_BAL, AGE, IS_MALE, IS_MARRIED, HS_GRADUATED,
                            TOT_PAY_STATUS, TOT_BILL_AMT, TOT_PAY_AMT, default.payment.next.month))

print(head(features, 10))

##scale <- function(x){(x-min(x))/(max(x)-min(x))}

##features_scaled<-mutate(features, scale(LIMIT_BAL), scale(AGE), scale(IS_MALE), scale(IS_MARRIED), scale(HS_GRADUATED),
                        ##scale(TOT_PAY_STATUS), scale(TOT_BILL_AMT), scale(TOT_PAY_AMT))


features_train<-head(features,20000)
features_validate<-features[20000:25000,]
features_test<-tail(features,5000)

##Linear Regression Model
lrModel <- lm(default.payment.next.month ~ ., data=features_train)
summary(lrModel) # show results
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lrModel)

##Create the classification tree model
set.seed(12345)
rpartModel<-rpart(formula = default.payment.next.month ~ ., data = features_train, method="class") 
summary(rpartModel)


##Predict using the tesing set
predictionsrpart <- predict(rpartModel, newdata=features_test, type="class")
rpartConfMatrix<-confusionMatrix(predictionsrpart, features_test$default.payment.next.month)
print(rpartConfMatrix)

  