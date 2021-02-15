library(purrr)
library(plyr)
library(corrplot)
library(glmnet)
#METRICs FOR ML
library(ModelMetrics)
library(Metrics)
#Classification And Regression Training
library(caret)
#RANDOM FOREST
library(randomForest)
library(e1071)
#Boosted Regression
library(gbm)
library(xgboost)
#Decision Tree
library(rpart)
library(rpart.plot)
library(rattle)
#Classification and cluster
library(class)
library(cluster)
library(OneR)
library(writexl)
library(standardize)

library(ROSE)

IE582_Fall20_ProjectTrain <- read.csv("C:/Users/Ezgi/Desktop/IE 582/Proje/IE582_Fall20_ProjectTrain.csv", sep=";")
IE582_Fall20_ProjectTest <- read.csv("C:/Users/Ezgi/Desktop/IE 582/Proje/IE582_Fall20_ProjectTest.csv")


classImbalanceRaito <- table(IE582_Fall20_ProjectTrain[,ncol(IE582_Fall20_ProjectTrain)])
classImbalanceRaito


data.balanced.over<-ovun.sample(y~., data=IE582_Fall20_ProjectTrain, method="both", p=0.5, 
                                subset=options("subset")$subset,
                                na.action=options("na.action")$na.action, seed =1)

data_oversampled<-data.balanced.over$data

data_in<-data_oversampled

data_in <- data_in[,-c(37,50,52,57)]
set.seed(123) # 

Index      <- sample(1:nrow(data_in), 0.7*nrow(data_in)) 
data_train <- data_in[Index, ]  # model data
data_test  <- data_in[-Index, ]   # model test data 


folds     <- cut(seq(1,nrow(data_train)),breaks=10,labels=FALSE)
Class_data  <- cbind(folds,data_train)
dat<-Class_data 



#### RF ####

t=0
c=0

result_rf<- data.frame(total_accuracy = numeric(), firstaccuracy = numeric(), secondaccuracy = numeric(),
                       
                       ntree = numeric(), nodesize = numeric(), f = numeric(), c = numeric())

average_rf= data.frame(total_accuracy = numeric(), firstaccuracy = numeric(), secondaccuracy = numeric(),
                       
                       ntree = numeric(), nodesize=numeric(), c = numeric())

total_accuracy = numeric()
firstaccuracy = numeric()
secondaccuracy = numeric()

#start<-Sys.time()
set.seed(1218)

# m=j 

for(j in seq(from=150, to=450, by=2)){
  for(i in seq(from=1, to=5, by=1)){
    
    c=c+1
    for(f in 1:10){    
      
      index <- which(dat[,1] == f )
      train <- dat[-index, ]
      train <- train[,-1]
      test  <- dat[index, ]
      test  <- test[,-1]
      
      model  <- randomForest(formula = as.factor(y) ~., data = train, ntree = j, nodesize = i,importance = T, na.action = na.omit)
      prediction   <- predict(model,test ,type = "class")
      
      test.y <- (test[,ncol(test)])  
      
      class.pred     <- table(prediction,test.y)
      
      total_accuracy=sum(diag(class.pred))/sum(class.pred)
      firstaccuracy=class.pred[1,1]/sum(class.pred[1,])
      secondaccuracy=class.pred[2,2]/sum(class.pred[2,]) 
      
      
      t=t+1
      
      result_rf[t,1] = total_accuracy
      result_rf[t,2] = firstaccuracy
      result_rf[t,3] = secondaccuracy
      result_rf[t,4] = j
      result_rf[t,5] = i
      result_rf[t,6] = f
      result_rf[t,7] = c
      
      print(c("alive", "ntree=", j, "nodesize=", i))
      
    }  
    
    for( k in 1:c){
      
      average_rf[k,1]  = mean(result_rf[which(result_rf$c==k), ]$total_accuracy)
      average_rf[k,2]  = mean(result_rf[which(result_rf$c==k), ]$firstaccuracy)
      average_rf[k,3]  = mean(result_rf[which(result_rf$c==k), ]$secondaccuracy)
      average_rf[k,4]  = mean(result_rf[which(result_rf$c==k), ]$ntree)
      average_rf[k,5]  = mean(result_rf[which(result_rf$c==k), ]$nodesize)
      average_rf[k,6]  = mean(result_rf[which(result_rf$c==k), ]$c)}
  }
}



## RF
pointer<-which.max(average_rf$total_accuracy)
total_accuracy_validation_rf<-max(average_rf$total_accuracy)

ntree<-average_rf[pointer,"ntree"]
nodesize<-average_rf[pointer,"nodesize"]
mtry<-average_rf[pointer,"mtry"]

# We found out that the values below gave the best performance during our analysis.
ntree<-308
nodesize<-3
mtry<-5
## Testing

model_rf  <- randomForest(formula = as.factor(y) ~., data = data_train, ntree = ntree, nodesize = nodesize ,importance = T, na.action = na.omit)
pred_rf   <- as.factor(predict(model_rf,data_test ,type = "class"))
test.y <- (data_test[,ncol(data_test)])  

class.pred_rf     <- table(pred_rf,test.y)

total_accuracy_rf=sum(diag(class.pred_rf))/sum(class.pred_rf)


total_accuracy_validation_rf
total_accuracy_rf      # Just to check if everything is okay.

model_rf  <- randomForest(formula = as.factor(y) ~., data = data_oversampled, ntree = ntree, nodesize = nodesize , importance = T, na.action = na.omit)
pred_rf   <- as.matrix((predict(model_rf,IE_test ,type = "prob")))
pred<-as.matrix(pred_rf[,2])
View(pred)

# then we directly submit this pred variable into the submission platform.



