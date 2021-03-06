library(ie2misc)
library(dplyr)
library(readr)
require(utils)
library(lubridate)
library(glmnet)
library(CVXR)

hw3ConsData<-read_csv("GercekZamanliTuketim-01012016-01122020.csv")

## PART A

indexPartA<-which(hw3ConsData$Tarih=="01.11.2020") # Start Data


temp_lag168<-hw3ConsData[(indexPartA[1] - 168):(nrow(hw3ConsData)-168),]
dataForPartA_lag168 <- cbind(hw3ConsData[(indexPartA[1]):(nrow(hw3ConsData)),],temp_lag168)

temp_lag48<-hw3ConsData[(indexPartA[1] - 48):(nrow(hw3ConsData)-48),]
dataForPartA_lag48 <- cbind(hw3ConsData[(indexPartA[1]):(nrow(hw3ConsData)),],temp_lag48)

mape_lag168 <- mape(as.numeric(dataForPartA_lag168[,6]) , dataForPartA_lag168[,3])
summary(mape_lag168)
mape_lag48 <- mape(dataForPartA_lag48[,6] , dataForPartA_lag48[,3])
summary(mape_lag48)


## PART B

startB<-cbind(dataForPartA_lag48,dataForPartA_lag168[,6])
startB<-startB[,c(3,6,7)]
colnames(startB)<-c("A", "B","C")

dataForPartB <- cbind(hw3ConsData[169:(indexPartA[1]-1),],hw3ConsData[121:(indexPartA[1]-49),],hw3ConsData[1:(indexPartA[1]-169),])

dataForPartB<-dataForPartB[-which(dataForPartB[,1]=="27.03.2016"),]
dataForPartB<-dataForPartB[-which(dataForPartB[,4]=="27.03.2016"),]
dataForPartB<-dataForPartB[-which(dataForPartB[,7]=="27.03.2016"),]

dataForPartB<-dataForPartB[,c(1,2,3,6,9)]

colnames(dataForPartB)<-c("Date", "Hour", "Data Real", "Data lag_48","Data lag_168")

#linearFit_lag48 <- lm(dataForPartB$`Data Real`~ dataForPartB$`Data lag_48`,dataForPartB)
#summary(linearFit_lag48)


linearFit_bothLagsPartB <- lm(`Data Real`~.,data=dataForPartB[,c(3,4,5)])
summary(linearFit_bothLagsPartB)  



testB<- as.data.frame(cbind(startB$B,startB$C))
colnames(testB)<-c("Data lag_48","Data lag_168")
estimatePartB<-(predict(linearFit_bothLagsPartB, newdata = testB))

b<-data.frame(dataForPartA_lag168[,3])


ww<-cbind(estimatePartB,b)
mapePartB<- mape(ww[,1],ww[,2])
summary(mapePartB)     

## PART C

elements <- matrix(0,(nrow(dataForPartB)/24),24)

# indexleri veriyor
for (i in 1:24)
{
  elements[,i]<-which((as.numeric(dataForPartB$Hour)/3600)==(i-1))
}

combinedDataForPartC_0h<-dataForPartB[elements[,1],] # 
linearFit_bothLags_0h <- lm(`Data Real`~.,data=combinedDataForPartC_0h[,c(3,4,5)])  
summary(linearFit_bothLags_0h) 

test_0h<-dataForPartB[seq(1,nrow(dataForPartB)-23, by = 24),c(4,5)]
estimatePartc_0h<-(predict(linearFit_bothLags_0h, newdata = test_0h))

tempC_0h <- as.data.frame(cbind(estimatePartc_0h,combinedDataForPartC_0h[,3]))
mapePartC_0h <- mape(tempC_0h[,1],tempC_0h[,2])
summary(mapePartC_0h)


combinedDataForPartC_1h<-dataForPartB[elements[,2],]
linearFit_bothLags_1h <- lm(`Data Real`~.,data=combinedDataForPartC_1h[,c(3,4,5)])  
summary(linearFit_bothLags_1h) 
test_1h<- dataForPartB[seq(2,nrow(dataForPartB)-22, by = 24),]
estimatePartc_1h<-(predict(linearFit_bothLags_1h, newdata = test_1h))
tempC_1h <- as.data.frame(cbind(estimatePartc_1h,combinedDataForPartC_1h[,3]))
mapePartC_1h <- mape(tempC_1h[,1],tempC_1h[,2])
summary(mapePartC_1h)


combinedDataForPartC_2h<-dataForPartB[elements[,3],]
linearFit_bothLags_2h <- lm(`Data Real`~.,data=combinedDataForPartC_2h[,c(3,4,5)])  
summary(linearFit_bothLags_2h) 
test_2h<- dataForPartB[seq(3,nrow(dataForPartB)-21, by = 24),]
estimatePartc_2h<-(predict(linearFit_bothLags_2h, newdata = test_2h))
tempC_2h <- as.data.frame(cbind(estimatePartc_2h,combinedDataForPartC_2h[,3]))
mapePartC_2h <- mape(tempC_2h[,1],tempC_2h[,2])
summary(mapePartC_2h)

combinedDataForPartC_3h<-dataForPartB[elements[,4],]
linearFit_bothLags_3h <- lm(`Data Real`~.,data=combinedDataForPartC_3h[,c(3,4,5)])  
summary(linearFit_bothLags_3h) 
test_3h<- dataForPartB[seq(4,nrow(dataForPartB)-20, by = 24),]
estimatePartc_3h<-(predict(linearFit_bothLags_3h, newdata = test_3h))
tempC_3h <- as.data.frame(cbind(estimatePartc_3h,combinedDataForPartC_3h[,3]))
mapePartC_3h <- mape(tempC_3h[,1],tempC_3h[,2])
summary(mapePartC_3h)

combinedDataForPartC_4h<-dataForPartB[elements[,5],]
linearFit_bothLags_4h <- lm(`Data Real`~.,data=combinedDataForPartC_4h[,c(3,4,5)])  
summary(linearFit_bothLags_4h) 
test_4h<- dataForPartB[seq(5,nrow(dataForPartB)-19, by = 24),]
estimatePartc_4h<-(predict(linearFit_bothLags_4h, newdata = test_4h))
tempC_4h <- as.data.frame(cbind(estimatePartc_4h,combinedDataForPartC_4h[,3]))
mapePartC_4h <- mape(tempC_4h[,1],tempC_4h[,2])
summary(mapePartC_4h)


combinedDataForPartC_5h<-dataForPartB[elements[,6],]
linearFit_bothLags_5h <- lm(`Data Real`~.,data=combinedDataForPartC_5h[,c(3,4,5)])  
summary(linearFit_bothLags_5h) 
test_5h<- dataForPartB[seq(6,nrow(dataForPartB)-18, by = 24),]
estimatePartc_5h<-(predict(linearFit_bothLags_5h, newdata = test_5h))
tempC_5h <- as.data.frame(cbind(estimatePartc_5h,combinedDataForPartC_5h[,3]))
mapePartC_5h <- mape(tempC_5h[,1],tempC_5h[,2])
summary(mapePartC_5h)

combinedDataForPartC_6h<-dataForPartB[elements[,7],]
linearFit_bothLags_6h <- lm(`Data Real`~.,data=combinedDataForPartC_6h[,c(3,4,5)])  
summary(linearFit_bothLags_6h) 
test_6h<- dataForPartB[seq(7,nrow(dataForPartB)-17, by = 24),]
estimatePartc_6h<-(predict(linearFit_bothLags_6h, newdata = test_6h))
tempC_6h <- as.data.frame(cbind(estimatePartc_6h,combinedDataForPartC_6h[,3]))
mapePartC_6h <- mape(tempC_6h[,1],tempC_6h[,2])
summary(mapePartC_6h)

combinedDataForPartC_7h<-dataForPartB[elements[,8],]
linearFit_bothLags_7h <- lm(`Data Real`~.,data=combinedDataForPartC_7h[,c(3,4,5)])  
summary(linearFit_bothLags_7h) 
test_7h<- dataForPartB[seq(8,nrow(dataForPartB)-16, by = 24),]
estimatePartc_7h<-(predict(linearFit_bothLags_7h, newdata = test_7h))
tempC_7h <- as.data.frame(cbind(estimatePartc_7h,combinedDataForPartC_7h[,3]))
mapePartC_7h <- mape(tempC_7h[,1],tempC_7h[,2])
summary(mapePartC_7h)

combinedDataForPartC_8h<-dataForPartB[elements[,9],]
linearFit_bothLags_8h <- lm(`Data Real`~.,data=combinedDataForPartC_8h[,c(3,4,5)])  
summary(linearFit_bothLags_8h) 
test_8h<- dataForPartB[seq(9,nrow(dataForPartB)-15, by = 24),]
estimatePartc_8h<-(predict(linearFit_bothLags_8h, newdata = test_8h))
tempC_8h <- as.data.frame(cbind(estimatePartc_8h,combinedDataForPartC_8h[,3]))
mapePartC_8h <- mape(tempC_8h[,1],tempC_8h[,2])
summary(mapePartC_8h)

combinedDataForPartC_9h<-dataForPartB[elements[,10],]
linearFit_bothLags_9h <- lm(`Data Real`~.,data=combinedDataForPartC_9h[,c(3,4,5)])  
summary(linearFit_bothLags_9h) 
test_9h<- dataForPartB[seq(10,nrow(dataForPartB)-14, by = 24),]
estimatePartc_9h<-(predict(linearFit_bothLags_9h, newdata = test_9h))
tempC_9h <- as.data.frame(cbind(estimatePartc_9h,combinedDataForPartC_9h[,3]))
mapePartC_9h <- mape(tempC_9h[,1],tempC_9h[,2])
summary(mapePartC_9h)

combinedDataForPartC_10h<-dataForPartB[elements[,11],]
linearFit_bothLags_10h <- lm(`Data Real`~.,data=combinedDataForPartC_10h[,c(3,4,5)])  
summary(linearFit_bothLags_10h) 
test_10h<- dataForPartB[seq(11,nrow(dataForPartB)-13, by = 24),]
estimatePartc_10h<-(predict(linearFit_bothLags_10h, newdata = test_10h))
tempC_10h <- as.data.frame(cbind(estimatePartc_10h,combinedDataForPartC_10h[,3]))
mapePartC_10h <- mape(tempC_10h[,1],tempC_10h[,2])
summary(mapePartC_10h)

combinedDataForPartC_11h<-dataForPartB[elements[,12],]
linearFit_bothLags_11h <- lm(`Data Real`~.,data=combinedDataForPartC_11h[,c(3,4,5)])  
summary(linearFit_bothLags_11h) 
test_11h<- dataForPartB[seq(12,nrow(dataForPartB)-12, by = 24),]
estimatePartc_11h<-(predict(linearFit_bothLags_11h, newdata = test_11h))
tempC_11h <- as.data.frame(cbind(estimatePartc_11h,combinedDataForPartC_11h[,3]))
mapePartC_11h <- mape(tempC_11h[,1],tempC_11h[,2])
summary(mapePartC_11h)

combinedDataForPartC_12h<-dataForPartB[elements[,13],]
linearFit_bothLags_12h <- lm(`Data Real`~.,data=combinedDataForPartC_12h[,c(3,4,5)])  
summary(linearFit_bothLags_12h) 
test_12h<- dataForPartB[seq(13,nrow(dataForPartB)-11, by = 24),]
estimatePartc_12h<-(predict(linearFit_bothLags_12h, newdata = test_12h))
tempC_12h <- as.data.frame(cbind(estimatePartc_12h,combinedDataForPartC_12h[,3]))
mapePartC_12h <- mape(tempC_12h[,1],tempC_12h[,2])
summary(mapePartC_12h)

combinedDataForPartC_13h<-dataForPartB[elements[,14],]
linearFit_bothLags_13h <- lm(`Data Real`~.,data=combinedDataForPartC_13h[,c(3,4,5)])  
summary(linearFit_bothLags_13h) 
test_13h<- dataForPartB[seq(14,nrow(dataForPartB)-10, by = 24),]
estimatePartc_13h<-(predict(linearFit_bothLags_13h, newdata = test_13h))
tempC_13h <- as.data.frame(cbind(estimatePartc_13h,combinedDataForPartC_13h[,3]))
mapePartC_13h <- mape(tempC_13h[,1],tempC_13h[,2])
summary(mapePartC_13h)

combinedDataForPartC_14h<-dataForPartB[elements[,15],]
linearFit_bothLags_14h <- lm(`Data Real`~.,data=combinedDataForPartC_14h[,c(3,4,5)])  
summary(linearFit_bothLags_14h) 
test_14h<- dataForPartB[seq(15,nrow(dataForPartB)-9, by = 24),]
estimatePartc_14h<-(predict(linearFit_bothLags_14h, newdata = test_14h))
tempC_14h <- as.data.frame(cbind(estimatePartc_14h,combinedDataForPartC_14h[,3]))
mapePartC_14h <- mape(tempC_14h[,1],tempC_14h[,2])
summary(mapePartC_14h)

combinedDataForPartC_15h<-dataForPartB[elements[,16],]
linearFit_bothLags_15h <- lm(`Data Real`~.,data=combinedDataForPartC_15h[,c(3,4,5)])  
summary(linearFit_bothLags_15h) 
test_15h<- dataForPartB[seq(16,nrow(dataForPartB)-8, by = 24),]
estimatePartc_15h<-(predict(linearFit_bothLags_15h, newdata = test_15h))
tempC_15h <- as.data.frame(cbind(estimatePartc_15h,combinedDataForPartC_15h[,3]))
mapePartC_15h <- mape(tempC_15h[,1],tempC_15h[,2])
summary(mapePartC_15h)

combinedDataForPartC_16h<-dataForPartB[elements[,17],]
linearFit_bothLags_16h <- lm(`Data Real`~.,data=combinedDataForPartC_16h[,c(3,4,5)])  
summary(linearFit_bothLags_16h) 
test_16h<- dataForPartB[seq(17,nrow(dataForPartB)-7, by = 24),]
estimatePartc_16h<-(predict(linearFit_bothLags_16h, newdata = test_16h))
tempC_16h <- as.data.frame(cbind(estimatePartc_16h,combinedDataForPartC_16h[,3]))
mapePartC_16h <- mape(tempC_16h[,1],tempC_16h[,2])
summary(mapePartC_16h)

combinedDataForPartC_17h<-dataForPartB[elements[,18],]
linearFit_bothLags_17h <- lm(`Data Real`~.,data=combinedDataForPartC_17h[,c(3,4,5)])  
summary(linearFit_bothLags_17h) 
test_17h<- dataForPartB[seq(18,nrow(dataForPartB)-6, by = 24),]
estimatePartc_17h<-(predict(linearFit_bothLags_17h, newdata = test_17h))
tempC_17h <- as.data.frame(cbind(estimatePartc_17h,combinedDataForPartC_17h[,3]))
mapePartC_17h <- mape(tempC_17h[,1],tempC_17h[,2])
summary(mapePartC_17h)

combinedDataForPartC_18h<-dataForPartB[elements[,19],]
linearFit_bothLags_18h <- lm(`Data Real`~.,data=combinedDataForPartC_18h[,c(3,4,5)])  
summary(linearFit_bothLags_18h) 
test_18h<- dataForPartB[seq(19,nrow(dataForPartB)-5, by = 24),]
estimatePartc_18h<-(predict(linearFit_bothLags_18h, newdata = test_18h))
tempC_18h <- as.data.frame(cbind(estimatePartc_18h,combinedDataForPartC_18h[,3]))
mapePartC_18h <- mape(tempC_18h[,1],tempC_18h[,2])
summary(mapePartC_18h)

combinedDataForPartC_19h<-dataForPartB[elements[,20],]
linearFit_bothLags_19h <- lm(`Data Real`~.,data=combinedDataForPartC_19h[,c(3,4,5)])  
summary(linearFit_bothLags_19h) 
test_19h<- dataForPartB[seq(20,nrow(dataForPartB)-4, by = 24),]
estimatePartc_19h<-(predict(linearFit_bothLags_19h, newdata = test_19h))
tempC_19h <- as.data.frame(cbind(estimatePartc_19h,combinedDataForPartC_19h[,3]))
mapePartC_19h <- mape(tempC_19h[,1],tempC_19h[,2])
summary(mapePartC_19h)

combinedDataForPartC_20h<-dataForPartB[elements[,21],]
linearFit_bothLags_20h <- lm(`Data Real`~.,data=combinedDataForPartC_20h[,c(3,4,5)])  
summary(linearFit_bothLags_20h) 
test_20h<- dataForPartB[seq(21,nrow(dataForPartB)-3, by = 24),]
estimatePartc_20h<-(predict(linearFit_bothLags_20h, newdata = test_20h))
tempC_20h <- as.data.frame(cbind(estimatePartc_20h,combinedDataForPartC_20h[,3]))
mapePartC_20h <- mape(tempC_20h[,1],tempC_20h[,2])
summary(mapePartC_20h)

combinedDataForPartC_21h<-dataForPartB[elements[,22],]
linearFit_bothLags_21h <- lm(`Data Real`~.,data=combinedDataForPartC_21h[,c(3,4,5)])  
summary(linearFit_bothLags_21h) 
test_21h<- dataForPartB[seq(22,nrow(dataForPartB)-2, by = 24),]
estimatePartc_21h<-(predict(linearFit_bothLags_21h, newdata = test_21h))
tempC_21h <- as.data.frame(cbind(estimatePartc_21h,combinedDataForPartC_21h[,3]))
mapePartC_21h <- mape(tempC_21h[,1],tempC_21h[,2])
summary(mapePartC_21h)

combinedDataForPartC_22h<-dataForPartB[elements[,23],]
linearFit_bothLags_22h <- lm(`Data Real`~.,data=combinedDataForPartC_22h[,c(3,4,5)])  
summary(linearFit_bothLags_22h) 
test_22h<- dataForPartB[seq(23,nrow(dataForPartB)-1, by = 24),]
estimatePartc_22h<-(predict(linearFit_bothLags_22h, newdata = test_22h))
tempC_22h <- as.data.frame(cbind(estimatePartc_22h,combinedDataForPartC_22h[,3]))
mapePartC_22h <- mape(tempC_22h[,1],tempC_22h[,2])
summary(mapePartC_22h)

combinedDataForPartC_23h<-dataForPartB[elements[,24],]
linearFit_bothLags_23h <- lm(`Data Real`~.,data=combinedDataForPartC_23h[,c(3,4,5)])  
summary(linearFit_bothLags_23h) 
test_23h<- dataForPartB[seq(24,nrow(dataForPartB), by = 24),]
estimatePartc_23h<-(predict(linearFit_bothLags_23h, newdata = test_23h))
tempC_23h <- as.data.frame(cbind(estimatePartc_23h,combinedDataForPartC_23h[,3]))
mapePartC_23h <- mape(tempC_23h[,1],tempC_23h[,2])
summary(mapePartC_23h)

mapePartC<-rbind(mapePartC_0h,mapePartC_1h,mapePartC_2h, mapePartC_3h,mapePartC_4h,mapePartC_5h,mapePartC_6h,mapePartC_7h,mapePartC_8h,mapePartC_9h,mapePartC_10h,mapePartC_11h,mapePartC_12h,mapePartC_13h,mapePartC_14h,mapePartC_15h,mapePartC_16h,mapePartC_17h,mapePartC_18h,mapePartC_19h,mapePartC_20h,mapePartC_21h,mapePartC_22h,mapePartC_23h)


## PART D


dataForPart<-as.numeric(dataForPartB$Hour)/3600

partD_0h<-dataForPartB[which(dataForPart==0),]
colnames(partD_0h)<-c("Date", "Hour", "Data Real 0h", "Data lag_48 0h","Data lag_168 0h")

partD_1h<-dataForPartB[which(dataForPart==1),]
colnames(partD_1h)<-c("Date", "Hour", "Data Real 1h", "Data lag_48 1h","Data lag_168 1h")

partD_2h<-dataForPartB[which(dataForPart==2),]
colnames(partD_2h)<-c("Date", "Hour", "Data Real 2h", "Data lag_48 2h","Data lag_168 2h")

partD_3h<-dataForPartB[which(dataForPart==3),]
colnames(partD_3h)<-c("Date", "Hour", "Data Real 3h", "Data lag_48 3h","Data lag_168 3h")

partD_4h<-dataForPartB[which(dataForPart==4),]
colnames(partD_4h)<-c("Date", "Hour", "Data Real 4h", "Data lag_48 4h","Data lag_168 4h")

partD_5h<-dataForPartB[which(dataForPart==5),]
colnames(partD_5h)<-c("Date", "Hour", "Data Real 5h", "Data lag_48 5h","Data lag_168 5h")

partD_6h<-dataForPartB[which(dataForPart==6),]
colnames(partD_6h)<-c("Date", "Hour", "Data Real 6h", "Data lag_48 6h","Data lag_168 6h")

partD_7h<-dataForPartB[which(dataForPart==7),]
colnames(partD_7h)<-c("Date", "Hour", "Data Real 7h", "Data lag_48 7h","Data lag_168 7h")

partD_8h<-dataForPartB[which(dataForPart==8),]
colnames(partD_8h)<-c("Date", "Hour", "Data Real 8h", "Data lag_48 8h","Data lag_168 8h")

partD_9h<-dataForPartB[which(dataForPart==9),]
colnames(partD_9h)<-c("Date", "Hour", "Data Real 9h", "Data lag_48 9h","Data lag_168 9h")

partD_10h<-dataForPartB[which(dataForPart==10),]
colnames(partD_10h)<-c("Date", "Hour", "Data Real 10h", "Data lag_48 10h","Data lag_168 10h")

partD_11h<-dataForPartB[which(dataForPart==11),]
colnames(partD_11h)<-c("Date", "Hour", "Data Real 11h", "Data lag_48 11h","Data lag_168 11h")

partD_12h<-dataForPartB[which(dataForPart==12),]
colnames(partD_12h)<-c("Date", "Hour", "Data Real 12h", "Data lag_48 12h","Data lag_168 12h")

partD_13h<-dataForPartB[which(dataForPart==13),]
colnames(partD_13h)<-c("Date", "Hour", "Data Real 13h", "Data lag_48 13h","Data lag_168 13h")

partD_14h<-dataForPartB[which(dataForPart==14),]
colnames(partD_14h)<-c("Date", "Hour", "Data Real 14h", "Data lag_48 14h","Data lag_168 14h")

partD_15h<-dataForPartB[which(dataForPart==15),]
colnames(partD_15h)<-c("Date", "Hour", "Data Real 15h", "Data lag_48 15h","Data lag_168 15h")

partD_16h<-dataForPartB[which(dataForPart==16),]
colnames(partD_16h)<-c("Date", "Hour", "Data Real 16h", "Data lag_48 16h","Data lag_168 16h")

partD_17h<-dataForPartB[which(dataForPart==17),]
colnames(partD_17h)<-c("Date", "Hour", "Data Real 17h", "Data lag_48 17h","Data lag_168 17h")

partD_18h<-dataForPartB[which(dataForPart==18),]
colnames(partD_18h)<-c("Date", "Hour", "Data Real 18h", "Data lag_48 18h","Data lag_168 18h")

partD_19h<-dataForPartB[which(dataForPart==19),]
colnames(partD_19h)<-c("Date", "Hour", "Data Real 19h", "Data lag_48 19h","Data lag_168 19h")

partD_20h<-dataForPartB[which(dataForPart==20),]
colnames(partD_20h)<-c("Date", "Hour", "Data Real 20h", "Data lag_48 20h","Data lag_168 20h")

partD_21h<-dataForPartB[which(dataForPart==21),]
colnames(partD_21h)<-c("Date", "Hour", "Data Real 21h", "Data lag_48 21h","Data lag_168 21h")

partD_22h<-dataForPartB[which(dataForPart==22),]
colnames(partD_22h)<-c("Date", "Hour", "Data Real 22h", "Data lag_48 22h","Data lag_168 22h")

partD_23h<-dataForPartB[which(dataForPart==23),]
colnames(partD_23h)<-c("Date", "Hour", "Data Real 23h", "Data lag_48 23h","Data lag_168 23h")

wideFirstStep<-cbind(partD_0h[,c(1,4,5)],partD_1h[,c(4,5)],partD_2h[,c(4,5)],partD_3h[,c(4,5)],partD_4h[,c(4,5)],partD_5h[,c(4,5)],partD_6h[,c(4,5)],partD_7h[,c(4,5)],partD_8h[,c(4,5)],partD_9h[,c(4,5)],partD_10h[,c(4,5)],partD_11h[,c(4,5)],partD_12h[,c(4,5)],partD_13h[,c(4,5)],partD_14h[,c(4,5)],partD_15h[,c(4,5)],partD_16h[,c(4,5)],partD_17h[,c(4,5)],partD_18h[,c(4,5)],partD_19h[,c(4,5)],partD_20h[,c(4,5)],partD_21h[,c(4,5)],partD_22h[,c(4,5)],partD_23h[,c(4,5)] )

wideDataTrain<-cbind(wideFirstStep,partD_0h[,3])



tempPartD_<-cbind(dataForPartA_lag48[,c(1,2,3,6)],dataForPartA_lag168[,6])
colnames(tempPartD_)<-c("Date", "Hour", "Data Real", "Data lag_48","Data lag_168")

dataPartD_numeric <-as.numeric(tempPartD_$Hour)/3600

tempPartD_0h<-tempPartD_[which(dataPartD_numeric==0),]
colnames(tempPartD_0h)<-c("Date", "Hour", "Data Real 0h", "Data lag_48 0h","Data lag_168 0h")
tempPartD_1h<-tempPartD_[which(dataPartD_numeric==1),]
colnames(tempPartD_1h)<-c("Date", "Hour", "Data Real 1h", "Data lag_48 1h","Data lag_168 1h")
tempPartD_2h<-tempPartD_[which(dataPartD_numeric==2),]
colnames(tempPartD_2h)<-c("Date", "Hour", "Data Real 2h", "Data lag_48 2h","Data lag_168 2h")
tempPartD_3h<-tempPartD_[which(dataPartD_numeric==3),]
colnames(tempPartD_3h)<-c("Date", "Hour", "Data Real 3h", "Data lag_48 3h","Data lag_168 3h")
tempPartD_4h<-tempPartD_[which(dataPartD_numeric==4),]
colnames(tempPartD_4h)<-c("Date", "Hour", "Data Real 4h", "Data lag_48 4h","Data lag_168 4h")
tempPartD_5h<-tempPartD_[which(dataPartD_numeric==5),]
colnames(tempPartD_5h)<-c("Date", "Hour", "Data Real 5h", "Data lag_48 5h","Data lag_168 5h")
tempPartD_6h<-tempPartD_[which(dataPartD_numeric==6),]
colnames(tempPartD_6h)<-c("Date", "Hour", "Data Real 6h", "Data lag_48 6h","Data lag_168 6h")
tempPartD_7h<-tempPartD_[which(dataPartD_numeric==7),]
colnames(tempPartD_7h)<-c("Date", "Hour", "Data Real 7h", "Data lag_48 7h","Data lag_168 7h")
tempPartD_8h<-tempPartD_[which(dataPartD_numeric==8),]
colnames(tempPartD_8h)<-c("Date", "Hour", "Data Real 8h", "Data lag_48 8h","Data lag_168 8h")
tempPartD_9h<-tempPartD_[which(dataPartD_numeric==9),]
colnames(tempPartD_9h)<-c("Date", "Hour", "Data Real 9h", "Data lag_48 9h","Data lag_168 9h")
tempPartD_10h<-tempPartD_[which(dataPartD_numeric==10),]
colnames(tempPartD_10h)<-c("Date", "Hour", "Data Real 10h", "Data lag_48 10h","Data lag_168 10h")
tempPartD_11h<-tempPartD_[which(dataPartD_numeric==11),]
colnames(tempPartD_11h)<-c("Date", "Hour", "Data Real 11h", "Data lag_48 11h","Data lag_168 11h")
tempPartD_12h<-tempPartD_[which(dataPartD_numeric==12),]
colnames(tempPartD_12h)<-c("Date", "Hour", "Data Real 12h", "Data lag_48 12h","Data lag_168 12h")
tempPartD_13h<-tempPartD_[which(dataPartD_numeric==13),]
colnames(tempPartD_13h)<-c("Date", "Hour", "Data Real 13h", "Data lag_48 13h","Data lag_168 13h")
tempPartD_14h<-tempPartD_[which(dataPartD_numeric==14),]
colnames(tempPartD_14h)<-c("Date", "Hour", "Data Real 14h", "Data lag_48 14h","Data lag_168 14h")
tempPartD_15h<-tempPartD_[which(dataPartD_numeric==15),]
colnames(tempPartD_15h)<-c("Date", "Hour", "Data Real 15h", "Data lag_48 15h","Data lag_168 15h")
tempPartD_16h<-tempPartD_[which(dataPartD_numeric==16),]
colnames(tempPartD_16h)<-c("Date", "Hour", "Data Real 16h", "Data lag_48 16h","Data lag_168 16h")
tempPartD_17h<-tempPartD_[which(dataPartD_numeric==17),]
colnames(tempPartD_17h)<-c("Date", "Hour", "Data Real 17h", "Data lag_48 17h","Data lag_168 17h")
tempPartD_18h<-tempPartD_[which(dataPartD_numeric==18),]
colnames(tempPartD_18h)<-c("Date", "Hour", "Data Real 18h", "Data lag_48 18h","Data lag_168 18h")
tempPartD_19h<-tempPartD_[which(dataPartD_numeric==19),]
colnames(tempPartD_19h)<-c("Date", "Hour", "Data Real 19h", "Data lag_48 19h","Data lag_168 19h")
tempPartD_20h<-tempPartD_[which(dataPartD_numeric==20),]
colnames(tempPartD_20h)<-c("Date", "Hour", "Data Real 20h", "Data lag_48 20h","Data lag_168 20h")
tempPartD_21h<-tempPartD_[which(dataPartD_numeric==21),]
colnames(tempPartD_21h)<-c("Date", "Hour", "Data Real 21h", "Data lag_48 21h","Data lag_168 21h")
tempPartD_22h<-tempPartD_[which(dataPartD_numeric==22),]
colnames(tempPartD_22h)<-c("Date", "Hour", "Data Real 22h", "Data lag_48 22h","Data lag_168 22h")
tempPartD_23h<-tempPartD_[which(dataPartD_numeric==23),]
colnames(tempPartD_23h)<-c("Date", "Hour", "Data Real 23h", "Data lag_48 23h","Data lag_168 23h")

wideTestData<-cbind(tempPartD_0h[,c(1,4,5)],tempPartD_1h[,c(4,5)],tempPartD_2h[,c(4,5)],tempPartD_3h[,c(4,5)],tempPartD_4h[,c(4,5)],tempPartD_5h[,c(4,5)],tempPartD_6h[,c(4,5)],tempPartD_7h[,c(4,5)],tempPartD_8h[,c(4,5)],tempPartD_9h[,c(4,5)],tempPartD_10h[,c(4,5)],tempPartD_11h[,c(4,5)],tempPartD_12h[,c(4,5)],tempPartD_13h[,c(4,5)],tempPartD_14h[,c(4,5)],tempPartD_15h[,c(4,5)],tempPartD_16h[,c(4,5)],tempPartD_17h[,c(4,5)],tempPartD_18h[,c(4,5)],tempPartD_19h[,c(4,5)],tempPartD_20h[,c(4,5)],tempPartD_21h[,c(4,5)],tempPartD_22h[,c(4,5)],tempPartD_23h[,c(4,5)] )




lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_0h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_0h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_0h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_0h<-predict(praModel_0h, newx= as.matrix(wideTestData[,-c(1)])) 

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_1h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_1h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_1h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_1h<-predict(praModel_1h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_2h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_2h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_2h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_2h<-predict(praModel_2h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_3h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_3h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_3h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_3h<-predict(praModel_3h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_4h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_4h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_4h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_4h<-predict(praModel_4h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_5h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_5h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_5h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_5h<-predict(praModel_5h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_6h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_6h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_6h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_6h<-predict(praModel_6h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_7h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_7h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_7h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_7h<-predict(praModel_7h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_8h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_8h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_8h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_8h<-predict(praModel_8h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_9h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_9h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_9h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_9h<-predict(praModel_9h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_10h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_10h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_10h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_10h<-predict(praModel_10h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_11h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_11h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_11h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_11h<-predict(praModel_11h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_12h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_12h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_12h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_12h<-predict(praModel_12h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_13h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_13h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_13h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_13h<-predict(praModel_13h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_14h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_14h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_14h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_14h<-predict(praModel_14h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_15h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_15h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_15h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_15h<-predict(praModel_15h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_16h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_16h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_16h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_16h<-predict(praModel_16h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_17h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_17h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_17h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_17h<-predict(praModel_17h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_18h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_18h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_18h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_18h<-predict(praModel_18h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_19h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_19h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_19h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_19h<-predict(praModel_19h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_20h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_20h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_20h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_20h<-predict(praModel_20h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_21h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_21h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_21h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_21h<-predict(praModel_21h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_22h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_22h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_22h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_22h<-predict(praModel_22h, newx= as.matrix(wideTestData[,-c(1)]))

lambdaCalc<-cv.glmnet(as.matrix(wideDataTrain[,-c(1,50)]), partD_23h[,3], type.measure='mse', nfolds=10, family='gaussian', alpha=1)$lambda.min
praModel_23h<-glmnet(as.matrix(wideDataTrain[,-c(1,50)]),partD_23h[,3],alpha=1,family='gaussian',lambda = lambdaCalc)
predict_23h<-predict(praModel_23h, newx= as.matrix(wideTestData[,-c(1)]))

lambdas<-rbind(praModel_0h$lambda,praModel_1h$lambda,praModel_2h$lambda,praModel_3h$lambda,praModel_4h$lambda,praModel_5h$lambda,praModel_6h$lambda,praModel_7h$lambda,praModel_8h$lambda,praModel_9h$lambda,praModel_10h$lambda,praModel_11h$lambda,praModel_12h$lambda,praModel_13h$lambda,praModel_14h$lambda,praModel_15h$lambda,praModel_16h$lambda,praModel_17h$lambda,praModel_18h$lambda,praModel_19h$lambda,praModel_20h$lambda,praModel_21h$lambda,praModel_22h$lambda,praModel_23h$lambda)


summary(praModel_0h)

mapePartD_0h<-mape(predict_0h,tempPartD_0h[,3])
mapePartD_1h<-mape(predict_1h,tempPartD_1h[,3])
mapePartD_2h<-mape(predict_2h,tempPartD_2h[,3])
mapePartD_3h<-mape(predict_3h,tempPartD_3h[,3])
mapePartD_4h<-mape(predict_4h,tempPartD_4h[,3])
mapePartD_5h<-mape(predict_5h,tempPartD_5h[,3])
mapePartD_6h<-mape(predict_6h,tempPartD_6h[,3])
mapePartD_7h<-mape(predict_7h,tempPartD_7h[,3])
mapePartD_8h<-mape(predict_8h,tempPartD_8h[,3])
mapePartD_9h<-mape(predict_9h,tempPartD_9h[,3])
mapePartD_10h<-mape(predict_10h,tempPartD_10h[,3])
mapePartD_11h<-mape(predict_11h,tempPartD_11h[,3])
mapePartD_12h<-mape(predict_12h,tempPartD_12h[,3])
mapePartD_13h<-mape(predict_13h,tempPartD_13h[,3])
mapePartD_14h<-mape(predict_14h,tempPartD_14h[,3])
mapePartD_15h<-mape(predict_15h,tempPartD_15h[,3])
mapePartD_16h<-mape(predict_16h,tempPartD_16h[,3])
mapePartD_17h<-mape(predict_17h,tempPartD_17h[,3])
mapePartD_18h<-mape(predict_18h,tempPartD_18h[,3])
mapePartD_19h<-mape(predict_19h,tempPartD_19h[,3])
mapePartD_20h<-mape(predict_20h,tempPartD_20h[,3])
mapePartD_21h<-mape(predict_21h,tempPartD_21h[,3])
mapePartD_22h<-mape(predict_22h,tempPartD_22h[,3])
mapePartD_23h<-mape(predict_23h,tempPartD_23h[,3])

mapePartD<-rbind(mapePartD_0h,mapePartD_1h,mapePartD_2h,mapePartD_3h,mapePartD_4h,mapePartD_5h,mapePartD_6h,mapePartD_7h,mapePartD_8h,mapePartD_9h,mapePartD_10h,mapePartD_11h,mapePartD_12h,mapePartD_13h,mapePartD_14h,mapePartD_15h,mapePartD_16h,mapePartD_17h,mapePartD_18h,mapePartD_19h,mapePartD_20h,mapePartD_21h,mapePartD_22h,mapePartD_23h)

## PART E

yTestPartE <-cbind(tempPartD_0h[,3],tempPartD_1h[,3],tempPartD_2h[,3],tempPartD_3h[,3],tempPartD_4h[,3],tempPartD_5h[,3],tempPartD_6h[,3],tempPartD_7h[,3],tempPartD_8h[,3],tempPartD_9h[,3],tempPartD_10h[,3],tempPartD_11h[,3],tempPartD_12h[,3],tempPartD_13h[,3],tempPartD_14h[,3],tempPartD_15h[,3],tempPartD_16h[,3],tempPartD_17h[,3],tempPartD_18h[,3],tempPartD_19h[,3],tempPartD_20h[,3],tempPartD_21h[,3],tempPartD_22h[,3],tempPartD_23h[,3])
wideFirstStep_PartE<-cbind(partD_0h[,c(1,3,4,5)],partD_1h[,c(3,4,5)],partD_2h[,c(3,4,5)],partD_3h[,c(3,4,5)],partD_4h[,c(3,4,5)],partD_5h[,c(3,4,5)],partD_6h[,c(3,4,5)],partD_7h[,c(3,4,5)],partD_8h[,c(3,4,5)],partD_9h[,c(3,4,5)],partD_10h[,c(3,4,5)],partD_11h[,c(3,4,5)],partD_12h[,c(3,4,5)],partD_13h[,c(3,4,5)],partD_14h[,c(3,4,5)],partD_15h[,c(3,4,5)],partD_16h[,c(3,4,5)],partD_17h[,c(3,4,5)],partD_18h[,c(3,4,5)],partD_19h[,c(3,4,5)],partD_20h[,c(3,4,5)],partD_21h[,c(3,4,5)],partD_22h[,c(3,4,5)],partD_23h[,c(3,4,5)] )
beta<-Variable(48)
mapePartE<-vector()
for (i in 1:24)
{
indx<-(i*3)-1
yPartE<-as.matrix(wideFirstStep_PartE[,c(indx)])
xPartE<-as.matrix(wideFirstStep[,c(-1)])
  

objectiveFunction<-(sum_squares(yPartE-xPartE %*% beta) + max(lambdas)*sum(beta^2)) + max(lambdas)*p_norm(diff(beta, differences=1),1)
resultsParams<-solve(Problem(Minimize(objectiveFunction)))

foundBeta<-resultsParams$getValue(beta) 

xTestPartE<-as.matrix(wideTestData[,c(-1)])
TestY<-as.matrix(yTestPartE[,i])

fusedPred<-xTestPartE %*% foundBeta
mapePartE[i]<-mape(fusedPred,TestY)
}
as.matrix(mapePartE)


## PART F

boxplot(mape_lag48,mape_lag168,mapePartB,mapePartC, mapePartD ,mapePartE, names = c("Lag_48","Lag_168","Part B","Part C","Part D", "Part E"), xlab = "Approaches" , ylab = "Mape Values (%)")

