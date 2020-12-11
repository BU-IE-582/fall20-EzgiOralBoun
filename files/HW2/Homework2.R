library(readr)
library(scatterplot3d)
library(ggplot2)
library(RColorBrewer)
library(grDevices)
library(colorRamps)

uWaveGestureLibrary_X_TRAIN <- read_table2("UWave/uWaveGestureLibrary_X_TRAIN", col_names = FALSE)
View(uWaveGestureLibrary_X_TRAIN)

uWaveGestureLibrary_Y_TRAIN <- read_table2("UWave/uWaveGestureLibrary_Y_TRAIN", col_names = FALSE)
View(uWaveGestureLibrary_Y_TRAIN)

uWaveGestureLibrary_Z_TRAIN <- read_table2("UWave/uWaveGestureLibrary_Z_TRAIN", col_names = FALSE)
View(uWaveGestureLibrary_Z_TRAIN)



dataX<-uWaveGestureLibrary_X_TRAIN
dataY<-uWaveGestureLibrary_Y_TRAIN
dataZ<-uWaveGestureLibrary_Z_TRAIN
noOfTimeIndex = NCOL(dataX[,-1])
noOfCol = NROW(dataX)

## PART A

sampleInstances <- c(11, 15, 4, 5, 2, 1, 7, 6) # classes 1...8

for(i in 1:8) #8 classes
{
  instance <- sampleInstances[i]
  class<-as.numeric(dataX[instance,1])
  accData<-t(rbind(dataX[instance,-1],dataY[instance,-1],dataZ[instance,-1])) 
  
  
  
  
  Velocity <- matrix(0,3,noOfTimeIndex)
  Location <- matrix(0,3,noOfTimeIndex)
  
  k=1
  for(j in 1:3) 
  {
    Velocity[k,] <- cumsum(accData[,j]) 
    k=k+1
  }
  
  
  p=1
  for(m in 1:3) 
  {
    Location[p,] <- cumsum(t(Velocity)[,m]) 
    p=p+1
  }
  
  
  scatterplot3d(t(Location)[,1],t(Location)[,2],t(Location)[,3],main = paste("Gesture", i),xlab = "X location",ylab = "Y location",zlab = "Z location", type = "p", color = "red")
}


## PART B

combineAll<-cbind(1,1:noOfTimeIndex,t(dataX[1,-1]),t(dataY[1,-1]),t(dataZ[1,-1]),as.numeric(dataX[1,1])) #initialize

for(s in 2:noOfCol)
{
  
  temp<-cbind(s,1:noOfTimeIndex,t(dataX[s,-1]),t(dataY[s,-1]),t(dataZ[s,-1]),as.numeric(dataX[s,1]))
  combineAll<-rbind(combineAll,temp)
}
colnames(combineAll,do.NULL = TRUE, prefix = "col")
colnames(combineAll)<-c("Time Series ID", "Time Index","X","Y","Z","Class")

dataForPartB<-cbind(combineAll[,3],combineAll[,4],combineAll[,5])
pcaPartB<-prcomp(dataForPartB, scale. = TRUE)
summary(pcaPartB)


randomChoiceClass1_1<-11
randomChoiceClass1_2<-19

randomChoiceClass2_1<-15
randomChoiceClass2_2<-20

randomChoiceClass3_1<-27
randomChoiceClass3_2<-61

randomChoiceClass4_1<-5
randomChoiceClass4_2<-51

randomChoiceClass5_1<-2
randomChoiceClass5_2<-3

randomChoiceClass6_1<-1
randomChoiceClass6_2<-10

randomChoiceClass7_1<-7
randomChoiceClass7_2<-32

randomChoiceClass8_1<-21
randomChoiceClass8_2<-40

t1_1<-seq(((randomChoiceClass1_1-1)*noOfTimeIndex+1),randomChoiceClass1_1*noOfTimeIndex)
t1_2<-seq(((randomChoiceClass1_2-1)*noOfTimeIndex+1) ,randomChoiceClass1_2*noOfTimeIndex)

t2_1<-seq(((randomChoiceClass2_1-1)*noOfTimeIndex+1),randomChoiceClass2_1*noOfTimeIndex)
t2_2<-seq(((randomChoiceClass2_2-1)*noOfTimeIndex+1) ,randomChoiceClass2_2*noOfTimeIndex)

t3_1<-seq(((randomChoiceClass3_1-1)*noOfTimeIndex+1),randomChoiceClass3_1*noOfTimeIndex)
t3_2<-seq(((randomChoiceClass3_2-1)*noOfTimeIndex+1) ,randomChoiceClass3_2*noOfTimeIndex)

t4_1<-seq(((randomChoiceClass4_1-1)*noOfTimeIndex+1),randomChoiceClass4_1*noOfTimeIndex)
t4_2<-seq(((randomChoiceClass4_2-1)*noOfTimeIndex+1) ,randomChoiceClass4_2*noOfTimeIndex)

t5_1<-seq(((randomChoiceClass5_1-1)*noOfTimeIndex+1),randomChoiceClass5_1*noOfTimeIndex)
t5_2<-seq(((randomChoiceClass5_2-1)*noOfTimeIndex+1) ,randomChoiceClass5_2*noOfTimeIndex)

t6_1<-seq(((randomChoiceClass6_1-1)*noOfTimeIndex+1),randomChoiceClass6_1*noOfTimeIndex)
t6_2<-seq(((randomChoiceClass6_2-1)*noOfTimeIndex+1) ,randomChoiceClass6_2*noOfTimeIndex)

t7_1<-seq(((randomChoiceClass7_1-1)*noOfTimeIndex+1),randomChoiceClass7_1*noOfTimeIndex)
t7_2<-seq(((randomChoiceClass7_2-1)*noOfTimeIndex+1),randomChoiceClass7_2*noOfTimeIndex)

t8_1<-seq(((randomChoiceClass8_1-1)*noOfTimeIndex+1),randomChoiceClass8_1*noOfTimeIndex)
t8_2<-seq(((randomChoiceClass8_2-1)*noOfTimeIndex+1),randomChoiceClass8_2*noOfTimeIndex)

time<-1:noOfTimeIndex
plot(time, dataForPartB[t1_1,1], type = "l", ylim=c(-3, 3), lwd=2, main = "Example Data for Class 1", xlab="Time", ylab="PCA Observation")
lines(time, dataForPartB[t1_2,1], col="red", lwd=2)
legend("topright", legend = c(paste("Data No.",randomChoiceClass1_1) , paste("Data No.",randomChoiceClass1_2)),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(time, dataForPartB[t2_1,1], type = "l", ylim=c(-3, 3), lwd=2, main = "Example Data fot Class 2", xlab="Time", ylab="PCA Observation")
lines(time, dataForPartB[t2_2,1], col="red", lwd=2)
legend("topright", legend = c(paste("Data No.",randomChoiceClass2_1) , paste("Data No.",randomChoiceClass2_2)),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(time, dataForPartB[t3_1,1], type = "l", ylim=c(-3, 3), lwd=2, main = "Example Data for Class 3", xlab="Time", ylab="PCA Observation")
lines(time, dataForPartB[t3_2,1], col="red", lwd=2)
legend("topright", legend = c(paste("Data No.",randomChoiceClass3_1) , paste("Data No.",randomChoiceClass3_2)),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(time, dataForPartB[t4_1,1], type = "l", ylim=c(-3, 3), lwd=2, main = "Example Data for Class 4", xlab="Time", ylab="PCA Observation")
lines(time, dataForPartB[t4_2,1], col="red", lwd=2)
legend("topright", legend = c(paste("Data No.",randomChoiceClass4_1) , paste("Data No.",randomChoiceClass4_2)),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(time, dataForPartB[t5_1,1], type = "l", ylim=c(-3, 3), lwd=2, main = "Example Data for Class 5", xlab="Time", ylab="PCA Observation")
lines(time, dataForPartB[t5_2,1], col="red", lwd=2)
legend("topright", legend = c(paste("Data No.",randomChoiceClass5_1) , paste("Data No.",randomChoiceClass5_2)),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(time, dataForPartB[t6_1,1], type = "l", ylim=c(-3, 3), lwd=2, main = "Example Data for Class 6", xlab="Time", ylab="PCA Observation")
lines(time, dataForPartB[t6_2,1], col="red", lwd=2)
legend("topright", legend = c(paste("Data No.",randomChoiceClass6_1) , paste("Data No.",randomChoiceClass6_2)),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(time, dataForPartB[t7_1,1], type = "l", ylim=c(-3, 3), lwd=2, main = "Example Data for Class 7", xlab="Time", ylab="PCA Observation")
lines(time, dataForPartB[t7_2,1], col="red", lwd=2)
legend("topright", legend = c(paste("Data No.",randomChoiceClass7_1) , paste("Data No.",randomChoiceClass7_2)),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(time, dataForPartB[t8_1,1], type = "l", ylim=c(-3, 3), lwd=2, main = "Example Data for Class 8", xlab="Time", ylab="PCA Observation")
lines(time, dataForPartB[t8_2,1], col="red", lwd=2)
legend("topright", legend = c(paste("Data No.",randomChoiceClass8_1) , paste("Data No.",randomChoiceClass8_2)),
       col = c("black", "red"), lty = 1:1, lwd = 2)

## PART C

combineAll = as.data.frame(combineAll)

dataForPartC_1<-cbind(combineAll[which(combineAll$Class==1),3],combineAll[which(combineAll$Class==1),4],combineAll[which(combineAll$Class==1),5])
pcaPartC_1<-princomp(dataForPartC_1)
summary(pcaPartC_1,loadings = TRUE)

dataForPartC_2<-cbind(combineAll[which(combineAll$Class==2),3],combineAll[which(combineAll$Class==2),4],combineAll[which(combineAll$Class==2),5])
pcaPartC_2<-princomp(dataForPartC_2)
summary(pcaPartC_2,loadings = TRUE)

dataForPartC_3<-cbind(combineAll[which(combineAll$Class==3),3],combineAll[which(combineAll$Class==3),4],combineAll[which(combineAll$Class==3),5])
pcaPartC_3<-princomp(dataForPartC_3)
summary(pcaPartC_3,loadings = TRUE)

dataForPartC_4<-cbind(combineAll[which(combineAll$Class==4),3],combineAll[which(combineAll$Class==4),4],combineAll[which(combineAll$Class==4),5])
pcaPartC_4<-princomp(dataForPartC_4)
summary(pcaPartC_4,loadings = TRUE)

dataForPartC_5<-cbind(combineAll[which(combineAll$Class==5),3],combineAll[which(combineAll$Class==5),4],combineAll[which(combineAll$Class==5),5])
pcaPartC_5<-princomp(dataForPartC_5)
summary(pcaPartC_5,loadings = TRUE)

dataForPartC_6<-cbind(combineAll[which(combineAll$Class==6),3],combineAll[which(combineAll$Class==6),4],combineAll[which(combineAll$Class==6),5])
pcaPartC_6<-princomp(dataForPartC_6)
summary(pcaPartC_6,loadings = TRUE)

dataForPartC_7<-cbind(combineAll[which(combineAll$Class==7),3],combineAll[which(combineAll$Class==7),4],combineAll[which(combineAll$Class==7),5])
pcaPartC_7<-princomp(dataForPartC_7)
summary(pcaPartC_7,loadings = TRUE)


dataForPartC_8<-cbind(combineAll[which(combineAll$Class==8),3],combineAll[which(combineAll$Class==8),4],combineAll[which(combineAll$Class==8),5])
pcaPartC_8<-princomp(dataForPartC_8)
summary(pcaPartC_8,loadings = TRUE)

## PART D
XYZ <- cbind(dataX[,-1],dataY[,-1],dataZ[,-1])
asd<-dist(XYZ)
temp<-cmdscale(asd,k=2)
dataForPartD<-cbind(dataX[,1],temp)
dataForPartD_wClass<-as.data.frame(dataForPartD)
#plot(temp,col = dataForPartD_wClass$X1, lwd = 2)
#legend("bottomright", legend = c("1","2","3","4","5","6","7","8"), col = rainbow(8),lwd = 2)
colnames(dataForPartD_wClass,do.NULL = TRUE, prefix = "col")
colnames(dataForPartD_wClass)<-c("Class", "A","B")
ggplot(dataForPartD_wClass) + geom_point(aes(x=A, y=B, color=as.factor(Class))) +
  labs(title="Multidimesional Scaling Observations", y="Axis 2", x="Axis1") +
  scale_color_manual(name = "Class",
                     breaks = c("1", "2", "3","4","5","6","7","8"),
                     values = primary.colors(8, steps = 3.75, no.white = TRUE))

