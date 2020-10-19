#Goal: Build a decision tree.
#Part 1: Build an example tree.
#Part 2: Optimize for accuracy. 
Data <- read.csv("DT-Credit.csv", header=TRUE, sep= ";") 
#Remove index row
Data <- Data[,-1]
str(Data) 
attach(Data)
install.packages("rpart")
library(rpart)

install.packages ("partykit") 
library("partykit") 
plot(as.party(DT_Model)) 
str(Data)
#Change discrete/binary data to factors
cols <- c(1, 3:9, 11:21, 23:31)
Data[cols] <- lapply(Data[cols],factor)


# Change the response to Y/N answers. 
Target=ifelse(RESPONSE==1,'Y','N')
Data <- data.frame(Data,Target)
str(Data)
Data=Data[,-31] 
# minsplit: the minimum number of observations that must exist in a node for a new split 
# minbucket: the minimum number of observations in any terminal node 
# Maxdepth: Maximum depth for any node, with the root node counted as depth 0.
DT_Model1<-rpart(Target~., data=Data, control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 ))
plot(as.party(DT_Model1))
print(DT_Model1)

##Part 2:
# Change the control parameters and see the change in the output (minsplit=60, minbucket=30, maxdepth=8).
install.packages ("caTools") 
library("caTools") 

#Split into training and testing data sets to measure accuracy. 
mysplit <- sample.split(Data,SplitRatio = 0.65)
Train <- subset(Data,mysplit==T)
Test <- subset(Data,mysplit==F)

#Check Split
Train



testModelAccuracy <- function(minSplit, minBucket, maxDepth) {
  
  DT_Model2<-rpart(Target~., data=Train, control=rpart.control(minsplit=minSplit,
                                                               minbucket=minBucket, maxdepth=maxDepth ))
  
  #Applying our model to the testing data. 
  #This way we avoid false "higher accuracy" from overfitting,etc. 
  
  predictionRes <- predict(DT_Model2,Test,type = "class")
  #ConfusionMatrix
  confusionMatrix <- table(Test$Target,predictionRes)
  confusionMatrix
  trueNeg <- confusionMatrix[1,1]
  truePos <- confusionMatrix[2,2]
  
  #Proportion of correct answers over total answers.
  baseAccuracy <- (trueNeg+truePos)/sum(confusionMatrix)
  print(paste("maxDepth: ", maxDepth))
  print(paste("baseAccuracy: ", baseAccuracy))
  plot(as.party(DT_Model2))
}

#Will only be adjusting maxDepth for now, other parameters can also be experimented with.

for(maxDepth in 1:10){
  testModelAccuracy(60,30,maxDepth)
}

#Using visual inspection we can deduce that tree Depth does not increase past 6.
#Maximum accuracy tends to fluctuate between a depth of 5/6 depending on training/test data.
#Regardless improvements are marginal and we remain around 74%. 

