Data <- read.csv("DT-Credit.csv", header=TRUE, sep= ";") 
Data <- Data[,-1]
str(Data) 
attach(Data)
install.packages("rpart")
library(rpart)
DT_Model <-rpart(RESPONSE~., data=Data, control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 )) 
# minsplit: the minimum number of observations that must exist in a node for a new split 
# minbucket: the minimum number of observations in any terminal node 
# Maxdepth: Maximum depth for any node, with the root node counted as depth 0. 
DT_Model
mean(RESPONSE)
install.packages ("partykit") 
library("partykit") 
plot(as.party(DT_Model)) 
str(Data) 
cols <- c(1, 3:9, 11:21, 23:31)
Data[cols] <- lapply(Data[cols],factor)
str(Data)
DT_Model <-rpart(RESPONSE~., data=Data, control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 )) 
print(DT_Model)
plot(as.party(DT_Model)) 

# Change the response to Y/N answers. 
Target=ifelse(RESPONSE==1,'Y','N')
Data <- data.frame(Data,Target)
str(Data)
Data1=Data[,-31] 
DT_Model1<-rpart(Target~., data=Data1, control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 ))
plot(as.party(DT_Model1))
print(DT_Model1)

# Change the control parameters and see the change in the output (minsplit=60, minbucket=30, maxdepth=8). 
