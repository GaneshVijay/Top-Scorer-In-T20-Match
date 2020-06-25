topscore <- read.csv("E:\\STUDIES\\MINING\\Dataset.csv")
View(topscore)
table(topscore$SR)
data <- sample(2, nrow(topscore), replace=TRUE, prob = c(0.6,0.4))
trainD <- topscore[data==1,]
testD <- topscore[data==2,]
nrow(trainD)
nrow(testD)
library(e1071)
library(rminer)
e1071model <- naiveBayes(SR~.,data=trainD)
e1071model

e1071prediction <- predict(e1071model, testD)
e1071model
e1071prediction
mmetric(testD$Strike,e1071prediction, c("ACC","PRECISION","TPR","F1"))
mmetric(testD$Strike,e1071prediction, c("ACC"))

#plot
library(datasets)
head(topscore)
library(ggplot2)
p1 <- ggplot(topscore, aes(Run,BF, color= SR)) + geom_point()
p2 <- ggplot(topscore, aes(Run,X4s, color= SR)) + geom_point()
p3 <- ggplot(topscore, aes(Run,X6s, color= SR)) + geom_point()
p4 <- ggplot(topscore, aes(BF,X4s, color= SR)) + geom_point()
p5 <- ggplot(topscore, aes(BF,X6s, color= SR )) + geom_point()
p6 <- ggplot(topscore, aes(X4s,X6s, color= SR)) + geom_point()
library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6)
