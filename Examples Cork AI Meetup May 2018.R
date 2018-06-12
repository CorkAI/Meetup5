# Libraries
library(ggplot2)
library(e1071)


# Cats intro
library(MASS)
data(cats)
plot(cats)
ggplot(data = cats, aes(x = Bwt, y = Hwt, color = Sex, shape = Sex)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

#Show linear model
catmodel <- svm(Sex~., data = cats, kernel = "linear" )
print(catmodel)
summary(catmodel)
plot(catmodel, cats)

#Show non-linear radial model
catmodel <- svm(Sex~., data = cats)  #svm(Sex~., data = cats, kernel = "radial", cost = 100, gamma = 0.001)
catmodel <- svm(Sex~., data = cats, kernel = "radial")
plot(catmodel, cats)
print(catmodel)
summary(catmodel)


#GeneCounts example linear
GeneCounts$Disease <- factor(GeneCounts$Disease)
ggplot(data = GeneCounts, aes(x = Gene2, y = Gene1, color = Disease, shape = Disease)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")
# Fit Support Vector Machine model to data set
genemodel <- svm(Disease ~ ., data = GeneCounts, kernel = "linear")
# Plot Results
plot(genemodel, GeneCounts)
pred <- predict(genemodel, GeneCounts)
(misclassified <- table(predict = pred, truth = GeneCounts$Disease))  # It has fit model to trianing data

# Load test data
ggplot(data = TestDataGencount, aes(x = Gene2, y = Gene1, color = Disease, shape = Disease)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

#genemodel <- svm(Disease ~ ., data = GeneCounts, kernel = "linear", Cost = 10)
pred <- predict(genemodel, TestDataGencount)
(misclassified <- table(predict = pred, truth = TestDataGencount$Disease))

#Check for normality
par(mfrow=c(1, 2))  # divide graph area in 2 columns
dfPos <- TestDataGencount[50:100,]
plot(density(dfPos$Gene1), main="Density Plot: Gene1", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dfPos$Gene1), 2)))  # density plot for 'speed'
polygon(density(dfPos$Gene1), col="red")
plot(density(dfPos[1:50,]$Gene2), main="Density Plot: Gene2", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dfPos$Gene2), 2)))  # density plot for 'speed'
polygon(density(dfPos$Gene2), col="purple")

#Check for outliers in TestData
dfPos <- TestDataGencount[51:100,]
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(dfPos$Gene1, main="Gene1", sub=paste("Outlier rows: ", boxplot.stats(dfPos$Gene1)$out))  
boxplot(dfPos$Gene2, main="Gene2", sub=paste("Outlier rows: ", boxplot.stats(dfPos$Gene2)$out)) 

pred <- predict(svmmodel, TestDataGencount)
(misclassified <- table(predict = pred, truth = TestDataGencount$Disease))

# find optimal cost of misclassification
tuned <- tune.svm(Disease~., data = GeneCounts, kernel = "linear", gamma = seq(0,1,0.01), cost = 2^(2:9))
# extract the best model
bestmod <- tuned$best.model
plot(tuned)

#Breast cancer example
#https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM#cite_note-Dimitriadou2005-8

wdbc <- read.csv('wdbc.data', head = FALSE)

ggplot(data = wdbc, aes(x = V3, y = V4, color = V2, shape = V2)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

index <- 1:nrow(wdbc)
testindex <- sample(index, trunc(length(index)*20/100))
testset <- wdbc[testindex,]
trainset <- wdbc[-testindex,]
tuned <- tune.svm(V2~., data = trainset, gamma = seq(0,1,0.02), cost = 10^(-1:3))
#Use this one
tuned <- tune.svm(V2~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))
#gamma = 10^(-6:-1), cost = 10^(-1:1))
print(tuned)
plot(tuned)
bestmod <- tuned$best.model


model  <- svm(V2~., data = trainset, kernel = "radial", gamma = 0.2, cost = 1) 
prediction <- predict(model, testset[,-2])
tab <- table(pred = prediction, true = testset[,2])

model  <- svm(V2~., data = trainset, kernel = "radial", gamma = 0.001, cost = 10) 
prediction <- predict(model, testset[,-2])
tab <- table(pred = prediction, true = testset[,2])


