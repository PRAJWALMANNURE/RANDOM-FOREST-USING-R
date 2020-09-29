# Load the data set
data <- read.csv(choose.files())

#loading the required libraries
library(tree)
library(gmodels)
library(caTools)
library(randomForest)

#exploratory data analysis
summary(data)


boxplot(data,horizontal = T) # there are outliers present in the data set


#histogram 
hist(data$Sales)
hist(data$CompPrice)
hist(data$Income)
hist(data$Advertising)
hist(data$Population)
hist(data$Price)
hist(data$Age)
hist(data$Education)


high <- ifelse(data$Sales<10,'no','yes')

data <- data.frame(data,high)

#splitting data into train and test
sample <- sample.split(data,SplitRatio = 0.75)

train <- subset(data,sample=='TRUE')
test <- subset(data,sample=='FALSE')

# model creation
model <- randomForest(high~.-Sales, data = train)
model
summary(model)

pred <- predict(model,test,type = 'class')
tab<- table(pred,test$high)
(sum(diag(tab))/sum(tab)) 

plot(model,lwd=3)
legend("topright", colnames(model$err.rate),col=1:4,cex=0.8,fill=1:4)

confusionMatrix(pred,test$high) # Accuracy : 87.88%  
