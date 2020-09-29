data <- read.csv(choose.files())

# Loading required packages
library(tree)
library(gmodels)
library(caret)
library(caTools)
library(randomForest)

#Exploratory Data Analysis
summary(data)

#boxplot
boxplot(data$Taxable.Income,horizontal = T)  # there are no outliers
boxplot(data$City.Population,horizontal = T) #No outliers
boxplot(data$Work.Experience,horizontal = T) #No outliers
boxplot(data)

#histogram
hist(data$Taxable.Income)
hist(data$City.Population)
hist(data$Work.Experience)

#model creation
risky_gud <- ifelse(data$Taxable.Income<=30000,'fraud','not_fraud')

data1 <- data.frame(data,risky_gud)


#spliting data into train and test data
sample <- sample.split(data1,SplitRatio = 0.75)

train <- subset(data1,sample=="TRUE")
test <- subset(data1,sample=='FALSE')


#model
model <- randomForest(risky_gud~.-Taxable.Income,data = train)
model
plot(model,lwd=3)

pred <- predict(model,test,type='class')
tab<- table(pred,test$risky_gud)
tab
confusionMatrix(pred,test$risky_gud) # Accuracy : 79.07% 


