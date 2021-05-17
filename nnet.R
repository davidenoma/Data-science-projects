data <- read.csv('C:/Users/HP/Downloads/assign5.csv')
library(dplyr)
library(tidyverse)
#View the properties of the data 
dim(data) 
str(data)
    
#Data Preprocessing 

#Subset dataset 
data <- data[c(2:12,14)]

#validation_data <- validation_data[2:13]
#convert the power, engine and mileage column to numeric:

#Remove NA values
data <- na.omit(data)
##or replace the na


str(data)
#Engine

for (x in 1:nrow(data)){
  z=substring(data$Engine[x],-1, 4)
  data$Engine[x] <- as.numeric(z)
}

#Power

for (x in 1:nrow(data)){
  data$Power[x] <- as.numeric(str_replace_all(data$Power[x],"[a-z]"," "))
}

#mileage
for (x in 1:nrow(data)){
  data$Mileage[x] = as.numeric(str_replace_all(data$Mileage[x],"[a-z]"," "))
}

#Convert power, engine and mileage to numeric 
data$Power <- as.numeric(data$Power)
data$Mileage <- as.numeric(data$Mileage)
data$Engine <- as.numeric(data$Engine)

#We recode the Location 
data$Location <- as.numeric(as.factor(data$Location))
locationsCode = c(legend="Ahmedabad,Bangalore,Chennai,Coimbatore,Delhi,Hyderabad,Jaipur,Kochi,Kolkata,Mumbai,Pune")
     
#Feature selection 

#We are selecting the variables location,
#We are selecting  Location, Kms Driven, mileage, Engine, Power, to predict Price. 
str(data)#To view variables

# ensure the results are repeatable
set.seed(7)

# load the library
library(mlbench)
library(caret)

#omit
data <- na.omit(data)

# calculate correlation matrix
correlationMatrix <- cor(data[,c(2,4,8:10,12)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly crrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


#Random FOrest Feature selectoin
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#results <- rfe(data[,c(2,4,8:10)], data[,12], sizes=c(1:5), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


#dFinal data with only selected features. 
new_data <- data[,c(4,8:10,12)]



# TRAINING AND TEST DATA

inp <- sample(2, nrow(new_data), replace = TRUE, prob = c(0.7, 0.3))
training_data <- new_data[inp==1, ]
test_data <- new_data[inp==2, ]


Price_max_pre_normalize = max(test_data$Price)
Price_min_pre_normalize = min(test_data$Price) 

# Min-Max Normalization

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#Normalize only once as this reasigns to the values.
training_data$Kilometers_Driven <- normalize(training_data$Kilometers_Driven)
training_data$Mileage <- normalize(training_data$Mileage)
training_data$Engine <- normalize(training_data$Engine)
training_data$Price <- normalize(training_data$Price)
training_data$Power <- normalize(training_data$Power)

#Normalizing the test set.
test_data$Kilometers_Driven <- normalize(test_data$Kilometers_Driven)
test_data$Mileage <- normalize(test_data$Mileage)
test_data$Engine <- normalize(test_data$Engine)
test_data$Price <- normalize(test_data$Price)
test_data$Power <- normalize(test_data$Power)

#. NEURAL NETWORK
library(neuralnet)
#We can chage the hidden layers configuration to c(2,1)
nn <- neuralnet(Price ~Kilometers_Driven + Mileage+Power+Engine,data=training_data, hidden=c(2,1), linear.output=TRUE, threshold=0.01)
nn$result.matrix
# plot our neural network 
plot(nn, rep = 1)

# Prediction
output <- compute(nn, rep = 1, test_data)


head(output$net.result)
results <- data.frame(actual = test_data$Price, prediction = output$net.result)
head(results)
#Accuracy

#REnormalize
predicted=results$prediction * (Price_max_pre_normalize - Price_min_pre_normalize) + Price_min_pre_normalize


actual=results$actual * (Price_max_pre_normalize - Price_min_pre_normalize) + Price_min_pre_normalize

deviation=((actual-predicted)/actual)
accuracy=1-abs(mean(deviation))
accuracy

##Regression Tree Example 

# Install the package
# Load the package
library(rpart)

# Create decision tree using regression
fit <- rpart(Price ~ Kilometers_Driven +Mileage+Power+Engine, 
             method = "anova",data=training_data )

library(rpart.plot)
rpart.plot(fit)
fit$variable.importance

# Output to be present as PNG file
png(file = "decTree2GFG.png", width = 600,
    height = 600)

# Plot
plot(fit, uniform = TRUE,
     main = "Price Decision Tree using Regression")


text(fit, use.n = TRUE, cex = .6)

# Saving the file
dev.off()
# Print model
print(fit)
# Predicting Price using testing data and model
cat("Predicted value:\n")
predPrice <- predict(fit, test_data, method = "anova")
results <- data.frame(actual = test_data$Price, prediction = predPrice)
head(results)
#Accuracy
predicted=results$prediction * (Price_max_pre_normalize - Price_min_pre_normalize) + Price_min_pre_normalize
actual=results$actual * (Price_max_pre_normalize - Price_min_pre_normalize) + Price_min_pre_normalize
deviation=((actual-predicted)/actual)
accuracy=1-abs(mean(deviation))
accuracy

#Learn other measures such as precision, recall, F1 Score, Building AUC-ROC Curves

#Multiple LInear REgression Example 
model <- lm(Price ~ Kilometers_Driven +Mileage+Power+Engine, data=
              training_data)
summary(model)
confint(model)
mlrPrice<-predict(model,test_data)
results <- data.frame(actual = test_data$Price, prediction = mlrPrice)
#Accuracy
predicted=results$prediction * (Price_max_pre_normalize - Price_min_pre_normalize) + Price_min_pre_normalize
actual=results$actual * (Price_max_pre_normalize - Price_min_pre_normalize) + Price_min_pre_normalize
deviation=((actual-predicted)/actual)
accuracy=1-abs(mean(deviation))
accuracy
