data <- read.csv('C:/Users/HP/Downloads/assign5.csv')
validation_data <- read.csv("C:/Users/HP/Downloads/test-data.csv" )
library(dplyr)
library(tidyverse)
#Data Preprocessing 
#Compare the column names of the train and validation dataset.
colnames(data) == colnames(validation_data)
#Subset dataset and validation 
data <- data[c(2:12,14)]
validation_data <- validation_data[2:13]
#convert the power, engine and mileage column to numeric:

data <- na.omit(data)
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
#Remove columns without a price on the validation set
validation_data_filtered <-filter(validation_data, validation_data$New_Price!="")
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

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
data <- na.omit(data)
# calculate correlation matrix
correlationMatrix <- cor(data[,c(2,4,8:10,12)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly crrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(data[,c(2,4,8:10)], data[,12], sizes=c(1:5), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))




regModel <- lm(Price~, data=data)
su

# Sampling of the data
set.seed(222)
inp <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training_data <- data[inp==1, ]
test_data <- data[inp==2, ]

#Step 3: Fitting a Neural Network

library(neuralnet)
set.seed(333)
n <- neuralnet(Price~ Location,
               data = data,
               hidden = 5,
               err.fct = "sse",
               linear.output = FALSE,
               lifesign = 'full',
               rep = 2,
               algorithm = "rprop+",
               stepmax = 100000)

# plot our neural network 
plot(n, rep = 1)


# error
n$result.matrix


# Prediction
output <- compute(n, rep = 1, training_data[, -1])
head(output$net.result)


# confusion Matrix $Misclassification error -Training data
output <- compute(n, rep = 1, training_data[, -1])
p1 <- output$net.result
pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(pred1, training_data$admit)
tab1

#4. NEURAL NETWORK
library(neuralnet)
nn <- neuralnet(consumption ~ capacity + gasoline + hours,data=trainset, hidden=c(2,1), linear.output=TRUE, threshold=0.01)
nn$result.matrix



