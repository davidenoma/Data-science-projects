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
str(data)#To view variables
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

#dFinal data with only selected features. 
new_data <- data[,c(4,8:10,12)]
# TRAINING AND TEST DATA
inp <- sample(2, nrow(new_data), replace = TRUE, prob = c(0.7, 0.3))
training_data <- new_data[inp==1, ]
test_data <- new_data[inp==2, ]

#. NEURAL NETWORK
library(neuralnet)
nn <- neuralnet(Price ~ Kilometers_Driven + Mileage+Power + Engine,data=training_data, hidden=c(2,1), linear.output=TRUE, threshold=0.01)
nn$result.matrix

# plot our neural network 
plot(nn, rep = 1)

# Predictionnn
output <- compute(nn, rep = 1, test_data[-1])
prediction
head(output$net.result)


results <- data.frame(actual = test_data$Price, prediction = nn$net.result)
results
##Regression Tree Example 


# Install the package
install.packages("rpart")


# Load the package
library(rpart)


# Create decision tree using regression
fit <- rpart(mpg ~ disp + hp + cyl, 
             method = "anova", data = mtcars )


# Output to be present as PNG file
png(file = "decTree2GFG.png", width = 600,
    height = 600)

# Plot
plot(fit, uniform = TRUE,
     main = "MPG Decision Tree using Regression")
text(fit, use.n = TRUE, cex = .6)

# Saving the file
dev.off()


# Print model
print(fit)


# Create test data
df  <- data.frame (disp = 351, hp = 250, 
                   cyl = 8)

# Predicting mpg using testing data and model
cat("Predicted value:\n")
predict(fit, df, method = "anova")


#Multiple LIneat REgression Example 




