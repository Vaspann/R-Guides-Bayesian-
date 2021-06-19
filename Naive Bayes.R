#Predicting vintages using naive bayes

library(ggplot2)
library(mlbench)
library(kohonen)
library(tibble)
library(caTools)
library(naivebayes)
library(caret)

#load the data
data("wines", package = "kohonen")
df <- as.tibble(wines)

df <-  cbind(df, vintages)

#Examine the data 

str(df)
summary(df)
table(df$vintages)

#Plot densities 

par(mfrow = c(4,4))

d <- list()
for (i in 1:13) {
  
  d <- density(df[,i])
  
  plot(d,main = colnames(df)[i], 
       ylab  = "density",
       xlab = colnames(df)[i], col = "red")
  
}


#example of geom_density with fill = vintages


par(mfrow = c(1,1))

#There is no overlap between Barbera and Barolo vintages when x = flavonoids
#Grignolino on the other hand does overlap

ggplot(df, aes(x=flavonoids,fill = vintages))+
  geom_density(alpha = 0.6, color = "black")+
  ggtitle("Density Plot")


#train and test
set.seed(123)  # for reproducibility

splitcc <- sample.split(df, SplitRatio = 0.8)
train <- subset(df, splitcc == "TRUE")
test <- subset(df, splitcc == "FALSE")


#Naive Bayes model

model <- naive_bayes(vintages~., data = train)
model


#Predicted Probabilities for train set
#The model assigns extremely high and low probabilities to different observations
pred_train <- predict(model, data = train, type = "prob")
head(pred_train)


# % 99.2 accuracy
p1 <- predict(model, train)
confusionMatrix(train$vintages, p1)


#implement on test model
# % 97.3 accuracy
p2 <- predict(model, newdata = test)
confusionMatrix(test$vintages, p2)

#The naive bayes model predicts wine vontages almost perfectly 

