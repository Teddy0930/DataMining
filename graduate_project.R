rm(list = ls())

install.packages("rpart.plot")
install.packages("rattle")
install.packages("caret")
install.packages("e1071")
install.packages("randomForest")
library(e1071)
library(caret)
library(randomForest)
library(rpart)
library("rattle")


dataset <- Clothes_simulation

# demo do not compile
treeThree <- rpart(Fraud ~ Status, data = anotherTest, method = "class", 
                   parms = list(split = 'information'), minsplit = 2, 
                   minbucket = 1, cp = -1)
# demo

# first test
set.seed(22)
set.seed(23)
# seed24 acc:0.83%
set.seed(24)
set.seed(32)
train.index <- sample(x=1:nrow(dataset), 
                      size=ceiling(0.8*nrow(dataset) ))
train <- dataset[train.index,]
test <-  dataset[-train.index,]

treeOne<- rpart(Person ~. ,data = train)
treeOne                

# predict
pred_treeOne <- predict(treeOne, newdata = test, type = "class")
table(real=test$Person, predict=pred_treeOne)

confus.matrix <- table(real=test$Person, predict=pred_treeOne)
sum(diag(confus.matrix))/sum(confus.matrix)
# 對角線的數量/總數量

#draw treeOne
tree_One_pic1 <- prp( treeOne,
     faclen = 0,
     fallen.leaves=TRUE,
     shadow.col="gray",
     extra=2,
     cex = 1)
tree_One_pic2 <- fancyRpartPlot(treeOne,
                                cex = 0.7)

# K-Fold Cross Validation (k=10)
train_control <- trainControl(method = "cv",number = 1) # k = 10
# specify the model 
train_control.model <- train(Perso.n ~ .,
                             data = dataset,
                             method = 'rpart',
                             na.action = na.pass,
                             trControl = train_control)
train_control.model
