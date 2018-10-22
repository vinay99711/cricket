


## Regression and Classification Trees

#Installing Libraries


install.packages('rpart')
install.packages("rpart.plot")
install.packages('caret')
library(rpart)
library(rpart.plot)
library(caret)

## CLASSIFICATION TREES

## Importing dataset into the model

chennai.df <- read.csv("CART_Model.csv")
summary(chennai.df)

head(chennai.df)
  


### Create Training and Validation sets
set.seed(1)  


test.index <- sample(c(1:dim(chennai.df)[1]), dim(chennai.df)[1]*0.2)  
test.df <- chennai.df[test.index, ]

train.index <- sample(c(1:dim(chennai.df)[1]), dim(chennai.df)[1]*0.6)  
train.df <- chennai.df[train.index, ]
valid.df <- chennai.df[-train.index, ]






### Generate classification tree
default.ct <- rpart(Winner ~ ., data = train.df, method = "class")
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)



### Fully-Grown Tree
deeper.ct <- rpart(Winner ~ ., data = train.df, 
                   method = "class", cp = 0, minsplit = 1)

length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])  # count number of leaves
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  




### Confusion Matrices 
#library(caret)
### for Training set
default.ct.point.pred.train <- predict(default.ct, 
                                       data = train.df, 
                                       type = "class")
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Winner))

### for Validation set
default.ct.point.pred.valid <- predict(default.ct, 
                                       newdata = valid.df, 
                                       type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Winner))


### Complexity Parameters
### xval:  # of folds to use cross-validation procedure
### CP: sets the smallest value for the complexity paraeter

### Best-Pruned Tree
set.seed(1)
cv.ct <- rpart(Winner ~ ., data = train.df, method = "class", cp = 0.00001, minsplit = 1, xval = 5)  # minsplit is the minimum number of observations in a node for a split to be attempted. xval is number K of folds in a K-fold cross-validation.
printcp(cv.ct)  
pruned.ct <- prune(cv.ct, cp = 0.0154639)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 



### Confusion Matrices 
#library(caret)
### for Test data

pruned.ct.point.pred.test <- predict(pruned.ct, 
                                       newdata = test.df, 
                                       type = "class")
confusionMatrix(pruned.ct.point.pred.test, as.factor(test.df$Winner))


