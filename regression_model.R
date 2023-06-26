library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(maps)
library(ggh4x)
library(gridExtra)
library(caret)

data <- read_excel("/Documents and Settings/mrose/Documents/Window_Manufacturing.xlsx")
names(data) <- gsub(" ", "_", names(data))

data <- data %>%
  mutate_at(vars(1:8, 11), as.numeric)

data <- data %>%
  mutate_at(vars(9, 10, 12), as.factor)

d <- data %>% na.omit()
names(d)[1] <- "y"

set.seed(123)

dummies <- dummyVars(y ~ ., data = d)            
ex <- data.frame(predict(dummies, newdata = d))  
d <- cbind(d$y, ex)                              
names(d)[1] <- "y"                             

descrCor <- cor(d[,2:ncol(d)])                          
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .80) 

highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80)
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] 
descrCor2 <- cor(filteredDescr)               

d <- cbind(d$y, filteredDescr)
names(d)[1] <- "y"

y <- d$y

d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
names(d)[1] <- "ones"

comboInfo <- findLinearCombos(d)
d <- d[, -comboInfo$remove]
d <- d[, c(2:ncol(d))]

d <- cbind(y, d)

numeric_features <- sapply(d, is.numeric)
numeric_features[1] <- FALSE
preProcValues <- preProcess(d[, numeric_features], method = c("range"))

d[, numeric_features] <- predict(preProcValues, d[, numeric_features])

inTrain <- createDataPartition(y = d$y, p = .70, list = FALSE)

train <- d[inTrain,] 
test <- d[-inTrain,] 

ctrl <- trainControl(method="cv", number=5, summaryFunction = defaultSummary)

myModel <- train(y ~ ., data = train, method = "lm", trControl = ctrl, metric = "RMSE")

predictions <- predict(myModel, newdata = test)

options(scipen=999) 
# train
train_stats <- defaultSummary(data=data.frame(obs=train$y, pred=predict(myModel, newdata=train))
               , model=myModel)
# test 
test_stats <- defaultSummary(data=data.frame(obs=test$y, pred=predict(myModel, newdata=test))
               , model=myModel)
train_stats <- data.frame(train_stats)
test_stats <- data.frame(test_stats)

combined_stats <- data.frame(
  RowNames = row.names(train_stats),
  Train_Stats = train_stats[, 1],
  Test_Stats = test_stats[, 1]
)

melted_stats <- melt(combined_stats, id.vars = "RowNames", variable.name = "Dataset")
