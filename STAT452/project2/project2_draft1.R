data <- read.csv("project2/Data2021_final.csv")

data

x <- data %>%
  select(X1:X15)
y <- data$Y

index <- createDataPartition(y, p = 0.75, list = F)
x_train <- x[ index, ]
x_test <- x[-index, ]
y_train <- y[index]
y_test <- y[-index]

regr <- randomForest(x = x_train,
                     y = y_train,
                     maxnodes = 10,
                     ntree = 10)

predictions <- predict(regr, x_test)

result <- x_test
result['Y'] <- y_test
result['prediction'] <- predictions

head(result)

# Import library for visualization
library(ggplot2)

# Build scatterplot
ggplot(  ) + 
  geom_point( aes(x = x_test$X1, y = y_test, color = 'red', alpha = 0.5) ) + 
  geom_point( aes(x = x_test$X1 , y = predictions, color = 'blue',  alpha = 0.5)) + 
  labs(x = "X1", y = "Y", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red")) 

library(Metrics)
print(paste0('MAE: ', mae(y_test,predictions)))
print(paste0('MSE: ' ,caret::postResample(predictions , y_test)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(predictions , y_test)['Rsquared'] ))



# If training the model takes too long try setting up lower value of N
N=500 #length(X_train)
x_train_ = x_train[1:N , ]
y_train_ = y_train[1:N]

seed <- 301325351
metric <- 'RMSE'

customRF <- list(type = "Regression", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("maxnodes", "ntree"), class = rep("numeric", 2), label = c("maxnodes", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, maxnodes = param$maxnodes, ntree=param$ntree, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        search = 'grid')

tunegrid <- expand.grid(.maxnodes = c(70,80,90,100),
                        .ntree = c(900,1000,1100))
set.seed(seed)


rf_gridsearch <- train(x=x_train_,
                       y=y_train_,
                       method = customRF,
                       metric = metric,
                       tuneGrid = tunegrid,
                       trControl = control)

plot(rf_gridsearch)

rf_gridsearch$bestTune # maxnodes = 70; ntree = 1100

tuned_regr <- randomForest(x = x_train,
                     y = y_train,
                     maxnodes = 70,
                     ntree = 1100)
tuned_predictions <- predict(tuned_regr, x_test)

tuned_result <- x_test
tuned_result['Y'] <- y_test
tuned_result['prediction'] <- tuned_predictions

head(tuned_result)
