
library(randomForest)
library(caret)
# library(mlbench)
library(e1071)
library(MLmetrics)

# RF
# RF_hr <- train(HOSPITAL_EXPIRE_FLAG ~.,
#                data = train_hr,
#                method = "rf",
#                metric="Sens",
#                trControl = control)

RF_hr_rr <- train(HOSPITAL_EXPIRE_FLAG ~.,
                  data = train_hr_rr,
                  method = "rf",
                  metric="Sens",
                  trControl = control)

plot(RF_hr_rr)

probabilities <- RF_hr_rr %>% predict(test_hr_rr, type = "prob")
predicted_classes <- as.factor(ifelse(probabilities$Death >= 0.5, "Death", "Alive"))
# head(predicted_classes)

actual <- test_hr_rr %>% select(HOSPITAL_EXPIRE_FLAG) %>% unlist() 
actual <- factor(actual, levels = c("Death", "Alive"))

confusionMatrix(predicted_classes, reference = actual, positive = "Death")
confusionMatrix(predicted_classes, reference = actual, positive = "Death", mode = "prec_recall")
auc(actual, probabilities$Death)


############################################custom RF
# customRF <- list(type = "Classification",
#                  library = "randomForest",
#                  loop = NULL)
# 
# customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
#                                   class = rep("numeric", 2),
#                                   label = c("mtry", "ntree"))
# 
# customRF$grid <- function(x, y, len = NULL, search = "grid") {}
# 
# customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
#   randomForest(x, y,
#                mtry = param$mtry,
#                ntree=param$ntree)
# }
# 
# #Predict label
# customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata)
# 
# #Predict prob
# customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata, type = "prob")
# 
# customRF$sort <- function(x) x[order(x[,1]),]
# customRF$levels <- function(x) x$classes
# 
# library(doParallel)
# control <- trainControl(method="cv", 
#                         number=5,
#                         allowParallel = TRUE)
# 
# tunegrid <- expand.grid(.mtry=c(1:15),.ntree=c(1000,1500))
# set.seed(123)
# 
# custom_rf <- train(HOSPITAL_EXPIRE_FLAG~.,
#                    data = train_hr_rr, 
#                    method = customRF, 
#                    metric = 'Accuracy', 
#                    tuneGrid = tunegrid, 
#                    trControl = control)
# 
# print(custom_rf)
# plot(custom_rf)
# 
# rf_hr_rr <- randomForest(HOSPITAL_EXPIRE_FLAG~., data = train_hr_rr, keep.forest=FALSE,
#                       ntree=1000, importance=TRUE)
# 
# imp <- varImpPlot(rf_hr_rr)
# imp <- as.data.frame(imp)
# imp$varnames <- rownames(imp) # row names to column
# rownames(imp) <- NULL  
# 
# 
# library(ggplot2) 
# ggplot(imp, 
#        aes(x=reorder(varnames, -MeanDecreaseAccuracy), 
#            weight=MeanDecreaseAccuracy)) + 
#   geom_bar() +
#   scale_fill_discrete(name="Variable Group") +
#   ylab("MeanDecreaseAccuracy") +
#   xlab("") +
#   theme(axis.text = element_text(size = 10, face = "bold"))


