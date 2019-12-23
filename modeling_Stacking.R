# 5-Fold cross validation, attaning OOF predictions
# Create custom folds
CCU_hr_rr <- fread("CCU_hr_rr.csv")

CCU_hr_rr %<>% filter(Los <= 25) 
# CCU_hr_rr$mortality <- NULL
CCU_hr_rr$Gender <- NULL
CCU_hr_rr$Los <- NULL

CCU_hr_rr$HOSPITAL_EXPIRE_FLAG <- as.factor(CCU_hr_rr$HOSPITAL_EXPIRE_FLAG)
CCU_hr_rr %<>% mutate(HOSPITAL_EXPIRE_FLAG = as.factor(ifelse(HOSPITAL_EXPIRE_FLAG == "0", "Alive", "Death")))
CCU_hr_rr$HOSPITAL_EXPIRE_FLAG <- factor(CCU_hr_rr$HOSPITAL_EXPIRE_FLAG, levels = c("Death", "Alive"))

set.seed(2019)
library(caret)
ind <- createDataPartition(CCU_hr_rr$HOSPITAL_EXPIRE_FLAG, p=0.7, list=F)

# train_hr_nonsampling <- CCU_hr_rr %>% dplyr::slice(ind) %>% select(HOSPITAL_EXPIRE_FLAG, AGE, Max_hr:Kurt_hr)
# train_hr_rr_nonsampling <- CCU_hr_rr %>% dplyr::slice(ind)

test_hr_rr <- CCU_hr_rr %>% dplyr::slice(-ind)

train <- CCU_hr_rr %>% dplyr::slice(ind)

over_samp <- rbind(train %>% filter(HOSPITAL_EXPIRE_FLAG == "Death") %>% sample_n(3000, replace = T),
                   train %>% filter(HOSPITAL_EXPIRE_FLAG == "Alive"))
over_samp <- over_samp[sample(nrow(over_samp)),]

train_hr_rr <- over_samp


train_hr_rr$HOSPITAL_EXPIRE_FLAG <- factor(train_hr_rr$HOSPITAL_EXPIRE_FLAG, levels = c("Death", "Alive"))
test_hr_rr$HOSPITAL_EXPIRE_FLAG <- factor(test_hr_rr$HOSPITAL_EXPIRE_FLAG, levels = c("Death", "Alive"))

library(caret)
FOLDS = createFolds(train_hr_rr$HOSPITAL_EXPIRE_FLAG, k=5, returnTrain = T)
FOLDS_1 = setdiff(1:nrow(train_hr_rr), FOLDS$Fold1)
FOLDS_2 = setdiff(1:nrow(train_hr_rr), FOLDS$Fold2)
FOLDS_3 = setdiff(1:nrow(train_hr_rr), FOLDS$Fold3)
FOLDS_4 = setdiff(1:nrow(train_hr_rr), FOLDS$Fold4)
FOLDS_5 = setdiff(1:nrow(train_hr_rr), FOLDS$Fold5)

# Index test folds (later used)
FOLDS_TEST = list()
FOLDS_TEST$Fold1 = FOLDS_1
FOLDS_TEST$Fold2 = FOLDS_2
FOLDS_TEST$Fold3 = FOLDS_3
FOLDS_TEST$Fold4 = FOLDS_4
FOLDS_TEST$Fold5 = FOLDS_5

MySummary <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

control <- trainControl(method = "repeatedcv", 
                        number = 5,
                        index = FOLDS,
                        savePredictions = T,
                        summaryFunction = MySummary,
                        classProbs = T)

# first save OOF predictions of LR model
library(MLmetrics)
LR_hr_rr <- train(HOSPITAL_EXPIRE_FLAG ~.,
                  data = train_hr_rr,
                  method = "glm",
                  metric="Sens",
                  trControl = control,
                  family = binomial)

LR_OOF_Pred = LR_hr_rr$pred
LR_OOF_Pred$Resample = as.factor(LR_OOF_Pred$Resample)
LR_OOF_Pred = LR_OOF_Pred[order(LR_OOF_Pred$rowIndex),]

LR_final <- train(HOSPITAL_EXPIRE_FLAG ~.,
                  data = train_hr_rr,
                  method = "glm",
                  metric="Sens",
                  family = binomial)

# second save OOF predictions of RF model
library(randomForest)

RF_hr_rr <- train(HOSPITAL_EXPIRE_FLAG ~.,
                  data = train_hr_rr,
                  method = "rf",
                  metric="Sens",
                  trControl = control)

plot(RF_hr_rr)

RF_OOF_Pred = RF_hr_rr$pred %>% filter(mtry == 11)
RF_OOF_Pred$Resample = as.factor(RF_OOF_Pred$Resample)
RF_OOF_Pred = RF_OOF_Pred[order(RF_OOF_Pred$rowIndex),]
RF_final <- randomForest(HOSPITAL_EXPIRE_FLAG~., data = train_hr_rr, mtry = 11, ntree= 500)

# third save OOF predictions of RF model
library(xgboost)
xgb_matrix_train = xgb.DMatrix(as.matrix(train_hr_rr[, -1], label = as.numeric(train_hr_rr$HOSPITAL_EXPIRE_FLAG) - 1))
xgb_matrix_test = xgb.DMatrix(as.matrix(test_hr_rr[, -1], label = test_hr_rr$HOSPITAL_EXPIRE_FLAG))
dtrain <- as.matrix(train_hr_rr[, -1])  
label <- as.numeric(train_hr_rr$HOSPITAL_EXPIRE_FLAG) - 1
dtest <- as.matrix(test_hr_rr[, -1])

params <- data.frame(eta = 0.01, 
                     nrounds = 995,
                     gamma = 0.05,
                     max_depth = 7, 
                     min_child_weight = 4, 
                     subsample = 0.7, 
                     colsample_bytree = 0.5)

XG_hr_rr <- train(HOSPITAL_EXPIRE_FLAG ~.,
                  data = train_hr_rr,
                  method = "xgbTree",
                  metric = "Sens",
                  trControl = control)

XG_OOF_Pred = XG_hr_rr$pred %>% filter(nrounds == 150, max_depth ==3, eta == 0.4, gamma == 0, colsample_bytree == 0.8, min_child_weight == 1, subsample == 0.75)
XG_OOF_Pred$Resample = as.factor(XG_OOF_Pred$Resample)
XG_OOF_Pred = XG_OOF_Pred[order(XG_OOF_Pred$rowIndex),]
# XG_final <- xgboost(data = dtrain, label = label, nrounds = 1000, early_stopping_rounds = 150, objective = "binary:logistic", params = params) # early stopping

# XG_final <- xgboost(data = dtrain, label = label, nrounds = 995, objective = "binary:logistic", params = params)
xgControl <- trainControl(number=1, verboseIter=TRUE)
XG_final <- train(HOSPITAL_EXPIRE_FLAG ~.,
                  data = train_hr_rr,
                  method = "xgbTree",
                  tunegrid = params,
                  trControl = xgControl)

# KNN
KNN_hr_rr <- train(HOSPITAL_EXPIRE_FLAG ~.,
                  data = train_hr_rr,
                  method = "knn",
                  metric="Sens",
                  trControl = control,
                  preProcess = c("center","scale"),
                  tuneLength = 15)

# plot(KNN_hr)
# 
# probabilities_knn <- KNN_hr %>% predict(test_hr, type = "prob")
# predicted_classes <- as.factor(ifelse(probabilities_knn$Death >= 0.5, "Death", "Alive"))
# head(predicted_classes)
# 
# actual <- test_hr_rr %>% select(HOSPITAL_EXPIRE_FLAG) %>% unlist() 
# actual <- factor(actual, levels = c("Death", "Alive"))
# 
# confusionMatrix(predicted_classes, reference = actual, positive = "Death")
# confusionMatrix(predicted_classes, reference = actual, positive = "Death", mode = "prec_recall")
# auc(actual, probabilities_knn$Death)
KNN_OOF_Pred = KNN_hr_rr$pred %>% filter(k == 5)
KNN_OOF_Pred$Resample = as.factor(KNN_OOF_Pred$Resample)
KNN_OOF_Pred = KNN_OOF_Pred[order(KNN_OOF_Pred$rowIndex),]


library(ROCR)
library(e1071)
library(pROC)

# last save OOF predictions of ANN model
library(keras)
library(BBmisc)

# train_hr_rr %<>% mutate(HOSPITAL_EXPIRE_FLAG = ifelse(HOSPITAL_EXPIRE_FLAG == "Alive", 0, 1))
# test_hr_rr %<>% mutate(HOSPITAL_EXPIRE_FLAG = ifelse(HOSPITAL_EXPIRE_FLAG == "Alive", 0, 1))
# 
# 
# x_train_hr_rr <- train_hr_rr %>% dplyr::select( -HOSPITAL_EXPIRE_FLAG)%>% as.matrix()
# x_train_hr_rr %<>% normalize()
# y_train_hr_rr <- to_categorical(train_hr_rr$HOSPITAL_EXPIRE_FLAG, 2)
# 
# x_test_hr_rr <- test_hr_rr %>% dplyr::select( -HOSPITAL_EXPIRE_FLAG) %>% as.matrix()
# x_test_hr_rr %<>% normalize()
# y_test_hr_rr <- to_categorical(test_hr_rr$HOSPITAL_EXPIRE_FLAG, 2)
# 
# model <- keras_model_sequential()
# 
# model %>%
#   layer_dense(units = 256, activation = 'relu', input_shape = ncol(x_train_hr_rr)) %>%
#   layer_dropout(rate = 0.1) %>%
#   layer_dense(units = 128, activation = 'relu') %>%
#   layer_dropout(rate = 0.1) %>%
#   layer_dense(units = 64, activation = 'relu') %>%
#   # layer_dropout(rate = 0.1) %>%
#   layer_dense(units = 2, activation = 'sigmoid')
# 
# model %>% compile(
#   loss = 'binary_crossentropy',
#   optimizer = optimizer_rmsprop(),
#   metrics = c('accuracy')
# )
# 
# model %>% fit(
#   x_train_hr_rr, y_train_hr_rr,
#   epochs = 30,
#   batch_size = 100,
#   validation_split = 0.2
# )
# 
# NN_pred <- model %>% predict_proba(x_test_hr_rr)
# NN_pred <- NN_pred[,2]
# # probabilities <- RF_hr_rr %>% predict(test_hr_rr, type = "prob")
# predicted_classes <- as.factor(ifelse(probabilities_NN[,2] >= 0.5, "1", "0"))


ANN_hr_rr <- train(HOSPITAL_EXPIRE_FLAG ~.,
                   data = train_hr_rr,
                   method = "nnet",
                   # metric="Sens",
                   trControl = control,
                   preProcess = c("center","scale"),
                   # tuneGrid=expand.grid(size=c(10), decay=c(0.1)),
                   # maxit = 30,
                   # linout = TRUE,
                   trace = FALSE)

ANN_OOF_Pred = ANN_hr_rr$pred %>% filter(size == 5 & decay == 0)
ANN_OOF_Pred$Resample = as.factor(ANN_OOF_Pred$Resample)
ANN_OOF_Pred = ANN_OOF_Pred[order(ANN_OOF_Pred$rowIndex),]



# Level2 모델을 위한 데이터셋(OOF 예측값) 생성
stacking = as.data.frame(matrix(nrow=7988, ncol=5, NA))
names(stacking) = c("Logistic", "RandomForest", "XGBoost", "KNN", "ANN")
str(stacking)

stacking$Logistic <- LR_OOF_Pred$Death
stacking$RandomForest <- RF_OOF_Pred$Death
stacking$XGBoost <- XG_OOF_Pred$Death
stacking$KNN <- KNN_OOF_Pred$Death
stacking$ANN <- ANN_OOF_Pred$Death

stacking$HOSPITAL_EXPIRE_FLAG <- train_hr_rr$HOSPITAL_EXPIRE_FLAG
stacking %>% str()

stacking$AGE <- train_hr_rr$AGE
stacking$Range_hr <- train_hr_rr$Range_hr

# Stacking - Elastic Net
# grid search precess with level 2 model
# library(glmnet)
# elasticnet_result = as.data.frame(matrix(nrow = 11, ncol = 3, NA))
# names(elasticnet_result) = c("alpha", "lambda", "Accuracy")
# x = 1

control_final <- trainControl(method = "repeatedcv", 
                              number = 5,
                              savePredictions = T,
                              summaryFunction = MySummary,
                              classProbs = T)

# Elastic_stacking <- train(HOSPITAL_EXPIRE_FLAG ~.,
#                           data = stacking,
#                           method = "glmnet",
#                           metric = "Sens",
#                           trControl = control_final,
#                           tuneLength = 10) # alpha = 0.6, lambda = 0.005
# 
# Elastic_final <- glmnet(as.matrix(stacking[, -4]),
#                         stacking$HOSPITAL_EXPIRE_FLAG,
#                         family="binomial",
#                         alpha = 0.6, lambda = 0.005)

# Elastic_stacking <- train(HOSPITAL_EXPIRE_FLAG ~.,
#                           data = stacking,
#                           method = "rf",
#                           metric = "Sens",
#                           trControl = control_final,
#                           tuneLength = 4) # 
# plot(Elastic_stacking)

random_final <- randomForest(HOSPITAL_EXPIRE_FLAG~., data = stacking, mtry = 2, ntree= 500)



# Predict on holdout set for stacking using Level1 Models
LR_pred = predict(LR_final, newdata = test_hr_rr[, -1], type = "prob")[, 1]
RF_pred = predict(RF_final, newdata = test_hr_rr[, -1], type = "prob")[, 1]
XG_pred = predict(XG_hr_rr, newdata = test_hr_rr[, -1], type = 'prob')[, 1]
KNN_pred = predict(KNN_hr_rr, newdata = test_hr_rr[, -1], type = 'prob')[, 1]
ANN_pred = predict(ANN_hr_rr, newdata = test_hr_rr[, -1], type = 'prob')[, 1]


# NN_pred <- model %>% predict_proba(x_test_hr_rr)
# NN_pred <- NN_pred[,2]


predicted_classes_LR <- factor(ifelse(LR_pred >= 0.5, "Death", "Alive"), levels = c("Death", "Alive"))
predicted_classes_RF <- factor(ifelse(RF_pred >= 0.5, "Death", "Alive"), levels = c("Death", "Alive"))
predicted_classes_XG <- factor(ifelse(XG_pred >= 0.5, "Death", "Alive"), levels = c("Death", "Alive"))
predicted_classes_KNN <- factor(ifelse(KNN_pred >= 0.5, "Death", "Alive"), levels = c("Death", "Alive"))
predicted_classes_ANN <- factor(ifelse(ANN_pred >= 0.5, "Death", "Alive"), levels = c("Death", "Alive"))


actual <- factor(test_hr_rr %>% select(HOSPITAL_EXPIRE_FLAG) %>% unlist())
actual <- factor(actual, levels = c("Death", "Alive"))

confusionMatrix(predicted_classes_LR, reference = actual)
confusionMatrix(predicted_classes_RF, reference = actual)
confusionMatrix(predicted_classes_XG, reference = actual)
confusionMatrix(predicted_classes_KNN, reference = actual)
confusionMatrix(predicted_classes_ANN, reference = actual)


# confusionMatrix(predicted_classes_LR, reference = actual, mode = "prec_recall")
# confusionMatrix(predicted_classes_RF, reference = actual, mode = "prec_recall")
# confusionMatrix(predicted_classes_XG, reference = actual, mode = "prec_recall")
# 
# library(pROC)
# auc(actual, LR_pred)
# auc(actual, RF_pred)
# auc(actual, XG_pred)

# Dataset for final stacking to check on holdout set
stacking_final = as.data.frame(matrix(nrow = nrow(test_hr_rr), ncol = 5, NA))
names(stacking_final) = c("Logistic", "RandomForest", "XGBoost", "KNN", "ANN")
str(stacking_final)

stacking_final$Logistic = LR_pred
stacking_final$RandomForest = RF_pred
stacking_final$XGBoost = XG_pred
stacking_final$KNN = KNN_pred
stacking_final$ANN = ANN_pred
stacking_final$AGE = test_hr_rr$AGE
stacking_final$Range_hr = test_hr_rr$Range_hr

# predict on holdout set using Level2 model
holdout_prediction = predict(random_final, newdata = stacking_final, type = "prob")[, 1]
predicted_classes_Stacking <- factor(ifelse(holdout_prediction >= 0.5, "Death", "Alive"), levels = c("Death", "Alive"))

confusionMatrix(predicted_classes_Stacking, reference = actual)
confusionMatrix(predicted_classes_Stacking, reference = actual, mode = "prec_recall")
auc(actual, holdout_prediction)

################################################################################
# final comparing ROC curve
# library(ROCR)
# library(e1071)
# library(pROC)
# 
# # # Generate an ROC curve for the rf method
# predRF <- prediction(RF_pred, test_hr_rr$HOSPITAL_EXPIRE_FLAG)
# perfRF <- performance(predRF, "tpr", "fpr")
# plot(perfRF, main = "ROC curves for LR, RF, XG, KNN, ANN and Ensemble", col = 'brown')
# 
# # Generate an ROC curve for the LR method
# pred_LR <- prediction(LR_pred, test_hr_rr$HOSPITAL_EXPIRE_FLAG)
# perf_LR <- performance(pred_LR, "tpr", "fpr")
# plot(perf_LR, add = TRUE, main = "ROC curves for LR, RF, XG and ANN")
# 
# #Generate an ROC curve for the 'XG' method
# pred_XG <- prediction(XG_pred, test_hr_rr$HOSPITAL_EXPIRE_FLAG)
# perf_XG <- performance(pred_XG, "tpr", "fpr")
# plot(perf_XG, add = TRUE, col = "red")
# 
# #Generate an ROC curve for the 'ANN' method
# pred_NN <- prediction(ANN_pred, test_hr_rr$HOSPITAL_EXPIRE_FLAG)
# perf_NN <- performance(pred_NN, "tpr", "fpr")
# plot(perf_NN, add = TRUE, col = "steelblue")
# 
# #Generate an ROC curve for the 'KNN' method
# pred_KNN <- prediction(KNN_pred, test_hr_rr$HOSPITAL_EXPIRE_FLAG)
# perf_KNN <- performance(pred_KNN, "tpr", "fpr")
# plot(perf_KNN, add = TRUE, col = "purple")
# 
# #Generate an ROC curve for the 'stacking' method
# pred_stack <- prediction(holdout_prediction, test_hr_rr$HOSPITAL_EXPIRE_FLAG)
# perf_stack <- performance(pred_stack, "tpr", "fpr")
# plot(perf_stack, add = TRUE, col = "green")
# 
# # Add legends to the plot
# legend("right",
#        legend = c("Ensemble", "Randomforest", "XGboost", "ANN", "Logistic Regression", "KNN"),
#        # bty = "n",
#        cex = 1,
#        lty = 1,
#        col = c("brown", "black", "red", "green", "steelblue", "purple"))



# a1 <- ROC(form = HOSPITAL_EXPIRE_FLAG ~ AGE + Max_hr + Min_hr + Mean_hr + Median_hr + Mode_hr + Std_hr + Var_hr + Range_hr + Skew_hr + Kurt_hr,
#           data=train_hr_rr,
#           plot="ROC")
# 
# a2 <- ROC(form = HOSPITAL_EXPIRE_FLAG ~.,
#           data=train_hr_rr,
#           plot="ROC")
# 
# plot_ROC(a1,a2)









