
# XG
library(xgboost)
library(caret)
library(dplyr)


# xgb_grid <- expand.grid(max_depth = seq(4, 8),
#                         eta = c(0.01, 0.3, 1),
#                         gamma = c(0.0, 0.05, 0.2, 1),
#                         colsample_bytree = c(0.5, 0.7, 0.8, 1),
#                         min_child_weight = seq(3, 7),
#                         subsample = c(0.3, 0.5, 0.7, 0.9),
#                         nrounds = 1000)
# 
# # 5-fold cv using caret package
# control <- trainControl(method = "repeatedcv", 
#                         number = 5,
#                         savePredictions = T,
#                         summaryFunction = MySummary,
#                         classProbs = T)
# 
# # Grid search
# xgb_train <- train(HOSPITAL_EXPIRE_FLAG~.,
#                    data = train_hr_rr,
#                    method = "xgbTree",
#                    tuneGrid = xgb_grid,
#                    trControl = control,
#                    maximize = F,
#                    prediction = T) 
# acc <- xgb_train$results
# acc %>% arrange(desc(Accuracy)) %>% dplyr::slice(1)


# find optimal nrounds
# xgb_cv_optiaml <- xgb.cv(params = params,
#                          data = dtrain,
#                          nrounds = 10000,
#                          maximize = F,
#                          prediction = T,
#                          early_stopping_rounds = 150)


params <- expand.grid(eta = 0.01, 
                      nrounds = 1000,
                      gamma = 0.05, 
                      max_depth = 7, 
                      min_child_weight = 4, 
                      subsample = 0.7, 
                      colsample_bytree = 0.5)

# 
# XG_hr <- train(HOSPITAL_EXPIRE_FLAG ~.,
#                data = train_hr,
#                method = "xgbTree",
#                trControl = control,
#                tuneGrid = params)

XG_hr_rr <- train(HOSPITAL_EXPIRE_FLAG ~.,
                  data = train_hr_rr,
                  method = "xgbTree",
                  trControl = control,
                  tuneGrid = params)

probabilities_XG <- XG_hr_rr %>% predict(test_hr_rr, type = "prob")
predicted_classes <- as.factor(ifelse(probabilities_XG$Death >= 0.5, "Death", "Alive"))
# head(predicted_classes)

actual <- test_hr_rr %>% select(HOSPITAL_EXPIRE_FLAG) %>% unlist() 
actual <- factor(actual, levels = c("Death", "Alive"))

confusionMatrix(predicted_classes, reference = actual, positive = "Death")
confusionMatrix(predicted_classes, reference = actual, positive = "Death", mode = "prec_recall")
auc(actual, probabilities_XG$Death)

# xgimp <- varImp(XG_hr_rr, scale = T)
# ggplot(xgimp) +
#   theme_minimal() +
#   ggtitle("XG Importance Plot") +
#   theme(axis.text.x = element_text(size = 11, face = "bold"),
#         axis.text.y = element_text(size = 11, face = "bold"),
#         plot.title = element_text(size = 13, face = "bold"))
# 
# rfimp <- varImp(RF_hr_rr, scale=T)
# ggplot(rfimp) +
#   theme_minimal() +
#   ggtitle("RF Importance Plot") +
#   theme(axis.text.x = element_text(size = 11, face = "bold"),
#         axis.text.y = element_text(size = 11, face = "bold"),
#         plot.title = element_text(size = 13, face = "bold"))


# ################## visualization of grid search ###################
# result_vis <- xgb_train$result %>% mutate( 
#   Depth= paste('Depth',max_depth,sep = ':') %>% as.factor,
#   MinChild= paste('MinChild', min_child_weight, sep = ':') %>% as.factor)
# 
# ggplot(result_vis,aes(x=subsample,y=colsample_bytree))+ 
#   geom_tile(aes(fill=Accuracy)) + 
#   scale_fill_gradient(low = "lightblue", high = "black")+
#   facet_grid(Depth ~ MinChild)+
#   labs(x="SubSampRate",
#        y="ColSampRate",
#        title = "XGboost Grid Search", 
#        subtitle="Maxdepth, Minchild, Colsample, Subsample")
