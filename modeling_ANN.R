
# ANN

library(keras)
library(BBmisc)

train_hr %<>% mutate(HOSPITAL_EXPIRE_FLAG = ifelse(HOSPITAL_EXPIRE_FLAG == "Alive", 0, 1))
test_hr %<>% mutate(HOSPITAL_EXPIRE_FLAG = ifelse(HOSPITAL_EXPIRE_FLAG == "Alive", 0, 1))
train_hr_rr %<>% mutate(HOSPITAL_EXPIRE_FLAG = ifelse(HOSPITAL_EXPIRE_FLAG == "Alive", 0, 1))
test_hr_rr %<>% mutate(HOSPITAL_EXPIRE_FLAG = ifelse(HOSPITAL_EXPIRE_FLAG == "Alive", 0, 1))


# x_train_hr <- train_hr %>% dplyr::select( -HOSPITAL_EXPIRE_FLAG)%>% as.matrix()
# y_train_hr <- to_categorical(train_hr$HOSPITAL_EXPIRE_FLAG, 2)
# 
# x_test_hr <- test_hr %>% dplyr::select( -HOSPITAL_EXPIRE_FLAG) %>% as.matrix()
# y_test_hr <- to_categorical(test_hr$HOSPITAL_EXPIRE_FLAG, 2)

x_train_hr_rr <- train_hr_rr %>% dplyr::select( -HOSPITAL_EXPIRE_FLAG)%>% as.matrix()
x_train_hr_rr %<>% normalize()
y_train_hr_rr <- to_categorical(train_hr_rr$HOSPITAL_EXPIRE_FLAG, 2)

x_test_hr_rr <- test_hr_rr %>% dplyr::select( -HOSPITAL_EXPIRE_FLAG) %>% as.matrix()
x_test_hr_rr %<>% normalize()
y_test_hr_rr <- to_categorical(test_hr_rr$HOSPITAL_EXPIRE_FLAG, 2)


model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(x_train_hr_rr)) %>% 
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 64, activation = 'relu') %>% 
  # layer_dropout(rate = 0.1) %>%
  layer_dense(units = 2, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

model %>% fit(
  x_train_hr_rr, y_train_hr_rr, 
  epochs = 30, 
  batch_size = 100,
  validation_split = 0.2
)


probabilities_NN <- model %>% predict_proba(x_test_hr_rr)
# probabilities <- RF_hr_rr %>% predict(test_hr_rr, type = "prob")
predicted_classes <- as.factor(ifelse(probabilities_NN[,2] >= 0.5, "1", "0"))
# head(predicted_classes)

actual_nn <- test_hr_rr %>% select(HOSPITAL_EXPIRE_FLAG) %>% unlist()
actual_nn <- factor(actual_nn)

confusionMatrix(predicted_classes, reference = actual_nn, positive = "1")
confusionMatrix(predicted_classes, reference = actual_nn, positive = "1", mode = "prec_recall")
auc(actual_nn, probabilities_NN[,2])
