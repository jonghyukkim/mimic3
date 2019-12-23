# LR
CCU_hr_rr$HOSPITAL_EXPIRE_FLAG <- as.factor(CCU_hr_rr$HOSPITAL_EXPIRE_FLAG)
CCU_hr_rr %<>% mutate(HOSPITAL_EXPIRE_FLAG = as.factor(ifelse(HOSPITAL_EXPIRE_FLAG == "0", "Alive", "Death")))
CCU_hr_rr$HOSPITAL_EXPIRE_FLAG <- factor(CCU_hr_rr$HOSPITAL_EXPIRE_FLAG, levels = c("Death", "Alive"))

set.seed(2019)
library(caret)
ind <- createDataPartition(CCU_hr_rr$HOSPITAL_EXPIRE_FLAG, p=0.7, list=F)

# train_hr_nonsampling <- CCU_hr_rr %>% dplyr::slice(ind) %>% select(HOSPITAL_EXPIRE_FLAG, AGE, Max_hr:Kurt_hr)
# train_hr_rr_nonsampling <- CCU_hr_rr %>% dplyr::slice(ind)

test_hr <- CCU_hr_rr %>% dplyr::slice(-ind) %>% select(HOSPITAL_EXPIRE_FLAG, AGE, Max_hr:Kurt_hr)
test_hr_rr <- CCU_hr_rr %>% dplyr::slice(-ind)

train <- CCU_hr_rr %>% dplyr::slice(ind)

over_samp <- rbind(train %>% filter(HOSPITAL_EXPIRE_FLAG == "Death") %>% sample_n(3000, replace = T),
                   train %>% filter(HOSPITAL_EXPIRE_FLAG == "Alive"))
over_samp <- over_samp[sample(nrow(over_samp)),]


# str(over_samp)
# str(test_hr_rr)
# over_samp$HOSPITAL_EXPIRE_FLAG %>% table()
# str(CCU_hr_rr)


# only hr

# ind <- createDataPartition(over_samp$HOSPITAL_EXPIRE_FLAG, p=0.8, list=F)
train_hr <- over_samp %>% select(HOSPITAL_EXPIRE_FLAG, AGE, Max_hr:Kurt_hr)
train_hr_rr <- over_samp


MySummary <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

control <- trainControl(method = "repeatedcv", 
                        number = 5,
                        savePredictions = T,
                        summaryFunction = MySummary,
                        classProbs = T)

library(MLmetrics)
LR_hr <- train(HOSPITAL_EXPIRE_FLAG ~.,
               data = train_hr,
               method = "glm",
               metric = "Sens",
               trControl = control,
               family = binomial)

LR_hr_rr <- train(HOSPITAL_EXPIRE_FLAG ~.,
                  data = train_hr_rr,
                  method = "glm",
                  metric = "Sens",
                  trControl = control,
                  family = binomial)

probabilities <- LR_hr %>% predict(test_hr, type = "prob")
predicted_classes <- as.factor(ifelse(probabilities$Death >= 0.5, "Death", "Alive"))
head(predicted_classes)

actual <- test_hr_rr %>% select(HOSPITAL_EXPIRE_FLAG) %>% unlist() 
actual <- factor(actual, levels = c("Death", "Alive"))

confusionMatrix(predicted_classes, reference = actual, positive = "Death")
confusionMatrix(predicted_classes, reference = actual, positive = "Death", mode = "prec_recall")
auc(actual, probabilities$Death)


