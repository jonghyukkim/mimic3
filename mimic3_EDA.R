## 
rm(list=ls())

# inslib function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

inslib <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("ggplot2", "data.table", "tidyverse", "RColorBrewer", "lubridate", 
              "magrittr", "lsr", "moments", "corrplot", "moonBook", "caret", "MLmetrics",
              "randomForest", "xgboost", "ROCR", "e1071", "pROC", "keras", "BBmisc")

inslib(packages)


#-------------- patient variables : Defines each SUBJECT_ID in the database
# SUBJECT_ID is a unique identifier which specifies an individual patient
# GENDER is the genotypical sex of the patient
# DOB is the date of birth of the given patient
# DOD is the date of death for the given patient
# DOD_HOSP is the date of death as recorded in the hospital database
# DOD_SSN is the date of death from the social security database
# EXPIRE_FLAG is a binary flag which indicates whether the patient died, i.e. whether DOD is null or not

# EDA - Patient
# patient %>% str()
# patient$SUBJECT_ID %>% unique() %>% length() # 46520
# table(patient$GENDER) # F:20399 M:26121
# table(patient$EXPIRE_FLAG) # 0:30761 1:15759


#-----------------------------------------------------------------------------------------------------------------------------------------------

#-------------- icustay variables : Defines each ICUSTAY_ID in the database
# SUBJECT_ID is unique to a patient
# HADM_ID is unique to a patient hospital stay
# ICUSTAY_ID is unique to a patient ICU stay
# DBSOURCE contains the original ICU database the data was sourced from
# FIRST_CAREUNIT and LAST_CAREUNIT contain, respectively, the first and last ICU type in which the patient was cared for
# FIRST_WARDID and LAST_WARDID contain the first and last ICU unit in which the patient stayed
# INTIME provides the date and time the patient was transferred into the ICU
# OUTTIME provides the date and time the patient was transferred out of the ICU
# LOS is the length of stay for the patient for the given ICU stay

# EDA - icustay
# icustay %>% str()
# icustay$SUBJECT_ID %>% unique() %>% length() # 46470
# icustay$HADM_ID %>% unique() %>% length() # 57786 (같은 환자 병원 중복 방문)
# icustay$ICUSTAY_ID %>% unique() %>% length() # 61532 (같은 환자 중환자실 중복 방문)
# 
# table(patient$GENDER) # F:20399 M:26121
# table(patient$EXPIRE_FLAG) # 0:30761 1:15759

#-----------------------------------------------------------------------------------------------------------------------------------------------

#-------------- admission variables : Define a patient’s hospital admission, HADM_ID
# covering an admission period between 1 June 2001 and 10 October 2012.
# 한 명의 환자가 여러 개의 HADM_ID를 가질 수 있음 (여러 번 방문할 시 방문시마다 다른 HADM_ID 부여)
# ADMITTIME provides the date and time the patient was admitted to the hospital
# DISCHTIME provides the date and time the patient was discharged from the hospital
# DEATHTIME provides the time of in-hospital death for the patient only present if the patient died in-hospital
# DEATHTIME  is almost always the same as the patient’s DISCHTIME
# ADMISSION_TYPE describes the type of the admission: ‘ELECTIVE’, ‘URGENT’, ‘NEWBORN’ or ‘EMERGENCY’
# ADMISSION_LOCATION provides information about the previous location of the patient prior to arriving at the hospital
# INSURANCE, LANGUAGE, RELIGION, MARITAL_STATUS, ETHNICITY columns describe patient demographics
# EDREGTIME, EDOUTTIME is time that the patient was registered and discharged from the emergency department.


#-------------- chartevent variables : Contains all charted data for all patients
# ITEMID : Identifier for a single measurement type in the database
# CHARTTIME records the time at which an observation was made, and is usually the closest proxy to the time the data was actually measured
# STORETIME records the time at which an observation was manually input or manually validated by a member of the clinical staff
# CGID is the identifier for the caregiver who validated the given measurement
# VALUE contains the value measured for the concept identified by the ITEMID
# VALUENUM contains the same data in a numeric format. If this data is not numeric, VALUENUM is null
# VALUEUOM is the unit of measurement for the VALUE, if appropriate
# WARNING and ERROR are Metavision specific columns which specify if a warning for the value was raised and if an error occurred during the measurement
# RESULTSTATUS and STOPPED are CareVue specific columns which specify the type of measurement (RESULTSTATUS is ‘Manual’ or ‘Automatic’) and whether the measurement was stopped
# DIAGNOSIS column provides a preliminary, free text diagnosis for the patient on hospital admission
# 15,693 distinct diagnoses for 58,976 admissions
# Final diagnoses for a patient’s hospital stay are coded on discharge and can be found in the DIAGNOSES_ICD table
# HOSPITAL_EXPIRE_FLAG indicates whether the patient died within the given hospitalization. 1 indicates death


#
# library(data.table)
# library(tidyverse)
# library(lubridate)
# library(magrittr)
# 
# # Data load
# patient <- fread("PATIENTS.csv")
# admission <- fread("ADMISSIONS.csv")
# icustay <- fread("ICUSTAYS.csv")
# 
# 
# # Useless feature
# patient %<>% select(-ROW_ID) 
# admission %<>% select(-ROW_ID) 
# icustay %<>% select(-ROW_ID) 
# 
# # patient with admmision
# pat_adm <- left_join(admission, patient, by = "SUBJECT_ID")
# pat_adm %>% 
#   arrange(SUBJECT_ID) %>% 
#   select(SUBJECT_ID, HADM_ID, ADMITTIME, DISCHTIME, HOSPITAL_EXPIRE_FLAG, EXPIRE_FLAG)
# # EXPIRE_FLAG는 최종적으로 환자가 사망하였으면 1 아니면 0
# # HOSPITAL_EXPIRE_FLAG는 중환자실에 방문하였을 때의 사망 여부 
# # 즉, 첫번째 방문시 HOSPITAL_EXPIRE_FLAG는 0이지만 두번째 방문시 HOSPITAL_EXPIRE_FLAG가 1이라면 최종적으로 EXPIRE_FLAG는 1
# 
# pat_adm$ADMITTIME <- as_datetime(pat_adm$ADMITTIME)
# pat_adm$DOB <- as_datetime(pat_adm$DOB)
# 
# # first admmision date
# first_adm <- pat_adm %>% 
#   group_by(SUBJECT_ID) %>%
#   summarise(FIRST_ADMIT = min(ADMITTIME))
# 
# pat_adm <- left_join(pat_adm, first_adm, by = "SUBJECT_ID")
# 
# # age calculation
# pat_adm %<>% 
#   select(SUBJECT_ID, HADM_ID, ADMITTIME, GENDER, DOB, FIRST_ADMIT, HOSPITAL_EXPIRE_FLAG) %>% 
#   mutate(AGE = round(as.numeric(difftime(pat_adm$FIRST_ADMIT, pat_adm$DOB, 
#                                          unit="weeks"))/52.25))
# 
# # patient & admmision & icustay
# pat_adm_icu <- left_join(icustay, pat_adm, by = c("SUBJECT_ID", "HADM_ID")) 
# pat_adm_icu %>% filter(FIRST_CAREUNIT == "CCU") # 7726 // unique pt : 6802 // unique HADM : 7517
# 
# 
# # Chartevent
# rm(admission, first_adm, icustay, pat_adm, patient)
# chartevent <- fread("chartevent_vs.csv")
# chartevent %<>% select(-ROW_ID.x, -ROW_ID.y) 
# # colSums(is.na(chartevent))
# 
# 
# 
# 
# # Preprocess: vital sign : 오류값 및 이상치 처리
# # 1. Heart rate - ITEMID: 211,220045, outlier: <= 0, >=300 (NA)
# #           obs : 7,943,034
# #           NA 개수: 1,446(기존) + 2,520 (outlier) = 3,966
# 
# 
# # # 2. Respiratory rate - ITEMID: 615,618,220210,224690, outlier: <= 0, >= 70 (NA)
# # #         obs : 6,940,625
# # #         NA 개수: 6,158(기존) +  46,251(outlier) = 52,409
# 
# 
# library(lsr)
# library(moments)
# 
# hr_rr <- chartevent[ITEMID == 211 | ITEMID == 220045 | ITEMID == 615 | ITEMID == 618 | ITEMID == 220210 | ITEMID == 224690]
# rm(chartevent)
# gc()
# 
# total_join <- left_join(hr_rr, pat_adm_icu, by = c("SUBJECT_ID", "HADM_ID", "ICUSTAY_ID"))
# total_join %<>% as.data.table() 
# 
# total_join$INTIME <- as_datetime(total_join$INTIME)
# total_join$CHARTTIME <- as_datetime(total_join$CHARTTIME)
# total_join$STORETIME <- as_datetime(total_join$STORETIME)
# 
# 
# total_join %<>% 
#   mutate(charttime_duration = round(difftime(total_join$CHARTTIME, total_join$INTIME, units = "hours"), 1))
# total_join %<>% 
#   mutate(charttime_duration2 = round(difftime(total_join$STORETIME, total_join$INTIME, units = "hours"), 1))
# 
# total_join %<>% as.data.table()
# 
# hr <- total_join[charttime_duration < 7 | charttime_duration2 < 7
#                  ][ITEMID == 211 | ITEMID == 220045
#                    ][!is.na(VALUENUM)
#                      ][!VALUENUM <= 0 & !VALUENUM >= 300
#                        ][, .(Max_hr = max(VALUENUM, na.rm = T),
#                              Min_hr = min(VALUENUM, na.rm = T),
#                              Mean_hr = mean(VALUENUM, na.rm = T),
#                              Median_hr = median(VALUENUM, na.rm = T),
#                              Mode_hr = modeOf(VALUENUM, na.rm = T)[1],
#                              Std_hr = ifelse(is.na(sd(VALUENUM, na.rm = T)), 0, sd(VALUENUM, na.rm = T)),
#                              Var_hr = ifelse(is.na(var(VALUENUM, na.rm = T)), 0, var(VALUENUM, na.rm = T)),
#                              Range_hr = max(VALUENUM, na.rm = T) - min(VALUENUM, na.rm = T),
#                              Skew_hr = ifelse(is.nan(skewness(VALUENUM, na.rm = T)), 0, skewness(VALUENUM, na.rm = T)), 
#                              Kurt_hr = ifelse(is.nan(kurtosis(VALUENUM, na.rm = T)), 0, kurtosis(VALUENUM, na.rm = T))), by = .(SUBJECT_ID, HADM_ID, ICUSTAY_ID)]
# colSums(is.na(hr))
# 
# rr <- total_join[charttime_duration < 7 | charttime_duration2 < 7
#                  ][ITEMID == 615 | ITEMID == 618 | ITEMID == 220210 | ITEMID == 224690
#                    ][!is.na(VALUENUM)
#                      ][!VALUENUM <= 0 & !VALUENUM >= 70
#                        ][, .(Max_rr = max(VALUENUM, na.rm = T),
#                              Min_rr = min(VALUENUM, na.rm = T),
#                              Mean_rr = mean(VALUENUM, na.rm = T),
#                              Median_rr = median(VALUENUM, na.rm = T),
#                              Mode_rr = modeOf(VALUENUM, na.rm = T)[1],
#                              Std_rr = ifelse(is.na(sd(VALUENUM, na.rm = T)), 0, sd(VALUENUM, na.rm = T)),
#                              Var_rr = ifelse(is.na(var(VALUENUM, na.rm = T)), 0, var(VALUENUM, na.rm = T)),
#                              Range_rr = max(VALUENUM, na.rm = T) - min(VALUENUM, na.rm = T),
#                              Skew_rr = ifelse(is.nan(skewness(VALUENUM, na.rm = T)), 0, skewness(VALUENUM, na.rm = T)), 
#                              Kurt_rr = ifelse(is.nan(kurtosis(VALUENUM, na.rm = T)), 0, kurtosis(VALUENUM, na.rm = T))), by = .(SUBJECT_ID, HADM_ID, ICUSTAY_ID)] %>% view()
# colSums(is.na(rr))
# 
# set_1 <- left_join(pat_adm_icu, hr, by = c("SUBJECT_ID", "HADM_ID", "ICUSTAY_ID"))
# set_2 <- left_join(pat_adm_icu, rr, by = c("SUBJECT_ID", "HADM_ID", "ICUSTAY_ID"))
# colSums(is.na(set_1))
# colSums(is.na(set_2))
# 
# 
# CCU_hr <- set_1 %>% filter(FIRST_CAREUNIT == "CCU" | LAST_CAREUNIT == "CCU") 
# CCU_rr <- set_2 %>% filter(FIRST_CAREUNIT == "CCU" | LAST_CAREUNIT == "CCU") 
# 
# colSums(is.na(CCU_hr)) # 255
# colSums(is.na(CCU_rr)) # 259
# 
# CCU_hr_new <- na.omit(CCU_hr)
# CCU_rr_new <- na.omit(CCU_rr)
# 
# CCU_hr_rr <- inner_join(CCU_hr_new, CCU_rr_new, by = c("SUBJECT_ID", "HADM_ID", "ICUSTAY_ID"))
# colSums(is.na(CCU_hr_rr)) # 0
# 
# table(CCU_hr_rr$HOSPITAL_EXPIRE_FLAG.y)
# CCU_hr_rr %<>% select(-ROW_ID) 
# CCU_hr_rr$SUBJECT_ID %>% unique() %>% length() # 7144
# 
# CCU_hr_rr_tidy <- CCU_hr_rr %>% select(HOSPITAL_EXPIRE_FLAG = HOSPITAL_EXPIRE_FLAG.x, 
#                                        AGE = AGE.x, 
#                                        Gender = GENDER.x,
#                                        Los = LOS.x,
#                                        Max_hr:Kurt_hr,
#                                        Max_rr:Kurt_rr)
# 
# CCU_hr_rr_tidy %<>% mutate(AGE = ifelse(AGE > 89, round(runif(1, 90, 100)), AGE))
# # fwrite(CCU_hr_rr_tidy, "CCU_hr_rr_final.csv")
# 
# 
# library(corrplot)
# M <- round(cor(CCU_hr_rr_tidy %>% select(-HOSPITAL_EXPIRE_FLAG)),1)
# 
# cor.mtest <- function(mat, ...) {
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat<- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       tmp <- cor.test(mat[, i], mat[, j], ...)
#       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#     }
#   }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# # matrix of the p-value of the correlation
# p.mat <- cor.mtest(CCU_hr_rr_tidy %>% select(-HOSPITAL_EXPIRE_FLAG))
# 
# 
# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(M, method="color", col=col(200),  
#          type="upper", order="hclust", 
#          addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, #Text label color and rotation
#          # Combine with significance
#          p.mat = p.mat, sig.level = 0.01, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE 
# )


# fwrite(CCU_hr_rr_tidy, "CCU_hr_rr.csv")
CCU_hr_rr <- fread("CCU_hr_rr.csv")



# EDA 
#1) 성별, 연령별 분포 check
CCU_hr_rr[, .N, by = .(Gender, AGE)] %>%
  ggplot(aes(x = AGE, y = N)) +
  geom_col(aes(fill = as.factor(Gender))) +
  scale_fill_brewer(palette="Dark2") +
  labs(fill = "Gender") +
  ggtitle("성별 연령별 분포") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_blank()) +
  theme(axis.title.x=element_blank())

#2)LOS(Length of Stay)
CCU_hr_rr %>%
  ggplot() +
  geom_histogram(aes(x=Los), color = "skyblue", fill = "skyblue", bins=50, alpha = 0.7) +
  scale_x_continuous(breaks = c(0,3,5,10,15,25,50,100)) +
  ggtitle("LOS(Length of Stay) distribution") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_blank()) +
  theme(axis.title.x=element_blank())

CCU_hr_rr %>%
  ggplot(aes("", Los)) +
  geom_boxplot(varwidth = T, fill = "skyblue") +
  geom_hline(yintercept = 25, color = "coral", size = 1.5) +
  theme_minimal() +
  labs(title = "Boxplot of LOS",
       x = "",
       y = "LOS(Length of Stay)") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))

#3)Mortality rate
CCU_hr_rr$HOSPITAL_EXPIRE_FLAG <- as.factor(CCU_hr_rr$HOSPITAL_EXPIRE_FLAG)
CCU_hr_rr %<>% mutate(mortality = ifelse(HOSPITAL_EXPIRE_FLAG == "1", "Death", "Alive"))
CCU_hr_rr %<>% as.data.table()

CCU_hr_rr[, .N, by = mortality] %>%
  mutate(prob = round(N/sum(N),2)*100) %>%
  ggplot(aes(x = "", y = N, fill = factor(mortality))) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(prob, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("lavender", "lightgrey")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Rate of mortality") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))


CCU_hr_rr %<>% filter(Los <= 25) 
# CCU_hr_rr$mortality <- NULL
CCU_hr_rr$Gender <- NULL
CCU_hr_rr$Los <- NULL

# Demographic
library(moonBook)
mytable(mortality ~ ., data = CCU_hr_rr %>% select(-HOSPITAL_EXPIRE_FLAG))
CCU_hr_rr %<>% select(-mortality)



