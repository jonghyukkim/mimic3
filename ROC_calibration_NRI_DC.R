require(Epi)
require(pROC)
require(ztable)
require(moonBook)
library(e1071) 
library(ROCR)

source("ROC_sub.R")


# Compare ROC
# # Generate an ROC curve for the rf method
predRF <- prediction(probabilities_RF[,2], test_hr_rr$HOSPITAL_EXPIRE_FLAG)
perfRF <- performance(predRF, "tpr", "fpr")
plot(perfRF, main = "ROC curves for LR, RF, XG and ANN models")

# Generate an ROC curve for the LR method
pred_LR <- prediction(probabilities_LR[,2], test_hr_rr$HOSPITAL_EXPIRE_FLAG)
perf_LR <- performance(pred_LR, "tpr", "fpr")
plot(perf_LR, add = TRUE, col = "blue")

#Generate an ROC curve for the 'xg' method
pred_XG <- prediction(probabilities_XG[,2], test_hr_rr$HOSPITAL_EXPIRE_FLAG)
perf_XG <- performance(pred_XG, "tpr", "fpr")
plot(perf_XG, add = TRUE, col = "red")

#Generate an ROC curve for the 'ann' method
pred_NN <- prediction(probabilities_NN[,2], test_hr_rr$HOSPITAL_EXPIRE_FLAG)
perf_NN <- performance(pred_NN, "tpr", "fpr")
plot(perf_NN, add = TRUE, col = "green")

# Add legends to the plot
legend("right", 
       legend = c("RandomForest", "Logistic Regression", "XGboost", "ANN"), 
       # bty = "n", 
       cex = 1, 
       lty = 1,
       col = c("black", "blue", "red", "green"))

#######################
a1 <- ROC(form = HOSPITAL_EXPIRE_FLAG ~ AGE + Max_hr + Min_hr + Mean_hr + Median_hr + Mode_hr + Std_hr + Var_hr + Range_hr + Skew_hr + Kurt_hr, 
         data=train_hr_rr,
         plot="ROC")

a2 <- ROC(form = HOSPITAL_EXPIRE_FLAG ~., 
          data=train_hr_rr,
          plot="ROC")

plot_ROC(a1,a2, show.AUC = F)

train_hr_rr_nonsampling$HOSPITAL_EXPIRE_FLAG <- factor(train_hr_nonsampling$HOSPITAL_EXPIRE_FLAG, levels = c("Alive","Death"))
train_hr_rr_nonsampling$HOSPITAL_EXPIRE_FLAG <- as.numeric(train_hr_rr_nonsampling$HOSPITAL_EXPIRE_FLAG) - 1

a1 <- ROC(form = HOSPITAL_EXPIRE_FLAG ~ AGE + Max_hr + Min_hr + Mean_hr + Median_hr + Mode_hr + Std_hr + Var_hr + Range_hr + Skew_hr + Kurt_hr, 
          data=train_hr_rr_nonsampling,
          plot="ROC")

a2 <- ROC(form = HOSPITAL_EXPIRE_FLAG ~., 
          data=train_hr_rr_nonsampling,
          plot="ROC")

plot_ROC(a1,a2)


# Decision Curve
library(rmda)
set.seed(123)
train_hr_rr %<>% as.tibble() 
#first use rmda with the default settings (set bootstraps = 50 here to reduce computation time). 
baseline.model <- decision_curve(HOSPITAL_EXPIRE_FLAG~AGE + Max_hr + Min_hr + Mean_hr + Median_hr + Mode_hr + Std_hr + Var_hr + Range_hr + Skew_hr + Kurt_hr +
                                   Max_rr + Min_rr + Mean_rr + Median_rr + Mode_rr + Std_rr + Var_rr + Range_rr + Skew_rr + Kurt_rr, #fitting a logistic model
                                 data = train_hr_rr, 
                                 study.design = "cohort", 
                                 policy = "opt-in",  #default 
                                 bootstraps = 50)

#plot the curve
plot_decision_curve(baseline.model,  
                    curve.names = "HOSPITAL_EXPIRE_FLAG with HR and RR", 
                    legend.position = "topright", 
                    standardize = F)


# Calibration
library(gbm)

## Generate the test set results
lift_results <- data.frame(Class = actual) 
rownames(lift_results) <- NULL
lift_results$LR <- probabilities_LR$Alive
lift_results$RF <- probabilities_RF$Alive
lift_results$XG <- probabilities_XG$Alive
lift_results$ANN <- probabilities_NN[,1]

head(lift_results)

trellis.par.set(caretTheme())
cal_obj <- calibration(Class ~ LR + RF + XG + ANN,
                       data = lift_results,
                       cuts = 4)

plot(cal_obj, type = "l", auto.key = list(columns = 4,
                                          lines = TRUE,
                                          points = FALSE))
ggplot(cal_obj)

################################################################################################################3

# Model predicted probabilties
train_hr_rr$m1_pred <- probabilities_LR$Death


train_hr_rr$m2_pred <- probabilities_LR1$Death
test_hr_rr$XG_pred <- probabilities_XG$Death
test_hr_rr$ANN_pred <- probabilities_NN[,2]

# Metrics  
library(rms)
val_m1 <- rms::val.prob(probabilities_LR$Death, as.numeric(actual) - 1, 
                        pl = FALSE) %>% round(3)
val_m2 <- rms::val.prob(probabilities_LR1$Death, as.numeric(actual) - 1, 
                        pl = FALSE) %>% round(3)

val_m2 <- rms::val.prob(probabilities_RF$Death, as.numeric(actual) - 1, 
                        pl = FALSE) %>% round(3)
val_m3 <- rms::val.prob(probabilities_XG$Death, as.numeric(actual) - 1, 
                        pl = FALSE) %>% round(3)
val_m4 <- rms::val.prob(probabilities_NN[,2], as.numeric(actual) - 1, 
                        pl = FALSE) %>% round(3)

rescale_brier <- function(x, p, ...){ 
  format(round(1 - (x / (mean(p) * (1 - mean(p)))), digits = 2), nsmall = 2)
}

b1 <- rescale_brier(val_m1["Brier"], 0.106) 
b2 <- rescale_brier(val_m2["Brier"], 0.106)
b3 <- rescale_brier(val_m3["Brier"], 0.106)
b4 <- rescale_brier(val_m4["Brier"], 0.106)
# Note: 0.106 is the marginal probabilty of Death in the entire sample


set.seed(48572)

boot_val <- function(data, formula, ...){
  out <- list()
  for(i in 1:500){
    df <- sample_n(data, nrow(data), replace = TRUE)
    md <- glm(formula, data = df, family = binomial)
    out[[i]] <- rms::val.prob(predict(md, type = "response"),
                              as.numeric(df$HOSPITAL_EXPIRE_FLAG) - 1, 
                              pl = FALSE) %>% round(3)
  }
  return(out)
}


boot_vals_m1 <- boot_val(train_hr,  as.formula("HOSPITAL_EXPIRE_FLAG ~ ."))
boot_vals_m2 <- boot_val(train_hr_rr,  as.formula("HOSPITAL_EXPIRE_FLAG ~ ."))


boot_vals_m3 <- boot_val(data,  as.formula("alive ~  1 +        z_homr "))
boot_vals_m4 <- boot_val(data2, formula_4) 


calc_ci <- function(metric, boot_vals, n){
  x <- unlist(map(boot_vals, `[`, c(metric)))
  # if(metric == 'Brier'){x <- as.numeric(rescale_brier(x, 0.184))}
  paste0("(", round(quantile(x, 0.025), n), " to ", 
         round(quantile(x, 0.975), n), ")")
}

# m1
m1_c_boot_ci     <- calc_ci("C (ROC)", boot_vals_m1, 2)
m1_brier_boot_ci <- calc_ci("Brier",   boot_vals_m1, 2)
m1_emax_boot_ci  <- calc_ci("Emax",    boot_vals_m1, 2)
m1_eavg_boot_ci  <- calc_ci("Eavg",    boot_vals_m1, 2)

# m2
m2_c_boot_ci     <- calc_ci("C (ROC)", boot_vals_m2, 2)
m2_brier_boot_ci <- calc_ci("Brier",   boot_vals_m2, 2)
m2_emax_boot_ci  <- calc_ci("Emax",    boot_vals_m2, 2)
m2_eavg_boot_ci  <- calc_ci("Eavg",    boot_vals_m2, 2)

# m3
m3_c_boot_ci     <- calc_ci("C (ROC)", boot_vals_m3, 2)
m3_brier_boot_ci <- calc_ci("Brier",   boot_vals_m3, 2)
m3_emax_boot_ci  <- calc_ci("Emax",    boot_vals_m3, 2)
m3_eavg_boot_ci  <- calc_ci("Eavg",    boot_vals_m3, 2)

# m4
m4_c_boot_ci     <- calc_ci("C (ROC)", boot_vals_m4, 2)
m4_brier_boot_ci <- calc_ci("Brier",   boot_vals_m4, 2)
m4_emax_boot_ci  <- calc_ci("Emax",    boot_vals_m4, 2)
m4_eavg_boot_ci  <- calc_ci("Eavg",    boot_vals_m4, 2)

# To use validate, we need to estimate m4 with lrm instead of glm. 

d <- rms::datadist(data)
options(datadist = "d")

m4b <- rms::lrm(formula_4, data = data2, x = TRUE, y = TRUE)

set.seed(04012019)
val_new <- rms::validate(m4b, B = 500)

shrink_factor <- round(val_new["Slope","index.corrected"], 2)
c_corrected <- round(0.5 * (1 + val_new["Dxy","index.corrected"]), 2)


# Function to produce the calibration plots
library(grid)
cal_plot <- function(model, model_name, pred_var, ...){
  
  require(tidyverse)
  require(viridis)
  require(gridExtra)
  
  # The calibration plot        
  g1 <- mutate(train_hr_rr, bin = ntile(get(pred_var), 10)) %>% 
    # Bin prediction into 10ths
    group_by(bin) %>%
    mutate(n = n(), # Get ests and CIs
           bin_pred = mean(get(pred_var)), 
           bin_prob = mean(as.numeric(HOSPITAL_EXPIRE_FLAG) - 1), 
           se = sqrt((bin_prob * (1 - bin_prob)) / n), 
           ul = bin_prob + 1.96 * se, 
           ll = bin_prob - 1.96 * se) %>%
    ungroup() %>%
    ggplot(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul)) +
    geom_pointrange(size = 0.5, color = "black") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_abline() + # 45 degree line indicating perfect calibration
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", 
                color = "black", formula = y~-1 + x) + 
    # straight line fit through estimates
    geom_smooth(aes(x = get(pred_var), y = as.numeric(HOSPITAL_EXPIRE_FLAG) - 1), 
                color = "red", se = FALSE, method = "loess") + 
    # loess fit through estimates
    xlab("") +
    ylab("Observed Probability") +
    theme_minimal() +
    ggtitle(model_name)
  
  # The distribution plot        
  g2 <- ggplot(train_hr_rr, aes(x = get(pred_var))) +
    geom_histogram(fill = "black", bins = 200) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    xlab("Predicted Probability") +
    ylab("") +
    theme_minimal() +
    scale_y_continuous(breaks = c(0, 40)) +
    theme(panel.grid.minor = element_blank())
  
  # Combine them    
  g <- arrangeGrob(g1, g2, respect = TRUE, heights = c(1, 0.25), ncol = 1)
  grid.newpage()
  grid.draw(g)
  return(g[[3]])
  
}


# Combine all the results into a single dataframe 
library(broom)
model_tab_1 <- data_frame(
  est = c("Intercept", "Slope", "Residual deviance", "Df", 
          "LRT Chisq p-value", "Brier score (rescaled)",
          "Emax", "Eavg", "c-statistic"),
  m1_est = c(round(c(0, 1, m1$deviance, m1$df.residual, 0), 2), 
             paste(b1, m1_brier_boot_ci), 
             paste(val_m1["Emax"], m1_emax_boot_ci),
             paste(val_m1["Eavg"], m1_eavg_boot_ci),
             paste(round(val_m1["C (ROC)"], 2), m1_c_boot_ci)),
  m2_est = c(round(c(tidy(m2)$estimate, 1, m2$deviance, m2$df.residual, 0), 2), 
             paste(b2, m2_brier_boot_ci), 
             paste(val_m2["Emax"], m2_emax_boot_ci),
             paste(val_m2["Eavg"], m2_eavg_boot_ci), 
             paste(round(val_m2["C (ROC)"], 2), m2_c_boot_ci)),
  m3_est = c(round(c(tidy(m3)$estimate,    m3$deviance, m3$df.residual, 0), 2), 
             paste(b3, m3_brier_boot_ci), 
             paste(val_m3["Emax"], m3_emax_boot_ci),
             paste(val_m3["Eavg"], m3_eavg_boot_ci),
             paste(round(val_m3["C (ROC)"], 2), m3_c_boot_ci)),
  m4_est = c("", "",               round(c(m4$deviance, m4$df.residual, 0), 2), 
             paste(b4, m4_brier_boot_ci), 
             paste(val_m4["Emax"], m4_emax_boot_ci),
             paste(val_m4["Eavg"], m4_eavg_boot_ci),
             paste(round(val_m4["C (ROC)"], 2), m4_c_boot_ci))
)

names(model_tab_1) <- c("", "Model with HR & RR", "Model without RR", 
                        "Logistic Recalibration", "Model Revision")

model_tab_1[5, 2:5] <- c(
  "-", "<0.001", round(anova(m2, m3, test = "Chisq")[5][2, ], 2), "-"
)


x <- cal_plot(m1, "Model with HR & RR", "m1_pred")





##########################################################################################
cal_plot_data_lr = calibration(actual ~ LR_hr_rr, 
                               data = test_hr_rr, cuts = seq(0, 1, by=0.1), class = 1)$data 

ggplot() + xlab("Bin Midpoint") +
  geom_line(data = cal_plot_data_lr, aes(midpoint, Percent),
            color = "#F8766D") +
  geom_point(data = cal_plot_data_lr, aes(midpoint, Percent),
             color = "#F8766D", size = 3) +
  geom_line(aes(c(0, 100), c(0, 100)), linetype = 2, 
            color = 'grey50')
