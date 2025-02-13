
library(shiny)
library(tidyverse)
library(caret)
library(pROC)


severity_vars <- c("aps", "ards_severity", "scoma", "sps")
comorbid_vars <- c("dementia", "diabetes", "num.co")
lab_value_vars <- c("alb", "bili", "bun", "crea", "meanbp", "pafi", 
                    "sod", "temp", "urine", "wblc")
#demographic_vars <- c("age", "sex", "race", "edu", "income")

log_model_function <- function(data = support, 
                               predictor_sets = list(), 
                               outcome = "death",
                               split_ratio = 0.75,
                               seed = 123) {
  
  all_predictors <- unique(unlist(predictor_sets))
  
  model_data <- data |>
    select(all_of(c(outcome, all_predictors)))
  
  lab_vars_present <- intersect(lab_value_vars, names(model_data))
  if(length(lab_vars_present) > 0) {
    model_data <- model_data |>
      mutate(across(all_of(lab_vars_present), as.numeric))
  }
  
  if("ards_severity" %in% names(model_data)) {
    model_data <- model_data |>
      mutate(ards_severity = factor(ards_severity))
  }
  
  comorbid_vars_present <- intersect(comorbid_vars, names(model_data))
  if(length(comorbid_vars_present) > 0) {
    model_data <- model_data |>
      mutate(across(all_of(comorbid_vars_present), factor))
  }
  
  model_data <- model_data |>
    na.omit()
  
  set.seed(seed)
  train_index <- createDataPartition(model_data[[outcome]], p = split_ratio, list = FALSE)
  train <- model_data[train_index, ]
  test <- model_data[-train_index, ]
  
  categorical_vars <- c("ards_severity", comorbid_vars_present)
  numeric_vars <- setdiff(names(model_data), 
                          c(outcome, categorical_vars))
  
  train_scaled <- train |> 
    mutate(across(all_of(numeric_vars), scale))
  test_scaled <- test |> 
    mutate(across(all_of(numeric_vars), scale))
  
  formula <- as.formula(paste(outcome, "~ ."))
  model <- glm(formula, data = train_scaled, family = "binomial")
  
  predictions <- predict(model, test_scaled, type = "response")
  roc_curve <- roc(test_scaled[[outcome]], predictions)
  auc_value <- auc(roc_curve)
  
  plot(roc_curve, 
       main = paste("ROC Curve\nAUC =", round(auc_value, 3)),
       col = "blue",
       lwd = 2)
  
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  conf_matrix <- table(Predicted = predicted_classes, 
                       Actual = test_scaled[[outcome]])
  
  sensitivity <- conf_matrix[2,2] / sum(conf_matrix[2,])
  specificity <- conf_matrix[1,1] / sum(conf_matrix[1,])
  
  return(list(
    model = model,
    summary = summary(model),
    confusion_matrix = conf_matrix,
    roc_curve = roc_curve,
    auc = auc_value,
    sensitivity = sensitivity,
    specificity = specificity
  ))
}



