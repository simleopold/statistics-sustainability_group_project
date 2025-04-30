library(haven)
library(dplyr)
library(rpart)
library(caret)

#step1 list data
wave3 <- read_dta('xxx/0020-03 GUI Child Cohort Wave 3_Data revised/Stata/0020-03_GUI_Data_ChildCohortWave3_V1.3.dta')

# table(wave3$)
attr(wave3$cq3b8c2, 'labels')
attr(wave3$cq3b8c2, 'label')

X <- wave3 %>% select(
  cq3n1a, cq3n1b, cq3n1c, cq3n1d, cq3n1e, cq3n1f, cq3n1g, cq3n1h, cq3n1i, cq3n1j, cq3n1k, cq3n1l, cq3n1m, cq3n1n, cq3n1o, cq3n1p, cq3n1q, cq3n1r, cq3n1s, cq3n1t,
  cq3n2a,
  cq3n7, cq3n8,
  cq3k1,
  cq3k3a, cq3k3c,
  cq3k3bcode1, 
  cq3l3a, cq3l3b, cq3l3c, cq3l3d,
  cq3l4a, cq3l4b, cq3l4c, cq3l4d,
  cq3o1, cq3o2, cq3o3, cq3o4, cq3o5, 
  cq3j6i1, cq3j6i2,
  #cq3sn3a, cq3sn3b, cq3sn3c, cq3sn3d, cq3sn3e, cq3sn3f, cq3sn3g, cq3sn3h, cq3sn3i, cq3sn3j, cq3sn3k, cq3sn3l, cq3sn3m, cq3sn3n, cq3sn3o, cq3sn3p, cq3sn3q, 
  p3q6,
  p3q11_1, p3q11_2, p3q11_3, p3q11_4, p3q11_5, p3q11_6, p3q11_7, p3q11_8, 
  p3q17_1, p3q17_2, p3q17_3, p3q17_4, p3q17_5, p3q17_6, p3q17_7, p3q17_8, p3q17_9, 
  hsdclassW3,
  pcgparclassW3,
  pc3f1educ,
  pc3h1,
  pc3e2, pc3e19,
  pc3g11,
  pc3g13,
  pc3c3
)

Y <- wave3 %>% select(cq3b8c2)

#step2 filter

df <- data.frame(y = Y, X)
#df <- df[complete.cases(df$cq3b8c2), ]
df_selected <- df %>%
  filter(!(cq3b8c2 %in% 4)) %>%
  select(where(~ mean(is.na(.)) < 0.5)) %>%
  filter(rowMeans(is.na(.)) < 0.25) %>%
  mutate(across(everything(), as.factor))

#step3 na.data

df_list <- split(df_selected, df_selected$cq3b8c2)

for (i in seq_along(df_list)) {
  group_data <- df_list[[i]]
  
  group_features <- group_data %>% select(-cq3b8c2)
  
  for (col in colnames(group_features)) {
    if (any(is.na(group_features[[col]]))) {
      cat("Processing group", names(df_list)[i], "| Imputing variable:", col, "\n")
      
      train_data <- group_features[!is.na(group_features[[col]]), ]
      test_data <- group_features[is.na(group_features[[col]]), ]
      
      if (nrow(train_data) >= 10 && length(unique(train_data[[col]])) > 1) {
        predictors <- setdiff(colnames(train_data), col)
        formula <- as.formula(paste(col, "~", paste(predictors, collapse = " + ")))
        
        model <- rpart(formula, 
                       data = train_data,
                       method = "class",
                       na.action = na.omit,
                       control = rpart.control(minsplit = 5, cp = 0.01))
        
        if (nrow(test_data) > 0) {
          pred <- predict(model, newdata = test_data, type = "class")
          group_features[is.na(group_features[[col]]), col] <- pred
        }
      } else {
        
        mode_val <- names(which.max(table(group_features[[col]])))
        group_features[is.na(group_features[[col]]), col] <- mode_val
        cat("Used mode imputation for group", names(df_list)[i], "| Variable:", col, "\n")
      }
    }
  }
  
  df_list[[i]] <- bind_cols(y = group_data$cq3b8c2, group_features)
}

df_imputed <- bind_rows(df_list)

#step4 oversampling
library(themis)

set.seed(42)
trainIndex <- createDataPartition(df_imputed$y, p = 0.75, list = FALSE)
train_filled <- df_imputed[trainIndex, ]
test_filled <- df_imputed[-trainIndex, ]

X_train <- train_filled[, -which(names(train_filled) == "y")]
y_train <- train_filled$y

smote_output <- smotenc(
  df = train_filled, 
  var = "y", 
  k = 5,
  over_ratio = 1   
)

balanced_train <- smote_output
table(balanced_train$y)

#step5 model

fix_factor_levels <- function(train, test) {
  for (col in names(train)) {
    if (is.factor(train[[col]])) {
      test[[col]] <- factor(test[[col]], levels = levels(train[[col]]))
    }
  }
  return(test)
}

test_filled <- fix_factor_levels(balanced_train, test_filled)

library(glmnet)

train_matrix <- model.matrix(y ~ .-1, data = balanced_train)
test_matrix <- model.matrix(y ~ .-1, data = test_filled)

class_weights <- 1 / table(balanced_train$y)
weights <- class_weights[as.numeric(balanced_train$y)]

set.seed(123)
cv_fit <- cv.glmnet(
  x = train_matrix,
  y = balanced_train$y,
  family = "multinomial",
  alpha = 0.5,  
  weights = weights,
  type.measure = "class"
)

pred_probs <- predict(cv_fit, newx = test_matrix, s = "lambda.min", type = "response")

print(dim(pred_probs))

prob_matrix <- pred_probs[, , 1]
colnames(prob_matrix) <- levels(balanced_train$y)

pred_labels <- colnames(prob_matrix)[max.col(prob_matrix)]

pred_class <- factor(
  pred_labels, 
  levels = levels(test_filled$y)
)

cat("predicted types:", length(pred_class), "\n")
cat("real types:", nrow(test_filled), "\n")

conf_matrix <- confusionMatrix(pred_class, test_filled$y)
print(conf_matrix)

#step6 index
class_metrics <- function(conf_matrix) {
  data.frame(
    Sensitivity = conf_matrix$byClass[, "Sensitivity"],  
    Specificity = conf_matrix$byClass[, "Specificity"],
    Precision = conf_matrix$byClass[, "Pos Pred Value"],  
    F1 = conf_matrix$byClass[, "F1"]
  )
}
metrics <- class_metrics(conf_matrix)
print(metrics)

macro_metrics <- list(
  Macro_Recall = mean(metrics$Sensitivity, na.rm = TRUE),
  Macro_Precision = mean(metrics$Precision, na.rm = TRUE),
  Macro_F1 = mean(metrics$F1, na.rm = TRUE)
)
print(macro_metrics)

balanced_acc <- mean(metrics$Sensitivity)
print(balanced_acc)

library(mltools)
mcc <- mcc(preds = pred_class, actuals = test_filled$y)
print(mcc)

#step7 visualization

library(reshape2)
library(ggplot2)
library(dplyr)

#7.1 Extract the coefficients of each class under the optimal lambda
coef_list <- coef(cv_fit, s = "lambda.min")
coef_matrix <- do.call(cbind, lapply(coef_list, as.matrix))
colnames(coef_matrix) <- names(coef_list)
coef_matrix_no_intercept <- coef_matrix[-1, ]

#7.2 Calculate global coefficient information
global_coef <- apply(coef_matrix_no_intercept, 1, mean)
global_importance <- abs(global_coef)
importance_df <- data.frame(
  Feature = names(global_coef),
  Coefficient = global_coef,
  Importance = global_importance,
  stringsAsFactors = FALSE
)
importance_df <- importance_df %>% arrange(desc(Importance))

#7.3 Define functions to construct "composite tags" based on feature names
get_combined_label <- function(feat) {
  candidate_vars <- names(X)[sapply(names(X), function(v) startsWith(feat, v))]
  if(length(candidate_vars) == 0) {
    return(feat)
  }
  varname <- candidate_vars[1]
  level_code <- substring(feat, nchar(varname) + 1)
  var_label <- attr(wave3[[varname]], "label")
  level_labels <- attr(wave3[[varname]], "labels")
  level_num <- suppressWarnings(as.numeric(level_code))
  if(!is.na(level_num) && !is.null(level_labels)) {
    match_idx <- which(level_labels == level_num)
    if(length(match_idx) > 0) {
      level_label <- names(level_labels)[match_idx[1]]
    } else {
      level_label <- level_code
    }
  } else {
    level_label <- level_code
  }
  if(is.null(var_label)) var_label <- varname
  if(is.null(level_label) || level_label == "") level_label <- level_code
  ###
  ###
  if (!(level_label %in% c("Don't know", "Don't Know", "Refusal", "No", "Not at all", "Never/almost never"))) {
    return(paste0(var_label, ": ", level_label))
  } else {
      return (0)
  }
}

#7.4 Add a combination label for each feature
importance_df$CombinedLabel <- sapply(importance_df$Feature, get_combined_label)

#7.5 Choose top 20 features_filter_meaningless_data
importance_df_not0 <- importance_df[importance_df$CombinedLabel != 0, ]
top_features <- importance_df_not0$Feature[1:20]
top_importance_df <- importance_df_not0 %>% filter(Feature %in% top_features)

#7.6 Heatmap
coef_df <- as.data.frame(coef_matrix_no_intercept)
coef_df$Feature <- rownames(coef_matrix_no_intercept)
coef_df_melt <- melt(coef_df, id.vars = "Feature", 
                     variable.name = "Class", 
                     value.name = "Coefficient")
coef_df_melt$CombinedLabel <- sapply(coef_df_melt$Feature, get_combined_label)
coef_df_melt_subset <- coef_df_melt %>% filter(Feature %in% top_features)
heatmap_plot <- ggplot(coef_df_melt_subset, aes(x = Class, y = reorder(CombinedLabel, Coefficient))) +
  geom_tile(aes(fill = Coefficient), color = "grey90") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() +
  labs(title = "Feature Importance Heatmap (Top 20 Features)",
       x = "Class",
       y = "Feature (Variable Label: Level Label)")
print(heatmap_plot)

#7.7 Barplot of features
bar_plot <- ggplot(top_importance_df, aes(x = reorder(CombinedLabel, Importance), y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                    labels = c("FALSE" = "Negative", "TRUE" = "Positive"),
                    name = "Coefficient Sign") +
  theme_minimal() +
  labs(title = "Global Feature Importance (Top 20 Features)",
       x = "Feature (Variable Label: Level Label)",
       y = "Mean Coefficient")
print(bar_plot)
