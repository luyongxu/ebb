#' ---
#' title: "Cross Validate Model"
#' author: "Kevin Lu"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output: 
#'   html_document: 
#'     theme: default 
#'     highlight: tango
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#'     fig_width: 8 
#'     fig_height: 5 
#' ---

#' # 1. Source Load Packages
source(here::here("src/01-load-packages.R"))

#' # 2. Load Training Data 
train <- read_feather(here::here("data/train.feather"))
glimpse(train) 

#' # 3. Convert Position Label 
#' Convert to a numeric value from 0 to 4 because the xgboost requires 
#' this format for multiclass prediction. A numeric value of 0 is equivalent 
#' to a position label of -10 and a value of 4 is equivalent to a position 
#' label of +10. 
train <- train %>% 
  mutate(position_label = as.numeric(factor(position_label)) - 1)

#' # 3. Select Features 
xgb_features <- train %>%
  select(matches("return_"), matches("drawdown_"), matches("drawup_"),
         matches("positive_"), matches("volatility_"), matches("large_jump_"), 
         matches("rsi_"), matches("aroonUp_"), matches("aroonDn_"), 
         matches("aroon_"), matches("cci_"), matches("chaikinvol_"), 
         matches("cmf_"), matches("snr_"), matches("williamsr_"), 
         matches("mfi_"), matches("cmo_"), matches("vhf_")) %>%
  colnames()

#' # 4. Set Parameters 
xgb_params <- list(booster = "gbtree", 
                   eta = 0.1, 
                   gamma = 0.1, 
                   max_depth = 2, 
                   min_child_weight = 1, 
                   subsample = 0.7, 
                   colsample_bytree = 0.1, 
                   colsample_bylevel = 0.3, 
                   lambda = 0.15, 
                   alpha = 0.0, 
                   objective = "multi:softmax", 
                   eval_metric = "mlogloss", 
                   num_class = 5)

#' # 5. Create XGB Data Objects
xgb_train <- xgb.DMatrix(data = as.matrix(train[, xgb_features]), 
                         label = as.matrix(train[, "position_label"]))

#' # 6. Generate Fold IDs 
#' Generate a list of test fold indices such that all observations that belong to a single 
#' symbol are assigned to one fold. 
xgb_folds <- vector("list", length(unique(train[["id"]])))
for (id in unique(train[["id"]])) { 
  xgb_folds[[id]] <- which(train[["id"]] %in% id)
}

#' # 7. Cross Validate For Parameter Tuning
# Best iteration:
# [70]	train-mlogloss:0.185265+0.006789	test-mlogloss:0.355103+0.206063
set.seed(5)
xgb_cv <- xgb.cv(params = xgb_params, 
                 data = xgb_train, 
                 nrounds = 100000, 
                 showsd = TRUE, 
                 folds = xgb_folds, 
                 print_every_n = 1, 
                 early_stopping_rounds = 20,
                 prediction = TRUE)

#' # 8. Extract and Score Predictions 
#' Cap predictions beyond the -10 or +10 range to -10 or +10. 
train_pred <- train %>% 
  mutate(pred = xgb_cv[["pred"]][, 1])
Accuracy(train_pred[["pred"]], train_pred[["position_label"]])

#' # 9. Save Predictions 
write_feather(train_pred, here::here("/data/train-with-pred.feather"))


