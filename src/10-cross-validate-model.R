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
source(here::here("/src/01-load-packages.R"))

#' # 2. Load Training Data 
train <- read_csv(here::here("/data/train.csv"))
glimpse(train) 

#' # 3. Select Features 
xgb_features <- train %>%
  select(matches("return_"), matches("drawdown_"), matches("drawup_"),
         matches("positive_"), matches("volatility_"), matches("rsi_"),
         matches("aroonUp_"), matches("aroonDn_"), matches("aroon_"),
         matches("cci_"), matches("chaikinvol_"), matches("cmf_"),
         matches("snr_"), matches("williamsr_"), matches("mfi_"),
         matches("cmo_"), matches("vhf_")) %>%
  colnames()

#' # 4. Set Parameters 
xgb_params <- list(booster = "gblinear", 
                   eta = 0.001, 
                   lambda = 1, 
                   alpha = 30,  
                   lambda_bias = 0.0, 
                   objective = "reg:linear", 
                   eval_metric = "rmse")

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
# [30]	train-rmse:1.203154+0.023987	test-rmse:1.709590+0.728223
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
  mutate(pred = xgb_cv[["pred"]], 
         pred = ifelse(pred >= 10, 10, pred), 
         pred = ifelse(pred <= -10, -10, pred))
RMSE(train_pred[["pred"]], train_pred[["position_label"]])

#' # 9. Save Predictions 
write_csv(train_pred, here::here("/data/train-pred.csv"))



