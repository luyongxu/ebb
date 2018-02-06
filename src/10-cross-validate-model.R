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

# Candidate learners:
# regr.lm
# regr.rpart
# regr.glmnet
# regr.ksvm
# regr.randomForest
# regr.ranger
# regr.extraTrees
# regr.gbm
# regr.xgboost

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
xgb_params <- list(booster = "gbtree", 
                   eta = 0.1, 
                   gamma = 0.1, 
                   max_depth = 3, 
                   min_child_weight = 3, 
                   subsample = 0.5, 
                   colsample_bytree = 0.3, 
                   lambda = 0, 
                   alpha = 0, 
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
set.seed(5)
xgb_cv <- xgb.cv(params = xgb_params, 
                 data = xgb_train, 
                 nrounds = 1000, 
                 showsd = TRUE, 
                 folds = xgb_folds, 
                 print_every_n = 1, 
                 early_stopping_rounds = 10, 
                 prediction = TRUE)

#' # 8. Extract and Save Predictions 
train_pred <- train %>% mutate(pred = xgb_cv[["pred"]])
write_csv(train_pred, here::here("/data/train-pred.csv"))










