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

#' # 1. Source Engineer Features
source("EMS.003 Engineer Features.R")

#' # 2. Set Features
xgb_features <- train %>% 
  select(matches("close_change_..d$"), matches("close_drawdown"), matches("close_drawup"), matches("close_positive_..d$")) %>% 
  colnames()

#' # 3. Set Parameters
xgb_params <- list(booster = "gbtree", 
                   eta = 0.3, 
                   gamma = 0, 
                   max_depth = 3, 
                   min_child_weight = 1, 
                   subsample = 0.7, 
                   colsample_bytree = 0.7, 
                   lambda = 0, 
                   alpha = 0, 
                   objective = "reg:linear", 
                   eval_metric = "rmse")

#' # 4. Create XGB Data Objects
xgb_train <- xgb.DMatrix(data = as.matrix(train[, xgb_features]), 
                         label = as.matrix(train[, "future_return_sign"]))
xgb_test <- xgb.DMatrix(data = as.matrix(test[, xgb_features]), 
                        label = as.matrix(test[, "future_return_sign"]))

#' # 5. Cross Validate For Parameter Tuning
set.seed(5)
xgb_cv <- xgb.cv(data = xgb_train, 
                 params = xgb_params, 
                 showsd = TRUE, 
                 early_stopping_rounds = 50, 
                 print_every_n = 1, 
                 nfold = 5, 
                 nrounds = 100)
xgb_nrounds <- 100
