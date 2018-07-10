#' ---
#' title: "Train Model"
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
                   objective = "multi:softprob", 
                   eval_metric = "mlogloss", 
                   num_class = 5)

#' # 5. Create XGB Data Objects
xgb_train <- xgb.DMatrix(data = as.matrix(train[, xgb_features]), 
                         label = as.matrix(train[, "position_label"]))

#' # 6. Train XGB Model 
xgb_model <- xgb.train(params = xgb_params, data = xgb_train, nrounds = 70)
xgb_importance <- xgb.importance(model = xgb_model)

#' # 7. Save XGB Model and Importance
xgb.save(xgb_model, here::here("./output/xgboost.model"))


