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
                   eta = 0.01, 
                   lambda = 1, 
                   alpha = 30,  
                   lambda_bias = 0.0, 
                   objective = "reg:linear", 
                   eval_metric = "rmse")

#' # 5. Create XGB Data Objects
xgb_train <- xgb.DMatrix(data = as.matrix(train[, xgb_features]), 
                         label = as.matrix(train[, "position_label"]))

#' # 6. Train XGB Model 
xgb_model <- xgb.train(params = xgb_params, data = xgb_train, nrounds = 4341)
xgb_importance <- xgb.importance(model = xgb_model)

#' # 7. Save XGB Model and Importance
xgb.save(xgb_model, here::here("./output/xgboost.model"))
write_csv(xgb_importance, here::here("./output/xgb_importance.csv"))


