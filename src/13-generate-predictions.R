#' ---
#' title: "Generate Predictions"
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

#' # 2. Load Test Data 
test <- read_feather(here::here("/data/test.feather"))
glimpse(test) 

#' # 3. Filter Stocks 
#' Remove stocks that have had large one-day jumps within the last year. 
min_max_return <- test %>% 
  filter(timestamp >= Sys.Date() - 365) %>% 
  group_by(symbol) %>% 
  summarise(max_return_001 = max(return_001), 
            min_return_001 = min(return_001))
test <- test %>% 
  left_join(min_max_return) %>% 
  filter(max_return_001 <= 25, 
         min_return_001 >= -25) %>% 
  select(-max_return_001, -min_return_001)

#' # 4. Load XGBoost Model 
xgb_model <- xgb.load(here::here("/output/xgboost.model"))

#' # 5. Select Features 
xgb_features <- test %>%
  select(matches("return_"), matches("drawdown_"), matches("drawup_"),
         matches("positive_"), matches("volatility_"), matches("rsi_"),
         matches("aroonUp_"), matches("aroonDn_"), matches("aroon_"),
         matches("cci_"), matches("chaikinvol_"), matches("cmf_"),
         matches("snr_"), matches("williamsr_"), matches("mfi_"),
         matches("cmo_"), matches("vhf_")) %>%
  colnames()

#' # 6. Create XGB Data Objects
xgb_test <- xgb.DMatrix(data = as.matrix(test[, xgb_features]), 
                        label = as.matrix(test[, "position_label"]))

#' # 7. Generate Predictions 
predictions <- test %>% 
  mutate(pred = predict(object = xgb_model, newdata = xgb_test)) %>% 
  filter(timestamp == max(timestamp)) %>% 
  select(timestamp, symbol, pred) %>% 
  arrange(desc(pred))

#' # 8. Save Predictions 
write_csv(predictions, here::here("/data/predictions-2018-02-08.csv"))
  