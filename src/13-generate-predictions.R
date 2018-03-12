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

#' # 3. Load XGBoost Model 
xgb_model <- xgb.load(here::here("/output/xgboost.model"))

#' # 4. Select Features 
xgb_features <- test %>%
  select(matches("return_"), matches("drawdown_"), matches("drawup_"),
         matches("positive_"), matches("volatility_"), matches("large_jump_"), 
         matches("rsi_"), matches("aroonUp_"), matches("aroonDn_"), 
         matches("aroon_"), matches("cci_"), matches("chaikinvol_"), 
         matches("cmf_"), matches("snr_"), matches("williamsr_"), 
         matches("mfi_"), matches("cmo_"), matches("vhf_")) %>%
  colnames()

#' # 5. Create XGB Data Objects
xgb_test <- xgb.DMatrix(data = as.matrix(test[, xgb_features]), 
                        label = as.matrix(test[, "position_label"]))

#' # 6. Generate Predictions
pred <- predict(object = xgb_model, newdata = xgb_test, reshape = TRUE) %>% 
  as_tibble() %>% 
  mutate_all(round, 4) %>% 
  rename(prob_negative_10 = V1, 
         prob_negative_5 = V2, 
         prob_0 = V3, 
         prob_positive_5 = V4, 
         prob_positive_10 = V5) %>% 
  mutate(prob_max = pmax(prob_negative_10, prob_negative_5, prob_0, prob_positive_5, prob_positive_10), 
         prediction = case_when(
           prob_max == prob_negative_10 ~ -10, 
           prob_max == prob_negative_5 ~ -5, 
           prob_max == prob_0 ~ 0, 
           prob_max == prob_positive_5 ~ 5, 
           prob_max == prob_positive_10 ~ 10, 
           TRUE ~ 0
         )) %>% 
  select(-prob_max)

#' # 7. Generate Output 
predictions_history <- test %>% 
  select(timestamp, symbol, starts_with("yahoo")) %>% 
  bind_cols(pred) %>% 
  filter(timestamp >= Sys.Date() - 365) %>% 
  mutate_at(vars(starts_with("yahoo")), round, 2)
predictions_current <- predictions_history %>% 
  mutate(opportunity_score = prob_positive_5 + prob_positive_10 - prob_negative_5 - prob_negative_10, 
         opportunity_score_005 = opportunity_score + roll_meanr(prob_0, 5, na.rm = TRUE), 
         opportunity_score_010 = opportunity_score + roll_meanr(prob_0, 10, na.rm = TRUE), 
         opportunity_score_021 = opportunity_score + roll_meanr(prob_0, 21, na.rm = TRUE), 
         opportunity_score_042 = opportunity_score + roll_meanr(prob_0, 42, na.rm = TRUE), 
         opportunity_score_063 = opportunity_score + roll_meanr(prob_0, 63, na.rm = TRUE), 
         opportunity_score_084 = opportunity_score + roll_meanr(prob_0, 84, na.rm = TRUE), 
         opportunity_score_105 = opportunity_score + roll_meanr(prob_0, 105, na.rm = TRUE), 
         opportunity_score_126 = opportunity_score + roll_meanr(prob_0, 126, na.rm = TRUE)) %>% 
  mutate_at(vars(starts_with("opportunity")), round, 4) %>% 
  filter(timestamp == max(timestamp)) %>% 
  select(-opportunity_score)

#' # 8. Save Output 
write_csv(predictions_history, here::here(str_c("output/predictions-history-", Sys.Date(), ".csv")))
write_csv(predictions_current, here::here(str_c("output/predictions-current-", Sys.Date(), ".csv")))
  