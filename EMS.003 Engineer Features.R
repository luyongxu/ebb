#' ---
#' title: "Engineer Features"
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
source("EMS.001 Load Packages.R")

#' # 2. Load Data 
pricing_data <- read_csv("./Raw Data/pricing_data.csv") %>% 
  filter(timestamp >= "2015-01-01")

#' # 3. Set Label 
pricing_data <- pricing_data %>% 
  group_by(symbol) %>% 
  mutate(future_return = lead(close, 1)  / close - 1, 
         future_return_sign = ifelse(future_return > 0, 10, -10), 
         future_return_sign = ifelse(is.na(future_return_sign), 0, future_return_sign)) 

#' # 4. Price Change 
pricing_data <- pricing_data %>% 
  group_by(symbol) %>% 
  mutate(close_change_01d = close / lag(close, 1) - 1, 
         close_change_02d = close / lag(close, 2) - 1, 
         close_change_03d = close / lag(close, 3) - 1, 
         close_change_04d = close / lag(close, 4) - 1, 
         close_change_05d = close / lag(close, 5) - 1, 
         close_change_06d = close / lag(close, 6) - 1, 
         close_change_07d = close / lag(close, 7) - 1, 
         close_change_08d = close / lag(close, 8) - 1, 
         close_change_09d = close / lag(close, 9) - 1, 
         close_change_10d = close / lag(close, 10) - 1, 
         close_change_20d = close / lag(close, 20) - 1, 
         close_change_30d = close / lag(close, 30) - 1, 
         close_change_40d = close / lag(close, 40) - 1, 
         close_change_50d = close / lag(close, 50) - 1, 
         close_change_60d = close / lag(close, 60) - 1, 
         close_change_70d = close / lag(close, 70) - 1, 
         close_change_80d = close / lag(close, 80) - 1, 
         close_change_90d = close / lag(close, 90) - 1)

#' # 5. Price Drawdown 
pricing_data <- pricing_data %>% 
  group_by(symbol) %>% 
  mutate(close_drawdown_max = 1 - close / cummax(close), 
         close_drawdown_12m = 1 - close / roll_maxr(close, n = 12 * 21), 
         close_drawdown_11m = 1 - close / roll_maxr(close, n = 11 * 21), 
         close_drawdown_10m = 1 - close / roll_maxr(close, n = 10 * 21), 
         close_drawdown_09m = 1 - close / roll_maxr(close, n =  9 * 21), 
         close_drawdown_08m = 1 - close / roll_maxr(close, n =  8 * 21), 
         close_drawdown_07m = 1 - close / roll_maxr(close, n =  7 * 21), 
         close_drawdown_06m = 1 - close / roll_maxr(close, n =  6 * 21), 
         close_drawdown_05m = 1 - close / roll_maxr(close, n =  5 * 21), 
         close_drawdown_04m = 1 - close / roll_maxr(close, n =  4 * 21), 
         close_drawdown_03m = 1 - close / roll_maxr(close, n =  3 * 21), 
         close_drawdown_02m = 1 - close / roll_maxr(close, n =  2 * 21), 
         close_drawdown_01m = 1 - close / roll_maxr(close, n =  1 * 21))

#' # 6. Price Drawup
pricing_data <- pricing_data %>% 
  group_by(symbol) %>% 
  mutate(close_drawup_max = 1 - close / cummin(close), 
         close_drawup_12m = 1 - close / roll_minr(close, n = 12 * 21), 
         close_drawup_11m = 1 - close / roll_minr(close, n = 11 * 21), 
         close_drawup_10m = 1 - close / roll_minr(close, n = 10 * 21), 
         close_drawup_09m = 1 - close / roll_minr(close, n =  9 * 21), 
         close_drawup_08m = 1 - close / roll_minr(close, n =  8 * 21), 
         close_drawup_07m = 1 - close / roll_minr(close, n =  7 * 21), 
         close_drawup_06m = 1 - close / roll_minr(close, n =  6 * 21), 
         close_drawup_05m = 1 - close / roll_minr(close, n =  5 * 21), 
         close_drawup_04m = 1 - close / roll_minr(close, n =  4 * 21), 
         close_drawup_03m = 1 - close / roll_minr(close, n =  3 * 21), 
         close_drawup_02m = 1 - close / roll_minr(close, n =  2 * 21), 
         close_drawup_01m = 1 - close / roll_minr(close, n =  1 * 21))

#' # 7. Number of Positive Days
pricing_data <- pricing_data %>% 
  group_by(symbol) %>% 
  mutate(close_positive = ifelse(close_change_01d > 0, 1, 0), 
         close_negative = ifelse(close_change_01d <= 0, 1, 0), 
         close_positive_01d = roll_sumr(close_positive, 1, fill = NA), 
         close_positive_02d = roll_sumr(close_positive, 2, fill = NA), 
         close_positive_03d = roll_sumr(close_positive, 3, fill = NA), 
         close_positive_04d = roll_sumr(close_positive, 4, fill = NA), 
         close_positive_05d = roll_sumr(close_positive, 5, fill = NA), 
         close_positive_06d = roll_sumr(close_positive, 6, fill = NA), 
         close_positive_07d = roll_sumr(close_positive, 7, fill = NA), 
         close_positive_08d = roll_sumr(close_positive, 8, fill = NA), 
         close_positive_09d = roll_sumr(close_positive, 9, fill = NA), 
         close_positive_10d = roll_sumr(close_positive, 10, fill = NA), 
         close_positive_20d = roll_sumr(close_positive, 20, fill = NA), 
         close_positive_30d = roll_sumr(close_positive, 30, fill = NA), 
         close_positive_40d = roll_sumr(close_positive, 40, fill = NA), 
         close_positive_50d = roll_sumr(close_positive, 50, fill = NA), 
         close_positive_60d = roll_sumr(close_positive, 60, fill = NA), 
         close_positive_70d = roll_sumr(close_positive, 70, fill = NA),
         close_positive_80d = roll_sumr(close_positive, 80, fill = NA), 
         close_positive_90d = roll_sumr(close_positive, 90, fill = NA)) 

#' 8. Train and Test Split 
train <- pricing_data %>% 
  ungroup() %>% 
  filter(timestamp < "2017-01-01") 
test <- pricing_data %>% 
  ungroup() %>% 
  filter(timestamp >= "2017-01-01")
