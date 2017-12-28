#' ---
#' title: "Create Training Data"
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
#' This script creates the training data. 

#' # 1. Source Load Packages
source("./R/01-load-packages.R")

#' # 2. Load Data 
#' Load OHLC and original labeled data.  
ohlc_data <- read_csv("./data/ohlc-data.csv", col_types = c("Dddddddc"))
original_clean <- read_csv("./data/original-clean.csv", col_types = c("icdiccii"))

#' # 3. Create Symbol Vector 
symbols <- original_clean %>% 
  mutate(symbol = ifelse(exchange == "CT Equity", str_c(symbol, ".TO"), symbol)) %>% 
  .[["symbol"]] %>% 
  unique()
  
#' # 4. Prepare OHLC Data 
#' Prepend yahoo to OHLC field names to prevent collisions with field names in original data.  
ohlc_data <- ohlc_data %>% 
  filter(symbol %in% symbols) %>% 
  rename(yahoo_open = open, 
         yahoo_high = high, 
         yahoo_low = low, 
         yahoo_close = close, 
         yahoo_volume = volume, 
         yahoo_adjusted_close = adjusted_close)

#' # 5. Prepare Original Data 
#' Parse timetstamp field, temporarily change the symbol in the original labeled data to conform 
#' to Yahoo! Finance symbol naming conventions to join on the OHLC data. 
original_clean <- original_clean %>% 
  mutate(timestamp = as.Date(timestamp, format = "%m/%d/%Y"), 
         symbol = ifelse(exchange == "CT Equity", str_c(symbol, ".TO"), symbol))

#' # 6. Create Train
train <- ohlc_data %>% left_join(original_clean) %>% mutate(diff = yahoo_close - close)

#' # 7. Print Data 
glimpse(train)

#' # 8. Save Data 
write_csv(train, "./data/train.csv")


