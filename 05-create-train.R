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

#' # 1. Source Load Packages
source("01-load-packages.R")

#' # 2. Load Data 
list_clean <- read_csv("./data/list-clean.csv", col_types = c("ccdddcccc"))
ohlc_data <- read_csv("./data/ohlc-data.csv", col_types = c("icdiccii"))
original_clean <- read_csv("./data/original-clean.csv", col_types = c("icdiccii"))

#' # 3. Clean Original  
original_clean <- original_clean %>% 
  mutate(timestamp = as.Date(timestamp, format = "%m/%d/%Y"))

#' # 5. Combine Data
train <- pricing_data %>% 
  filter(symbol %in% original_clean[["symbol"]]) %>% 
  filter(timestamp >= "2013-11-02", 
         timestamp <= "2017-11-02") %>% 
  left_join(original_clean, by = c("timestamp" = "timestamp", "symbol" = "symbol")) 

#' # 6. Check Prices 
#' Check the difference between Bloomberg prices and AlphaVantage prices. 
check <- train %>% 
  mutate(close_diff = close.x - close.y) %>% 
  filter(close_diff > 0) %>% 
  select(timestamp, symbol, close.x, close.y)



#' # 6. Print Data 
print(train)
glimpse(train)
summary(train)

#' # 6. Save Data 
write_csv(train, "./Output/train.csv")


