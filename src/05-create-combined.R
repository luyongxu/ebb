#' ---
#' title: "Create Combined Data"
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
#' This script creates the combined data prior to being split into train and test.   

#' # 1. Source Load Packages 
source(here::here("/src/01-load-packages.R"))

#' # 2. Load OHLC Data 
ohlc_data <- read_csv("./data/ohlc-data.csv", col_types = c("Dddddddc"))
print(ohlc_data)
glimpse(ohlc_data)

#' # 3. Load Position Label Data 
original_clean <- read_csv("./data/original-clean.csv", col_types = c("icdiccii"))
print(original_clean)
glimpse(original_clean)

#' # 4. Prepare OHLC Data 
#' Prepend "yahoo" to OHLC field names to prevent collisions with field names in original data.  
ohlc_data <- ohlc_data %>% 
  rename(yahoo_open = open, 
         yahoo_high = high, 
         yahoo_low = low, 
         yahoo_close = close, 
         yahoo_volume = volume, 
         yahoo_adjusted_close = adjusted_close)

#' # 5. Prepare Original Data 
#' Parse timetstamp field, change the symbol in the original labeled data to conform to Yahoo! Finance 
#' symbol naming conventions to join on the OHLC data. 
original_clean <- original_clean %>% 
  mutate(timestamp = as.Date(timestamp, format = "%m/%d/%Y"), 
         symbol = ifelse(exchange == "CT Equity", str_c(symbol, ".TO"), symbol))

#' # 6. Create Combined
combined <- ohlc_data %>% 
  left_join(original_clean) %>% 
  filter(is.na(yahoo_adjusted_close) == FALSE) %>% 
  arrange(symbol, timestamp)

#' # 7. Filter Symbols 
#' Filter observations to companies that have at least one year of trade history. 
combined <- combined %>% 
  left_join(combined %>% count(symbol)) %>% 
  filter(n >= 252) %>% 
  select(-n)

#' # 8. Create Train and Test Labels 
combined <- combined %>% 
  mutate(source = ifelse(is.na(position_label) == TRUE, "test", "train"))

#' # 9. Print Data 
print(combined)
glimpse(combined)
summary(combined)

#' # 10. Save Data 
write_csv(combined, here::here("/data/combined.csv"))


