#' ---
#' title: "Clean Data"
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
labels <- read_csv("./Raw Data/labels_a.csv", col_types = c("cdici"))
pricing_data <- read_csv("./Raw Data/pricing_data.csv", col_types = c("Ddddddc"))

#' # 3. Print Data 
print(labels)
print(pricing_data)
glimpse(labels)
glimpse(pricing_data)

#' # 4. Clean Labels 
labels <- labels %>% 
  mutate(timestamp = as.Date(timestamp, format = "%m/%d/%Y"))

#' # 5. Combine Data
train <- pricing_data %>% 
  filter(symbol %in% labels[["symbol"]]) %>% 
  filter(timestamp >= "2013-11-02", 
         timestamp <= "2017-11-02") %>% 
  left_join(labels, by = c("timestamp" = "timestamp", "symbol" = "symbol")) %>% 
  rename(close = close.x) %>% 
  select(-close.y)

#' # 6. Print Data 
print(train)
glimpse(train)
summary(train)

#' # 6. Save Data 
write_csv(train, "./Output/train.csv")


