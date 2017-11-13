#' ---
#' title: "Plot Features"
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

#' # 2. Inspect Data 
print(pricing_data)
glimpse(pricing_data) 
summary(pricing_data)

#' # 3. Plot Distribution of Individual Variables 

#' ## 3.1 Timestamp 
ggplot(pricing_data, aes(x = timestamp)) + 
  geom_histogram(binwidth = 21, fill = "blue") 

#' ## 3.2 Open 
ggplot(pricing_data, aes(x = open)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  coord_cartesian(xlim = c(0, 500)) 

#' ## 3.3 High
ggplot(pricing_data, aes(x = high)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  coord_cartesian(xlim = c(0, 500)) 

#' ## 3.4 Low 
ggplot(pricing_data, aes(x = low)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  coord_cartesian(xlim = c(0, 500)) 

#' ## 3.5 Close 
ggplot(pricing_data, aes(x = close)) + 
  geom_histogram(binwidth = 1, fill = "blue") + 
  coord_cartesian(xlim = c(0, 500)) 

#' ## 3.6 Volume 
ggplot(pricing_data, aes(x = volume)) + 
  geom_histogram(binwidth = 500000, fill = "blue") + 
  coord_cartesian(xlim = c(0, 50000000))

#' ## 3.7 Symbol 
pricing_data %>% 
  count(symbol) %>% 
  ggplot(aes(x = n)) + 
  geom_histogram(binwidth = 21, fill = "blue")

#' ## 3.8 Future Return 
ggplot(pricing_data, aes(x = future_return)) + 
  geom_histogram(binwidth = 0.001, fill = "blue") + 
  coord_cartesian(xlim = c(-0.20, 0.20))

#' ## 3.9 Future Return Sign
ggplot(pricing_data, aes(x = factor(future_return_sign), fill = factor(future_return_sign))) + 
  geom_bar()


#' # 4. Plot Distribution of Two Variables 
ggplot(pricing_data, aes(x = close_change_90d, y = future_return)) + 
  geom_point(alpha = 0.5) + 
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))
ggplot(pricing_data, aes(x = close_change_90d, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity") + 
  coord_cartesian(xlim = c(-1, 1))
ggplot(pricing_data, aes(x = close_change_07d, fill = factor(future_return_sign))) + 
  geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity") + 
  coord_cartesian(xlim = c(-0.20, 0.20))