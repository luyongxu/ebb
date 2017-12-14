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
train <- read_csv("./Output/train.csv", col_types = c("Ddddddcii"))

#' # 3. Print 
train %>% count(symbol)

#' # 4. Unique Symbols  
symbols <- train[["symbol"]] %>% unique()
print(symbols) 

#' # 5. Plots 
for (i in symbols) { 
  plot <- train %>% 
    filter(symbol == i) %>% 
    mutate(close_earnings = ifelse(earnings == 1, close, NA)) %>% 
    ggplot(aes(x = timestamp, colour = position_label)) + 
    geom_line(aes(y = close), size = 1) + 
    geom_point(aes(y = close_earnings), colour = "black") + 
    geom_vline(xintercept = as.Date("2015-11-02")) + 
    scale_colour_gradientn(limits = c(-10, 10), 
                           colours = c("#ff8000", "#b4b4b4", "#0000ff"), 
                           breaks = c(-10, -5, 0, 5, 10), 
                           na.value = "#b4b4b4") + 
    labs(title = str_c(i, " Closing Price With Position Label and Earnings Announcements"), 
         x = "Date", 
         y = "Closing Price", 
         colour = "Position Label")
  print(plot)
}

