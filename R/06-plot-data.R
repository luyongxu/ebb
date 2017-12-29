#' ---
#' title: "Calculate Return"
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
#' This script calculates the return of the strategy in the original labeled data.  

#' # 1. Source Load Packages
source("./R/01-load-packages.R")

#' # 2. Load Data 
train <- read_csv("./data/train.csv", col_types = c("Dddddddcidicii"))

#' # 3. Calculate Individual Strategy Return and Buy-and-Hold Return 
train <- train %>% 
  group_by(symbol) %>% 
  mutate(close_return_01d = yahoo_adjusted_close / lag(yahoo_adjusted_close, 1) - 1, 
         strategy_return_01d = close_return_01d * (lag(position_label, 1) / 10), 
         strategy_return_01d = ifelse(is.na(lag(position_label, 1)), 0, strategy_return_01d), 
         buy_hold_return_01d = ifelse(is.na(lag(position_label, 1)), 0, close_return_01d), 
         strategy_cumulative_return = cumprod(1 + strategy_return_01d) - 1, 
         buy_hold_cumulative_return = cumprod(1 + buy_hold_return_01d) - 1)

#' # 4. Individual Plots 
for (i in unique(train[["symbol"]])) { 
  
  # Limit train to symbol 
  plot_train <- train %>% 
    filter(symbol == i) 
  
  # Plot of closing price with signal overlayed as a color gradient 
  plot_signal <- plot_train %>% 
    mutate(yahoo_adjusted_close_earnings = ifelse(earnings == 1, yahoo_adjusted_close, NA)) %>% 
    ggplot(aes(x = timestamp, colour = position_label)) + 
    geom_line(aes(y = yahoo_adjusted_close), size = 1) + 
    geom_point(aes(y = yahoo_adjusted_close_earnings), colour = "black") + 
    geom_vline(xintercept = as.Date("2015-11-02")) + 
    scale_colour_gradientn(limits = c(-10, 10), 
                           colours = c("#ff8000", "#b4b4b4", "#0000ff"), 
                           breaks = c(-10, -5, 0, 5, 10), 
                           na.value = "#b4b4b4") + 
    labs(title = str_c(i, " Closing Price With Position Label and Earnings Announcements"), 
         x = "Date", 
         y = "Closing Price", 
         colour = "Position Label")
  
  # Calculate cumulative return and sharpe ratio for subtitle label
  strategy_cumulative_return <- plot_train %>% 
    filter(row_number() == n()) %>% 
    .[["strategy_cumulative_return"]] %>% 
    percent(2)
  buy_hold_cumulative_return <- plot_train %>% 
    filter(row_number() == n()) %>% 
    .[["buy_hold_cumulative_return"]] %>% 
    percent(2)
  strategy_sharpe <- 
    ((mean(plot_train %>% filter(lag(position_label, 1) != 0) %>% .[["strategy_return_01d"]] %>% round(2)) / 
    sd(plot_train %>% filter(lag(position_label, 1) != 0) %>% .[["strategy_return_01d"]])) *
    sqrt(252)) %>%
    digits(2)
  buy_hold_sharpe <- 
    ((mean(plot_train %>% .[["buy_hold_return_01d"]] %>% round(2)) / 
       sd(plot_train %>% .[["buy_hold_return_01d"]])) *
    sqrt(252)) %>% 
    digits(2)
  
  # Plot of strategy cumulative return and buy-and-hold return 
  plot_return <- plot_train %>% 
    ggplot(aes(x = timestamp)) + 
    geom_line(aes(y = strategy_cumulative_return, colour = "Strategy"), size = 1) + 
    geom_line(aes(y = buy_hold_cumulative_return, colour = "Buy-and-Hold"), size = 1) + 
    geom_vline(xintercept = as.Date("2015-11-02")) + 
    scale_colour_manual(name = "Return", values = c("Strategy" = "darkblue", "Buy-and-Hold" = "darkgreen")) + 
    scale_y_continuous(labels = function(x) percent(x, 0)) + 
    annotate("text", x = as.Date("2014-01-01"), y = Inf, 
             label = str_c("Strategy \n   Return: ", strategy_cumulative_return, "\n   Sharpe: ", strategy_sharpe), 
             hjust = 0, vjust = 1.5, colour = "darkblue") + 
    annotate("text", x = as.Date("2014-01-01"), y = Inf, 
             label = str_c("Buy-and-Hold \n   Return: ", buy_hold_cumulative_return, "\n   Sharpe: ", buy_hold_sharpe), 
             hjust = 0, vjust = 3, colour = "darkgreen") + 
    labs(title = str_c(i, " Strategy Cumulative Return vs Buy-and-Hold Cumulative Return"), 
         x = "Date", 
         y = "Cumulative Return")
  
  print(plot_signal + plot_return + plot_layout(ncol = 1))
}

