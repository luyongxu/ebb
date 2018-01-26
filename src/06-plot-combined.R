#' ---
#' title: "Plot Training Data"
#' author: "Kevin Lu"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output: 
#'   html_document: 
#'     theme: default 
#'     highlight: tango
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#'     fig_width: 12
#'     fig_height: 10 
#' ---
#' This script calculates the return of the strategy in the original labeled data.  

#' # 1. Source Load Packages
source(here::here("/src/01-load-packages.R"))

#' # 2. Load Data 
combined <- read_csv(here::here("/data/combined.csv"), col_types = c("Dddddddcidiciic"))

#' # 3. Filter Data 
#' Filter data to observations belonging to symbols in the original position label data. 
symbols <- combined %>% 
  filter(source == "train") %>% 
  .[["symbol"]] %>% 
  unique()
combined_plot <- combined %>% 
  filter(symbol %in% symbols, 
         timestamp <= "2017-11-02")

#' # 4. Calculate Individual Strategy Return and Buy-and-Hold Return 
combined_plot <- combined_plot %>% 
  group_by(symbol) %>% 
  mutate(close_return_01d = yahoo_adjusted_close / lag(yahoo_adjusted_close, 1) - 1, 
         close_return_01d = ifelse(is.na(close_return_01d), 0, close_return_01d), 
         strategy_return_01d = close_return_01d * (lag(position_label, 1) / 10), 
         strategy_return_01d = ifelse(is.na(strategy_return_01d), 0, strategy_return_01d), 
         buy_hold_return_01d = ifelse(is.na(position_label), 0, close_return_01d), 
         strategy_cumulative_return = cumprod(1 + strategy_return_01d) - 1, 
         buy_hold_cumulative_return = cumprod(1 + buy_hold_return_01d) - 1)

#' # 4. Calculate Combined Strategy Return and Buy-and-Hold Return 
combined_plot <- combined_plot %>% 
  bind_rows(combined_plot %>% 
              group_by(timestamp) %>% 
              summarise(yahoo_adjusted_close = mean(yahoo_adjusted_close), 
                        position_label = mean(position_label), 
                        strategy_return_01d = mean(strategy_return_01d), 
                        buy_hold_return_01d = mean(buy_hold_return_01d)) %>% 
              ungroup() %>%
              mutate(symbol = "Combined", 
                     strategy_cumulative_return = cumprod(1 + strategy_return_01d) - 1, 
                     buy_hold_cumulative_return = cumprod(1 + buy_hold_return_01d) - 1, 
                     earnings = 0))
symbols <- combined_plot %>% 
  group_by(symbol) %>% 
  filter(row_number() == n(), 
         symbol != "Combined") %>% 
  arrange(desc(strategy_cumulative_return)) %>% 
  .[["symbol"]]

#' # 5. Generate Plots 
plot_list <- list()
for (i in c("Combined", symbols)) { 
  
  # Print statement 
  print(str_c("Creating plot for ", i, "."))
  
  # Limit train to symbol 
  df <- combined_plot %>% 
    filter(symbol == i) 
  
  # Plot of closing price with signal overlayed as a color gradient 
  plot_signal <- df %>% 
    mutate(yahoo_adjusted_close_earnings = ifelse(earnings == 1, yahoo_adjusted_close, NA)) %>% 
    ggplot(aes(x = timestamp, colour = position_label)) + 
    geom_line(aes(y = yahoo_adjusted_close), size = 1) + 
    geom_vline(xintercept = as.Date("2015-11-02")) + 
    scale_colour_gradientn(limits = c(-10, 10), 
                           colours = c("#ff8000", "#b4b4b4", "#0000ff"), 
                           breaks = c(-10, -5, 0, 5, 10), 
                           na.value = "#b4b4b4") + 
    labs(title = str_c(i, " Closing Price With Position Label and Earnings Announcements"), 
         x = "Date", 
         y = "Closing Price", 
         colour = "Position Label")
  
  # Add back earnings announcement dates 
  if (i != "Combined") { 
    plot_signal <- plot_signal + geom_point(aes(y = yahoo_adjusted_close_earnings), colour = "black")
  }
  
  # Calculate cumulative return and sharpe ratio for subtitle label
  strategy_cumulative_return <- df %>% 
    filter(row_number() == n()) %>% 
    .[["strategy_cumulative_return"]] %>% 
    percent(2)
  buy_hold_cumulative_return <- df %>% 
    filter(row_number() == n()) %>% 
    .[["buy_hold_cumulative_return"]] %>% 
    percent(2)
  strategy_sharpe <- 
    ((mean(df %>% filter(!is.na(position_label)) %>% .[["strategy_return_01d"]]) / 
    sd(df %>% filter(!is.na(position_label)) %>% .[["strategy_return_01d"]])) *
    sqrt(252)) %>%
    digits(2)
  buy_hold_sharpe <- 
    ((mean(df %>% .[["buy_hold_return_01d"]] %>% round(2)) / 
       sd(df %>% .[["buy_hold_return_01d"]])) *
    sqrt(252)) %>% 
    digits(2)
  
  # Plot of strategy cumulative return and buy-and-hold return 
  plot_return <- df %>% 
    ggplot(aes(x = timestamp)) + 
    geom_line(aes(y = strategy_cumulative_return, colour = "Strategy"), size = 1) + 
    geom_line(aes(y = buy_hold_cumulative_return, colour = "Buy-and-Hold"), size = 1) + 
    geom_vline(xintercept = as.Date("2015-11-02")) + 
    scale_colour_manual(name = "Return", values = c("Strategy" = "darkblue", "Buy-and-Hold" = "darkgreen")) + 
    scale_y_continuous(labels = function(x) percent(x, 0)) + 
    annotate("text", x = min(df[["timestamp"]]), y = Inf, 
             label = str_c("Strategy \n   Return: ", strategy_cumulative_return, "\n   Sharpe: ", strategy_sharpe), 
             hjust = 0, vjust = 1.5, colour = "darkblue") + 
    annotate("text", x = min(df[["timestamp"]]), y = Inf, 
             label = str_c("Buy-and-Hold \n   Return: ", buy_hold_cumulative_return, "\n   Sharpe: ", buy_hold_sharpe), 
             hjust = 0, vjust = 3, colour = "darkgreen") + 
    labs(title = str_c(i, " Strategy Cumulative Return vs Buy-and-Hold Cumulative Return"), 
         x = "Date", 
         y = "Cumulative Return")
  
  # Print plots and save plots to list 
  if (i != "Combined") { 
    plot_combined <- plot_signal + plot_return + plot_layout(ncol = 1)
    plot_list[[i]] <- plot_combined
    print(plot_combined)
  }
  if (i == "Combined") {
    plot_combined <- plot_return
    plot_list[[i]] <- plot_combined
    print(plot_combined, height = 6)
  }
}



