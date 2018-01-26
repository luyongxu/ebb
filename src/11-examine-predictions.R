#' ---
#' title: "Examine Predictions"
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

#' # 2. Load Results 
results <- read_csv("./Output/results.csv")

#' # 2. Examine Prediction 
ggplot(results, aes(x = pred)) + 
  geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.6) + 
  geom_vline(xintercept = 0)

#' # 3. Table Ranked by Pred 
results_top <- results %>% 
  group_by(symbol) %>% 
  mutate(pred = round(pred, 2), 
         pred_7d = round(pred - lag(pred, 5), 2), 
         pred_1m = round(pred - lag(pred, 21), 2), 
         close = round(close, 2)) %>% 
  filter(timestamp == max(timestamp)) %>% 
  select(symbol, pred, pred_7d, pred_1m, close) %>% 
  arrange(desc(pred)) %>% 
  ungroup() %>% 
  filter(row_number() <= 50) 
DT::datatable(results_top)

#' # 4. Table Ranked by Pred 7d 
results_top <- results %>% 
  group_by(symbol) %>% 
  mutate(pred = round(pred, 2), 
         pred_7d = round(pred - lag(pred, 5), 2), 
         pred_1m = round(pred - lag(pred, 21), 2), 
         close = round(close, 2)) %>% 
  filter(timestamp == max(timestamp)) %>% 
  select(symbol, pred, pred_7d, pred_1m, close) %>% 
  arrange(desc(pred_7d)) %>% 
  ungroup() %>% 
  filter(row_number() <= 50) 
DT::datatable(results_top)

#' # 5. Table With Features  
results_features <- results %>% 
  group_by(symbol) %>% 
  mutate(pred = round(pred, 2), 
         close_change_10d = round(close_change_10d, 2), 
         close_drawdown_03m = round(close_drawdown_03m, 2), 
         close_drawup_03m = round(close_drawup_03m, 2), 
         close_positive_10d = round(close_positive_10d, 2)) %>% 
  filter(timestamp == max(timestamp)) %>% 
  select(symbol, pred, close_change_10d, close_drawdown_03m, close_drawup_03m, close_positive_10d) %>% 
  arrange(desc(pred)) %>% 
  filter(row_number() <= 50) 
DT::datatable(results_features)

#' # 6. Table With Sparklines 
results_spark <- results %>% 
  filter(symbol %in% results_top[["symbol"]]) %>% 
  group_by(symbol) %>% 
  summarise(pred = as.character(htmltools::as.tags(sparkline(c(pred)))), 
            close = as.character(htmltools::as.tags(sparkline(c(close))))) %>% 
  formattable() %>% 
  formattable::as.htmlwidget()
results_spark[["dependencies"]] <- c(results_spark[["dependencies"]], 
                                   htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
results_spark

#' # 7. Plots 
ggplot(results %>% filter(symbol == "FB"), aes(x = timestamp)) + 
  geom_line(aes(y = close, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(title = "Facebook (FB)", subtitle = "One year chart with prediction overlay")
ggplot(results %>% filter(symbol == "AAPL"), aes(x = timestamp)) + 
  geom_line(aes(y = close, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", high = "blue", mid = "gray", midpoint = 0) + 
  labs(title = "Apple (AAPL)", subtitle = "One year chart with prediction overlay")
ggplot(results %>% filter(symbol == "NFLX"), aes(x = timestamp)) + 
  geom_line(aes(y = close, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", high = "blue", mid = "gray", midpoint = 0) + 
  labs(title = "Netflix (NFLX)", subtitle = "One year chart with prediction overlay")
ggplot(results %>% filter(symbol == "GOOG"), aes(x = timestamp)) + 
  geom_line(aes(y = close, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(title = "Google (GOOG)", subtitle = "One year chart with prediction overlay") 

#' # 8. Interactive Plots 
#' ## 8.1 Plotly
p01 <- ggplot(results %>% filter(symbol == "FB"), aes(x = timestamp)) + 
  geom_line(aes(y = close), colour = "blue", size = 1) + 
  labs(title = "Facebook (FB)", subtitle = "A Plotly interactive graphic")
plotly::ggplotly(p01) 

#' ## 8.2 dygraphs 
p01 <- results %>% 
  filter(symbol == "FB") %>% 
  select(timestamp, close) %>% 
  timetk::tk_xts(date_col = timestamp)
dygraphs::dygraph(p01, main = "Facebook (FB)")

#' ## 8.3 Highcharter
p01 <- quantmod::getSymbols("GOOG", auto.assign = FALSE)
highcharter::hchart(p01)

#' # 9. Faceted Plot 
results %>% 
  filter(symbol %in% c("FB", "AAPL", "NFLX", "GOOG")) %>% 
  ggplot(aes(x = timestamp)) + 
  geom_line(aes(y = close, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  facet_wrap(~ symbol, scales = "free_y") 
results %>% 
  filter(symbol %in% c("FB", "AAPL", "NFLX", "GOOG")) %>% 
  ggplot(aes(x = timestamp)) + 
  geom_line(aes(y = pred, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  facet_wrap(~ symbol, scales = "free_y") 

#' # 10. Close With Signal 
p01 <- ggplot(results %>% filter(symbol == "FB"), aes(x = timestamp)) + 
  geom_line(aes(y = close, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(title = "Facebook (FB)", subtitle = "One year price chart with prediction overlay")
p02 <- ggplot(results %>% filter(symbol == "FB"), aes(x = timestamp)) + 
  geom_line(aes(y = pred, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(subtitle = "One year prediction chart")
grid.arrange(p01, p02, ncol = 1)

p01 <- ggplot(results %>% filter(symbol == "AAPL"), aes(x = timestamp)) + 
  geom_line(aes(y = close, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(title = "Apple (AAPL)", subtitle = "One year price chart with prediction overlay")
p02 <- ggplot(results %>% filter(symbol == "AAPL"), aes(x = timestamp)) + 
  geom_line(aes(y = pred, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(subtitle = "One year prediction chart")
grid.arrange(p01, p02, ncol = 1)

p01 <- ggplot(results %>% filter(symbol == "NFLX"), aes(x = timestamp)) + 
  geom_line(aes(y = close, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(title = "Netflix (NFLX)", subtitle = "One year price chart with prediction overlay")
p02 <- ggplot(results %>% filter(symbol == "NFLX"), aes(x = timestamp)) + 
  geom_line(aes(y = pred, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(subtitle = "One year prediction chart")
grid.arrange(p01, p02, ncol = 1)

p01 <- ggplot(results %>% filter(symbol == "GOOG"), aes(x = timestamp)) + 
  geom_line(aes(y = close, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(title = "Google (GOOG)", subtitle = "One year price chart with prediction overlay")
p02 <- ggplot(results %>% filter(symbol == "GOOG"), aes(x = timestamp)) + 
  geom_line(aes(y = pred, colour = pred), size = 1.5) + 
  scale_colour_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0) + 
  labs(subtitle = "One year prediction chart")
grid.arrange(p01, p02, ncol = 1)
