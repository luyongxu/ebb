#' ---
#' title: "Plot Raw Features"
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
source(here::here("src/01-load-packages.R"))

#' # 2. Load Training Data 
train <- read_feather(here::here("data/train.feather"))
print(train)
glimpse(train) 
summary(train)

#' # 3. Plot OHLCV 
plot_gradient <- list( 
  scale_colour_gradientn(
    limits = c(-10, 10), 
    colours = c("#ff8000", "#b4b4b4", "#0000ff"), 
    breaks = c(-10, -5, 0, 5, 10), 
    na.value = "#b4b4b4"
  )
)
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = yahoo_open, colour = position_label)) + 
  labs(title = "Yahoo Open With Position Label") + 
  facet_wrap(~ symbol, scales = "free_y") + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = yahoo_high, colour = position_label)) + 
  labs(title = "Yahoo High With Position Label") + 
  facet_wrap(~ symbol, scales = "free_y") + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = yahoo_low, colour = position_label)) + 
  labs(title = "Yahoo Low With Position Label") + 
  facet_wrap(~ symbol, scales = "free_y") + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = yahoo_close, colour = position_label)) + 
  labs(title = "Yahoo Close With Position Label") + 
  facet_wrap(~ symbol, scales = "free_y") + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = yahoo_volume, colour = position_label)) + 
  labs(title = "Yahoo Volume With Position Label") + 
  facet_wrap(~symbol, scales = "free_y") + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = yahoo_adjusted_close, colour = position_label)) + 
  labs(title = "Yahoo Adjusted Close With Position Label") + 
  facet_wrap(~ symbol, scales = "free_y") + 
  plot_gradient


#' # 4. Categorical Data 
table(train[["symbol"]])
table(train[["id"]])
table(train[["exchange"]])
table(train[["initial_trend"]])
table(train[["position_label"]])
table(train[["source"]])


