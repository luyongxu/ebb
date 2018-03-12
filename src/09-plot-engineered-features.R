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

#' # 1. Source Load Packages
source(here::here("src/01-load-packages.R"))

#' # 2. Load Training Data 
train <- read_feather(here::here("data/train.feather"))
print(train)
glimpse(train) 
summary(train)

#' # 3. Plot Layers 
plot_gradient <- list( 
  scale_colour_gradientn(
    limits = c(-10, 10), 
    colours = c("#ff8000", "#b4b4b4", "#0000ff"), 
    breaks = c(-10, -5, 0, 5, 10), 
    na.value = "#b4b4b4"
  )
)

#' # 4. Correlation Between Technical Indicators and Position Labels 
technical_indicators <- colnames(train)[16:ncol(train)]
correlation <- vector("list", length(technical_indicators))
for (i in technical_indicators) { 
  df <- tibble(
    feature = i, 
    correlation = cor(x = train[[i]], y = train[["position_label"]], use = "complete.obs") 
  )
  correlation[[i]] <- df
}
correlation <- correlation %>% map_df(bind_rows)
print(correlation, n = nrow(correlation))
rm(df, i, technical_indicators)
    
#' # 5. Rolling Return 
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = return_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~ symbol, scales = "free_y") + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = return_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol, scales = "free_y") + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = return_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = return_252), fill = "blue", alpha = 0.5)

#' # 6. Rolling Drawdown 
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = drawdown_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = drawdown_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = drawdown_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = drawdown_252), fill = "blue", alpha = 0.5)

#' # 7. Rolling Drawup 
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = drawup_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = drawup_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = drawup_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = drawup_252), fill = "blue", alpha = 0.5)

#' # 8. Rolling Number of Positive Days
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = positive_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = positive_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 120) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = drawup_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = drawup_252), fill = "blue", alpha = 0.5)

#' # 9. Rolling Volatility 
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = volatility_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = volatility_252, colour = position_label), size = 1) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = volatility_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = volatility_252), fill = "blue", alpha = 0.5)

#' # 9. Rolling Large Jump 
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = large_jump_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = large_jump_252, colour = position_label), size = 1) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = large_jump_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = large_jump_252), fill = "blue", alpha = 0.5)

#' # 10. RSI
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = rsi_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 50) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = rsi_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 50) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = rsi_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = rsi_252), fill = "blue", alpha = 0.5)

#' # 11. Aroon
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = aroon_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = aroon_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = aroon_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = aroon_252), fill = "blue", alpha = 0.5)

#' # 12. CCI
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = cci_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = cci_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = cci_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = cci_252), fill = "blue", alpha = 0.5)

#' # 13. Chaikin Volatility
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = chaikinvol_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = chaikinvol_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = chaikinvol_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = chaikinvol_252), fill = "blue", alpha = 0.5)

#' # 14. Chaikin Money Flow 
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = cmf_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = cmf_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = cmf_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = cmf_252), fill = "blue", alpha = 0.5)

#' # 15. Signal to Noise Ratio 
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = snr_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = snr_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = snr_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = snr_252), fill = "blue", alpha = 0.5)

#' # 16. Williams R
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = williamsr_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = williamsr_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = williamsr_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = williamsr_252), fill = "blue", alpha = 0.5)

#' # 17. Money Flow Index
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = mfi_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 50) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = mfi_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 50) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = mfi_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = mfi_252), fill = "blue", alpha = 0.5)

#' # 18. Chande Momentum Oscillator
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = cmo_005, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = cmo_252, colour = position_label), size = 1) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = cmo_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = cmo_252), fill = "blue", alpha = 0.5)

#' # 19. Vertical Horizonal Filter 
ggplot(train, aes(x = timestamp)) +
  geom_line(aes(y = vhf_005, colour = position_label), size = 1) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = timestamp)) + 
  geom_line(aes(y = vhf_252, colour = position_label), size = 1) + 
  facet_wrap(~ symbol) + 
  plot_gradient
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = vhf_005), fill = "blue", alpha = 0.5)
ggplot(train, aes(x = factor(position_label))) + 
  geom_boxplot(aes(y = vhf_252), fill = "blue", alpha = 0.5)
