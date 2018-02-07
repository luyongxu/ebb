#' ---
#' title: "Plot Predictions"
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
source(here::here("/src/01-load-packages.R"))

#' # 2. Load Predictions  
train_pred <- read_csv(here::here("/data/train-pred.csv"))

#' # 3. Create Prediction Error 
train_pred <- train_pred %>% 
  mutate(error = position_label - pred)

#' # 4. Create Plot Layer
plot_gradient <- list( 
  scale_colour_gradientn(
    limits = c(-10, 10), 
    colours = c("#ff8000", "#b4b4b4", "#0000ff"), 
    breaks = c(-10, -5, 0, 5, 10), 
    na.value = "#b4b4b4"
  )
)

#' # 5. Create Plots For Each Symbol 
#' Loop through vector of symbols and produce a composite of three plots for each symbol. 
plot_combined <- list()
for (i in unique(train_pred[["symbol"]])) { 
  df <- train_pred %>% 
    filter(symbol == i)
  print(str_c("Creating plots for ", i, "."))
  rmse <- round(RMSE(y_pred = df[["pred"]], y_true = df[["position_label"]]), 2)
  plot_truth <- ggplot(df, aes(x = timestamp)) + 
    geom_line(aes(y = yahoo_close, colour = position_label), size = 1.5) + 
    plot_gradient + 
    labs(title = str_c(i, " Closing Price With Ground Truth Position Label"), y = "Closing Price")
  plot_pred <- ggplot(df, aes(x = timestamp)) + 
    geom_line(aes(y = yahoo_close, colour = pred), size = 1.5) + 
    plot_gradient + 
    labs(title = str_c(i, " Closing Price With Predicted Position Label"), y = "Closing Price")
  plot_error <- ggplot(df, aes(x = timestamp)) + 
    geom_line(aes(y = error, colour = position_label), size = 1.5) + 
    geom_hline(yintercept = 0, colour = "darkgreen", alpha = 0.5) + 
    geom_hline(yintercept = -10, colour = "darkgreen", alpha = 0.5) + 
    geom_hline(yintercept = +10, colour = "darkgreen", alpha = 0.5) + 
    coord_cartesian(ylim = c(-20, 20)) + 
    plot_gradient + 
    labs(title = str_c(i, " Prediction Error (RMSE: ", rmse, ")"), y = "Error")
  plot_combined[[i]] <- plot_truth + plot_pred + plot_error + plot_layout(ncol = 1)
}

