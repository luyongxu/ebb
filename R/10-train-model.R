#' ---
#' title: "Train Model"
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

#' # 1. Source Cross Validate Model 
source("EMS.005 Cross Validate Model.R")

#' # 2. Train Model 
xgb_model <- xgb.train(data = xgb_train, 
                       params = xgb_params, 
                       nrounds = xgb_nrounds)

#' # 3. Save Model 
xgb.save(xgb_model, "./Output/xgb_model.model")

#' # 4. Plot Importance
xgb_importance <- xgb.importance(model = xgb_model, 
                                 feature_names = xgb_features)
xgb.plot.importance(xgb_importance)

#' # 5. Generate Predictions 
results <- test %>% 
  mutate(pred = predict(xgb_model, xgb_test))

#' # 6. Save Predictions 
write_csv(results, "./Output/results.csv")
