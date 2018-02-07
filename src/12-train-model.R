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


#' # 6. Train Learner 
model <- train(learner = learner, task = task)
print(model)

#' # 7. Predict 
pred <- predict(object = model, task = task)

#' # 8. Evaluate Performance 
performance(pred, measures = list(rsq, mse, mae, rmse, timetrain), model = model)


list_learners <- listLearners()