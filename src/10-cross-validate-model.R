#' ---
#' title: "Cross Validate Model"
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

#' # 2. Start Parallelization 
parallelStartSocket(2)

#' # 3. Load Training Data 
train <- read_csv(here::here("/data/train.csv"))
glimpse(train) 

#' # 4. Remove Incomplete Observations 
mlr_train <- train %>% 
  filter(is.na(return_252) == FALSE, 
         is.na(chaikinvol_252) == FALSE)

#' # 5. Save Fold ID 
fold_id <- mlr_train %>% .[["id"]]

#' # 6. Select Features and Position Label
mlr_train <- mlr_train %>% 
  select(matches("return_"), matches("drawdown_"), matches("drawup_"), 
         matches("positive_"), matches("volatility_"), matches("rsi_"), 
         matches("aroonUp_"), matches("aroonDn_"), matches("aroon_"), 
         matches("cci_"), matches("chaikinvol_"), matches("cmf_"), 
         matches("snr_"), matches("williamsr_"), matches("mfi_"), 
         matches("cmo_"), 
         position_label) %>% 
  as.data.frame()
glimpse(mlr_train)

#' # 7. Make Task 
mlr_task <- makeRegrTask(
  id = "mlr_task", 
  data = mlr_train, 
  target = "position_label", 
  blocking = factor(fold_id)
)
print(mlr_task)

#' # 7. Make Learner 
mlr_learner <- makeLearner(
  cl = "regr.lm", 
  predict.type = "se"
)
print(mlr_learner)

#' # 10. Make Resample Description 
mlr_cv <- makeResampleDesc(
  method = "CV", 
  iters = 26, 
  predict = "both"
)
print(mlr_cv)

#' # 12. Resample Learner
set.seed(5) 
mlr_resample <- resample(
  learner = mlr_learner, 
  task = mlr_task, 
  resampling = mlr_cv, 
  measures = list(mse, setAggregation(mse, train.mean), timetrain)
)
print(mlr_resample)
print(mlr_resample[["measures.train"]])
print(mlr_resample[["measures.test"]])
print(mlr_resample[["aggr"]])




#' #' # 8. Make Parameter Set 
#' mlr_param <- makeParamSet( 
#'   makeNumericParam("C", lower = 0.01, upper = 0.10)
#' )
#' print(mlr_param)
#' 
#' #' # 9. Make Tune Control 
#' mlr_tunecontrol <- makeTuneControlRandom(
#'   maxit = 100L
#' )
#' print(mlr_tunecontrol)
#' 
#' #' # 11. Tune Parameters 
#' mlr_tune <- tuneParams(
#'   learner = mlr_learner, 
#'   task = mlr_task, 
#'   resampling = mlr_cv, 
#'   par.set = mlr_param, 
#'   control = mlr_tunecontrol
#' )
#' print(mlr_tune)
#' 
#' #' # 13. Make Resample Instance 
#' mlr_rinstance <- makeResampleInstance( 
#'   desc = mlr_cv, 
#'   task = mlr_task
#' )
#' print(mlr_rinstance)
#' 
#' 
#' parallelStop()
