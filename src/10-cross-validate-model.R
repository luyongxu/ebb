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

# Candidate learners:
# regr.lm
# regr.rpart
# regr.glmnet
# regr.ksvm
# regr.randomForest
# regr.ranger
# regr.extraTrees
# regr.gbm
# regr.xgboost

#' # 1. Source Load Packages
source(here::here("/src/01-load-packages.R"))

#' # 2. Load Training Data 
train <- read_csv(here::here("/data/train.csv")) %>% 
  filter(is.na(return_252) == FALSE, 
         is.na(chaikinvol_252) == FALSE)
glimpse(train) 

#' # 3. Save Fold ID 
#' The fold is is used to block obsevations together so that all observations that belong to a 
#' single stock always are all assigned to train or all assigned to test.  
fold_id <- train %>% .[["id"]] 

#' # 4. Select Features 
mlr_train <- train %>% 
  select(matches("return_"), matches("drawdown_"), matches("drawup_"), 
         matches("positive_"), matches("volatility_"), matches("rsi_"), 
         matches("aroonUp_"), matches("aroonDn_"), matches("aroon_"), 
         matches("cci_"), matches("chaikinvol_"), matches("cmf_"), 
         matches("snr_"), matches("williamsr_"), matches("mfi_"), 
         matches("cmo_"), matches("vhf_"), position_label) %>% 
  as.data.frame()
glimpse(mlr_train)

#' # 5. Make Task 
mlr_task <- makeRegrTask(
  id = "mlr_task", 
  data = mlr_train, 
  target = "position_label", 
  blocking = factor(fold_id)
)
print(mlr_task)

#' # 6. Make Learner 
#' Choose learner with default paramaters. Parameters will be tuned in a later step. 
#' Also add necessary preprocessing steps (centering, scaling, dealing with missings). 
mlr_learner <- makeLearner(
  cl = "regr.randomForest"
)
print(mlr_learner) 
print(mlr_learner[["par.set"]]) 
# mlr_learner <- makePreprocWrapperCaret(
#   learner = mlr_learner, 
#   ppc.center = FALSE, 
#   ppc.scale = FALSE,
#   ppc.na.remove = TRUE
# )
# print(mlr_learner)
# print(mlr_learner[["par.set"]])

#' # 7. Make Resample Description 
mlr_cv <- makeResampleDesc(
  method = "CV", 
  iters = 26, 
  predict = "both"
)
print(mlr_cv)

#' #' # 8. Make Parameter Set
#' mlr_param <- makeParamSet(
#'   makeDiscreteParam("ntree", values = c(250, 500, 1500)), 
#'   makeDiscreteParam("mtry", values = seq(20, 80, 1)), 
#'   makeDiscreteParam("nodesize", values = seq(5, 100, 1))
#' )
#' print(mlr_param)
#' 
#' #' # 9. Tune Parameters
#' mlr_tune <- tuneParams(
#'   learner = mlr_learner,
#'   task = mlr_task,
#'   resampling = mlr_cv,
#'   par.set = mlr_param,
#'   control = makeTuneControlRandom(maxit = 50L)
#' )
#' print(mlr_tune)

#' # 10. Make New Learner 
#' Make a new learner with optimal paramaters as determined by paramter tuning.  
mlr_learner <- makeLearner(
  cl = "regr.randomForest", 
  ntree = 500, 
  mtry = 2, 
  nodesize = 100
)
# mlr_learner <- makePreprocWrapperCaret(
#   learner = mlr_learner, 
#   ppc.center = TRUE, 
#   ppc.scale = TRUE, 
#   ppc.na.remove = TRUE
# )

#' # 11. Resample Learner
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

#' # 13. Make Resample Instance
mlr_rinstance <- makeResampleInstance(
  desc = mlr_cv,
  task = mlr_task
)
print(mlr_rinstance)