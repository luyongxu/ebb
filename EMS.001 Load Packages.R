#' ---
#' title: "Load Packages"
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

#' # 1. Load Packages 
suppressWarnings(suppressMessages({ 
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(jsonlite)
  library(timetk)
  library(rvest)
  library(RcppRoll)
  library(xgboost)
  library(sparkline)
  library(htmltools)
  library(htmlwidgets)
  library(formattable)
  library(gridExtra)
}))

#' # 2. Options
#' Turn off scientific notation. 
options(scipen = 999) 