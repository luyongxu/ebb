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
#' This script loads the necessary packages. 

#' # 1. Install Libraries Not on CRAN 
#' These packages require a one-time installation from Github.  
# devtools::install_github("thomasp85/patchwork")

#' # 2. Load libraries 
#' Description  
#' The load_library() function will load a package and will install the package first if necessary.  
#' 
#' Arguments  
#' package_name: A string indicating the package name.  
#' 
#' Returns  
#' None.  
load_library <- function(package_name) { 
  library_path <- c(.libPaths())
  suppressWarnings(suppressMessages({
    if(require(package_name, character.only = TRUE, lib.loc = library_path) == FALSE) {
      install.packages(package_name, repos = "https://cloud.r-project.org/")
      require(package_name, character.only = TRUE, lib.loc = library_path)
    }
  }))
}
load_library("tidyverse")
load_library("readr")
load_library("rvest")
load_library("jsonlite")
load_library("stringr")
load_library("timetk")
load_library("xts")
load_library("TTR")
load_library("alphavantager")
load_library("quantmod")
load_library("formattable")
load_library("patchwork")
load_library("here")

#' # 3. Options 
#' Turn off scientific notation. 
options(scipen = 999)

#' # 4. Clean
rm(load_library)
