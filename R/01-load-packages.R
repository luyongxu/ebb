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
# devtools::install_github("thomasp85/patchwork")

#' # 2. Load libraries 
#' The RStudio Server Amazon Machine Image provided by Louis Aslett (http://www.louisaslett.com/RStudio_AMI/) provides 
#' an easy way to start an ec2 instance with RStudio Server and all major libraries installed. This machine image installs 
#' packages in the "/home/rstudio/R/x86_64-pc-linux-gnu-library/3.4" directory. When running scripts using Rscript in the 
#' command line, however, this directory is not present in the library path. This section adds the folder to the library 
#' path. 
#' 
#' Each library may depend on certain dependencies are those dependencies are loaded first before loading the parent 
#' library. When library dependencies are loaded, they may also be looking in incorrect library paths and may cause 
#' the parent library to not load. If this happens, load the dependencies manually first before loading the parent 
#' library.  
#' 
#' The load_library() function will load a package and will install the package first if necessary.  
load_library <- function(package_name) { 
  library_path <- c(.libPaths(), "/home/rstudio/R/x86_64-pc-linux-gnu-library/3.4")
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

#' # 3. Options 
#' Turn off scientific notation. 
options(scipen = 999)

#' # 4. Clean
rm(load_library)
