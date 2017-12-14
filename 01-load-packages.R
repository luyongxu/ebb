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

#' # 1. Capture Command Line Arguments 
#' This script when called through the command line using Rscript has the option of including a debug argument 
#' indicating whether to be verbose in the output or not to aid in debugging package dependency errors.  
args_debug <- commandArgs(trailingOnly = TRUE) 
if (length(args_debug) == 0) { 
  debug <- FALSE 
} else if (args_debug[1] == "debug") { 
  debug <- TRUE
}

#' # 2. Print library Paths and Packages
#' The .libPaths() function returns the libraries that R looks in to load packages. There are differences in the 
#' library path folder list when running scripts using Rscript and running scripts using RStudio. This section 
#' explicitly prints out the library path and the installed packages in each directory to aid in debugging any 
#' problems regarding loading packages and loading package dependencies.  
if (debug == TRUE) { 
  print("The following directories are in the library path: ")
  print(.libPaths())
  cat("\n")
  for (i in .libPaths()) { 
    print(paste("The following packages are installed in", i, ":"))
    print(rownames(installed.packages(lib.loc = i)))
    cat("\n")
  }
}

#' # 3. Load libraries 
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

#' # 4. Print Session Info 
#' A debugging step used to verify that all libraries have been loaded properly.  
if (debug == TRUE) { 
  sessionInfo()
}

#' # 5. Options 
#' Turn off scientific notation.
options(scipen = 999)

#' # 6. Clean
rm(args_debug, debug, load_library)
