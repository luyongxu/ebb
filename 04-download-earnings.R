#' ---
#' title: "Download Earnings Data"
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
#' This script downloads the historical and future earnings announcements.  

#' # 1. Source Load Packages
source("01-load-packages.R")

#' # 2. Download Earnings Announcements 
#' Description 
#' 
#' Arguments 
#' 
#' Value  
download_earnings <- function(date, type) { 
  
  # Download confirmed earnings date by Zacks
  if (type == "confirmed") { 
    url <- read_html("http://www.nasdaq.com/earnings/earnings-calendar.aspx?date=2017-Dec-19") %>% 
      html_nodes("#ECCompaniesTable td, #two_column_main_content_Pnunconfirm td") %>% 
      html_text()
    temp <- url %>% 
      str_replace_all("(\t|\r|\n)", "")
  }
  
  # Download estimated earnings date by Zacks
  if (type == "estimated") { 
    url <- read_html("http://www.nasdaq.com/earnings/earnings-calendar.aspx?date=2017-Dec-19") %>% 
      html_nodes("#ECCompaniesTable td, #two_column_main_content_Pnunconfirm td") %>% 
      html_table()
    
  }
} 




