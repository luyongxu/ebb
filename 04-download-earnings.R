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

#' # 2. Download Earnings Announcements Function 
#' Description 
#' Download confirmed or unconfirmed earnings announcements from http://www.nasdaq.com/earnings/earnings-calendar.aspx.  
#' 
#' Arguments 
#' date: A string indicating the day to download earnings announcement for.  
#' type: A string indicating whether to download confirmed or estimated earnings announcements.  
#' 
#' Value  
#' Returns a dataframe containing the companies expected to announce earnings.  
download_earnings <- function(date, type) { 
  
  # Parse date
  date <- format(as.Date(date), "%Y-%b-%d")
  
  # Download confirmed earnings date by Zacks
  if (type == "confirmed") { 
    earnings <- read_html(str_c("http://www.nasdaq.com/earnings/earnings-calendar.aspx?date=", date)) %>% 
      html_nodes("#ECCompaniesTable td") %>% 
      html_text() %>% 
      str_replace_all("(\t|\r|\n)", "") %>% 
      str_trim() %>% 
      matrix(ncol = 9, byrow = TRUE) %>% 
      as_tibble() %>% 
      select(-V1, -V9) %>% 
      mutate(earnings_type = "confirmed")
  }
  
  # Download estimated earnings date by Zacks
  if (type == "estimated") { 
    earnings <- read_html(str_c("http://www.nasdaq.com/earnings/earnings-calendar.aspx?date=2017-Dec-19")) %>% 
      html_nodes("#two_column_main_content_Pnunconfirm td") %>% 
      html_text() %>% 
      str_replace_all("(\t|\r|\n)", "") %>% 
      str_trim() %>% 
      matrix(ncol = 8, byrow = TRUE) %>% 
      as_tibble() %>% 
      select(-V8) %>% 
      mutate(earnings_type = "estimated")
  }
  
  # Clean earnings 
  colnames(earnings) <- c("name_full", "earnings_date", "fiscal_quarter", "consensus_eps", "number_ests", 
                          "last_year_earnings_date", "last_year_eps", "earnings_type") 
  name_parsed <- str_match(earnings[["name_full"]], "(.*)(\\()(.*)(\\))(.*)(:)(.*)")
  earnings <- earnings %>% 
    mutate(name = str_trim(name_parsed[, 2]),  
           symbol = str_trim(name_parsed[, 4]), 
           market_cap = str_trim(name_parsed[, 8]), 
           market_cap_number = as.numeric(str_extract(market_cap, "([0-9]*\\.[0-9]+|[0-9]+)")),  
           market_cap_letter = str_extract(market_cap, "[TBM]"), 
           market_cap_scale = ifelse(market_cap_letter == "T", 1000000000000, NA), 
           market_cap_scale = ifelse(market_cap_letter == "B", 1000000000, market_cap_scale), 
           market_cap_scale = ifelse(market_cap_letter == "M", 1000000, market_cap_scale), 
           market_cap = market_cap_number * market_cap_scale) %>%  
    mutate_at(vars(earnings_date, last_year_earnings_date), funs(ifelse(. == "n/a", NA, .))) %>% 
    mutate_at(vars(earnings_date, last_year_earnings_date), funs(as.Date(., format = "%m/%d/%Y"))) %>% 
    mutate(last_year_eps = ifelse(last_year_eps == "$n/a", NA, last_year_eps)) %>% 
    select(name, symbol, market_cap, earnings_date, fiscal_quarter, consensus_eps, 
           number_ests, last_year_earnings_date, last_year_eps, earnings_type)
  
  # Return dataframe
  return(earnings)
} 

#' # 3. Download Current Earnings Announcements 
earnings_confirmed <- download_earnings(date = Sys.Date(), type = "confirmed")
earnings_estimated <- download_earnings(date = Sys.Date(), type = "estimated")

#' # 4. Save Current Earnings Announcements
write_csv(earnings_confirmed, "./data/earnings-confirmed.csv")
write_csv(earnings_estimated, "./data/earnings-estimated.csv")
