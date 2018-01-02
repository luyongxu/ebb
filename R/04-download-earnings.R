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
source(here::here("/R/01-load-packages.R"))

#' # 2. Download Earnings Announcements Function 
#' Description 
#' Download confirmed or unconfirmed earnings announcements from http://www.nasdaq.com/earnings/earnings-calendar.aspx.  
#' 
#' Arguments 
#' date: A string indicating the day to download earnings announcement for.  
#' type: A string indicating whether to download confirmed or estimated earnings announcements.  
#' 
#' Value  
#' Returns a dataframe containing the earnings announcement data.  
download_earnings <- function(date, type) { 
  
  # Parse date
  print(str_c("Downloading ", type, " earnings data for ", date, "."))
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
    earnings <- read_html(str_c("http://www.nasdaq.com/earnings/earnings-calendar.aspx?date=", date)) %>% 
      html_nodes("#two_column_main_content_Pnunconfirm td") %>% 
      html_text() %>% 
      str_replace_all("(\t|\r|\n)", "") %>% 
      str_trim() %>% 
      matrix(ncol = 8, byrow = TRUE) %>% 
      as_tibble() %>% 
      select(-V8) %>% 
      mutate(earnings_type = "estimated")
  }
  
  # Clean column names  
  colnames(earnings) <- c("name_full", "earnings_date", "fiscal_quarter", "consensus_eps", "number_ests", 
                          "last_year_earnings_date", "last_year_eps", "earnings_type") 
  
  # Return null dataframe if no data is available
  if(nrow(earnings) == 0) { 
    return(NULL)
  }
  
  # Parse and clean
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
    mutate(consensus_eps = ifelse(consensus_eps == "$n/a", NA, consensus_eps), 
           last_year_eps = ifelse(last_year_eps == "$n/a", NA, last_year_eps), 
           consensus_eps = as.numeric(str_replace(consensus_eps, "\\$", "")), 
           last_year_eps = as.numeric(str_replace(last_year_eps, "\\$", "")), 
           number_ests = as.numeric(number_ests)) %>% 
    select(name, symbol, market_cap, earnings_date, fiscal_quarter, consensus_eps, 
           number_ests, last_year_earnings_date, last_year_eps, earnings_type)
  
  # Return dataframe
  return(earnings)
} 

#' # 3. Download Earnings Announcements Many Function  
#' Description  
#' A wrapper around download_earnings() that loops through a vector of dates and returns the data in a 
#' tidy dataframe.  
#' 
#' Arguments  
#' date: A vector of dates indicating the day to download earnings announcement for.  
#' type: A string indicating whether to download confirmed or estimated earnings announcements.  
#' 
#' Value  
#' Returns a tidy dataframe containing the earnings announcement data.  
download_earnings_many <- function(dates, type) { 
  df_master <- tibble() 
  for(date in dates) { 
    date <- as.Date(date)
    df <- download_earnings(date = date, type = type)
    df_master <- bind_rows(df_master, df)
    Sys.sleep(2)
  }
  return(df_master)
}

#' # 4. Download Earnings Announcements 
#' Create a vector of dates from January 1, 2014 to 30 days in the future and download both 
#' confirmed and estimated earnings announcements.  
dates <- seq(from = as.Date("2014-01-01"), to = Sys.Date() + 30, by = "day")
earnings_confirmed  <- download_earnings_many(dates = dates, type = "confirmed")
earnings_estimated <- download_earnings_many(dates = dates, type = "estimated")

#' # 5. Save Earnings Announcements
write_csv(earnings_confirmed, here::here("/data/earnings-confirmed.csv"))
write_csv(earnings_estimated, here::here("/data/earnings-estimated.csv"))
