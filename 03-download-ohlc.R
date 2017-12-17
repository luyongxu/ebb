#' ---
#' title: "Download OHLC Data"
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
#' This script downloads the OHLC data using the AlphaVantage API.  
#' The documentation can be found at https://www.alphavantage.co/documentation/.  

#' # 1. Source Load Packages
source("01-load-packages.R")

#' # 2. Download OHLC Data Function 
#' Description  
#' Downloads the OHLC data from AlphaVantage for the given vector of symbols. 
#' 
#' Arguments  
#' symbols: A vector containing the symbols to download data for.   
#' av_fun: A string indicating the AlphaVantage endpoint.  
#' outputsize: A string indicating the amount of data to download. Can take values compact or full.  
#' 
#' Value  
#' Returns a dataframe in tidy format containing the OHLC data for the selection of symbols.  
download_ohlc <- function(symbol, av_fun, outputsize) { 
  
  # Set AlphaVantage API key
  av_api_key("E6TPW2MZ9X85ZC34")
  
  # Loop through symbols and download data for each symbol 
  df_master <- tibble()
  for (i in seq_along(symbol)) { 
    
    # Call the AlphaVantage endpoint
    print(str_c(i, ": Download data for ", symbol[i], "."))
    df <- tryCatch(av_get(av_fun = av_fun, symbol = symbol[i], outputsize = outputsize) %>% 
                     mutate(symbol = symbol[i]), 
                   error = function(c) {
                     print(str_c("Unable to download data for ", symbol[i], ".")) 
                     return(NULL)
                   })
    
    # Add symbol 
    if(!is.null(df)) 
      df <- df %>% mutate(symbol = symbol[i]) 
    
    # Append to master dataframe
    df_master <- bind_rows(df_master, df)
  }
  
  # Return dataframe 
  return(df_master)
}

#' # 3. Load Company List 
list_clean <- read_csv("./data/list-clean.csv")

#' # 4. Download OHLC Data 
ohlc_data <- download_ohlc(symbol = list_clean[["symbol"]], 
                           av_fun = "TIME_SERIES_DAILY_ADJUSTED", 
                           outputsize = "full")

#' # 5. Print Data 
print(ohlc_data)

#' # 6. Save Data 
write_csv(ohlc, "./data/ohlc-data.csv")

