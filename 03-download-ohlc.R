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
#' This script downloads OHLC data using AlphaVantage or Yahoo! Finance.    

#' # 1. Source Load Packages
source("01-load-packages.R")

#' # 2. Download AlphaVantage Data Function 
#' Description  
#' Downloads data from AlphaVantage. The documentation can be found at https://www.alphavantage.co/documentation/.  
#' 
#' Arguments  
#' symbol: A string indicating the symbol to download data for.  
#' av_fun: A string indicating the AlphaVantage endpoint.  
#' outputsize: A string indicating the amount of data to download. Can take values compact or full.  
#' 
#' Value  
#' Returns a dataframe containing the OHLC data for the symbol.  
download_alphavantage <- function(symbol, av_fun, outputsize) { 
  
  # Set AlphaVantage API key
  av_api_key("E6TPW2MZ9X85ZC34")
  
  # Call the AlphaVantage endpoint
  print(str_c("Downloading data for ", symbol, "."))
  df <- tryCatch(av_get(av_fun = av_fun, symbol = symbol, outputsize = outputsize) %>% 
                   mutate(symbol = symbol), 
                 error = function(e) { 
                   print(str_c("Unable to download data for ", symbol, ". Retrying.")) 
                   df <- NULL
                   try(df <- av_get(av_fun = av_fun, symbol = symbol, outputsize = outputsize))
                   if (!is_null(df)) print("Success!") 
                   if (is_null(df)) print("Failed!")
                   return(df)
                 })
  
  # Add symbol 
  if(!is.null(df)) 
    df <- df %>% mutate(symbol = symbol) 
  
  # Return dataframe 
  return(df)
}

#' # 3. Download Yahoo! Finance Data Function 
#' Description  
#' Downloads historical OHLC data from Yahoo! Finance. A wrapper around quantmod::getSymbols().   
#' 
#' Arguments  
#' symbol: A string indicating the symbol to download data for.   
#' from: A string indicating the download start date.  
#' to: A string indicating the download end date.  
#' 
#' Value  
#' Returns a dataframe containing the OHLC data for the symbol.  
download_yahoo <- function(symbol, from, to) { 
  
  # Download using quantmod 
  print(str_c("Downloading data for ", symbol, ".")) 
  xts <- tryCatch(getSymbols(Symbols = symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE), 
                  error = function(e) { 
                    print(str_c("Unable to download data for ", symbol, "."))
                    return(NULL)
                  })
  
  # Convert to dataframe and clean 
  if (!is_null(xts)) { 
    df <- xts %>% 
      as_tibble() %>% 
      mutate(timestamp = index(xts), 
             symbol = symbol) %>% 
      select(timestamp, everything(), symbol)
    colnames(df) <- c("timestamp", "open", "high", "low", "close", "volume", "adjusted_close", "symbol")
  } 
  if (is_null(xts)) { 
    df <- NULL
  }
  
  # Return dataframe
  return(df)
}

#' # 4. Load Company List and Original 
list_all <- read_csv("./data/list-all.csv")
original_clean <- read_csv("./data/original-clean.csv")

#' # 5. Clean Company List 
#' Use only companies listed on the NASDAQ, NYSE, and AMEX with a market capitalization of $10 billion or greater. Also include
#' all stocks present in the original data. 
list_clean <- list_all %>% 
  filter(exchange %in% c("NASDAQ", "NYSE", "AMEX"), 
         market_cap >= 10000000000 | symbol %in% original_clean[["symbol"]])
print(list_clean)

#' # 6. Download OHLC Data 
#' Iterates through the symbols in list_clean and calls the download_yahoo() function.  
ohlc_data <- tibble()
for (symbol in list_clean[["symbol"]]) { 
  df <- download_yahoo(symbol = symbol, from = "2014-01-01", to = Sys.Date())
  ohlc_data <- bind_rows(ohlc_data, df)
  Sys.sleep(1)
}

#' # 7. Save Data 
write_csv(ohlc_data, "./data/ohlc-data.csv")

