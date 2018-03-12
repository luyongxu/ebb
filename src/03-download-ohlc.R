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
#' This script downloads OHLC data using AlphaVantage or Yahoo! Finance. Currently Yahoo! Finance is used as the 
#' primary data source.  

#' # 1. Source Load Packages
source(here::here("src/01-load-packages.R"))

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
  
  # Add symbol field 
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

#' # 4. Download Yahoo! Finance Data Many Function  
#' Description  
#' Downloads historical OHLC data from Yahoo! Finance. A wrapper around download_yahoo(). 
#' Can download many symbols at once and returns the data in a tidy dataframe.  
#' 
#' Arguments  
#' symbol: A string or vector indicating the symbols to download data for.   
#' from: A string indicating the download start date.  
#' to: A string indicating the download end date.  
#' 
#' Value  
#' Returns a tidy dataframe containing the OHLC data for the symbols.  
download_yahoo_many <- function(symbols, from, to) { 
  df_master <- tibble()
  for (symbol in symbols) { 
    df <- download_yahoo(symbol = symbol, from = from, to = to)
    df_master <- bind_rows(df_master, df)
    Sys.sleep(1)
  }
  return(df_master)
}

#' # 5. Create Vector of Symbols
#' Use only companies listed on the NASDAQ, NYSE, and AMEX with a market capitalization of $10 billion or greater or 
#' appear in the original position label data.  
symbols_us <- read_csv(here::here("data/list-all.csv")) %>% 
  filter(exchange %in% c("NASDAQ", "NYSE", "AMEX"), 
         market_cap >= 10000000000) %>% 
  .[["symbol"]]
symbols_original <- read_csv(here::here("data/original-clean.csv")) %>% 
  mutate(symbol = ifelse(exchange == "CT Equity", str_c(symbol, ".TO"), symbol)) %>% 
  .[["symbol"]] %>% 
  unique()
symbols <- unique(c(symbols_us, symbols_original))
print(symbols)

#' # 6. Download OHLC Data 
#' Call the download_yahoo_many() function and add to the list of symbols some of the symbols that showed up in the 
#' original labeled data. The original labeled data contains some symbols that trade on the Toronto Stock Exchange 
#' which is indicated by CT Equity in the exchange field. For Yahoo! Finance, the symbol naming convention is to append 
#' .TO to the name of the symbol to pull data from those quotes on the Toronto Stock Exchange. RST is also manually added 
#' because it is below $10 billion cap. 
ohlc_data <- download_yahoo_many(symbols = symbols, 
                                 from = "2014-01-01", 
                                 to = Sys.Date()) 

#' # 7. Save Data 
write_csv(ohlc_data, here::here("data/ohlc-data.csv"))

