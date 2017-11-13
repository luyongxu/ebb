#' ---
#' title: "Download Data"
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

#' # 1. Source Load Packages
source("EMS.001 Load Packages.R")

#' # 2. Set Alpha Vantage API Key 
api_key <- "E6TPW2MZ9X85ZC34"

#' # 3. Download Data Function 
download_data <- function(symbol, outputsize, datatype, api_key) { 
  url <- str_c("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&", 
               "symbol=", symbol, "&", 
               "outputsize=", outputsize, "&", 
               "datatype=", datatype, "&", 
               "apikey=", api_key) 
  df <- do.call(rbind, lapply(fromJSON(url)[[2]], unlist)) %>%
    tk_tbl(rename_index = "timestamp") %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, as.numeric) %>% 
    mutate(symbol = symbol) %>% 
    arrange(timestamp) 
  names(df) <- names(df) %>% 
    str_replace("[0-9]+\\. ", "") %>% 
    make.names() %>% 
    tolower()
  return(df)
}

#' # 4. Download List of S&P 500 Companies 
sp500_stocks <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>% 
  html_nodes("td:nth-child(1) .text") %>% 
  html_text()


#' # 5. Download Pricing Data 
pricing_data <- tibble() 
for (ticker in sp500_stocks) { 
  print(str_c("Download data for ", ticker, "."))
  tryCatch(df <- download_data(symbol = ticker, outputsize = "full", datatype = "json", api_key = api_key) , 
           error = function(c) {
             df <- NULL
             print(str_c("Unable to download data for ", ticker, ".")) 
           })
  pricing_data <- pricing_data %>% bind_rows(df)
}

#' # 6. Inspect Data 
print(pricing_data)
glimpse(pricing_data) 
summary(pricing_data)

#' # 7. Save Data 
write_csv(pricing_data, "./Raw Data/pricing_data.csv")
