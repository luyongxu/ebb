#' ---
#' title: "Download Company List"
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
#' This script downloads a list of publically-traded companies and their associated tickers that are listed on the 
#' NASDAQ, NYSE, AMEX, and TSX. 

#' # 1. Load Packages
source("01-load-packages.R")

#' # 2. Download Company List Function 
#' Description  
#' Downloads a list of all companies listed on an exchange.  
#' 
#' Arguments  
#' exchange: A string that takes values "nasdaq", "nyse", "amex", or "tsx". 
#' 
#' Value  
#' Returns a dataframe containing the companies and their symbol. 
download_company_list <- function(exchange) { 
  
  # For companies listed on NASDAQ, NYSE, and AMEX, use the company list at http://www.nasdaq.com/screening/company-list.aspx
  if(exchange %in% c("nasdaq", "nyse", "amex")) { 
    
    # Download file 
    url <- str_c("http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=", exchange, "&render=download")
    file <- read_csv(url)
    
    # Clean column names 
    colnames(file) <- c("symbol", "name", "last_sale", "market_cap", "ipo_year", 
                        "sector", "industry", "summary_quote", "blank_column")
    
    # Clean dataframe 
    df <- file %>% 
      mutate_all(funs(ifelse(. == "n/a", NA, .))) %>% 
      mutate(last_sale = as.numeric(last_sale), 
             ipo_year = as.numeric(ipo_year), 
             market_cap_number = as.numeric(str_extract(market_cap, "([0-9]*\\.[0-9]+|[0-9]+)")),  
             market_cap_letter = str_extract(market_cap, "[TBM]"), 
             market_cap_scale = ifelse(market_cap_letter == "T", 1000000000000, NA), 
             market_cap_scale = ifelse(market_cap_letter == "B", 1000000000, market_cap_scale), 
             market_cap_scale = ifelse(market_cap_letter == "M", 1000000, market_cap_scale), 
             market_cap = market_cap_number * market_cap_scale, 
             exchange = toupper(exchange)) %>% 
      select(symbol, name, last_sale, market_cap, ipo_year, sector, industry, summary_quote, exchange)
      
    # Return dataframe
    return(df)
  }
  
  # For companies listed on TSX, use the company list at https://www.tsx.com/listings/listing-with-us/listed-company-directory
  if (exchange == "tsx") { 
    
    # Initialize json endpoints 
    endpoints <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", 
                  "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", 
                  "U", "V", "W", "X", "Y", "Z", "0-9")
    
    # Loop through endpoints and download company list 
    df_master <- tibble()
    for (endpoint in endpoints) { 
      
      # Download json file 
      url <- str_c("https://www.tsx.com/json/company-directory/search/tsx/", endpoint)
      
      # Clean dataframe
      json_file <- fromJSON(url, flatten = TRUE)[["results"]]
      df <- json_file[["instruments"]] %>% 
        map_df(bind_rows) %>% 
        left_join(json_file[, c("symbol", "name")], by = "symbol") %>% 
        rename(name = name.y) %>% 
        mutate(name = na.locf(name), 
               exchange = toupper(exchange)) %>% 
        select(symbol, name, exchange) %>% 
        as_tibble()
        
      # Append to master dataframe 
      df_master <- bind_rows(df_master, df)
    }
    
    # Remove duplicates 
    df_master <- df_master %>% unique() 
    
    # Return dataframe
    return(df_master)
  }
}

#' # 3. Download Company Lists
list_nasdaq <- download_company_list(exchange = "nasdaq")
list_nyse <- download_company_list(exchange = "nyse")
list_amex <- download_company_list(exchange = "amex")
list_tsx <- download_company_list(exchange = "tsx")

#' # 4. Combine Company List 
list_all <- bind_rows(list_nasdaq, list_nyse, list_amex, list_tsx)

#' # 5. Print Lists
print(list_nasdaq)
print(list_nyse)
print(list_amex)
print(list_tsx)
print(list_all)

#' # 6. Save Company Lists 
write_csv(list_nasdaq, "./data/list-nasdaq.csv")
write_csv(list_nyse, "./data/list-nyse.csv")
write_csv(list_amex, "./data/list-amex.csv")
write_csv(list_tsx, "./data/list-tsx.csv")
write_csv(list_all, "./data/list-all.csv")
