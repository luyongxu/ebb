#' ---
#' title: "Engineer Features"
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
source(here::here("/src/01-load-packages.R"))

#' # 2. Load Data 
combined <- read_csv(here::here("/data/combined.csv"), col_types = c("Dddddddcidiciic"))

#' # 3. Choose Data to Calculate Features 
#' Can choose either training data for training the model or test data for prediction. Move this option into 
#' a command line argument later.  
data_source <- "train"
if (data_source == "train") { 
  train_symbols <- combined %>% 
    filter(source == "train") %>% 
    .[["symbol"]] %>% 
    unique()
  combined <- combined %>% 
    filter(symbol %in% train_symbols)
}
if (data_source =="test") { 
  combined <- combined %>% 
    filter(timestamp >= Sys.Date() - 400)
}

#' # 4. Technical Indicator Function 
#' Description  
#' A wrapper around TTR functions so that technical indicators can be calculated for multiple symbols. 
#' Takes tibbles as inputs and returns a tibble.  
#' 
#' Arguments  
#' combined: A tibble representing the combined data.  
#' input: A string indicating the various OHLCV (open, high, low, close, volume) columns that are passed 
#'  to the TTR function.  
#' f: The name of the TTR function.  
#' 
#' Value 
#' Returns a tibble with observations in the same order as the combined data.   
create_indicator <- function(combined, input, f, ...) { 
  
  # Initialize new list to save output 
  indicator <- vector("list", length(unique(combined[["symbol"]]))) 
  
  # Loop through symbols and create the technical indicator for each symbol
  for (i in unique(combined[["symbol"]])) { 
    
    # Limit dataframe to an individual symbol to pass to TTR function 
    df_input <- combined %>% filter(symbol == i)
    
    # Create volume input for indicators that take a volume argument 
    df_volume <- df_input %>% select(yahoo_volume)
    
    # Create the dataframe to be passed to the TTR function 
    if (input == "OHLC") { 
      df_indicator <- df_input %>% 
        select(yahoo_open, yahoo_high, yahoo_low, yahoo_close) %>% 
        f(...) %>% 
        as_tibble() 
    } else if (input == "HLC") { 
      df_indicator <- df_input %>% 
        select(yahoo_high, yahoo_low, yahoo_close) %>% 
        f(...) %>% 
        as_tibble()  
    } else if (input == "HL") { 
      df_indicator <- df_input %>% 
        select(yahoo_high, yahoo_low) %>% 
        f(...) %>% 
        as_tibble()  
    } else if (input == "C") { 
      df_indicator <- df_input %>% 
        select(yahoo_close) %>% 
        f(...) %>% 
        as_tibble()  
    } else if (input == "V") { 
      df_indicator <- df_input %>% 
        select(yahoo_volume) %>% 
        f(...) %>% 
        as_tibble()  
    } else if (input == "HLCV") { 
      df_indicator <- df_input %>% 
        select(yahoo_high, yahoo_low, yahoo_close) %>% 
        f(volume = df_volume, ...) %>% 
        as_tibble()
    } else if (input == "HLV") { 
      df_indicator <- df_input %>% 
        select(yahoo_high, yahoo_low) %>% 
        f(volume = df_volume, ...) %>% 
        as_tibble()
    } else if (input == "CV") { 
      df_indicator <- df_input %>% 
        select(yahoo_close) %>% 
        f(volume = df_volume, ...) %>% 
        as_tibble()
    }
    
    # Save created indicators to indicator list 
    indicator[[i]] <- df_indicator
  } 
  
  # Convert indicator list to dataframe 
  indicator <- indicator %>% map_df(bind_rows)
  
  # Return indicator for all symbols
  return(indicator)
} 

#' # 5. Add Functions 
#' Fix error in TTR::SNR function. 
SNR <- function (HLC, n, ...) {
  HLC <- try.xts(HLC, error = as.matrix)
  snr <- abs(HLC[, 3] - lag.xts(HLC[, 3], n))/ATR(HLC, n, ...)[ ,"atr"]
  return(reclass(snr, HLC))
}

#' # 6. Rolling Return 
combined <- combined %>% 
  group_by(symbol) %>% 
  mutate(return_001 = yahoo_close / lag(yahoo_close, n = 1) - 1, 
         return_005 = yahoo_close / lag(yahoo_close, n = 5) - 1, 
         return_010 = yahoo_close / lag(yahoo_close, n = 10) - 1, 
         return_021 = yahoo_close / lag(yahoo_close, n = 21) - 1, 
         return_042 = yahoo_close / lag(yahoo_close, n = 42) - 1, 
         return_063 = yahoo_close / lag(yahoo_close, n = 63) - 1, 
         return_084 = yahoo_close / lag(yahoo_close, n = 84) - 1, 
         return_105 = yahoo_close / lag(yahoo_close, n = 105) - 1, 
         return_126 = yahoo_close / lag(yahoo_close, n = 126) - 1, 
         return_147 = yahoo_close / lag(yahoo_close, n = 147) - 1, 
         return_168 = yahoo_close / lag(yahoo_close, n = 168) - 1, 
         return_189 = yahoo_close / lag(yahoo_close, n = 189) - 1, 
         return_210 = yahoo_close / lag(yahoo_close, n = 210) - 1, 
         return_231 = yahoo_close / lag(yahoo_close, n = 231) - 1, 
         return_252 = yahoo_close / lag(yahoo_close, n = 252) - 1)

#' # 7. Rolling Drawdown 
combined <- combined %>% 
  group_by(symbol) %>% 
  mutate(drawdown_005 = yahoo_close / roll_maxr(yahoo_close, n = 5) - 1, 
         drawdown_010 = yahoo_close / roll_maxr(yahoo_close, n = 10) - 1, 
         drawdown_021 = yahoo_close / roll_maxr(yahoo_close, n = 21) - 1, 
         drawdown_042 = yahoo_close / roll_maxr(yahoo_close, n = 42) - 1, 
         drawdown_063 = yahoo_close / roll_maxr(yahoo_close, n = 63) - 1, 
         drawdown_084 = yahoo_close / roll_maxr(yahoo_close, n = 84) - 1, 
         drawdown_105 = yahoo_close / roll_maxr(yahoo_close, n = 105) - 1, 
         drawdown_126 = yahoo_close / roll_maxr(yahoo_close, n = 126) - 1, 
         drawdown_147 = yahoo_close / roll_maxr(yahoo_close, n = 147) - 1, 
         drawdown_168 = yahoo_close / roll_maxr(yahoo_close, n = 168) - 1, 
         drawdown_189 = yahoo_close / roll_maxr(yahoo_close, n = 189) - 1, 
         drawdown_210 = yahoo_close / roll_maxr(yahoo_close, n = 210) - 1, 
         drawdown_231 = yahoo_close / roll_maxr(yahoo_close, n = 231) - 1, 
         drawdown_252 = yahoo_close / roll_maxr(yahoo_close, n = 252) - 1)

#' # 8. Rolling Drawup 
combined <- combined %>% 
  group_by(symbol) %>% 
  mutate(drawup_005 = yahoo_close / roll_minr(yahoo_close, n = 5) - 1, 
         drawup_010 = yahoo_close / roll_minr(yahoo_close, n = 10) - 1, 
         drawup_021 = yahoo_close / roll_minr(yahoo_close, n = 21) - 1, 
         drawup_042 = yahoo_close / roll_minr(yahoo_close, n = 42) - 1, 
         drawup_063 = yahoo_close / roll_minr(yahoo_close, n = 63) - 1, 
         drawup_084 = yahoo_close / roll_minr(yahoo_close, n = 84) - 1, 
         drawup_105 = yahoo_close / roll_minr(yahoo_close, n = 105) - 1, 
         drawup_126 = yahoo_close / roll_minr(yahoo_close, n = 126) - 1, 
         drawup_147 = yahoo_close / roll_minr(yahoo_close, n = 147) - 1, 
         drawup_168 = yahoo_close / roll_minr(yahoo_close, n = 168) - 1, 
         drawup_189 = yahoo_close / roll_minr(yahoo_close, n = 189) - 1, 
         drawup_210 = yahoo_close / roll_minr(yahoo_close, n = 210) - 1, 
         drawup_231 = yahoo_close / roll_minr(yahoo_close, n = 231) - 1, 
         drawup_252 = yahoo_close / roll_minr(yahoo_close, n = 252) - 1)

#' # 9. Rolling Number of Positive Days 
combined <- combined %>% 
  group_by(symbol) %>% 
  mutate(positive = ifelse(return_001 > 0, 1, 0), 
         positive_005 = roll_sumr(positive, n = 5), 
         positive_010 = roll_sumr(positive, n = 10), 
         positive_021 = roll_sumr(positive, n = 21), 
         positive_042 = roll_sumr(positive, n = 42), 
         positive_063 = roll_sumr(positive, n = 63), 
         positive_084 = roll_sumr(positive, n = 84), 
         positive_105 = roll_sumr(positive, n = 105), 
         positive_126 = roll_sumr(positive, n = 126), 
         positive_147 = roll_sumr(positive, n = 147), 
         positive_168 = roll_sumr(positive, n = 168), 
         positive_189 = roll_sumr(positive, n = 189), 
         positive_210 = roll_sumr(positive, n = 210), 
         positive_231 = roll_sumr(positive, n = 231), 
         positive_252 = roll_sumr(positive, n = 252))

#' # 10. Rolling Volatility 
combined <- combined %>% 
  group_by(symbol) %>% 
  mutate(volatility_005 = roll_sdr(return_001, n = 5), 
         volatility_010 = roll_sdr(return_001, n = 10), 
         volatility_021 = roll_sdr(return_001, n = 21), 
         volatility_042 = roll_sdr(return_001, n = 42), 
         volatility_063 = roll_sdr(return_001, n = 63), 
         volatility_084 = roll_sdr(return_001, n = 84), 
         volatility_105 = roll_sdr(return_001, n = 105), 
         volatility_126 = roll_sdr(return_001, n = 126), 
         volatility_147 = roll_sdr(return_001, n = 147), 
         volatility_168 = roll_sdr(return_001, n = 168), 
         volatility_189 = roll_sdr(return_001, n = 189), 
         volatility_210 = roll_sdr(return_001, n = 210), 
         volatility_231 = roll_sdr(return_001, n = 231), 
         volatility_252 = roll_sdr(return_001, n = 252)) %>% 
  ungroup()

#' # 11. Relative Strength Index 
#' #' Adds rsi.  
i_rsi <- bind_cols(
  create_indicator(combined, "C", RSI, n = 5) %>% rename(rsi_005 = value), 
  create_indicator(combined, "C", RSI, n = 10) %>% rename(rsi_010 = value), 
  create_indicator(combined, "C", RSI, n = 21) %>% rename(rsi_021 = value), 
  create_indicator(combined, "C", RSI, n = 42) %>% rename(rsi_042 = value), 
  create_indicator(combined, "C", RSI, n = 63) %>% rename(rsi_063 = value), 
  create_indicator(combined, "C", RSI, n = 84) %>% rename(rsi_084 = value), 
  create_indicator(combined, "C", RSI, n = 105) %>% rename(rsi_105 = value), 
  create_indicator(combined, "C", RSI, n = 126) %>% rename(rsi_126 = value), 
  create_indicator(combined, "C", RSI, n = 147) %>% rename(rsi_147 = value), 
  create_indicator(combined, "C", RSI, n = 168) %>% rename(rsi_168 = value), 
  create_indicator(combined, "C", RSI, n = 189) %>% rename(rsi_189 = value), 
  create_indicator(combined, "C", RSI, n = 210) %>% rename(rsi_210 = value), 
  create_indicator(combined, "C", RSI, n = 231) %>% rename(rsi_231 = value), 
  create_indicator(combined, "C", RSI, n = 252) %>% rename(rsi_252 = value)
)
combined <- bind_cols(combined, i_rsi)

#' # 12. Aroon
#' Adds aroonUp, aroonDn, oscillator.
i_aroon <- bind_cols( 
  create_indicator(combined, "HL", aroon, n = 5) %>% 
    rename(aroonUp_005 = aroonUp, aroonDn_005 = aroonDn, aroon_005 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 10) %>% 
    rename(aroonUp_010 = aroonUp, aroonDn_010 = aroonDn, aroon_010 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 21) %>% 
    rename(aroonUp_021 = aroonUp, aroonDn_021 = aroonDn, aroon_021 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 42) %>% 
    rename(aroonUp_042 = aroonUp, aroonDn_042 = aroonDn, aroon_042 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 63) %>% 
    rename(aroonUp_063 = aroonUp, aroonDn_063 = aroonDn, aroon_063 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 84) %>% 
    rename(aroonUp_084 = aroonUp, aroonDn_084 = aroonDn, aroon_084 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 105) %>% 
    rename(aroonUp_105 = aroonUp, aroonDn_105 = aroonDn, aroon_105 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 126) %>% 
    rename(aroonUp_126 = aroonUp, aroonDn_126 = aroonDn, aroon_126 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 147) %>% 
    rename(aroonUp_147 = aroonUp, aroonDn_147 = aroonDn, aroon_147 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 168) %>% 
    rename(aroonUp_168 = aroonUp, aroonDn_168 = aroonDn, aroon_168 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 189) %>% 
    rename(aroonUp_189 = aroonUp, aroonDn_189 = aroonDn, aroon_189 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 210) %>% 
    rename(aroonUp_210 = aroonUp, aroonDn_210 = aroonDn, aroon_210 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 231) %>% 
    rename(aroonUp_231 = aroonUp, aroonDn_231 = aroonDn, aroon_231 = oscillator), 
  create_indicator(combined, "HL", aroon, n = 252) %>% 
    rename(aroonUp_252 = aroonUp, aroonDn_252 = aroonDn, aroon_252 = oscillator)
)
combined <- bind_cols(combined, i_aroon)

#' # 13. Commodity Channel Index (CCI)
#' Adds cci.
i_cci <- bind_cols(
  create_indicator(combined, "HLC", CCI, n = 5) %>% rename(cci_005 = value), 
  create_indicator(combined, "HLC", CCI, n = 10) %>% rename(cci_010 = value), 
  create_indicator(combined, "HLC", CCI, n = 21) %>% rename(cci_021 = value), 
  create_indicator(combined, "HLC", CCI, n = 42) %>% rename(cci_042 = value), 
  create_indicator(combined, "HLC", CCI, n = 63) %>% rename(cci_063 = value), 
  create_indicator(combined, "HLC", CCI, n = 84) %>% rename(cci_084 = value), 
  create_indicator(combined, "HLC", CCI, n = 105) %>% rename(cci_105 = value), 
  create_indicator(combined, "HLC", CCI, n = 126) %>% rename(cci_126 = value), 
  create_indicator(combined, "HLC", CCI, n = 147) %>% rename(cci_147 = value), 
  create_indicator(combined, "HLC", CCI, n = 168) %>% rename(cci_168 = value), 
  create_indicator(combined, "HLC", CCI, n = 189) %>% rename(cci_189 = value), 
  create_indicator(combined, "HLC", CCI, n = 210) %>% rename(cci_210 = value), 
  create_indicator(combined, "HLC", CCI, n = 231) %>% rename(cci_231 = value), 
  create_indicator(combined, "HLC", CCI, n = 252) %>% rename(cci_252 = value)
)
combined <- bind_cols(combined, i_cci)

#' # 14. Chaikin Volatility
#' Adds chaikinvol.
i_chaikinvol <- bind_cols(
  create_indicator(combined, "HL", chaikinVolatility, n = 5) %>% rename(chaikinvol_005 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 10) %>% rename(chaikinvol_010 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 21) %>% rename(chaikinvol_021 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 42) %>% rename(chaikinvol_042 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 63) %>% rename(chaikinvol_063 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 84) %>% rename(chaikinvol_084 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 105) %>% rename(chaikinvol_105 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 126) %>% rename(chaikinvol_126 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 147) %>% rename(chaikinvol_147 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 168) %>% rename(chaikinvol_168 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 189) %>% rename(chaikinvol_189 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 210) %>% rename(chaikinvol_210 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 231) %>% rename(chaikinvol_231 = value), 
  create_indicator(combined, "HL", chaikinVolatility, n = 252) %>% rename(chaikinvol_252 = value)
)
combined <- bind_cols(combined, i_chaikinvol)

#' # 15. Chaikin Money Flow
#' Adds cmf.
i_cmf <- bind_cols(
  create_indicator(combined, "HLCV", CMF, n = 5) %>% rename(cmf_005 = value), 
  create_indicator(combined, "HLCV", CMF, n = 10) %>% rename(cmf_010 = value), 
  create_indicator(combined, "HLCV", CMF, n = 21) %>% rename(cmf_021 = value), 
  create_indicator(combined, "HLCV", CMF, n = 42) %>% rename(cmf_042 = value), 
  create_indicator(combined, "HLCV", CMF, n = 63) %>% rename(cmf_063 = value), 
  create_indicator(combined, "HLCV", CMF, n = 84) %>% rename(cmf_084 = value), 
  create_indicator(combined, "HLCV", CMF, n = 105) %>% rename(cmf_105 = value), 
  create_indicator(combined, "HLCV", CMF, n = 126) %>% rename(cmf_126 = value), 
  create_indicator(combined, "HLCV", CMF, n = 147) %>% rename(cmf_147 = value), 
  create_indicator(combined, "HLCV", CMF, n = 168) %>% rename(cmf_168 = value), 
  create_indicator(combined, "HLCV", CMF, n = 189) %>% rename(cmf_189 = value), 
  create_indicator(combined, "HLCV", CMF, n = 210) %>% rename(cmf_210 = value), 
  create_indicator(combined, "HLCV", CMF, n = 231) %>% rename(cmf_231 = value), 
  create_indicator(combined, "HLCV", CMF, n = 252) %>% rename(cmf_252 = value)
)
combined <- bind_cols(combined, i_cmf)

#' # 16. Signal to Noise Ratio
#' Adds snr. 
i_snr <- bind_cols(
  create_indicator(combined, "HLC", SNR, n = 5) %>% rename(snr_005 = value), 
  create_indicator(combined, "HLC", SNR, n = 10) %>% rename(snr_010 = value), 
  create_indicator(combined, "HLC", SNR, n = 21) %>% rename(snr_021 = value), 
  create_indicator(combined, "HLC", SNR, n = 42) %>% rename(snr_042 = value), 
  create_indicator(combined, "HLC", SNR, n = 63) %>% rename(snr_063 = value), 
  create_indicator(combined, "HLC", SNR, n = 84) %>% rename(snr_084 = value), 
  create_indicator(combined, "HLC", SNR, n = 105) %>% rename(snr_105 = value), 
  create_indicator(combined, "HLC", SNR, n = 126) %>% rename(snr_126 = value), 
  create_indicator(combined, "HLC", SNR, n = 147) %>% rename(snr_147 = value), 
  create_indicator(combined, "HLC", SNR, n = 168) %>% rename(snr_168 = value), 
  create_indicator(combined, "HLC", SNR, n = 189) %>% rename(snr_189 = value), 
  create_indicator(combined, "HLC", SNR, n = 210) %>% rename(snr_210 = value), 
  create_indicator(combined, "HLC", SNR, n = 231) %>% rename(snr_231 = value), 
  create_indicator(combined, "HLC", SNR, n = 252) %>% rename(snr_252 = value)
)
combined <- bind_cols(combined, i_snr)

#' # 17. Williams R
#' Adds williams_r.
i_williamsr <- bind_cols(
  create_indicator(combined, "HLC", WPR, n = 5) %>% rename(williamsr_005 = value), 
  create_indicator(combined, "HLC", WPR, n = 10) %>% rename(williamsr_010 = value), 
  create_indicator(combined, "HLC", WPR, n = 21) %>% rename(williamsr_021 = value), 
  create_indicator(combined, "HLC", WPR, n = 42) %>% rename(williamsr_042 = value), 
  create_indicator(combined, "HLC", WPR, n = 63) %>% rename(williamsr_063 = value), 
  create_indicator(combined, "HLC", WPR, n = 84) %>% rename(williamsr_084 = value), 
  create_indicator(combined, "HLC", WPR, n = 105) %>% rename(williamsr_105 = value), 
  create_indicator(combined, "HLC", WPR, n = 126) %>% rename(williamsr_126 = value), 
  create_indicator(combined, "HLC", WPR, n = 147) %>% rename(williamsr_147 = value), 
  create_indicator(combined, "HLC", WPR, n = 168) %>% rename(williamsr_168 = value), 
  create_indicator(combined, "HLC", WPR, n = 189) %>% rename(williamsr_189 = value), 
  create_indicator(combined, "HLC", WPR, n = 210) %>% rename(williamsr_210 = value), 
  create_indicator(combined, "HLC", WPR, n = 231) %>% rename(williamsr_231 = value), 
  create_indicator(combined, "HLC", WPR, n = 252) %>% rename(williamsr_252 = value)
)
combined <- bind_cols(combined, i_williamsr)


#' 
#' #' # 7. Average True Range (ATR)
#' #' Adds tr, atr, trueHigh, trueLow.
#' i_atr <- create_indicator(combined, "HLC", ATR, n = 14)
#' tail(i_atr)
#' combined <- bind_cols(combined, i_atr)
#' 
#' #' # 8. Bollinger Bands
#' #' Adds dn, mavg, up, pctB.
#' i_bbands <- create_indicator(combined, "HLC", BBands, n = 20,  sd = 2)
#' tail(i_bbands)
#' combined <- bind_cols(combined, i_bbands)
#' 

#' 

#' 

#' 
#' #' # 12. Close Location Value 
#' #' Adds clv. 
#' i_clv <- create_indicator(combined, "HLC", CLV) %>%
#'   rename(clv = value)
#' tail(i_clv)
#' combined <- bind_cols(combined, i_clv)
#' 

#' 
#' #' # 14. Chande Momentum Oscillator Close 
#' #' Adds cmo_close.
#' i_cmo_close <- create_indicator(combined, "C", CMO, n = 14 ) %>%
#'   rename(cmo_close = value)
#' tail(i_cmo_close)
#' combined <- bind_cols(combined, i_cmo_close)
#' 
#' #' # 15. Chande Momentum Oscillator Volume 
#' #' Adds cmo_volume. 
#' i_cmo_volume <- create_indicator(combined, "V", CMO, n = 14) %>%
#'   rename(cmo_volume = value)
#' tail(i_cmo_volume)
#' combined <- bind_cols(combined, i_cmo_volume)
#' 
#' #' # 16. Donchian Channel 
#' #' Adds dc_high, dc_mid, dc_low. 
#' i_donchian <- create_indicator(combined, "HL", DonchianChannel, n = 10, include.lag = FALSE) %>%
#'   rename(dc_high = high, dc_mid = mid, dc_low = low)
#' tail(i_donchian)
#' combined <- bind_cols(combined, i_donchian)
#' 
#' #' # 17. Detrended Price Oscillator Close 
#' #' Adds dpo_close, dpo_volume. 
#' i_dpo_close <- create_indicator(combined, "C", DPO, n = 10,  shift = 0, percent = TRUE) %>%
#'   rename(dpo_close = value)
#' tail(i_dpo_close)
#' combined <- bind_cols(combined, i_dpo_close)
#' 
#' #' # 18. Detrended Price Oscillator Volume 
#' i_dpo_volume <- create_indicator(combined, "V", DPO, n = 10,  shift = 0, percent = TRUE) %>%
#'   rename(dpo_volume = value)
#' tail(i_dpo_volume)
#' combined <- bind_cols(combined, i_dpo_volume)
#' 
#' #' # 19. DV Intermediate Oscillator 
#' #' Adds dvi_mag, dvi_str, dvi.  
#' i_dvi <- create_indicator(combined, "C", DVI, n = 252, wts = c(0.8, 0.2), smooth = 3,
#'                           magnitude = c(5, 100, 5), stretch = c(10, 100, 2), exact.multiplier = 1) %>%
#'   rename(dvi_mag = dvi.mag, dvi_str = dvi.str)
#' tail(i_dvi)
#' combined <- bind_cols(combined, i_dvi)
#' 
#' #' # 20. Ease of Movement Value 
#' #' Adds emv, maEMV. 
#' i_emv <- create_indicator(combined, "HLV", EMV, n = 9,  vol.divisor = 1)
#' tail(i_emv)
#' combined <- bind_cols(combined, i_dvi)
#' 
#' #' # 21. Guppy Multiple Moving Averages 
#' #' Adds short_lag_x, long_lag_x. 
#' i_gmma <- create_indicator(combined, "C", GMMA, short = c(3, 5, 8, 10, 12, 15),
#'                            long = c(30, 35, 40, 45, 50, 60))
#' colnames(i_gmma) <- str_replace_all(colnames(i_gmma), " ", "_")
#' tail(i_gmma)
#' combined <- bind_cols(combined, i_gmma)
#' 
#' #' # 22. Know Sure Thing 
#' #' Adds kst, kst_signal. 
#' i_kst <- create_indicator(combined, "C", KST, n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30),
#'                           nSig = 9,  wts = 1:4) %>%
#'   rename(kst_signal = signal)
#' tail(i_kst)
#' combined <- bind_cols(combined, i_kst)
#' 

#' 
#' #' # 24. MACD Volume 
#' #' Adds macd_volume, macd_signal_volume. 
#' i_macd_volume <- create_indicator(combined, "V", MACD, nFast = 12, nSlow = 26, nSig = 9, percent = TRUE) %>%
#'   rename(macd_volume = macd, macd_signal_volume = signal)
#' tail(i_macd_volume)
#' combined <- bind_cols(combined, i_macd_volume)
#' 
#' #' # 25. Money Flow Index 
#' #' Adds mfi. 
#' i_mfi <- create_indicator(combined, "HLCV", MFI, n = 14) %>%
#'   rename(mfi = value)
#' tail(i_mfi)
#' combined <- bind_cols(combined, i_mfi)
#' 
#' #' # 26. On Balance Volume 
#' #' Adds obv. 
#' i_obv <- create_indicator(combined, "CV", OBV) %>%
#'   rename(obv = value)
#' tail(i_obv)
#' combined <- bind_cols(combined, i_obv)
#' 
#' #' # 27. Volatility Bands 
#' #' Adds pbands_dn, pbands_center, pbands_up.  
#' i_pbands <- create_indicator(combined, "C", PBands, n = 20,
#'                              sd = 2, fastn = 2, centered = FALSE, lavg = FALSE) %>%
#'   rename(pbands_dn = dn, pbands_center = center, pbands_up = up)
#' tail(i_pbands)
#' combined <- bind_cols(combined, i_pbands)
#' 

#' 
#' #' # 29. Percent Rank Close 
#' #' Adds percent_rank_close. 
#' i_percent_rank_close <- create_indicator(combined, "C", runPercentRank, n = 252, cumulative = FALSE, exact.multiplier = 0.5) %>%
#'   rename(percent_rank_close = value)
#' tail(i_percent_rank_close)
#' combined <- bind_cols(combined, i_percent_rank_close)
#' 
#' #' # 30. Percent Rank Volume 
#' #' Adds percent_rank_volume.  
#' i_percent_rank_volume <- create_indicator(combined, "V", runPercentRank, n = 252, cumulative = FALSE, exact.multiplier = 0.5) %>%
#'   rename(percent_rank_volume = value)
#' tail(i_percent_rank_volume)
#' combined <- bind_cols(combined, i_percent_rank_volume)
#' 
#' #' # 31. Parabolic Stop and Reverse 
#' #' Adds sar.  
#' i_sar <- create_indicator(combined, "HL", SAR, accel = c(0.02, 0.2)) %>%
#'   rename(sar = value)
#' tail(i_sar)
#' combined <- bind_cols(combined, i_sar)
#' 

#' 
#' #' # 33. Stochastic Oscillator 
#' #' Adds fastK, fastD, slowD. 
#' i_stoch <- create_indicator(combined, "HLC", stoch, nFastK = 14, nFastD = 3, nSlowD = 3, bounded = TRUE, smooth = 1)
#' tail(i_stoch)
#' combined <- bind_cols(combined, i_stoch)
#' 
#' #' # 34. Stochastic Momentum Index 
#' #' Adds smi. SMI function in TTR produces an error. Debug later.  
#' i_smi <- create_indicator(combined, "HLC", SMI, n = 13, nFast = 2, nSlow = 25, nSig = 9,  bounded = TRUE)
#' 
#' #' # 35. Trend Detection Index 
#' #' Adds tdi, di.  
#' i_tdi <- create_indicator(combined, "C", TDI, n = 20, multiple = 2)
#' tail(i_tdi)
#' combined <- bind_cols(combined, i_tdi)
#' 
#' #' # 36. Triple Smoothed Exponential Oscillator 
#' #' Adds trix, trix_signal. 
#' i_trix <- create_indicator(combined, "C", TRIX, n = 20, nSig = 9,  percent = TRUE) %>%
#'   rename(trix = TRIX, trix_signal = signal)
#' tail(i_trix)
#' combined <- bind_cols(combined, i_trix)
#' 
#' #' # 37. Ultimate Oscillator 
#' #' Adds ultimate_oscillator. 
#' i_ultimate <- create_indicator(combined, "HLC", ultimateOscillator, n = c(7, 14, 28), wts = c(4, 2, 1)) %>%
#'   rename(ultimate_oscillator = value)
#' tail(i_ultimate)
#' combined <- bind_cols(combined, i_ultimate)
#' 
#' #' # 38. Vertical Horizonal Filter
#' #' Adds vhf. 
#' i_vhf <- create_indicator(combined, "C", VHF, n = 28) %>%
#'   rename(vhf = value)
#' tail(i_vhf)
#' combined <- bind_cols(combined, i_vhf)
#' 
#' #' # 39. Volatility Indicators 
#' #' Adds volatility_close, volatility_garman, volatility_parkinson, volatility_rogers, volatility_garman2, volatility_yang. 
#' i_volatility_close <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "close", N = 260, mean0 = FALSE) %>%
#'   rename(volatility_close = value)
#' i_volatility_garman <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "garman.klass", N = 260, mean0 = FALSE) %>%
#'   rename(volatility_garman = value)
#' i_volatility_parkinson <- create_indicator(combined, "OHLC", volatility, calc = "parkinson", N = 260, mean0 = FALSE) %>%
#'   rename(volatility_parkinson = value)
#' i_volatility_rogers <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "rogers.satchell", N = 260, mean0 = FALSE) %>%
#'   rename(volatility_rogers = value)
#' i_volatility_garman2 <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "gk.yz", N = 260, mean0 = FALSE) %>%
#'   rename(volatility_garman2 = value)
#' i_volatility_yang <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "yang.zhang", N = 260, mean0 = FALSE) %>%
#'   rename(volatility_yang = value)
#' tail(i_volatility_close)
#' tail(i_volatility_garman)
#' tail(i_volatility_parkinson)
#' tail(i_volatility_rogers)
#' tail(i_volatility_garman2)
#' tail(i_volatility_yang)
#' combined <- bind_cols(combined, i_volatility_close)
#' combined <- bind_cols(combined, i_volatility_garman)
#' combined <- bind_cols(combined, i_volatility_parkinson)
#' combined <- bind_cols(combined, i_volatility_rogers)
#' combined <- bind_cols(combined, i_volatility_garman2)
#' combined <- bind_cols(combined, i_volatility_yang)
#' 
#' #' # 40. Williams Accumulation Distribution 
#' #' Adds williams_ad. 
#' i_williamsad <- create_indicator(combined, "HLC", williamsAD) %>%
#'   rename(williams_ad = value)
#' tail(i_williamsad)
#' combined <- bind_cols(combined, i_williamsad)
#' 

#' 
#' #' # 42. Zig Zag 
#' i_zigzag <- create_indicator(combined, "HL", ZigZag, change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE) %>%
#'   rename(zigzag = value)
#' tail(i_zigzag)
#' combined <- bind_cols(combined, i_zigzag)

#' # 43. Split into Train and Test 
train <- combined %>%
  filter(is.na(position_label) == FALSE)
test <- combined %>%
  filter(is.na(position_label) == TRUE)

#' # 44. Save Data 
write_csv(train, "./data/train.csv")
write_csv(test, "./data/test.csv")
