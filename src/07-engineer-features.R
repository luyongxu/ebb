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
  mutate(return_001 = (yahoo_close / lag(yahoo_close, n = 1) - 1) * 100, 
         return_005 = (yahoo_close / lag(yahoo_close, n = 5) - 1) * 100, 
         return_010 = (yahoo_close / lag(yahoo_close, n = 10) - 1) * 100, 
         return_021 = (yahoo_close / lag(yahoo_close, n = 21) - 1) * 100, 
         return_042 = (yahoo_close / lag(yahoo_close, n = 42) - 1) * 100, 
         return_063 = (yahoo_close / lag(yahoo_close, n = 63) - 1) * 100, 
         return_084 = (yahoo_close / lag(yahoo_close, n = 84) - 1) * 100, 
         return_105 = (yahoo_close / lag(yahoo_close, n = 105) - 1) * 100, 
         return_126 = (yahoo_close / lag(yahoo_close, n = 126) - 1) * 100, 
         return_147 = (yahoo_close / lag(yahoo_close, n = 147) - 1) * 100, 
         return_168 = (yahoo_close / lag(yahoo_close, n = 168) - 1) * 100, 
         return_189 = (yahoo_close / lag(yahoo_close, n = 189) - 1) * 100, 
         return_210 = (yahoo_close / lag(yahoo_close, n = 210) - 1) * 100, 
         return_231 = (yahoo_close / lag(yahoo_close, n = 231) - 1) * 100, 
         return_252 = (yahoo_close / lag(yahoo_close, n = 252) - 1) * 100)

#' # 7. Rolling Drawdown 
combined <- combined %>% 
  group_by(symbol) %>% 
  mutate(drawdown_005 = (yahoo_close / roll_maxr(yahoo_close, n = 5) - 1) * 100, 
         drawdown_010 = (yahoo_close / roll_maxr(yahoo_close, n = 10) - 1) * 100, 
         drawdown_021 = (yahoo_close / roll_maxr(yahoo_close, n = 21) - 1) * 100, 
         drawdown_042 = (yahoo_close / roll_maxr(yahoo_close, n = 42) - 1) * 100, 
         drawdown_063 = (yahoo_close / roll_maxr(yahoo_close, n = 63) - 1) * 100, 
         drawdown_084 = (yahoo_close / roll_maxr(yahoo_close, n = 84) - 1) * 100, 
         drawdown_105 = (yahoo_close / roll_maxr(yahoo_close, n = 105) - 1) * 100, 
         drawdown_126 = (yahoo_close / roll_maxr(yahoo_close, n = 126) - 1) * 100, 
         drawdown_147 = (yahoo_close / roll_maxr(yahoo_close, n = 147) - 1) * 100, 
         drawdown_168 = (yahoo_close / roll_maxr(yahoo_close, n = 168) - 1) * 100, 
         drawdown_189 = (yahoo_close / roll_maxr(yahoo_close, n = 189) - 1) * 100, 
         drawdown_210 = (yahoo_close / roll_maxr(yahoo_close, n = 210) - 1) * 100, 
         drawdown_231 = (yahoo_close / roll_maxr(yahoo_close, n = 231) - 1) * 100, 
         drawdown_252 = (yahoo_close / roll_maxr(yahoo_close, n = 252) - 1) * 100)

#' # 8. Rolling Drawup 
combined <- combined %>% 
  group_by(symbol) %>% 
  mutate(drawup_005 = (yahoo_close / roll_minr(yahoo_close, n = 5) - 1) * 100, 
         drawup_010 = (yahoo_close / roll_minr(yahoo_close, n = 10) - 1) * 100, 
         drawup_021 = (yahoo_close / roll_minr(yahoo_close, n = 21) - 1) * 100, 
         drawup_042 = (yahoo_close / roll_minr(yahoo_close, n = 42) - 1) * 100, 
         drawup_063 = (yahoo_close / roll_minr(yahoo_close, n = 63) - 1) * 100, 
         drawup_084 = (yahoo_close / roll_minr(yahoo_close, n = 84) - 1) * 100, 
         drawup_105 = (yahoo_close / roll_minr(yahoo_close, n = 105) - 1) * 100, 
         drawup_126 = (yahoo_close / roll_minr(yahoo_close, n = 126) - 1) * 100, 
         drawup_147 = (yahoo_close / roll_minr(yahoo_close, n = 147) - 1) * 100, 
         drawup_168 = (yahoo_close / roll_minr(yahoo_close, n = 168) - 1) * 100, 
         drawup_189 = (yahoo_close / roll_minr(yahoo_close, n = 189) - 1) * 100, 
         drawup_210 = (yahoo_close / roll_minr(yahoo_close, n = 210) - 1) * 100, 
         drawup_231 = (yahoo_close / roll_minr(yahoo_close, n = 231) - 1) * 100, 
         drawup_252 = (yahoo_close / roll_minr(yahoo_close, n = 252) - 1) * 100)

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

#' # 18. Money Flow Index 
#' Adds mfi. 
i_mfi <- bind_cols(
  create_indicator(combined, "HLCV", MFI, n = 5) %>% rename(mfi_005 = value), 
  create_indicator(combined, "HLCV", MFI, n = 10) %>% rename(mfi_010 = value), 
  create_indicator(combined, "HLCV", MFI, n = 21) %>% rename(mfi_021 = value), 
  create_indicator(combined, "HLCV", MFI, n = 42) %>% rename(mfi_042 = value), 
  create_indicator(combined, "HLCV", MFI, n = 63) %>% rename(mfi_063 = value), 
  create_indicator(combined, "HLCV", MFI, n = 84) %>% rename(mfi_084 = value), 
  create_indicator(combined, "HLCV", MFI, n = 105) %>% rename(mfi_105 = value), 
  create_indicator(combined, "HLCV", MFI, n = 126) %>% rename(mfi_126 = value), 
  create_indicator(combined, "HLCV", MFI, n = 147) %>% rename(mfi_147 = value), 
  create_indicator(combined, "HLCV", MFI, n = 168) %>% rename(mfi_168 = value), 
  create_indicator(combined, "HLCV", MFI, n = 189) %>% rename(mfi_189 = value), 
  create_indicator(combined, "HLCV", MFI, n = 210) %>% rename(mfi_210 = value), 
  create_indicator(combined, "HLCV", MFI, n = 231) %>% rename(mfi_231 = value), 
  create_indicator(combined, "HLCV", MFI, n = 252) %>% rename(mfi_252 = value)
)
combined <- bind_cols(combined, i_mfi)

#' # 20. Chande Momentum Oscillator
#' Adds cmo.
i_cmo <- bind_cols(
  create_indicator(combined, "C", CMO, n = 5) %>% rename(cmo_005 = value), 
  create_indicator(combined, "C", CMO, n = 10) %>% rename(cmo_010 = value), 
  create_indicator(combined, "C", CMO, n = 21) %>% rename(cmo_021 = value), 
  create_indicator(combined, "C", CMO, n = 42) %>% rename(cmo_042 = value), 
  create_indicator(combined, "C", CMO, n = 63) %>% rename(cmo_063 = value), 
  create_indicator(combined, "C", CMO, n = 84) %>% rename(cmo_084 = value), 
  create_indicator(combined, "C", CMO, n = 105) %>% rename(cmo_105 = value), 
  create_indicator(combined, "C", CMO, n = 126) %>% rename(cmo_126 = value), 
  create_indicator(combined, "C", CMO, n = 147) %>% rename(cmo_147 = value), 
  create_indicator(combined, "C", CMO, n = 168) %>% rename(cmo_168 = value), 
  create_indicator(combined, "C", CMO, n = 189) %>% rename(cmo_189 = value), 
  create_indicator(combined, "C", CMO, n = 210) %>% rename(cmo_210 = value), 
  create_indicator(combined, "C", CMO, n = 231) %>% rename(cmo_231 = value), 
  create_indicator(combined, "C", CMO, n = 252) %>% rename(cmo_252 = value)
)
combined <- bind_cols(combined, i_cmo)

#' # 21. Vertical Horizonal Filter
#' Adds vhf. 
i_vhf <- bind_cols(
  create_indicator(combined, "C", VHF, n = 5) %>% rename(vhf_005 = value), 
  create_indicator(combined, "C", VHF, n = 10) %>% rename(vhf_010 = value), 
  create_indicator(combined, "C", VHF, n = 21) %>% rename(vhf_021 = value), 
  create_indicator(combined, "C", VHF, n = 42) %>% rename(vhf_042 = value), 
  create_indicator(combined, "C", VHF, n = 63) %>% rename(vhf_063 = value), 
  create_indicator(combined, "C", VHF, n = 84) %>% rename(vhf_084 = value), 
  create_indicator(combined, "C", VHF, n = 105) %>% rename(vhf_105 = value), 
  create_indicator(combined, "C", VHF, n = 126) %>% rename(vhf_126 = value), 
  create_indicator(combined, "C", VHF, n = 147) %>% rename(vhf_147 = value), 
  create_indicator(combined, "C", VHF, n = 168) %>% rename(vhf_168 = value), 
  create_indicator(combined, "C", VHF, n = 189) %>% rename(vhf_189 = value), 
  create_indicator(combined, "C", VHF, n = 210) %>% rename(vhf_210 = value), 
  create_indicator(combined, "C", VHF, n = 231) %>% rename(vhf_231 = value), 
  create_indicator(combined, "C", VHF, n = 252) %>% rename(vhf_252 = value)
)
combined <- bind_cols(combined, i_vhf)

#' # 22. Clean Combined 
combined[do.call(cbind, lapply(combined, is.nan))] <- NA
  
#' # 23. Split into Train and Test 
train <- combined %>% filter(is.na(position_label) == FALSE)
test <- combined %>% filter(is.na(position_label) == TRUE) 

#' # 24. Save Data 
write_csv(train, "./data/train.csv")
write_csv(test, "./data/test.csv")
