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
  
#' # 3. Technical Indicator Function 
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
  
  # Initialize new combined tibble 
  indicator <- tibble() 
  
  # Loop through symbols and create the technical indicator for each symbol
  for (i in unique(combined[["symbol"]])) { 
    
    # Limit dataframe to an individual symbol to pass to TTR function 
    df_input <- combined %>% filter(symbol == i)
    
    # Create volume input for indicators that take a volume argument 
    df_volume <- df_input %>% select(yahoo_volume)
    
    # Create the dataframe to be passed to the TTR function 
    if (input == "OHLC") { 
      df_indicator <- df_input %>% 
        select(yahoo_open, yahoo_high, yahoo_low, yahoo_adjusted_close) %>% 
        f(...) %>% 
        as_tibble() 
    } else if (input == "HLC") { 
      df_indicator <- df_input %>% 
        select(yahoo_high, yahoo_low, yahoo_adjusted_close) %>% 
        f(...) %>% 
        as_tibble()  
    } else if (input == "HL") { 
      df_indicator <- df_input %>% 
        select(yahoo_high, yahoo_low) %>% 
        f(...) %>% 
        as_tibble()  
    } else if (input == "C") { 
      df_indicator <- df_input %>% 
        select(yahoo_adjusted_close) %>% 
        f(...) %>% 
        as_tibble()  
    } else if (input == "V") { 
      df_indicator <- df_input %>% 
        select(yahoo_volume) %>% 
        f(...) %>% 
        as_tibble()  
    } else if (input == "HLCV") { 
      df_indicator <- df_input %>% 
        select(yahoo_high, yahoo_low, yahoo_adjusted_close) %>% 
        f(volume = df_volume, ...) %>% 
        as_tibble()
    } else if (input == "HLV") { 
      df_indicator <- df_input %>% 
        select(yahoo_high, yahoo_low) %>% 
        f(volume = df_volume, ...) %>% 
        as_tibble()
    } else if (input == "CV") { 
      df_indicator <- df_input %>% 
        select(yahoo_adjusted_close) %>% 
        f(volume = df_volume, ...) %>% 
        as_tibble()
    }
    
    # Append created indicators 
    indicator <- bind_rows(indicator, df_indicator)
  } 
  
  # Return indicator for all symbols
  return(indicator)
} 

#' # 4. Add Functions 
#' Fix error in TTR::SNR function. 
SNR <- function (HLC, n, ...) {
  HLC <- try.xts(HLC, error = as.matrix)
  snr <- abs(HLC[, 3] - lag.xts(HLC[, 3], n))/ATR(HLC, n, ...)[ ,"atr"]
  return(reclass(snr, HLC))
}

#' # 5. ADX 
#' Adds DIp, DIn, DX, ADX. 
i_adx <- create_indicator(combined, "HLC", ADX, n = 14)
tail(i_adx)
combined <- bind_cols(combined, i_adx)

#' # 6. Aroon 
#' Adds aroonUp, aroonDn, oscillator. 
i_aroon <- create_indicator(combined, "HL", aroon, n = 20)
tail(i_aroon)
combined <- bind_cols(combined, i_aroon)

#' # 7. Average True Range (ATR) 
#' Adds tr, atr, trueHigh, trueLow. 
i_atr <- create_indicator(combined, "HLC", ATR, n = 14)
tail(i_atr)
combined <- bind_cols(combined, i_atr)

#' # 8. Bollinger Bands 
#' Adds dn, mavg, up, pctB. 
i_bbands <- create_indicator(combined, "HLC", BBands, n = 20,  sd = 2)
tail(i_bbands) 
combined <- bind_cols(combined, i_bbands)

#' # 9. Commodity Channel Index (CCI)  
#' Adds cci. 
i_cci <- create_indicator(combined, "HLC", CCI, n = 20) %>% 
  rename(cci = value)
tail(i_cci)
combined <- bind_cols(combined, i_cci)

#' # 10. Chaikin Accumulation / Distribution 
#' Adds chaikinad. 
i_chaikinad <- create_indicator(combined, "HLCV", chaikinAD) %>% 
  rename(chaikinad = value)
tail(i_chaikinad)
combined <- bind_cols(combined, i_chaikinad)

#' # 11. Chaikin Volatility 
#' Adds chaikinvol. 
i_chaikinvol <- create_indicator(combined, "HL", chaikinVolatility, n = 10) %>% 
  rename(chaikinvol = value)
tail(i_chaikinvol)
combined <- bind_cols(combined, i_chaikinvol)

#' # 12. Close Location Value 
#' Adds clv. 
i_clv <- create_indicator(combined, "HLC", CLV) %>% 
  rename(clv = value) 
tail(i_clv)
combined <- bind_cols(combined, i_clv)

#' # 13. Chaikin Money Flow 
#' Adds cmf. 
i_cmf <- create_indicator(combined, "HLCV", CMF, n = 20) %>% 
  rename(cmf = value) 
tail(i_cmf)
combined <- bind_cols(combined, i_cmf)

#' # 14. Chande Momentum Oscillator Close 
#' Adds cmo_close.
i_cmo_close <- create_indicator(combined, "C", CMO, n = 14 ) %>% 
  rename(cmo_close = value)
tail(i_cmo_close)
combined <- bind_cols(combined, i_cmo_close)

#' # 15. Chande Momentum Oscillator Volume 
#' Adds cmo_volume. 
i_cmo_volume <- create_indicator(combined, "V", CMO, n = 14) %>% 
  rename(cmo_volume = value)
tail(i_cmo_volume)
combined <- bind_cols(combined, i_cmo_volume)

#' # 16. Donchian Channel 
#' Adds dc_high, dc_mid, dc_low. 
i_donchian <- create_indicator(combined, "HL", DonchianChannel, n = 10, include.lag = FALSE) %>%
  rename(dc_high = high, dc_mid = mid, dc_low = low)
tail(i_donchian)
combined <- bind_cols(combined, i_donchian)

#' # 17. Detrended Price Oscillator Close 
#' Adds dpo_close, dpo_volume. 
i_dpo_close <- create_indicator(combined, "C", DPO, n = 10,  shift = 0, percent = TRUE) %>% 
  rename(dpo_close = value) 
tail(i_dpo_close) 
combined <- bind_cols(combined, i_dpo_close)

#' # 18. Detrended Price Oscillator Volume 
i_dpo_volume <- create_indicator(combined, "V", DPO, n = 10,  shift = 0, percent = TRUE) %>% 
  rename(dpo_volume = value) 
tail(i_dpo_volume) 
combined <- bind_cols(combined, i_dpo_volume)

#' # 19. DV Intermediate Oscillator 
#' Adds dvi_mag, dvi_str, dvi.  
# i_dvi <- create_indicator(combined, "C", DVI, n = 252, wts = c(0.8, 0.2), smooth = 3,
#                           magnitude = c(5, 100, 5), stretch = c(10, 100, 2), exact.multiplier = 1) %>%
#   rename(dvi_mag = dvi.mag, dvi_str = dvi.str)
# tail(i_dvi)
# combined <- bind_cols(combined, i_dvi)

#' # 20. Ease of Movement Value 
#' Adds emv, maEMV. 
# i_emv <- create_indicator(combined, "HLV", EMV, n = 9,  vol.divisor = 1)
# tail(i_emv)
# combined <- bind_cols(combined, i_dvi)

#' # 21. Guppy Multiple Moving Averages 
#' Adds short_lag_x, long_lag_x. 
i_gmma <- create_indicator(combined, "C", GMMA, short = c(3, 5, 8, 10, 12, 15), 
                           long = c(30, 35, 40, 45, 50, 60)) 
colnames(i_gmma) <- str_replace_all(colnames(i_gmma), " ", "_")
tail(i_gmma) 
combined <- bind_cols(combined, i_gmma)

#' # 22. Know Sure Thing 
#' Adds kst, kst_signal. 
i_kst <- create_indicator(combined, "C", KST, n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30), 
                          nSig = 9,  wts = 1:4) %>% 
  rename(kst_signal = signal)
tail(i_kst)
combined <- bind_cols(combined, i_kst)

#' # 23. MACD Close  
#' Adds macd_close, macd_signal_close.  
i_macd_close <- create_indicator(combined, "C", MACD, nFast = 12, nSlow = 26, nSig = 9, percent = TRUE) %>% 
  rename(macd_close = macd, macd_signal_close = signal) 
tail(i_macd_close)
combined <- bind_cols(combined, i_macd_close)

#' # 24. MACD Volume 
#' Adds macd_volume, macd_signal_volume. 
i_macd_volume <- create_indicator(combined, "V", MACD, nFast = 12, nSlow = 26, nSig = 9, percent = TRUE) %>% 
  rename(macd_volume = macd, macd_signal_volume = signal) 
tail(i_macd_volume)
combined <- bind_cols(combined, i_macd_volume)

#' # 25. Money Flow Index 
#' Adds mfi. 
i_mfi <- create_indicator(combined, "HLCV", MFI, n = 14) %>% 
  rename(mfi = value)
tail(i_mfi)
combined <- bind_cols(combined, i_mfi)

#' # 26. On Balance Volume 
#' Adds obv. 
i_obv <- create_indicator(combined, "CV", OBV) %>% 
  rename(obv = value)
tail(i_obv)
combined <- bind_cols(combined, i_obv)

#' # 27. Volatility Bands 
#' Adds pbands_dn, pbands_center, pbands_up.  
i_pbands <- create_indicator(combined, "C", PBands, n = 20,  
                             sd = 2, fastn = 2, centered = FALSE, lavg = FALSE) %>% 
  rename(pbands_dn = dn, pbands_center = center, pbands_up = up)
tail(i_pbands)
combined <- bind_cols(combined, i_pbands)

#' # 28. Relative Strength Index 
#' Adds rsi.  
i_rsi <- create_indicator(combined, "C", RSI, n = 14) %>% 
  rename(rsi = value)
tail(i_rsi)
combined <- bind_cols(combined, i_rsi)

#' # 29. Percent Rank Close 
#' Adds percent_rank_close. 
i_percent_rank_close <- create_indicator(combined, "C", runPercentRank, n = 252, cumulative = FALSE, exact.multiplier = 0.5) %>% 
  rename(percent_rank_close = value)
tail(i_percent_rank_close)
combined <- bind_cols(combined, i_percent_rank_close)

#' # 30. Percent Rank Volume 
#' Adds percent_rank_volume.  
i_percent_rank_volume <- create_indicator(combined, "V", runPercentRank, n = 252, cumulative = FALSE, exact.multiplier = 0.5) %>% 
  rename(percent_rank_volume = value)
tail(i_percent_rank_volume)
combined <- bind_cols(combined, i_percent_rank_volume)

#' # 31. Parabolic Stop and Reverse 
#' Adds sar.  
i_sar <- create_indicator(combined, "HL", SAR, accel = c(0.02, 0.2)) %>% 
  rename(sar = value)
tail(i_sar) 
combined <- bind_cols(combined, i_sar)

#' # 32. Signal to Noise Ratio 
#' Adds snr. 
i_snr <- create_indicator(combined, "HLC", SNR, n = 20) %>% 
  rename(snr = value)
tail(i_snr)
combined <- bind_cols(combined, i_snr)

#' # 33. Stochastic Oscillator 
#' Adds fastK, fastD, slowD. 
# i_stoch <- create_indicator(combined, "HLC", stoch, nFastK = 14, nFastD = 3, nSlowD = 3, bounded = TRUE, smooth = 1)
# tail(i_stoch)
# combined <- bind_cols(combined, i_stoch)

#' # 34. Stochastic Momentum Index 
#' Adds smi. SMI function in TTR produces an error. Debug later.  
# i_smi <- create_indicator(combined, "HLC", SMI, n = 13, nFast = 2, nSlow = 25, nSig = 9,  bounded = TRUE)

#' # 35. Trend Detection Index 
#' Adds tdi, di.  
i_tdi <- create_indicator(combined, "C", TDI, n = 20, multiple = 2)
tail(i_tdi)
combined <- bind_cols(combined, i_tdi)

#' # 36. Triple Smoothed Exponential Oscillator 
#' Adds trix, trix_signal. 
i_trix <- create_indicator(combined, "C", TRIX, n = 20, nSig = 9,  percent = TRUE) %>% 
  rename(trix = TRIX, trix_signal = signal)
tail(i_trix)
combined <- bind_cols(combined, i_trix)

#' # 37. Ultimate Oscillator 
#' Adds ultimate_oscillator. 
i_ultimate <- create_indicator(combined, "HLC", ultimateOscillator, n = c(7, 14, 28), wts = c(4, 2, 1)) %>% 
  rename(ultimate_oscillator = value)
tail(i_ultimate)
combined <- bind_cols(combined, i_ultimate)

#' # 38. Vertical Horizonal Filter
#' Adds vhf. 
i_vhf <- create_indicator(combined, "C", VHF, n = 28) %>% 
  rename(vhf = value)
tail(i_vhf)
combined <- bind_cols(combined, i_vhf)

#' # 39. Volatility Indicators 
#' Adds volatility_close, volatility_garman, volatility_parkinson, volatility_rogers, volatility_garman2, volatility_yang. 
i_volatility_close <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "close", N = 260, mean0 = FALSE) %>% 
  rename(volatility_close = value)
i_volatility_garman <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "garman.klass", N = 260, mean0 = FALSE) %>% 
  rename(volatility_garman = value)
i_volatility_parkinson <- create_indicator(combined, "OHLC", volatility, calc = "parkinson", N = 260, mean0 = FALSE) %>% 
  rename(volatility_parkinson = value)
i_volatility_rogers <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "rogers.satchell", N = 260, mean0 = FALSE) %>% 
  rename(volatility_rogers = value)
i_volatility_garman2 <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "gk.yz", N = 260, mean0 = FALSE) %>% 
  rename(volatility_garman2 = value)
i_volatility_yang <- create_indicator(combined, "OHLC", volatility, n = 30, calc = "yang.zhang", N = 260, mean0 = FALSE) %>% 
  rename(volatility_yang = value)
tail(i_volatility_close)
tail(i_volatility_garman)
tail(i_volatility_parkinson)
tail(i_volatility_rogers)
tail(i_volatility_garman2)
tail(i_volatility_yang)
combined <- bind_cols(combined, i_volatility_close)
combined <- bind_cols(combined, i_volatility_garman)
combined <- bind_cols(combined, i_volatility_parkinson)
combined <- bind_cols(combined, i_volatility_rogers)
combined <- bind_cols(combined, i_volatility_garman2)
combined <- bind_cols(combined, i_volatility_yang)

#' # 40. Williams Accumulation Distribution 
#' Adds williams_ad. 
i_williamsad <- create_indicator(combined, "HLC", williamsAD) %>% 
  rename(williams_ad = value)
tail(i_williamsad)
combined <- bind_cols(combined, i_williamsad)

#' # 41. Williams R 
#' Adds williams_r. 
i_williamsr <- create_indicator(combined, "HLC", WPR, n = 14) %>% 
  rename(williams_r = value)
tail(i_williamsr)
combined <- bind_cols(combined, i_williamsr)

#' # 42. Zig Zag 
i_zigzag <- create_indicator(combined, "HL", ZigZag, change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE) %>% 
  rename(zigzag = value)
tail(i_zigzag)
combined <- bind_cols(combined, i_zigzag)

#' # 43. Split into Train and Test 
train <- combined %>% 
  filter(is.na(position_label) == FALSE)
test <- combined %>% 
  filter(is.na(position_label) == TRUE)

#' # 44. Save Data 
write_csv(train, "./data/train.csv")
write_csv(test, "./data/test.csv")
