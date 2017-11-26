
#' # 4. Plot #' ---
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
source("EMS.001 Load Packages.R")

#' # 2. Load Data 
train <- read_csv("./Output/train.csv", col_types = c("Ddddddcii"))

#' # 3. Unique Symbols  
symbols <- train[["symbol"]] %>% unique()
print(symbols) 

#' # 4. Add Functions 
SNR <- function(HLC, n, ...) {
  HLC <- try.xts(HLC, error = as.matrix)
  snr <- abs(HLC[, 3] - lag.xts(HLC[, 3], n)) / ATR(HLC, n, ...)[ ,"atr"]
  return(reclass(snr, HLC))
}

#' # 4. Technical Indicator Function 
create_indicator <- function(train, input, f, option, ...) { 
  
  # Initialize new train tibble 
  indicator <- tibble() 
  
  # Loop through symbols and add the technical indicator 
  for (i in symbols) { 
    
    # If input uses open, high, low, close 
    if (input == "OHLC") { 
      df_input <- train %>% 
        filter(symbol == i) %>% 
        select(open, high, low, close) 
    }
    
    # If input uses high, low, close 
    if (input == "HLC") { 
      df_input <- train %>% 
        filter(symbol == i) %>% 
        select(high, low, close) 
    }
    
    # If input uses high, low 
    if (input == "HL") { 
      df_input <- train %>% 
        filter(symbol == i) %>% 
        select(high, low) 
    }
    
    # If input uses close  
    if (input == "C") { 
      df_input <- train %>% 
        filter(symbol == i) %>% 
        select(close) 
    }
    
    # If input uses volume 
    if (input == "V") { 
      df_input <- train %>% 
        filter(symbol == i) %>% 
        select(volume) 
    }
    
    # Custom options for functions that require a volume argument 
    if (option == "volume") { 
      df_volume <- train %>% 
        filter(symbol == i) %>% 
        select(volume) %>% 
        as.vector()
      df_indicator <- df_input %>% 
        f(volume = df_volume, ...) %>% 
        as_tibble() 
    }
    
    # Default options for functions that follow standard format 
    if (option == "default") { 
      df_indicator <- df_input %>% 
        f(...) %>% 
        as_tibble() 
    }

    # Append created indicators 
    indicator <- bind_rows(indicator, df_indicator)
  } 
  
  # Return indicator 
  return(indicator)
} 

#' # 5. Directional Movement Index (ADX)  
#' Adds DIp, DIn, DX, ADX. 
i_adx <- create_indicator(train = train, 
                          input = "HLC",
                          f = ADX, 
                          option = "default", 
                          n = 14) 
tail(i_adx)
train <- bind_cols(train, i_adx)


#' # 6. Aroon Indicator 
#' Adds aroonUp, aroonDn, oscillator. 
i_aroon <- create_indicator(train = train, 
                            input = "HL", 
                            f = aroon, 
                            option = "default", 
                            n = 20)
tail(i_aroon)
train <- bind_cols(train, i_aroon)


#' # 7. Average True Range (ATR) 
#' Adds tr, atr, trueHigh, trueLow. 
i_atr <- create_indicator(train = train, 
                          input = "HLC", 
                          f = ATR, 
                          option = "default", 
                          n = 14)
tail(i_atr)
train <- bind_cols(train, i_atr)


#' # 8. Bollinger Bands 
#' Adds dn, mavg, up, pctB. 
i_bbands <- create_indicator(train = train, 
                             input = "HLC", 
                             f = BBands, 
                             option = "default", 
                             n = 20, 
                             sd = 2)
tail(i_bbands) 
train <- bind_cols(train, i_bbands)


#' # 9. Commodity Channel Index (CCI)  
#' Adds cci. 
i_cci <- create_indicator(train = train, 
                          input = "HLC", 
                          f = CCI, 
                          option = "default", 
                          n = 20, 
                          c = 0.015) %>% 
  rename(cci = value)
tail(i_cci)
train <- bind_cols(train, i_cci)


#' # 10. Chaikin Accumulation / Distribution 
#' Adds chaikinad. 
i_chaikinad <- create_indicator(train = train, 
                                input = "HLC", 
                                f = chaikinAD, 
                                option = "volume") %>% 
  rename(chaikinad = value)
tail(i_chaikinad)
train <- bind_cols(train, i_chaikinad)


#' # 11. Chaikin Volatility 
#' Adds chaikinvol. 
i_chaikinvol <- create_indicator(train = train, 
                                 input = "HL", 
                                 f = chaikinVolatility, 
                                 option = "default", 
                                 n = 10) %>% 
  rename(chaikinvol = value)
tail(i_chaikinvol)
train <- bind_cols(train, i_chaikinvol) 


#' # 12. Close Location Value 
#' Adds clv. 
i_clv <- create_indicator(train = train, 
                          input = "HLC", 
                          f = CLV, 
                          option = "default") %>% 
  rename(clv = value) 
tail(i_clv)
train <- bind_cols(train, i_clv)


#' # 13. Chaikin Money Flow 
#' Adds cmf. 
i_cmf <- create_indicator(train = train, 
                          input = "HLC", 
                          f = CMF, 
                          option = "volume", 
                          n = 20) %>% 
  rename(cmf = value) 
tail(i_cmf)
train <- bind_cols(train, i_cmf) 


#' # 14. Chande Momentum Oscillator 
#' Adds cmo_close, cmo_volume 
i_cmo_close <- create_indicator(train = train, 
                                input = "C", 
                                f = CMO, 
                                option = "default", 
                                n = 14) %>% 
  rename(cmo_close = value)
i_cmo_volume <- create_indicator(train = train, 
                                 input = "V", 
                                 f = CMO, 
                                 option = "default", 
                                 n = 14) %>% 
  rename(cmo_volume = value) 
tail(i_cmo_close)
tail(i_cmo_volume)
train <- bind_cols(train, i_cmo_close)
train <- bind_cols(train, i_cmo_volume)


#' # 15. Donchian Channel 
#' Adds dc_high, dc_mid, dc_low. Needs to be normalized. 
i_donchianchannel <- create_indicator(train = train, 
                                      input = "HL", 
                                      f = DonchianChannel, 
                                      option = "default", 
                                      n = 10, 
                                      include.lag = FALSE) %>% 
  rename(dc_high = high, 
         dc_mid = mid, 
         dc_low = low)
tail(i_donchianchannel)
train <- bind_cols(train, i_donchianchannel)


#' # 16. Detrended Price Oscillator 
#' Adds dpo_close, dpo_volume. 
i_dpo_close <- create_indicator(train = train, 
                                input = "C", 
                                f = DPO, 
                                option = "default", 
                                n = 10, 
                                shift = 10/2 + 1, 
                                percent = FALSE) %>% 
  rename(dpo_close = value) 
i_dpo_volume <- create_indicator(train = train, 
                                 input = "V", 
                                 f = DPO, 
                                 option = "default", 
                                 n = 10, 
                                 shift = 10/2 + 1, 
                                 percent = FALSE) %>% 
  rename(dpo_volume = value)
tail(i_dpo_close) 
tail(i_dpo_volume)
train <- bind_cols(train, i_dpo_close) 
train <- bind_cols(train, i_dpo_volume) 


#' # 17. DV Intermediate Oscillator 
#' Adds dvi_mag, dvi_str, dvi.  
i_dvi <- create_indicator(train = train, 
                          input = "C", 
                          f = DVI, 
                          option = "default", 
                          n = 252, 
                          wts = c(0.8, 0.2), 
                          smooth = 3, 
                          magnitude = c(5, 100, 5), 
                          stretch = c(10, 100, 2), 
                          exact.multiplier = 1) %>% 
  rename(dvi_mag = dvi.mag, 
         dvi_str = dvi.str)
tail(i_dvi)
train <- bind_cols(train, i_dvi)


#' # 18. Ease of Movement Value 
#' Adds emv, maEMV. 
i_emv <- create_indicator(train = train, 
                          input = "HL", 
                          f = EMV, 
                          option = "volume", 
                          n = 9, 
                          vol.divisor = 10000) 
tail(i_emv)
train <- bind_cols(train, i_emv)


#' # 19. Guppy Multiple Moving Averages 
#' Adds short_lag_x, long_lag_x. 
i_gmma <- create_indicator(train = train, 
                           input = "C", 
                           f = GMMA, 
                           option = "default", 
                           short = c(3, 5, 8, 10, 12, 15), 
                           long = c(30, 35, 40, 45, 50, 60)) 
colnames(i_gmma) <- str_replace_all(colnames(i_gmma), " ", "_")
tail(i_gmma) 
train <- bind_cols(train, i_gmma)


#' # 20. Know Sure Thing 
#' Adds kst, kst_signal. 
i_kst <- create_indicator(train = train, 
                          input = "C", 
                          f = KST, 
                          option = "default", 
                          n = c(10, 10, 10, 15), 
                          nROC = c(10, 15, 20, 30), 
                          nSig = 9, 
                          wts = 1:4) %>% 
  rename(kst_signal = signal)
tail(i_kst)
train <- bind_cols(train, i_kst)


#' # 21. MACD 
#' Adds macd_close, macd_signal_close, macd_volume, macd_signal_volume.  
i_macd_close <- create_indicator(train = train, 
                                 input = "C", 
                                 f = MACD, 
                                 option = "default", 
                                 nFast = 12, 
                                 nSlow = 26, 
                                 nSig = 9, 
                                 percent = TRUE) %>% 
  rename(macd_close = macd, 
         macd_signal_close = signal) 
i_macd_volume <- create_indicator(train = train, 
                                 input = "V", 
                                 f = MACD, 
                                 option = "default", 
                                 nFast = 12, 
                                 nSlow = 26, 
                                 nSig = 9, 
                                 percent = TRUE) %>% 
  rename(macd_volume = macd, 
         macd_signal_volume = signal) 
tail(i_macd_close)
tail(i_macd_volume)
train <- bind_cols(train, i_macd_close) 
train <- bind_cols(train, i_macd_volume) 


#' # 22. Money Flow Index 
#' Adds mfi. 
i_mfi <- create_indicator(train = train, 
                          input = "HLC", 
                          f = MFI, 
                          option = "volume", 
                          n = 14) %>% 
  rename(mfi = value)
tail(i_mfi)
train <- bind_cols(train, i_mfi)


#' # 23. On Balance Volume 
#' Adds obv. 
i_obv <- create_indicator(train = train, 
                          input = "C", 
                          f = OBV, 
                          option = "volume") %>% 
  rename(obv = value)
tail(i_obv)
train <- bind_cols(train, i_obv)


#' # 24. Volatility Bands 
#' Adds pbands_dn, pbands_center, pbands_up.  
i_pbands <- create_indicator(train = train, 
                             input = "C", 
                             f = PBands, 
                             option = "default", 
                             n = 20, 
                             maType = "SMA", 
                             sd = 2, 
                             fastn = 2, 
                             centered = FALSE, 
                             lavg = FALSE) %>% 
  rename(pbands_dn = dn, 
         pbands_center = center, 
         pbands_up = up)
tail(i_pbands)
train <- bind_cols(train, i_pbands)


#' # 25. Relative Strength Index 
#' Adds rsi.  
i_rsi <- create_indicator(train = train, 
                          input = "C", 
                          f = RSI, 
                          option = "default", 
                          n = 14) %>% 
  rename(rsi = value)
tail(i_rsi)
train <- bind_cols(train, i_rsi)


#' # 26. Percent Rank 
#' Adds percent_rank_close, percent_rank_volume.  
i_percent_rank_close <- create_indicator(train = train, 
                                         input = "C", 
                                         f = runPercentRank, 
                                         option = "default", 
                                         n = 260, 
                                         cumulative = FALSE, 
                                         exact.multiplier = 0.5) %>% 
  rename(percent_rank_close = value)
i_percent_rank_volume <- create_indicator(train = train, 
                                          input = "V", 
                                          f = runPercentRank,
                                          option = "default", 
                                          n = 260, 
                                          cumulative = FALSE, 
                                          exact.multiplier = 0.5) %>% 
  rename(percent_rank_volume = value)
tail(i_percent_rank_close)
tail(i_percent_rank_volume)
train <- bind_cols(train, i_percent_rank_close)
train <- bind_cols(train, i_percent_rank_volume)


#' # 27. Parabolic Stop and Reverse 
#' Adds sar.  
i_sar <- create_indicator(train = train, 
                          input = "HL", 
                          f = SAR, 
                          option = "default", 
                          accel = c(0.02, 0.2)) %>% 
  rename(sar = value)
tail(i_sar) 
train <- bind_cols(train, i_sar)


#' # 28. Signal to Noise Ratio 
#' Adds snr. 
i_snr <- create_indicator(train = train, 
                          input = "HLC", 
                          f = SNR, 
                          option = "default", 
                          n = 20) %>% 
  rename(snr = value)
tail(i_snr)
train <- bind_cols(train, i_snr)


#' # 29. Stochastic Oscillator 
#' Adds fastK, fastD, slowD. 
i_stoch <- create_indicator(train = train, 
                            input = "HLC", 
                            f = stoch, 
                            option = "default", 
                            nFastK = 14, 
                            nFastD = 3, 
                            nSlowD = 3, 
                            bounded = TRUE, 
                            smooth = 1)
tail(i_stoch)
train <- bind_cols(train, i_stoch)


#' # 30. Stochastic Momentum Index 
#' Adds smi. SMI function in TTR produces an error. Debug later.  


#' # 31. Trend Detection Index 
#' Adds tdi, di.  
i_tdi <- create_indicator(train = train, 
                          input = "C", 
                          f = TDI, 
                          option = "default", 
                          n = 20, 
                          multiple = 2)
tail(i_tdi)
train <- bind_cols(train, i_tdi)


#' # 32. Triple Smoothed Exponential Oscillator 
#' Adds trix, trix_signal. 
i_trix <- create_indicator(train = train, 
                           input = "C", 
                           f = TRIX, 
                           option = "default", 
                           n = 20, 
                           nSig = 9, 
                           percent = TRUE) %>% 
  rename(trix = TRIX, 
         trix_signal = signal)
tail(i_trix)
train <- bind_cols(train, i_trix)


#' # 33. Ultimate Oscillator 
#' Adds ultimate_oscillator. 
i_ultimate <- create_indicator(train = train, 
                               input = "HLC", 
                               f = ultimateOscillator, 
                               option = "default", 
                               n = c(7, 14, 28), 
                               wts = c(4, 2, 1)) %>% 
  rename(ultimate_oscillator = value)
tail(i_ultimate)
train <- bind_cols(train, i_ultimate)


#' # 34. Vertical Horizonal Filter
#' Adds vhf. 
i_vhf <- create_indicator(train = train, 
                          input = "C", 
                          f = VHF, 
                          option = "default", 
                          n = 28) %>% 
  rename(vhf = value)
tail(i_vhf)
train <- bind_cols(train, i_vhf)


#' # 35. Volatility Indicators 
#' Adds volatility_close, volatility_garman, volatility_parkinson, volatility_rogers, volatility_garman2, volatility_yang. 
i_volatility_close <- create_indicator(train = train, 
                                       input = "OHLC", 
                                       f = volatility, 
                                       option = "default", 
                                       n = 30, 
                                       calc = "close", 
                                       N = 260, 
                                       mean0 = FALSE) %>% 
  rename(volatility_close = value)
i_volatility_garman <- create_indicator(train = train, 
                                        input = "OHLC", 
                                        f = volatility, 
                                        option = "default", 
                                        n = 30, 
                                        calc = "garman.klass", 
                                        N = 260, 
                                        mean0 = FALSE) %>% 
  rename(volatility_garman = value)
i_volatility_parkinson <- create_indicator(train = train, 
                                           input = "OHLC", 
                                           f = volatility, 
                                           option = "default", 
                                           n = 30, 
                                           calc = "parkinson", 
                                           N = 260, 
                                           mean0 = FALSE) %>% 
  rename(volatility_parkinson = value)
i_volatility_rogers <- create_indicator(train = train, 
                                        input = "OHLC", 
                                        f = volatility, 
                                        option = "default", 
                                        n = 30, 
                                        calc = "rogers.satchell", 
                                        N = 260, 
                                        mean0 = FALSE) %>% 
  rename(volatility_rogers = value)
i_volatility_garman2 <- create_indicator(train = train, 
                                         input = "OHLC", 
                                         f = volatility, 
                                         option = "default", 
                                         n = 30, 
                                         calc = "gk.yz", 
                                         N = 260, 
                                         mean0 = FALSE) %>% 
  rename(volatility_garman2 = value)
i_volatility_yang <- create_indicator(train = train, 
                                      input = "OHLC", 
                                      f = volatility, 
                                      option = "default", 
                                      n = 30, 
                                      calc = "yang.zhang", 
                                      N = 260, 
                                      mean0 = FALSE) %>% 
  rename(volatility_yang = value)
tail(i_volatility_close)
tail(i_volatility_garman)
tail(i_volatility_parkinson)
tail(i_volatility_rogers)
tail(i_volatility_garman2)
tail(i_volatility_yang)
train <- bind_cols(train, i_volatility_close, i_volatility_garman, i_volatility_parkinson, i_volatility_rogers, 
                   i_volatility_garman2, i_volatility_yang)


#' # 36. Williams Accumulation Distribution 
#' Adds williams_ad. 
i_williamsad <- create_indicator(train = train, 
                                 input = "HLC", 
                                 f = williamsAD, 
                                 option = "default") %>% 
  rename(williams_ad = value)
tail(i_williamsad)
train <- bind_cols(train, i_williamsad)


#' # 37. Williams R 
#' Adds williams_r. 
i_williamsr <- create_indicator(train = train, 
                                input = "HLC", 
                                f = WPR, 
                                option = "default", 
                                n = 14) %>% 
  rename(williams_r = value)
tail(i_williamsr)
train <- bind_cols(train, i_williamsr)


#' # 37. Zig Zag 
i_zigzag <- create_indicator(train = train, 
                             input = "HL", 
                             f = ZigZag, 
                             option = "default", 
                             change = 10, 
                             percent = TRUE, 
                             retrace = FALSE, 
                             lastExtreme = TRUE) %>% 
  rename(zigzag = value)
tail(i_zigzag)
train <- bind_cols(train, i_zigzag)

#' #' 38. Extract Train  
train <- train %>%
  filter(timestamp >= "2015-11-02", 
         timestamp <= "2017-11-02")