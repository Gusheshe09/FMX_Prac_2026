# Daily returns of each size index rebuilt from monthly weights and daily stock returns
# (Safe_Return.portfolio: month-end weights applied from the next trading day)
Size_Index_Returns <- function(Size_Idx, Stock_Rets, StartDate) {
  
  Funds <- unique(Size_Idx$Fund)
  Out <- list()
  
  for (f in Funds) {
    
    W_xts <-
      Size_Idx %>% filter(Fund == f, date >= StartDate) %>%
      select(date, Tickers, Weight) %>%
      tbl_xts(cols_to_xts = Weight, spread_by = Tickers)
    W_xts[is.na(W_xts)] <- 0
    
    R_xts <-
      Stock_Rets %>% filter(Tickers %in% colnames(W_xts), date > StartDate) %>%
      tbl_xts(cols_to_xts = Return, spread_by = Tickers)
    R_xts[is.na(R_xts)] <- 0
    
    W_xts <- W_xts[, colnames(R_xts)]                       # stocks without return data dropped
    W_xts <- W_xts / rowSums(W_xts)                          # re-normalise to 100%
    
    Port <- rmsfuns::Safe_Return.portfolio(R = R_xts, weights = W_xts, lag_weights = TRUE,
                                           verbose = TRUE, value = 1, geometric = TRUE)
    
    Out[[f]] <- Port$returns %>% xts_tbl() %>% setNames(c("date", "Return")) %>%
      mutate(date = as.Date(date), Name = f)
  }
  
  bind_rows(Out)
  
}