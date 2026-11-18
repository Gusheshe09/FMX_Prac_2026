# Backtest rebalance weights on monthly returns.
# lag_weights = FALSE: with monthly data, quarter-end weights apply from the next month's return.
Port_Return <- function(W, Rets_M, Label) {
  
  W_xts <- W %>% tbl_xts(cols_to_xts = Weight, spread_by = Name)
  W_xts[is.na(W_xts)] <- 0
  
  R_xts <- Rets_M %>% filter(Name %in% colnames(W_xts), date > min(W$date)) %>%
    tbl_xts(cols_to_xts = Return, spread_by = Name)
  R_xts[is.na(R_xts)] <- 0
  W_xts <- W_xts[, colnames(R_xts)]
  
  P <- rmsfuns::Safe_Return.portfolio(R = R_xts, weights = W_xts, lag_weights = FALSE,
                                      verbose = TRUE, geometric = TRUE)
  
  list(Ret = P$returns %>% xts_tbl() %>% setNames(c("date", "Return")) %>%
         mutate(date = as.Date(date), Name = Label),
       BOP = P$BOP.Weight)
  
}