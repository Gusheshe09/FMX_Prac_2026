# Component contribution to portfolio SD (Prac 2) at one rebalance date
# Wts: tidy weights (date, Tickers, Sector, Weight); Rets: tidy daily returns (date, Tickers, Return)
Risk_Contribution <- function(RebDate, Wts, Rets, LookBack = 252) {
  
  w <- Wts %>% filter(date == RebDate) %>% arrange(Tickers)
  
  R <-
    Rets %>%
    filter(Tickers %in% w$Tickers, date <= RebDate) %>%
    group_by(Tickers) %>% filter(row_number() > n() - LookBack) %>% ungroup() %>%
    tbl_xts(cols_to_xts = Return, spread_by = Tickers)
  
  R[is.na(R)] <- 0
  w <- w %>% filter(Tickers %in% colnames(R))
  R <- R[, w$Tickers]
  
  Contrib <- StdDev(R, weights = w$Weight / sum(w$Weight), portfolio_method = "component")
  
  tibble(date = RebDate, Tickers = w$Tickers, Sector = w$Sector,
         Weight = w$Weight / sum(w$Weight),
         Risk_Share = as.numeric(Contrib$pct_contrib_StdDev),
         Port_SD = as.numeric(Contrib$StdDev) * sqrt(252))
  
}