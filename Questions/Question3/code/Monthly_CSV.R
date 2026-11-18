# Monthly market dispersion: weighted cross-sectional volatility of Capped SWIX constituents,
# using previous month-end weights and monthly (compounded daily) stock returns
Monthly_CSV <- function(Size_Idx, Stock_Rets, Fund = "CappedSWIX") {
  
  Stock_M <-
    Stock_Rets %>% filter(!is.na(Return)) %>%
    mutate(YM = format(date, "%Y%m")) %>%
    group_by(Tickers, YM) %>%
    summarise(date = max(date), Ret = prod(1 + Return) - 1, .groups = "drop")
  
  W <-
    Size_Idx %>% filter(Fund == !!Fund) %>%
    mutate(YM = format(date %m+% months(1), "%Y%m")) %>%      # weights at end of t-1 apply in month t
    select(YM, Tickers, Weight)
  
  Stock_M %>%
    inner_join(W, by = c("YM", "Tickers")) %>%
    group_by(YM) %>%
    mutate(w = Weight / sum(Weight)) %>%
    summarise(date = max(date), CSV = sqrt(sum(w * (Ret - sum(w * Ret))^2)), .groups = "drop")
  
}