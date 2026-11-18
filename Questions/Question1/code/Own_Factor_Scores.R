# Own Momentum and Low-Volatility scores from daily price behaviour
# Momentum: 6-month return skipping the most recent month (t-125 to t-21 trading days)
# Low-Vol : minus the 6-month (126-day) annualised realised volatility (higher = calmer stock)
# Both standardised cross-sectionally each month and capped at +-3
Own_Factor_Scores <- function(Rets, MinObs = 126) {
  
  Raw <-
    Rets %>%
    arrange(Tickers, date) %>%
    group_by(Tickers) %>%
    filter(n() > MinObs) %>%
    mutate(Mom_Raw = lag(RcppRoll::roll_prod(1 + Return, n = 105, fill = NA, align = "right"), 21) - 1,
           Vol_Raw = RcppRoll::roll_sd(Return, n = 126, fill = NA, align = "right") * sqrt(252)) %>%
    ungroup() %>%
    mutate(YM = format(date, "%Y%m")) %>%
    group_by(YM) %>% filter(date == max(date)) %>% ungroup()     # last trading day of each month
  
  Zscore <- function(x) { z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE); pmin(pmax(z, -3), 3) }
  
  Raw %>%
    filter(!is.na(Mom_Raw), !is.na(Vol_Raw)) %>%
    group_by(YM) %>%
    mutate(Own_Momentum = Zscore(Mom_Raw), Own_LowVol = Zscore(-Vol_Raw)) %>%
    ungroup() %>%
    select(YM, Tickers, Own_Momentum, Own_LowVol)
  
}