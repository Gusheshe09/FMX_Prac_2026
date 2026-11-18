# Realised volatility of constituents vs the index itself at a date (trailing year):
# weighted-average stock volatility, median stock volatility, index volatility,
# and the diversification ratio = weighted-average stock vol / index vol
Constituent_Vol <- function(RebDate, Size_Idx, Stock_Rets, Index_Rets, LookBack = 252) {
  
  W <- Size_Idx %>% filter(date == RebDate) %>% group_by(Fund) %>% mutate(w = Weight / sum(Weight)) %>% ungroup()
  
  Stock_Vol <-
    Stock_Rets %>%
    filter(Tickers %in% W$Tickers, date <= RebDate, date > RebDate - 2 * LookBack) %>%
    group_by(Tickers) %>% filter(row_number() > n() - LookBack) %>%
    summarise(Vol = sd(Return, na.rm = TRUE) * sqrt(252), N = sum(!is.na(Return)), .groups = "drop") %>%
    filter(N > LookBack / 2)
  
  Idx_Vol <-
    Index_Rets %>% filter(date <= RebDate) %>%
    group_by(Name) %>% filter(row_number() > n() - LookBack) %>%
    summarise(Index_Vol = sd(Return) * sqrt(252), .groups = "drop")
  
  W %>% inner_join(Stock_Vol, by = "Tickers") %>%
    group_by(Fund) %>%
    summarise(Wtd_Stock_Vol = sum(w * Vol) / sum(w), Median_Stock_Vol = median(Vol), .groups = "drop") %>%
    left_join(Idx_Vol, by = c("Fund" = "Name")) %>%
    mutate(Div_Ratio = Wtd_Stock_Vol / Index_Vol, date = RebDate)
  
}