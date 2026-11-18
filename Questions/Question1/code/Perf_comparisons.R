# Stratification: volatility of each series during selected months relative to its full-period volatility
# Idxs: tidy frame with date, YearMonth, Name, Return (winsorised daily returns)
# YMs: YearMonths in the regime; Alias: regime label
Perf_comparisons <- function(Idxs, YMs, Alias, Scale = 252) {
  
  Unconditional_SD <-
    Idxs %>%
    group_by(Name) %>%
    summarise(Full_SD = sd(Return, na.rm = TRUE) * sqrt(Scale), .groups = "drop")
  
  Idxs %>%
    filter(YearMonth %in% YMs) %>%
    group_by(Name) %>%
    summarise(SD = sd(Return, na.rm = TRUE) * sqrt(Scale), .groups = "drop") %>%
    left_join(Unconditional_SD, by = "Name") %>%
    mutate(Ratio = SD / Full_SD, Period = Alias)
  
}