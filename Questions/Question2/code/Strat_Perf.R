# Stratification: annualised return and volatility of each index in the selected months,
# against its full-sample values (Prac 2 Bonus idea, extended to returns)
# Idxs: tidy (date, YearMonth, Name, Return) of winsorised daily returns
Strat_Perf <- function(Idxs, YMs, Alias, Scale = 252) {
  
  Full <- Idxs %>% group_by(Name) %>%
    summarise(Full_Ret = mean(Return) * Scale, Full_SD = sd(Return) * sqrt(Scale), .groups = "drop")
  
  Idxs %>% filter(YearMonth %in% YMs) %>%
    group_by(Name) %>%
    summarise(Ret = mean(Return) * Scale, SD = sd(Return) * sqrt(Scale), .groups = "drop") %>%
    left_join(Full, by = "Name") %>%
    mutate(SD_Ratio = SD / Full_SD, Period = Alias)
  
}