# Stratification of a monthly alpha-opportunity measure by macro regime:
# average of each measure in the top and bottom quintile months of a driver, relative to its full-sample mean
# Measures: tidy (YearMonth, Measure, Value); Driver: (YearMonth, Level)
Regime_Means <- function(Measures, Driver, Alias) {
  
  Hi <- Driver %>% filter(Level > quantile(Level, 0.8, na.rm = TRUE)) %>% pull(YearMonth)
  Lo <- Driver %>% filter(Level < quantile(Level, 0.2, na.rm = TRUE)) %>% pull(YearMonth)
  
  Measures %>%
    group_by(Measure) %>%
    summarise(High = mean(Value[YearMonth %in% Hi], na.rm = TRUE) / mean(Value, na.rm = TRUE),
              Low  = mean(Value[YearMonth %in% Lo], na.rm = TRUE) / mean(Value, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(Driver = Alias)
  
}