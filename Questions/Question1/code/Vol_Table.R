# Annualised realised volatility per calendar year, one column per series
# df: tidy frame with columns date, Name, Return (daily simple returns)
Vol_Table <- function(df, Scale = 252) {
  
  df %>%
    mutate(Year = format(date, "%Y")) %>%
    group_by(Year, Name) %>%
    summarise(Vol = sd(Return, na.rm = TRUE) * sqrt(Scale), .groups = "drop") %>%
    spread(Name, Vol)
  
}