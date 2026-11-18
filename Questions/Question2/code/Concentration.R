# Stock concentration per index and month:
# number of stocks, weight of the 10 largest, Herfindahl index and effective number of stocks (1 / HHI)
Concentration <- function(Size_Idx) {
  
  Size_Idx %>%
    group_by(Fund, date) %>%
    mutate(w = Weight / sum(Weight)) %>%
    summarise(N_Stocks = n(),
              Top10 = sum(sort(w, decreasing = TRUE)[1:min(10, n())]),
              HHI = sum(w^2),
              Eff_N = 1 / HHI, .groups = "drop")
  
}