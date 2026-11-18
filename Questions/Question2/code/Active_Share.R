# Active share of each index relative to a benchmark index, per month:
# AS = 0.5 * sum_i | w_i(index) - w_i(benchmark) |, stocks missing from one side get weight 0
Active_Share <- function(Size_Idx, Bench = "Capped SWIX") {
  
  W <- Size_Idx %>% group_by(Fund, date) %>% mutate(w = Weight / sum(Weight)) %>% ungroup()
  B <- W %>% filter(Fund == Bench) %>% select(date, Tickers, w_B = w)
  
  W %>%
    filter(Fund != Bench) %>%
    select(Fund, date, Tickers, w) %>%
    group_by(Fund) %>%
    group_modify(~ full_join(.x, B %>% filter(date %in% .x$date), by = c("date", "Tickers"))) %>%
    ungroup() %>%
    mutate(w = coalesce(w, 0), w_B = coalesce(w_B, 0)) %>%
    group_by(Fund, date) %>%
    summarise(Active_Share = 0.5 * sum(abs(w - w_B)), .groups = "drop")
  
}

