# Average pairwise correlation of the N largest constituents over a trailing window
Avg_Pairwise_Cor <- function(RebDate, Constituents, N = 40, LookBack = 63) {
  
  Top <-
    Constituents %>% filter(date == RebDate) %>%
    arrange(desc(weight)) %>% head(N) %>% pull(Tickers)
  
  R <-
    Constituents %>%
    filter(Tickers %in% Top, date <= RebDate, date > RebDate - 2 * LookBack) %>%
    select(date, Tickers, Return) %>%
    spread(Tickers, Return) %>%
    tail(LookBack) %>%
    select(-date)
  
  C <- cor(R, use = "pairwise.complete.obs")
  tibble(date = RebDate, Avg_Cor = mean(C[upper.tri(C)], na.rm = TRUE))
  
}