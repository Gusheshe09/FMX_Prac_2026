# Share of portfolio variance from each asset at one rebalance date: RC_i = w_i (Sigma w)_i / (w' Sigma w)
Risk_Contrib <- function(W, Rets_M, Info, RebDate, Label, WindowStart = ymd(20100101)) {
  
  w <- W %>% filter(date == RebDate, Weight > 0)
  R <- Rets_M %>% filter(Name %in% w$Name, date >= WindowStart, date <= RebDate) %>%
    spread(Name, Return) %>% select(all_of(w$Name))
  Sigma <- as.matrix(Matrix::nearPD(cov(R, use = "pairwise.complete.obs"))$mat)
  MRC <- as.numeric(Sigma %*% w$Weight)
  
  tibble(Port = Label, Name = w$Name, Weight = w$Weight,
         Risk_Share = w$Weight * MRC / sum(w$Weight * MRC)) %>%
    left_join(Info, by = "Name")
  
}