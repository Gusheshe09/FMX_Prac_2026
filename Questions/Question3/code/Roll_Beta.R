# Rolling CAPM statistics of each fund against the benchmark (monthly returns)
# beta = cov(R_f, R_b) / var(R_b); R2 = cor^2 (share of return variation explained by the market);
# alpha = annualised intercept. Computed from rolling means, so each fund needs Window months of data.
Roll_Beta <- function(Funds, Bench, Window = 36) {
  
  Funds %>%
    inner_join(Bench %>% select(date, BM = Return), by = "date") %>%
    group_by(Name) %>% arrange(date) %>%
    filter(n() >= Window) %>%
    mutate(mx  = RcppRoll::roll_mean(BM, Window, fill = NA, align = "right"),
           my  = RcppRoll::roll_mean(Return, Window, fill = NA, align = "right"),
           mxy = RcppRoll::roll_mean(BM * Return, Window, fill = NA, align = "right"),
           mxx = RcppRoll::roll_mean(BM^2, Window, fill = NA, align = "right"),
           myy = RcppRoll::roll_mean(Return^2, Window, fill = NA, align = "right")) %>%
    ungroup() %>%
    filter(!is.na(mx)) %>%
    mutate(Beta  = (mxy - mx * my) / (mxx - mx^2),
           R2    = (mxy - mx * my)^2 / ((mxx - mx^2) * (myy - my^2)),
           Alpha = (my - Beta * mx) * 12) %>%
    select(date, Name, Beta, R2, Alpha)
  
}