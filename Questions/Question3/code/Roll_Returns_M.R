# Rolling annualised returns from monthly returns
Roll_Returns_M <- function(df, Months = 12) {
  
  df %>%
    group_by(Name) %>% arrange(date) %>%
    mutate(RollRet = RcppRoll::roll_prod(1 + Return, n = Months, fill = NA, align = "right")^(12 / Months) - 1) %>%
    ungroup() %>%
    filter(!is.na(RollRet))
  
}