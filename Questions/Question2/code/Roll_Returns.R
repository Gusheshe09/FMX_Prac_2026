# Rolling annualised returns from daily returns (Prac 1: RcppRoll::roll_prod)
Roll_Returns <- function(df, Years = 3, Scale = 252) {
  
  df %>%
    group_by(Name) %>% arrange(date) %>%
    mutate(RollRet = RcppRoll::roll_prod(1 + Return, n = Years * Scale, fill = NA, align = "right")^(1 / Years) - 1) %>%
    ungroup() %>%
    filter(!is.na(RollRet))
  
}

