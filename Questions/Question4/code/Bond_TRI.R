# Monthly 10-year bond returns from yields: carry minus duration x yield change
Bond_TRI <- function(Yields) {
  
  Yields %>%
    mutate(YM = format(date, "%Y%m")) %>%
    group_by(Name, YM) %>% filter(date == max(date)) %>% ungroup() %>%
    group_by(Name) %>% arrange(date) %>%
    mutate(y = TRI / 100,
           D = ifelse(abs(y) < 1e-4, 10, (1 - (1 + y)^-10) / y),
           Return = lag(y) / 12 - lag(D) * (y - lag(y))) %>%
    ungroup() %>% filter(!is.na(Return)) %>%
    select(date, Name, Return)
  
}