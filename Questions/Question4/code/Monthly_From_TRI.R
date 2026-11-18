# Month-end returns from a daily total return index (TRI levels)
Monthly_From_TRI <- function(df) {
  
  df %>%
    mutate(YM = format(date, "%Y%m")) %>%
    group_by(Name, YM) %>% filter(date == max(date)) %>% ungroup() %>%
    group_by(Name) %>% arrange(date) %>%
    mutate(Return = TRI / lag(TRI) - 1) %>% ungroup() %>%
    filter(!is.na(Return)) %>%
    select(date, Name, Return)
  
}