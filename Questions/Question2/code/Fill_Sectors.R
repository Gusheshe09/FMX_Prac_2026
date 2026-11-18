# Fill missing sector labels: first from the same stock's other months,
# then from the daily index constituents and SafeCore holdings; the rest = "Unclassified"
Fill_Sectors <- function(Size_Idx, Index_Constituents, SafeCore) {
  
  Lookup <-
    bind_rows(
      Size_Idx %>% filter(!is.na(Sector)) %>% select(Tickers, Sector),
      Index_Constituents %>% filter(SuperSector != "") %>% select(Tickers, Sector = SuperSector),
      SafeCore %>% filter(!is.na(Sector)) %>% select(Tickers, Sector)) %>%
    count(Tickers, Sector) %>%
    group_by(Tickers) %>% filter(n == max(n)) %>% slice(1) %>% ungroup() %>%
    select(Tickers, Sector_Fill = Sector)
  
  Size_Idx %>%
    left_join(Lookup, by = "Tickers") %>%
    mutate(Sector = coalesce(Sector, Sector_Fill, "Unclassified")) %>%
    select(-Sector_Fill)
  
}