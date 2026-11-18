# Check the mandate limits for a set of weights at each date
Check_Limits <- function(W, Info) {
  
  W %>% left_join(Info, by = "Name") %>%
    group_by(date) %>%
    summarise(Sum = sum(Weight), Max_Single = max(Weight),
              Bonds = sum(Weight[Class == "Bonds"]), Equity = sum(Weight[Class == "Equity"]),
              Commodity = sum(Weight[Class == "Commodity"]), Global = sum(Weight[Region == "Global"]),
              N_Assets = sum(Weight > 0), .groups = "drop")
  
}