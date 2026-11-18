# Daily cross-sectional volatility (dispersion) of index constituents
# Weighted:   sqrt( sum w_i (R_i - Rbar_w)^2 ),  Rbar_w = sum w_i R_i
# Unweighted: plain cross-sectional standard deviation
CSV_Calc <- function(Constituents) {
  
  Constituents %>%
    filter(!is.na(Return)) %>%
    group_by(date) %>%
    mutate(w = weight / sum(weight)) %>%
    summarise(Weighted = sqrt(sum(w * (Return - sum(w * Return))^2)),
              Unweighted = sd(Return), .groups = "drop")
  
}