# Daily index returns rebuilt from constituent weights and returns
# (weights rescaled to sum to 1 each day)
Index_From_Constituents <- function(Constituents, Indexes) {
  
  Constituents %>%
    filter(Index %in% Indexes, !is.na(Return)) %>%
    group_by(Index, date) %>%
    mutate(weight = weight / sum(weight, na.rm = TRUE)) %>%
    summarise(Return = sum(weight * Return, na.rm = TRUE), .groups = "drop") %>%
    rename(Name = Index)
  
}