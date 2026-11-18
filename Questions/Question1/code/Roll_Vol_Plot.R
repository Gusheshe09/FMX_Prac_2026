# Rolling annualised volatility plot
# df: tidy frame with columns date, Name, Return (daily simple returns)
Roll_Vol_Plot <- function(df, Width = 63, Scale = 252, Title = "", Subtitle = "") {
  
  df_plot <-
    df %>%
    group_by(Name) %>%
    arrange(date) %>%
    mutate(RollVol = RcppRoll::roll_sd(Return, n = Width, fill = NA, align = "right") * sqrt(Scale)) %>%
    ungroup() %>%
    filter(!is.na(RollVol))
  
  g <-
    df_plot %>%
    ggplot() +
    geom_line(aes(date, RollVol, color = Name), alpha = 0.8, linewidth = 0.8) +
    labs(title = Title, subtitle = Subtitle, x = "", y = "Annualised volatility") +
    fmxdat::theme_fmx(title.size = fmxdat::ggpts(30), subtitle.size = fmxdat::ggpts(25),
                      legend.size = fmxdat::ggpts(25), axis.size = fmxdat::ggpts(25))
  
  fmxdat::finplot(g, y.pct = TRUE, y.pct_acc = 1, x.date.type = "%Y", x.date.dist = "1 year")
  
}