# Fit an AR(1)-GARCH(1,1) to one return series (Prac 6 specification)
# Return: xts of demeaned daily log returns (one column)
GARCH_Fit <- function(Return, Model = "gjrGARCH", Dist = "sstd") {
  
  spec <- rugarch::ugarchspec(
    variance.model = list(model = Model, garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
    distribution.model = Dist)
  
  rugarch::ugarchfit(spec = spec, data = Return, solver = "hybrid")
  
}

# Compare the four GARCH variants used in Prac 6 on information criteria
GARCH_Compare <- function(Return, Dist = "sstd") {
  
  Models <- c("sGARCH", "gjrGARCH", "eGARCH", "apARCH")
  model.list <- list()
  for (p in seq_along(Models)) {
    model.list[[p]] <- GARCH_Fit(Return, Model = Models[p], Dist = Dist)
  }
  fit.mat <- sapply(model.list, rugarch::infocriteria)
  colnames(fit.mat) <- Models
  rownames(fit.mat) <- rownames(rugarch::infocriteria(model.list[[1]]))
  fit.mat
  
}

# Extract annualised conditional volatility from a fit as a tidy frame
GARCH_Sigma <- function(Fit, Name, Scale = 252) {
  
  rugarch::sigma(Fit) %>%
    tbl2xts::xts_tbl() %>%
    setNames(c("date", "Sigma")) %>%
    mutate(date = as.Date(date), Sigma = Sigma * sqrt(Scale), Name = Name)
  
}