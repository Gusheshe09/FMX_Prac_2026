# Constrained optimal weights at one rebalance date (quadprog)
Optim_Weights <- function(RebDate, Rets_M, Info, Type = "MinVol", WindowStart = ymd(20100101),
                          MinObs = 60, Lambda = 4, Max_Single = 0.35,
                          Max_Bonds = 0.25, Max_Equity = 0.75, Max_Commod = 0.15, Max_Global = 0.60) {
  
  # Expanding window from 2010; only assets with at least 5 years (60 months) of data
  Window <- Rets_M %>% filter(date >= WindowStart, date <= RebDate)
  Eligible <- Window %>% group_by(Name) %>%
    summarise(N = sum(!is.na(Return)), .groups = "drop") %>% filter(N >= MinObs) %>% pull(Name)
  
  R <- Window %>% filter(Name %in% Eligible) %>% spread(Name, Return) %>% select(-date)
  A <- Info %>% filter(Name %in% colnames(R)) %>% arrange(match(Name, colnames(R)))
  
  Mu <- sapply(R, function(x) exp(mean(log(1 + x), na.rm = TRUE)) - 1)          # geometric mean
  Sigma <- as.matrix(Matrix::nearPD(cov(R, use = "pairwise.complete.obs"))$mat)  # positive definite
  N <- ncol(R)
  
  # Constraints: sum = 1 | w >= 0 | w <= max | class and region caps
  Amat <- cbind(1, diag(N), -diag(N),
                -as.numeric(A$Class == "Bonds"), -as.numeric(A$Class == "Equity"),
                -as.numeric(A$Class == "Commodity"), -as.numeric(A$Region == "Global"))
  bvec <- c(1, rep(0, N), rep(-Max_Single, N), -Max_Bonds, -Max_Equity, -Max_Commod, -Max_Global)
  
  if (Type == "MinVol") { Dmat <- 2 * Sigma; dvec <- rep(0, N) } else { Dmat <- Lambda * Sigma; dvec <- Mu }
  
  w <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)$solution
  w[abs(w) < 1e-6] <- 0
  
  tibble(date = RebDate, Name = colnames(R), Weight = w / sum(w))
  
}