power_estimates <- c()

set.seed(2025)
n_control <- 10
n_cp_peer <- 11

rates <- seq(from = .35, to = .20, by = -.005)

for (rate in rates) {
  power_estimates[match(rate, rates)] <- 0
  p_values <- c()
  
  for (j in 1:2000) {
    control_matches <- rbinom(n_control, 72, .387)
    cp_peer_matches <- rbinom(n_cp_peer, 72, rate)
    
    dat <- as.data.frame(cbind(
      c(control_matches, cp_peer_matches),
      rep(c("control", "cp_peer"), times = c(n_control, n_cp_peer))
    ))
    colnames(dat) <- c("num_matches", "condition")
    dat <- dat %>% mutate(
      num_matches = as.numeric(num_matches),
      condition = factor(condition)
    )
    
    # Mann-Whitney U test
    mw_test <- wilcox.test(num_matches ~ condition, data = dat, exact = FALSE)
    p_values[j] <- mw_test$p.value
  }
  
  whether_sig <- p_values <= 0.05
  power_at_rate <- sum(whether_sig) / length(whether_sig)
  power_estimates[match(rate, rates)] <- power_at_rate
}

power_analysis <- as.data.frame(cbind(rates, power_estimates))
power_analysis
