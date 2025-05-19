#' Cross Correlation Analysis between BTC, SP500, and NASDAQ Composite
#' 
#' This script performs cross-correlation analysis between the time series
#' to identify potential lagged relationships.

library(fpp3)
library(ggplot2)

# Load the data
setwd("/Users/adambatten/Projects/ABE-NewsLetters/NL-Analysis/Final/ABE-NL-Spring2005")
load("./Data/timeseries.Rdata")

# Function to plot cross-correlation
plot_cross_correlation <- function(series1, series2, lag.max = 30) {
  # Calculate cross-correlation
  cc <- ccf(x = series1, y = series2, lag.max = lag.max, plot = FALSE)
  
  # Create plot
  ggplot(data = data.frame(lag = cc$lag, acf = cc$acf)) +
    geom_hline(yintercept = c(-1.96/sqrt(length(series1)), 1.96/sqrt(length(series1))), 
               linetype = "dashed", color = "red") +
    geom_line(aes(x = lag, y = acf)) +
    geom_point(aes(x = lag, y = acf)) +
    labs(title = paste("Cross-Correlation between", deparse(substitute(series1)), "and", deparse(substitute(series2))),
         x = "Lag", y = "Cross-Correlation") +
    theme_minimal()
}

# Extract the time series
btc <- dsClose$Close.BTC
sp500 <- dsClose$Close.SP500
comp <- dsClose$Close.COMP

# Perform cross-correlation analysis
# BTC vs SP500
plot_cross_correlation(btc, sp500) -> btc_sp500_cc

# BTC vs NASDAQ
plot_cross_correlation(btc, comp) -> btc_comp_cc

# SP500 vs NASDAQ
plot_cross_correlation(sp500, comp) -> sp500_comp_cc

# Save plots
pdf("./img/cross_correlation_plots.pdf", width = 12, height = 8)
print(btc_sp500_cc)
print(btc_comp_cc)
print(sp500_comp_cc)
dev.off()

# Save results
cross_cor_results <- list(
  btc_sp500 = ccf(btc, sp500, lag.max = 30, plot = FALSE),
  btc_comp = ccf(btc, comp, lag.max = 30, plot = FALSE),
  sp500_comp = ccf(sp500, comp, lag.max = 30, plot = FALSE)
)

save(cross_cor_results, file = "./Models/cross_correlation_results.Rdata")
