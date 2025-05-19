# Exploratory Data Analysis for Time Series Data

library(fpp3)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggdark)
library(zoo)
library(cowplot)

# Load the data
load("./Data/timeseries.Rdata")

# Extract time series
ds <- dsClose |> 
  select(Date, Close.BTC, Close.SP500, Close.COMP) |> 
  rename(
    btc = Close.BTC,
    sp500 = Close.SP500,
    nasdaq = Close.COMP
  )

# Convert to tsibble
tsData <- ds |> 
  as_tsibble(index = Date) |> 
  fill_gaps() |> 
  replace_na(list(btc = 0, sp500 = 0, nasdaq = 0))

# Basic statistics
summaryStats <- tsData |> 
  summarise(
    btcMean = mean(btc, na.rm = TRUE),
    btcSd = sd(btc, na.rm = TRUE),
    sp500Mean = mean(sp500, na.rm = TRUE),
    sp500Sd = sd(sp500, na.rm = TRUE),
    nasdaqMean = mean(nasdaq, na.rm = TRUE),
    nasdaqSd = sd(nasdaq, na.rm = TRUE)
  )

print("Summary Statistics:")
print(summaryStats)

# Plot individual time series and save
pdf("./img/eda_time_series.pdf", width = 12, height = 8)
print(ggplot(tsData, aes(x = Date)) +
  geom_line(aes(y = btc, color = "Bitcoin")) +
  geom_line(aes(y = sp500, color = "S&P 500")) +
  geom_line(aes(y = nasdaq, color = "NASDAQ")) +
  labs(
    title = "Time Series Comparison",
    y = "Closing Price",
    color = "Index"
  ) +
  dark_theme_bw())
dev.off()

# Save ACF plots
gg1 <- tdsDaily |> 
  filter(Series == "Close.COMP") |> 
  fill_gaps() |> 
  ACF(lag.max = 30) |> 
  autoplot() +
  ggtitle("NASDAQ")
gg2 <- tdsDaily |> 
  filter(Series == "Close.SP500") |> 
  fill_gaps() |> 
  ACF(lag.max = 30) |> 
  autoplot() +
  ggtitle("S&P 500")
gg3 <- tdsDaily |> 
  filter(Series == "Close.BTC") |> 
  fill_gaps() |> 
  ACF(lag.max = 30) |> 
  autoplot() +
  ggtitle("Bitcoin")
pdf("./img/eda_acf_plots.pdf", width = 12, height = 8)
plot_grid(gg1 + ylab("USD ($)"), gg2 + ylab(""), gg3 + ylab(""), nrow = 1) +
  dark_theme_bw(base_size = 10)
dev.off()



# Calculate and plot rolling correlations
correlations <- tsData |> 
  mutate(
    btcSp500 = zoo::rollapplyr(cbind(btc, sp500), width = 30, 
                           FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
                           by.column = FALSE, fill = NA),
    btcNasdaq = zoo::rollapplyr(cbind(btc, nasdaq), width = 30, 
                         FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
                         by.column = FALSE, fill = NA),
    sp500Nasdaq = zoo::rollapplyr(cbind(sp500, nasdaq), width = 30, 
                           FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
                           by.column = FALSE, fill = NA)
  )

ggplot(correlations, aes(x = Date)) +
  geom_line(aes(y = btcSp500, color = "BTC vs SP500")) +
  geom_line(aes(y = btcNasdaq, color = "BTC vs NASDAQ")) +
  geom_line(aes(y = sp500Nasdaq, color = "SP500 vs NASDAQ")) +
  labs(
    title = "Rolling 30-Day Correlations",
    y = "Correlation",
    color = "Pair"
  ) +
  dark_theme_bw()

# Save correlation plots
pdf("./img/eda_correlations.pdf", width = 12, height = 8)
print(ggplot(correlations, aes(x = Date)) +
        geom_line(aes(y = btcSp500, color = "BTC vs SP500")) +
        geom_line(aes(y = btcNasdaq, color = "BTC vs NASDAQ")) +
        geom_line(aes(y = sp500Nasdaq, color = "SP500 vs NASDAQ")) +
        labs(
          title = "Rolling 30-Day Correlations",
          y = "Correlation",
          color = "Pair"
        ) +
        dark_theme_bw())
dev.off()

# Save EDA results
save(list = c("summaryStats", "tsData", "correlations"), 
     file = "./Models/eda_results.Rdata")
