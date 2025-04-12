#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' ABE Newsletter
#' Spring 2025
#' ABE
#' Dependencies:
#'  ~/etl.R
#' Description:
#'  EDA for the BTC, SP500, and NASDAQ composite daily closing prices from 2021
#'  to current.
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(cowplot)
library(ggthemes)
library(ggdark)
library(lubridate)
library(fpp3)

theme_set(dark_theme_bw())
# To change them back, use invert_geom_defaults()
options(tibble.width = Inf)

load("./Drafts/Spring2025/Data/timeseries.Rdata")

# TS plot
gg1 <- tdsDaily |> 
  filter(Series == "Close.COMP") |> 
  autoplot() +
  ggtitle("NASDAQ")
gg2 <- tdsDaily |> 
  filter(Series == "Close.SP500") |> 
  autoplot() +
  ggtitle("S&P 500")
gg3 <- tdsDaily |> 
  filter(Series == "Close.BTC") |> 
  autoplot() +
  ggtitle("Bitcoin")
plot_grid(gg1 + ylab("USD ($)"), gg2 + ylab(""), gg3 + ylab(""), nrow = 1)
plot_grid(gg1 + ylab("USD ($)"), gg3 + ylab(""), nrow = 1) +
  dark_theme_bw(base_size = 10)

ggsave(filename = "./Drafts/Spring2025/img/fig1-tsplot-Close.png"
       , height = 4, width = 7)

# Cross-correlation
# The lag k value returned by ccf(x, y) estimates the correlation between BTC[t+k] and COMP[t].
ccf(y = dsClose$Close.COMP, x = dsClose$Close.BTC, lag.max = 90
    , main = "NASDAQ Composite & BTC", ylab = "Cross-Correlation", xlab = "Lag (Days)")
dev.off()

png(filename = "./Drafts/Spring2025/img/fig2-ccfplot-Close.png"
    , height = 4, width = 7, units = "in", res = 300)
ccf(y = dsClose$Close.COMP, x = dsClose$Close.BTC, lag.max = 90
    , main = "NASDAQ Composite & Bitcoin", ylab = "Cross-Correlation", xlab = "Lag (Days)")
dev.off()
