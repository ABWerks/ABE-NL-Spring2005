#' Nonlinear Least Squares Analysis between BTC and NASDAQ Composite
#' 
#' This script performs NLS regression analysis to model the relationship
#' between BTC and NASDAQ Composite closing prices.

library(fpp3)
library(ggplot2)
library(entropy)
library(nonlinearTseries)
library(magrittr)

# Load the data
load("./Data/timeseries.Rdata")

# Extract the relevant time series
btcTimeSeries <- dsClose$Close.BTC
compTimeSeries <- dsClose$Close.COMP

# Create a data frame for analysis
nlsDataFrame <- data.frame(
  btcTimeSeries = btcTimeSeries,
  compTimeSeries = compTimeSeries,
  timeIndex = 1:length(btcTimeSeries)
)

# Remove NA values
nlsDataFrame <- na.omit(nlsDataFrame)

# Normalize the data
nlsDataFrame$btcNormalized <- scale(nlsDataFrame$btcTimeSeries)
nlsDataFrame$compNormalized <- scale(nlsDataFrame$compTimeSeries)

# Create time series objects
tsBtc <- ts(nlsDataFrame$btcNormalized, frequency = 252)
tsComp <- ts(nlsDataFrame$compNormalized, frequency = 252)
par(mfrow = c(2, 1))
plot(tsBtc)
plot(tsComp)
# Calculate mutual information between BTC and NASDAQ Composite


# Estimate embedding dimension using False Nearest Neighbors
embeddingDim <- falseNearest(tsBtc, dim = 1:10, tau = 1)

# Create time delay embedding
embedding <- timeDelayEmbedding(tsBtc, dimension = embeddingDim$dimension, timeLag = 1)

# Calculate correlation dimension using box-counting method
corDim <- correlationDimension(embedding)

# Calculate largest Lyapunov exponent using Wolf's algorithm
lyapunov <- largestLyapunov(embedding)

# Calculate sample entropy
sampleEntropy <- sampleEntropy(embedding, m = 2, r = 0.2 * sd(tsBtc))

# Create plots
# 1. Mutual Information Plot
mutualInfoDf <- data.frame(
  lag = seq(-10, 10),
  mutualInformation = mutualInfo
)

ggplot(mutualInfoDf, aes(x = lag, y = mutualInformation)) +
  geom_line() +
  geom_point() +
  labs(title = "Mutual Information between BTC and NASDAQ",
       x = "Time Lag",
       y = "Mutual Information") +
  theme_minimal()

# 2. Correlation Dimension Plot
corDimDf <- data.frame(
  radius = corDim$radius,
  correlation = corDim$correlation
)

# Save results
pdf("./img/nls_analysis_plots.pdf", width = 12, height = 8)

# Print Mutual Information plot
print(ggplot(mutualInfoDf, aes(x = lag, y = mutualInformation)) +
        geom_line() +
        geom_point() +
        labs(title = "Mutual Information between BTC and NASDAQ",
             x = "Time Lag",
             y = "Mutual Information") +
        theme_minimal())

dev.off()

# Save analysis results
analysisResults <- list(
  mutualInfo = mutualInfo,
  embeddingDim = embeddingDim,
  embedding = embedding,
  corDim = corDim,
  lyapunov = lyapunov,
  sampleEntropy = sampleEntropy
)

save(analysisResults, file = "./Models/nls_analysis_results.Rdata")

# Print Phase Space plot
print(ggplot(data.frame(V1 = embedding[, 1], V2 = embedding[, 2]), aes(x = V1, y = V2)) +
        geom_point(alpha = 0.5) +
        labs(title = "Phase Space Reconstruction",
             x = "BTC (t)",
             y = "BTC (t + τ)") +
        theme_minimal())

# Print Lyapunov Exponent plot
print(ggplot(lyapunov_df, aes(x = time, y = lyapunov)) +
        geom_line() +
        labs(title = "Lyapunov Exponent",
             x = "Time",
             y = "Lyapunov Exponent") +
        theme_minimal())

dev.off()

# Save analysis results
analysis_results <- list(
  mutual_info = mutual_info,
  embedding_dim = embedding_dim,
  phase_space = phase_space,
  lyapunov = lyapunov
)

save(nls_results, file = "./Models/nls_analysis_results.Rdata")
