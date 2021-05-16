## Introduction to Porfolio Analysis in R
library(quantmod)
# Chapter 1: Portfolio Weights & Returns
ko <- getSymbols("KO", src = "yahoo", auto.assign = FALSE)
pep <- getSymbols("PEP", src = "yahoo", auto.assign = FALSE)
ko <- ko["2003/2016"]
pep <- pep["2003/2016"]
ko <- ko[ ,4]
pep <- pep[ ,4]
head(ko)
head(pep)
tail(ko)
tail(pep)
plot(ko)
lines(pep, col = "red", lwd = 2)

ko_pep <- ko/pep
head(ko_pep)
plot.zoo(ko_pep)
abline(h = 0.50)

# Compute portfolio weight by asset
#example

values <- c(500000, 200000, 100000, 20000)
names(values) <- c("Inv 1", "Inv 2", "Inv 3", "Inv 4")
weights <- values/sum(values)
barplot(weights)

# Chapter 2: Portfolio Performance Evaluation
library(PerformanceAnalytics)

# Create the weights
eq_weights <- c(0.50, 0.50)

# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(R = returns, weights = eq_weights)

# Create a portfolio rebalancing monthly 
pf_rebal <- Return.portfolio(R = returns, weights = eq_weights, rebalance_on = "months")

# Plot the time-series
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(pf_bh)
plot.zoo(pf_rebal)

# Create a portfolio that rebalances monthly
pf_rebal <- Return.portfolio(returns, weights = eq_weights, rebalance_on = "months", verbose = TRUE )

# Create eop_weight_bh
eop_weight_bh <- pf_bh$EOP.Weight

# Create eop_weight_rebal
eop_weight_rebal <- pf_rebal$EOP.Weight

# Plot end of period weights
par(mfrow = c(2, 1), mar=c(2, 4, 2, 2))
plot.zoo(eop_weight_bh$AAPL)
plot.zoo(eop_weight_rebal$AAPL)

# Chapter 3: Drivers of Performance
sample_returns <- c(-0.02,0.00,0.00,0.06,0.02,0.03,-0.01,0.04)
mean(sample_returns)
mean.geometric(sample_returns)
sd(sample_returns)
sharpe <- (mean(sample_returns)-0.004)/StdDev(sample_returns)
sharpe

# Compute the annualized mean
Return.annualized(sp500_returns)

# Compute the annualized standard deviation
StdDev.annualized(sp500_returns)

# Compute the annualized Sharpe ratio: ann_sharpe
ann_sharpe <- mean(Return.annualized(sp500_returns))/StdDev.annualized(sp500_returns) 

# Compute all of the above at once using table.AnnualizedReturns()
table.AnnualizedReturns(sp500_returns)

# Create a matrix with variances on the diagonal
diag_cov <- diag(sds^2)

# Create a covariance matrix of returns
cov_matrix <- cov(returns)

# Create a correlation matrix of returns
cor_matrix <- cor(returns)

# Verify covariances equal the product of standard deviations and correlation
all.equal(cov_matrix[1,2], cor_matrix[1,2] * sds[1] * sds[2])

# Create a weight matrix w
w <- as.matrix(weights)

# Create a matrix of returns
mu <- as.matrix(vmeans)

# Calculate portfolio mean monthly returns
t(w) %*% mu

# Calculate portfolio volatility
sqrt(t(w) %*% sigma %*% w)

# Create portfolio weights
weights <- c(.40,.40, .10,.10)
names(weights) <- c("equities","bonds","real estate","commodities")

# Create volatility budget
vol_budget <- StdDev(returns, portfolio_method = "component", weights = weights)

# Make a table of weights and risk contribution
weights_percrisk <- cbind(weights, vol_budget$pct_contrib_StdDev)
colnames(weights_percrisk) <- c("weights", "perc vol contrib")

# Print the table
print(weights_percrisk)
# Chapter 4: Portfolio Optimization

# Verify the class of returns 
class(returns)

# Investigate the dimensions of returns
dim(returns)

# Create a vector of row means
ew_preturn <- rowMeans(returns)

# Cast the numeric vector back to an xts object
ew_preturns <- xts(ew_preturn, order.by = time(returns))

# Plot ew_preturns
plot.zoo(ew_preturns)
# Load tseries
library(tseries)

# Create an optimized portfolio of returns
opt <- portfolio.optim(returns)

# Create pf_weights
pf_weights <- opt$pw

# Assign asset names
names(pf_weights) <- colnames(returns)

# Select optimum weights opt_weights
opt_weights <- pf_weights[pf_weights >= 0.01]

# Bar plot of opt_weights
barplot(opt_weights)

# Print expected portfolio return and volatility
opt$pm
opt$ps

# Create vectors of maximum weights
max_weights1 <- rep(1, ncol(returns))
max_weights2 <- rep(0.1, ncol(returns))
max_weights3 <- rep(0.05, ncol(returns))

# Create an optimum portfolio with max weights of 100%
opt1 <- portfolio.optim(returns, reshigh = max_weights1)

# Create an optimum portfolio with max weights of 10%
opt2 <- portfolio.optim(returns, reshigh = max_weights2)

# Create an optimum portfolio with max weights of 5%
opt3 <- portfolio.optim(returns, reshigh = max_weights3)

# Calculate how many assets have a weight that is greater than 1% for each portfolio
sum(opt1$pw > .01)
sum(opt2$pw > .01)
sum(opt3$pw > .01)

# Print portfolio volatilites 
opt1$ps
opt2$ps
opt3$ps

## Computing the efficient frontier
# Calculate each stocks mean returns
stockmu <- colMeans(returns)

# Create a grid of target values
grid <- seq(0.01, max(stockmu), length.out = 50)  

# Create empty vectors to store means and deviations
vpm <- vpsd <- rep(NA, length(grid))

# Create an empty matrix to store weights
mweights <- matrix(NA, 50, 30)

# Create your for loop
for(i in 1:length(grid)) {
  opt <- portfolio.optim(x = returns , pm = grid[i])
  vpm[i] <- opt$pm
  vpsd[i] <- opt$ps
  mweights[i, ] <- opt$pw
}

# Load the package PortfolioAnalytics
library(PortfolioAnalytics)

# Explore PortfolioAnalytics 
?PortfolioAnalytics

# Happy reading!