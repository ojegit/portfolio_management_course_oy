#
# Fama-French 5 factor model and PF optimization
#

#https://www.r-bloggers.com/2016/11/russell-2000-quantitative-stock-analysis-in-r-six-stocks-with-amazing-consistent-growth/


################################################################################
# 1. Load Necessary Libraries
################################################################################

#install.packages(c("quantmod", "PerformanceAnalytics", "PortfolioAnalytics", "tidyverse"))
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tidyverse)

################################################################################
# 2. Get Russell 2000 Index Data
################################################################################

# # Get the list of Russell 2000 components
# russell2000 <- stockSymbols("RUSSELL2000")
# 
# # Sample 5 stocks
# set.seed(123) # For reproducibility
# sampled_stocks <- sample(russell2000$Symbol, 5)
# print(sampled_stocks)
# 
# # Get historical data for the sampled stocks
# getSymbols(sampled_stocks, from = "2020-01-01", to = "2023-01-01")


library(rvest)      # web scraping
library(quantmod)   # get stock prices; useful stock analysis functions
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(modelr)     # geom_ref_line() function
library(lubridate)  # working with dates 
library(plotly)     # interactive plots


# Base path and page rows from www.marketvolume.com
base_path <- "http://www.marketvolume.com/indexes_exchanges/r2000_components.asp?s=RUT&row="
row_num <- seq(from = 0, by = 250, length.out = 9)

get_stocklist <- function(base_path, row_num) {
  path <- paste0(base_path, row_num)
  # rvest functions: Get table of stocks
  stock_table <- read_html(path) %>%
    html_node("table") %>%
    html_table() 
  # Format table
  stock_table <- stock_table[-1, 1:2] %>%
    as_tibble() %>%
    rename(symbol = X1, company = X2)
  stock_table
}

get_stocklist(base_path, 0)


################################################################################
# 3. Prepare Data for Fama-French 5-Factor Model
################################################################################

# Download Fama-French 5-Factor data
ff5 <- read.csv("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip", skip = 3)

# Convert dates to Date format
ff5$Date <- as.Date(as.character(ff5$Date), format = "%Y%m%d")

# Merge with stock returns
stock_returns <- do.call(merge, lapply(sampled_stocks, function(x) dailyReturn(Cl(get(x)))))
merged_data <- merge(stock_returns, ff5, by = "Date")

################################################################################
# 4. Run Regressions to Estimate Factor Loadings
################################################################################

factor_loadings <- lapply(sampled_stocks, function(stock) {
  lm(as.formula(paste(stock, "~ Mkt.RF + SMB + HML + RMW + CMA")), data = merged_data)
})

# Extract betas and residuals
betas <- sapply(factor_loadings, coef)
residuals <- sapply(factor_loadings, residuals)

################################################################################
# 5. Calculate Covariance Matrix
################################################################################

# Covariance matrix of factors
factor_cov <- cov(merged_data[, c("Mkt.RF", "SMB", "HML", "RMW", "CMA")])

# Covariance matrix of residuals
residual_cov <- diag(apply(residuals, 2, var))

# Total covariance matrix
cov_matrix <- betas %*% factor_cov %*% t(betas) + residual_cov

################################################################################
# 6. Estimate Optimal Weights Using PortfolioAnalytics
################################################################################


# Create portfolio specification
portfolio_spec <- portfolio.spec(assets = sampled_stocks)
portfolio_spec <- add.constraint(portfolio_spec, type = "full_investment")
portfolio_spec <- add.constraint(portfolio_spec, type = "long_only")
portfolio_spec <- add.objective(portfolio_spec, type = "risk", name = "var")

# Optimize portfolio
opt_portfolio <- optimize.portfolio(R = stock_returns, portfolio = portfolio_spec, optimize_method = "ROI", covmat = cov_matrix)

# Get optimal weights
optimal_weights <- extractWeights(opt_portfolio)
print(optimal_weights)

