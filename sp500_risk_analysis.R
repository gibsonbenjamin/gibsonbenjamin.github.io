# S&P 500 Risk & Return Analysis
# Ben Gibson
# Loads price data into SQLite, calculates returns and Sharpe ratios, exports for Tableau

library(DBI)
library(RSQLite)
library(tidyverse)


# Connect to SQLite database (creates sp500.db if it doesn't exist)
con <- dbConnect(RSQLite::SQLite(), "sp500.db")


# Load CSVs
prices_wide <- read_csv("SP500_closing_prices_10_years.csv")
tickers     <- read_csv("tickers_gics.csv")


# Reshape prices from wide to long format (one row per ticker per day)
prices_long <- prices_wide %>%
  pivot_longer(
    cols      = -Date,
    names_to  = "ticker",
    values_to = "close_price"
  ) %>%
  rename(date = Date) %>%
  filter(!is.na(close_price))


# Write tables to SQLite
dbWriteTable(con, "tickers_gics", tickers, overwrite = TRUE)
dbWriteTable(con, "daily_prices", prices_long, overwrite = TRUE)


# Calculate daily returns using LAG() window function
returns_query <- "
  SELECT
    ticker,
    date,
    close_price,
    (close_price - LAG(close_price) OVER (PARTITION BY ticker ORDER BY date))
      / LAG(close_price) OVER (PARTITION BY ticker ORDER BY date) AS daily_return
  FROM daily_prices
"

dbExecute(con, "DROP TABLE IF EXISTS daily_returns")
dbExecute(con, paste("CREATE TABLE daily_returns AS", returns_query))


# Calculate annualized metrics and Sharpe ratio (252 trading days, risk-free rate = 0)
metrics_query <- "
  SELECT
    ticker,
    AVG(daily_return)                              AS avg_daily_return,
    STDEV(daily_return)                            AS std_daily_return,
    AVG(daily_return)  * 252                       AS avg_annual_return,
    STDEV(daily_return) * SQRT(252)                AS std_annual_return,
    ROUND(
      (AVG(daily_return) * 252)
      / (STDEV(daily_return) * SQRT(252)), 2
    )                                              AS sharpe_ratio
  FROM daily_returns
  WHERE daily_return IS NOT NULL
  GROUP BY ticker
"

sharpe_metrics <- dbGetQuery(con, metrics_query)


# Join with GICS sector classifications for Tableau filtering
dbWriteTable(con, "sharpe_metrics", sharpe_metrics, overwrite = TRUE)

final_query <- "
  SELECT
    m.ticker,
    m.avg_daily_return,
    m.std_daily_return,
    m.avg_annual_return,
    m.std_annual_return,
    m.sharpe_ratio,
    g.Security,
    g.[GICS Sector]       AS sector,
    g.[GICS Sub-Industry] AS sub_industry
  FROM sharpe_metrics m
  LEFT JOIN tickers_gics g ON m.ticker = g.Symbol
"

final_metrics <- dbGetQuery(con, final_query)


# Export to CSV for Tableau
write_csv(final_metrics, "sp500_sharpe_metrics.csv")


# Close connection
dbDisconnect(con)
