install.packages("tidyquant")

# Add required libraries
library(moments) 
library(pdfetch)
library(graphics)
library(psych)
library(tidyquant)
library(zoo)
options(digits=4)

"
Homework 2
The assignment will study if a stock/mutual fund can outperform the market, i.e. do financial assets earn significant positive Jensen’s alpha? Each group collects 20 stocks/mutual funds price data (weekly/monthly/daily) over 3-5 years. Make sure the equity selection is well diversified 
Including different types of financial assets. Download the market index data over the same time periods. Download the Fama-French 5 factor monthly data from  
https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html. Merge it with the Fund data to conduct the following analysis.  

1.	Use the Data Exploratory Analysis to explore the equity returns including descriptive statistics, histogram, distribution curves, boxplot, and correlation analysis and so on.  
2.	Based on returns for equity and market index. Use two factor models (time series regression) to compute the alpha estimation of all equities. The two factor models are the market model (CAPM model) and Fama-French 5 factor separately. 
3.	Create histogram of t-ratios of the alphas (intercepts in each model) following the analysis on lecture notes of Jensen’s alpha.
4.	From the step 3, identify how many assets outperform significantly at 5% of alpha level.

Please organize the R code in R script file and name the file by your group #. Include the group members’ name at the beginning of the file. Your homework will graded by the following criteria:
1.	Program runs correctly.
2.	Program specification is complete and correct.
3.	The code is well-formatted and understandable.
4.	The comments and notes are concise and necessary to understand the program
5.	The objects are named appropriately. 
6.	The program is well written and algorithm is sufficient.

"

   


# tq_index("SP500")

"
Use the function pdfetch_YAHOO in package “pdfetch” to retrieve 
the daily price data of 5 stocks over two years (from 2022-01-01 to 2023-12-31).  
For the same time period, download S&P 500 daily data (ticker: ^GSPC) and 
13 week treasury bill (ticker: ^IRX ) from yahoo.finance.
Use the sixth column of adjusted market close price (treasury annual yield in percentage) and 
merge it with the stock data.
"

# parameter to fetch data from 2022-01-01 to 2023-12-31. 
# from_date = as.Date("2019-01-01")
# to_date = as.Date("2024-01-01")
# parameter to fetch daily data
# interval_param = "1d"
# parameter to fetch adjusted market close price
# field_param = "adjclose"

# ticker for diversified 20 stocks

# Select a diversified list of stocks and mutual funds
# tickers = [
#     "AAPL",  # Apple Inc.
#     "GOOGL",  # Alphabet Inc.
#     "AMZN",  # Amazon.com Inc.
#     "MSFT",  # Microsoft Corporation
#     "NVDA",  # NVIDIA Corporation
#     "TSLA",  # Tesla, Inc.
#     "JPM",  # JPMorgan Chase & Co.
#     "BAC",  # Bank of America Corporation
#     "V",  # Visa Inc.
#     "MA",  # Mastercard Incorporated
#     "UNH",  # UnitedHealth Group Incorporated
#     "JNJ",  # Johnson & Johnson
#     "PG",  # Procter & Gamble Company
#     "KO",  # The Coca-Cola Company
#     "MCD",  # McDonald's Corporation
#     "SPY",  # SPDR S&P 500 ETF Trust
#     "QQQ",  # Invesco QQQ Trust
#     "IWM",  # iShares Russell 2000 ETF
#     "VTI",  # Vanguard Total Stock Market ETF
#     "BND",  # Vanguard Total Bond Market ETF
# ]


# fetch data for 20 stock from Yahoo.finance
# data_index = pdfetch_YAHOO(
#   tickers_index, 
#   fields = field_param, 
#   from = from_date,
#   to = to_date, 
#   interval = interval_param
# )

# read data from csv file. pdfetch not working 
data_combined = read.csv("stock_20_data.csv")
data_z = zoo(data_combined[, -1], order.by = data_combined$Date)
print(dim(data_z))
data_z = na.omit(data_z)
print(dim(data_z))


# 1. Conduct data exploratory analysis using all 5 stock daily log returns over two years.
log_df = diff(log(data_z))
# remove data that contains na. 
log_combined_df = na.omit(log_df)
paste("Is there any NA?", anyNA(log_combined_df))

get_summary_stats <- function(data, stat_methods = c("mean", "std","skewness","kurtosis")) {
  stock_names = colnames(data)
  paste("Number of stock: ", length(stock_names))
  
  cat("Statistics methods : ", stat_methods)
  
  sumstats = matrix(0, nrow = length(stat_methods), ncol = length(stock_names))
  rownames(sumstats) = stat_methods
  colnames(sumstats) = stock_names
  
  for (i in 1:length(colnames(sumstats)))
  {
    for (x in rownames(sumstats)){
      if (x == "mean"){
        sumstats[x, i] = mean(data[,i])
      }
      else if (x == "std"){
        sumstats[x, i] = sd(data[,i])
      }
      else if (x == "skewness"){
        sumstats[x, i] = skewness(data[,i])
      }
      else if (x == "kurtosis"){
        sumstats[x, i] = kurtosis(data[,i])
      }
    }
  }
  return(sumstats)
}

# 2. Compute the four summary statistics for each stock's log returns 
# including mean, standard deviation, skewness, kurtosis. 
print("Computed the four summary statistics")
sumstats = get_summary_stats(data = log_combined_df, stat_methods = c("mean", "std","skewness","kurtosis"))
print(sumstats)

# 3. Plot the histogram of each stock's log returns and fit a 
# normal distribution to the histogram separately.
stock_names = colnames(log_combined_df)
par(mfrow=c(2,2))
for (i in 1:length(stock_names))
{
  hist(log_combined_df[, i], breaks=50, main = paste("Hist of ", stock_names[i], "'s return"), xlab = "Daily Log Return")
  curve(dnorm(x,mean=sumstats[1,i],sd=sumstats[2,i]),add=T,col="red")
}


"4. For each stock, use the last close price as the current price. Assume 
the returns have the normal distribution. Simulate the stock prices 
in next 30 days."


get_stock_price_simulation <- function(niter, days_count, mean_daily, sd_daily, base_price) {
  sim_price = matrix(nrow = niter, ncol = days_count)

  set.seed(2009)
  for (i in 1:niter) { 
    r = rnorm(days_count, mean = mean_daily, sd = sd_daily) 
    logPrice = log(base_price) + cumsum(r)
    sim_price[i, ] = exp(logPrice)
    '
    for (j in col_names){
      if (j == "min"){
        sim_price[i, j] = exp(min(logPrice))
      }
      else if (j == "max"){
        sim_price[i, j] = exp(max(logPrice))
      }
      else if (j == "mean"){
        sim_price[i, j] = exp(mean(logPrice))
      }
      '
  }
  
  return(sim_price)
}

# sumstats_daily_price = get_summary_stats(na.omit(data_combined), c("mean", "std","skewness","kurtosis"))
days_count = 30
sim_price_all = matrix(nrow = days_count, ncol = length(stock_names))
colnames(sim_price_all) = stock_names
  
for (i in colnames(data_combined)){
  print(i)
  current_price = as.numeric(tail(data_combined[, i], 1))
  mean_daily = sumstats["mean", i]
  sd_daily = sumstats["std", i]
  sim_price = get_stock_price_simulation(niter = 1, days_count = days_count, mean_daily = mean_daily, sd_daily = sd_daily, base_price = current_price)
  sim_price_all[, i] = as.vector(sim_price)
}

print("4: Simaulated price for 30 days.")
print(sim_price_all)


# 5. For each stock, plot the time series of the historical prices with the simulated prices.
# get historical data for next 30 days. i.e. from "2024-01-01" to "2024-01-30"
tickers_all = c("AAPL","WMT","IBM", "^GSPC", "^IRX")
new_from_date = "2024-01-01"
new_to_date = "2024-01-30"

data_stock_30_days = pdfetch_YAHOO(
  tickers_all,
  fields=field_param,
  from=new_from_date,
  to=new_to_date, 
  interval= interval_param
)

par(mfrow=c(2, 3))
for (i in stock_names)
{
  print(i)
  # print(data_stock_30_days[1:30, i])
  # print(sim_price_all[1:30, i])
  y_lim = c(min(min(sim_price_all[1:30, i]), min(data_stock_30_days[1:30, i])), 
            max(max(sim_price_all[1:30, i]), max(data_stock_30_days[1:30, i])))
  print(y_lim)
  plot(x=1:30, y=as.vector(data_stock_30_days[1:30, i]), type = "l", col = "blue", xlab = "Days", ylab = "Stock Price", main = paste(i, ": Historical Vs Simulated"))
  lines(x=1:30, y=sim_price_all[1:30, i], col = "red")
  # legend("topright", legend = c("Historical", "Simulation"), col = c("blue", "red"), lty = 1)
}