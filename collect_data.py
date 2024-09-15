import yfinance as yf
import pandas as pd
import numpy as np

start_date = "2019-01-01"
end_date = "2024-01-01"

# Select a diversified list of stocks and mutual funds
tickers = [
    "AAPL",  # Apple Inc.
    "GOOGL",  # Alphabet Inc.
    "AMZN",  # Amazon.com Inc.
    "MSFT",  # Microsoft Corporation
    "NVDA",  # NVIDIA Corporation
    "TSLA",  # Tesla, Inc.
    "JPM",  # JPMorgan Chase & Co.
    "BAC",  # Bank of America Corporation
    "V",  # Visa Inc.
    "MA",  # Mastercard Incorporated
    "UNH",  # UnitedHealth Group Incorporated
    "JNJ",  # Johnson & Johnson
    "PG",  # Procter & Gamble Company
    "KO",  # The Coca-Cola Company
    "MCD",  # McDonald's Corporation
    "SPY",  # SPDR S&P 500 ETF Trust
    "QQQ",  # Invesco QQQ Trust
    "IWM",  # iShares Russell 2000 ETF
    "VTI",  # Vanguard Total Stock Market ETF
    "BND",  # Vanguard Total Bond Market ETF
]

data = yf.download(tickers, start=start_date, end=end_date, interval="1d")  # Adjust interval for weekly/monthly data