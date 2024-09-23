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

'1. Use the Data Exploratory Analysis to explore the equity returns including descriptive statistics, histogram, distribution curves, boxplot, and correlation analysis and so on.'

data.env = new.env()
tickers <- c(
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
    "WMT", # Walmart
    "PFE", # Pfizer Inc. (PFE) - Healthcare
    "PEP", # PepsiCo Inc. (PEP) - Consumer Staples
    "^GSPC",
    "^IRX")

stock_names = getSymbols(tickers, from = '2018-01-01',
           to = "2024-01-01",
           env = data.env,
           warnings = FALSE,
           auto.assign = TRUE)



for (i in stock_names){
    print(i)
    print(dim(data.env[[i]]))
}

# find common index
common_index_date = vector()
for (i in 1:length(stock_names)){
    current_index = index(data.env[[stock_names[i]]])
    print(stock_names[i])
    print(length(current_index))
    if (i == 1){
        common_index_date = current_index
    }
    else{
        common_index_date = intersect(current_index, common_index_date)    
    }
}

print(length(common_index_date))

ncol = length(stock_names)
nrow = length(common_index_date)
st_data = matrix(nrow=nrow, ncol=ncol)
colnames(st_data) = stock_names
rownames(st_data) = as.Date(common_index_date)

# populate the data matrix with share adjusted price
for (i in 1:length(stock_names)){
    sn = stock_names[i]
    col_name = paste(sn, ".Adjusted", sep="")
    st_data[, sn] = data.env[[sn]][as.Date(common_index_date), col_name]
}

# data cleaning
print(paste("Data dimension before cleaning:", as.character(dim(st_data)) ))

# funcion for getting summary stats
get_summary_stats <- function(data, stat_methods = c("mean","median","Q1","Q3","std","skewness","kurtosis")) {
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
            else if (x == "median"){
                sumstats[x, i] = median(data[,i])
            }
            else if (x == "Q1"){
                sumstats[x, i] = t(quantile(data[,i],p=c(0.25)))
            }
            else if (x == "Q3"){
                sumstats[x, i] = t(quantile(data[,i],p=c(0.75)))
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

sum_stats = get_summary_stats(data=st_data)
print(sum_stats)

# show box plots, qq plots
show_plots <- function(dat, is_save = FALSE, plot_types=c("box", "qq")){
    names = colnames(dat)
    
    if ("box" %in% plot_types){
        par(mfrow=c(2,2))
        for (i in 1:length(names))
        {
            if (is_save){
                dir_name = "boxplot"
                dir.create(dir_name)
                plot_fn = paste(dir_name, "/", "boxplot-", names[i], ".pdf", sep="")
                print(plot_fn)
                pdf(file=plot_fn)
                boxplot(as.vector(dat[,i]),main=names[i])
                dev.off()    
            }
            else{
                boxplot(as.vector(dat[,i]),main=names[i])    
            }
        }    
    }
    
    if ("qq" %in% plot_types){
        par(mfrow=c(2,2))
        for (i in 1:length(names))
        {
            if (is_save){
                dir_name = "qqplot"
                dir.create(dir_name)
                plot_fn = paste(dir_name, "/", "qqplot-", names[i], ".pdf", sep="")
                print(plot_fn)
                pdf(file=plot_fn)
                
                qqnorm(as.vector(dat[,i]),datax=TRUE,main=names[i])
                qqline(as.vector(dat[,i]),datax=TRUE)
                print(shapiro.test(as.vector(dat[,i])))
                
                dev.off()    
            }
            else{
                qqnorm(as.vector(dat[,i]),datax=TRUE,main=names[i])
                qqline(as.vector(dat[,i]),datax=TRUE)
                print(shapiro.test(as.vector(dat[,i])))    
            }
        }        
    }
}


# show_plots(st_data)
show_plots(st_data, plot_types=c("qq"), is_save = FALSE)