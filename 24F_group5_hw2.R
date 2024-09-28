# 24F_group 5
# Md Kauser Ahmmed
# Tafadzwa Chirwa
# Drager Mandiya
# Date: 09/27/2024
# Description: Solution for Homework 2.

install.packages("tidyquant")
install.packages("corrplot")

# Add required libraries
library(moments) 
library(pdfetch)
library(graphics)
library(psych)
library(tidyquant)
library(zoo)
library(corrplot)
library(tidyverse)
library(glue)
library(dplyr)
library(lubridate)
options(digits=4)



'1. Use the Data Exploratory Analysis to explore the equity returns including 
descriptive statistics, histogram, distribution curves, boxplot, and 
correlation analysis and so on.'

# Create new environment to store stock data.
data.env = new.env()

# 20 stocks and mutual funds
stock_mf_tickers <- c(
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
    "VTI", # Vanguard Total Stock Market Index Fund (VTI)
    "FIGIX" # Fidelity International Index Fund (FIGIX)
)

market_premium_ticker <- "^GSPC"
market_premium_name <- "GSPC"

risk_free_ticker <- "^IRX"
risk_free_name <- "IRX"

tickers <- c(
    stock_mf_tickers,
    market_premium_ticker,
    risk_free_ticker
)

# collect daily data for the tickers from 2018 january to 2024 January.
stock_names = getSymbols(tickers, from = '2018-01-01',
           to = "2024-01-01",
           env = data.env,
           warnings = FALSE,
           auto.assign = TRUE)


# Show data dimension for each of the stock data
for (i in stock_names){
    print(i)
    print(dim(data.env[[i]]))
}

# find common index date
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

# function for getting summary stats
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


# Show box and qq plot
show_plots(st_data, plot_types=c("box","qq"), is_save = FALSE)

# function to show histogram plot of return distribution of each stock 
# (Option for saving plot image) 
plot_hist <- function(dat, is_save = FALSE){
    names = colnames(dat)
    par(mfrow=c(2,2))
    for (i in 1:length(names))
    {
        plt_label = paste(names[i], "return distribution")
        st_retrun_values = dat[, names[i]]
        if (is_save){
            dir_name = "hist"
            dir.create(dir_name)
            hist_density_dist_curve_fn = paste(dir_name, "/", 
                                               "hist-return-dist-", 
                                               names[i], 
                                               ".pdf", 
                                               sep="")
            print(hist_density_dist_curve_fn)
            pdf(file=hist_density_dist_curve_fn)
            
            hist(st_retrun_values,prob=T,breaks=50,main=plt_label, xlab = "")
            curve(dnorm(x,mean=mean(st_retrun_values),sd=sd(st_retrun_values)),
                  add=T,
                  col="red", 
                  main=plt_label
                  )
            dev.off()    
        }
        else{
            hist(st_retrun_values,prob=T,breaks=50,main=plt_label, xlab = "")
            curve(dnorm(x,mean=mean(st_retrun_values),sd=sd(st_retrun_values)),
                  add=T,
                  col="red", 
                  main=plt_label
                  )
        }
    }   
}

# correlation analysis
analyze_and_get_correlation <- function(dat, is_save = FALSE){
    par(mfrow=c(1,1))
    cor_mat = cor(dat)
    if (is_save){
        dir_name = "correlation2"
        dir.create(dir_name)
        plot_fn = paste(dir_name, "/", "correlation-analysis", names[i], ".pdf", 
                        sep="")
        print(plot_fn)
        pdf(file=plot_fn)
        corrplot(cor_mat, method = "circle", type = "upper")
        dev.off()
    }
    else{
        corrplot(cor_mat, method = "circle", type = "upper")
        
    }
    return(cor_mat)
}



# calculate returns in log scale, excluding IRX
ret=diff(log(st_data[, -length(stock_names)])) * 100
# omit na values
#ret_no_na = na.omit(ret)

rf = as.vector(st_data[-1, risk_free_name]/100/253)*100

ret.x <- as.data.frame(ret-rf)
ret.x$date <- as.Date(as.numeric(row.names(as.data.frame(st_data[-1, ]))))



plot_hist(na.omit(ret))


# get correlation matrix
cor_mat = analyze_and_get_correlation(na.omit(ret))
print("Correlation Matrix")
print(cor_mat)

'
2.	Based on returns for equity and market index. Use two factor models (time series regression) to compute the alpha estimation of all equities. The two factor models are the market model (CAPM model) and Fama-French 5 factor separately. 
'
# function to get alpha t ratio values for the the model
get_alpha_t_ratios <- function(returns, stock_names, independent_vars = c('GSPC')){
    summary(returns)
    no_of_coefficients = (length(independent_vars) + 1)
    attr_mat = matrix(nrow=length(stock_names), ncol=2)
    colnames(attr_mat) = c("alpha", "t_alpha")
    rownames(attr_mat) = stock_names
    
    for (i in 1: length(stock_names)){
        stock_name = stock_names[i]
        print(stock_name)
        data = returns[,c(stock_name, independent_vars)]
        
        formula_str <- paste(stock_name, '~', paste(independent_vars, 
                                                    collapse = "+"))
        par(mfrow=c(1,1))
        plot(data[, -3], main="Scatter plot")    # scatter plot
        
        abline(lm(formula_str,data=data))
        
        
        ##Regression analysis
        # independent_vars <- c('GSPC', 'SMB', 'HML', 'RMW', 'CMA')
        
        
        fit<-lm(formula_str, data=data)
        print(summary(fit))
        
        attr_mat[stock_name, 'alpha'] = summary(fit)$coefficients["(Intercept)", "Estimate"]
        attr_mat[stock_name, 't_alpha'] = summary(fit)$coefficients["(Intercept)", "t value"]
    }
    
    return(attr_mat)
}

# get list of alpha and t ratios of corresponding alphas for CAPM model
alpha_t_ratios <- get_alpha_t_ratios(ret.x, 
                                     stock_names=head(colnames(ret.x), -2),
                                     independent_vars = c('GSPC'))

# create histogram of t ratios of alphas
hist(alpha_t_ratios[, 't_alpha'], xlab = "t-ratios", main = "Frequency distribution of Stock Alphas (CAPM Model)")


'4.	From the step 3, identify how many assets outperform significantly at 5% of alpha level.'
# Stock data contains  1 GSPC (Market premium) and 1 IRX
# generic function to get outperforming stocks at a given significance level 
# percentage for the given t_ratio values of alpha.
get_outperforming_stock_analysis <- function(no_of_stock, 
                                             significance_level_perc, 
                                             alpha_t_ratios){
    degree_of_freedom = no_of_stock - 2
    significance_level = significance_level_perc/100
    
    t_ratio_at_sig_level = qt(p = 1 - significance_level, 
                              df = degree_of_freedom)
    print(paste("T ratio at sigficance level", significance_level, ":", 
                t_ratio_at_sig_level))
    
    t_ratio_out_performing = alpha_t_ratios[alpha_t_ratios[, 't_alpha'] > t_ratio_at_sig_level, 't_alpha']
    print(paste("Number of assets outperform significantly at", significance_level * 100, "%", ":", length(t_ratio_out_performing)))
    print("Outperming shares are given below:")
    print(names(t_ratio_out_performing))
}

# Analyze and get stocks outperforming significantly at 5% of alpha level. 
# (for CAPM model)
get_outperforming_stock_analysis(no_of_stock=length(stock_names) - 2, 
                              significance_level_perc=5, 
                              alpha_t_ratios=alpha_t_ratios)



# Fama-French 5 factor:
temp = tempfile()
base = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
factor = "F-F_Research_Data_5_Factors_2x3_daily"
format =  "_TXT.zip"
full_url =  glue(base, factor,  format,  sep ="")

#Now we pass full_url to download.file().
download.file(full_url,temp,quiet = TRUE)

#Finally, we can read the txt file using read_table() after unzipping that data with the unz() function.

ff_5factors =  read.table(unz(temp,"F-F_Research_Data_5_Factors_2x3_daily.txt"),
                          skip = 4, header=TRUE)
head(ff_5factors)

# set column names for the factors
colnames(ff_5factors) = c("date_epoch","Mkt-RF","SMB","HML","RMW","CMA","RF")

ff_5factors <- ff_5factors %>%
    mutate(date = ff_5factors[, 'date_epoch']) %>%
    mutate(date = ymd(parse_date_time(date,"%y%m%d"))+days(2))


# left join on the date to get all data of the matching
ff_all <- 
    ret.x %>% 
    left_join(ff_5factors, by = "date")

ff_all <-na.omit(ff_all)

ff_all <- ff_all %>%
    mutate(SMB=as.numeric(as.character(SMB)),
           HML=as.numeric(as.character(HML)),
           RMW=as.numeric(as.character(RMW)),
           CMA=as.numeric(as.character(CMA)),
           RF=as.numeric(as.character(RF))
           )

head(ff_all)
summary(ff_all)

# get list of alpha and t ratios of corresponding alphas.
ff5_alpha_t_ratios <- get_alpha_t_ratios(ff_all, 
                                     stock_names=head(colnames(ret.x), -2),
                                     independent_vars = c('GSPC', 'SMB', 'HML', 'RMW', 'CMA'))

# create histogram of t ratios of alphas
hist(ff5_alpha_t_ratios[, 't_alpha'], xlab = "t-ratios", main = "Frequency distribution of Stock Alphas (Fama-French 5 factor)")

# Analyze and get stocks outperforming significantly at 5% of alpha level. 
# (for fama-french 5 factor model)
get_outperforming_stock_analysis(no_of_stock=length(stock_names) - 2, 
                              significance_level_perc=5, 
                              alpha_t_ratios=ff5_alpha_t_ratios)



