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
#    "SPAXX", # Fidelity Government Money Market Fund"
    "^GSPC",
    "^IRX")


get_data <- function(from_file=FALSE){
    
    
}

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
            hist_density_dist_curve_fn = paste(dir_name, "/", "hist-return-dist-", names[i], ".pdf", sep="")
            print(hist_density_dist_curve_fn)
            pdf(file=hist_density_dist_curve_fn)
            
            hist(st_retrun_values,prob=T,breaks=50,main=plt_label, xlab = "")
            curve(dnorm(x,mean=mean(st_retrun_values),sd=sd(st_retrun_values)),add=T,col="red", main=plt_label)
            dev.off()    
        }
        else{
            hist(st_retrun_values,prob=T,breaks=50,main=plt_label, xlab = "")
            curve(dnorm(x,mean=mean(st_retrun_values),sd=sd(st_retrun_values)),add=T,col="red", main=plt_label)
        }
    }   
    
    
}



# correlation analysis
analyze_and_get_correlation <- function(dat, is_save = FALSE){
    cor_mat = cor(dat)
    if (is_save){
        dir_name = "correlation2"
        dir.create(dir_name)
        plot_fn = paste(dir_name, "/", "correlation-analysis", names[i], ".pdf", sep="")
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
ret=diff(log(st_data[, -20])) * 100
# omit na values
#ret_no_na = na.omit(ret)

rf = as.vector(st_data[-1, 'IRX']/100/253)*100

ret.x <- as.data.frame(ret-rf)
ret.x$date <- as.Date(as.numeric(row.names(as.data.frame(st_data[-1, ]))))



plot_hist(na.omit(ret))



cor_mat = analyze_and_get_correlation(ret_no_na)
print("Correlation Matrix")
print(cor_mat)

'
2.	Based on returns for equity and market index. Use two factor models (time series regression) to compute the alpha estimation of all equities. The two factor models are the market model (CAPM model) and Fama-French 5 factor separately. 
'

get_alpha_t_ratios <- function(returns){
    returns <- ret.x
    summary(returns)
    column_names = colnames(returns)
    stock_names = head(column_names, -2)
    market_premium = tail(column_names, 2)[1]
    date_column_name = tail(column_names, 2)[2]
    attr_mat = matrix(nrow=length(stock_names), ncol=4)
    colnames(attr_mat) = c("alpha", "t_alpha", "beta", "t_beta")
    rownames(attr_mat) = stock_names
    
    for (i in 1: length(stock_names)){
        stock_name = stock_names[i]
        print(stock_name)
        data = returns[,c(stock_name, market_premium, date_column_name)]
        par(mfrow=c(1,1))
        plot(data[, -3], main="Scatter plot")    # scatter plot
        abline(lm(data[[stock_name]]~data[[market_premium]],data=data))
        
        
        ##Regression analysis
        fit<-lm(data[[stock_name]]~data[[market_premium]], data=data)
        summary(fit)
        
        attr_mat[stock_name, 'alpha'] = summary(fit)$coefficients["(Intercept)", "Estimate"]
        attr_mat[stock_name, 'beta'] = summary(fit)$coefficients["data[[market_premium]]", "Estimate"]
        
        attr_mat[stock_name, 't_alpha'] = summary(fit)$coefficients["(Intercept)", "t value"]
        attr_mat[stock_name, 't_beta'] = summary(fit)$coefficients["data[[market_premium]]", "t value"]
    }
    
    return(attr_mat['alpha', 't_alpha'])
}

# get list of alpha and t ratios of corresponding alphas.
alpha_t_ratios <- get_alpha_beta_t_value(na.omit(ret.x))

# create histogram of t ratios of alphas
hist(alpha_t_ratios[, 't_alpha'], xlab = "t-ratios", main = "Frequency distribution of Stock Alphas")


'4.	From the step 3, identify how many assets outperform significantly at 5% of alpha level.'
# Stock data contains  1 GSPC (Market premium) and 1 IRX
no_of_stock = length(stock_names) - 2
degree_of_freedom = no_of_stock - 2
significance_level = 5/100

t_ratio_at_sig_level = qt(p = 1 - significance_level, df = no_of_stock - 2)
print(paste("T ratio at sigficance level", significance_level, ":", t_ratio_at_sig_level))

t_ratio_out_performing = alpha_t_ratios[alpha_t_ratios[, 't_alpha'] > t_ratio_at_sig_level, 't_alpha']
print(paste("Number of assets outperform significantly at", significance_level * 100, "%", ":", length(t_ratio_out_performing)))
names(t_ratio_out_performing)





