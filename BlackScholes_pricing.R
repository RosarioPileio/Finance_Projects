library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(anytime)

options(scipen=999)

options = read.csv('tsla_eod_202201.csv')

df = as.data.frame(options)
df = na.omit(df)
df = dplyr::select(df, -c('X.QUOTE_UNIXTIME.', 'X.EXPIRE_UNIX.'))

df$Time_to_maturity = df$X.DTE./365

df$X.C_IV. = df$X.C_IV./100
df$X.P_IV. = df$X.P_IV/100

type = c('C', 'P')
df$type = sample(type)

df$rf = 0.103


BlackScholes = function(St, K, r, t, vol, type) {
    
  d1 = ((log(St/K) + (r + vol^2/2)*t)) / (vol*sqrt(t))
  d2 = d1 - vol*sqrt(t)
    
  price_c = St*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  price_p = (K*exp(-r*t))*pnorm(-d2) - St*pnorm(-d1)
  
  ifelse(type=='C', price_c, price_p)
}

df$value = BlackScholes(df$X.UNDERLYING_LAST.,df$X.STRIKE.,df$rf, df$Time_to_maturity,
                        ifelse(df$type == 'P', df$X.P_IV., df$X.C_IV.), df$type)
