library(readxl)
library(magrittr)
library(ggplot2)
library(dplyr)

data = read_excel('JL.xlsx')
training_months = 180
runs = 10000

ret = as.matrix(dplyr::select(data, legal, bpet, pey, Invesco, igukpia, vnq, 
                              msci, ftse250, on, sp500, er_small))

w = c(0.0871, 0.03712, 0.01682, 0572, 0.00416, 0.0998, 0.02905, 0.02905, 0.3347, 0.1592, 
      0.02905) 

covar_mat = as.matrix(cov(ret))

mean = colMeans(ret)
meanext = matrix(rep(mean, training_months), nrow = 11)
p_retm = matrix(0, training_months, runs)



set.seed(69420)
for (i in 1:runs) {
  Z = matrix ( rnorm( dim(ret)[2] * training_months ), ncol = training_months )
  L = t( chol(covar_mat))
  monthly_returns = meanext + L %*% Z
  p_ret = cumprod( w %*% monthly_returns + 1 )
  p_retm[,i] = p_ret;
}


x_axis = rep(1:training_months, runs)
y_axis = as.vector(p_retm-1)
plot_data = data.frame(x_axis, y_axis)
ggplot(data = plot_data, aes(x = x_axis, y = y_axis)) + geom_path(col = 'dark blue', size = 0.1) +
  xlab('Months') + ylab('Portfolio Returns') + 
  ggtitle('Simulated Portfolio Returns in 15 years')+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

Avg= mean(p_retm[180,]-1)
SD = sd(p_retm[180,]-1)
Median = median(p_retm[180,]-1)
print(c(Avg,SD,Median))

Avg_CI = quantile(p_retm[180,]-1, c(0.001, 0.1, 0.68, 0.95, 0.997))
print(Avg_CI)

Min_ret = min(p_retm[180,]-1)
print(Min_ret)
