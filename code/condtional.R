# Conditional Value at Risk  Distributions


pacman::p_load(tidyverse,ggplot2,dpylr,magrittr)

readxl::read_excel('data/bovespa.xlsx')  -> bovespa_index

bovespa_index %<>% mutate(returns_log = log(Close/lag(Close))) %>% drop_na()

bovespa_index %>% summarise(lower_tail = quantile(probs=c(.05),returns_log)) %>% pull() -> quantile_lower

bovespa_index %>% filter(returns_log <= quantile_lower) -> lower_tail_bovespa



bovespa_index %<>% mutate(lower_tail = if_else(returns_log <= quantile_lower,1,0))

ggplot(bovespa_index,aes(x=Date,returns_log)) + geom_line() + theme_classic()+
  xlab('') + ylab('log returns') + ggtitle('Ibovespa Index Returns') 
  

ggplot(bovespa_index, aes(x=returns_log,fill=as.factor(lower_tail))) +geom_histogram(bins =50, 
                                                                                    alpha = 0.4,) + 
  theme_light() +  xlab('') + ylab('log returns') + ggtitle('Ibovespa Index Returns Distriburion') +
  scale_fill_viridis_d(guide=FALSE)+guides()
require(gamlss)

size_sample<- dim(bovespa_index)[1]

fit_dists_aic <- fitDist(lower_tail_bovespa$returns_log,k = 6)

fit_dists_bic <-fitDist(lower_tail_bovespa$returns_log,k = log(size_sample))

#Table
fit_dists_aic$fits
fit_dists_bic$fits




min_log_returns<- min(lower_tail_bovespa$returns_log); max_log_returns<- max(lower_tail_bovespa$returns_log);




x_hat <- mean(lower_tail_bovespa$returns_log) #gaussian
sd_hat <- sd(lower_tail_bovespa$returns_log) # Gaussian

calculate_mean_ST2 <-function(mu,sigma,tau,nu){
  
  EZ <- ((nu*sqrt(tau))* gamma((tau-1)/2)) /( sqrt(1+nu^2) * sqrt(pi)*gamma(tau/2))
   mean <- mu+sigma*EZ
  return(mean)
}

mean_ST2 <- calculate_mean(mu = fit_dists_aic$mu, sigma =fit_dists_aic$sigma,tau = fit_dists_aic$tau,
                       nu = fit_dists_aic$nu)

# ST2
ggplot(lower_tail_bovespa, aes(x=returns_log)) +geom_histogram(bins = 15, alpha = 0.4) + 
 
  stat_function(fun=dST2,args = list(mu = fit_dists_aic$mu, 
                                     sigma = (fit_dists_aic$sigma),
                         nu = fit_dists_aic$nu, tau = (fit_dists_aic$tau)), colour = "coral3", size =1) +
      theme_light() + ggtitle('Left Tail Distribution Ibovespa',subtitle = 'Skew T - Type II' ) +
  xlab('log  returns') + 
  geom_vline(xintercept = mean_ST2,size= .8, colour = "darkgreen")


# Gaussian plot

ggplot(lower_tail_bovespa, aes(x=returns_log)) +geom_histogram(bins = 15, alpha = 0.4) + 
  stat_function(fun=dnorm,args = 
                  list(mean = x_hat,sd = sd_hat), colour = "aquamarine3",size=1) +
  geom_vline(xintercept = x_hat, colour = "blueviolet",size= .8)+ theme_light()+
  ggtitle('Left Tail Distribution Ibovespa',subtitle = 'Gaussian Distribution' ) +
  geom_vline(xintercept = mean_ST2,size= .8, colour = "darkgreen") +
  xlab('log  returns') 






