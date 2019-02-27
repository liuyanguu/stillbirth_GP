
totalIndex_covar <- jags.data$totalIndex_covar

summary(mod.basic,pars=c(paste("mu_ct[", seq(1, totalIndex_covar), ",", t, "]", sep = "")))$summary[,c(4,6,8)]

df <- data.frame(summary(mod.basic)$summary)

get_plot_dat <- function(mod,jags.data,alpha){
  df <- data.frame(summary(mod)$summary)
#  df <- data.frame(mod$BUGSoutput$summary)
  colnames(df) <- c("mean","se_mean","sd","2.5%","25%","median","75%","97.5%","n.eff","Rhat")
  
  z.i <- df %>% mutate(paraname=rownames(df)) %>% 
    filter(substring(paraname,1,3)=="z_i")
  z.i <- z.i$median
  
  jd <- jags.data
  n <- jd$totalObs_SBR
  sigma.j<-rep(NA,5)
  sigma.j <- df %>% mutate(paraname=rownames(df)) %>% 
    filter(substring(paraname,1,7)=="sigma_j")
  sigma.j <- sigma.j$mean

  
  sigma.y <-rep(NA,n)
  sigma.y <- sigma.j[jd$getj_i]
  
  expy.i=exp(jd$y_i)
  logyhat.i<- rep(NA,n)
  logyhat.i[jd$getitrain_k] <- jd$y_i[jd$getitrain_k]-z.i
  yhat.i <-exp(logyhat.i)
  
  ci <- array(NA, c(n,2))
  ci[,2] <- logyhat.i+qnorm(alpha/2)*sigma.y
  ci[,1] <- logyhat.i-qnorm(alpha/2)*sigma.y
  expci <- exp(ci)
  
  # the data source that the data come from
  dat_source <- factor(SBR_input$n_context5, 
                       labels = c("High National Subroutine", 
                                  "Low National Subroutine", 
                                  "National Survey", 
                                  "Sub-national", 
                                  "Other"))
  
  
  inclusion <- rep(0,n)
  inclusion[jd$getitrain_k]=1
  res <- data.frame(y_i=expy.i, yhat.i, low = expci[,2], up=expci[,1], 
                    getc.i=jd$getc_i, year = SBR_input$year, 
                    Source = dat_source,inclusion)
  
  return(res)
}



GetCIs_adjp <- function(mod, jags.data, lower = 0.025, upper = 0.975, est.name ) {
  ## size of CIs
  percentiles <- c(lower, 0.5, upper)
  
  ## estimates for every country, quantile, year
  estimates_1 <- data.frame(summary(mod.basic)$summary)
  error= lower+1-upper
  jd<-jags.data
  rawdata <- get_plot_dat(mod ,jags.data=jd , alpha=error)
  CIs.tqc <- list()
  for (c in 1:jd$totalIndex_covar) {
    ## dataset1
    CIs.tqc[[c]] <- data.frame(jd$estyears, 
                               exp(estimates_1[paste0(est.name,"[", c, ",", seq(1, jd$yearLength), "]"),c(4,6,8)])) %>% 
      set_names(c("year", "lower_1", "median_1", "upper_1"))
    
    CIs.tqc[[c]] <- CIs.tqc[[c]] %>% 
      full_join(rawdata[jd$getc_i == c, ], by = 'year') %>% 
      mutate(country = jd$country_covar[c])
    
  }
  return(CIs.tqc)
}
