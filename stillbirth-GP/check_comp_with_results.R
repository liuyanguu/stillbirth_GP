sbr_result <- read.csv("input/SBR results.csv")
sbr_result <- sbr_result%>% rename(iso3=Isocode)

countryRegionList <- national_covar[,c(1,2,8)] %>% distinct()  
CountryRegionList <- mutate(countryRegionList,country_idx=as.numeric(factor(countryRegionList$iso3)))

# getc.i: the country index of ith observation
getc.i.comp<- merge(sbr_result,CountryRegionList,by="iso3")$country_idx

SBR_result <- data.frame(country=sbr_result$iso3,
                         year=sbr_result$Year,
                         SBR.est.comp=sbr_result$Stillbirth.Rate,
                         lower.comp=sbr_result$Lower.uncertainty.number.of.stillbirths/(sbr_result$Number.of.livebirths+sbr_result$Number.of.stillbirths)*1000,
                         upper.comp=sbr_result$Upper.uncertainty.number.of.stillbirths/(sbr_result$Number.of.livebirths+sbr_result$Number.of.stillbirths)*1000,
                         getc.i=getc.i.comp)



  mod <- mod.ar1MVN
  mod2 <- mod.basic
  alpha <- 0.05
  df <- data.frame(mod$BUGSoutput$summary)
  colnames(df) <- c("mean","sd","2.5%","25%","median","75%","97.5%","Rhat","n.eff")
  
  z.i <- df %>% mutate(paraname=rownames(df)) %>% 
    filter(substring(paraname,1,3)=="z.i")
  z.i <- z.i$median
  
  jd <- jags.data
  n <- jd$totalObs_SBR
  sigma.j<-rep(NA,5)
  sigma.j[1] <- mod$BUGSoutput$summary["sdy.j[1]","mean"]
  sigma.j[2] <- mod$BUGSoutput$summary["sdy.j[2]","mean"]
  sigma.j[3] <- mod$BUGSoutput$summary["sdy.j[3]","mean"]
  sigma.j[4] <- mod$BUGSoutput$summary["sdy.j[4]","mean"]
  sigma.j[5] <- mod$BUGSoutput$summary["sdy.j[5]","mean"]
  
  sigma.y <-rep(NA,n)
  sigma.y <- sigma.j[jd$getj.i]
  
  expy.i=exp(jd$y.i)
  logyhat.i<- rep(NA,n)
  logyhat.i[jd$getitrain.k] <- jd$y.i[jd$getitrain.k]-z.i
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
  inclusion[jd$getitrain.k]=1
  res <- data.frame(y_i=expy.i, yhat.i, low = expci[,2], up=expci[,1], 
                    getc.i=jd$getc.i, year = SBR_input$year, 
                    Source = dat_source,inclusion)
  
  



lower = 0.025
upper = 0.975
  ## size of CIs
  percentiles <- c(lower, 0.5, upper)
  est.name <- "adyhat.ct"
  est.name2 <- "mu.ct"
  ## estimates for every country, quantile, year
  estimates_1 <- mod$BUGSoutput$summary
  estimates_2 <- mod2$BUGSoutput$summary
  error= lower+1-upper
  jd<-jags.data
  rawdata <- res
  CIs.tqc <- list()
  LOESS.CIs.tqc <- list()
  basic.CIs.tqc <- list()
  CIs.with.basic.tqc <- list()
  CIs.with.loess.tqc <- list()
  for (c in 1:jd$totalIndex_covar) {
    ## dataset1
    CIs.tqc[[c]] <- data.frame(jd$estyears, 
                               exp(estimates_1[paste0(est.name,"[", c, ",", seq(1, jd$yearLength), "]"), 
                                               paste0(100 * percentiles, "%")])) %>% 
      set_names(c("year", "lower_1", "median_1", "upper_1"))
    
    CIs.tqc[[c]] <- CIs.tqc[[c]] %>% 
      full_join(rawdata[jd$getc.i == c, ], by = 'year') %>% 
      mutate(country = jd$country_covar[c],country_name = jd$country_name[c],mod="AR1")
    #%>% 
    #  left_join(SBR_result,by=c('year',"country"),all.x = F)
    
    LOESS.CIs.tqc[[c]] <- data.frame(year = jd$estyears,
                                     lower_1 = SBR_result$lower.comp[SBR_result$getc.i==c],
                                     median_1 = SBR_result$SBR.est.comp[SBR_result$getc.i==c],
                                     upper_1 = SBR_result$upper.comp[SBR_result$getc.i==c])
    LOESS.CIs.tqc[[c]] <- LOESS.CIs.tqc[[c]] %>% 
                          mutate(y_i=NA,yhat.i=NA,low=NA,up=NA,getc.i=NA,Source=NA,inclusion=NA,
                                 country=jd$country_covar[c],
                                 country_name= jd$country_name[c],
                                 mod="OLD LOESS")
    
   basic.CIs.tqc[[c]] <- data.frame(jd$estyears, 
                               exp(estimates_2[paste0(est.name2,"[", c, ",", seq(1, jd$yearLength), "]"), 
                                               paste0(100 * percentiles, "%")])) %>% 
      set_names(c("year", "lower_1", "median_1", "upper_1"))
    
    basic.CIs.tqc[[c]] <- basic.CIs.tqc[[c]] %>% 
      full_join(rawdata[jd$getc.i == c, ], by = 'year') %>% 
      mutate(country = jd$country_covar[c],country_name = jd$country_name[c],mod="basic")
    

    
    CIs.with.basic.tqc[[c]] <- rbind(CIs.tqc[[c]],basic.CIs.tqc[[c]])
    CIs.with.loess.tqc[[c]] <- rbind(CIs.tqc[[c]],LOESS.CIs.tqc[[c]])
    
      }



Check.comp <- function(CIs.tq,bias.adjust=T,do.validation=T){
  estyears<-seq(2000,2015)
  ## dataset includes points
  point_dat <- CIs.tq %>% dplyr::select(year, y_i, yhat.i, up, low , getc.i, Source,inclusion) %>% 
    drop_na(y_i) %>% filter(inclusion==1)
  point_val <- CIs.tq %>% dplyr::select(year, y_i,  getc.i,inclusion) %>% 
    filter(inclusion==0)
  ## plot title
  plot_title <- unique(CIs.tq[, "country_name"])
  ## plot dataset1
  est_plot <- ggplot() +
    theme_bw() +
    
    geom_line(aes(x = year, y = median_1,colour =mod),data = CIs.tq) +
    geom_ribbon(aes(x = year, ymin = lower_1, ymax = upper_1,colour=mod),alpha=0.3,data = CIs.tq) +
    scale_fill_manual(values=c("grey", "pink")) +
    
    geom_point(aes(x = year, y = y_i, shape = Source), size =2, data = point_dat) +
    scale_x_continuous(name = 'Time', breaks = estyears, minor_breaks = NULL) +
    

    
    scale_y_continuous(name = 'Stillbirth Rate') +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  
  if(bias.adjust==T){
    est_plot<- est_plot+    
      geom_point(aes(x = year, y = yhat.i),shape = 1, size =3,data = point_dat)+
      
      geom_errorbar(aes(x = year, ymin = low, ymax = up),col="dark grey",data = point_dat)
  }
  if(do.validation==T){
    est_plot<- est_plot+
      geom_point(aes(x = year, y = y_i),col="red",shape = 17, size =3,data = point_val)
  }  
  return(est_plot)
}

