#' Check_est_table
#'
#' @param mod model name
#' @param est.list a char vector of interested estimates
#' @param want a char vecthor of interested summary statistics
#' @param round number of digits
#'
#' @return estimates in a table with interested summary statistics
#'
#' @examples showed in master.rmd
check_est_table <- function(mod,est.list,want=c("mean", "sd", "2.5%", "97.5%", "n.eff", "Rhat"),round){
    t<-c()
  for(est in est.list){
    t<-rbind(t,round(mod$BUGSoutput$summary[paste(est), c(paste(want))],round))
  }
    row.names(t)<-est.list
    return(t)
}




#' get_plot_dat
#'
#' @param mod model name
#' @param jags.data jags.data used for model 
#' @param alpha type 1 error
#'
#' @return one data frame with observation.i, yhat.i= obs.i-z.i(and its CI), year, country and source type.
#'
get_plot_dat <- function(mod,jags.data,alpha){
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
  
  return(res)
}

#' Title
#'
#' @param mod model name
#' @param jags.data jags.data used for model
#' @param lower lower bound
#' @param upper upper bound
#' @param est.name the estimate you want to plot
#'
#' @return a list for each country with its information 

GetCIs_adjp <- function(mod, jags.data, lower = 0.025, upper = 0.975, est.name ) {
  ## size of CIs
  percentiles <- c(lower, 0.5, upper)
  
  ## estimates for every country, quantile, year
  estimates_1 <- mod$BUGSoutput$summary
  error= lower+1-upper
  jd<-jags.data
  rawdata <- get_plot_dat(mod ,jags.data=jd , alpha=error)
  CIs.tqc <- list()
  for (c in 1:jd$totalIndex_covar) {
    ## dataset1
    CIs.tqc[[c]] <- data.frame(jd$estyears, 
                               exp(estimates_1[paste0(est.name,"[", c, ",", seq(1, jd$yearLength), "]"), 
                               paste0(100 * percentiles, "%")])) %>% 
                               set_names(c("year", "lower_1", "median_1", "upper_1"))

    CIs.tqc[[c]] <- CIs.tqc[[c]] %>% 
     full_join(rawdata[jd$getc.i == c, ], by = 'year') %>% 
      mutate(country = jd$country_covar[c])
    
  }
  return(CIs.tqc)
}



#CIs.tq<- GetCIs_adjp(mod=mod.basicVAL, jags.data=jags.datacv, lower = 0.025, upper = 0.975, "mu.ct" )
#GetCIs_adjp(mod=mod.basic, jags.data=jags.data, lower = 0.025, upper = 0.975, "mu.ct" )



#' Title
#'
#' @param CIs.tq the element returned from Gets_adjp(mod, jags.data, lower = 0.025, upper = 0.975, "mu.ct" )
#' @param bias.adjust TRUE or False, add bias adjusted point and unc or not
#' @param do.validation TRUE or False, present the left out point in validation if do.validation is TRUE
#'
#' @return plot

Check <- function(CIs.tq,bias.adjust=T,do.validation=T){
  estyears<-seq(2000,2015)
  ## dataset includes points
  point_dat <- CIs.tq %>% dplyr::select(year, y_i, yhat.i, up, low , getc.i, Source,inclusion) %>% 
    drop_na(y_i) %>% filter(inclusion==1)
  point_val <- CIs.tq %>% dplyr::select(year, y_i,  getc.i,inclusion) %>% 
    filter(inclusion==0)
  ## plot title
  plot_title <- unique(CIs.tq[, "country"])
  ## plot dataset1
  est_plot <- ggplot() +
    theme_bw() +
    geom_line(aes(x = year, y = median_1), size = 2,data = CIs.tq) +
    geom_ribbon(aes(x = year, ymin = lower_1, ymax = upper_1), alpha = 0.3,data = CIs.tq) +
    geom_point(aes(x = year, y = y_i, colour = Source), size =3, data = point_dat) +
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
           geom_point(aes(x = year, y = yhat.i),shape = 17, size =3,data = point_dat)+
   
           geom_errorbar(aes(x = year, ymin = low, ymax = up),col="dark grey",data = point_dat)
           }
if(do.validation==T){
 est_plot<- est_plot+
            geom_point(aes(x = year, y = y_i),col="red",shape = 17, size =3,data = point_val)
}  
  return(est_plot)
}


