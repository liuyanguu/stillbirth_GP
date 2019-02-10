#  BindDat
#' Extract estimates and CIs for one country given by different models
#'
#' @param estimates_list A list includes all the estimates for all countries
#' @param estimates_name A vector includes all the names of the estimates
#' @param c Country number
#' @param percentiles The bound of credible sets
#'
#' @return All estimates for one country
#'
#' @examples
#' estimates_list <- list(estimates1, estimates2)
#' estimates_name <- c('adyhat.ct', 'mu.ct')
#' percentiles <- c(0.025, 0.5, 0.975)
#' BindDat(estimates_list, estimates_name, 1, percentiles)
BindDat <- function(estimates_list, estimates_name, c, percentiles){
  ## the number of datasets
  nEst <- length(estimates_list)
  ## column names
  CIs.name <- paste(c('lower', 'median', 'upper'), rep(1:nEst, each = 3), sep = '_')
  ## get the credible intervals for a country
  CIs.c <- data.frame(jags.data$estyears)
  for (i in 1:nEst){
    CIs.c <- data.frame(CIs.c,
                        exp(estimates_list[[i]][paste0(estimates_name[i], "[", c, ",", 
                                                       seq(1, jags.data$yearLength), "]"), 
                                                paste0(100 * percentiles, "%")])
    )
  }
  ## set names of the columns
  CIs.c <- CIs.c %>% set_names(c("year", CIs.name))
  return(CIs.c)
}
#  GetCIs
#' Get the estimates and the CIs for each country in 2000-2015 and combine with the raw data
#'
#' @param dataset_list A list includes all datasets you want to get estimates from
#' @param lower Lower bound of the CIs, can be 0.025, 0.25
#' @param upper Upper bound of the CIs, can be 0.75, 0.975.
#'
#' @return A list includes all the estimates, CIs, and raw data for 195 countries in 2000-2015
#'
#' @examples
#' dataset_list <- list(dataset1, dataset2)
#' GetCIs(dataset_list, lower = 0.25, upper = 0.75)
GetCIs <- function(dataset_list, estimates_name, lower = 0.025, upper = 0.975) {
  ## size of CIs
  percentiles <- c(lower, 0.5, upper)
  ## the number of dataset you have
  nDataset <- length(dataset_list)
  ## extract the data we need from the datasets
  estimates_list <- list()
  for (i in 1:nDataset){
    estimates_list[[i]] <- dataset_list[[i]]$BUGSoutput$summary
  }
  ## merge the datasets
  CIs.tqc <- list()
  for (c in 1:jags.data$totalIndex_covar) {
    CIs.tqc[[c]] <-BindDat(estimates_list, estimates_name, c, percentiles)
    ## merge rawdata
    CIs.tqc[[c]] <- CIs.tqc[[c]] %>% 
      full_join(rawdat[jags.data$getc.i == c, ], by = 'year') %>% 
      mutate(country = jags.data$country_covar[c])
  }
  return(CIs.tqc)
}

# CheckEsts
#' Plot the estimates and the CIs for one of 195 countries
#' 
#' @param CIs.tq An element of the list given by GetCIs function
#'
#' @return A ggplot of the estimates and the CIs and the raw data points from different data sources
#'
#' @examples
#' dataset_list <- list(dataset1, dataset2)
#' GetCIs(dataset_list, lower = 0.025, upper = 0.975) %>% lapply(CheckEsts)
CheckEsts <- function(CIs.tq){
  ## dataset only includes points
  point_dat <- CIs.tq %>% 
    dplyr::select(year, y_i, getc.i, Source) %>% 
    drop_na(y_i)
  ## plot title
  plot_title <- unique(CIs.tq[, "country"])
  ## ggplot basic 
  est_plot <- 
    ggplot(data = CIs.tq) +
    theme_bw() +
    coord_cartesian(ylim = c(0, 60)) +
    scale_x_continuous(name = 'Time', breaks = jags.data$estyears, minor_breaks = NULL) +
    scale_y_continuous(name = 'Stillbirth Rate') +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  ## plot dataset
  n <- (ncol(CIs.tq)-5)/3
  for (i in 1:n){
    est_plot <- est_plot +
      geom_line(aes_string(x = 'year', y = paste0('median_', i)), size = 2, col = i) +
      geom_ribbon(aes_string(x = 'year', ymin = paste0('lower_', i),
                             ymax = paste0('upper_', i)), alpha = 0.3, fill = i)
  }
  ## add points
  est_plot <- est_plot +
    geom_point(aes(x = year, y = y_i, col = Source, shape = Source), size = 3, data = point_dat)
  return(est_plot)
}