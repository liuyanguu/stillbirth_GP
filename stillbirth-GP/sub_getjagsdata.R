#############################################################
##########            Data import           #################
#############################################################
get_jagsdata <- function(dataset1 = SBR_input, dataset2 = national_covar , do.validation, staryear = 2000, endyear = 2015){
  
  #############################################################
  ##########        Data clean for jags       #################
  #############################################################
  
  # filter SBR_input. Keep the data of year 2000-2015
  SBR_input<-filter(SBR_input, year>= startyear & year <= endyear)
  
  #  list of country, its matching region, and a index 
  countryRegionList <- national_covar[,c(1,8)] %>% distinct()  
  CountryRegionList <- mutate(countryRegionList,country_idx=as.numeric(factor(countryRegionList$iso3)))
  
  # getc.i: the country index of ith observation
  getc.i<- merge(SBR_input,CountryRegionList,by="iso3")$country_idx
  
  # gett.i: the time index of ith obervation
  estyears <- seq(startyear, endyear)
  gett.i <- SBR_input$year-estyears[1]+1
  
  # getr.c: the region given country_idx
  getr.c <- countryRegionList$shmdg2
  
  # Store each covariate infomration in c*t matrix 
  yearLength <- max(gett.i)+1
  #function for creating matrix
  numcoun <- length(countryRegionList$iso3)
  country_covar <- as.vector(countryRegionList$iso3)
  
  
  ## 5 covariates
  edu_matrix = covarMatrix("mean_edu")
  anc_matrix = covarMatrix("anc4")
  nmr_matrix = covarMatrix("ln_nmr")
  gni_matrix = covarMatrix("ln_gni")
  lbw_matrix = covarMatrix("ln_lbw")
  
  
  #creating dummies for datatype
  # getj.i: the datatype for ith obs
  dummy_datatype2.i <- ifelse(SBR_input$n_context5==2,1,0)
  dummy_datatype3.i <- ifelse(SBR_input$n_context5==3,1,0)
  dummy_datatype4.i <- ifelse(SBR_input$n_context5==4,1,0)
  dummy_datatype5.i <- ifelse(SBR_input$n_context5==5,1,0)
  getj.i <-SBR_input$n_context5
  
  
  totalObs_SBR = nrow(SBR_input)
  totalRegion = length(unique(countryRegionList$shmdg2))
  totalIndex_covar = length(countryRegionList$iso3)
  
  y.i<- SBR_input$ln_sbr
  
  #######################################################
  ################ Jags modelS ###########################
  #######################################################
  
  jags.data<- list(  y.i = y.i, getj.i=getj.i, getc.i = getc.i, getr.c = getr.c, gett.i= gett.i, 
                     dummy_datatype2.i=dummy_datatype2.i, dummy_datatype3.i=dummy_datatype3.i,dummy_datatype4.i=dummy_datatype4.i,dummy_datatype5.i = dummy_datatype5.i,
                     totalObs_SBR = totalObs_SBR, totalIndex_covar=totalIndex_covar, totalRegion= totalRegion,estyears=estyears,country_covar=country_covar,
                     yearLength= yearLength, edu_matrix= edu_matrix, gni_matrix=gni_matrix, lbw_matrix= lbw_matrix, nmr_matrix=nmr_matrix, anc_matrix= anc_matrix)
  
  if (!do.validation){
    # all observations are in the training set
    jags.data$getitrain.k <- seq(1, totalObs_SBR)
  } else {
    # this is one particular type of validation, 
    # leaving out the most recent data point in all countries with at least 2 observations
    indiceslastobs <- list()
    for (c in 1:totalIndex_covar){
      if (sum(getc.i==c)>2){
        indiceslastobs[[c]] <- which(gett.i==max(gett.i[getc.i==c]) & getc.i==c)   
      }
    }
    jags.data$getitrain.k <- seq(1,totalObs_SBR)[!is.element(seq(1,totalObs_SBR), unlist(indiceslastobs))]
  }
  
  jags.data$ntrain <- length(jags.data$getitrain.k)
  
  return(jags.data)
}
