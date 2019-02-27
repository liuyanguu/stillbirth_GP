#' Write a JAGS model to fit AR(1) model
#'
#' Write a JAGS model to fit AR(1) model, with or without time trend
#'
#' @param time.trend if `TRUE` a linear time trend is estimated. Default 'FALSE'.
#' @param file.name name of file to be saved. Must be a `.txt` file. Default "model/SBmodel.txt".
#' @export
#' @return A text file that contains a JAGS model
#' @examples
#' time.trend <- TRUE
#' 
writeModel <- function( # Write JAGS model out as a .txt file
  time.trend = F,
  file.name = "model/SBmodel.txt"
)
{
  if(time.trend){
    delta_getitrain <- paste("delta.ct[getc.i[getitrain.j[j]],gett.i[getitrain.j[j]]]")
  } else{
    delta_getitrain <- paste("0")
  }

  # start txt-file:
  cat("model{  ", file = file.path(file.name), fill = T, append = FALSE)
    cat("
        for (j in 1:ntrain){
        y.i[getitrain.j[j]] ~ dnorm(mu.ct[getc.i[getitrain.j[j]], gett.i[getitrain.j[j]]]+z.i[getitrain.j[j]]+", delta_getitrain, ", tauy)
        yhat.i[getitrain.j[j]]<- mu.ct[getc.i[getitrain.j[j]],gett.i[getitrain.j[j]]]+z.i[getitrain.j[j]]+", delta_getitrain
        , file = file.path(file.name), fill = T, append = T)

  cat("z.i[getitrain.j[j]] <- beta_datatype2*dummy_datatype2.i[getitrain.j[j]]+
      beta_datatype3*dummy_datatype3.i[getitrain.j[j]]+
      beta_datatype4*dummy_datatype4.i[getitrain.j[j]]+
      beta_datatype5*dummy_datatype5.i[getitrain.j[j]]} #end j loop
      ", file = file.path(file.name), fill = T, append = T)
  
  z_i <- paste(" beta_datatype2*dummy_datatype2.i[i]+
                 beta_datatype3*dummy_datatype3.i[i]+
                 beta_datatype4*dummy_datatype4.i[i]+
                 beta_datatype5*dummy_datatype5.i[i]")
  
  if(time.trend){
    delta_ci <- paste("delta.ct[getc.i[i], gett.i[i]]")
  } else{
    delta_ci <- paste("0")
  }
  
    cat("
        for (i in 1:totalObs_SBR){
        yrep.i[i] ~ dnorm(mu.ct[getc.i[i], gett.i[i]]+", z_i,"+", delta_ci, ", tauy)
        loglike.i[i] <- logdensity.norm(y.i[i], mu.ct[getc.i[i], gett.i[i]]+", z_i,"+", delta_ci, ", tauy)} #end i loop
        ", file = file.path(file.name), fill = T, append = T)
  
  matrix_ct <- paste("beta2*edu_matrix[country,t]+
                    beta3*anc_matrix[country,t]+
                     beta4*lbw_matrix[country,t]+
                     beta5*nmr_matrix[country,t]+
                     beta6*gni_matrix[country,t])") 
  cat("
      for (country in 1:totalIndex_covar)
{
  a.c[country] ~ dnorm(a.r[getr.c[country]], tau.c)
  ", file = file.path(file.name), fill = T, append = T)
  
  if(time.trend){
    cat("
        delta.ct[country,1:yearLength] ~ dmnorm(mu.g[country,1:yearLength], Sigma.inv[1:yearLength,1:yearLength])
        ", file = file.path(file.name), fill = T, append = T)
  }
  cat("for( t in 1:yearLength)
        {
      mu.ct[country,t] <- (a.c[country] +", matrix_ct,""
, file = file.path(file.name), fill = T, append = T )
  
  if(time.trend){
    cat("
adyhat.ct[country, t] =  mu.ct[country,t] + delta.ct[country, t]
mu.g[country,t]<-0", file = file.path(file.name), fill = T, append = T)}
  
  cat("
}#end t loop
        
}#end country loop
      ",file = file.path(file.name), fill = T, append = T)

  if(time.trend){
    cat("
        for(t in 1:yearLength){
        Sigma[t,t] <- pow(tau.g,-1)/(1-rho^2) + 0.00001 
        
        for(j in (t+1):yearLength) {
        Sigma[t,j]<- pow(tau.g,-1)/(1-rho^2)*pow(rho,j-t) 
        Sigma[j,t]<- Sigma[t,j]
        } #End j loop
        } #End t loop 
        
        Sigma.inv[1:yearLength,1:yearLength] <- inverse(Sigma[1:yearLength,1:yearLength])
        rho ~ dnorm(0,5)T(0,1)
        #tau.stat <-  (1-pow(rho,2))/pow(sigma,2)
        #tau <- pow(sigma,-2)
        tau.g<-pow(sigma.g,-2)
        sigma.g ~ dunif(0,10)
        
        ", file = file.path(file.name), fill = T, append = T)
  }
  cat("
      for (r in 1:totalRegion)
      {
      a.r[r] ~ dnorm(beta0, 0.001)
      } 

      tauy <- pow(sdy,-2)
      sdy~ dunif (0,5)
      tau.c <- pow(sd.c,-2)
      sd.c ~ dunif(0,5)
      beta_datatype2 ~ dnorm(0,1/1000)
      beta_datatype3 ~ dnorm(0,1/1000)
      beta_datatype4 ~ dnorm(0,1/1000)
      beta_datatype5 ~ dnorm(0,1/1000)
      beta0 ~ dnorm(0,0.001)
      beta2 ~ dnorm(0,0.001)
      beta3 ~ dnorm(0,0.001)
      beta4 ~ dnorm(0,0.001)
      beta5 ~ dnorm(0,0.001)
      beta6 ~ dnorm(0,0.001)
      ", file = file.path(file.name), fill = T, append = T)
  # close model file
  cat("} # end model ", file = file.path(file.name), fill = T, append = T)
  cat(paste0("Model file written to ", getwd(),"/",file.name, "\n"))
  return(invisible())
  }

