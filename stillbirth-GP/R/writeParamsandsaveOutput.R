#' Write a JAGS model to fit AR(1) model
#'
#' Write a JAGS model to fit AR(1) model, with or without time trend
#'
#' @param time.trend if `TRUE` a linear time trend is estimated. Default is 'FALSE'.
#' @param input.file.name name of model file. Must be a `.txt` file. Default is "model/SBmodel.txt".
#'                        Must be the same as the second argument in writeModel() for the same model.
#' @param output.file.name name of output file. Must be a '.R' file. Default is "model_basic.R".
#' @export
#' @return A text file that contains a JAGS model
#' @examples
#' time.trend <- TRUE
#' 
getParamsandOutput<- function( # get parameters and jags outputs
  time.trend = F,
  input.file.name = "model/SBmodel.txt",
  output.file.name = "model_basic.R"
)
  {
  if(time.trend){
    params <- paste(",\"rho\",\"delta.ct\", \"adyhat.ct\", \"sigma.g\")")
  } else{
    params <- paste(")")
  }
  
  # start R-file:
  cat("parnames.to.save<-c( ", file = file.path(output.file.name), fill = T, append = FALSE)
  cat(" \"mu.ct\",\"z.i\",\"a.r\",\"sdy\",\"beta0\",\"beta2\",\"beta3\",\"beta4\",\"beta5\",\"beta6\",\"beta_datatype2\",\"beta_datatype3\",

        \"beta_datatype4\",\"beta_datatype5\",\"yhat.i\",\"yrep.i\",\"loglike.i\"", params
      , file = file.path(output.file.name), fill = T, append = T)
  
  if(time.trend){
    output.name <- paste("mod.ar1MVN.rds")
    mod.name<-paste("mod.ar1MVN")
  } else{
    output.name <- paste("mod.basic.rds")
    mod.name<-paste("mod.basic")
  }
  
    cat(paste0(mod.name,"<-jags.parallel(data = jags.data,
                                  parameters.to.save=parnames.to.save, 
                                  model.file =\"", input.file.name, "\", n.chains = 3, n.iter = 12000, n.burnin = 3000, n.thin=9)
    saveRDS(",mod.name,", file =\"output/", output.name, "\")")
        , file = file.path(output.file.name), fill = T, append = T)
 
  # close file
  cat(" # end file ", file = file.path(output.file.name), fill = T, append = T)
  cat(paste0("Model file written to ", getwd(),"/",output.file.name, "\n"))
  return(invisible())

}

