mod.basic <- readRDS(file ="output/mod.basic.rds")
mod.ar1 <- readRDS(file ="output/mod.ar.rds")

############cov check############
mod.basic.array <- as.array(mod.basic)

pdf_name4 <- paste0("fig/convergence_basic_stan.pdf")
pdf(pdf_name4, width = 40, height = 20)
par(mfrow=c(3,3))
traceplot(basic,pars=c("beta_r[1]"))
traceplot(basic,pars=c("beta_r[2]"))
traceplot(basic,pars=c("beta_r[3]"))

traceplot(basic,pars=c("beta_edu"))
traceplot(basic,pars=c("beta_gni"))
traceplot(basic,pars=c("beta_lbw"))
traceplot(basic,pars=c("beta_nmr"))
traceplot(basic,pars=c("beta_anc"))

traceplot(basic,pars=c("beta_dt2"))
traceplot(basic,pars=c("beta_dt3"))
traceplot(basic,pars=c("beta_dt4"))
traceplot(basic,pars=c("beta_dt5"))


traceplot(basic,pars=c("sigma_j[1]"))
traceplot(basic,pars=c("sigma_j[2]"))
traceplot(basic,pars=c("sigma_j[3]"))
traceplot(basic,pars=c("sigma_j[4]"))
traceplot(basic,pars=c("sigma_j[5]"))
dev.off()

pdf_name4 <- paste0("fig/convergence_AR1_stan.pdf")
pdf(pdf_name4, width = 40, height = 20)
par(mfrow=c(3,3))
traceplot(ar,pars=c("rho"))
traceplot(ar,pars=c("sigma_ar"))

traceplot(ar,pars=c("beta_r[1]"))
traceplot(ar,pars=c("beta_r[2]"))
traceplot(ar,pars=c("beta_r[3]"))

traceplot(ar,pars=c("beta_edu"))
traceplot(ar,pars=c("beta_gni"))
traceplot(ar,pars=c("beta_lbw"))
traceplot(ar,pars=c("beta_nmr"))
traceplot(ar,pars=c("beta_anc"))

traceplot(ar,pars=c("beta_dt2"))
traceplot(ar,pars=c("beta_dt3"))
traceplot(ar,pars=c("beta_dt4"))
traceplot(ar,pars=c("beta_dt5"))


traceplot(ar,pars=c("sigma_j[1]"))
traceplot(ar,pars=c("sigma_j[2]"))
traceplot(ar,pars=c("sigma_j[3]"))
traceplot(ar,pars=c("sigma_j[4]"))
traceplot(ar,pars=c("sigma_j[5]"))
dev.off()

###############est table################


print(basic,pars=c("beta_r[1]","beta_r[2]","beta_r[3]","beta_edu","beta_gni","beta_lbw","beta_nmr","beta_anc",
                       "beta_dt2","beta_dt3","beta_dt4","beta_dt5","sigma_j[1]","sigma_j[2]","sigma_j[3]","sigma_j[4]",
                       "sigma_j[5]"))
print(ar,pars=c("rho","sigma_ar","beta_r[1]","beta_r[2]","beta_r[3]","beta_edu","beta_gni","beta_lbw","beta_nmr","beta_anc",
                       "beta_dt2","beta_dt3","beta_dt4","beta_dt5","sigma_j[1]","sigma_j[2]","sigma_j[3]","sigma_j[4]",
                       "sigma_j[5]"))
basic.mcmc <- As.mcmc.list(mod.basic, permuted = TRUE) # return a list of arrays
basic.df <- as.data.frame(mod.basic)
###############################


rawdat <- data.frame(y_i = exp(jags.data$y_i), 
                     getc_i = jags.data$getc_i,
                     year = SBR_input$year, 
                     Source = factor(SBR_input$n_context5, 
                                     labels = c("High National Subroutine", 
                                                "Low National Subroutine", 
                                                "National Survey", 
                                                "Sub-national", 
                                                "Other") ))

# save the plots in a pdf
pdf_name <- paste0("fig/graphsCT_ar1&basic.pdf")
pdf(pdf_name, width = 8, height = 5)

### you should input a dataset_list inlcuding all your datasets and get different CIs
dataset_list <- list(basic.df)
estimates_name <- c('mu.ct')
GetCIs(dataset_list, estimates_name, lower = 0.025, upper = 0.975) %>% lapply(CheckEsts)

# pdf off
dev.off()



# get_plot_dat provide a data frame used for plot. 
# the data frame contains observed y_i, yhat.i=y_i-z.i, CI for bias adjusted point, validation or not
plot.dat <- get_plot_dat(mod=basic.mcmc,
                         jags.data = jags.data,
                         alpha=0.05)

#GetCIs_adjp returns a list of information from different country. The est.name is mean.
plot.list <- GetCIs_adjp(mod.ar1MVN, jags.data=jags.data ,est.name = "adyhat.ct" )


mod.ar1MVNVAL <- readRDS(file ="output/mod.ar1MVNVAL.rds")

# NO bias adjusted information, no validation information
pdf_name <- paste0("fig/CT_Nadj_Nval.pdf")
pdf(pdf_name, width = 8, height = 5)
GetCIs_adjp(mod.ar1MVN, jags.data=jags.data ,est.name = "adyhat.ct" )%>% lapply(Check,bias.adjust=F,do.validation=F)
dev.off()

# Bias adjusted infomration, no validation information
pdf_name <- paste0("fig/ar1_Yadj.pdf")
pdf(pdf_name, width = 8, height = 5)
GetCIs_adjp(mod.ar1MVN, jags.data=jags.data ,est.name = "adyhat.ct" )%>% lapply(Check,bias.adjust=T,do.validation=F)
dev.off()

pdf_name <- paste0("fig/w.pdf")
pdf(pdf_name, width = 8, height = 5)
GetCIs_adjp(mod.ar1MVN, jags.data=jags.data ,est.name = "mu.ct" )%>% lapply(Check,bias.adjust=T,do.validation=F)
dev.off()

# No bias adjusted information, do validation
pdf_name <- paste0("fig/CT_Nadj_Yval.pdf")
pdf(pdf_name, width = 8, height = 5)
GetCIs_adjp(mod.ar1MVNVAL, jags.data=jags.dataVAL ,est.name = "mu.ct" )%>% lapply(Check,bias.adjust=F,do.validation=T)
dev.off()

# Bias adjusted information, do validation
pdf_name <- paste0("fig/CT_Yadj_Yval.pdf")
pdf(pdf_name, width = 8, height = 5)
GetCIs_adjp(mod.basicVAL, jags.data=jags.dataVAL ,est.name = "mu.ct" )%>% lapply(Check,bias.adjust=T,do.validation=T)
dev.off()

basic.mcmc$BUGSoutput$summary[paste("mu.ct[", seq(1, totalIndex_covar), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]
