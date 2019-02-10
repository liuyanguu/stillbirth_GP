
# get_plot_dat provide a data frame used for plot. 
# the data frame contains observed y_i, yhat.i=y_i-z.i, CI for bias adjusted point, validation or not
plot.dat <- get_plot_dat(mod=mod.basic,
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



