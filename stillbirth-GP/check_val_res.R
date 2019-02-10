#----
# model fit, residuals
mcmc.array.mod<-mod.ar1MVNVAL$BUGSoutput$sims.array

totalObs_SBR <- jags.dataVAL$totalObs_SBR
getitest <- setdiff(seq(1,jags.dataVAL$totalObs_SBR), jags.dataVAL$getitrain.k)

y.i <- jags.data$y.i


errors <- jags.dataVAL$y.i[getitest] - mod.ar1MVNVAL$BUGSoutput$summary[paste0("yrep.i[",getitest,"]"), "50%"]

sd <- mod.ar1MVNVAL$BUGSoutput$summary[paste0("sdy.j[",getj.test,"]"), "mean"]

st.errors <- errors/sd
##individual observation function
getc.i=jags.dataVAL$getc.i
gett.i=jags.dataVAL$gett.i
getr.c=jags.dataVAL$getr.c
getj.i=jags.dataVAL$getj.i

x.i<-function(varmatrix){
  varname.i<-rep(NA,totalObs_SBR)
  for (i in 1:totalObs_SBR) {varname.i[i]<-varmatrix[getc.i[i],gett.i[i]]}
  varname.i
}

edu.i<-x.i(jags.dataVAL$edu_matrix)
anc.i<-x.i(jags.dataVAL$anc_matrix)
lbw.i<-x.i(jags.dataVAL$lbw_matrix)
nmr.i<-x.i(jags.dataVAL$nmr_matrix)
gni.i<-x.i(jags.dataVAL$gni_matrix)
getr.i <- getr.c[getc.i]
getj.i

edu.test <- edu.i[getitest]
anc.test <- anc.i[getitest]
lbw.test <- lbw.i[getitest]
nmr.test <- nmr.i[getitest]
gni.test <- gni.i[getitest]
getr.test <- getr.i[getitest]
getj.test <- getj.i[getitest]

# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/val_res_region.pdf")
pdf(pdf_name3, width = 10, height = 12)
par(mfrow=c(2,2))

plot(st.errors~edu.test, col = getr.test, cex=1)
curve(predict(loess(st.errors~edu.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.errors~anc.test, col = getr.test,cex=1)
curve(predict(loess(st.errors~anc.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.errors~lbw.test, col = getr.test,cex=1)
curve(predict(loess(st.errors~lbw.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.errors~nmr.test, col = getr.test,cex=1)
curve(predict(loess(st.errors~nmr.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.errors~gni.test, col = getr.test,cex=1)
curve(predict(loess(st.errors~gni.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)



plot(st.errors~seq(2000, 2015)[gett.i][getitest], col=getr.test,cex=1,xlab="Year")
curve(predict(loess(st.errors~seq(2000, 2015)[gett.i][getitest]),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

dev.off()


# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/res_source.pdf")
pdf(pdf_name3, width = 10, height = 12)
par(mfrow=c(2,2))
#plot(res.i~edu.i+anc.i+lbw.i+nmr.i+gni.i, col = getr.i)
plot(st.errors~edu.test, col = getj.test, cex=1)
curve(predict(loess(st.errors~edu.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                           "Low National Subroutine", 
                           "National Survey", 
                           "Sub-national", 
                           "Other"),
       col=c(1:5), pch=1, cex=0.8)

plot(st.errors~anc.test, col = getj.test,cex=1)
curve(predict(loess(st.errors~anc.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft",legend=c("High National Subroutine", 
                          "Low National Subroutine", 
                          "National Survey", 
                          "Sub-national", 
                          "Other"),
       col=c(1:5), pch=1, cex=0.8)

plot(st.errors~lbw.test, col = getj.test,cex=1)
curve(predict(loess(st.errors~lbw.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                           "Low National Subroutine", 
                           "National Survey", 
                           "Sub-national", 
                           "Other"),
       col=c(1:5), pch=1, cex=0.8)

plot(st.errors~nmr.test, col = getj.test,cex=1)
curve(predict(loess(st.errors~nmr.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                           "Low National Subroutine", 
                           "National Survey", 
                           "Sub-national", 
                           "Other"),
       col=c(1:5), pch=1, cex=0.8)

plot(st.errors~gni.test, col = getj.test,cex=1)
curve(predict(loess(st.errors~gni.test),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                           "Low National Subroutine", 
                           "National Survey", 
                           "Sub-national", 
                           "Other"),
       col=c(1:5), pch=1, cex=0.8)




plot(st.errors~seq(2000, 2015)[gett.i][getitest], col=getj.test,cex=1,xlab="Year")
curve(predict(loess(st.errors~seq(2000, 2015)[gett.i][getitest]),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                           "Low National Subroutine", 
                           "National Survey", 
                           "Sub-national", 
                           "Other"),
       col=c(1:5), pch=1, cex=0.8)

dev.off()