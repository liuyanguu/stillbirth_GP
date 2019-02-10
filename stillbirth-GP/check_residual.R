#----
# model fit, residuals
mcmc.array.mod<-mod.ar1MVN$BUGSoutput$sims.array

totalObs_SBR <- jags.data$totalObs_SBR
y.i <- jags.data$y.i

yhat.i <- rep(NA, totalObs_SBR)
for (i in 1:totalObs_SBR) yhat.i[i] <- quantile(c(mcmc.array.mod[,, paste0("yhat.i[",i,"]")]), 0.5)


sd <- mod.ar1MVN$BUGSoutput$summary[paste0("sdy.j[",jags.data$getj.i,"]"), "mean"]

res.i <- y.i - yhat.i
st.res.i <- res.i/sd


##individual observation function
getc.i=jags.data$getc.i
gett.i=jags.data$gett.i
getr.c=jags.data$getr.c
getj.i=jags.data$getj.i
x.i<-function(varmatrix){
  varname.i<-rep(NA,totalObs_SBR)
  for (i in 1:totalObs_SBR) {varname.i[i]<-varmatrix[getc.i[i],gett.i[i]]}
  varname.i
}

edu.i<-x.i(jags.data$edu_matrix)
anc.i<-x.i(jags.data$anc_matrix)
lbw.i<-x.i(jags.data$lbw_matrix)
nmr.i<-x.i(jags.data$nmr_matrix)
gni.i<-x.i(jags.data$gni_matrix)
getr.i <- getr.c[getc.i]


# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/res_region.pdf")
pdf(pdf_name3, width = 10, height = 12)
par(mfrow=c(2,2))
#plot(res.i~edu.i+anc.i+lbw.i+nmr.i+gni.i, col = getr.i)
plot(st.res.i~edu.i, col = getr.i, cex=1)
curve(predict(loess(st.res.i~edu.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~anc.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i~anc.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~lbw.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i~lbw.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~nmr.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i~nmr.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~gni.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i~gni.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~ yhat.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i ~ yhat.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~seq(2000, 2015)[gett.i], col=getr.i,cex=1,xlab="Year")
curve(predict(loess(st.res.i~seq(2000, 2015)[gett.i]),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

dev.off()


# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/res_source.pdf")
pdf(pdf_name3, width = 10, height = 12)
par(mfrow=c(2,2))
#plot(res.i~edu.i+anc.i+lbw.i+nmr.i+gni.i, col = getr.i)
plot(st.res.i~edu.i, col = getj.i, cex=1)
curve(predict(loess(st.res.i~edu.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                            "Low National Subroutine", 
                            "National Survey", 
                            "Sub-national", 
                            "Other"),
       col=c(1:5), pch=1, cex=0.8)

plot(st.res.i~anc.i, col = getj.i,cex=1)
curve(predict(loess(st.res.i~anc.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft",legend=c("High National Subroutine", 
                           "Low National Subroutine", 
                           "National Survey", 
                           "Sub-national", 
                           "Other"),
       col=c(1:5), pch=1, cex=0.8)

plot(st.res.i~lbw.i, col = getj.i,cex=1)
curve(predict(loess(st.res.i~lbw.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                            "Low National Subroutine", 
                            "National Survey", 
                            "Sub-national", 
                            "Other"),
       col=c(1:5), pch=1, cex=0.8)

plot(st.res.i~nmr.i, col = getj.i,cex=1)
curve(predict(loess(st.res.i~nmr.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                            "Low National Subroutine", 
                            "National Survey", 
                            "Sub-national", 
                            "Other"),
       col=c(1:5), pch=1, cex=0.8)

plot(st.res.i~gni.i, col = getj.i,cex=1)
curve(predict(loess(res.i~gni.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                            "Low National Subroutine", 
                            "National Survey", 
                            "Sub-national", 
                            "Other"),
       col=c(1:5), pch=1, cex=0.8)


plot(st.res.i~ yhat.i, col = getj.i,cex=1)
curve(predict(loess(res.i ~ yhat.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                            "Low National Subroutine", 
                            "National Survey", 
                            "Sub-national", 
                            "Other"),
       col=c(1:5), pch=1, cex=0.8)

plot(st.res.i~seq(2000, 2015)[gett.i], col=getj.i,cex=1,xlab="Year")
curve(predict(loess(st.res.i~seq(2000, 2015)[gett.i]),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("High National Subroutine", 
                            "Low National Subroutine", 
                            "National Survey", 
                            "Sub-national", 
                            "Other"),
       col=c(1:5), pch=1, cex=0.8)

dev.off()