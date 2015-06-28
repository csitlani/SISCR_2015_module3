### Module 3

## Exploratory data analysis

rm(list=ls())
load('growth.RData')
ages <- c(8,10,12,14)

# Individual trajectories
par(bty="n", mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.5, cex.lab=1.5)
with(growth, plot(age, length, type="n", xlab="Age (years)", ylab="Length (mm)",  ylim=c(15, 31), axes=FALSE))
axis(1, ages)
axis(2, c(15, 20, 25, 30))
with(growth, for(i in id){
    lines(age[id==i&gender=="female"], length[id==i&gender=="female"], col="firebrick4", lwd=3)	
    lines(age[id==i&gender=="male"], length[id==i&gender=="male"], col="skyblue4", lty=2, lwd=3)
})
legend("topleft", legend=c("Male", "Female"), lty=c(2,1), lwd=3, col=c("skyblue4", "firebrick4"), bty="n", cex=1.5)

# Means by gender
mm1 <- with(growth, mean(length[gender=="male"&age==8]))
mm2 <- with(growth, mean(length[gender=="male"&age==10]))
mm3 <- with(growth, mean(length[gender=="male"&age==12]))
mm4 <- with(growth, mean(length[gender=="male"&age==14]))
mms <- c(mm1, mm2, mm3, mm4)

m1 <- with(growth, mean(length[gender=="female"&age==8]))
m2 <- with(growth, mean(length[gender=="female"&age==10]))
m3 <- with(growth, mean(length[gender=="female"&age==12]))
m4 <- with(growth, mean(length[gender=="female"&age==14]))
ms <- c(m1, m2, m3, m4)

# Female individual trajectories
par(bty="n", mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
with(growth, plot(age, length, type="n", xlab="Age (years)", ylab="Length (mm)",  ylim=c(15, 31), axes=FALSE, main="Observed Data"))
axis(1, ages)
axis(2, c(15, 20, 25, 30))
with(growth, for(i in unique(id)){
    lines(age[id==i&gender=="female"], length[id==i&gender=="female"], col="firebrick4", lwd=3)
})
with(growth, text(age[gender=="female"], length[gender=="female"], labels=id[gender=="female"], cex=1.25))
lines(ages, ms, lwd=4)

# Female fitted lines
par(bty="n", mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
with(growth, plot(age, length, type="n", xlab="Age (years)", ylab="Length (mm)",  ylim=c(15, 31), axes=FALSE, main="Fitted Lines"))
axis(1, ages)
axis(2, c(15, 20, 25, 30))
with(growth, for(i in unique(id[gender=="female"])){
	mi <- lm(length ~ age, subset=id==i)
	abline(a=coef(mi)[1], b=coef(mi)[2], lty=2, lwd=3, col="firebrick4")
})
m1 <- lm(length~ age, data=growth, subset=gender=="female")
abline(a=coef(m1)[1], b=coef(m1)[2], lwd=4)

growth <- within(growth, agec<-age-8)
m1 <- lm(length ~ gender*agec, data=growth)
res <- m1$residuals

# Individual female residuals
par(bty="n", mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
with(growth, plot(age, res, type="n", xlab="Age (years)", ylab="Length residuals"))
with(growth, for(i in id){
    lines(age[id==i&gender=="female"], res[id==i&gender=="female"], col="firebrick4", lwd=3)
})
abline(h=0)
with(growth, text(age[gender=="female"], res[gender=="female"], labels=id[gender=="female"], cex=1.25))


## Extended dental growth data

rm(list=ls())
load('growthmore.Rdata')

# Individual trajectories
par(bty="n", mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.5, cex.lab=1.5)
with(growthmore, plot(time, length, type="n", xlab="Age (years)", ylab="Length (mm)",  ylim=c(15, 32), axes=FALSE))
axis(1, c(ages, 16, 18))
axis(2, c(15, 20, 25, 30))
with(growthmore, for(i in id){
    lines(time[id==i&gender=="female"], length[id==i&gender=="female"], col="firebrick4", lwd=3)	
    lines(time[id==i&gender=="male"], length[id==i&gender=="male"], col="skyblue4", lty=2, lwd=3)
})
legend("topleft", legend=c("Male", "Female"), lty=c(2,1), lwd=3, col=c("skyblue4", "firebrick4"), bty="n", cex=1.5)

# Categorical time
mcat <- lm(length~as.factor(time),data=growthmore)
summary(mcat)

par(bty="n", mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.5, cex.lab=1.5)
with(growthmore, plot(time, length, type="p", xlab="Age (years)", ylab="Length (mm)",  ylim=c(15, 32), axes=FALSE))
axis(1, c(ages, 16, 18))
axis(2, c(15, 20, 25, 30))
points(c(ages,16,18),mcat$coef[1]+c(0,mcat$coef[-1]),pch=16,cex=2)

# Linear time
mlin <- lm(length~time,data=growthmore)
summary(mlin)

par(bty="n", mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.5, cex.lab=1.5)
with(growthmore, plot(time, length, type="p", xlab="Age (years)", ylab="Length (mm)",  ylim=c(15, 32), axes=FALSE))
axis(1, c(ages, 16, 18))
axis(2, c(15, 20, 25, 30))
lines(c(ages,16,18),mlin$coef[1]+mlin$coef[2]*c(ages,16,18),lwd=2)
points(c(ages,16,18),mcat$coef[1]+c(0,mcat$coef[-1]),pch=16,cex=2)

# Quadratic time
growthmore <- within(growthmore, time2<-time^2)
mquad <- lm(length~time+time2,data=growthmore)
summary(mquad)

par(bty="n", mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.5, cex.lab=1.5)
with(growthmore, plot(time, length, type="p", xlab="Age (years)", ylab="Length (mm)",  ylim=c(15, 32), axes=FALSE))
axis(1, c(ages, 16, 18))
axis(2, c(15, 20, 25, 30))
lines(c(ages,16,18),mquad$coef[1]+mquad$coef[2]*c(ages,16,18)+mquad$coef[3]*c(ages,16,18)^2,lwd=2)
points(c(ages,16,18),mcat$coef[1]+c(0,mcat$coef[-1]),pch=16,cex=2)

# Dental growth data

rm(list=ls())
load('growth.RData')

# Regression model with interaction
model <- lm(length~I(age-8)*gender, data=growth)
summary(model)

par(bty="n", mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.5, cex.lab=1.5)
with(growth, plot(age, length, type="n", xlab="Age (years)", ylab="Length (mm)",  ylim=c(15, 30), axes=FALSE))
axis(1, ages)
axis(2, c(15, 20, 25, 30))
lines(ages, model$coef[1]+model$coef[2]*(ages-8), col="firebrick4", lwd=3)	
lines(ages, model$coef[1]+model$coef[2]*(ages-8)+model$coef[3]+model$coef[4]*(ages-8), col="skyblue4", lty=2, lwd=3)
legend("topleft", legend=c("Male", "Female"), lty=c(2,1), lwd=3, col=c("skyblue4","firebrick4"), bty="n", cex=1.5)

# Variance, covariance, correlations
# Female
with(growth, round(var(length[age==8&gender=="female"]), 2))
with(growth, round(var(length[age==10&gender=="female"]), 2))
with(growth, round(var(length[age==12&gender=="female"]), 2))
with(growth, round(var(length[age==14&gender=="female"]), 2))

with(growth, round(cov(length[age==8&gender=="female"], length[age==10&gender=="female"]), 2)) 
with(growth, round(cov(length[age==8&gender=="female"], length[age==12&gender=="female"]), 2))
with(growth, round(cov(length[age==8&gender=="female"], length[age==14&gender=="female"]), 2))
with(growth, round(cov(length[age==10&gender=="female"], length[age==12&gender=="female"]), 2))
with(growth, round(cov(length[age==10&gender=="female"], length[age==14&gender=="female"]), 2))
with(growth, round(cov(length[age==12&gender=="female"], length[age==14&gender=="female"]), 2))

with(growth, round(cor(length[age==8&gender=="female"], length[age==10&gender=="female"]), 2)) 
with(growth, round(cor(length[age==8&gender=="female"], length[age==12&gender=="female"]), 2))
with(growth, round(cor(length[age==8&gender=="female"], length[age==14&gender=="female"]), 2))
with(growth, round(cor(length[age==10&gender=="female"], length[age==12&gender=="female"]), 2))
with(growth, round(cor(length[age==10&gender=="female"], length[age==14&gender=="female"]), 2))
with(growth, round(cor(length[age==12&gender=="female"], length[age==14&gender=="female"]), 2))

# Male
with(growth, round(var(length[age==8&gender=="male"]), 2))
with(growth, round(var(length[age==10&gender=="male"]), 2))
with(growth, round(var(length[age==12&gender=="male"]), 2))
with(growth, round(var(length[age==14&gender=="male"]), 2))

with(growth, round(cov(length[age==8&gender=="male"], length[age==10&gender=="male"]), 2)) 
with(growth, round(cov(length[age==8&gender=="male"], length[age==12&gender=="male"]), 2))
with(growth, round(cov(length[age==8&gender=="male"], length[age==14&gender=="male"]), 2))
with(growth, round(cov(length[age==10&gender=="male"], length[age==12&gender=="male"]), 2))
with(growth, round(cov(length[age==10&gender=="male"], length[age==14&gender=="male"]), 2))
with(growth, round(cov(length[age==12&gender=="male"], length[age==14&gender=="male"]), 2))

with(growth, round(cor(length[age==8&gender=="male"], length[age==10&gender=="male"]), 2)) 
with(growth, round(cor(length[age==8&gender=="male"], length[age==12&gender=="male"]), 2))
with(growth, round(cor(length[age==8&gender=="male"], length[age==14&gender=="male"]), 2))
with(growth, round(cor(length[age==10&gender=="male"], length[age==12&gender=="male"]), 2))
with(growth, round(cor(length[age==10&gender=="male"], length[age==14&gender=="male"]), 2))
with(growth, round(cor(length[age==12&gender=="male"], length[age==14&gender=="male"]), 2))

# Generalized estimating equations

library(geepack)
?geeglm

m_ind <- geeglm(length ~ I(age-8)*gender, id=id, 
                corstr="independence", data=growth)
m_exc <- geeglm(length ~ I(age-8)*gender, id=id, 
                corstr="exchangeable", data=growth)
m_ar1 <- geeglm(length ~ I(age-8)*gender, id=id, 
                corstr="ar1", data=growth)
m_uns <- geeglm(length ~ I(age-8)*gender, id=id, 
                corstr="unstructured", data=growth)

m_ols <- lm(length ~ I(age-8)*gender, data=growth)

# Linear mixed-effects models

library(lme4)
?lmer

m_ri <- lmer(length ~ (1 | id) + I(age-8)*gender, data=growth)

m_rs <- lmer(length ~ (I(age-8) | id) + I(age-8)*gender, data=growth)

anova(m_ri, m_rs)
