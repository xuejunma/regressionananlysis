rm(list=ls())
dat <- read.table("E:\\回归分析\\例解回归分析\\p130.txt", head=TRUE)
library(MASS)
#attach(dat)
head(dat)

E1 <- as.numeric(dat$E == 1)
E2 <- as.numeric(dat$E == 2)
fit <- lm(S~X+E1+E2+M, data=dat)
summary(fit)



CG <- NULL
#index E1,2,3, M0,1  begin
indexE1 <- which(dat$E==1)
indexE2 <- which(dat$E==2)
indexE3 <- which(dat$E==3)
indexM0 <- which(dat$M==0)
indexM1 <- which(dat$M==1)

index1 <-  intersect(indexE1, indexM0)
CG[index1] <- 1
index2 <-  intersect(indexE1, indexM1)
CG[index2] <- 2
index3 <-  intersect(indexE2, indexM0)
CG[index3] <- 3
index4 <-  intersect(indexE2, indexM1)
CG[index4] <- 4
index5 <-  intersect(indexE3, indexM0)
CG[index5] <- 5
index6 <-  intersect(indexE3, indexM1)
CG[index6] <- 6

indexfull <- c(index1,index2,index3,index4,index5,index6)
#index E1,2,3, M0,1  end

#student residuals
res_stu<-  MASS::studres(fit)
par(mfrow=c(1,1))
plot(dat$X, res_stu, ylab = "student residuals",xlab = "X")
plot(CG, res_stu[indexfull], ylab = "student residuals",xlab = "category")
plot(CG, res_stu, ylab = "student residuals",xlab = "category")




##########################################Interaction effect
fit1 <- lm(S~X+E1+E2+M +E1*M +E2*M, data=dat)
summary(fit1)

par(mfrow=c(1,1))
res_stu1<-  MASS::studres(fit1)
plot(dat$X, res_stu1, ylab = "student residuals",xlab = "X")
plot(CG, res_stu1[indexfull], ylab = "student residuals",xlab = "category")


####remove 33th observation 
indexfulln <- setdiff(indexfull, 33)
datn <- dat[-33, ]
E1n <- as.numeric(datn$E == 1)
E2n <- as.numeric(datn$E == 2)
fitn <- lm(S~X+E1n+E2n+M +E1n*M +E2n*M, data=datn)
summary(fitn)
par(mfrow=c(1,1))
res_stun <-  MASS::studres(fitn)
plot(datn$X, res_stun, ylab = "student residuals",xlab = "X")
plot(CG[-33], res_stun[indexfulln], ylab = "student residuals",xlab = "category")
