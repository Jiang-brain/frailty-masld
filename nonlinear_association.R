library(survival)
library(rms)
library(dplyr)
library(Gmisc)
library(splines)
library(Greg)
library(tidyverse)

dd <- datadist(datas) #为后续程序设定数据环境
options(datadist='dd') #为后续程序设定数据环境

fit<- cph(Surv(time, status) ~ rcs(frailty,3) +
            age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome, data = datas)
anova(fit)#test the significance of nolinear regression

plotHR(fit, term="frailty", xlab="Physical frailty",se=TRUE,
       polygon_ci=TRUE,
       col.term = "#0070b9",
       col.se = "#DEEBF7BB",
       lwd.term=2,
       lty.term = 1,
       ylog=FALSE,
       alpha=0.05,#alpha level, 95%
       cex=1,
       axes = TRUE,
       plot.bty="l", xlim=c(0,6),ylim=c(0,5.5),rug="density")
Pre0 <-rms::Predict(fit,frailty,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=2)
