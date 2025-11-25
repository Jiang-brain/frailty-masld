############################################interaction########################################
fit0<- coxph(Surv(time, status) ~ frailty+age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
fit1<- coxph(Surv(time, status) ~ frailty*age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
fit2<- coxph(Surv(time, status) ~ frailty*gender+age+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
fit3<- coxph(Surv(time, status) ~ frailty*race+age+gender+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
fit4<- coxph(Surv(time, status) ~ frailty*deprivation+age+gender+race+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
fit5<- coxph(Surv(time, status) ~ frailty*income+age+gender+race+deprivation+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
fit6<- coxph(Surv(time, status) ~ frailty*university_education+age+gender+race+deprivation+income+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
fit7<- coxph(Surv(time, status) ~ frailty*smoking+age+gender+race+deprivation+income+university_education+alcohol+sedentary+metabolic_syndrome,data = datas)
fit8<- coxph(Surv(time, status) ~ frailty*alcohol+age+gender+race+deprivation+income+university_education+smoking+sedentary+metabolic_syndrome,data = datas)
fit9<- coxph(Surv(time, status) ~ frailty*sedentary+age+gender+race+deprivation+income+university_education+smoking+alcohol+metabolic_syndrome,data = datas)
fit10<-coxph(Surv(time, status) ~ frailty*metabolic_syndrome+age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary,data = datas)

fit_models=list(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10)
rm(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10)
P_interaction=data.frame(matrix(ncol = 1, nrow = 10))
row.names(P_interaction)=c('Age','Sex','Race','Deprivation','Income','Education','Smoking','Alcohol','Sedentary','Metabolic_syndrome')
colnames(P_interaction)=c('P_interaction')
for (i in c(1:10)) {
  P_interaction$P_interaction[i]=signif(anova(fit0,fit_models[[i]],test="Chisq")$`Pr(>|Chi|)`[2],3)
}
rm(i,fit0,fit_models)
