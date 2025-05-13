###########################################frailty components#########################################################
model1 <- coxph(Surv(time, status) ~ weakness+
                   age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
model2 <- coxph(Surv(time, status) ~ tiredness+
                  age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
model3 <- coxph(Surv(time, status) ~ walking_speed+
                  age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
model4 <- coxph(Surv(time, status) ~ weight_loss+
                  age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
model5 <- coxph(Surv(time, status) ~ activity+
                  age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
mutual_model<- coxph(Surv(time, status) ~ weakness+tiredness+walking_speed+weight_loss+activity+
                  age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
individual_models=list(model1,model2,model3,model4,model5)
rm(model1,model2,model3,model4,model5)

result=data.frame(matrix(nrow = 7,ncol = 10))
row.names(result)=c('Weakness','Exhaustion','Slow walking speed','Weight loss','Physical inactivity','Prefrailty','Frailty')
colnames(result)=c('HR_1','Lower_1','Upper_1','CI_1','P_value1','HR_2','Lower_2','Upper_2','CI_2','P_value2')
for (i in 1:5) {
  coeff=summary(individual_models[[i]])
  result$HR_1[i]=round(coeff$coefficients[1,2],2)
  result$Lower_1[i]=round(coeff$conf.int[1,'lower .95'],2)
  result$Upper_1[i]=round(coeff$conf.int[1,'upper .95'],2)
  result$CI_1[i]=paste(result$HR_1[i],' (',result$Lower_1[i],'-',result$Upper_1[i],')',sep='')
  result$P_value1[i]=signif(coeff$coefficients[1,'Pr(>|z|)'],3)
  rm(coeff)
  ##mutual model
  coeff=summary(mutual_model)
  result$HR_2[i]=round(coeff$coefficients[i,2],2)
  result$Lower_2[i]=round(coeff$conf.int[i,'lower .95'],2)
  result$Upper_2[i]=round(coeff$conf.int[i,'upper .95'],2)
  result$CI_2[i]=paste(result$HR_2[i],' (',result$Lower_2[i],'-',result$Upper_2[i],')',sep='')
  result$P_value2[i]=signif(coeff$coefficients[i,'Pr(>|z|)'],3)
  rm(coeff)
}
rm(individual_models,mutual_model,i)

frailty_model<- coxph(Surv(time, status) ~ frailty+
                        age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
for (i in 1:2) {
  coeff=summary(frailty_model)
  result$HR_1[5+i]=round(coeff$coefficients[i,2],2)
  result$Lower_1[5+i]=round(coeff$conf.int[i,'lower .95'],2)
  result$Upper_1[5+i]=round(coeff$conf.int[i,'upper .95'],2)
  result$CI_1[5+i]=paste(result$HR_1[5+i],' (',result$Lower_1[5+i],'-',result$Upper_1[5+i],')',sep='')
  result$P_value1[5+i]=signif(coeff$coefficients[i,'Pr(>|z|)'],3)
}
rm(coeff,frailty_model)
#write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_components.csv')
######################################################################################################################
