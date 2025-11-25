############################################PERM########################################
model_0 <- coxph(Surv(time, status) ~ frailty+age+gender+race,data = datas)
model_1 <- coxph(Surv(time, status) ~ frailty+age+gender+race+smoking+alcohol+sedentary,data = datas)
model_2 <- coxph(Surv(time, status) ~ frailty+age+gender+race+deprivation+income+university_education,data = datas)
model_3 <- coxph(Surv(time, status) ~ frailty+age+gender+race+metabolic_syndrome,data = datas)
model_4 <- coxph(Surv(time, status) ~ frailty+age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
coeffs=list(summary(model_0),summary(model_1),summary(model_2),summary(model_3),summary(model_4))
rm(model_0,model_1,model_2,model_3,model_4)

result=data.frame(matrix(ncol = 12, nrow = 5))
row.names(result)=c('Minimally','Lifestyle factors','Socieconomic factors','Metabolic syndrome','All')
colnames(result)=c('hr_1','HR_1','lower_1','upper_1','P_value_1','PERM_1','hr_2','HR_2','lower_2','upper_2','P_value_2','PERM_2')

for (i in c(seq(1:length(coeffs)))) {
  temp_coeff<-coeffs[[i]]
  
  result$hr_1[i]=round(temp_coeff$coefficients['frailty1','exp(coef)'],2)
  result$lower_1[i]=round(temp_coeff$conf.int['frailty1','lower .95'],2)
  result$upper_1[i]=round(temp_coeff$conf.int['frailty1','upper .95'],2)
  result$HR_1[i]=paste(result$hr_1[i],' (',result$lower_1[i],'-',result$upper_1[i],')',sep ='')
  result$P_value_1[i]=signif(temp_coeff$coefficients['frailty1','Pr(>|z|)'],3)
  result$PERM_1[i]=paste(round((result$hr_1[1]-result$hr_1[i])/(result$hr_1[1]-1),3)*100,'%',sep ='')
  
  result$hr_2[i]=round(temp_coeff$coefficients['frailty2','exp(coef)'],2)
  result$lower_2[i]=round(temp_coeff$conf.int['frailty2','lower .95'],2)
  result$upper_2[i]=round(temp_coeff$conf.int['frailty2','upper .95'],2)
  result$HR_2[i]=paste(result$hr_2[i],' (',result$lower_2[i],'-',result$upper_2[i],')',sep ='')
  result$P_value_2[i]=signif(temp_coeff$coefficients['frailty2','Pr(>|z|)'],3)
  result$PERM_2[i]=paste(round((result$hr_2[1]-result$hr_2[i])/(result$hr_2[1]-1),3)*100,'%',sep ='')
  
  rm(temp_coeff)
}
rm(i,coeffs)
#write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_perm.csv')
########################################################################################
