########################################subgroup analysis by all covariates#########################################################
result<-data.frame(HR_prefrail=1,P_prefrail=NA,CI_prefrail=NA,lower_prefrail=1,upper_prefrail=1,
                   HR_frail=1,P_frail=NA,CI_frail=NA,lower_frail=1,upper_frail=1,id=seq(1:38))
row.names(result)=c('Age group','Age<=45 years','45-55 years','55-65 years','>=65 yeas',
                    'Sex','Females','Males','Race','Ethnic minorities','White',
                    'Deprivation','Quarter 1','Quarter 2','Quarter 3','Quarter 4',
                    'Household income','Not answer','Low (<£51999)','Middle (£52000-£100000)','High (>£100000)',
                    'Education level','No education','Less than college','Above Colleage',
                    'Smoking','Never','Ever','Alcohol','Never_','Ever_',
                    'Sedentary behavior','<=2 hours/day','2-4 hours/day','>=4 hours/day','Metabolic syndrome','No','Yes')

for (frailty_status in c(1:2)) {
  
  temp_data <- subset(datas, frailty==0|frailty==frailty_status)
  temp_data$frailty=factor(temp_data$frailty)
  
  ##age
  fit1<- coxph(Surv(time, status) ~ frailty:age+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1:4)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+1,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-4+i,'exp(coef)']),2)
    result[i+1,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-4+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-4+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-4+i,'upper .95'],2)
    result[i+1,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+1,5*frailty_status-1]=lower
    result[i+1,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  ##sex
  fit1<- coxph(Surv(time, status) ~ frailty:gender+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1,2)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+6,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-2+i,'exp(coef)']),2)
    result[i+6,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-2+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-2+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-2+i,'upper .95'],2)
    result[i+6,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+6,5*frailty_status-1]=lower
    result[i+6,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  ##race
  fit1<- coxph(Surv(time, status) ~ frailty:race+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1,2)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+9,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-2+i,'exp(coef)']),2)
    result[i+9,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-2+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-2+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-2+i,'upper .95'],2)
    result[i+9,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+9,5*frailty_status-1]=lower
    result[i+9,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  ##deprivation
  fit1<- coxph(Surv(time, status) ~ frailty:deprivation+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1:4)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+12,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-4+i,'exp(coef)']),2)
    result[i+12,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-4+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-4+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-4+i,'upper .95'],2)
    result[i+12,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+12,5*frailty_status-1]=lower
    result[i+12,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  ##income
  fit1<- coxph(Surv(time, status) ~ frailty:income+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1:4)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+17,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-4+i,'exp(coef)']),2)
    result[i+17,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-4+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-4+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-4+i,'upper .95'],2)
    result[i+17,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+17,5*frailty_status-1]=lower
    result[i+17,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  ##education
  fit1<- coxph(Surv(time, status) ~ frailty:university_education+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1:3)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+22,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-3+i,'exp(coef)']),2)
    result[i+22,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-3+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-3+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-3+i,'upper .95'],2)
    result[i+22,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+22,5*frailty_status-1]=lower
    result[i+22,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  ##smoking
  fit1<- coxph(Surv(time, status) ~ frailty:smoking+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1:2)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+26,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-2+i,'exp(coef)']),2)
    result[i+26,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-2+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-2+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-2+i,'upper .95'],2)
    result[i+26,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+26,5*frailty_status-1]=lower
    result[i+26,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  ##alcohol
  fit1<- coxph(Surv(time, status) ~ frailty:alcohol+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1:2)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+29,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-2+i,'exp(coef)']),2)
    result[i+29,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-2+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-2+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-2+i,'upper .95'],2)
    result[i+29,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+29,5*frailty_status-1]=lower
    result[i+29,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  ##sedentary
  fit1<- coxph(Surv(time, status) ~ frailty:sedentary+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1:3)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+32,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-3+i,'exp(coef)']),2)
    result[i+32,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-3+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-3+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-3+i,'upper .95'],2)
    result[i+32,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+32,5*frailty_status-1]=lower
    result[i+32,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  ##metabolic syndrome
  fit1<- coxph(Surv(time, status) ~ frailty:metabolic_syndrome+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,data = temp_data)
  for (i in c(1:2)) {
    nrow_summary<-nrow(summary(fit1)$conf.int)
    result[i+36,5*frailty_status-4]=round((summary(fit1)$conf.int[nrow_summary-2+i,'exp(coef)']),2)
    result[i+36,5*frailty_status-3]=signif((summary(fit1)$coefficients[nrow_summary-2+i,'Pr(>|z|)']),3)
    lower=round(summary(fit1)$conf.int[nrow_summary-2+i,'lower .95'],2)
    upper=round(summary(fit1)$conf.int[nrow_summary-2+i,'upper .95'],2)
    result[i+36,5*frailty_status-2]=paste('(',lower,'-',upper,')',sep ='')
    result[i+36,5*frailty_status-1]=lower
    result[i+36,5*frailty_status]=upper
    rm(lower,upper,nrow_summary)
  }
  rm(i,fit1)
  rm(temp_data)
}
result$CI_prefrail=paste(result$HR_prefrail,result$CI_prefrail,sep=' ')
result$CI_frail=paste(result$HR_frail,result$CI_frail,sep=' ')
result$HR_nonfrail=NA
result$HR_nonfrail[c(2:4,7:8,10:11,13:16,18:21,23:25,27:28,30:31,33:35,37:38)]='1.00 (Ref)'
#write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_subgroup.csv')
#############################################################################################################
