########################################association results 3 category####################################################
res.cox <- coxph(Surv(time, status) ~ frailty+age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = datas)
summary(res.cox)
coeff<-summary(res.cox)
rm(res.cox)
result=data.frame(HR=round(coeff$coefficients[,2],2),lower=round(coeff$conf.int[,3],2),upper=round(coeff$conf.int[,4],2),p_value=signif(coeff$coefficients[,5],3))
for (i in 1:nrow(result)){
  result$CI[i]=paste(result$HR[i],' (',result$lower[i],'-',result$upper[i],')',sep='')
}
result<-rbind(c(1, 1, 1, NA, '1.00 (Ref)'),result[1:2,],c(1, 1, 1, NA, '1.00 (Ref)'),result[3:5,],c(1, 1, 1, NA, '1.00 (Ref)'),result[6,],c(1, 1, 1, NA, '1.00 (Ref)'),result[7,],c(1, 1, 1, NA, '1.00 (Ref)'),
              result[8:10,],c(1, 1, 1, NA, '1.00 (Ref)'),result[11:13,],c(1, 1, 1, NA, '1.00 (Ref)'),result[14:15,],c(1, 1, 1, NA, '1.00 (Ref)'),result[16,],c(1, 1, 1, NA, '1.00 (Ref)'),result[17,],
              c(1, 1, 1, NA, '1.00 (Ref)'),result[18:19,],c(1, 1, 1, NA, '1.00 (Ref)'),result[20,])
result$N=NA
result$cases=NA
#frailty
for (i in 1:3){
  result$N[i]=length(which(datas$frailty==i-1))
  result$cases[i]=length(which(datas$frailty==(i-1)&datas$status==1))
}
{ #age
  for (i in 1:4){
    result$N[i+3]=length(which(datas$age==i-1))
    result$cases[i+3]=length(which(datas$age==(i-1)&datas$status==1))
  }
  #gender
  for (i in 1:2){
    result$N[i+7]=length(which(datas$gender==i-1))
    result$cases[i+7]=length(which(datas$gender==(i-1)&datas$status==1))
  }
  #race
  for (i in 1:2){
    result$N[i+9]=length(which(datas$race==i-1))
    result$cases[i+9]=length(which(datas$race==(i-1)&datas$status==1))
  }
  #deprivation
  for (i in 1:4){
    result$N[i+11]=length(which(datas$deprivation==i-1))
    result$cases[i+11]=length(which(datas$deprivation==(i-1)&datas$status==1))
  }
  #income
  for (i in 1:4){
    result$N[i+15]=length(which(datas$income==i-1))
    result$cases[i+15]=length(which(datas$income==(i-1)&datas$status==1))
  }
  #education
  for (i in 1:3){
    result$N[i+19]=length(which(datas$university_education==i-1))
    result$cases[i+19]=length(which(datas$university_education==(i-1)&datas$status==1))
  }
  #smoking
  for (i in 1:2){
    result$N[i+22]=length(which(datas$smoking==i-1))
    result$cases[i+22]=length(which(datas$smoking==(i-1)&datas$status==1))
  }
  #alcohol
  for (i in 1:2){
    result$N[i+24]=length(which(datas$alcohol==i-1))
    result$cases[i+24]=length(which(datas$alcohol==(i-1)&datas$status==1))
  }
  #sedentary
  for (i in 1:3){
    result$N[i+26]=length(which(datas$sedentary==i-1))
    result$cases[i+26]=length(which(datas$sedentary==(i-1)&datas$status==1))
  }
  #metabolic_syndrome
  for (i in 1:2){
    result$N[i+29]=length(which(datas$metabolic_syndrome==i-1))
    result$cases[i+29]=length(which(datas$metabolic_syndrome==(i-1)&datas$status==1))
  }
  rm(coeff,i)
  #write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2023-Cox-model/datas/HR_frailty_status.csv')
}
result<-rbind(c(NA,NA,NA,NA,NA,NA,NA),result[1:3,],c(NA,NA,NA,NA,NA,NA,NA),result[4:7,],c(NA,NA,NA,NA,NA,NA,NA),result[8:9,],c(NA,NA,NA,NA,NA,NA,NA),result[10:11,],c(NA,NA,NA,NA,NA,NA,NA),
              result[12:15,],c(NA,NA,NA,NA,NA,NA,NA),result[16:19,],c(NA,NA,NA,NA,NA,NA,NA),result[20:22,],c(NA,NA,NA,NA,NA,NA,NA),result[23:24,],c(NA,NA,NA,NA,NA,NA,NA),result[25:26,],
              c(NA,NA,NA,NA,NA,NA,NA),result[27:29,],c(NA,NA,NA,NA,NA,NA,NA),result[30:31,])
row.names(result)=c('Frailty status','Nonfrail','Prefrail','Frail','Age group','Age<=45 years','45-55 years','55-65 years','>=65 yeas',
                                 'Sex','Females','Males','Race','Ethnic minorities','White',
                                 'Deprivation','1 quarter','2 quarter','3 quarter','4 quarter',
                                 'Household income','Not answer','Low (<£51999)','Middle (£52000-£100000)','High (>£100000)',
                                 'Education level','No education','Less than college','Above Colleage',
                                 'Smoking','Never','Ever','Alcohol','Never_','Ever_',
                                 'Sedentary behavior','<=2 hours/day','2-4 hours/day','>=4 hours/day','Metabolic syndrome','No','Yes')
#write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_3_category.csv')
#####################################################################################################################
