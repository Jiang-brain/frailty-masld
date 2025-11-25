#################################joint reference group of metabolic syndrome#################################
new_data=datas
new_data$frailty=as.numeric(as.character(new_data$frailty))
index_0=which(new_data$frailty==0&new_data$metabolic_syndrome==0)
index_1=which(new_data$frailty==1&new_data$metabolic_syndrome==0)
index_2=which(new_data$frailty==2&new_data$metabolic_syndrome==0)
index_3=which(new_data$frailty==0&new_data$metabolic_syndrome==1)
index_4=which(new_data$frailty==1&new_data$metabolic_syndrome==1)
index_5=which(new_data$frailty==2&new_data$metabolic_syndrome==1)
new_data$frailty[index_0]=0
new_data$frailty[index_1]=1
new_data$frailty[index_2]=2
new_data$frailty[index_3]=3
new_data$frailty[index_4]=4
new_data$frailty[index_5]=5
new_data$frailty=factor(new_data$frailty)
rm(index_0,index_1,index_2,index_3,index_4,index_5)

fit_joint<- coxph(Surv(time, status) ~ frailty+age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary,data = new_data)
result<-summary(fit_joint)
rm(fit_joint)
results<-data.frame(id=seq(1:6),HR=NA,P_value=NA,lower=NA, upper=NA,CI=NA,N=NA,events=NA)
row.names(results)=c('Nonfrail_Met0','Prefrail_Met0','Frail_Met0','Nonfrail_Met1','Prefrail_Met1','Frail_Met1')
for (i in c(2:6)) {
  results$HR[i]=round(result$conf.int[i-1,'exp(coef)'],2)
  results$lower[i]=round(result$conf.int[i-1,'lower .95'],2)
  results$upper[i]=round(result$conf.int[i-1,'upper .95'],2)
  results$P_value[i]=signif(result$coefficients[i-1,'Pr(>|z|)'],3)
  results$CI[i]=paste('(',results$lower[i],'-',results$upper[i],')',sep ='')
}
results$N=table(new_data$frailty)
results$events[1]=length(which(new_data$frailty==0&new_data$status==1))
results$events[2]=length(which(new_data$frailty==1&new_data$status==1))
results$events[3]=length(which(new_data$frailty==2&new_data$status==1))
results$events[4]=length(which(new_data$frailty==3&new_data$status==1))
results$events[5]=length(which(new_data$frailty==4&new_data$status==1))
results$events[6]=length(which(new_data$frailty==5&new_data$status==1))
results$HR[1]=1
results$lower[1]=1
results$upper[1]=1
rm(new_data,i,result)
#write.csv(results,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_frailty_metabolic_joint.csv')
#######################################################################################

#################################joint reference group of sex#################################
new_data=datas
new_data$frailty=as.numeric(as.character(new_data$frailty))
index_0=which(new_data$frailty==0&new_data$gender==0)
index_1=which(new_data$frailty==1&new_data$gender==0)
index_2=which(new_data$frailty==2&new_data$gender==0)
index_3=which(new_data$frailty==0&new_data$gender==1)
index_4=which(new_data$frailty==1&new_data$gender==1)
index_5=which(new_data$frailty==2&new_data$gender==1)
new_data$frailty[index_0]=0
new_data$frailty[index_1]=1
new_data$frailty[index_2]=2
new_data$frailty[index_3]=3
new_data$frailty[index_4]=4
new_data$frailty[index_5]=5
new_data$frailty=factor(new_data$frailty)
rm(index_0,index_1,index_2,index_3,index_4,index_5)

fit_joint<- coxph(Surv(time, status) ~ frailty+age+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = new_data)
result<-summary(fit_joint)
rm(fit_joint)
results<-data.frame(id=seq(1:6),HR=NA,P_value=NA,lower=NA, upper=NA,CI=NA,N=NA,events=NA)
row.names(results)=c('Nonfrail_sex0','Prefrail_sex0','Frail_sex0','Nonfrail_sex1','Prefrail_sex1','Frail_sex1')
for (i in c(2:6)) {
  results$HR[i]=round(result$conf.int[i-1,'exp(coef)'],2)
  results$lower[i]=round(result$conf.int[i-1,'lower .95'],2)
  results$upper[i]=round(result$conf.int[i-1,'upper .95'],2)
  results$P_value[i]=signif(result$coefficients[i-1,'Pr(>|z|)'],3)
  results$CI[i]=paste('(',results$lower[i],'-',results$upper[i],')',sep ='')
}
results$N=table(new_data$frailty)
results$events[1]=length(which(new_data$frailty==0&new_data$status==1))
results$events[2]=length(which(new_data$frailty==1&new_data$status==1))
results$events[3]=length(which(new_data$frailty==2&new_data$status==1))
results$events[4]=length(which(new_data$frailty==3&new_data$status==1))
results$events[5]=length(which(new_data$frailty==4&new_data$status==1))
results$events[6]=length(which(new_data$frailty==5&new_data$status==1))
results$HR[1]=1
results$lower[1]=1
results$upper[1]=1
rm(new_data,i,result)
#write.csv(results,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_frailty_sex_joint.csv')
#######################################################################################
