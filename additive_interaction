###################test addictive interaction for metabolic syndrome###################
result<-data.frame(id=seq(1:5000),reri_frailty=NA,ap_frailty=NA,s_frailty=NA,reri_prefrailty=NA,ap_prefrailty=NA,s_prefrailty=NA)
colnames(result)=c('boot','reri_frailty','ap_frailty','s_frailty','reri_prefrailty','ap_prefrailty','s_pre_frailty')

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

##botstrapping 5000 times
for (boot_strap in 1:5000) {
  random_index=sample(1:nrow(new_data), nrow(new_data), replace = TRUE)
  boot_datas=new_data[random_index,]
  rm(random_index)
  model<- coxph(Surv(time, status) ~ frailty+age+gender+race+deprivation+income+university_education+smoking+alcohol+sedentary,data = boot_datas)
  coeffs<-summary(model)$coefficients
  rm(model)
  
  result$reri_frailty[boot_strap]=coeffs['frailty5','exp(coef)']-coeffs['frailty3','exp(coef)']-coeffs['frailty2','exp(coef)']+1
  result$ap_frailty[boot_strap]=result$reri_frailty[boot_strap]/coeffs['frailty5','exp(coef)']
  result$s_frailty[boot_strap]=(coeffs['frailty5','exp(coef)']-1)/(coeffs['frailty3','exp(coef)']-1+coeffs['frailty2','exp(coef)']-1)
  
  result$reri_prefrailty[boot_strap]=coeffs['frailty4','exp(coef)']-coeffs['frailty3','exp(coef)']-coeffs['frailty1','exp(coef)']+1
  result$ap_prefrailty[boot_strap]=result$reri_prefrailty[boot_strap]/coeffs['frailty4','exp(coef)']
  result$s_prefrailty[boot_strap]=(coeffs['frailty4','exp(coef)']-1)/(coeffs['frailty3','exp(coef)']-1+coeffs['frailty1','exp(coef)']-1)
  
  print(c(boot_strap,result$reri_frailty[boot_strap],result$reri_prefrailty[boot_strap]))
}
#write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/reri_metabolic_syndrome.csv')
################################################################

###################test addictive interaction for sex###################
result<-data.frame(id=seq(1:5000),reri_frailty=NA,ap_frailty=NA,s_frailty=NA,reri_prefrailty=NA,ap_prefrailty=NA,s_prefrailty=NA)
colnames(result)=c('boot','reri_frailty','ap_frailty','s_frailty','reri_prefrailty','ap_prefrailty','spre_frailty')

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

##botstrapping 5000 times
for (boot_strap in 1:5000) {
  random_index=sample(1:nrow(new_data), nrow(new_data), replace = TRUE)
  boot_datas=new_data[random_index,]
  rm(random_index)
  model<- coxph(Surv(time, status) ~ frailty+age+race+deprivation+income+university_education+smoking+alcohol+sedentary+metabolic_syndrome,data = boot_datas)
  coeffs<-summary(model)$coefficients
  rm(model)
  
  result$reri_frailty[boot_strap]=coeffs['frailty5','exp(coef)']-coeffs['frailty3','exp(coef)']-coeffs['frailty2','exp(coef)']+1
  result$ap_frailty[boot_strap]=result$reri_frailty[boot_strap]/coeffs['frailty5','exp(coef)']
  result$s_frailty[boot_strap]=(coeffs['frailty5','exp(coef)']-1)/(coeffs['frailty3','exp(coef)']-1+coeffs['frailty2','exp(coef)']-1)
  
  result$reri_prefrailty[boot_strap]=coeffs['frailty4','exp(coef)']-coeffs['frailty3','exp(coef)']-coeffs['frailty1','exp(coef)']+1
  result$ap_prefrailty[boot_strap]=result$reri_prefrailty[boot_strap]/coeffs['frailty4','exp(coef)']
  result$s_prefrailty[boot_strap]=(coeffs['frailty4','exp(coef)']-1)/(coeffs['frailty3','exp(coef)']-1+coeffs['frailty1','exp(coef)']-1)
  
  print(c(boot_strap,result$reri_frailty[boot_strap],result$reri_prefrailty[boot_strap]))
}
#write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/reri_gender.csv')
################################################################
