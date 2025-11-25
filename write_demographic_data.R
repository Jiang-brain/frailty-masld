#########################################demographic information for matched data#############################################
summary_information=data.frame(matrix(ncol = 4, nrow = 39))
colnames(summary_information)=c('Whole population','Non-frail','Frail','P for difference')
row.names(summary_information)=c('Total N','Age group','Age<=45 years','45-55 years','55-65 years','>=65 yeas',
                                 'Sex','Females','Males','Race','Ethnic minorities','White',
                                 'Deprivation','Quarter 1','Quarter 2','Quarter 3','Quarter 4',
                                 'Household income','Not answer','Low (<£51999)','Middle (£52000-£100000)','High (>£100000)',
                                 'Education level','No education','Less than college','Above Colleage',
                                 'Smoking','Never','Ever','Alcohol','Never_','Ever_',
                                 'Sedentary behavior','<=2 hours/day','2-4 hours/day','>=4 hours/day','Metabolic syndrome','No','Yes')
for (frailty_status in c(10,0,1)) {
  if (frailty_status==10){
    index_frailty=c(1:nrow(new_data))#10=whole samples
    column_index=1
  }else{
    index_frailty=which(new_data$frailty==frailty_status)
    column_index=frailty_status+2
  }
  temp_datas=new_data[index_frailty,]
  rm(index_frailty)
  #N
  summary_information[1,column_index]=nrow(temp_datas)
  #age
  for (i in c(1:4)) {
    n_temp=length(which(temp_datas$age==i-1))
    summary_information[i+2,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #gender
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$gender==i-1))
    summary_information[i+7,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #race
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$race==i-1))
    summary_information[i+10,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #deprivation
  for (i in c(1:4)) {
    n_temp=length(which(temp_datas$deprivation==i-1))
    summary_information[i+13,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #income
  for (i in c(1:4)) {
    n_temp=length(which(temp_datas$income==i-1))
    summary_information[i+18,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #education
  for (i in c(1:3)) {
    n_temp=length(which(temp_datas$university_education==i-1))
    summary_information[i+23,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #smoking
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$smoking==i-1))
    summary_information[i+27,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #alcohol
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$alcohol==i-1))
    summary_information[i+30,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #sedentary
  for (i in c(1:3)) {
    n_temp=length(which(temp_datas$sedentary==i-1))
    summary_information[i+33,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #metabolic_syndrome
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$metabolic_syndrome==i-1))
    summary_information[i+37,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp,i)
  rm(temp_datas)
}
rm(new_data)
#P-value
{#age
  a<-t(matrix(c(tabulate(treat_data$age),tabulate(control_data$age)),nrow=4,ncol=2))
  summary_information[2,4]=signif(chisq.test(a)$p.value,3)
  #sex
  a<-t(matrix(c(tabulate(treat_data$gender),tabulate(control_data$gender)),nrow=2,ncol=2))
  summary_information[7,4]=signif(chisq.test(a)$p.value,3)
  #race
  a<-t(matrix(c(tabulate(treat_data$race),tabulate(control_data$race)),nrow=2,ncol=2))
  summary_information[10,4]=signif(chisq.test(a)$p.value,3)
  #deprivation
  a<-t(matrix(c(tabulate(treat_data$deprivation),tabulate(control_data$deprivation)),nrow=4,ncol=2))
  summary_information[13,4]=signif(chisq.test(a)$p.value,3)
  #income
  a<-t(matrix(c(tabulate(treat_data$income),tabulate(control_data$income)),nrow=4,ncol=2))
  summary_information[18,4]=signif(chisq.test(a)$p.value,3)
  #education
  a<-t(matrix(c(tabulate(treat_data$university_education),tabulate(control_data$university_education)),nrow=3,ncol=2))
  summary_information[23,4]=signif(chisq.test(a)$p.value,3)
  #smoking
  a<-t(matrix(c(tabulate(treat_data$smoking),tabulate(control_data$smoking)),nrow=2,ncol=2))
  summary_information[27,4]=signif(chisq.test(a)$p.value,3)
  #alcohol
  a<-t(matrix(c(tabulate(treat_data$alcohol),tabulate(control_data$alcohol)),nrow=2,ncol=2))
  summary_information[30,4]=signif(chisq.test(a)$p.value,3)
  #sedentary
  a<-t(matrix(c(tabulate(treat_data$sedentary),tabulate(control_data$sedentary)),nrow=3,ncol=2))
  summary_information[33,4]=signif(chisq.test(a)$p.value,3)
  #metabolic_syndrome
  a<-t(matrix(c(tabulate(treat_data$metabolic_syndrome),tabulate(control_data$metabolic_syndrome)),nrow=2,ncol=2))
  summary_information[37,4]=signif(chisq.test(a)$p.value,3)
  rm(a,treat_data,control_data)
}
#write.csv(summary_information,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/demographic_matched_frail.csv')




###########################################demographic information for original data##########################################
summary_information=data.frame(matrix(ncol = 4, nrow = 39))
colnames(summary_information)=c('Whole population','Non-frail','Prefrail','Frail')
row.names(summary_information)=c('Total N','Age group','Age<=45 years','45-55 years','55-65 years','>=65 yeas',
                                 'Sex','Females','Males','Race','Ethnic minorities','White',
                                 'Deprivation','Quarter 1','Quarter 2','Quarter 3','Quarter 4',
                                 'Household income','Not answer','Low (<£51999)','Middle (£52000-£100000)','High (>£100000)',
                                 'Education level','No education','Less than college','Above Colleage',
                                 'Smoking','Never','Ever','Alcohol','Never_','Ever_',
                                 'Sedentary behavior','<=2 hours/day','2-4 hours/day','>=4 hours/day','Metabolic syndrome','No','Yes')
for (frailty_status in c(10,0,1,2)) {
  if (frailty_status==10){
    index_frailty=c(1:nrow(datas))#10=whole samples
    column_index=1
  }else{
    index_frailty=which(datas$frailty==frailty_status)
    column_index=frailty_status+2
  }
  temp_datas=datas[index_frailty,]
  rm(index_frailty)
  #N
  summary_information[1,column_index]=nrow(temp_datas)
  #age
  for (i in c(1:4)) {
    n_temp=length(which(temp_datas$age==i-1))
    summary_information[i+2,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #gender
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$gender==i-1))
    summary_information[i+7,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #race
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$race==i-1))
    summary_information[i+10,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #deprivation
  for (i in c(1:4)) {
    n_temp=length(which(temp_datas$deprivation==i-1))
    summary_information[i+13,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #income
  for (i in c(1:4)) {
    n_temp=length(which(temp_datas$income==i-1))
    summary_information[i+18,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #education
  for (i in c(1:3)) {
    n_temp=length(which(temp_datas$university_education==i-1))
    summary_information[i+23,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #smoking
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$smoking==i-1))
    summary_information[i+27,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #alcohol
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$alcohol==i-1))
    summary_information[i+30,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #sedentary
  for (i in c(1:3)) {
    n_temp=length(which(temp_datas$sedentary==i-1))
    summary_information[i+33,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp)
  #metabolic_syndrome
  for (i in c(1:2)) {
    n_temp=length(which(temp_datas$metabolic_syndrome==i-1))
    summary_information[i+37,column_index]=paste(n_temp,' (',round(n_temp/nrow(temp_datas)*100,2),'%)',sep='')
  }
  rm(n_temp,i)
  rm(temp_datas)
}
rm(frailty_status,column_index)
#write.csv(summary_information,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/demographic.csv')
##############################################################################################################################
