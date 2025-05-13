###################################linear association between metabolites and physical frailty#######################################
library(lmerTest)
result_metabolites=data.frame(matrix(nrow=ncol(metabolites),ncol=5))
row.names(result_metabolites)=colnames(metabolites)
colnames(result_metabolites)=c('coeff','se','T_values','P_value','N')
for (i in 1:ncol(metabolites)) {
  datas$metabolite=metabolites[,i]
  number_na<-rowSums(is.na(datas))
  index_nonNA<-which(number_na==0)
  new_data<-datas[index_nonNA,]
  rm(number_na,index_nonNA)
  # if (min(new_data$metabolite,na.rm = TRUE)==0) {
  #   new_data$metabolite=log(new_data$metabolite+1)
  #   print(i)
  # }else{
  #   new_data$metabolite=log(new_data$metabolite)
  # }
  
  ##remove outliers
  # index_outlier=which(new_data$metabolite<(mean(new_data$metabolite)-4*sd(new_data$metabolite)) | new_data$metabolite>(mean(new_data$metabolite)+4*sd(new_data$metabolite)))
  # if (length(index_outlier>0)){
  #   new_data=new_data[-index_outlier,]
  #   print(length(index_outlier))
  # }
  # rm(index_outlier)

  
  model <- lmer(scale(metabolite)~ scale(frailty)+age+gender+race+deprivation+income+university_education+
                  smoking+alcohol+sedentary+metabolic_syndrome
                +(1|sites),data=new_data)
  coeff<-summary(model)$coefficients
  result_metabolites$coeff[i]=round(coeff[2,1],3)
  result_metabolites$se[i]=round(coeff[2,2],4)
  result_metabolites$T_values[i]=round(coeff['scale(frailty)','t value'],2)
  result_metabolites$P_value[i]=signif(coeff[2,5],3)
  result_metabolites$N[i]=nrow(new_data)
  rm(model,coeff,new_data)
}
rm(i)
phenotype<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/Metabolities_name.csv',header=T)
result_metabolites$meta_name=phenotype$Biomarker
result_metabolites$full_name=phenotype$Description
rm(phenotype)
a_1=order(abs(result_metabolites$coeff),decreasing = TRUE)
result_metabolites=result_metabolites[a_1,]
#write.csv(result_metabolites,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/frailty_metabolites.csv')
#write.csv(result_metabolites,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/frailty_metabolites_ordered.csv')
################################################################################################################################
