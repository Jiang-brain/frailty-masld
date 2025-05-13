########################################frailty-pdff, 3-category####################################################
library("survival")
library("survminer")

index_masld=which(datas$pdff>5)
index_hc=which(datas$pdff<=5)
datas$pdff_status=NA
datas$pdff_status[index_masld]=1
datas$pdff_status[index_hc]=0
rm(index_masld,index_hc)

model <- glm(pdff_status~ frailty+age+gender+race+deprivation+income+university_education+
               smoking+alcohol+sedentary+metabolic_syndrome,family=binomial(link='logit'),data=datas)

coeff<-summary(model)$coefficients
ci=exp(confint(model))
result=data.frame(OR=round(exp(coeff[,1]),2),lower=round(ci[,1],2),upper=round(ci[,2],2),p_value=signif(coeff[,4],3))
rm(coeff,ci,model)

result=result[2:3,]
result$N=NA
result$events=NA
result<-rbind(c(1, 1, 1,'1.00 (Ref)',NA,NA),result)
result$CI=NA

for (i in 1:3) {
  result$CI[i]=paste(result$OR[i],' (',result$lower[i],'-',result$upper[i],')',sep='')
  result$N[i]=length(which(datas$frailty==i-1))
  result$events[i]=length(which(datas$frailty==i-1&datas$pdff_status==1))
}
rm(i)
#write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_3_category_pdff.csv')

#######################################metabolites-nafld, pdff###################################################
index_masld=which(datas$pdff>5)
index_hc=which(datas$pdff<=5)
datas$pdff_status=NA
datas$pdff_status[index_masld]=1
datas$pdff_status[index_hc]=0
rm(index_masld,index_hc)

result_meta_nafld=data.frame(matrix(nrow=ncol(metabolites),ncol=6))
row.names(result_meta_nafld)=colnames(metabolites)
colnames(result_meta_nafld)=c('P for linear','OR','lower','upper','CI','N')

for (index_metabolite in c(1:ncol(metabolites))) {
  new_data=datas
  new_data$metabolite=metabolites[,index_metabolite]
  number_na<-rowSums(is.na(new_data))
  index_nonNA<-which(number_na==0)
  new_data<-new_data[index_nonNA,]
  rm(number_na,index_nonNA)
  
  ##linear model
  model <- glm(pdff_status~ scale(metabolite)+age+gender+race+deprivation+income+university_education+
                 smoking+alcohol+sedentary+metabolic_syndrome,family=binomial(link='logit'),data=new_data)
  
  coeff<-summary(model)$coefficients
  ci=exp(confint(model))
  
  result_meta_nafld$OR[index_metabolite]=round(exp(coeff['scale(metabolite)','Estimate']),2)
  result_meta_nafld$`P for linear`[index_metabolite]=signif(coeff['scale(metabolite)','Pr(>|z|)'],3)
  lower=round(ci['scale(metabolite)','2.5 %'],2)
  upper=round(ci['scale(metabolite)','97.5 %'],2)
  result_meta_nafld$lower[index_metabolite]=lower
  result_meta_nafld$upper[index_metabolite]=upper
  result_meta_nafld$CI[index_metabolite]=paste(result_meta_nafld$OR[index_metabolite],' (',lower,'-',upper,')',sep='')
  result_meta_nafld$N[index_metabolite]=nrow(new_data)
  rm(model,lower,upper,coeff,ci,new_data)
}
rm(index_metabolite)
