#######################################metabolites-nafld, linear/nonlinear###################################################
result_meta_nafld=data.frame(matrix(nrow=ncol(metabolites),ncol=7))
row.names(result_meta_nafld)=colnames(metabolites)
colnames(result_meta_nafld)=c('P_overall','P for nonlinear','P for linear','HR','lower','upper','CI')

for (index_metabolite in c(1:ncol(metabolites))) {
  new_data=datas
  new_data$metabolite=metabolites[,index_metabolite]
  number_na<-rowSums(is.na(new_data))
  index_nonNA<-which(number_na==0)
  new_data<-new_data[index_nonNA,]
  rm(number_na,index_nonNA)
  
  ##remove outliers
  # index_outlier=which(new_data$metabolite<(mean(new_data$metabolite)-4*sd(new_data$metabolite)) | new_data$metabolite>(mean(new_data$metabolite)+4*sd(new_data$metabolite)))
  # if (length(index_outlier>0)){
  #   new_data=new_data[-index_outlier,]
  #   print(length(index_outlier))
  # }
  # rm(index_outlier)
  
  {
  # b=quantile(new_data$metabolite,seq(0,1,0.0005))
  # new_data$quantiled_metabolite=new_data$metabolite
  # for (i in c(1:length(b)-1)) {
  #   index=which(new_data$metabolite>b[i]&new_data$metabolite<=b[i+1])
  #   new_data$quantiled_metabolite[index]=b[i]
  # }
  # rm(i,index,b)
  # 
  # dd <- datadist(new_data) #为后续程序设定数据环境
  # options(datadist='dd') #为后续程序设定数据环境
  # fit<- cph(Surv(time, status) ~ rcs(quantiled_metabolite,3) +age+gender+race+deprivation+income+university_education+
  #             smoking+alcohol+sedentary+metabolic_syndrome, data=new_data)
  # result_meta_nafld$`P for nonlinear`[index_metabolite]<-anova(fit)[2,3]
  # result_meta_nafld$P_overall[index_metabolite]<-anova(fit)[1,3]
  # p<-plotHR(fit, term="quantiled_metabolite", xlab="metabolite",se=TRUE,
  #           polygon_ci=TRUE,
  #           col.term = "#0070b9",
  #           col.se = "#DEEBF7BB",
  #           lwd.term=2,
  #           lty.term = 1,
  #           ylog=FALSE,
  #           alpha=0.05,#alpha level, 95%
  #           cex=1,
  #           axes = TRUE,
  #           plot.bty="l", xlim=c(min(new_data$quantiled_metabolite),max(new_data$quantiled_metabolite)),ylim=c(0.6,1.6),rug="density")
  # plot(p)
  # Pre0 <-rms::Predict(fit,quantiled_metabolite,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=2)
  # rm(fit,p,Pre0,dd)
  }

  # if (min(new_data$metabolite,na.rm = TRUE)==0) {
  #   new_data$metabolite=log(new_data$metabolite+1)
  #   print(index_metabolite)
  # }else{
  #   new_data$metabolite=log(new_data$metabolite)
  # }
  
  ##linear model
  fit_linear<- coxph(Surv(time, status) ~ scale(metabolite) +age+gender+race+deprivation+income+university_education+
                       smoking+alcohol+sedentary+metabolic_syndrome, data=new_data)
  coeff<-summary(fit_linear)
  result_meta_nafld$HR[index_metabolite]=round(coeff$coefficients['scale(metabolite)','exp(coef)'],2)
  result_meta_nafld$`P for linear`[index_metabolite]=signif(coeff$coefficients['scale(metabolite)','Pr(>|z|)'],3)
  lower=round(coeff$conf.int['scale(metabolite)','lower .95'],2)
  upper=round(coeff$conf.int['scale(metabolite)','upper .95'],2)
  result_meta_nafld$lower[index_metabolite]=lower
  result_meta_nafld$upper[index_metabolite]=upper
  result_meta_nafld$CI[index_metabolite]=paste(result_meta_nafld$HR[index_metabolite],' (',lower,'-',upper,')',sep='')
  rm(fit_linear,lower,upper,coeff,new_data)
}
rm(index_metabolite)
#write.csv(result_meta_nafld,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/metabolites_NAFLD_linear_nonlinear.csv')
phenotype<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/Metabolities_name.csv',header=T)
result_meta_nafld$meta_name=phenotype$Biomarker
result_meta_nafld$full_name=phenotype$Description
rm(phenotype)
a_1=order(abs(result_meta_nafld$HR-1),decreasing = TRUE)
result_meta_nafld=result_meta_nafld[a_1,]
#write.csv(result_meta_nafld,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/metabolites_NAFLD_linear_nonlinear_ordered.csv')


################################################################################################################################
# result_metabolites<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/frailty_metabolites_log.csv',header=T)
# result_meta_nafld<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/metabolites_NAFLD_linear_nonlinear_log.csv',header=T)

result_metabolites<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/frailty_metabolites.csv',header=T)
result_meta_nafld<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/metabolites_NAFLD_linear_nonlinear.csv',header=T)

significance_index=which(result_metabolites$P_value<0.05/251&result_meta_nafld$P.for.linear<0.05/251)
rm(result_metabolites,result_meta_nafld)

###
# for (i in 1:ncol(metabolites)) {
#   if (min(metabolites[,i],na.rm = TRUE)==0) {
#     metabolites[,i]=log(metabolites[,i]+1)
#     print(i)
#   }else{
#     metabolites[,i]=log(metabolites[,i])
#   }
# }
# rm(i)


##pca
temp_metabolites=metabolites[,significance_index]
number_na<-rowSums(is.na(temp_metabolites))
index_nonNA<-which(number_na==0)
temp_metabolites<-temp_metabolites[index_nonNA,]
datas=datas[index_nonNA,]
rm(number_na,index_nonNA)


# for (i in 1:length(significance_index)) {
#   if (min(temp_metabolites[,i])==0) {
#     temp_metabolites[,i]=log(temp_metabolites[,i]+1)
#     print(i)
#   }else{
#     temp_metabolites[,i]=log(temp_metabolites[,i])
#   }
# }
# rm(i,significance_index)
temp_metabolites=scale(temp_metabolites)
pca_result <- princomp(temp_metabolites, cor=TRUE) 
summary(pca_result)
scores=pca_result$scores
screeplot(pca_result, type="line", main="碎石图", lwd=2)
screeplot(pca_result, type="barplot", main="碎石图", lwd=2)

metabolites=scores


