######################################PSM#####################################################################################
library(MatchIt)
index=which(datas$frailty==0|datas$frailty==2)
temp_datas=datas[index,]
rm(index)
index=which(temp_datas$frailty==2)
temp_datas$frailty[index]=1
rm(index)
temp_datas$frailty=factor(temp_datas$frailty)
row.names(temp_datas)=c(1:nrow(temp_datas))
#
m.out1 <- matchit(frailty ~ age+gender+race+deprivation+income+university_education+
                    #smoking+alcohol+sedentary+metabolic_syndrome, exact=c('race','alcohol'),##frailty
                    smoking+alcohol+sedentary+metabolic_syndrome,exact=c('race','alcohol','sedentary','age'),caliper = 0.002,##prefrailty
                    method = "nearest",distance = "glm", link = "logit",ratio=1,data = temp_datas)
match_index=m.out1$match.matrix
index_non=which(is.na(match_index)==0)
match_index=as.matrix(match_index[index_non,])
rm(index_non)
controlled_index=as.numeric(match_index[,1])
treated_index=as.numeric(row.names(match_index))
control_data=temp_datas[controlled_index,]
treat_data=temp_datas[treated_index,]
rm(controlled_index,treated_index,m.out1,match_index,temp_datas)

##cox proportional model
new_data=rbind(control_data,treat_data)
library("survival")
library("survminer")
model <- coxph(Surv(time, status) ~ frailty,data = new_data)
model1=summary(model)
rm(model)
##############################################################################################################################

##write result
coeff=model1$coefficients
confi=model1$conf.int
result=data.frame(frailty_status=c('Prefrail'),HR=NA,lower=NA,upper=NA,CI=NA,p_value=NA,N=NA, Cases=NA)

result$p_value[1]=signif(coeff[1,'Pr(>|z|)'],3)
result$HR[1]=round(confi[1,'exp(coef)'],2)
result$lower[1]=round(confi[1,3],2)
result$upper[1]=round(confi[1,4],2)
result$CI[1]=paste(result$HR[1],' (',result$lower[1],'-',result$upper[1],')',sep='')
result$N[1]=nrow(treat_data)
result$Cases[1]=length(which(treat_data$status==1))
result=rbind(c('Matched non-frail',1, 1, 1,'1.00 (Ref)',NA,nrow(control_data),length(which(treat_data$status==1))),result)
#write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_psm.csv')

