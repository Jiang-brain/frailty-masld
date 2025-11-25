####################################plot HR for subgroup######################################################################
library(forestplot)
library(dplyr)
library(ggthemes)

mydata<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_subgroup.csv',header=T)
mydata=rbind(mydata,mydata[1,])
mydata$HR_prefrail[nrow(mydata)]=4
mydata$lower_prefrail[nrow(mydata)]=4
mydata$upper_prefrail[nrow(mydata)]=4

mydata$HR_frail[nrow(mydata)]=4
mydata$lower_frail[nrow(mydata)]=4
mydata$upper_frail[nrow(mydata)]=4

mydata$X[nrow(mydata)]='Null'
result1=mydata[,c(1,2,5,6)]#prefrail
#result1=mydata[,c(1,7,10,11)]#frail
colnames(result1)=c('subgroup','HR','lower','upper')

result_HR <- tibble(HR  = result1$HR, subgroup = result1$subgroup,lower = result1$lower,upper = result1$upper)
#rm(result)
header <- tibble(subgroup='subgroup')
combined_data <- bind_rows(header,result_HR)
rm(header,result_HR)
combined_data %>% 
  forestplot(labeltext = c(subgroup),
             mean=HR,
             graph.pos =2,boxsize=0.38,zero=1,
             is.summary = FALSE,clip = c(1,6), xlog = FALSE,graphwidth = unit(.38,"npc"),
             col = fpColors(box = "#3c5a66",line = "black",summary = "grey"),
             mar=unit(rep(1.0, times = 4), "cm"),
             txt_gp=fpTxtGp(label=gpar(cex=0.8), ticks=gpar(cex=0.6), xlab=gpar(cex = 0.8), title=gpar(cex = 0.8)),
             xlab = "HR (95% CI)",lwd.xaxis = 1.0)
########################################################################################################################

####################################plot HR for 6-category######################################################################
library(forestplot)
library(dplyr)
library(ggthemes)

result<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_6_category.csv',header=T)

result=rbind(result,result[1,])
result$HR[nrow(result)]=5
result$lower[nrow(result)]=5
result$upper[nrow(result)]=5
result$X[7]=c('Null')

result_HR <- tibble(HR  = result$HR, frailty_status = result$X,p_value=as.character(result$p_value),ci=result$CI,
                    lower = result$lower,upper = result$upper,participants=as.character(result$N),
                    events=as.character(result$cases))
rm(result)
header <- tibble(frailty_status='frailty_status',p_value='P value',ci='HR (95% CI)',participants='Participants',events='Events')
combined_data <- bind_rows(header,result_HR)
rm(header,result_HR)
combined_data %>% 
  forestplot(labeltext = c(frailty_status, ci,participants,events,p_value),
             mean=HR,
             graph.pos =3,boxsize=0.35,zero=1,
             is.summary = FALSE,clip = c(1,5), xlog = FALSE,graphwidth = unit(.35,"npc"),
             col = fpColors(box = "#356591",line = "black",summary = "grey"),
             mar=unit(rep(1.0, times = 4), "cm"),
             txt_gp=fpTxtGp(label=gpar(cex=0.8), ticks=gpar(cex=0.6), xlab=gpar(cex = 0.8), title=gpar(cex = 0.8)),
             xlab = "HR (95% CI)",lwd.xaxis = 1.0)
########################################################################################################################

####################################plot HR for adjustment model######################################################################
library(forestplot)
library(dplyr)
library(ggthemes)

rm(list=ls())
result<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_perm.csv',header=T)

result=rbind(result,result[1,])
result$hr_1[nrow(result)]=2.3
result$lower_1[nrow(result)]=2.3
result$upper_1[nrow(result)]=2.3
result$X[nrow(result)]=c('Null')

result_HR <- tibble(HR  = result$hr_1, adjustment = result$X,p_value=as.character(result$P_value_1),ci=result$HR_1,
                    lower = result$lower_1,upper = result$upper_1,perm=result$PERM_1)
rm(result)
header <- tibble(adjustment='Adjustment',ci='HR (95% CI)',perm='PERM')
combined_data <- bind_rows(header,result_HR)
rm(header,result_HR)
combined_data %>% 
  forestplot(labeltext = c(adjustment,ci,perm),
             mean=HR,
             graph.pos =2,boxsize=0.35,zero=1,
             is.summary = FALSE,clip = c(1,2.3), xlog = FALSE,graphwidth = unit(.3,"npc"),
             col = fpColors(box = "#356591",line = "black",summary = "grey"),
             mar=unit(rep(1.0, times = 4), "cm"),
             txt_gp=fpTxtGp(label=gpar(cex=0.8), ticks=gpar(cex=0.6), xlab=gpar(cex = 0.8), title=gpar(cex = 0.8)),
             xlab = "HR (95% CI)",lwd.xaxis = 1.0)

#frail
rm(list=ls())
result<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_perm.csv',header=T)

result=rbind(result,result[1,])
result$hr_2[nrow(result)]=5.5
result$lower_2[nrow(result)]=5.5
result$upper_2[nrow(result)]=5.5
result$X[nrow(result)]=c('Null')

result_HR <- tibble(HR  = result$hr_2, adjustment = result$X,p_value=as.character(result$P_value_2),ci=result$HR_2,
                    lower = result$lower_2,upper = result$upper_2,perm=result$PERM_2)
rm(result)
header <- tibble(adjustment='Adjustment',ci='HR (95% CI)',perm='PERM')
combined_data <- bind_rows(header,result_HR)
rm(header,result_HR)
combined_data %>% 
  forestplot(labeltext = c(adjustment,ci,perm),
             mean=HR,
             graph.pos =2,boxsize=0.35,zero=1,
             is.summary = FALSE,clip = c(1,5.6), xlog = FALSE,graphwidth = unit(.3,"npc"),
             col = fpColors(box = "#356591",line = "black",summary = "grey"),
             mar=unit(rep(1.0, times = 4), "cm"),
             txt_gp=fpTxtGp(label=gpar(cex=0.8), ticks=gpar(cex=0.6), xlab=gpar(cex = 0.8), title=gpar(cex = 0.8)),
             xlab = "HR (95% CI)",lwd.xaxis = 1.0)
########################################################################################################################

####################################plot component HR########################################################
result<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_components_Figure.csv',header=T)
library(ggplot2)
#behavior_order=order((result$HR_1))
ggplot(result, aes(x=X, y=HR_1)) +
  geom_pointrange(aes(ymin=Lower_1, ymax=Upper_1))+
  geom_point(data=result, mapping=aes(x=X, y=HR_1,fill=X), size=2.6, shape=21,stroke = 0.9)+
  scale_x_discrete(limits=c('Slow walking speed','Slow walking speed_1','Weakness','Weakness_1',
                            'Exhaustion','Exhaustion_1','Physical inactivity','Physical inactivity_1',
                            'Weight loss','Weight loss_1'))+
  theme_classic()+theme(axis.text.x = element_text(angle = 45,vjust = 0.9,hjust = 0.85))+coord_flip()
##############################################################################################################

####################################plot MR######################################################################
library(forestplot)
library(dplyr)
library(ggthemes)

result<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/mr.csv',header=T)
#result<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/mr_reverse_causality.csv',header=T)
result=result[c(1,2,3,5,4),]
result$or=round(result$or,2)
result$lower=round(result$or_lci95,2)
result$upper=round(result$or_uci95,2)
result$ci=paste(result$or,' (',result$lower,'-',result$upper,')',sep='')
result$pval=signif(result$pval,2)

result_OR <- tibble(OR  = result$or, mr_method = result$method,p_value=as.character(result$pval),ci=result$ci,
                    lower = result$lower,upper = result$upper)
rm(result)
header <- tibble(mr_method='MR method',p_value='P value',ci='OR (95% CI)')
combined_data <- bind_rows(header,result_OR)
rm(header,result_OR)
combined_data %>% 
  forestplot(labeltext = c(mr_method, ci,p_value),
             mean=OR,
             graph.pos =3,boxsize=0.35,zero=1,
             is.summary = FALSE,clip = c(0,14), xlog = FALSE,graphwidth = unit(.35,"npc"),
             col = fpColors(box = "#356591",line = "black",summary = "grey"),
             mar=unit(rep(1.0, times = 4), "cm"),
             txt_gp=fpTxtGp(label=gpar(cex=0.8), ticks=gpar(cex=0.6), xlab=gpar(cex = 0.8), title=gpar(cex = 0.8)),
             xlab = "HR (95% CI)",lwd.xaxis = 1.0)
########################################################################################################################

####################################plot HR broad definition############################################################
library(forestplot)
library(dplyr)
library(ggthemes)

result<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/HR_3_category_broad_definition.csv',header=T)

result_HR <- tibble(HR  = result$HR, frailty_status = result$X,p_value=as.character(result$p_value),ci=result$CI,
                    lower = result$lower,upper = result$upper,participants=as.character(result$N),
                    events=as.character(result$cases))
rm(result)
header <- tibble(frailty_status='frailty_status',p_value='P value',ci='HR (95% CI)',participants='Participants',events='Events')
combined_data <- bind_rows(header,result_HR)
rm(header,result_HR)
combined_data %>% 
  forestplot(labeltext = c(frailty_status, ci,participants,events,p_value),
             mean=HR,
             graph.pos =3,boxsize=0.35,zero=1,
             is.summary = FALSE,clip = c(1,5), xlog = FALSE,graphwidth = unit(.35,"npc"),
             col = fpColors(box = "#356591",line = "black",summary = "grey"),
             mar=unit(rep(1.0, times = 4), "cm"),
             txt_gp=fpTxtGp(label=gpar(cex=0.8), ticks=gpar(cex=0.6), xlab=gpar(cex = 0.8), title=gpar(cex = 0.8)),
             xlab = "HR (95% CI)",lwd.xaxis = 1.0)
########################################################################################################################
