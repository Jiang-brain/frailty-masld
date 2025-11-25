###################################association between effect sizes####################################
result_1  <- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/frailty_metabolites.csv',header=T)
result_2  <- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/metabolites_NAFLD_linear_nonlinear.csv',header=T)
phenotype<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2024-frailty_Nafld/datas/Metabolities_name.csv',header=T)
significance_group=rep(0,nrow(result_1))
index=which(result_1$P_value<0.05/251&result_2$P.for.linear<0.05/251)
significance_group[index]=1

result=data.frame(coeff=result_1$coeff,hr=result_2$HR,significance_group=significance_group,meta_name=phenotype$Biomarker)
result$significance_group=as.factor(result$significance_group)

ggplot(result,aes(x=coeff,y=hr))+
  geom_point(size=1.6,shape=19,aes(colour=significance_group))+
  scale_color_manual(values=c("1"='lightblue4',"0"="grey"))+theme_classic()+
  geom_segment(aes(x = -0.1, y =0.6, xend =0.1, yend =1.4),colour="grey",linewidth=0.6,linetype=1)+
  geom_text_repel(
    data = subset(result, abs(hr-1)>0.32|abs(coeff)>0.072),
    aes(label = meta_name),size = 1.5,angle=0,color='black',
    point.padding = unit(0.3, "lines"),segment.size = 0.2)
