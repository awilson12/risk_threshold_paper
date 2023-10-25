
#generate indifference points
source("newcastle_survey_data.R")

#calculate thresholds
source("threshold_calc.R")

save.single.touch.list<-list()
save.day.touch.list<-list()

#QMRA model runs

source("qmra_model.R")

modelrun<-c("Median Dist. 1","Median Dist. 2",
            "Mean Dist. 1", "Mean Dist. 2",
            "5th Dist. 1","5th Dist. 2",
            "1st Dist. 1","1st Dist. 2")

iterations.total<-10000


  
  for (i in 1:length(runs)){
    
    for (k in 1:iterations.total){
    
    require(truncdist)
    
    #print(i)
    
    model<-modelrun[i]
    
    #run QMRA model (single fomite touch and daily fomite exposure)
    QMRA_model(threshold=runs[i],model=model)
    
    #save output from single fomite touch model
    save.single.touch.list[[i]]<-QMRA_model_output_single_touch
    
    #save output from daily fomite exposure model
    save.day.touch.list[[i]]<-QMRA_model_output_day_touch
    
    
    #------------ single fomite touch plot--------------------------------------------------------------
    
    #extracting data from list to do plotting
    data<-save.single.touch.list[[i]]
    data$threshold_compliance<-"No"
    data$threshold_compliance[data$risk_single<data$threshold]<-"Yes"
    max_allow_single<-min(data$C.surface[data$threshold_compliance=="No"])
    
    #-------------- daily fomite touch plot---------------------------------------------------------------------------
    
    data2<-save.day.touch.list[[i]]
    data2$threshold_compliance<-"No"
    data2$threshold_compliance[data2$risk_daily<data2$threshold]<-"Yes"
    max_allow_daily<-min(data2$C.surface[data2$threshold_compliance=="No"])
    
    #print(model)
    
    if (k==1 & i==1){
      max_allow_single_total<-max_allow_single
      max_allow_daily_total<-max_allow_daily
      modelall<-model
    }else{
      max_allow_single_total<-c(max_allow_single_total,max_allow_single)
      max_allow_daily_total<-c(max_allow_daily_total,max_allow_daily)
      modelall<-c(modelall,model)
    } 
    
    
    } #end of iterations through 10,000 sims of reverse QMRA 
 
  
  } #end of iterations through model types
  
  frame.max.conc<-data.frame(max_allow_single_total,max_allow_daily_total,modelall)
  
means_daily<-rep(NA,length(modelrun))
means_single<-rep(NA,length(modelrun))
sds_daily<-rep(NA,length(modelrun))
sds_single<-rep(NA,length(modelrun))

for (a in 1:length(modelrun)){
  means_daily[a]<-mean(frame.max.conc$max_allow_daily_total[frame.max.conc$modelall==modelrun[a]])
  means_single[a]<-mean(frame.max.conc$max_allow_single_total[frame.max.conc$modelall==modelrun[a]])
  sds_daily[a]<-sd(frame.max.conc$max_allow_daily_total[frame.max.conc$modelall==modelrun[a]])
  sds_single[a]<-sd(frame.max.conc$max_allow_single_total[frame.max.conc$modelall==modelrun[a]])
}

frame.summary<-data.frame(modelrun,means_daily,sds_daily,means_single,sds_single)
View(frame.summary)




#------------------ old figure code-----------------------------------------------------

  
 # p=ggplot(data)+geom_point(aes(x=C.surface,y=risk_single,color=threshold_compliance))+
#    geom_hline(yintercept=data$threshold[1],color="red",linetype="dashed",lwd=1)+
#    geom_vline(xintercept=max_allow_single,color="black",linetype="solid",lwd=1)+
#    scale_x_continuous(trans="log10",name=expression("Concentration on Surfaces (PFU/cm"^2*")"))+
#    scale_y_continuous(trans="log10",name="Infection Risk from Single Fomite Touch")+
#    geom_label(aes(label=paste("Maximum Allowable Conc.",round(max_allow_single,2)),x=max_allow_single,y=1e-8),fill="white")+
#    geom_label(aes(label=paste("Risk Threshold",round(data$threshold[1],3)),x=1e-3,y=data$threshold[1]),fill="white")+
#    scale_color_discrete(name="Threshold Compliance")+
#    theme(legend.position="top",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.title=element_text(size=12),
#          legend.text=element_text(size=12))
#  ggsave(p,file=sprintf("single_fomite_%.3f.png",data$threshold[1]),width=7,height=4, units=c("in"))
  
#  p2<-ggplot(data2)+geom_point(aes(x=C.surface,y=risk_daily,color=threshold_compliance))+
#    geom_hline(yintercept=data2$threshold[1],color="red",linetype="dashed",lwd=1)+
#    geom_vline(xintercept=max_allow_daily,color="black",linetype="solid",lwd=1)+
#    scale_x_continuous(trans="log10",name=expression("Concentration on Surfaces (PFU/cm"^2*")"))+
#    scale_y_continuous(trans="log10",name="Infection Risk from Daily Fomite Touch")+
#    geom_label(aes(label=paste("Maximum Allowable Conc.",round(max_allow_daily,2)),x=max_allow_daily,y=1e-8),fill="white")+
#    geom_label(aes(label=paste("Risk Threshold",round(data2$threshold[1],3)),x=1e-2,y=data2$threshold[1]),fill="white")+
#    scale_color_discrete(name="Threshold Compliance")+
#    theme(legend.position="top",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.title=element_text(size=12),
#          legend.text=element_text(size=12))
# ggsave(p2,file=sprintf("daily_fomite_%.3f.png",data2$threshold[1]),width=7,height=4, units=c("in"))

  
  
  
for (i in 1:length(runs)){
  
  print(i)
  
  model<-modelrun[i]
  
  #run QMRA model (single fomite touch and daily fomite exposure)
  QMRA_model(threshold=runs[i],model=model)
  
  #save output from single fomite touch model
  save.single.touch.list[[i]]<-QMRA_model_output_single_touch
  
  #save output from daily fomite exposure model
  save.day.touch.list[[i]]<-QMRA_model_output_day_touch
  
  
  #------------ single fomite touch plot--------------------------------------------------------------
  
  #extracting data from list to do plotting
  data<-save.single.touch.list[[i]]
  data$threshold_compliance<-"No"
  data$threshold_compliance[data$risk_single<data$threshold]<-"Yes"
  data$maximum_allowable_conc<-min(data$C.surface[data$threshold_compliance=="No"])
  
   p=ggplot(data)+geom_point(aes(x=C.surface,y=risk_single,color=threshold_compliance))+
    geom_hline(yintercept=data$threshold[1],color="grey",linetype="dashed",lwd=1)+
    geom_vline(xintercept=max_allow_single,color="black",linetype="solid",lwd=1)+
    scale_x_continuous(trans="log10",name=expression("Concentration on Surfaces (viral particles/cm"^2*")"))+
    scale_y_continuous(trans="log10",name="Infection Risk from Single Fomite Touch")+
    geom_label(aes(label=paste("Maximum Allowable Conc.",round(max_allow_single,2)),x=max_allow_single,y=1e-8),fill="white")+
    geom_label(aes(label=paste("Risk Threshold",round(data$threshold[1],3)),x=1e-3,y=data$threshold[1]),fill="white")+
     scale_color_manual(name="Threshold Compliance",values=c("#E69F00", "#56B4E9"))+
     theme(legend.position="top",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  ggsave(p,file=sprintf("single_fomite_%.3f.png",data$threshold[1]),width=7,height=4, units=c("in"))
  

  #-------------- daily fomite touch plot---------------------------------------------------------------------------
  
  data2<-save.day.touch.list[[i]]
  data2$threshold_compliance<-"No"
  data2$threshold_compliance[data2$risk_daily<data2$threshold]<-"Yes"
  data2$maximum_allowable_conc<-min(data2$C.surface[data2$threshold_compliance=="No"])
  
  p2<-ggplot(data2)+geom_point(aes(x=C.surface,y=risk_daily,color=threshold_compliance))+
    geom_hline(yintercept=data2$threshold[1],color="grey",linetype="dashed",lwd=1)+
    geom_vline(xintercept=max_allow_daily,color="black",linetype="solid",lwd=1)+
    scale_x_continuous(trans="log10",name=expression("Concentration on Surfaces (viral particles/cm"^2*")"))+
    scale_y_continuous(trans="log10",name="Infection Risk from Daily Fomite Touch")+
    geom_label(aes(label=paste("Maximum Allowable Conc.",round(max_allow_daily,2)),x=max_allow_daily,y=1e-8),fill="white")+
    geom_label(aes(label=paste("Risk Threshold",round(data2$threshold[1],3)),x=1e-2,y=data2$threshold[1]),fill="white")+
    scale_color_manual(name="Threshold Compliance",values=c("#E69F00", "#56B4E9"))+
    theme(legend.position="top",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  ggsave(p2,file=sprintf("daily_fomite_%.3f.png",data2$threshold[1]),width=7,height=4, units=c("in"))
  
  
}
  
  frame.summary2<-data.frame(means=c(frame.summary$means_daily,frame.summary$means_single),
                             SDs=c(frame.summary$sds_daily,frame.summary$sds_single),
                             type=c(rep("Daily",length(frame.summary$means_daily)),
                                    rep("Single Touch",length(frame.summary$means_single))),
                             model=rep(frame.summary$modelrun,2),
                             distribution=rep(c("Indifference Point Distribution 1","Indifference Point Distribution 2"),8),
                             percentile=c("Median","Median","Mean","Mean",
                                          "5th Percentile","5th Percentile","1st Percentile","1st Percentile"))
  
  
  #windows()
  #ggplot(frame.summary2,aes(x=interaction(percentile,distribution),y=means,fill=distribution))+
  #  geom_bar(stat="identity",alpha=0.8)+
  #  geom_errorbar(aes(ymin=means-SDs,ymax=means+SDs),width=.2,
  #                position=position_dodge(.9))+
  #  scale_y_continuous(trans="log10",name="Concentration Thresholds (Mean +/- SD)")+
  #  scale_x_discrete(name="",labels=c("1st Percentile","5th Percentile","Mean","Median",
  #                                    "1st Percentile","5th Percentile","Mean","Median"))+
  #  scale_fill_manual(name="",values=c("#E69F00", "#56B4E9"))+
  # theme_pubr()+
  #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),axis.text = element_text(size=16),
  #        axis.title=element_text(size=16),legend.text = element_text(size=16),strip.text = element_text(size=16))+
  #  facet_wrap(~type)
  
  frame.max.conc2<-data.frame(conc=c(frame.max.conc$max_allow_single_total,frame.max.conc$max_allow_daily_total),
                              type=c(rep("Single Touch",length(frame.max.conc$max_allow_single_total)),
                                     rep("Daily Exposure",length(frame.max.conc$max_allow_daily_total))),
                              model=rep(frame.max.conc$modelall,2))
  
  frame.max.conc2$distribution<-NA
  frame.max.conc2$distribution[frame.max.conc$model=="1st Dist. 1" |
                               frame.max.conc$model=="5th Dist. 1" |
                               frame.max.conc$model=="Mean Dist. 1" |
                               frame.max.conc$model=="Median Dist. 1"]<-"Indifference Point Distribution 1"
  frame.max.conc2$distribution[frame.max.conc$model=="1st Dist. 2" |
                                 frame.max.conc$model=="5th Dist. 2" |
                                 frame.max.conc$model=="Mean Dist. 2" |
                                 frame.max.conc$model=="Median Dist. 2"]<-"Indifference Point Distribution 2"
  frame.max.conc2$model[frame.max.conc$model=="1st Dist. 1" |frame.max.conc$model=="1st Dist. 2"]<-"1st Percentile"
  frame.max.conc2$model[frame.max.conc$model=="5th Dist. 1" | frame.max.conc$model=="5th Dist. 2"]<-"5th Percentile"
  frame.max.conc2$model[frame.max.conc$model=="Median Dist. 1" |frame.max.conc$model== "Median Dist. 2"]<-"Median"
  frame.max.conc2$model[frame.max.conc$model=="Mean Dist. 1" |frame.max.conc$model== "Mean Dist. 2"]<-"Mean"
  
  
  
  windows()
  ggplot(frame.max.conc2,aes(x=interaction(model,distribution),y=conc,fill=distribution))+
    geom_violin(draw_quantiles = c(0.25,0.5,0.75))+
    facet_grid(~type)+
    scale_y_continuous(trans="log10",name="Concentration Thresholds (Mean +/- SD)")+
    scale_x_discrete(name="",labels=c("1st Percentile","5th Percentile","Mean","Median",
                                      "1st Percentile","5th Percentile","Mean","Median"))+
    scale_fill_manual(name="",values=c("#E69F00", "#56B4E9"))+
    theme_pubr()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),axis.text = element_text(size=16),
          axis.title=element_text(size=16),legend.text = element_text(size=16),strip.text = element_text(size=16))+
    geom_hline(yintercept = 6.78E3)+
    annotate("rect",alpha=.2,fill="grey",xmin=0,xmax=9,ymin=1.68E-1*(1E-3),ymax=6.73E3*(1E-3))
  

#--------------calculating needed log10 reductions to reach threshold-------------------------
  
  gym.water.fountain<-6.78E-3*(1E-3)
  office.surface<-1.77E-1*(1E-3)
  classroom.desk<-7.95E-2*(1E-3)
  bus.surface<-1.68E-1*(1E-3)
  liquor.store<-102.43*(1E-3)
  grocery.store<-11.55*(1E-3)
  
  reduction.gym.water.fountain.single<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.office.surface.single<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.classroom.desk.single<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.bus.surface.single<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.liquor.store.single<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.grocery.store.single<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  
  reduction.gym.water.fountain.daily<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.office.surface.daily<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.classroom.desk.daily<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.bus.surface.daily<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.liquor.store.daily<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  reduction.grocery.store.daily<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  
  threshold.single<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  threshold.daily<-rep(NA,length(frame.max.conc$max_allow_single_daily))
  
  for (i in 1:length(frame.max.conc$max_allow_single_total)){
    reduction.gym.water.fountain.single[i]<-log10(gym.water.fountain/frame.max.conc$max_allow_single_total[i])
    reduction.office.surface.single[i]<-log10(office.surface/frame.max.conc$max_allow_single_total[i])
    reduction.classroom.desk.single[i]<-log10(classroom.desk/frame.max.conc$max_allow_single_total[i])
    reduction.bus.surface.single[i]<-log10(bus.surface/frame.max.conc$max_allow_single_total[i])
    reduction.liquor.store.single[i]<-log10(liquor.store/frame.max.conc$max_allow_single_total[i])
    reduction.grocery.store.single[i]<-log10(grocery.store/frame.max.conc$max_allow_single_total[i])
    threshold.single[i]<-frame.max.conc$modelall[i]
  
    reduction.gym.water.fountain.daily[i]<-log10(gym.water.fountain/frame.max.conc$max_allow_daily_total[i])
    reduction.office.surface.daily[i]<-log10(office.surface/frame.max.conc$max_allow_daily_total[i])
    reduction.classroom.desk.daily[i]<-log10(classroom.desk/frame.max.conc$max_allow_daily_total[i])
    reduction.bus.surface.daily[i]<-log10(bus.surface/frame.max.conc$max_allow_daily_total[i])
    reduction.liquor.store.daily[i]<-log10(liquor.store/frame.max.conc$max_allow_daily_total[i])
    reduction.grocery.store.daily[i]<-log10(grocery.store/frame.max.conc$max_allow_daily_total[i])
    threshold.daily[i]<-frame.max.conc$modelall[i]
    
    
  }
  
  
  reductions<-c(reduction.gym.water.fountain.daily,reduction.office.surface.daily,reduction.classroom.desk.daily,reduction.bus.surface.daily,
                reduction.liquor.store.daily,reduction.grocery.store.daily)
  thresholds<-rep(threshold.daily,6)
  risk<-c(rep("Daily Exposure",length(c(reduction.gym.water.fountain.daily,reduction.office.surface.daily,reduction.classroom.desk.daily,reduction.bus.surface.daily,
                             reduction.liquor.store.daily,reduction.grocery.store.daily))))
  environments<-c(rep("Gym Water Fountain",length(reduction.gym.water.fountain.single)),rep("Office Surface",length(reduction.office.surface.single)),
                 rep("Classroom Desk",length(reduction.classroom.desk.single)),rep("Bus Surface",length(reduction.bus.surface.single)),
                 rep("Liquor Store Handle",length(reduction.liquor.store.single)),rep("Grocery Store Handle",length(reduction.grocery.store.single)))
  
  frame.reductions<-data.frame(reductions,risk,environments,thresholds)
  
  frame.reductions$environments<-factor(frame.reductions$environments,levels=c("Liquor Store Handle","Grocery Store Handle","Office Surface",
                                                                               "Bus Surface","Classroom Desk","Gym Water Fountain"))
  
  frame.reductions$thresholds[frame.reductions$thresholds=="1st primary"]<-"1st Dist. 1"
  frame.reductions$thresholds[frame.reductions$thresholds=="5th primary"]<-"5th Dist. 1"
  frame.reductions$thresholds[frame.reductions$thresholds=="1st sensitivity"]<-"1st Dist. 2"
  frame.reductions$thresholds[frame.reductions$thresholds=="5th sensitivity"]<-"5th Dist. 2"
  frame.reductions$thresholds[frame.reductions$thresholds=="mean primary"]<-"Mean Dist. 1"
  frame.reductions$thresholds[frame.reductions$thresholds=="mean sensitivity"]<-"Mean Dist. 2"
  frame.reductions$thresholds[frame.reductions$thresholds=="median primary"]<-"Median Dist. 1"
  frame.reductions$thresholds[frame.reductions$thresholds=="median sensitivity"]<-"Median Dist. 2"
  
  frame.reductions$thresholds<-factor(frame.reductions$thresholds,levels=c("1st Dist. 1", "5th Dist. 1", "Median Dist. 1", "Mean Dist. 1",
                                                                           "1st Dist. 2", "5th Dist. 2", "Median Dist. 2", "Mean Dist. 2"))
  
  
  windows()
  ggplot(frame.reductions,aes(x=environments,y=reductions))+
    geom_violin(aes(fill=environments),draw_quantiles = c(0.25,0.5,0.75),alpha=0.4)+
    scale_y_continuous(name=expression("Required Log"[10]*phantom(x)*"Reductions"))+
    scale_x_discrete(name="")+
    geom_hline(yintercept = 0,linetype="dashed",size=1)+
    theme_pubr()+
    coord_flip()+
    facet_wrap(~thresholds,ncol=4)+
    theme(legend.position = "none",axis.text = element_text(size=14),axis.title = element_text(size=14),
          strip.text = element_text(size=14))
  
  #-------gathering summary stats for table for supplemental materials--------------
  
  summary.threshold<-function(surface,thresholdtype){
    print(mean(frame.reductions$reductions[frame.reductions$environments==surface & frame.reductions$thresholds==thresholdtype]))
    print(sd(frame.reductions$reductions[frame.reductions$environments==surface & frame.reductions$thresholds==thresholdtype]))
    }
  
  summary.threshold("Gym Water Fountain","1st Dist. 1")
  summary.threshold("Office Surface","1st Dist. 1")
  summary.threshold("Classroom Desk","1st Dist. 1")
  summary.threshold("Bus Surface","1st Dist. 1")
  summary.threshold("Liquor Store Handle","1st Dist. 1")
  summary.threshold("Grocery Store Handle","1st Dist. 1")
  
  summary.threshold("Gym Water Fountain","5th Dist. 1")
  summary.threshold("Office Surface","5th Dist. 1")
  summary.threshold("Classroom Desk","5th Dist. 1")
  summary.threshold("Bus Surface","5th Dist. 1")
  summary.threshold("Liquor Store Handle","5th Dist. 1")
  summary.threshold("Grocery Store Handle","5th Dist. 1")
  
  summary.threshold("Gym Water Fountain","Median Dist. 1")
  summary.threshold("Office Surface","Median Dist. 1")
  summary.threshold("Classroom Desk","Median Dist. 1")
  summary.threshold("Bus Surface","Median Dist. 1")
  summary.threshold("Liquor Store Handle","Median Dist. 1")
  summary.threshold("Grocery Store Handle","Median Dist. 1")
  
  summary.threshold("Gym Water Fountain","Mean Dist. 1")
  summary.threshold("Office Surface","Mean Dist. 1")
  summary.threshold("Classroom Desk","Mean Dist. 1")
  summary.threshold("Bus Surface","Mean Dist. 1")
  summary.threshold("Liquor Store Handle","Mean Dist. 1")
  summary.threshold("Grocery Store Handle","Mean Dist. 1")
  
  
  
  
  summary.threshold("Gym Water Fountain","1st Dist. 2")
  summary.threshold("Office Surface","1st Dist. 2")
  summary.threshold("Classroom Desk","1st Dist. 2")
  summary.threshold("Bus Surface","1st Dist. 2")
  summary.threshold("Liquor Store Handle","1st Dist. 1")
  summary.threshold("Grocery Store Handle","1st Dist. 1")
  
  summary.threshold("Gym Water Fountain","5th Dist. 1")
  summary.threshold("Office Surface","5th Dist. 1")
  summary.threshold("Classroom Desk","5th Dist. 1")
  summary.threshold("Bus Surface","5th Dist. 1")
  summary.threshold("Liquor Store Handle","5th Dist. 1")
  summary.threshold("Grocery Store Handle","5th Dist. 1")
  
  summary.threshold("Gym Water Fountain","Median Dist. 1")
  summary.threshold("Office Surface","Median Dist. 1")
  summary.threshold("Classroom Desk","Median Dist. 1")
  summary.threshold("Bus Surface","Median Dist. 1")
  summary.threshold("Liquor Store Handle","Median Dist. 1")
  summary.threshold("Grocery Store Handle","Median Dist. 1")
  
  summary.threshold("Gym Water Fountain","Mean Dist. 1")
  summary.threshold("Office Surface","Mean Dist. 1")
  summary.threshold("Classroom Desk","Mean Dist. 1")
  summary.threshold("Bus Surface","Mean Dist. 1")
  summary.threshold("Liquor Store Handle","Mean Dist. 1")
  summary.threshold("Grocery Store Handle","Mean Dist. 1")
  