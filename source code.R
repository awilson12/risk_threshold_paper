
#generate indifference points
source("newcastle_survey_data.R")

#calculate thresholds
source("threshold_calc.R")

save.single.touch.list<-list()
save.day.touch.list<-list()

#QMRA model runs

source("qmra_model.R")

modelrun<-c("median primary","median sensitivity",
            "mean primary", "mean sensitivity",
            "5th primary","5th sensitivity",
            "1st primary","1st sensitivity")

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

  
  p=ggplot(data)+geom_point(aes(x=C.surface,y=risk_single,color=threshold_compliance))+
    geom_hline(yintercept=data$threshold[1],color="red",linetype="dashed",lwd=1)+
    geom_vline(xintercept=data$maximum_allowable_conc[1],color="black",linetype="solid",lwd=1)+
    scale_x_continuous(trans="log10",name=expression("Concentration on Surfaces (PFU/cm"^2*")"))+
    scale_y_continuous(trans="log10",name="Infection Risk from Single Fomite Touch")+
    geom_label(aes(label=paste("Maximum Allowable Conc.",round(data$maximum_allowable_conc[1],2)),x=data$maximum_allowable_conc[1],y=1e-8),fill="white")+
    geom_label(aes(label=paste("Risk Threshold",round(data$threshold[1],3)),x=1e-3,y=data$threshold[1]),fill="white")+
    scale_color_discrete(name="Threshold Compliance")+
    theme(legend.position="top",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  ggsave(p,file=sprintf("single_fomite_%.3f.png",data$threshold[1]),width=7,height=4, units=c("in"))
  
  p2<-ggplot(data2)+geom_point(aes(x=C.surface,y=risk_daily,color=threshold_compliance))+
    geom_hline(yintercept=data2$threshold[1],color="red",linetype="dashed",lwd=1)+
    geom_vline(xintercept=data2$maximum_allowable_conc[1],color="black",linetype="solid",lwd=1)+
    scale_x_continuous(trans="log10",name=expression("Concentration on Surfaces (PFU/cm"^2*")"))+
    scale_y_continuous(trans="log10",name="Infection Risk from Daily Fomite Touch")+
    geom_label(aes(label=paste("Maximum Allowable Conc.",round(data2$maximum_allowable_conc[1],2)),x=data2$maximum_allowable_conc[1],y=1e-8),fill="white")+
    geom_label(aes(label=paste("Risk Threshold",round(data2$threshold[1],3)),x=1e-2,y=data2$threshold[1]),fill="white")+
    scale_color_discrete(name="Threshold Compliance")+
    theme(legend.position="top",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  ggsave(p2,file=sprintf("daily_fomite_%.3f.png",data2$threshold[1]),width=7,height=4, units=c("in"))

  
  
  
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
    geom_hline(yintercept=data$threshold[1],color="red",linetype="dashed",lwd=1)+
    geom_vline(xintercept=data$maximum_allowable_conc[1],color="black",linetype="solid",lwd=1)+
    scale_x_continuous(trans="log10",name=expression("Concentration on Surfaces (PFU/cm"^2*")"))+
    scale_y_continuous(trans="log10",name="Infection Risk from Single Fomite Touch")+
    geom_label(aes(label=paste("Maximum Allowable Conc.",round(data$maximum_allowable_conc[1],2)),x=data$maximum_allowable_conc[1],y=1e-8),fill="white")+
    geom_label(aes(label=paste("Risk Threshold",round(data$threshold[1],3)),x=1e-3,y=data$threshold[1]),fill="white")+
    scale_color_discrete(name="Threshold Compliance")+
    theme(legend.position="top",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  ggsave(p,file=sprintf("single_fomite_%.3f.png",data$threshold[1]),width=7,height=4, units=c("in"))
  

  #-------------- daily fomite touch plot---------------------------------------------------------------------------
  
  data2<-save.day.touch.list[[i]]
  data2$threshold_compliance<-"No"
  data2$threshold_compliance[data2$risk_daily<data2$threshold]<-"Yes"
  data2$maximum_allowable_conc<-min(data2$C.surface[data2$threshold_compliance=="No"])
  
  p2<-ggplot(data2)+geom_point(aes(x=C.surface,y=risk_daily,color=threshold_compliance))+
    geom_hline(yintercept=data2$threshold[1],color="red",linetype="dashed",lwd=1)+
    geom_vline(xintercept=data2$maximum_allowable_conc[1],color="black",linetype="solid",lwd=1)+
    scale_x_continuous(trans="log10",name=expression("Concentration on Surfaces (PFU/cm"^2*")"))+
    scale_y_continuous(trans="log10",name="Infection Risk from Daily Fomite Touch")+
    geom_label(aes(label=paste("Maximum Allowable Conc.",round(data2$maximum_allowable_conc[1],2)),x=data2$maximum_allowable_conc[1],y=1e-8),fill="white")+
    geom_label(aes(label=paste("Risk Threshold",round(data2$threshold[1],3)),x=1e-2,y=data2$threshold[1]),fill="white")+
    scale_color_discrete(name="Threshold Compliance")+
    theme(legend.position="top",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  ggsave(p2,file=sprintf("daily_fomite_%.3f.png",data2$threshold[1]),width=7,height=4, units=c("in"))
  
  
}

