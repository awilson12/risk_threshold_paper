
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

