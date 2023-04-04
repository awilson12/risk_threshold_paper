
#generate indifference points
source("newcastle_survey_data.R")

#calculate thresholds
source("threshold_calc.R")

#initiate lists for QMRA output saving
save.single.touch.list<-list()
save.day.touch.list<-list()

#QMRA model runs

source("qmra_model.R")

for (i in 1:length(runs)){
  
  QMRA_model(threshold=runs[i])
  
  save.single.touch.list[[i]]<-QMRA_model_output_single_touch
  
  save.day.touch.list[[i]]<-QMRA_model_output_day_touch
  
}

data<-save.single.touch.list[[1]]
data$threshold_compliance<-"No"
data$threshold_compliance[data$risk_single<data$threshold]<-"Yes"
data$maximum_allowable_conc<-min(data$C.surface[data$threshold_compliance=="No"])

ggplot(data)+geom_point(aes(x=C.surface,y=risk_single,color=threshold_compliance))+
  geom_hline(yintercept=data$threshold[1],color="red",linetype="dashed",lwd=1)+
  geom_vline(xintercept=data$maximum_allowable_conc[1],color="black",linetype="solid",lwd=1)+
  scale_x_continuous(trans="log10",name=expression("Concentration on Surfaces (PFU/cm"^2*")"))+
  scale_y_continuous(trans="log10",name="Infection Risk from Single Fomite Touch")+
  geom_label(aes(label=paste("Maximum Allowable Conc.",round(data$maximum_allowable_conc[1],1)),x=1e2,y=1e-8),fill="white")+
  geom_label(aes(label=paste("Risk Threshold",round(data$threshold[1],2)),x=1e-3,y=1e-1),fill="white")+
  scale_color_discrete(name="Threshold Compliance")+
  theme(legend.position="top",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.title=element_text(size=12),
        legend.text=element_text(size=12))
