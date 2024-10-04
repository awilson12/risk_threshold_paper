
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


#------- Table 5 Results-------------------------------------------------------------------------------------
for (a in 1:length(modelrun)){
  means_daily[a]<-mean(frame.max.conc$max_allow_daily_total[frame.max.conc$modelall==modelrun[a]])
  means_single[a]<-mean(frame.max.conc$max_allow_single_total[frame.max.conc$modelall==modelrun[a]])
  sds_daily[a]<-sd(frame.max.conc$max_allow_daily_total[frame.max.conc$modelall==modelrun[a]])
  sds_single[a]<-sd(frame.max.conc$max_allow_single_total[frame.max.conc$modelall==modelrun[a]])
}

frame.summary<-data.frame(modelrun,means_daily,sds_daily,means_single,sds_single)
View(frame.summary)

#--------------calculating needed log10 reductions to reach threshold-------------------------
  
  #adjusting for 1/1,000 infectious particle:gc ratio
  gym.water.fountain<-6.78E-3*(1E-3)
  office.surface<-1.77E-1*(1E-3)
  classroom.desk<-7.95E-2*(1E-3)
  bus.surface<-1.68E-1*(1E-3)
  liquor.store<-102.43*(1E-3)
  grocery.store<-11.55*(1E-3)
  
  log10(liquor.store/frame.summary$means_daily[frame.summary$modelrun=="1st Dist. 1"])
  log10(4.9E-3/5.8E-4)
  log10(330/39)
  log10(330/4.9E-3)
  