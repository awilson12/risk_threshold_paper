threshold_median_primary<-median(indiff_sim)
threshold_mean_primary<-mean(indiff_sim)
threshold_5th_primary<-quantile(indiff_sim,c(.05))
threshold_1st_primary<-quantile(indiff_sim,c(.01))

threshold_median_sensitivity<-median(S1_indiff_sim)
threshold_mean_sensitivity<-mean(S1_indiff_sim)
threshold_5th_sensitivity<-quantile(S1_indiff_sim,c(.05))
threshold_1st_sensitivity<-quantile(S1_indiff_sim,c(.01))

runs<-c(threshold_median_primary,threshold_median_sensitivity,
        threshold_mean_primary,threshold_mean_sensitivity,
        threshold_5th_primary,threshold_5th_sensitivity,
        threshold_1st_primary,threshold_1st_sensitivity)
