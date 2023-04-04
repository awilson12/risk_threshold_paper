library(readxl)
Data_raw1303 <- read_excel("Data_raw1303.xlsx")

#Sub-setting so only including responses for total risk (no marginal risk)
#The reason for this is our study is focused on total risk
Data<-Data_raw1303[Data_raw1303$Marginal_total==2 & Data_raw1303$Order==1,]

#-----------------------area 1 vs. area 2---------------------

a1v2.area1W<-c(5355,5730,6561,7511,9201,11272,14776,19368,27164,38900)/100000 #Risk of W in Area 1
a1v2.area2W<-5000/100000 #Risk of W in Area 2

a1v2.area1R<-600/100000 #Risk of R in Area 1
a1v2.area2R<-c(641,721,858,1083,1450,2057,3093,4929,8328,14913)/100000 #Risk of R in Area 2

#-----------------------area 1 vs. area 3---------------------

a1v3.area1W<-c(5355,5730,6561,7511,9201,11272,14776,19368,27164,38900)/100000 #Risk of W in Area 1
a1v3.area3W<-5000/100000 #Risk of W in Area 3

a1v3.area1F<-50/100000 #Risk of F in area 1
a1v3.area3F<-c(58,66,78,98,132,187,281,448,757,1356)/100000

#-----------------------area 2 vs. area 3---------------------

a2v3.area2R<-c(641,721,858,1083,1450,2057,3093,4929,8328,14913)/100000 #Risk of R in Area 2
a2v3.area3R<-600/100000 #risk of R in area 3

a2v3.area2F<-50/100000 #risk of F in area 2
a2v3.area3F<-c(58,66,78,98,132,187,281,448,757,1356)/100000 #risk of F in area 3

#Codebook vars are the same regardless of order in which survey was taken. However,
#some participants took the survey more than once. This is accounted for so that
#distributions do not represent any 1 individual more than once.

Data$acceptable_risk_W_q2<-NA
Data$acceptable_risk_R_q2<-NA

Data$acceptable_risk_W_q6<-NA
Data$acceptable_risk_F_q6<-NA

Data$acceptable_risk_R_q10<-NA
Data$acceptable_risk_F_q10<-NA


for (i in 1:10){
  
  #area 1 v 2----------------------------------------------------------
  Data$acceptable_risk_W_q2[Data$q1==1 & Data$q2==i]<-a1v2.area1W[i]
  Data$acceptable_risk_W_q2[Data$q1==2 & Data$q2==i]<-a1v2.area2W
  
  Data$acceptable_risk_R_q2[Data$q1==1 & Data$q2==i]<-a1v2.area1R
  Data$acceptable_risk_R_q2[Data$q1==2 & Data$q2==i]<-a1v2.area2R[i]
  
  #area 1 v 3----------------------------------------------------------
  Data$acceptable_risk_W_q6[Data$q5==1 & Data$q6==i]<-a1v3.area1W[i]
  Data$acceptable_risk_W_q6[Data$q5==3 & Data$q6==i]<-a1v3.area3W
  
  Data$acceptable_risk_F_q6[Data$q5==1 & Data$q6==i]<-a1v3.area1F
  Data$acceptable_risk_F_q6[Data$q5==3 & Data$q6==i]<-a1v3.area3F[i]
  
  #area 2 v 3----------------------------------------------------------
  Data$acceptable_risk_R_q10[Data$q9==2 & Data$q10==i]<-a2v3.area2R[i]
  Data$acceptable_risk_R_q10[Data$q9==3 & Data$q10==i]<-a2v3.area3R
  
  Data$acceptable_risk_F_q10[Data$q9==2 & Data$q2==i]<-a2v3.area2F
  Data$acceptable_risk_F_q10[Data$q9==3 & Data$q2==i]<-a2v3.area3F[i]
}

#Here we subset by order since some participants took the survey more than once (but different order) and will potentially have
#different values for q3
for (j in 1:max(Data$`Subject number`)){
  
  #area 1 v 2-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Data$acceptable_risk_W_q2[Data$q2==11 & Data$`Subject number`==j & Data$Order==1 & Data$q1==1]<-as.numeric(Data$q3[Data$q2==11 & Data$`Subject number`==j & Data$Order==1])/100000
  #Data$acceptable_risk_W_q2[Data$q2==11 & Data$`Subject number`==j & Data$Order==2 & Data$q1==1]<-as.numeric(Data$q3[Data$q2==11 & Data$`Subject number`==j & Data$Order==2])/100000
  
  Data$acceptable_risk_R_q2[Data$q2==11 & Data$`Subject number`==j & Data$Order==1 & Data$q1==2]<-as.numeric(Data$q3[Data$q2==11 & Data$`Subject number`==j & Data$Order==1])/100000
  #Data$acceptable_risk_R_q2[Data$q2==11 & Data$`Subject number`==j & Data$Order==2 & Data$q1==2]<-as.numeric(Data$q3[Data$q2==11 & Data$`Subject number`==j & Data$Order==2])/100000
  
  #area 1 v 3-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Data$acceptable_risk_W_q6[Data$q6==11 & Data$`Subject number`==j & Data$Order==1 & Data$q5==1]<-as.numeric(Data$q7[Data$q6==11 & Data$`Subject number`==j & Data$Order==1])/100000
  #Data$acceptable_risk_W_q6[Data$q6==11 & Data$`Subject number`==j & Data$Order==2 & Data$q5==1]<-as.numeric(Data$q7[Data$q6==11 & Data$`Subject number`==j & Data$Order==2])/100000
  
  Data$acceptable_risk_F_q6[Data$q6==11 & Data$`Subject number`==j & Data$Order==1 & Data$q5==3]<-as.numeric(Data$q7[Data$q6==11 & Data$`Subject number`==j & Data$Order==1])/100000
  #Data$acceptable_risk_F_q6[Data$q6==11 & Data$`Subject number`==j & Data$Order==2 & Data$q5==3]<-as.numeric(Data$q7[Data$q6==11 & Data$`Subject number`==j & Data$Order==2])/100000
  
  #area 2 v 3-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Data$acceptable_risk_R_q10[Data$q10==11 & Data$`Subject number`==j & Data$Order==1 & Data$q9==2]<-as.numeric(Data$q11[Data$q10==11 & Data$`Subject number`==j & Data$Order==1])/100000
  #Data$acceptable_risk_R_q10[Data$q10==11 & Data$`Subject number`==j & Data$Order==2 & Data$q9==2]<-as.numeric(Data$q11[Data$q10==11 & Data$`Subject number`==j & Data$Order==2])/100000
  
  Data$acceptable_risk_F_q10[Data$q10==11 & Data$`Subject number`==j & Data$Order==1 & Data$q9==3]<-as.numeric(Data$q11[Data$q10==11 & Data$`Subject number`==j & Data$Order==1])/100000
  #Data$acceptable_risk_F_q10[Data$q10==11 & Data$`Subject number`==j & Data$Order==2 & Data$q9==3]<-as.numeric(Data$q11[Data$q10==11 & Data$`Subject number`==j & Data$Order==2])/100000
  
}
  
#We want between variance not within. So,
#I am subsetting by order to ensure only 1 answer per participant is represented
#in the distribution of responses.

#hist((as.numeric(Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_W_q2)])))
#hist((as.numeric(Data$acceptable_risk_R_q2[!is.na(Data$acceptable_risk_R_q2)])))

require(ggplot2)
require(ggpubr)

data_compare<-data.frame(
                         #Acceptable Risks
                         risks=c(as.numeric(Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_W_q2)]),
                                 as.numeric(Data$acceptable_risk_R_q2[!is.na(Data$acceptable_risk_R_q2)]),
                                 as.numeric(Data$acceptable_risk_W_q6[!is.na(Data$acceptable_risk_W_q6)]),
                                 as.numeric(Data$acceptable_risk_F_q6[!is.na(Data$acceptable_risk_F_q6)]),
                                 as.numeric(Data$acceptable_risk_R_q10[!is.na(Data$acceptable_risk_R_q10)]),
                                 as.numeric(Data$acceptable_risk_F_q10[!is.na(Data$acceptable_risk_F_q10)])),
                         
                        #Type of Risk Outcome
                         type=c(rep("W",length(c(as.numeric(Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_W_q2)])))),
                                rep("R",length((Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_R_q2)]))),
                                rep("W",length((Data$acceptable_risk_W_q6[!is.na(Data$acceptable_risk_W_q6)]))),
                                rep("F",length((Data$acceptable_risk_F_q6[!is.na(Data$acceptable_risk_F_q6)]))),
                                rep("R",length((Data$acceptable_risk_R_q10[!is.na(Data$acceptable_risk_R_q10)]))),
                                rep("F",length((Data$acceptable_risk_F_q10[!is.na(Data$acceptable_risk_F_q10)])))),
                         
                        #Type of Survey
                         survey=c(rep("Area 1 v 2",length(c(rep("W",length(c(as.numeric(Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_W_q2)])))),
                                                            rep("R",length((Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_R_q2)])))))),
                                  rep("Area 1 v 3",length(c(rep("W",length((Data$acceptable_risk_W_q6[!is.na(Data$acceptable_risk_W_q6)]))),
                                                          rep("F",length((Data$acceptable_risk_F_q6[!is.na(Data$acceptable_risk_F_q6)])))))),
                                  rep("Area 2 v 3",length(c(rep("R",length((Data$acceptable_risk_R_q10[!is.na(Data$acceptable_risk_R_q10)]))),
                                                          rep("F",length((Data$acceptable_risk_F_q10[!is.na(Data$acceptable_risk_F_q10)])))))))
                        )



windows()
ggplot(data_compare,aes(fill=type,group=survey,x=risks))+geom_histogram(alpha=0.5,color="black")+facet_grid(type~survey)+
  scale_x_continuous(trans="log10",name="Risks")+
  scale_y_continuous(name="Number of Respondents")+
  theme_bw()+
  theme(legend.position = "none")

#Focusing on fitting distributions for W in both Area 1 v2 and 
#Area 1 v 3 and for R in Area 2 v 3

require(fitdistrplus)

#------------------ W in Area 1 v 2------------------------------------------------------------------------

weibull_W_q2<-fitdist(as.numeric(Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_W_q2)]),"weibull")
plot(weibull_W_q2)
exponential_W_q2<-fitdist(as.numeric(Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_W_q2)]),"exp")
plot(exponential_W_q2)
lognormal_W_q2<-fitdist(as.numeric(Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_W_q2)]),"lnorm")
plot(lognormal_W_q2)
gamma_W_q2<-fitdist(as.numeric(Data$acceptable_risk_W_q2[!is.na(Data$acceptable_risk_W_q2)]),"gamma")
plot(gamma_W_q2)

denscomp(list(weibull_W_q2,exponential_W_q2,lognormal_W_q2,gamma_W_q2),legendtext=c("Weibull","Exponential","Lognormal","Gamma"),plotstyle="ggplot") +
  geom_line(linetype = "dashed",size = 1)+ theme_bw()+ggtitle("Distribution Fit Comparison for Acceptable W Risks, Area 1 vs 2")

stats.1<-gofstat(list(weibull_W_q2,exponential_W_q2,lognormal_W_q2,gamma_W_q2))
stats.1$chisqpvalue
stats.1$kstest
#---------------- W in Area 1 v 3--------------------------------------------------------------------------

weibull_W_q6<-fitdist(as.numeric(Data$acceptable_risk_W_q6[!is.na(Data$acceptable_risk_W_q6)]),"weibull")
plot(weibull_W_q6)
exponential_W_q6<-fitdist(as.numeric(Data$acceptable_risk_W_q6[!is.na(Data$acceptable_risk_W_q6)]),"exp")
plot(exponential_W_q6)
lognormal_W_q6<-fitdist(as.numeric(Data$acceptable_risk_W_q6[!is.na(Data$acceptable_risk_W_q6)]),"lnorm")
plot(lognormal_W_q6)
gamma_W_q6<-fitdist(as.numeric(Data$acceptable_risk_W_q6[!is.na(Data$acceptable_risk_W_q6)]),"gamma")
plot(gamma_W_q6)

denscomp(list(weibull_W_q6,exponential_W_q6,lognormal_W_q6,gamma_W_q6),legendtext=c("Weibull","Exponential","Lognormal","Gamma"),plotstyle="ggplot") +
  geom_line(linetype = "dashed",size = 1)+ theme_bw()+ggtitle("Distribution Fit Comparison for Acceptable W Risks, Area 1 vs 3")

gofstat(list(weibull_W_q6,exponential_W_q6,lognormal_W_q6,gamma_W_q6))

#---------------- W in Area 2 v 3--------------------------------------------------------------------------
weibull_R_q10<-fitdist(as.numeric(Data$acceptable_risk_R_q10[!is.na(Data$acceptable_risk_R_q10)]),"weibull")
plot(weibull_R_q10)
exponential_R_q10<-fitdist(as.numeric(Data$acceptable_risk_R_q10[!is.na(Data$acceptable_risk_R_q10)]),"exp")
plot(exponential_R_q10)
lognormal_R_q10<-fitdist(as.numeric(Data$acceptable_risk_R_q10[!is.na(Data$acceptable_risk_R_q10)]),"lnorm")
plot(lognormal_R_q10)
gamma_R_q10<-fitdist(as.numeric(Data$acceptable_risk_R_q10[!is.na(Data$acceptable_risk_R_q10)]),"gamma")
plot(gamma_R_q10)

denscomp(list(weibull_R_q10,exponential_R_q10,lognormal_R_q10,gamma_R_q10),legendtext=c("Weibull","Exponential","Lognormal","Gamma"),plotstyle="ggplot",breaks=20) +
  geom_line(linetype = "dashed",size = 1)+ theme_bw()+ggtitle("Distribution Fit Comparison for Acceptable R Risks, Area 2 vs 3")

gofstat(list(weibull_R_q10,exponential_R_q10,lognormal_R_q10,gamma_R_q10))

#-------------- Using one of these distributions to produce 10,000 simulated indifference points (right-truncated at zero)---------
meanlog_dist<-lognormal_W_q2$estimate[1]
sdlog_dist<-lognormal_W_q2$estimate[2]

require(truncdist)

numit<-10000

indiff_sim<-rtrunc(numit,"lnorm",b=1,meanlog=meanlog_dist,sdlog=sdlog_dist)
hist(indiff_sim)
summary(indiff_sim)

#-------------- Using another distribution to produce 10,000 simulated indifference points (right-truncated at zero) for robustness-----
S1_meanlog_dist<-lognormal_W_q6$estimate[1]
S1_sdlog_dist<-lognormal_W_q6$estimate[2]

S1_indiff_sim<-rtrunc(numit,"lnorm",b=1,meanlog=S1_meanlog_dist,sdlog=S1_sdlog_dist)
hist(S1_indiff_sim)
summary(S1_indiff_sim)

#------------- Compare the original and sensitivity analysis distributions--------------------------------------------------------------
require(ggplot2)
require(ggpubr)
frame<-data.frame(indiff=c(indiff_sim,S1_indiff_sim),
                  type = c(rep("Primary Distribution",numit),
                           rep("Sensitivity Analysis Distribution",numit)))

ggplot(data=frame,aes(x=indiff,group=type))+geom_histogram(aes(y=..density..,fill=type),alpha=0.2)+geom_density(aes(color=type))+
  scale_x_continuous(name="Simulated Indifference Points")+
  scale_y_continuous(name="Density")+
  scale_fill_discrete(name="Data Source (n=10,000 per source)")+
  scale_color_discrete(name="Data Source (n=10,000 per source)")
