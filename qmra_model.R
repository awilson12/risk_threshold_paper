#---------------  QMRA Model  -------------------------------------------------


QMRA_model<-function(threshold,iterations=10000,model){
  #threshold will be informed by median, 95th, or 99th percentiles from generated
  #indifference points. iterations set to 10,000 but can be modified to investigate
  #appropriateness of 10,000 points for capturing central tendency.
  
  set.seed(34)
  
  
  #-------------------------parameters-----------------------------------------
  
  #fraction of total hand surface area used for surface-to-hand contact
  S.H<-runif(iterations,0.008,0.25) #comes from min and max of all hand configurations (min being min of front partial fingers/5)
  
  #fraction of total hand surface area used for hand-to-face contact
  S.F<-runif(iterations,0.008,0.012) #comes from min and max of front partial fingers/5
  
  #total hand surface area
  A.hand<-runif(iterations,445,535) #comes from US Exposure Factors Handbook and a model by Beamer et al. This is for 1 hand, not 2 (as used in Beamer et al)
  
  #transfer efficiency of surface to hand
  TE.SH<-rbeta(iterations,shape1=0.64,shape2=3.1) #comes from the recent Boehm paper for enveloped virus fit, NEED TO DOUBLE-CHECK SHAPE1 AND SHAPE 2 CORRESPOND TO ALPHA AND BETA
  
  #transfer efficiency of hand to face
  TE.HF<-rtrunc(iterations,"norm",a=0,b=1,mean=0.3390,sd=0.1318) #from Rusin, virus data, SD informed by Sarah's paper
  
  #transfer efficiency of hand to surface
  TE.HS<-rbeta(iterations,shape1=0.64,shape2=3.1) #comes from the recent Boehm paper for enveloped virus fit, NEED TO DOUBLE-CHECK SHAPE1 AND SHAPE 2 CORRESPOND TO ALPHA AND BETA
  
  #dose response parameter (k)
  k<-2.46E-3   #dose-response param used by Pitol & Julian in COVID-19 fomite model
  
  #concentration on the surface
  C.surface<-10^runif(iterations,-5,5) #exploring wide range to see which C.surf values yield acceptable risks
  
  #hand-to-surface contact rate
  H.surf<-rtrunc(iterations,"lnorm",a=0,b=9.7,meanlog=log(4.1),sdlog=log(1.6)) #from Beamer et al office paper, also used in Contreras et al office paper
  
  #hand-to-mouth contact rate
  H.mouth<-rtrunc(iterations,"norm",a=0,b=10,mean=2.9,sd=2.5) #from our paper in JESEE
  
  #hand-to-eyes contact rate
  H.eyes<-rtrunc(iterations,"norm",a=0,b=6,mean=2.4,sd=1.9) #from our paper in JESEE
  
  #hand-to-nose contact rate
  H.nose<-rtrunc(iterations,"norm",a=0,b=10.4,mean=2.5,sd=2.2) #from our paper in JESEE
  
  #inactivation rate (fraction/hr)
  inactiv<-runif(iterations,0.05,0.198) # from Kwon et al, indoor conditions min and max listed in text as half lives converted to k values by assuming 1st order
  
  #duration (# of hours in shift)
  duration<-12 #assuming a 12 hour shift for nurses based on Stimpfel et al (2013) listing that the greatest proportion of nurses reported ~12 hour shift
  
  #area of contact with mouth
  A.mouth<-runif(iterations,3.56,6.42) #Min and max were informed by the smallest and largest assumed fraction of total hand surface area used multiplied by the smallest and largest total hand surface areas assumed, respectively.
  
  #area of contact with eyes
  A.eyes<-runif(iterations,3.56,6.42) #same note as above
  
  #area of contact with nose
  A.nose<-runif(iterations,3.56,6.42) #same note as above
  
  #--------------------single touch model--------------------------------------
  
  #single contact with surface
  C.hand_single<-C.surface*S.H*TE.SH
  
  #single contact with face
  dose_single<-C.hand_single*TE.HF*S.F*A.hand
  
  #infection risk
  risk_single<-1-exp(-k*dose_single)
  
  #globally save frame
  QMRA_model_output_single_touch<<-data.frame(S.H,S.F,A.hand,TE.SH,TE.HF,k,C.surface,C.hand_single,dose_single,risk_single,threshold,model)
    
  #-------------------day touch model-------------------------------------------  
  
  #steady state concentration on the hand (Beamer et al. model, then used by Contreras et al.)
  C.hand_daily = (H.surf * TE.SH * C.surface * S.H) / ((inactiv) + (H.surf * TE.HS * S.H) + (TE.HF * ((H.mouth*A.mouth/A.hand) + (H.eyes*A.eyes/A.hand) + (H.nose*A.nose/A.hand))))
  
  #dose over the course of a day
  dose_daily = ((H.mouth * A.mouth) + (H.eyes * A.eyes) + (H.nose * A.nose)) * C.hand_daily * TE.HF * duration  
  
  #infection risk
  risk_daily<-1-exp(-k*dose_daily)
  
  #globally save frame
  QMRA_model_output_day_touch<<-data.frame(S.H,S.F,A.hand,TE.SH,TE.HF,k,C.surface,C.hand_daily,dose_daily,risk_daily,threshold,model)
  
  
  
}