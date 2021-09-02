##############################################
###   Install and load relevant packages   ###
##############################################

#install.packages("remotes")
#remotes::install_github("MRCIEU/TwoSampleMR")

#Set random gen.seed
set.seed(12345)

#Load libraries into R
library(TwoSampleMR)
library(ggplot2)

#Set number of iterations
K<-1000

#Set number of instruments
J<-100

#################################
### Set simulation parameters ###
#################################

# G-X Parameters

X1gammasF1<-rep(0,J)
X1gammasF5<-rnorm(J,1,0.5)
X1gammasF10<-rnorm(J,1,0.5)
X1gammasF100<-rnorm(J,1,0.5)
X1gammasF1000<-rnorm(J,1,0.5)

#Define effect of X1
beta1<-1

#Define empty vectors for estimates and standard errors without pruning
IVWest1<-rep(0,K)
EGGERest1<-rep(0,K)
WMedianest1<-rep(0,K)
WModeest1<-rep(0,K)

IVWest2<-rep(0,K)
EGGERest2<-rep(0,K)
WMedianest2<-rep(0,K)
WModeest2<-rep(0,K)

IVWest3<-rep(0,K)
EGGERest3<-rep(0,K)
WMedianest3<-rep(0,K)
WModeest3<-rep(0,K)

IVWest4<-rep(0,K)
EGGERest4<-rep(0,K)
WMedianest4<-rep(0,K)
WModeest4<-rep(0,K)

IVWest5<-rep(0,K)
EGGERest5<-rep(0,K)
WMedianest5<-rep(0,K)
WModeest5<-rep(0,K)

IVWse1<-rep(0,K)
EGGERse1<-rep(0,K)
WMedianse1<-rep(0,K)
WModese1<-rep(0,K)

IVWse2<-rep(0,K)
EGGERse2<-rep(0,K)
WMedianse2<-rep(0,K)
WModese2<-rep(0,K)

IVWse3<-rep(0,K)
EGGERse3<-rep(0,K)
WMedianse3<-rep(0,K)
WModese3<-rep(0,K)

IVWse4<-rep(0,K)
EGGERse4<-rep(0,K)
WMedianse4<-rep(0,K)
WModese4<-rep(0,K)

IVWse5<-rep(0,K)
EGGERse5<-rep(0,K)
WMedianse5<-rep(0,K)
WModese5<-rep(0,K)

#Define empty vectors for mean f-statistics without pruning
X1f<-rep(0,K)
X5f<-rep(0,K)
X10f<-rep(0,K)
X100f<-rep(0,K)
X1000f<-rep(0,K)

#########################
###   Generate data   ###
#########################

for(k in 1:K){
  
  #Print iteration number
  print(k)
  
  #Set number of instruments to 100
  
  #Set sample size
  N<-100000
  
  #Generate sets of instruments
  G.dat1<-data.frame(rep(0,N))

  G.dat2<-data.frame(rep(0,N))

  G.dat3<-data.frame(rep(0,N))

  G.dat4<-data.frame(rep(0,N))

  G.dat5<-data.frame(rep(0,N))

  for(i in 1:J){
    G.dat1[,i]<-rnorm(N,0,1)
  }
  
  for(i in 1:J){
    G.dat2[,i]<-rnorm(N,0,1)
  }
  
  for(i in 1:J){
    G.dat3[,i]<-rnorm(N,0,1)
  }
  
  for(i in 1:J){
    G.dat4[,i]<-rnorm(N,0,1)
  }
  
  for(i in 1:J){
    G.dat5[,i]<-rnorm(N,0,1)
  }
  
  
  #Generate unmeasured confounders
  U1<-rnorm(N,0,1)
  U2<-rnorm(N,0,1)
  U3<-rnorm(N,0,1)
  U4<-rnorm(N,0,1)
  U5<-rnorm(N,0,1)
  
  #Generate exposures
  X1_1<-1 + 1*U1 + rnorm(N,0,10)
  X1_2<-1 + 1*U2 + rnorm(N,0,129)
  X1_3<-1 + 1*U3 + rnorm(N,0,80)
  X1_4<-1 + 1*U4 + rnorm(N,0,34)
  X1_5<-1 + 1*U5 + rnorm(N,0,24)
  
  for (i in 1:J){
    X1_1<-X1_1 + X1gammasF1[i]*G.dat1[,i]
  }
  
  for (i in 1:J){
    X1_2<-X1_2 + X1gammasF5[i]*G.dat2[,i]
  }
  
  for (i in 1:J){
    X1_3<-X1_3 + X1gammasF10[i]*G.dat3[,i]
  }
  
  for (i in 1:J){
    X1_4<-X1_4 + X1gammasF100[i]*G.dat4[,i]
  }
  
  for (i in 1:J){
    X1_5<-X1_5 + X1gammasF1000[i]*G.dat5[,i]
  }
  
  #Generate outcomes
  Y1<-1 + rnorm(N,0,1)
  Y2<-1 + rnorm(N,0,1)
  Y3<-1 + rnorm(N,0,1)
  Y4<-1 + rnorm(N,0,1)
  Y5<-1 + rnorm(N,0,1)

  #Generate outcome
  Y1<- Y1+ beta1*X1_1 + 1*U1 + rnorm(N,0,1)
  Y2<- Y2+ beta1*X1_2 + 1*U2 + rnorm(N,0,1)
  Y3<- Y3+ beta1*X1_3 + 1*U3 + rnorm(N,0,1)
  Y4<- Y4+ beta1*X1_4 + 1*U4 + rnorm(N,0,1)
  Y5<- Y5+ beta1*X1_5 + 1*U5 + rnorm(N,0,1)
  
  #Create combined dataframes
  c.dat1<-data.frame(X1_1,Y1)
  c.dat1<-cbind(G.dat1,c.dat1)
  c.dat2<-data.frame(X1_2,Y2)
  c.dat2<-cbind(G.dat2,c.dat2)
  c.dat3<-data.frame(X1_3,Y3)
  c.dat3<-cbind(G.dat3,c.dat3)
  c.dat4<-data.frame(X1_4,Y4)
  c.dat4<-cbind(G.dat4,c.dat4)
  c.dat5<-data.frame(X1_5,Y5)
  c.dat5<-cbind(G.dat5,c.dat5)
  
  for(i in 1:J){
    names(c.dat1)[i]<-paste("rs",i,sep="")
    names(c.dat2)[i]<-paste("rs",i,sep="")
    names(c.dat3)[i]<-paste("rs",i,sep="")
    names(c.dat4)[i]<-paste("rs",i,sep="")
    names(c.dat5)[i]<-paste("rs",i,sep="")
  }
  
  #Divide sample into non-overlapping subsets for generating exposure and outcome associations
  exposure.sampleX1_1<-c.dat1[1:50000,]
  exposure.sampleX1_2<-c.dat2[1:50000,]
  exposure.sampleX1_3<-c.dat3[1:50000,]
  exposure.sampleX1_4<-c.dat4[1:50000,]
  exposure.sampleX1_5<-c.dat5[1:50000,]

  outcome.sampleY1<-c.dat1[50001:100000,]
  outcome.sampleY2<-c.dat2[50001:100000,]
  outcome.sampleY3<-c.dat3[50001:100000,]
  outcome.sampleY4<-c.dat4[50001:100000,]
  outcome.sampleY5<-c.dat5[50001:100000,]
  
  # Estimate G-X1 associations
  
  gammaX1_1hat<-rep(0,J)
  segammaX1_1hat<-rep(0,J)
  pgammaX1_1hat<-rep(0,J)
  gammaX1_2hat<-rep(0,J)
  segammaX1_2hat<-rep(0,J)
  pgammaX1_2hat<-rep(0,J)
  gammaX1_3hat<-rep(0,J)
  segammaX1_3hat<-rep(0,J)
  pgammaX1_3hat<-rep(0,J)
  gammaX1_4hat<-rep(0,J)
  segammaX1_4hat<-rep(0,J)
  pgammaX1_4hat<-rep(0,J)
  gammaX1_5hat<-rep(0,J)
  segammaX1_5hat<-rep(0,J)
  pgammaX1_5hat<-rep(0,J)
  
  for(i in 1:J){
    
    fit1<-summary(lm(exposure.sampleX1_1[,J+1]~exposure.sampleX1_1[,i]))
    fit2<-summary(lm(exposure.sampleX1_2[,J+1]~exposure.sampleX1_2[,i]))
    fit3<-summary(lm(exposure.sampleX1_3[,J+1]~exposure.sampleX1_3[,i]))
    fit4<-summary(lm(exposure.sampleX1_4[,J+1]~exposure.sampleX1_4[,i]))
    fit5<-summary(lm(exposure.sampleX1_5[,J+1]~exposure.sampleX1_5[,i]))
    
    gammaX1_1hat[i]<-fit1$coef[2,1]
    segammaX1_1hat[i]<-fit1$coef[2,2]
    pgammaX1_1hat[i]<-fit1$coef[2,4]
    
    gammaX1_2hat[i]<-fit2$coef[2,1]
    segammaX1_2hat[i]<-fit2$coef[2,2]
    pgammaX1_2hat[i]<-fit2$coef[2,4]
    
    gammaX1_3hat[i]<-fit3$coef[2,1]
    segammaX1_3hat[i]<-fit3$coef[2,2]
    pgammaX1_3hat[i]<-fit3$coef[2,4]
    
    gammaX1_4hat[i]<-fit4$coef[2,1]
    segammaX1_4hat[i]<-fit4$coef[2,2]
    pgammaX1_4hat[i]<-fit4$coef[2,4]
    
    gammaX1_5hat[i]<-fit5$coef[2,1]
    segammaX1_5hat[i]<-fit5$coef[2,2]
    pgammaX1_5hat[i]<-fit5$coef[2,4]
    
  }
  
  # Estimate G-Y associations
  
  gammaY1hat<-rep(0,J)
  segammaY1hat<-rep(0,J)
  gammaY2hat<-rep(0,J)
  segammaY2hat<-rep(0,J)
  gammaY3hat<-rep(0,J)
  segammaY3hat<-rep(0,J)
  gammaY4hat<-rep(0,J)
  segammaY4hat<-rep(0,J)
  gammaY5hat<-rep(0,J)
  segammaY5hat<-rep(0,J)
  
  for(i in 1:J){
    
    fitY1<-summary(lm(outcome.sampleY1[,J+2]~outcome.sampleY1[,i]))
    fitY2<-summary(lm(outcome.sampleY2[,J+2]~outcome.sampleY2[,i]))
    fitY3<-summary(lm(outcome.sampleY3[,J+2]~outcome.sampleY3[,i]))
    fitY4<-summary(lm(outcome.sampleY4[,J+2]~outcome.sampleY4[,i]))
    fitY5<-summary(lm(outcome.sampleY5[,J+2]~outcome.sampleY5[,i]))
    
    
    gammaY1hat[i]<-fitY1$coef[2,1]
    segammaY1hat[i]<-fitY1$coef[2,2]
    gammaY2hat[i]<-fitY2$coef[2,1]
    segammaY2hat[i]<-fitY2$coef[2,2]
    gammaY3hat[i]<-fitY3$coef[2,1]
    segammaY3hat[i]<-fitY3$coef[2,2]
    gammaY4hat[i]<-fitY4$coef[2,1]
    segammaY4hat[i]<-fitY4$coef[2,2]
    gammaY5hat[i]<-fitY5$coef[2,1]
    segammaY5hat[i]<-fitY5$coef[2,2]
    
  }
  
  #Create dataframes containing summary data
  
  sum.data1<-data.frame(names(c.dat1[1:J]),gammaX1_1hat,
                       segammaX1_1hat,gammaY1hat,segammaY1hat,pgammaX1_1hat,rep(0.4,J),rep("A",J))
  
  sum.data2<-data.frame(names(c.dat2[1:J]),gammaX1_2hat,
                        segammaX1_2hat,gammaY2hat,segammaY2hat,pgammaX1_2hat,rep(0.4,J),rep("A",J))
  
  sum.data3<-data.frame(names(c.dat3[1:J]),gammaX1_3hat,
                        segammaX1_3hat,gammaY3hat,segammaY3hat,pgammaX1_3hat,rep(0.4,J),rep("A",J))
  
  sum.data4<-data.frame(names(c.dat4[1:J]),gammaX1_4hat,
                        segammaX1_4hat,gammaY4hat,segammaY4hat,pgammaX1_4hat,rep(0.4,J),rep("A",J))
  
  sum.data5<-data.frame(names(c.dat5[1:J]),gammaX1_5hat,
                        segammaX1_5hat,gammaY5hat,segammaY5hat,pgammaX1_5hat,rep(0.4,J),rep("A",J))
  
  names(sum.data1)[1]<-"SNP"
  names(sum.data2)[1]<-"SNP"
  names(sum.data3)[1]<-"SNP"
  names(sum.data4)[1]<-"SNP"
  names(sum.data5)[1]<-"SNP"
  
  names(sum.data1)[7:8]<-c("eaf","ea")
  names(sum.data2)[7:8]<-c("eaf","ea")
  names(sum.data3)[7:8]<-c("eaf","ea")
  names(sum.data4)[7:8]<-c("eaf","ea")
  names(sum.data5)[7:8]<-c("eaf","ea")
  
  ####################
  ###   Analyses   ###
  ####################
  
  DatX1_1<-format_data(dat=sum.data1,type = "exposure", snp_col = "SNP" ,beta_col="gammaX1_1hat",se_col= "segammaX1_1hat",eaf_col = "eaf",
                    effect_allele_col = "ea")
  
  DatX1_2<-format_data(dat=sum.data2,type = "exposure", snp_col = "SNP" ,beta_col="gammaX1_2hat",se_col= "segammaX1_2hat",eaf_col = "eaf",
                    effect_allele_col = "ea")
  
  DatX1_3<-format_data(dat=sum.data3,type = "exposure", snp_col = "SNP" ,beta_col="gammaX1_3hat",se_col= "segammaX1_3hat",eaf_col = "eaf",
                    effect_allele_col = "ea")
  
  DatX1_4<-format_data(dat=sum.data4,type = "exposure", snp_col = "SNP" ,beta_col="gammaX1_4hat",se_col= "segammaX1_4hat",eaf_col = "eaf",
                    effect_allele_col = "ea")
  
  DatX1_5<-format_data(dat=sum.data5,type = "exposure", snp_col = "SNP" ,beta_col="gammaX1_5hat",se_col= "segammaX1_5hat",eaf_col = "eaf",
                    effect_allele_col = "ea")
  
  DatY1<-format_data(dat=sum.data1,type = "outcome", snp_col = "SNP" ,beta_col="gammaY1hat",se_col= "segammaY1hat",eaf_col = "eaf",
                       effect_allele_col = "ea")
  
  DatY2<-format_data(dat=sum.data2,type = "outcome", snp_col = "SNP" ,beta_col="gammaY2hat",se_col= "segammaY2hat",eaf_col = "eaf",
                       effect_allele_col = "ea")
  
  DatY3<-format_data(dat=sum.data3,type = "outcome", snp_col = "SNP" ,beta_col="gammaY3hat",se_col= "segammaY3hat",eaf_col = "eaf",
                       effect_allele_col = "ea")
  
  DatY4<-format_data(dat=sum.data4,type = "outcome", snp_col = "SNP" ,beta_col="gammaY4hat",se_col= "segammaY4hat",eaf_col = "eaf",
                       effect_allele_col = "ea")
  
  DatY5<-format_data(dat=sum.data5,type = "outcome", snp_col = "SNP" ,beta_col="gammaY5hat",se_col= "segammaY5hat",eaf_col = "eaf",
                       effect_allele_col = "ea")
  
  dat1 <- harmonise_data(
    exposure_dat = DatX1_1, 
    outcome_dat = DatY1
  )
  
  dat2 <- harmonise_data(
    exposure_dat = DatX1_2, 
    outcome_dat = DatY2
  )
  
  dat3 <- harmonise_data(
    exposure_dat = DatX1_3, 
    outcome_dat = DatY3
  )
  
  dat4 <- harmonise_data(
    exposure_dat = DatX1_4, 
    outcome_dat = DatY4
  )
  
  dat5 <- harmonise_data(
    exposure_dat = DatX1_5, 
    outcome_dat = DatY5
  )
  
  
  res1<-mr(dat1, method_list=c("mr_egger_regression", "mr_ivw","mr_weighted_median","mr_weighted_mode"))
  res2<-mr(dat2, method_list=c("mr_egger_regression", "mr_ivw","mr_weighted_median","mr_weighted_mode"))
  res3<-mr(dat3, method_list=c("mr_egger_regression", "mr_ivw","mr_weighted_median","mr_weighted_mode"))
  res4<-mr(dat4, method_list=c("mr_egger_regression", "mr_ivw","mr_weighted_median","mr_weighted_mode"))
  res5<-mr(dat5, method_list=c("mr_egger_regression", "mr_ivw","mr_weighted_median","mr_weighted_mode"))
  
  
  IVWest1[k]<-res1$b[2]
  EGGERest1[k]<-res1$b[1]
  WMedianest1[k]<-res1$b[3]
  WModeest1[k]<-res1$b[4]
  
  IVWest2[k]<-res2$b[2]
  EGGERest2[k]<-res2$b[1]
  WMedianest2[k]<-res2$b[3]
  WModeest2[k]<-res2$b[4]
  
  IVWest3[k]<-res3$b[2]
  EGGERest3[k]<-res3$b[1]
  WMedianest3[k]<-res3$b[3]
  WModeest3[k]<-res3$b[4]
  
  IVWest4[k]<-res4$b[2]
  EGGERest4[k]<-res4$b[1]
  WMedianest4[k]<-res4$b[3]
  WModeest4[k]<-res4$b[4]
  
  IVWest5[k]<-res5$b[2]
  EGGERest5[k]<-res5$b[1]
  WMedianest5[k]<-res5$b[3]
  WModeest5[k]<-res5$b[4]
  
  IVWse1[k]<-res1$se[2]
  EGGERse1[k]<-res1$se[1]
  WMedianse1[k]<-res1$se[3]
  WModese1[k]<-res1$se[4]
  
  IVWse2[k]<-res2$se[2]
  EGGERse2[k]<-res2$se[1]
  WMedianse2[k]<-res2$se[3]
  WModese2[k]<-res2$se[4]
  
  IVWse3[k]<-res3$se[2]
  EGGERse3[k]<-res3$se[1]
  WMedianse3[k]<-res3$se[3]
  WModese3[k]<-res3$se[4]
  
  IVWse4[k]<-res4$se[2]
  EGGERse4[k]<-res4$se[1]
  WMedianse4[k]<-res4$se[3]
  WModese4[k]<-res4$se[4]
  
  IVWse5[k]<-res5$se[2]
  EGGERse5[k]<-res5$se[1]
  WMedianse5[k]<-res5$se[3]
  WModese5[k]<-res5$se[4]
  
  #Define empty vectors for mean f-statistics without pruning
  X1f[k]<-mean(sum.data1[,2]^2 / sum.data1[,3]^2)
  X5f[k]<-mean(sum.data2[,2]^2 / sum.data2[,3]^2)
  X10f[k]<-mean(sum.data3[,2]^2 / sum.data3[,3]^2)
  X100f[k]<-mean(sum.data4[,2]^2 / sum.data4[,3]^2)
  X1000f[k]<-mean(sum.data5[,2]^2 / sum.data5[,3]^2)
  
}

mean(IVWest1)

