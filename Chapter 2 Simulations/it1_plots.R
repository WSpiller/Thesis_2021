##############################################
###   Install and load relevant packages   ###
##############################################


#install.packages("remotes")
#library(remotes)
#install_github("WSpiller/RMVMR", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
#install_github("WSpiller/MVMR", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
#install_github("WSpiller/RadialMR", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)

#Set random gen.seed
set.seed(52431)

#Load libraries into R
library(RMVMR)
library(RadialMR)
library(MVMR)
library(ggplot2)

#Set number of iterations
K<-100

#################################
### Set simulation parameters ###
#################################

# G-X Parameters

X1gammas<-rnorm(30,0,5)
X1gammas<-c(X1gammas,rep(0,30))
X1gammas<-c(X1gammas,rep(0,30))
X1gammas<-c(X1gammas,rnorm(30,0,5))
X1gammas<-c(X1gammas,rnorm(30,0,5))
X1gammas<-c(X1gammas,rep(0,30))
X1gammas<-c(X1gammas,rnorm(30,0,5))
X1gammas<-c(X1gammas,rnorm(30,0,5))

X1gammas<-abs(X1gammas)

for(i in 1:240){
  X1gammas[i]<-X1gammas[i] + sign(X1gammas[i]) * 5
}

X2gammas<-rep(0,30)
X2gammas<-c(X2gammas,rnorm(30,0,5))
X2gammas<-c(X2gammas,rep(0,30))
X2gammas<-c(X2gammas,rnorm(30,0,5))
X2gammas<-c(X2gammas,rep(0,30))
X2gammas<-c(X2gammas,rnorm(30,0,5))
X2gammas<-c(X2gammas,rnorm(30,0,5))
X2gammas<-c(X2gammas,rnorm(30,0,5))

X2gammas<-abs(X2gammas)

for(i in 1:240){
  X2gammas[i]<-X2gammas[i] + sign(X2gammas[i]) * 5
}

X3gammas<-rep(0,30)
X3gammas<-c(X3gammas,rep(0,30))
X3gammas<-c(X3gammas,rnorm(30,0,5))
X3gammas<-c(X3gammas,rep(0,30))
X3gammas<-c(X3gammas,rnorm(30,0,5))
X3gammas<-c(X3gammas,rnorm(30,0,5))
X3gammas<-c(X3gammas,rnorm(30,0,5))
X3gammas<-c(X3gammas,rnorm(30,0,5))

X3gammas<-abs(X3gammas)

for(i in 1:240){
  X3gammas[i]<-X3gammas[i] + sign(X3gammas[i]) * 5
}

#Define effect of X1
beta1<-1

#Define effect of X2
beta2<-0.2

#Define effect of X3
beta3<-(-0.5)

# G-Y Parameters

alphavec<-rep(0,210)
alphavec<-abs(c(alphavec,rnorm(30,10,5)))

for(i in 1:240){
  alphavec[i]<-alphavec[i] + sign(alphavec[i]) * 2
}

#Define empty vectors for estimates and standard errors without pruning
X1est<-rep(0,K)
X2est<-rep(0,K)
X3est<-rep(0,K)
X1se<-rep(0,K)
X2se<-rep(0,K)
X3se<-rep(0,K)

#Define empty vectors for estimates and standard errors after pruning
X1pest<-rep(0,K)
X2pest<-rep(0,K)
X3pest<-rep(0,K)
X1pse<-rep(0,K)
X2pse<-rep(0,K)
X3pse<-rep(0,K)

#Define empty vectors for conditional f-statistics without pruning
X1f<-rep(0,K)
X2f<-rep(0,K)
X3f<-rep(0,K)

#Define empty vectors for conditional f-statistics after pruning
X1fp<-rep(0,K)
X2fp<-rep(0,K)
X3fp<-rep(0,K)

#########################
###   Generate data   ###
#########################

for(k in 1:K){
  
  #Print iteration number
  print(k)
  
  #Set number of instruments to 240
  
  J<-240
  
  #Set sample size
  N<-200000
  
  #Generate set of instruments
  G.dat<-data.frame(rep(0,N))
  
  for(i in 1:J){
    G.dat[,i]<-rnorm(N,0,1)
  }
  
  #Generate unmeasured confounder
  U<-rnorm(N,0,1)
  
#  #shared effect between exposures
X1X2<-rnorm(N,0,1)
X1X3<-rnorm(N,0,1)
X2X1<-rnorm(N,0,1)
X2X3<-rnorm(N,0,1)
X3X1<-rnorm(N,0,1)
X3X2<-rnorm(N,0,1)
  
  #Generate exposures
X1<-1 + 1*U + rnorm(N,0,1) + rnorm(1,0,5)*X1X2 + rnorm(1,0,5)*X1X3 + rnorm(1,0,5)*X3X1 + rnorm(1,0,5)*X2X1
X2<-1 + 1*U + rnorm(N,0,1) + rnorm(1,0,5)*X1X2 + rnorm(1,0,5)*X2X1 + rnorm(1,0,5)*X2X3 + rnorm(1,0,5)*X3X2
X3<-1 + 1*U + rnorm(N,0,1) + rnorm(1,0,5)*X1X3 + rnorm(1,0,5)*X2X3 + rnorm(1,0,5)*X3X1 + rnorm(1,0,5)*X3X2

  
  #Generate outcome
  Y<-1 + rnorm(N,0,1)
  
  #Pleiotropic effects for instruments
  for (i in c(1:240)){
    Y<-Y + alphavec[i]*G.dat[,i]
  }
  
  #X1 associations
  for (i in 1:240){
    X1<-X1 + X1gammas[i]*G.dat[,i]
  }
  
  #X2 associations
  for (i in 1:240){
    X2<-X2 + X2gammas[i]*G.dat[,i]
  }
  
  #X3 associations
  for (i in 1:240){
    X3<-X3 + X3gammas[i]*G.dat[,i]
  }
  
  #Generate outcome
  Y<- Y + beta1*X1 + beta2*X2 + beta3*X3 + 1*U + rnorm(N,0,1)
  
  #Create combined dataframe
  c.dat<-data.frame(X1,X2,X3,Y)
  c.dat<-cbind(G.dat,c.dat)
  for(i in 1:J){
    names(c.dat)[i]<-paste("rs",i,sep="")
  }
  
  #Divide sample into non-overlapping subsets for generating exposure and outcome associations
  exposure.sampleX1<-c.dat[1:50000,]
  exposure.sampleX2<-c.dat[50001:100000,]
  exposure.sampleX3<-c.dat[100001:150000,]
  outcome.sampleY<-c.dat[150001:200000,]
  
  # Estimate G-X1 associations
  
  gammaX1hat<-rep(0,J)
  segammaX1hat<-rep(0,J)
  pgammaX1hat<-rep(0,J)
  
  for(i in 1:J){
    
    gammaX1hat[i]<-summary(lm(exposure.sampleX1[,J+1]~exposure.sampleX1[,i]))$coef[2,1]
    segammaX1hat[i]<-summary(lm(exposure.sampleX1[,J+1]~exposure.sampleX1[,i]))$coef[2,2]
    pgammaX1hat[i]<-summary(lm(exposure.sampleX1[,J+1]~exposure.sampleX1[,i]))$coef[2,4]
    
  }
  
  # Estimate G-X2 associations
  
  gammaX2hat<-rep(0,J)
  segammaX2hat<-rep(0,J)
  pgammaX2hat<-rep(0,J)
  
  for(i in 1:J){
    gammaX2hat[i]<-summary(lm(exposure.sampleX2[,J+2]~exposure.sampleX2[,i]))$coef[2,1]
    segammaX2hat[i]<-summary(lm(exposure.sampleX2[,J+2]~exposure.sampleX2[,i]))$coef[2,2]
    pgammaX2hat[i]<-summary(lm(exposure.sampleX2[,J+2]~exposure.sampleX2[,i]))$coef[2,4]
    
  }
  
  # Estimate G-X3 associations
  
  gammaX3hat<-rep(0,J)
  segammaX3hat<-rep(0,J)
  pgammaX3hat<-rep(0,J)
  
  for(i in 1:J){
    gammaX3hat[i]<-summary(lm(exposure.sampleX3[,J+3]~exposure.sampleX3[,i]))$coef[2,1]
    segammaX3hat[i]<-summary(lm(exposure.sampleX3[,J+3]~exposure.sampleX3[,i]))$coef[2,2]
    pgammaX3hat[i]<-summary(lm(exposure.sampleX3[,J+3]~exposure.sampleX3[,i]))$coef[2,4]
    
  }
  
  # Estimate G-Y associations
  
  gammaYhat<-rep(0,J)
  segammaYhat<-rep(0,J)
  for(i in 1:J){
    
    gammaYhat[i]<-summary(lm(outcome.sampleY[,J+4]~outcome.sampleY[,i]))$coef[2,1]
    segammaYhat[i]<-summary(lm(outcome.sampleY[,J+4]~outcome.sampleY[,i]))$coef[2,2]
    
  }
  
  #Create dataframe containing summary data
  
  sum.data<-data.frame(names(c.dat[1:J]),gammaX1hat,gammaX2hat,gammaX3hat,
                       segammaX1hat,segammaX2hat,segammaX3hat,
                       gammaYhat,segammaYhat,pgammaX1hat,pgammaX2hat,pgammaX3hat)
  
  names(sum.data)[1]<-"SNP"
  
  
  ################################################
  ###   Radial MVMR Analyses: X1, X2, and X3   ###
  ################################################
  
  # Select instruments for any exposure
  
  X1X2X3datuni<-sum.data
  
  #Format the X1-X2 data
  X1X2X3f.data<-format_rmvmr(sum.data[,2:4], sum.data[,8], sum.data[,5:7], sum.data[,9], sum.data[,1])
  
  #Estimate conditional instrument strength
  strength_X1X2X3<- suppressWarnings(strength_mvmr(X1X2X3f.data))
  
  #Save conditional F statistic for exposure 1
  X1f[k]<-strength_X1X2X3[1]
  
  #Save conditional F statistic for exposure 2
  X2f[k]<-strength_X1X2X3[2]
  
  #Save conditional F statistic for exposure 3
  X3f[k]<-strength_X1X2X3[3]
  
  #Estimate causal effects  using Radial MVMR
  X1X2X3res<-ivw_rmvmr(X1X2X3f.data,F)
  
  #Save effect estimate and se for exposure 1
  X1est[k]<-X1X2X3res$coef[1,1]
  X1se[k]<-X1X2X3res$coef[1,2]
  
  #Save effect estimate and se for exposure 2
  X2est[k]<-X1X2X3res$coef[2,1]
  X2se[k]<-X1X2X3res$coef[2,2]
  
  #Save effect estimate and se for exposure 3
  X3est[k]<-X1X2X3res$coef[3,1]
  X3se[k]<-X1X2X3res$coef[3,2]
  
  #Estimate pleiotropic effects and detect outliers
  pleioX1X2X3<-pleiotropy_rmvmr(X1X2X3f.data,X1X2X3res)
  
  #############################################################
  ###   Radial MVMR Analyses: X1, X2, and X3 with pruning   ###
  #############################################################
  X1X2X3fp.data<-X1X2X3f.data
  
  outliers<-"start"
  
  while(!is.null(outliers)){
    
    X1X2X3pres<-ivw_rmvmr(X1X2X3fp.data,F)
    pleioX1X2X3p<-pleiotropy_rmvmr(X1X2X3fp.data,X1X2X3pres)
    
    ###Prune###
    
    if(min(pleioX1X2X3p$qdat$qj_p)< 0.05){
      outliers<-pleioX1X2X3p$qdat[pleioX1X2X3p$qdat$qj_p <0.05,]$snp
      outliers<-unique(outliers)
      
      X1X2X3fp.data<-X1X2X3fp.data[!X1X2X3fp.data$SNP %in% outliers,]
      
    }else{
      outliers<-NULL
      
    }
    
  }
  
  #Estimate conditional instrument strength
  strength_X1X2X3p<- suppressWarnings(strength_mvmr(X1X2X3fp.data))
  
  #Save conditional F statistic for exposure 1
  X1fp[k]<-strength_X1X2X3p[1]
  
  #Save conditional F statistic for exposure 2
  X2fp[k]<-strength_X1X2X3p[2]
  
  #Save conditional F statistic for exposure 3
  X3fp[k]<-strength_X1X2X3p[3]
  
  #Estimate causal effects  using Radial MVMR
  X1X2X3resp<-ivw_rmvmr(X1X2X3fp.data,F)
  
  #Save effect estimate and se for exposure 1
  X1pest[k]<-X1X2X3resp$coef[1,1]
  X1pse[k]<-X1X2X3resp$coef[1,2]
  
  #Save effect estimate and se for exposure 2
  X2pest[k]<-X1X2X3resp$coef[2,1]
  X2pse[k]<-X1X2X3resp$coef[2,2]
  
  #Save effect estimate and se for exposure 3
  X3pest[k]<-X1X2X3resp$coef[3,1]
  X3pse[k]<-X1X2X3resp$coef[3,2]
  
}



#End of sims








estvec<-c(mean(X1est),mean(X1pest),mean(X2est),mean(X2pest),mean(X3est),mean(X3pest))
lcivec<-c(mean(X1est)-1.96*mean(X1se),mean(X1pest)-1.96*mean(X1pse),mean(X2est)-1.96*mean(X2se),
          mean(X2pest)-1.96*mean(X2pse),mean(X3est)-1.96*mean(X3se),mean(X3pest)-1.96*mean(X3pse))
ucivec<-c(mean(X1est)+1.96*mean(X1se),mean(X1pest)+1.96*mean(X1pse),mean(X2est)+1.96*mean(X2se),
          mean(X2pest)+1.96*mean(X2pse),mean(X3est)+1.96*mean(X3se),mean(X3pest)+1.96*mean(X3pse))
Exposure<-c("Exposure 1","Exposure 1","Exposure 2","Exposure 2","Exposure 3","Exposure 3")
Model<-c("Initial X1 estimate","Pruned X1 estimate",
         "Initial X2 estimate","Pruned X2 estimate",
         "Initial X3 estimate","Pruned X3 estimate")

index<-c(1,1.2,2,2.2,3,3.2)*-1

plotdat<-data.frame(estvec,lcivec,ucivec,Exposure,index,Model)

F8<-ggplot(data = plotdat, aes(x = index, y = estvec)) + geom_point(aes(colour = Exposure))+ coord_flip()+theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+ggtitle("")+
  geom_segment(aes(x = index, y = lcivec, xend = index, yend = ucivec, colour = Exposure), data = plotdat)+
  geom_hline(yintercept = 1,linetype = "dashed",colour = "#56B4E9")+
  geom_hline(yintercept = 0.2,linetype = "dashed",colour ="#D55E00")+
  geom_hline(yintercept = -0.5,linetype = "dashed",colour ="#009E73")+
  scale_x_continuous(name = "Model",breaks = index, labels = Model)+
  scale_color_manual(name = "Reference exposure",values=c("Exposure 1"="#56B4E9","Exposure 2"="#D55E00","Exposure 3"="#009E73"))+
  ylab("Mean effect estimate")

png(filename = "Figure 8.png",
    width = 1920 , height = 1080, units = "px", res=300,
    bg = "white")

F8

dev.off()


#############################
###  Sim 1 plots   ##########
#############################

#Select only instruments for exposure 1
X1datuni<-sum.data[sum.data$pgammaX1hat<0.05,]

#Format exposure 1 data
X1f.data<-format_radial(X1datuni[,2], X1datuni[,8], X1datuni[,5], X1datuni[,9], X1datuni[,1])

#Estimate causal effect of exposure 1 on Y
resX1uni<-ivw_radial(X1f.data,0.05,1,0.0001,T)

#Generate univariable radial plot for exposure 1
plotX1<-plot_radial(resX1uni,F,F,F)

png("Sim11AX1R.png",width=7,height=5.25,units="in",res=500)
plotX1
dev.off()

#Select only instruments for exposure 1
X2datuni<-sum.data[sum.data$pgammaX2hat<0.05,]

#Format exposure 1 data
X2f.data<-format_radial(X2datuni[,3], X2datuni[,8], X2datuni[,6], X2datuni[,9], X2datuni[,1])

#Estimate causal effect of exposure 1 on Y
resX2uni<-ivw_radial(X2f.data,0.05,1,0.0001,T)

#Generate univariable radial plot for exposure 1
plotX2<-plot_radial(resX2uni,F,F,F)

png("Sim11AX2R.png",width=7,height=5.25,units="in",res=500)
plotX2
dev.off()

#Select only instruments for exposure 1
X3datuni<-sum.data[sum.data$pgammaX3hat<0.05,]

#Format exposure 1 data
X3f.data<-format_radial(X3datuni[,4], X3datuni[,8], X3datuni[,7], X3datuni[,9], X3datuni[,1])

#Estimate causal effect of exposure 1 on Y
resX3uni<-ivw_radial(X3f.data,0.05,1,0.0001,T)

#Generate univariable radial plot for exposure 1
plotX3<-plot_radial(resX3uni,F,F,F)

png("Sim11AX3R.png",width=7,height=5.25,units="in",res=500)
plotX3
dev.off()



##############################################

###########################################
###   Radial MVMR Analyses: X1 and X2   ###
###########################################

# Select instruments for either exposure 1 or exposure 2

X1X2datuni<-sum.data[sum.data$pgammaX1hat<0.05 | sum.data$pgammaX2hat<0.05,]

#Format the X1-X2 data
X1X2f.data<-format_rmvmr(sum.data[,2:3], sum.data[,8], sum.data[,5:6], sum.data[,9], sum.data[,1])

#Estimate conditional instrument strength
strength_X1X2<- strength_rmvmr(X1X2f.data)

#Show conditional F statistic
strength_X1X2$f

#Estimate causal effects of exposure 1 and exposure 2 using Radial MVMR
X1X2res<-ivw_rmvmr(X1X2f.data,T)

#Estimate pleiotropic effects and detect outliers
pleioX1X2<-pleiotropy_rmvmr(X1X2f.data,X1X2res)

#Show Q-statistics and p-values for each SNP across all exposures
pleioX1X2$qdat

#Show global Q-statistics for all exposures
pleioX1X2$gq

X1X2plots<-plot_rmvmr(X1X2f.data,X1X2res)

png("Sim11X1X2R.png",width=7,height=5.25,units="in",res=500)
X1X2plots$p2
dev.off()



################################################
###   Radial MVMR Analyses: X1, X2, and X3   ###
################################################



# Select instruments for any exposure

X1X2X3datuni<-sum.data

#Format the X1-X2 data
X1X2X3f.data<-format_rmvmr(sum.data[,2:4], sum.data[,8], sum.data[,5:7], sum.data[,9], sum.data[,1])

#Estimate conditional instrument strength
strength_X1X2X3<- strength_rmvmr(X1X2X3f.data)

#Show conditional F statistic
strength_X1X2X3$f

#Generate conditional strength radial MVMR plot for exposure 1
strength_X1X2X3$plot[1]

#Generate conditional strength radial MVMR plot for exposure 2
strength_X1X2X3$plot[2]

#Generate conditional strength radial MVMR plot for exposure 3
strength_X1X2X3$plot[3]

#Estimate causal effects  using Radial MVMR
X1X2X3res<-ivw_rmvmr(X1X2X3f.data,T)

#Estimate pleiotropic effects and detect outliers
pleioX1X2X3<-pleiotropy_rmvmr(X1X2X3f.data,X1X2X3res)

#Show Q-statistics and p-values for each SNP across all exposures
pleioX1X2X3$qdat

#Show global Q-statistics for all exposures
pleioX1X2X3$gq



#############################################################
###   Radial MVMR Analyses: X1, X2, and X3 with pruning   ###
#############################################################
X1X2X3fp.data<-X1X2X3f.data

outliers<-"start"

while(!is.null(outliers)){
  
  X1X2X3pres<-ivw_rmvmr(X1X2X3fp.data,F)
  pleioX1X2X3p<-pleiotropy_rmvmr(X1X2X3fp.data,X1X2X3pres)
  
  ###Prune###
  
  if(min(pleioX1X2X3p$qdat$qj_p)< 0.05){
    outliers<-pleioX1X2X3p$qdat[pleioX1X2X3p$qdat$qj_p <0.05,]$snp
    outliers<-unique(outliers)
    
    X1X2X3fp.data<-X1X2X3fp.data[!X1X2X3fp.data$SNP %in% outliers,]
    
  }else{
    outliers<-NULL
    
  }
  
}

#Estimate conditional instrument strength
strength_X1X2X3p<- strength_rmvmr(X1X2X3fp.data)

#Show conditional F statistic
strength_X1X2X3p$f

#Estimate causal effects  using Radial MVMR
X1X2X3resp<-ivw_rmvmr(X1X2X3fp.data,T)

#Estimate pleiotropic effects and detect outliers
pleioX1X2X3p<-pleiotropy_rmvmr(X1X2X3fp.data,X1X2X3resp)

#Show global Q-statistics for all exposures
pleioX1X2X3p$gq

#Generate conditional strength radial MVMR plot for exposure 1
png("X1X2X3_strengthplotsX1.png",width=7,height=5.25,units="in",res=500)
strength_X1X2X3p$plot[1]
dev.off()

#Generate conditional strength radial MVMR plot for exposure 1
png("X1X2X3_strengthplotsX2.png",width=7,height=5.25,units="in",res=500)
strength_X1X2X3p$plot[2]
dev.off()

#Generate conditional strength radial MVMR plot for exposure 1
png("X1X2X3_strengthplotsX3.png",width=7,height=5.25,units="in",res=500)
strength_X1X2X3p$plot[3]
dev.off()




##Plot for detecting outliers

plotdat<-pleioX1X2X3$qdat

plotdat<-plotdat[plotdat$qj_p < 0.05/(nrow(plotdat)),]

names(plotdat)[6]<- "Exposure"

plotdat$Exposure<-factor(plotdat$Exposure)

levels(plotdat$Exposure)<-c("Exposure 1","Exposure 2","Exposure 3")

F6<-ggplot(data = plotdat, aes(x = snp, y = -log10(qj_p)))+geom_point(aes(colour = Exposure))+theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+ylab(expression(-log10(p)))+xlab("Instrument")+ggtitle("Q-statistics for identified outliers")+
  geom_hline(yintercept = -log10(0.05/240))+
  scale_color_manual(values=c("Exposure 1"="#56B4E9","Exposure 2"="#D55E00","Exposure 3"="#009E73"))

png(filename = "Figure 6.png",
    width = 1920 , height = 1080, units = "px", res=250,
    bg = "white")

F6

dev.off()




#Estimate plots



#Generate unadjusted and adjusted radial MVMR plots

X1X2plots<-plot_rmvmr(X1X2f.data,X1X2res)
X1X2X3plots<-plot_rmvmr(X1X2X3f.data,X1X2X3res)
X1X2X3pplots<-plot_rmvmr(X1X2X3fp.data,X1X2X3resp)


#Generate conditional strength radial MVMR plot for exposure 1
png("X1X2_estplot.png",width=7,height=5.25,units="in",res=500)
X1X2plots$p2
dev.off()

#Generate conditional strength radial MVMR plot for exposure 1
png("X1X2X3_estplot.png",width=7,height=5.25,units="in",res=500)
X1X2X3plots$p2
dev.off()

#Generate conditional strength radial MVMR plot for exposure 1
png("X1X2X3p_estplot.png",width=7,height=5.25,units="in",res=500)
X1X2X3pplots$p2
dev.off()





X1X2X3pplots$p2


















