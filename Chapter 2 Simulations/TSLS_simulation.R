library(AER)

set.seed(12345)

#Set number of iterations
K<-1000

#Define empty vectors for estimates and standard errors
X1estG1<-rep(0,K)
X1estG2<-rep(0,K)
X1estG3<-rep(0,K)
X1estG4<-rep(0,K)
X1estG5<-rep(0,K)

X1seG1<-rep(0,K)
X1seG2<-rep(0,K)
X1seG3<-rep(0,K)
X1seG4<-rep(0,K)
X1seG5<-rep(0,K)

#Define empty vectors for f-statistics
X1fG1<-rep(0,K)
X1fG2<-rep(0,K)
X1fG3<-rep(0,K)
X1fG4<-rep(0,K)
X1fG5<-rep(0,K)

#Define empty vectors for sargan p-values
X1sarG1<-rep(0,K)
X1sarG2<-rep(0,K)
X1sarG3<-rep(0,K)
X1sarG4<-rep(0,K)
X1sarG5<-rep(0,K)

#########################
###   Generate data   ###
#########################

for(k in 1:K){
  
  #Print iteration number
  print(k)
  
  #Set number of instruments to 50
  
  J<-50
  
  #Set sample size
  N<-100000
  
  #Generate set of exogenous instruments
  G.dat<-data.frame(rep(0,N))
  
  for(i in 1:J){
    G.dat[,i]<-1 + rnorm(N,0,1)
  }
  
  #Generate confounder U
  U<-1 + rnorm(N,0,1)
  
  #Set associations for group 3
  for(i in 21:28){
    U<-U + G.dat[,i]
  }
  
  #Set associations for group 2
  for(i in 11:18){
    G.dat[,i]<- G.dat[,i] + U
  }
  
  #Generate exposure
  X1<-1 + 1*U + rnorm(N,0,1)
  
  #Include instruments excluding group 1
  
  for(i in 11:50){
    X1<-X1 + G.dat[,i]
  }
  
  #Generate outcome
  Y<-1 + X1 + U + rnorm(N,0,1)
  
  #Set associations for group 4
  
  for(i in 31:38)
  Y<- Y + G.dat[,i]
  
  #Combine into single data frame
  simdata<-cbind(G.dat,X1,Y)
  
  fit1<-summary(ivreg(Y~X1|simdata[,1] + simdata[,2] + simdata[,3]  + simdata[,4] + simdata[,5]+
                      simdata[,6] + simdata[,7] + simdata[,8]  + simdata[,9] + simdata[,10]),diagnostics = T)
  
  fit2<-summary(ivreg(Y~X1|simdata[,11] + simdata[,12] + simdata[,13]  + simdata[,14] + simdata[,15]+
                        simdata[,16] + simdata[,17] + simdata[,18]  + simdata[,19] + simdata[,20]),diagnostics = T)
  
  fit3<-summary(ivreg(Y~X1|simdata[,21] + simdata[,22] + simdata[,23]  + simdata[,24] + simdata[,25]+
                        simdata[,26] + simdata[,27] + simdata[,28]  + simdata[,29] + simdata[,30]),diagnostics = T)
  
  fit4<-summary(ivreg(Y~X1|simdata[,31] + simdata[,32] + simdata[,33]  + simdata[,34] + simdata[,35]+
                        simdata[,36] + simdata[,37] + simdata[,38]  + simdata[,39] + simdata[,40]),diagnostics = T)
  
  fit5<-summary(ivreg(Y~X1|simdata[,41] + simdata[,42] + simdata[,43]  + simdata[,44] + simdata[,45]+
                        simdata[,46] + simdata[,47] + simdata[,48]  + simdata[,49] + simdata[,50]),diagnostics = T)
  
  X1estG1[k]<-fit1$coefficients[2,1]
  X1estG2[k]<-fit2$coefficients[2,1]
  X1estG3[k]<-fit3$coefficients[2,1]
  X1estG4[k]<-fit4$coefficients[2,1]
  X1estG5[k]<-fit5$coefficients[2,1]
  
  X1seG1[k]<-fit1$coefficients[2,2]
  X1seG2[k]<-fit2$coefficients[2,2]
  X1seG3[k]<-fit3$coefficients[2,2]
  X1seG4[k]<-fit4$coefficients[2,2]
  X1seG5[k]<-fit5$coefficients[2,2]
  
  #Define empty vectors for f-statistics
  X1fG1[k]<-fit1$diagnostics[1,3]
  X1fG2[k]<-fit2$diagnostics[1,3]
  X1fG3[k]<-fit3$diagnostics[1,3]
  X1fG4[k]<-fit4$diagnostics[1,3]
  X1fG5[k]<-fit5$diagnostics[1,3]
  
  #Define empty vectors for sargan p-values
  X1sarG1[k]<-fit1$diagnostics[3,4]
  X1sarG2[k]<-fit2$diagnostics[3,4]
  X1sarG3[k]<-fit3$diagnostics[3,4]
  X1sarG4[k]<-fit4$diagnostics[3,4]
  X1sarG5[k]<-fit5$diagnostics[3,4]
  
}
  
X1est_out<-c(mean(X1estG1),
             mean(X1estG2),
             mean(X1estG3),
             mean(X1estG4),
             mean(X1estG5))

X1se_out<-c(mean(X1seG1),
             mean(X1seG2),
             mean(X1seG3),
             mean(X1seG4),
             mean(X1seG5))

X1f_out<-c(mean(X1fG1),
             mean(X1fG2),
             mean(X1fG3),
             mean(X1fG4),
             mean(X1fG5))

X1sar_out<-c(mean(X1sarG1),
             mean(X1sarG2),
             mean(X1sarG3),
             mean(X1sarG4),
             mean(X1sarG5))
  
results<-data.frame(X1est_out,X1se_out,X1f_out,X1sar_out,1:5)

names(results)<-c("X1_Beta","X1_se","F-statistic","Sargan_pval","Group")

#Plot results


  
  