library(AER)
library(ggplot2)
library(devtools)
#install_github("jrs95/nlmr")
library(nlmr)
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

#Define empty vectors for linearity test p-values
X1linG1<-rep(0,K)
X1linG2<-rep(0,K)
X1linG3<-rep(0,K)
X1linG4<-rep(0,K)
X1linG5<-rep(0,K)

#########################
###   Generate data   ###
#########################

for(k in 1:K){
  
  #Print iteration number
  print(k)
  
  #Set number of instruments to 6
  
  J<-5
  
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
  U<-U + G.dat[,3]
  
  #Set associations for group 2
  G.dat[,2]<- G.dat[,2] + U
  
  #Generate exposure
  X1<-1 + 1*U + rnorm(N,0,1)
  
  #Include instruments excluding group 1
  
  for(i in c(2:5)){
    X1<-X1 + G.dat[,i]
  }
  
  X1<-X1 + abs(min(X1))+2
  
  #Generate outcome
  Y<-1 + 1*X1^2 + 1*U + rnorm(N,0,1)
  
  #Set associations for group 4
  
  Y<- Y + G.dat[,4]
  
  #Combine into single data frame
  simdata<-cbind(G.dat,X1,Y)
  
  fit1<-fracpoly_mr(simdata$Y,simdata$X1,g= simdata[,1])
  fit2<-fracpoly_mr(simdata$Y,simdata$X1,g= simdata[,2])
  fit3<-fracpoly_mr(simdata$Y,simdata$X1,g= simdata[,3])
  fit4<-fracpoly_mr(simdata$Y,simdata$X1,g= simdata[,4])
  fit5<-fracpoly_mr(simdata$Y,simdata$X1,g= simdata[,5])
  
  X1estG1[k]<-fit1$coefficients[1,1]
  X1estG2[k]<-fit2$coefficients[1,1]
  X1estG3[k]<-fit3$coefficients[1,1]
  X1estG4[k]<-fit4$coefficients[1,1]
  X1estG5[k]<-fit5$coefficients[1,1]
  
  X1seG1[k]<-fit1$coefficients[1,2]
  X1seG2[k]<-fit2$coefficients[1,2]
  X1seG3[k]<-fit3$coefficients[1,2]
  X1seG4[k]<-fit4$coefficients[1,2]
  X1seG5[k]<-fit5$coefficients[1,2]
  
  #Define empty vectors for linearity p-values
  X1linG1[k]<-summary(fit1)$p_tests[4]
  X1linG2[k]<-summary(fit2)$p_tests[4]
  X1linG3[k]<-summary(fit3)$p_tests[4]
  X1linG4[k]<-summary(fit4)$p_tests[4]
  X1linG5[k]<-summary(fit5)$p_tests[4]
  
}

X1est_out<-c(median(X1estG1),
             mean(X1estG2),
             mean(X1estG3),
             mean(X1estG4),
             mean(X1estG5))

X1se_out<-c(median(X1seG1),
            mean(X1seG2),
            mean(X1seG3),
            mean(X1seG4),
            mean(X1seG5))

X1lin_out<-c(median(unlist(X1linG1)),
             mean(unlist(X1linG2)),
             mean(unlist(X1linG3)),
             mean(unlist(X1linG4)),
             mean(unlist(X1linG5)))


results<-data.frame(X1est_out,X1se_out,X1lin_out,
                    X1est_out - 1.96 * X1se_out,
                    X1est_out + 1.96 * X1se_out,factor(1:5))

names(results)<-c("X1_Beta","X1_se","Linearity_pval","lci","uci","Group")





#Plot results

Figure1<-ggplot(data = results, aes(x = Group, y = X1_Beta)) + geom_point(aes(colour = Group))+ coord_flip()+theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+ggtitle("")+
  geom_segment(aes(x = Group, y = lci, xend = Group, yend = uci, colour = Group), data = results)+
  geom_hline(yintercept = 1,linetype = "dashed")+
  scale_x_discrete(name = "Instrument",breaks = 1:5)+
  scale_color_manual(name = "Instrument",values=c("1"="#56B4E9","2"="#D55E00","3"="#009E73","4"="#E69F00","5"="#CC79A7"))+
  ylab("Mean effect estimate")

png(filename = "NLMR_sim.png",
    width = 1920 , height = 1080, units = "px", res=300,
    bg = "white")

Figure1

dev.off()

