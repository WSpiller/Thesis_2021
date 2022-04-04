library(AER)
library(ggplot2)

set.seed(12345)

#Set number of iterations
K<-1000

#Define empty vectors for estimates and standard errors
X1estG1<-rep(0,K)
X1estG2<-rep(0,K)
X1estG3<-rep(0,K)
X1estG4<-rep(0,K)
X1estG5<-rep(0,K)
X2estG1<-rep(0,K)
X2estG2<-rep(0,K)
X2estG3<-rep(0,K)
X2estG4<-rep(0,K)
X2estG5<-rep(0,K)

X1seG1<-rep(0,K)
X1seG2<-rep(0,K)
X1seG3<-rep(0,K)
X1seG4<-rep(0,K)
X1seG5<-rep(0,K)
X2seG1<-rep(0,K)
X2seG2<-rep(0,K)
X2seG3<-rep(0,K)
X2seG4<-rep(0,K)
X2seG5<-rep(0,K)

#Define empty vectors for conditional f-statistics
X1fG1<-rep(0,K)
X1fG2<-rep(0,K)
X1fG3<-rep(0,K)
X1fG4<-rep(0,K)
X1fG5<-rep(0,K)
X2fG1<-rep(0,K)
X2fG2<-rep(0,K)
X2fG3<-rep(0,K)
X2fG4<-rep(0,K)
X2fG5<-rep(0,K)

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
  
  J<-100
  
  #Set sample size
  N<-100000
  
  #Generate set of exogenous instruments
  G.dat<-data.frame(rep(0,N))
  
  for(i in 1:J){
    G.dat[,i]<-1 + rnorm(N,0,1)
  }
  
  #Generate confounder U
  U<- 1 + rnorm(N,0,1)
  
  #Set associations for group 3
  for(i in c(41:48,51:58)){
    U<-U + G.dat[,i]
  }
  
  #Set associations for group 2
  for(i in c(21:28,31:38)){
    G.dat[,i]<- G.dat[,i] + U
  }
  
  #Generate exposure X1
  X1<-1 + 1*U + rnorm(N,0,1)
  
  #Include instruments for X1
  
  for(i in c(21:30,41:50,61:70,81:90)){
    X1<-X1 + G.dat[,i]
  }
  
  #Generate exposure X2
  X2<-1 + 1*U + 2*X1 + rnorm(N,0,1)
  
  for(i in c(31:40,51:60,71:80,91:100)){
    X2<-X2 + G.dat[,i]
  }
  
  #Generate outcome
  Y<-1 + X1 + X2 + U + rnorm(N,0,1)
  
  #Set associations for group 4
  
  for(i in c(61:68,71:78)){
    Y<- Y + G.dat[,i]
  }
  
  #Combine into single data frame
  simdata<-cbind(G.dat,X1,X2,Y)
  
  fit1<-summary(ivreg(Y~X1+X2|simdata[,1] + simdata[,2] + simdata[,3]  + simdata[,4] + simdata[,5]+
                        simdata[,6] + simdata[,7] + simdata[,8]  + simdata[,9] + simdata[,10]+
                      simdata[,11] + simdata[,12] + simdata[,13]  + simdata[,14] + simdata[,15]+
                        simdata[,16] + simdata[,17] + simdata[,18]  + simdata[,19] + simdata[,20]),diagnostics = T)
  
  fit2<-summary(ivreg(Y~X1+X2|simdata[,21] + simdata[,22] + simdata[,23]  + simdata[,24] + simdata[,25]+
                        simdata[,26] + simdata[,27] + simdata[,28]  + simdata[,29] + simdata[,30]+
                        simdata[,31] + simdata[,32] + simdata[,33]  + simdata[,34] + simdata[,35]+
                        simdata[,36] + simdata[,37] + simdata[,38]  + simdata[,39] + simdata[,40]),diagnostics = T)
  
  fit3<-summary(ivreg(Y~X1+X2|simdata[,41] + simdata[,42] + simdata[,43]  + simdata[,44] + simdata[,45]+
                        simdata[,46] + simdata[,47] + simdata[,48]  + simdata[,49] + simdata[,50]+
                        simdata[,51] + simdata[,52] + simdata[,53]  + simdata[,54] + simdata[,55]+
                        simdata[,56] + simdata[,57] + simdata[,58]  + simdata[,59] + simdata[,60]),diagnostics = T)
  
  fit4<-summary(ivreg(Y~X1+X2|simdata[,61] + simdata[,62] + simdata[,63]  + simdata[,64] + simdata[,65]+
                        simdata[,66] + simdata[,67] + simdata[,68]  + simdata[,69] + simdata[,70]+
                        simdata[,71] + simdata[,72] + simdata[,73]  + simdata[,74] + simdata[,75]+
                        simdata[,76] + simdata[,77] + simdata[,78]  + simdata[,79] + simdata[,80]),diagnostics = T)
  
  fit5<-summary(ivreg(Y~X1+X2|simdata[,81] + simdata[,82] + simdata[,83]  + simdata[,84] + simdata[,85]+
                         simdata[,86] + simdata[,87] + simdata[,88]  + simdata[,89] + simdata[,90]+
                         simdata[,91] + simdata[,92] + simdata[,93]  + simdata[,94] + simdata[,95]+
                         simdata[,96] + simdata[,97] + simdata[,98]  + simdata[,99] + simdata[,100]),diagnostics = T)
  
  X1estG1[k]<-fit1$coefficients[2,1]
  X1estG2[k]<-fit2$coefficients[2,1]
  X1estG3[k]<-fit3$coefficients[2,1]
  X1estG4[k]<-fit4$coefficients[2,1]
  X1estG5[k]<-fit5$coefficients[2,1]
  X2estG1[k]<-fit1$coefficients[3,1]
  X2estG2[k]<-fit2$coefficients[3,1]
  X2estG3[k]<-fit3$coefficients[3,1]
  X2estG4[k]<-fit4$coefficients[3,1]
  X2estG5[k]<-fit5$coefficients[3,1]
  
  X1seG1[k]<-fit1$coefficients[2,2]
  X1seG2[k]<-fit2$coefficients[2,2]
  X1seG3[k]<-fit3$coefficients[2,2]
  X1seG4[k]<-fit4$coefficients[2,2]
  X1seG5[k]<-fit5$coefficients[2,2]
  X2seG1[k]<-fit1$coefficients[3,2]
  X2seG2[k]<-fit2$coefficients[3,2]
  X2seG3[k]<-fit3$coefficients[3,2]
  X2seG4[k]<-fit4$coefficients[3,2]
  X2seG5[k]<-fit5$coefficients[3,2]
  
  #Conditional F-stats:Group 1 X1
  obj1 <- lm(X1~X2)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X1~X2+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[,1:20],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X2)
  X1fG1[k] <- anova(obj4, obj3)$F[2]
  
  #Conditional F-stats:Group 2 X1
  obj1 <- lm(X1~X2)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X1~X2+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[,21:40],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X2)
  X1fG2[k] <- anova(obj4, obj3)$F[2]
  
  #Conditional F-stats:Group 3 X1
  obj1 <- lm(X1~X2)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X1~X2+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[,41:60],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X2)
  X1fG3[k] <- anova(obj4, obj3)$F[2]
  
  #Conditional F-stats:Group 4 X1
  obj1 <- lm(X1~X2)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X1~X2+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[61:80],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X2)
  X1fG4[k] <- anova(obj4, obj3)$F[2]
  
  #Conditional F-stats:Group 5 X1
  obj1 <- lm(X1~X2)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X1~X2+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[,81:100],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X2)
  X1fG5[k] <- anova(obj4, obj3)$F[2]
  
  
  
  
  #Conditional F-stats:Group 1 X2
  obj1 <- lm(X2~X1)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X2~X1+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[,1:20],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X1)
  X2fG1[k] <- anova(obj4, obj3)$F[2]
  
  #Conditional F-stats:Group 2 X2
  obj1 <- lm(X2~X1)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X2~X1+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[,21:40],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X1)
  X2fG2[k] <- anova(obj4, obj3)$F[2]
  
  #Conditional F-stats:Group 3 X2
  obj1 <- lm(X2~X1)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X2~X1+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[,41:60],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X1)
  X2fG3[k] <- anova(obj4, obj3)$F[2]
  
  #Conditional F-stats:Group 4 X2
  obj1 <- lm(X2~X1)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X2~X1+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[61:80],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X1)
  X2fG4[k] <- anova(obj4, obj3)$F[2]
  
  #Conditional F-stats:Group 5 X2
  obj1 <- lm(X2~X1)
  preds1 <- fitted.values(obj1)
  obj2 <- lm(X2~X1+preds1)
  res2<-residuals(obj2)
  G.dat2<-data.frame(G.dat[,81:100],res2)
  obj3 <- lm(res2~.,data=G.dat2)
  obj4 <- lm(res2~X1)
  X2fG5[k] <- anova(obj4, obj3)$F[2]
  
  #Define empty vectors for sargan p-values
  X1sarG1[k]<-fit1$diagnostics[4,4]
  X1sarG2[k]<-fit2$diagnostics[4,4]
  X1sarG3[k]<-fit3$diagnostics[4,4]
  X1sarG4[k]<-fit4$diagnostics[4,4]
  X1sarG5[k]<-fit5$diagnostics[4,4]
  
}

X1est_out<-c(mean(X1estG1),
             mean(X1estG2),
             mean(X1estG3),
             mean(X1estG4),
             mean(X1estG5))

X2est_out<-c(mean(X2estG1),
             mean(X2estG2),
             mean(X2estG3),
             mean(X2estG4),
             mean(X2estG5))

X1se_out<-c(mean(X1seG1),
            mean(X1seG2),
            mean(X1seG3),
            mean(X1seG4),
            mean(X1seG5))

X2se_out<-c(mean(X2seG1),
            mean(X2seG2),
            mean(X2seG3),
            mean(X2seG4),
            mean(X2seG5))

X1f_out<-c(mean(X1fG1),
           mean(X1fG2),
           mean(X1fG3),
           mean(X1fG4),
           mean(X1fG5))

X2f_out<-c(mean(X2fG1),
           mean(X2fG2),
           mean(X2fG3),
           mean(X2fG4),
           mean(X2fG5))

sar_out<-c(mean(X1sarG1),
           mean(X1sarG2),
           mean(X1sarG3),
           mean(X1sarG4),
           mean(X1sarG5))

results<-data.frame(X1est_out,X1se_out,X1f_out,
                    X1est_out - 1.96 * X1se_out,
                    X1est_out + 1.96 * X1se_out,
                    X2est_out,X2se_out,X2f_out,
                    X2est_out - 1.96 * X2se_out,
                    X2est_out + 1.96 * X2se_out,
                    sar_out,factor(rep(1:5)))

names(results)<-c("X1_Beta","X1_se","X1_F-statistic","X1lci","X1uci",
                  "X2_Beta","X2_se","X2_F-statistic","X2lci","X2uci",
                  "Sargan_pval","Group")

#Plot results

plotdat<-data.frame(c(results$X1_Beta[1],results$X2_Beta[1],
                      results$X1_Beta[2],results$X2_Beta[2],
                      results$X1_Beta[3],results$X2_Beta[3],
                      results$X1_Beta[4],results$X2_Beta[4],
                      results$X1_Beta[5],results$X2_Beta[5]),
                    c(results$X1lci[1],results$X2lci[1],
                      results$X1lci[2],results$X2lci[2],
                      results$X1lci[3],results$X2lci[3],
                      results$X1lci[4],results$X2lci[4],
                      results$X1lci[5],results$X2lci[5]),
                    c(results$X1uci[1],results$X2uci[1],
                      results$X1uci[2],results$X2uci[2],
                      results$X1uci[3],results$X2uci[3],
                      results$X1uci[4],results$X2uci[4],
                      results$X1uci[5],results$X2uci[5]),
                    c(1,2,4,5,7,8,10,11,13,14),
                    c("X1","X2","X1","X2","X1","X2","X1","X2","X1","X2"))

names(plotdat)<-c("Beta","lci","uci","Group","Exposure")

plotdat$Exposure<-factor(plotdat$Exposure)

  Figure1<-ggplot(data = plotdat, aes(x = Group*(-1), y = Beta)) + geom_point(aes(colour = Exposure))+ coord_flip()+theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+ggtitle("")+
  geom_segment(aes(x = Group*(-1), y = lci, xend = Group*(-1), yend = uci, colour = Exposure), data = plotdat)+
  geom_hline(yintercept = 1,linetype = "dashed")+
  scale_x_continuous(limits = c(-15,0),name = "Instrument Group",breaks = c(1.5,4.5,7.5,10.5,13.5)*(-1),labels = c("1","2","3","4","5"))+
  scale_color_manual(name = "Estimated exposure",values=c("X1"="#56B4E9","X2"="#D55E00"))+
  ylab("Mean effect estimate")

png(filename = "MVMR_sim.png",
    width = 1920 , height = 1080, units = "px", res=300,
    bg = "white")

Figure1

dev.off()

png(filename = "TB.png",
    width = 1920 , height = 1920, units = "px", res=300,
    bg = "white")

plot(density(B, kernel = "gaussian", adjust = 1))

dev.off()


