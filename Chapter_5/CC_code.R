#Load libraries and read data
library(MASS)
library(xtable)
library(nlme)
library(lme4)

#####################
### Data cleaning ###
#####################

#Define variable types

data$t2d0<-as.factor(data$t2d0)
data$t2d1<-as.factor(data$t2d1)
data$gender<-as.factor(data$gender)
data$bmi0<-scale(data$bmi0)
data$bmi1<-scale(data$bmi1)
data$alc0<-data$alc0*(-1)
data$alc0<-as.factor(data$alc0)
data$alc1<-data$alc1*(-1)
data$alc1<-as.factor(data$alc1)
data$vact0<-scale(data$vact0)
data$vact1<-scale(data$vact1)
data$s_status0<-as.factor(data$s_status0)
data$s_status1<-as.factor(data$s_status1)
data$stroke0<-as.factor(data$stroke0)
data$stroke1<-as.factor(data$stroke1)

#Recode alcohol in ascending order
levels(data$alc0)<-c("1","2","3","4","5","6")
levels(data$alc1)<-c("1","2","3","4","5","6")

##############################
# Repeated Measures Analyses #
##############################

#Reformat data

id_numbers<-rep(data$id,2)

g_measures<-rep(data$gender,2)

bmi_measures<-c(data$bmi0,data$bmi1)

sbp_measures<-c(data$sbp0,data$sbp1)

vact_measures<-c(data$vact0,data$vact1)

alc_measures<-c(data$alc0,data$alc1)

smk_measures<-c(data$s_status0,data$s_status1)

t2d_measures<-c(data$t2d0,data$t2d1)

stk_measures<-c(data$stroke0,data$stroke1)

time<-c(rep("T0",nrow(data)),rep("T1",nrow(data)))

d2<-data.frame(id_numbers,g_measures,bmi_measures,
               sbp_measures,vact_measures,alc_measures,
               smk_measures,t2d_measures,stk_measures,time)

names(d2)<-c("id","gender","bmi","sbp","vact","alc","smk","t2d","stk","time")

d2$gender<-as.factor(d2$gender)
d2$time<-as.factor(d2$time)
d2$alc<-as.factor(d2$alc)
d2$t2d<-as.factor(d2$t2d)
d2$stk<-as.factor(d2$stk)

#BMI

bmi.lmer1 <- summary(lmer(bmi ~gender + sbp + vact + alc + smk + t2d+ (1|id), data = d2))

bmi.lmer1CI<-confint(lmer(bmi ~gender + sbp + vact + alc + smk + t2d+ (1|id), data = d2))

p.bmi1 <- (1 - pnorm(abs(bmi.lmer1$coefficients[,3]), 0, 1)) * 2

bmi.coefs1<-cbind(bmi.lmer1$coefficients,p.bmi1)




bmi.lmer2 <- summary(lmer(bmi ~gender + alc + (1|id), data = d2))

bmi.lmer2CI<-confint(lmer(bmi ~gender + alc + (1|id), data = d2))

p.bmi2 <- (1 - pnorm(abs(bmi.lmer2$coefficients[,3]), 0, 1)) * 2

bmi.coefs2<-cbind(bmi.lmer2$coefficients,p.bmi2)




bmi.lmer3 <- summary(lmer(bmi ~gender + sbp + (1|id), data = d2))

bmi.lmer3CI<-confint(lmer(bmi ~gender + sbp + (1|id), data = d2))

p.bmi3 <- (1 - pnorm(abs(bmi.lmer3$coefficients[,3]), 0, 1)) * 2

bmi.coefs3<-cbind(bmi.lmer3$coefficients,p.bmi3)



bmi.lmer4 <- summary(lmer(bmi ~gender + vact + (1|id), data = d2))

bmi.lmer4CI<-confint(lmer(bmi ~gender + vact + (1|id), data = d2))

p.bmi4 <- (1 - pnorm(abs(bmi.lmer4$coefficients[,3]), 0, 1)) * 2

bmi.coefs4<-cbind(bmi.lmer4$coefficients,p.bmi4)



bmi.lmer5 <- summary(lmer(bmi ~gender + smk + (1|id), data = d2))

bmi.lmer5CI<-confint(lmer(bmi ~gender + smk + (1|id), data = d2))

p.bmi5 <- (1 - pnorm(abs(bmi.lmer5$coefficients[,3]), 0, 1)) * 2

bmi.coefs5<-cbind(bmi.lmer5$coefficients,p.bmi5)


bmi.lmer6 <- summary(lmer(bmi ~gender + t2d + (1|id), data = d2))

bmi.lmer6CI<-confint(lmer(bmi ~gender + t2d + (1|id), data = d2))

p.bmi6 <- (1 - pnorm(abs(bmi.lmer6$coefficients[,3]), 0, 1)) * 2

bmi.coefs6<-cbind(bmi.lmer6$coefficients,p.bmi6)


#SBP

sbp.lmer1 <- summary(lmer(sbp ~gender + bmi + vact + alc + smk + t2d+ (1|id), data = d2))

sbp.lmer1CI<-confint(lmer(sbp ~gender + bmi + vact + alc + smk + t2d+ (1|id), data = d2))

p.sbp1 <- (1 - pnorm(abs(sbp.lmer1$coefficients[,3]), 0, 1)) * 2

sbp.coefs1<-cbind(sbp.lmer1$coefficients,p.sbp1)


sbp.lmer2 <- summary(lmer(sbp ~gender + alc + (1|id), data = d2))

sbp.lmer2CI<-confint(lmer(sbp ~gender + alc + (1|id), data = d2))

p.sbp2 <- (1 - pnorm(abs(sbp.lmer2$coefficients[,3]), 0, 1)) * 2

sbp.coefs2<-cbind(sbp.lmer2$coefficients,p.sbp2)


sbp.lmer3 <- summary(lmer(sbp ~gender + bmi + (1|id), data = d2))

sbp.lmer3CI<-confint(lmer(sbp ~gender + bmi + (1|id), data = d2))

p.sbp3 <- (1 - pnorm(abs(sbp.lmer3$coefficients[,3]), 0, 1)) * 2

sbp.coefs3<-cbind(sbp.lmer3$coefficients,p.sbp3)


sbp.lmer4 <- summary(lmer(sbp ~gender + vact + (1|id), data = d2))

sbp.lmer4CI<-confint(lmer(sbp ~gender + vact + (1|id), data = d2))

p.sbp4 <- (1 - pnorm(abs(sbp.lmer4$coefficients[,3]), 0, 1)) * 2

sbp.coefs4<-cbind(sbp.lmer4$coefficients,p.sbp4)


sbp.lmer5 <- summary(lmer(sbp ~gender + smk + (1|id), data = d2))

sbp.lmer5CI<-confint(lmer(sbp ~gender + smk + (1|id), data = d2))

p.sbp5 <- (1 - pnorm(abs(sbp.lmer5$coefficients[,3]), 0, 1)) * 2

sbp.coefs5<-cbind(sbp.lmer5$coefficients,p.sbp5)


sbp.lmer6 <- summary(lmer(sbp ~gender + t2d + (1|id), data = d2))

sbp.lmer6CI<-confint(lmer(sbp ~gender + t2d + (1|id), data = d2))

p.sbp6 <- (1 - pnorm(abs(sbp.lmer5$coefficients[,3]), 0, 1)) * 2

sbp.coefs6<-cbind(sbp.lmer6$coefficients,p.sbp6)



#SMK

smk.lmer1 <- summary(lmer(smk ~gender + sbp + bmi + alc + vact + t2d+ (1|id), data = d2))

smk.lmer1CI <- confint(lmer(smk ~gender + sbp + bmi + alc + vact + t2d+ (1|id), data = d2))

p.smk1 <- (1 - pnorm(abs(smk.lmer1$coefficients[,3]), 0, 1)) * 2

smk.coefs1<-cbind(smk.lmer1$coefficients,p.smk1)


smk.lmer2 <- summary(lmer(smk ~gender + alc + (1|id), data = d2))

smk.lmer2CI <- confint(lmer(smk ~gender + alc + (1|id), data = d2))

p.smk2 <- (1 - pnorm(abs(smk.lmer2$coefficients[,3]), 0, 1)) * 2

smk.coefs2<-cbind(smk.lmer2$coefficients,p.smk2)


smk.lmer3 <- summary(lmer(smk ~gender + bmi + (1|id), data = d2))

smk.lmer3CI <- confint(lmer(smk ~gender + bmi + (1|id), data = d2))

p.smk3 <- (1 - pnorm(abs(smk.lmer3$coefficients[,3]), 0, 1)) * 2

smk.coefs3<-cbind(smk.lmer3$coefficients,p.smk3)


smk.lmer4 <- summary(lmer(smk ~gender + sbp + (1|id), data = d2))

smk.lmer4CI <- confint(lmer(smk ~gender + sbp + (1|id), data = d2))

p.smk4 <- (1 - pnorm(abs(smk.lmer4$coefficients[,3]), 0, 1)) * 2

smk.coefs4<-cbind(smk.lmer4$coefficients,p.smk4)


smk.lmer5 <- summary(lmer(smk ~gender + vact + (1|id), data = d2))

smk.lmer5CI <- confint(lmer(smk ~gender + vact + (1|id), data = d2))

p.smk5 <- (1 - pnorm(abs(smk.lmer5$coefficients[,3]), 0, 1)) * 2

smk.coefs5<-cbind(smk.lmer5$coefficients,p.smk5)


smk.lmer6 <- summary(lmer(smk ~gender + t2d + (1|id), data = d2))

smk.lmer6CI <- confint(lmer(smk ~gender + t2d + (1|id), data = d2))

p.smk6 <- (1 - pnorm(abs(smk.lmer6$coefficients[,3]), 0, 1)) * 2

smk.coefs6<-cbind(smk.lmer6$coefficients,p.smk6)



#Vact

vact.lmer1 <- summary(lmer(vact ~gender + sbp + bmi + alc + smk + t2d+ (1|id), data = d2))

vact.lmer1CI <- confint(lmer(vact ~gender + sbp + bmi + alc + smk + t2d+ (1|id), data = d2))

p.vact1 <- (1 - pnorm(abs(vact.lmer1$coefficients[,3]), 0, 1)) * 2

vact.coefs1<-cbind(vact.lmer1$coefficients,p.vact1)


vact.lmer2 <- summary(lmer(vact ~gender + alc + (1|id), data = d2))

vact.lmer2CI <- confint(lmer(vact ~gender + alc + (1|id), data = d2))

p.vact2 <- (1 - pnorm(abs(vact.lmer2$coefficients[,3]), 0, 1)) * 2

vact.coefs2<-cbind(vact.lmer2$coefficients,p.vact2)


vact.lmer3 <- summary(lmer(vact ~gender + bmi + (1|id), data = d2))

vact.lmer3CI <- confint(lmer(vact ~gender + bmi + (1|id), data = d2))

p.vact3 <- (1 - pnorm(abs(vact.lmer3$coefficients[,3]), 0, 1)) * 2

vact.coefs3<-cbind(vact.lmer3$coefficients,p.vact3)


vact.lmer4 <- summary(lmer(vact ~gender + sbp + (1|id), data = d2))

vact.lmer4CI <- confint(lmer(vact ~gender + sbp + (1|id), data = d2))

p.vact4 <- (1 - pnorm(abs(vact.lmer4$coefficients[,3]), 0, 1)) * 2

vact.coefs4<-cbind(vact.lmer4$coefficients,p.vact4)


vact.lmer5 <- summary(lmer(vact ~gender + smk + (1|id), data = d2))

vact.lmer5CI <- confint(lmer(vact ~gender + smk + (1|id), data = d2))

p.vact5 <- (1 - pnorm(abs(vact.lmer5$coefficients[,3]), 0, 1)) * 2

vact.coefs5<-cbind(vact.lmer5$coefficients,p.vact5)


vact.lmer6 <- summary(lmer(vact ~gender + t2d + (1|id), data = d2))

vact.lmer6CI <- confint(lmer(vact ~gender + t2d + (1|id), data = d2))

p.vact6 <- (1 - pnorm(abs(vact.lmer6$coefficients[,3]), 0, 1)) * 2

vact.coefs6<-cbind(vact.lmer6$coefficients,p.vact6)



# Alcohol testing

a_cat1.1<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 1){
    a_cat1.1[i] <- 1
  }
}

a_cat1.2<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc1[i] == 1){
    a_cat1.2[i] <- 1
  }
}

non_drinker<-c(a_cat1.1,a_cat1.2)

a_cat2.1<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 2){
    a_cat2.1[i] <- 1
  }
}

a_cat2.2<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc1[i] == 2){
    a_cat2.2[i] <- 1
  }
}

so_drinker<-c(a_cat2.1,a_cat2.2)

a_cat3.1<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 3){
    a_cat3.1[i] <- 1
  }
}

a_cat3.2<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc1[i] == 3){
    a_cat3.2[i] <- 1
  }
}

month13_drinker<-c(a_cat3.1,a_cat3.2)

a_cat4.1<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 4){
    a_cat4.1[i] <- 1
  }
}

a_cat4.2<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc1[i] == 4){
    a_cat4.2[i] <- 1
  }
}

week12_drinker<-c(a_cat4.1,a_cat4.2)

a_cat5.1<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 5){
    a_cat5.1[i] <- 1
  }
}

a_cat5.2<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc1[i] == 5){
    a_cat5.2[i] <- 1
  }
}

week34_drinker<-c(a_cat5.1,a_cat5.2)

a_cat6.1<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 6){
    a_cat6.1[i] <- 1
  }
}

a_cat6.2<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc1[i] == 6){
    a_cat6.2[i] <- 1
  }
}

daily_drinker<-c(a_cat6.1,a_cat6.2)

d2<-cbind(d2,non_drinker,so_drinker,month13_drinker,week12_drinker,week34_drinker,daily_drinker)



#non-so drinker

dummy.dat1.1<-d2[d2$time == "T0",]

dummy.dat1.1<-dummy.dat1.1[dummy.dat1.1$alc == 1 | dummy.dat1.1$alc == 2,]

dummy.dat1.2<-d2[d2$time == "T1",]

dummy.dat1.2<-dummy.dat1.2[dummy.dat1.2$alc == 1 | dummy.dat1.2$alc == 2,]

dummy.dat1.1<-dummy.dat1.1[dummy.dat1.1$id %in% dummy.dat1.2$id,]

dummy.dat1.2<-dummy.dat1.2[dummy.dat1.2$id %in% dummy.dat1.1$id,]

dummy.dat1<-rbind(dummy.dat1.1,dummy.dat1.2)

dummy.dat1$alc<-factor(dummy.dat1$alc)

levels(dummy.dat1$alc)<-c("0","1")


alc.model1.1 <- glmer(alc ~ gender + bmi  + (1 | id), data = dummy.dat1, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

A1<-summary(alc.model1.1)

exp(A1$coefficients[,1])

exp(confint(alc.model1.1))


alc.model1.2 <- glmer(alc ~ gender + sbp  + (1 | id), data = dummy.dat1, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

A2<-summary(alc.model1.2)

exp(A2$coefficients[,1])

exp(confint.merMod(alc.model1.2,method="Wald"))

alc.model1.3 <- glmer(alc ~ gender + smk  + (1 | id), data = dummy.dat1, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

A3<-summary(alc.model1.3)

exp(A3$coefficients[,1])

exp(confint.merMod(alc.model1.3,method="Wald"))

alc.model1.4 <- glmer(alc ~ gender + vact  + (1 | id), data = dummy.dat1, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

A4<-summary(alc.model1.4)

exp(A4$coefficients[,1])

exp(confint.merMod(alc.model1.4,method="Wald"))

alc.model1.5 <- glmer(alc ~ gender + t2d  + (1 | id), data = dummy.dat1, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

A5<-summary(alc.model1.5)

exp(A5$coefficients[,1])

exp(confint.merMod(alc.model1.5,method="Wald"))



#non-13month drinker

dummy.dat2.1<-d2[d2$time == "T0",]

dummy.dat2.1<-dummy.dat2.1[dummy.dat2.1$alc == 1 | dummy.dat2.1$alc == 3,]

dummy.dat2.2<-d2[d2$time == "T1",]

dummy.dat2.2<-dummy.dat2.2[dummy.dat2.2$alc == 1 | dummy.dat2.2$alc == 3,]

dummy.dat2.1<-dummy.dat2.1[dummy.dat2.1$id %in% dummy.dat2.2$id,]

dummy.dat2.2<-dummy.dat2.2[dummy.dat2.2$id %in% dummy.dat2.1$id,]

dummy.dat2<-rbind(dummy.dat2.1,dummy.dat2.2)

dummy.dat2$alc<-factor(dummy.dat2$alc)

levels(dummy.dat2$alc)<-c("0","1")

alc.model2.1 <- glmer(alc ~ gender + bmi  + (1 | id), data = dummy.dat2, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

B1<-summary(alc.model2.1)

exp(B1$coefficients[,1])

exp(confint.merMod(alc.model2.1,method="Wald"))

alc.model2.2 <- glmer(alc ~ gender + sbp  + (1 | id), data = dummy.dat2, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

B2<-summary(alc.model2.2)

exp(B2$coefficients[,1])

exp(confint.merMod(alc.model2.2,method="Wald"))

alc.model2.3 <- glmer(alc ~ gender + smk  + (1 | id), data = dummy.dat2, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

B3<-summary(alc.model2.3)

exp(B3$coefficients[,1])

exp(confint.merMod(alc.model2.3,method="Wald"))

alc.model2.4 <- glmer(alc ~ gender + vact  + (1 | id), data = dummy.dat2, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

B4<-summary(alc.model2.4)

exp(B4$coefficients[,1])

exp(confint.merMod(alc.model2.4,method="Wald"))

alc.model2.5 <- glmer(alc ~ gender + t2d  + (1 | id), data = dummy.dat2, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

B5<-summary(alc.model2.5)

exp(B5$coefficients[,1])

exp(confint.merMod(alc.model2.5,method="Wald"))




#non-12week drinker

dummy.dat3.1<-d2[d2$time == "T0",]

dummy.dat3.1<-dummy.dat3.1[dummy.dat3.1$alc == 1 | dummy.dat3.1$alc == 4,]

dummy.dat3.2<-d2[d2$time == "T1",]

dummy.dat3.2<-dummy.dat3.2[dummy.dat3.2$alc == 1 | dummy.dat3.2$alc == 4,]

dummy.dat3.1<-dummy.dat3.1[dummy.dat3.1$id %in% dummy.dat3.2$id,]

dummy.dat3.2<-dummy.dat3.2[dummy.dat3.2$id %in% dummy.dat3.1$id,]

dummy.dat3<-rbind(dummy.dat3.1,dummy.dat3.2)

dummy.dat3$alc<-factor(dummy.dat3$alc)

levels(dummy.dat3$alc)<-c("0","1")

alc.model3.1 <- glmer(alc ~ gender + bmi  + (1 | id), data = dummy.dat3, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

C1<-summary(alc.model3.1)

exp(C1$coefficients[,1])

exp(confint.merMod(alc.model3.1,method="Wald"))

alc.model3.2 <- glmer(alc ~ gender + sbp  + (1 | id), data = dummy.dat3, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

C2<-summary(alc.model3.2)

exp(C2$coefficients[,1])

exp(confint.merMod(alc.model3.2,method="Wald"))

alc.model3.3 <- glmer(alc ~ gender + smk  + (1 | id), data = dummy.dat3, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

C3<-summary(alc.model3.3)

exp(C3$coefficients[,1])

exp(confint.merMod(alc.model3.3,method="Wald"))

alc.model3.4 <- glmer(alc ~ gender + vact  + (1 | id), data = dummy.dat3, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

C4<-summary(alc.model3.4)

exp(C4$coefficients[,1])

exp(confint.merMod(alc.model3.4,method="Wald"))


alc.model3.5 <- glmer(alc ~ gender + t2d  + (1 | id), data = dummy.dat3, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

C5<-summary(alc.model3.5)

exp(C5$coefficients[,1])

exp(confint.merMod(alc.model3.5,method="Wald"))

#non-34week drinker

dummy.dat4.1<-d2[d2$time == "T0",]

dummy.dat4.1<-dummy.dat4.1[dummy.dat4.1$alc == 1 | dummy.dat4.1$alc == 5,]

dummy.dat4.2<-d2[d2$time == "T1",]

dummy.dat4.2<-dummy.dat4.2[dummy.dat4.2$alc == 1 | dummy.dat4.2$alc == 5,]

dummy.dat4.1<-dummy.dat4.1[dummy.dat4.1$id %in% dummy.dat4.2$id,]

dummy.dat4.2<-dummy.dat4.2[dummy.dat4.2$id %in% dummy.dat4.1$id,]

dummy.dat4<-rbind(dummy.dat4.1,dummy.dat4.2)

dummy.dat4$alc<-factor(dummy.dat4$alc)

levels(dummy.dat4$alc)<-c("0","1")

alc.model4.1 <- glmer(alc ~ gender + bmi  + (1 | id), data = dummy.dat4, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

D1<-summary(alc.model4.1)

exp(D1$coefficients[,1])

exp(confint.merMod(alc.model4.1,method="Wald"))

alc.model4.2 <- glmer(alc ~ gender + sbp  + (1 | id), data = dummy.dat4, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

D2<-summary(alc.model4.2)

exp(D2$coefficients[,1])

exp(confint.merMod(alc.model4.2,method="Wald"))

alc.model4.3 <- glmer(alc ~ gender + smk  + (1 | id), data = dummy.dat4, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

D3<-summary(alc.model4.3)

exp(D3$coefficients[,1])

exp(confint.merMod(alc.model4.3,method="Wald"))

alc.model4.4 <- glmer(alc ~ gender + vact  + (1 | id), data = dummy.dat4, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

D4<-summary(alc.model4.4)

exp(D4$coefficients[,1])

exp(confint.merMod(alc.model4.4,method="Wald"))

alc.model4.5 <- glmer(alc ~ gender + t2d  + (1 | id), data = dummy.dat4, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

D5<-summary(alc.model4.5)

exp(D5$coefficients[,1])

exp(confint.merMod(alc.model4.5,method="Wald"))


#non-daily drinker

dummy.dat5.1<-d2[d2$time == "T0",]

dummy.dat5.1<-dummy.dat5.1[dummy.dat5.1$alc == 1 | dummy.dat5.1$alc == 6,]

dummy.dat5.2<-d2[d2$time == "T1",]

dummy.dat5.2<-dummy.dat5.2[dummy.dat5.2$alc == 1 | dummy.dat5.2$alc == 6,]

dummy.dat5.1<-dummy.dat5.1[dummy.dat5.1$id %in% dummy.dat5.2$id,]

dummy.dat5.2<-dummy.dat5.2[dummy.dat5.2$id %in% dummy.dat5.1$id,]

dummy.dat5<-rbind(dummy.dat5.1,dummy.dat5.2)

dummy.dat5$alc<-factor(dummy.dat5$alc)

levels(dummy.dat5$alc)<-c("0","1")

alc.model5.1 <- glmer(alc ~ gender + bmi  + (1 | id), data = dummy.dat5, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

E1<-summary(alc.model5.1)

exp(E1$coefficients[,1])

exp(confint.merMod(alc.model5.1,method="Wald"))

alc.model5.2 <- glmer(alc ~ gender + sbp  + (1 | id), data = dummy.dat5, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

E2<-summary(alc.model5.2)

exp(E2$coefficients[,1])

exp(confint.merMod(alc.model5.2,method="Wald"))

alc.model5.3 <- glmer(alc ~ gender + smk  + (1 | id), data = dummy.dat5, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

E3<-summary(alc.model5.3)

exp(E3$coefficients[,1])

exp(confint.merMod(alc.model5.3,method="Wald"))

alc.model5.4 <- glmer(alc ~ gender + vact  + (1 | id), data = dummy.dat5, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

E4<-summary(alc.model5.4)

exp(E4$coefficients[,1])

exp(confint.merMod(alc.model5.4,method="Wald"))

alc.model5.5 <- glmer(alc ~ gender + t2d  + (1 | id), data = dummy.dat5, family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

E5<-summary(alc.model5.5)

exp(E5$coefficients[,1])

exp(confint.merMod(alc.model5.5,method="Wald"))


#Saturated

alc.model6 <- glmer(alc ~ gender + bmi + sbp + smk + vact + t2d  + (1 | id), data = dummy.dat1, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

F1<-summary(alc.model6)

exp(F1$coefficients[,1])

exp(confint.merMod(alc.model6,method="Wald"))

alc.model7 <- glmer(alc ~ gender + bmi + sbp + smk + vact + t2d  + (1 | id), data = dummy.dat2, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

F2<-summary(alc.model7)

exp(F2$coefficients[,1])

exp(confint.merMod(alc.model7,method="Wald"))

alc.model8 <- glmer(alc ~ gender + bmi + sbp + smk + vact + t2d  + (1 | id), data = dummy.dat3, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

F3<-summary(alc.model8)

exp(F3$coefficients[,1])

exp(confint.merMod(alc.model8,method="Wald"))

alc.model9 <- glmer(alc ~ gender + bmi + sbp + smk + vact + t2d  + (1 | id), data = dummy.dat4, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

F4<-summary(alc.model9)

exp(F4$coefficients[,1])

exp(confint.merMod(alc.model9,method="Wald"))


alc.model10 <- glmer(alc ~ gender + bmi + sbp + smk + vact + t2d  + (1 | id), data = dummy.dat5, family = binomial,
                     control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

F5<-summary(alc.model10)

exp(F5$coefficients[,1])

exp(confint.merMod(alc.model10,method="Wald"))




#T2D

#t2d.dat1<-d2[d2$time == "T0",]

#t2d.dat1.1<-t2d.dat1[t2d.dat1$t2d == 1,]

#t2d.dat2<-d2[d2$time == "T1",]

#t2d.dat2<-t2d.dat2[t2d.dat2$id %in% t2d.dat1.1$id,]

#t2d.dat<-rbind(t2d.dat1.1,t2d.dat2)

#t2d.dat$t2d<-factor(t2d.dat$t2d)

#levels(t2d.dat$t2d)<-c("0","1")


t2d.model1 <- glmer(t2d ~ gender + alc  + (1 | id), data = d2, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

T2D_sum1<-summary(t2d.model1)

exp(T2D_sum1$coefficients[,1])

exp(confint.merMod(t2d.model1,method="Wald"))

summary(t2d.model1)$coef[,4]


t2d.model2 <- glmer(t2d ~ gender + bmi  + (1 | id), data = d2, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

T2D_sum2<-summary(t2d.model2)

exp(T2D_sum2$coefficients[,1])

exp(confint.merMod(t2d.model2,method="Wald"))

summary(t2d.model2)$coef[,4]

t2d.model3 <- glmer(t2d ~ gender + sbp  + (1 | id), data = d2, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

T2D_sum3<-summary(t2d.model3)

exp(T2D_sum3$coefficients[,1])

exp(confint.merMod(t2d.model3,method="Wald"))

summary(t2d.model3)$coef[,4]


t2d.model4 <- glmer(t2d ~ gender + scale(smk)  + (1 | id), data = d2, family = binomial,
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),nAGQ = 10)

T2D_sum4<-summary(t2d.model4)

exp(T2D_sum4$coefficients[,1])

exp(confint.merMod(t2d.model4,method="Wald"))

summary(t2d.model4)$coef[,4]


t2d.model5 <- glmer(t2d ~ gender + vact  + (1 | id), data = d2, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),nAGQ = 10)

T2D_sum5<-summary(t2d.model5)

exp(T2D_sum5$coefficients[,1])

exp(confint.merMod(t2d.model5,method="Wald"))

summary(t2d.model5)$coef[,4]

t2d.dat<-data[data$t2d0 == 0,]

t2d.dat_cases<-t2d.dat[t2d.dat$t2d1 == 1,]

t2d.dat_controls<-t2d.dat[!t2d.dat$t2d1 == 1,]

set.seed(12345)

t2d.dat_controls<-t2d.dat_controls[sample(nrow(t2d.dat_controls), nrow(t2d.dat_cases)*4), ,]

t2d.dat<-rbind(t2d.dat_controls,t2d.dat_cases)

t2d_cc<-glm(t2d1 ~ gender+bmi0+s_amount0+sbp0+alc0+vact0, data = t2d.dat,family = "binomial")

t2d_cc_res<-cbind(exp(coef(t2d_cc)),exp(confint(t2d_cc)),summary(t2d_cc)$coef[,4])

print(xtable(t2d_cc_res,digits=3),type="html")

#Stroke

stroke.dat<-data[data$stroke0 == 0,]

stroke.dat_cases<-stroke.dat[stroke.dat$stroke1 == 1,]

stroke.dat_controls<-stroke.dat[!stroke.dat$stroke0 == 1,]

set.seed(12345)

stroke.dat_controls<-stroke.dat_controls[sample(nrow(stroke.dat_controls), nrow(stroke.dat_cases)*4), ,]

stroke.dat<-rbind(stroke.dat_controls,stroke.dat_cases)

stroke_cc<-glm(stroke1 ~ gender+bmi0+s_amount0+sbp0+alc0+vact0+t2d0, data = stroke.dat,family = "binomial")

stroke_cc_res<-cbind(exp(coef(stroke_cc)),exp(confint(stroke_cc)),summary(stroke_cc)$coef[,4])

print(xtable(stroke_cc_res,digits=3))

table(stroke.dat$stroke1)




