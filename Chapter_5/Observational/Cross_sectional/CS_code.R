#Load libraries and read data (defined as an object "data")
#install.packages("nnet")
library(MASS)
library(xtable)

#####################
### Data cleaning ###
#####################

#Define variable types

data$t2d0<-as.factor(data$t2d0)
data$gender<-as.factor(data$gender)
data$bmi0<-scale(data$bmi0)
data$sbp0<-scale(data$sbp0)
data$dbp0<-scale(data$dbp0)
data$alc0<-data$alc0*(-1)
data$alc0<-as.factor(data$alc0)
data$vact0<-scale(data$vact0)
data$s_status0<-as.factor(data$s_status0)
data$stroke0<-as.factor(data$stroke0)

#Recode alcohol in ascending order
levels(data$alc0)<-c("1","2","3","4","5","6")

############
# Analyses #
############

#Alcohol

non_drinker<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 1){
    non_drinker[i] <- 1
  }
}

so_drinker<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 2){
    so_drinker[i] <- 1
  }
}

month13_drinker<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 3){
    month13_drinker[i] <- 1
  }
}

week12_drinker<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 4){
    week12_drinker[i] <- 1
  }
}

week34_drinker<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 5){
    week34_drinker[i] <- 1
  }
}

daily_drinker<-rep(0,nrow(data))

for(i in 1:nrow(data)){
  if(data$alc0[i] == 6){
    daily_drinker[i] <- 1
  }
}

data$non_drinker<-non_drinker
data$so_drinker<-so_drinker
data$month13_drinker<-month13_drinker
data$week12_drinker<-week12_drinker
data$week34_drinker<-week34_drinker
data$daily_drinker<-daily_drinker

#################
###  Alcohol  ###
#################

#non-so drinker

dummy.dat1<-data[data$alc0 == 1 | data$alc0 == 2,]

dummy.dat1$alc0<-factor(dummy.dat1$alc0)

levels(dummy.dat1$alc0)<-c("0","1")

alc.model1.1<-glm(alc0 ~ gender + bmi0, data=dummy.dat1, family="binomial")

Alc1.1_res<-cbind(exp(coef(alc.model1.1)),exp(confint(alc.model1.1)),summary(alc.model1.1)$coef[,4])

alc.model1.2<-glm(alc0 ~ gender + sbp0, data=dummy.dat1, family="binomial")

Alc1.2_res<-cbind(exp(coef(alc.model1.2)),exp(confint(alc.model1.2)),summary(alc.model1.2)$coef[,4])

alc.model1.3<-glm(alc0 ~ gender + s_amount0, data=dummy.dat1, family="binomial")

Alc1.3_res<-cbind(exp(coef(alc.model1.3)),exp(confint(alc.model1.3)),summary(alc.model1.3)$coef[,4])

alc.model1.4<-glm(alc0 ~ gender + vact0, data=dummy.dat1, family="binomial")

Alc1.4_res<-cbind(exp(coef(alc.model1.4)),exp(confint(alc.model1.4)),summary(alc.model1.4)$coef[,4])

alc.model1.5<-glm(alc0 ~ gender + t2d0, data=dummy.dat1, family="binomial")

Alc1.5_res<-cbind(exp(coef(alc.model1.5)),exp(confint(alc.model1.5)),summary(alc.model1.5)$coef[,4])

#non-13month drinker

dummy.dat2<-data[data$alc0 == 1 | data$alc0 == 3,]

dummy.dat2$alc0<-factor(dummy.dat2$alc0)

levels(dummy.dat2$alc0)<-c("0","1")

alc.model2.1<-glm(alc0 ~ gender + bmi0, data=dummy.dat2, family="binomial")

Alc2.1_res<-cbind(exp(coef(alc.model2.1)),exp(confint(alc.model2.1)),summary(alc.model2.1)$coef[,4])

alc.model2.2<-glm(alc0 ~ gender + sbp0, data=dummy.dat2, family="binomial")

Alc2.2_res<-cbind(exp(coef(alc.model2.2)),exp(confint(alc.model2.2)),summary(alc.model2.2)$coef[,4])

alc.model2.3<-glm(alc0 ~ gender + s_amount0, data=dummy.dat2, family="binomial")

Alc2.3_res<-cbind(exp(coef(alc.model2.3)),exp(confint(alc.model2.3)),summary(alc.model2.3)$coef[,4])

alc.model2.4<-glm(alc0 ~ gender + vact0, data=dummy.dat2, family="binomial")

Alc2.4_res<-cbind(exp(coef(alc.model2.4)),exp(confint(alc.model2.4)),summary(alc.model2.4)$coef[,4])

alc.model2.5<-glm(alc0 ~ gender + t2d0, data=dummy.dat2, family="binomial")

Alc2.5_res<-cbind(exp(coef(alc.model2.5)),exp(confint(alc.model2.5)),summary(alc.model2.5)$coef[,4])

#non-12week drinker

dummy.dat3<-data[data$alc0 == 1 | data$alc0 == 4,]

dummy.dat3$alc0<-factor(dummy.dat3$alc0)

levels(dummy.dat3$alc0)<-c("0","1")

alc.model3.1<-glm(alc0 ~ gender + bmi0, data=dummy.dat3, family="binomial")

Alc3.1_res<-cbind(exp(coef(alc.model3.1)),exp(confint(alc.model3.1)),summary(alc.model3.1)$coef[,4])

alc.model3.2<-glm(alc0 ~ gender + sbp0, data=dummy.dat3, family="binomial")

Alc3.2_res<-cbind(exp(coef(alc.model3.2)),exp(confint(alc.model3.2)),summary(alc.model3.2)$coef[,4])

alc.model3.3<-glm(alc0 ~ gender + s_amount0, data=dummy.dat3, family="binomial")

Alc3.3_res<-cbind(exp(coef(alc.model3.3)),exp(confint(alc.model3.3)),summary(alc.model3.3)$coef[,4])

alc.model3.4<-glm(alc0 ~ gender + vact0, data=dummy.dat3, family="binomial")

Alc3.4_res<-cbind(exp(coef(alc.model3.4)),exp(confint(alc.model3.4)),summary(alc.model3.4)$coef[,4])

alc.model3.5<-glm(alc0 ~ gender + t2d0, data=dummy.dat3, family="binomial")

Alc3.5_res<-cbind(exp(coef(alc.model3.5)),exp(confint(alc.model3.5)),summary(alc.model3.5)$coef[,4])

#non-34week drinker

dummy.dat4<-data[data$alc0 == 1 | data$alc0 == 5,]

dummy.dat4$alc0<-factor(dummy.dat4$alc0)

levels(dummy.dat4$alc0)<-c("0","1")

alc.model4.1<-glm(alc0 ~ gender + bmi0, data=dummy.dat4, family="binomial")

Alc4.1_res<-cbind(exp(coef(alc.model4.1)),exp(confint(alc.model4.1)),summary(alc.model4.1)$coef[,4])

alc.model4.2<-glm(alc0 ~ gender + sbp0, data=dummy.dat4, family="binomial")

Alc4.2_res<-cbind(exp(coef(alc.model4.2)),exp(confint(alc.model4.2)),summary(alc.model4.2)$coef[,4])

alc.model4.3<-glm(alc0 ~ gender + s_amount0, data=dummy.dat4, family="binomial")

Alc4.3_res<-cbind(exp(coef(alc.model4.3)),exp(confint(alc.model4.3)),summary(alc.model4.3)$coef[,4])

alc.model4.4<-glm(alc0 ~ gender + vact0, data=dummy.dat4, family="binomial")

Alc4.4_res<-cbind(exp(coef(alc.model4.4)),exp(confint(alc.model4.4)),summary(alc.model4.4)$coef[,4])

alc.model4.5<-glm(alc0 ~ gender + t2d0, data=dummy.dat4, family="binomial")

Alc4.5_res<-cbind(exp(coef(alc.model4.5)),exp(confint(alc.model4.5)),summary(alc.model4.5)$coef[,4])

#non-daily drinker

dummy.dat5<-data[data$alc0 == 1 | data$alc0 == 6,]

dummy.dat5$alc0<-factor(dummy.dat5$alc0)

levels(dummy.dat5$alc0)<-c("0","1")

alc.model5.1<-glm(alc0 ~ gender + bmi0, data=dummy.dat5, family="binomial")

Alc5.1_res<-cbind(exp(coef(alc.model5.1)),exp(confint(alc.model5.1)),summary(alc.model5.1)$coef[,4])

alc.model5.2<-glm(alc0 ~ gender + sbp0, data=dummy.dat5, family="binomial")

Alc5.2_res<-cbind(exp(coef(alc.model5.2)),exp(confint(alc.model5.2)),summary(alc.model5.2)$coef[,4])

alc.model5.3<-glm(alc0 ~ gender + s_amount0, data=dummy.dat5, family="binomial")

Alc5.3_res<-cbind(exp(coef(alc.model5.3)),exp(confint(alc.model5.3)),summary(alc.model5.3)$coef[,4])

alc.model5.4<-glm(alc0 ~ gender + vact0, data=dummy.dat5, family="binomial")

Alc5.4_res<-cbind(exp(coef(alc.model5.4)),exp(confint(alc.model5.4)),summary(alc.model5.4)$coef[,4])

alc.model5.5<-glm(alc0 ~ gender + t2d0, data=dummy.dat5, family="binomial")

Alc5.5_res<-cbind(exp(coef(alc.model5.5)),exp(confint(alc.model5.5)),summary(alc.model5.5)$coef[,4])

#Saturated models

alc.model6.1<-glm(alc0 ~ gender + bmi0 + sbp0 + s_amount0 + vact0 + t2d0, data=dummy.dat1, family="binomial")

Alc6.1_res<-cbind(exp(coef(alc.model6.1)),exp(confint(alc.model6.1)),summary(alc.model6.1)$coef[,4])

alc.model6.2<-glm(alc0 ~ gender + bmi0 + sbp0 + s_amount0 + vact0 + t2d0, data=dummy.dat2, family="binomial")

Alc6.2_res<-cbind(exp(coef(alc.model6.2)),exp(confint(alc.model6.2)),summary(alc.model6.2)$coef[,4])

alc.model6.3<-glm(alc0 ~ gender + bmi0 + sbp0 + s_amount0 + vact0 + t2d0, data=dummy.dat3, family="binomial")

Alc6.3_res<-cbind(exp(coef(alc.model6.3)),exp(confint(alc.model6.3)),summary(alc.model6.3)$coef[,4])

alc.model6.4<-glm(alc0 ~ gender + bmi0 + sbp0 + s_amount0 + vact0 + t2d0, data=dummy.dat4, family="binomial")

Alc6.4_res<-cbind(exp(coef(alc.model6.4)),exp(confint(alc.model6.4)),summary(alc.model6.4)$coef[,4])

alc.model6.5<-glm(alc0 ~ gender + bmi0 + sbp0 + s_amount0 + vact0 + t2d0, data=dummy.dat5, family="binomial")

Alc6.5_res<-cbind(exp(coef(alc.model6.5)),exp(confint(alc.model6.5)),summary(alc.model6.5)$coef[,4])

#BMI

BMI1<-lm(bmi0 ~ gender+alc0+sbp0+vact0+s_amount0+t2d0, data = data)

BMI2<-lm(bmi0 ~ gender+alc0, data = data)

BMI3<-lm(bmi0 ~ gender+sbp0, data = data)

BMI4<-lm(bmi0 ~ gender+vact0, data = data)

BMI5<-lm(bmi0 ~ gender+s_amount0, data = data)

BMI6<-lm(bmi0 ~ gender+t2d0, data = data)

#Smoking

SMOKING1<-lm(s_amount0 ~ gender+alc0+bmi0+sbp0+vact0+t2d0, data = data)

SMOKING2<-lm(s_amount0 ~ gender+alc0, data = data)

SMOKING3<-lm(s_amount0 ~ gender+bmi0, data = data)

SMOKING4<-lm(s_amount0 ~ gender+sbp0, data = data)

SMOKING5<-lm(s_amount0 ~ gender+vact0, data = data)

SMOKING6<-lm(s_amount0 ~ gender+t2d0, data = data)

#SBP

SBP1<-lm(sbp0 ~ gender+alc0+bmi0+vact0+s_amount0+t2d0, data = data)

SBP2<-lm(sbp0 ~ gender+alc0, data = data)

SBP3<-lm(sbp0 ~ gender+bmi0, data = data)

SBP4<-lm(sbp0 ~ gender+vact0, data = data)

SBP5<-lm(sbp0 ~ gender+s_amount0, data = data)

SBP6<-lm(sbp0 ~ gender+t2d0, data = data)


#Vig.activity

VA1<-lm(vact0 ~ gender+alc0+bmi0+sbp0+s_amount0+t2d0, data = data)

VA2<-lm(vact0 ~ gender+alc0, data = data)

VA3<-lm(vact0 ~ gender+bmi0, data = data)

VA4<-lm(vact0 ~ gender+sbp0, data = data)

VA5<-lm(vact0 ~ gender+s_amount0, data = data)

VA6<-lm(vact0 ~ gender+t2d0, data = data)

#T2D

T2D1<-glm(t2d0 ~ gender+alc0+bmi0+sbp0+s_amount0+vact0, data = data,family="binomial")
T2D1_res<-cbind(exp(coef(T2D1)),exp(confint(T2D1)),summary(T2D1)$coef[,4])

T2D2<-glm(t2d0 ~ gender+alc0, data = data,family="binomial")
T2D3<-glm(t2d0 ~ gender+bmi0, data = data,family="binomial")
T2D4<-glm(t2d0 ~ gender+sbp0, data = data,family="binomial")
T2D5<-glm(t2d0 ~ gender+s_amount0, data = data,family="binomial")
T2D6<-glm(t2d0 ~ gender+vact0, data = data,family="binomial")

T2D2_res<-cbind(exp(coef(T2D2)),exp(confint(T2D2)),summary(T2D2)$coef[,4])
T2D3_res<-cbind(exp(coef(T2D3)),exp(confint(T2D3)),summary(T2D3)$coef[,4])
T2D4_res<-cbind(exp(coef(T2D4)),exp(confint(T2D4)),summary(T2D4)$coef[,4])
T2D5_res<-cbind(exp(coef(T2D5)),exp(confint(T2D5)),summary(T2D5)$coef[,4])
T2D6_res<-cbind(exp(coef(T2D6)),exp(confint(T2D6)),summary(T2D6)$coef[,4])

#Stroke

STROKE1<-glm(stroke0 ~ gender+alc0+bmi0+sbp0+s_amount0+vact0+t2d0, data = data,family="binomial")

STROKE1_res<-cbind(exp(coef(STROKE1)),exp(confint(STROKE1)),summary(STROKE1)$coef[,4])

STROKE2<-glm(stroke0 ~ gender+alc0, data = data,family="binomial")
STROKE3<-glm(stroke0 ~ gender+bmi0, data = data,family="binomial")
STROKE4<-glm(stroke0 ~ gender+sbp0, data = data,family="binomial")
STROKE5<-glm(stroke0 ~ gender+s_amount0, data = data,family="binomial")
STROKE6<-glm(stroke0 ~ gender+vact0, data = data,family="binomial")
STROKE7<-glm(stroke0 ~ gender+t2d0, data = data,family="binomial")

STROKE2_res<-cbind(exp(coef(STROKE2)),exp(confint(STROKE2)),summary(STROKE2)$coef[,4])
STROKE3_res<-cbind(exp(coef(STROKE3)),exp(confint(STROKE3)),summary(STROKE3)$coef[,4])
STROKE4_res<-cbind(exp(coef(STROKE4)),exp(confint(STROKE4)),summary(STROKE4)$coef[,4])
STROKE5_res<-cbind(exp(coef(STROKE5)),exp(confint(STROKE5)),summary(STROKE5)$coef[,4])
STROKE6_res<-cbind(exp(coef(STROKE6)),exp(confint(STROKE6)),summary(STROKE6)$coef[,4])
STROKE7_res<-cbind(exp(coef(STROKE7)),exp(confint(STROKE7)),summary(STROKE7)$coef[,4])




