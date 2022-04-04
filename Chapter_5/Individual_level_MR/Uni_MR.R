#Load libraries and read data
library(AER)
data<-read.csv("obs_data.csv",header=T)
library(RadialMR)
library(OneSampleMR)

#############################
# Read in and format scores #
#############################

bmi_scores<-read.csv("scores_bmi.csv",header=T)

sbp_scores<-read.csv("scores_sbp.csv",header=T)

alc_scores<-read.csv("scores_alc.csv",header=T)

smk_scores<-read.csv("scores_smk.csv",header=T)

vact_scores<-read.csv("scores_vact.csv",header=T)

t2d_scores<-read.csv("scores_t2d.csv",header=T)

scores<-cbind(bmi_scores,sbp_scores$sbp_gscore,alc_scores$alc_gscore,smk_scores$smk_gscore,
              vact_scores$vact_gscore,t2d_scores$t2d_gscore)

scores<-scores[scores$id %in% data$id,]

data<-data[data$id %in% scores$id,]

scores<-scores[order(scores$id),]

data$bmi_score<-scores[,2]

data$sbp_score<-scores[,3]

data$alc_score<-scores[,4]

data$smk_score<-scores[,5]

data$vact_score<-scores[,6]

data$t2d_score<-scores[,7]

#####################
### Data cleaning ###
#####################

#Define variable types

data$bmi0<-scale(data$bmi0)
data$sbp0<-scale(data$sbp0)
data$dbp0<-scale(data$dbp0)
data$alc0<-data$alc0*(-1)
data$alc0<-as.factor(data$alc0)
data$vact0<-scale(data$vact0)
data$stroke0<-as.numeric(data$stroke0)

#Recode alcohol in ascending order
levels(data$alc0)<-c("1","2","3","4","5","6")

data$alc0<-as.numeric(data$alc0)

###########################
# Outcome BMI MR Analyses #
###########################

MR_BMI_SBP<-ivreg(bmi0 ~  sbp0|sbp_score, data = data)

summary(MR_BMI_SBP,diagnostics=T)

MR_BMI_ALC<-ivreg(bmi0 ~  alc0|alc_score, data = data)

summary(MR_BMI_ALC,diagnostics=T)

MR_BMI_SMK<-ivreg(bmi0 ~  s_amount0|smk_score, data = data)

summary(MR_BMI_SMK,diagnostics=T)

MR_BMI_VACT<-ivreg(bmi0 ~  vact0|vact_score, data = data)

summary(MR_BMI_VACT,diagnostics=T)

MR_BMI_T2D<-ivreg(bmi0 ~  t2d0|t2d_score, data = data)

summary(MR_BMI_T2D,diagnostics=T)

res_data.coefs<-data.frame("BMI",
                           summary(MR_BMI_SBP)$coefficients[2,1],
                           summary(MR_BMI_ALC)$coefficients[2,1],
                           summary(MR_BMI_SMK)$coefficients[2,1],
                           summary(MR_BMI_VACT)$coefficients[2,1],
                           summary(MR_BMI_T2D)$coefficients[2,1])

names(res_data.coefs)<-c("Outcome","SBP","ALC","SMK","VACT","T2D")

res_data.ps<-data.frame("BMI",
                        summary(MR_BMI_SBP)$coefficients[2,4],
                        summary(MR_BMI_ALC)$coefficients[2,4],
                        summary(MR_BMI_SMK)$coefficients[2,4],
                        summary(MR_BMI_VACT)$coefficients[2,4],
                        summary(MR_BMI_T2D)$coefficients[2,4])

names(res_data.ps)<-c("Outcome","SBP","ALC","SMK","VACT","T2D")



###########################
# Outcome SBP MR Analyses #
###########################



MR_SBP_BMI<-ivreg(sbp0 ~  bmi0|bmi_score, data = data)

summary(MR_SBP_BMI,diagnostics=T)

MR_SBP_ALC<-ivreg(sbp0 ~  alc0|alc_score, data = data)

summary(MR_SBP_ALC,diagnostics=T)

MR_SBP_SMK<-ivreg(sbp0 ~  s_amount0|smk_score, data = data)

summary(MR_SBP_SMK,diagnostics=T)

MR_SBP_VACT<-ivreg(sbp0 ~  vact0|vact_score, data = data)

summary(MR_SBP_VACT,diagnostics=T)

MR_SBP_T2D<-ivreg(sbp0 ~  t2d0|t2d_score, data = data)

summary(MR_SBP_T2D,diagnostics=T)


###########################
# Outcome ALC MR Analyses #
###########################

MR_ALC_BMI<-ivreg(alc0 ~  bmi0|bmi_score, data = data)

summary(MR_ALC_BMI,diagnostics=T)

MR_ALC_SBP<-ivreg(alc0 ~  sbp0|sbp_score, data = data)

summary(MR_ALC_SBP,diagnostics=T)

MR_ALC_SMK<-ivreg(alc0 ~  s_amount0|smk_score, data = data)

summary(MR_ALC_SMK,diagnostics=T)

MR_ALC_VACT<-ivreg(alc0 ~  vact0|vact_score, data = data)

summary(MR_ALC_VACT,diagnostics=T)

MR_ALC_T2D<-ivreg(alc0 ~  t2d0|t2d_score, data = data)

summary(MR_ALC_T2D,diagnostics=T)



###########################
# Outcome SMK MR Analyses #
###########################

MR_SMK_BMI<-ivreg(s_amount0 ~  bmi0|bmi_score, data = data)

summary(MR_SMK_BMI,diagnostics=T)

MR_SMK_SBP<-ivreg(s_amount0 ~  sbp0|sbp_score, data = data)

summary(MR_SMK_SBP,diagnostics=T)

MR_SMK_ALC<-ivreg(s_amount0 ~  alc0|alc_score, data = data)

summary(MR_SMK_ALC,diagnostics=T)

MR_SMK_VACT<-ivreg(s_amount0 ~  vact0|vact_score, data = data)

summary(MR_SMK_VACT,diagnostics=T)

MR_SMK_T2D<-ivreg(s_amount0 ~  t2d0|t2d_score, data = data)

summary(MR_SMK_T2D,diagnostics=T)


############################
# Outcome VACT MR Analyses #
############################

MR_VACT_BMI<-ivreg(vact0 ~  bmi0|bmi_score, data = data)

summary(MR_VACT_BMI,diagnostics=T)

MR_VACT_SBP<-ivreg(vact0 ~  sbp0|sbp_score, data = data)

summary(MR_VACT_SBP,diagnostics=T)

MR_VACT_ALC<-ivreg(vact0 ~  alc0|alc_score, data = data)

summary(MR_VACT_ALC,diagnostics=T)

MR_VACT_SMK<-ivreg(vact0 ~  s_amount0|smk_score, data = data)

summary(MR_VACT_SMK,diagnostics=T)

MR_VACT_T2D<-ivreg(vact0 ~  t2d0|t2d_score, data = data)

summary(MR_VACT_T2D,diagnostics=T)



###########################
# Outcome T2D MR Analyses #
###########################

MR_T2D_BMI<-ivreg(t2d0 ~  bmi0|bmi_score, data = data)

summary(MR_T2D_BMI,diagnostics=T)

MR_T2D_SBP<-ivreg(t2d0 ~  sbp0|sbp_score, data = data)

summary(MR_T2D_SBP,diagnostics=T)

MR_T2D_ALC<-ivreg(t2d0 ~  alc0|alc_score, data = data)

summary(MR_T2D_ALC,diagnostics=T)

MR_T2D_SMK<-ivreg(t2d0 ~  s_amount0|smk_score, data = data)

summary(MR_T2D_SMK,diagnostics=T)

MR_T2D_VACT<-ivreg(t2d0 ~  vact0|vact_score, data = data)

summary(MR_T2D_VACT,diagnostics=T)


##############################
# Outcome Stroke MR Analyses #
##############################

MR_STK_BMI<-ivreg(stroke0 ~  bmi0|bmi_score, data = data)

summary(MR_STK_BMI,diagnostics=T)

MR_STK_SBP<-ivreg(stroke0 ~  sbp0|sbp_score, data = data)

summary(MR_STK_SBP,diagnostics=T)

MR_STK_ALC<-ivreg(stroke0 ~  alc0|alc_score, data = data)

summary(MR_STK_ALC,diagnostics=T)

MR_STK_SMK<-ivreg(stroke0 ~  s_amount0|smk_score, data = data)

summary(MR_STK_SMK,diagnostics=T)

MR_STK_VACT<-ivreg(stroke0 ~  vact0|vact_score, data = data)

summary(MR_STK_VACT,diagnostics=T)

MR_STK_T2D<-ivreg(stroke0 ~  t2d0|t2d_score, data = data)

summary(MR_STK_T2D,diagnostics=T)


##############################
# Multivariable MR Analyses  #
##############################

MVMR_BMI<-ivreg(bmi0 ~  sbp0+alc0+s_amount0+vact0+t2d0|
                  sbp_score+alc_score+smk_score+vact_score+t2d_score,data = data)

summary(MVMR_BMI,diagnostics=T)

MVMR_SBP<-ivreg(sbp0 ~  bmi0+alc0+s_amount0+vact0+t2d0|
                  bmi_score+alc_score+smk_score+vact_score+t2d_score,data = data)

summary(MVMR_SBP,diagnostics=T)

MVMR_ALC<-ivreg(alc0 ~  bmi0+sbp0+s_amount0+vact0+t2d0|
                  bmi_score+sbp_score+smk_score+vact_score+t2d_score,data = data)

summary(MVMR_ALC,diagnostics=T)

MVMR_SMK<-ivreg(s_amount0 ~  bmi0+sbp0+alc0+vact0+t2d0|
                  bmi_score+sbp_score+alc_score+vact_score+t2d_score,data = data)

summary(MVMR_SMK,diagnostics=T)

MVMR_VACT<-ivreg(vact0 ~  bmi0+sbp0+alc0+s_amount0+t2d0|
                  bmi_score+sbp_score+alc_score+smk_score+t2d_score,data = data)

summary(MVMR_VACT,diagnostics=T)

MVMR_T2D<-ivreg(t2d0 ~  bmi0+sbp0+alc0+s_amount0+vact0|
                   bmi_score+sbp_score+alc_score+smk_score+vact_score,data = data)

summary(MVMR_T2D,diagnostics=T)



summary(tsps(stroke0 ~  bmi0|bmi_score, data = data))















