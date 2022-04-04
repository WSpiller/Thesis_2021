#Load libraries
library(RadialMR)
library(TwoSampleMR)

#Smoking

exposure_dat <- extract_instruments(c('ieu-b-142'))
exposure_dat <- clump_data(exposure_dat,clump_r2 = 0.01)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ukb-b-6358',
                                                        'ebi-a-GCST006910',
                                                        'ebi-a-GCST006909',
                                                        'ebi-a-GCST006908',
                                                        'ebi-a-GCST006907'),
                                    proxies = 0, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)
dat <- dat[dat$mr_keep == TRUE,]
mr_results <- mr(dat)

dat_temp<-dat[dat$id.outcome == "ukb-b-6358",]

RdatSMK1<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSMK1<-ivw_radial(RdatSMK1,0.05,3)

radEGGERSMK1<-egger_radial(RdatSMK1,0.05,1)

if(radIVWSMK1$outliers == "No significant outliers"){
  
  SMK_radplot1<-plot_radial(radIVWSMK1,F,F,F)
  
}else{
  
  SMK_radplot1<-plot_radial(c(radIVWSMK1,radEGGERSMK1),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006910",]

RdatSMK2<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSMK2<-ivw_radial(RdatSMK2,0.05,3)

radEGGERSMK2<-egger_radial(RdatSMK2,0.05,1)

if(radIVWSMK2$outliers == "No significant outliers"){
  
  SMK_radplot2<-plot_radial(radIVWSMK2,F,F,F)
  
}else{
  
  SMK_radplot2<-plot_radial(c(radIVWSMK2,radEGGERSMK2),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006909",]

RdatSMK3<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSMK3<-ivw_radial(RdatSMK3,0.05,3)

radEGGERSMK3<-egger_radial(RdatSMK3,0.05,1)

if(radIVWSMK3$outliers == "No significant outliers"){
  
  SMK_radplot3<-plot_radial(radIVWSMK3,F,F,F)
  
}else{
  
  SMK_radplot3<-plot_radial(c(radIVWSMK3,radEGGERSMK3),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006908",]

RdatSMK4<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSMK4<-ivw_radial(RdatSMK4,0.05,3)

radEGGERSMK4<-egger_radial(RdatSMK4,0.05,1)

if(radIVWSMK4$outliers == "No significant outliers"){
  
  SMK_radplot4<-plot_radial(radIVWSMK4,F,F,F)
  
}else{
  
  SMK_radplot4<-plot_radial(c(radIVWSMK4,radEGGERSMK4),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006907",]

RdatSMK5<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSMK5<-ivw_radial(RdatSMK5,0.05,3)

radEGGERSMK5<-egger_radial(RdatSMK5,0.05,1)

if(radIVWSMK5$outliers == "No significant outliers"){
  
  SMK_radplot5<-plot_radial(radIVWSMK5,F,F,F)
  
}else{
  
  SMK_radplot5<-plot_radial(c(radIVWSMK5,radEGGERSMK5),F,F,F)
  
}


#######


#Alcohol

exposure_dat <- extract_instruments(c('ieu-b-73'))
exposure_dat <- clump_data(exposure_dat,clump_r2 = 0.01)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ukb-b-6358',
                                                        'ebi-a-GCST006910',
                                                        'ebi-a-GCST006909',
                                                        'ebi-a-GCST006908',
                                                        'ebi-a-GCST006907'),
                                    proxies = 0, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)
dat <- dat[dat$mr_keep == TRUE,]
mr_results <- mr(dat)

dat_temp<-dat[dat$id.outcome == "ukb-b-6358",]

RdatALC1<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWALC1<-ivw_radial(RdatALC1,0.05,3)

radEGGERALC1<-egger_radial(RdatALC1,0.05,1)

if(radIVWALC1$outliers == "No significant outliers"){
  
  ALC_radplot1<-plot_radial(radIVWALC1,F,F,F)
  
}else{
  
  ALC_radplot1<-plot_radial(c(radIVWALC1,radEGGERALC1),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006910",]

RdatALC2<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWALC2<-ivw_radial(RdatALC2,0.05,3)

radEGGERALC2<-egger_radial(RdatALC2,0.05,1)

if(radIVWALC2$outliers == "No significant outliers"){
  
  ALC_radplot2<-plot_radial(radIVWALC2,F,F,F)
  
}else{
  
  ALC_radplot2<-plot_radial(c(radIVWALC2,radEGGERALC2),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006909",]

RdatALC3<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWALC3<-ivw_radial(RdatALC3,0.05,3)

radEGGERALC3<-egger_radial(RdatALC3,0.05,1)

if(radIVWALC3$outliers == "No significant outliers"){
  
  ALC_radplot3<-plot_radial(radIVWALC3,F,F,F)
  
}else{
  
  ALC_radplot3<-plot_radial(c(radIVWALC3,radEGGERALC3),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006908",]

RdatALC4<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWALC4<-ivw_radial(RdatALC4,0.05,3)

radEGGERALC4<-egger_radial(RdatALC4,0.05,1)

if(radIVWALC4$outliers == "No significant outliers"){
  
  ALC_radplot4<-plot_radial(radIVWALC4,F,F,F)
  
}else{
  
  ALC_radplot4<-plot_radial(c(radIVWALC4,radEGGERALC4),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006907",]

RdatALC5<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWALC5<-ivw_radial(RdatALC5,0.05,3)

radEGGERALC5<-egger_radial(RdatALC5,0.05,1)

if(radIVWALC5$outliers == "No significant outliers"){
  
  ALC_radplot5<-plot_radial(radIVWALC5,F,F,F)
  
}else{
  
  ALC_radplot5<-plot_radial(c(radIVWALC5,radEGGERALC5),F,F,F)
  
}



#Physical activity

exposure_dat <- extract_instruments(c('ukb-b-151'))
exposure_dat <- clump_data(exposure_dat,clump_r2 = 0.01)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ukb-b-6358',
                                                        'ebi-a-GCST006910',
                                                        'ebi-a-GCST006909',
                                                        'ebi-a-GCST006908',
                                                        'ebi-a-GCST006907'),
                                    proxies = 0, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)
dat <- dat[dat$mr_keep == TRUE,]
mr_results <- mr(dat)

dat_temp<-dat[dat$id.outcome == "ukb-b-6358",]

RdatVACT1<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                         dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWVACT1<-ivw_radial(RdatVACT1,0.05,3)

radEGGERVACT1<-egger_radial(RdatVACT1,0.05,1)

if(radIVWVACT1$outliers == "No significant outliers"){
  
  VACT_radplot1<-plot_radial(radIVWVACT1,F,F,F)
  
}else{
  
  VACT_radplot1<-plot_radial(c(radIVWVACT1,radEGGERVACT1),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006910",]

RdatVACT2<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                         dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWVACT2<-ivw_radial(RdatVACT2,0.05,3)

radEGGERVACT2<-egger_radial(RdatVACT2,0.05,1)

if(radIVWVACT2$outliers == "No significant outliers"){
  
  VACT_radplot2<-plot_radial(radIVWVACT2,F,F,F)
  
}else{
  
  VACT_radplot2<-plot_radial(c(radIVWVACT2,radEGGERVACT2),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006909",]

RdatVACT3<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                         dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWVACT3<-ivw_radial(RdatVACT3,0.05,3)

radEGGERVACT3<-egger_radial(RdatVACT3,0.05,1)

if(radIVWVACT3$outliers == "No significant outliers"){
  
  VACT_radplot3<-plot_radial(radIVWVACT3,F,F,F)
  
}else{
  
  VACT_radplot3<-plot_radial(c(radIVWVACT3,radEGGERVACT3),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006908",]

RdatVACT4<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                         dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWVACT4<-ivw_radial(RdatVACT4,0.05,3)

radEGGERVACT4<-egger_radial(RdatVACT4,0.05,1)

if(radIVWVACT4$outliers == "No significant outliers"){
  
  VACT_radplot4<-plot_radial(radIVWVACT4,F,F,F)
  
}else{
  
  VACT_radplot4<-plot_radial(c(radIVWVACT4,radEGGERVACT4),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006907",]

RdatVACT5<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                         dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWVACT5<-ivw_radial(RdatVACT5,0.05,3)

radEGGERVACT5<-egger_radial(RdatVACT5,0.05,1)

if(radIVWVACT5$outliers == "No significant outliers"){
  
  VACT_radplot5<-plot_radial(radIVWVACT5,F,F,F)
  
}else{
  
  VACT_radplot5<-plot_radial(c(radIVWVACT5,radEGGERVACT5),F,F,F)
  
}


#BMI

exposure_dat <- extract_instruments(c('ieu-a-835'))
exposure_dat <- clump_data(exposure_dat,clump_r2 = 0.01)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ukb-b-6358',
                                                        'ebi-a-GCST006910',
                                                        'ebi-a-GCST006909',
                                                        'ebi-a-GCST006908',
                                                        'ebi-a-GCST006907'),
                                    proxies = 0, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)
dat <- dat[dat$mr_keep == TRUE,]
mr_results <- mr(dat)

dat_temp<-dat[dat$id.outcome == "ukb-b-6358",]

RdatBMI1<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWBMI1<-ivw_radial(RdatBMI1,0.05,3)

radEGGERBMI1<-egger_radial(RdatBMI1,0.05,1)

if(radIVWBMI1$outliers == "No significant outliers"){
  
  BMI_radplot1<-plot_radial(radIVWBMI1,F,F,F)
  
}else{
  
  BMI_radplot1<-plot_radial(c(radIVWBMI1,radEGGERBMI1),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006910",]

RdatBMI2<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWBMI2<-ivw_radial(RdatBMI2,0.05,3)

radEGGERBMI2<-egger_radial(RdatBMI2,0.05,1)

if(radIVWBMI2$outliers == "No significant outliers"){
  
  BMI_radplot2<-plot_radial(radIVWBMI2,F,F,F)
  
}else{
  
  BMI_radplot2<-plot_radial(c(radIVWBMI2,radEGGERBMI2),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006909",]

RdatBMI3<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWBMI3<-ivw_radial(RdatBMI3,0.05,3)

radEGGERBMI3<-egger_radial(RdatBMI3,0.05,1)

if(radIVWBMI3$outliers == "No significant outliers"){
  
  BMI_radplot3<-plot_radial(radIVWBMI3,F,F,F)
  
}else{
  
  BMI_radplot3<-plot_radial(c(radIVWBMI3,radEGGERBMI3),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006908",]

RdatBMI4<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWBMI4<-ivw_radial(RdatBMI4,0.05,3)

radEGGERBMI4<-egger_radial(RdatBMI4,0.05,1)

if(radIVWBMI4$outliers == "No significant outliers"){
  
  BMI_radplot4<-plot_radial(radIVWBMI4,F,F,F)
  
}else{
  
  BMI_radplot4<-plot_radial(c(radIVWBMI4,radEGGERBMI4),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006907",]

RdatBMI5<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWBMI5<-ivw_radial(RdatBMI5,0.05,3)

radEGGERBMI5<-egger_radial(RdatBMI5,0.05,1)

if(radIVWBMI5$outliers == "No significant outliers"){
  
  BMI_radplot5<-plot_radial(radIVWBMI5,F,F,F)
  
}else{
  
  BMI_radplot5<-plot_radial(c(radIVWBMI5,radEGGERBMI5),F,F,F)
  
}






#SBP

exposure_dat <- read_exposure_data(
  filename = 'BaseSBP.txt',
  sep = ' ',
  snp_col = 'SNP',
  beta_col = 'beta',
  se_col = 'se',
  effect_allele_col = 'effect_allele',
  phenotype_col = 'Phenotype',
  units_col = 'units',
  other_allele_col = 'other_allele',
  eaf_col = 'eaf',
  samplesize_col = 'samplesize',
  ncase_col = 'ncase',
  ncontrol_col = 'ncontrol',
  gene_col = 'gene',
  pval_col = 'pval'
)
exposure_dat <- clump_data(exposure_dat,clump_r2 = 0.01)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ukb-b-6358','ebi-a-GCST006910','ebi-a-GCST006909','ebi-a-GCST006908','ebi-a-GCST006907'), proxies = 0, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)
mr_results <- mr(dat)

dat_temp<-dat[dat$id.outcome == "ukb-b-6358",]

RdatSBP1<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSBP1<-ivw_radial(RdatSBP1,0.05,3)

radEGGERSBP1<-egger_radial(RdatSBP1,0.05,1)

if(radIVWSBP1$outliers == "No significant outliers"){
  
  SBP_radplot1<-plot_radial(radIVWSBP1,F,F,F)
  
}else{
  
  SBP_radplot1<-plot_radial(c(radIVWSBP1,radEGGERSBP1),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006910",]

RdatSBP2<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSBP2<-ivw_radial(RdatSBP2,0.05,3)

radEGGERSBP2<-egger_radial(RdatSBP2,0.05,1)

if(radIVWSBP2$outliers == "No significant outliers"){
  
  SBP_radplot2<-plot_radial(radIVWSBP2,F,F,F)
  
}else{
  
  SBP_radplot2<-plot_radial(c(radIVWSBP2,radEGGERSBP2),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006909",]

RdatSBP3<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSBP3<-ivw_radial(RdatSBP3,0.05,3)

radEGGERSBP3<-egger_radial(RdatSBP3,0.05,1)

if(radIVWSBP3$outliers == "No significant outliers"){
  
  SBP_radplot3<-plot_radial(radIVWSBP3,F,F,F)
  
}else{
  
  SBP_radplot3<-plot_radial(c(radIVWSBP3,radEGGERSBP3),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006908",]

RdatSBP4<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSBP4<-ivw_radial(RdatSBP4,0.05,3)

radEGGERSBP4<-egger_radial(RdatSBP4,0.05,1)

if(radIVWSBP4$outliers == "No significant outliers"){
  
  SBP_radplot4<-plot_radial(radIVWSBP4,F,F,F)
  
}else{
  
  SBP_radplot4<-plot_radial(c(radIVWSBP4,radEGGERSBP4),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006907",]

RdatSBP5<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWSBP5<-ivw_radial(RdatSBP5,0.05,3)

radEGGERSBP5<-egger_radial(RdatSBP5,0.05,1)

if(radIVWSBP5$outliers == "No significant outliers"){
  
  SBP_radplot5<-plot_radial(radIVWSBP5,F,F,F)
  
}else{
  
  SBP_radplot5<-plot_radial(c(radIVWSBP5,radEGGERSBP5),F,F,F)
  
}

#T2D

exposure_dat <- extract_instruments(c('ieu-a-25'))
exposure_dat <- clump_data(exposure_dat,clump_r2 = 0.01)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ukb-b-6358',
                                                        'ebi-a-GCST006910',
                                                        'ebi-a-GCST006909',
                                                        'ebi-a-GCST006908',
                                                        'ebi-a-GCST006907'),
                                    proxies = 0, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)
dat <- dat[dat$mr_keep == TRUE,]
mr_results <- mr(dat)


dat_temp<-dat[dat$id.outcome == "ukb-b-6358",]

RdatT2D1<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWT2D1<-ivw_radial(RdatT2D1,0.05,3)

radEGGERT2D1<-egger_radial(RdatT2D1,0.05,1)

if(radIVWT2D1$outliers == "No significant outliers"){
  
  T2D_radplot1<-plot_radial(radIVWT2D1,F,F,F)
  
}else{
  
  T2D_radplot1<-plot_radial(c(radIVWT2D1,radEGGERT2D1),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006910",]

RdatT2D2<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWT2D2<-ivw_radial(RdatT2D2,0.05,3)

radEGGERT2D2<-egger_radial(RdatT2D2,0.05,1)

if(radIVWT2D2$outliers == "No significant outliers"){
  
  T2D_radplot2<-plot_radial(radIVWT2D2,F,F,F)
  
}else{
  
  T2D_radplot2<-plot_radial(c(radIVWT2D2,radEGGERT2D2),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006909",]

RdatT2D3<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWT2D3<-ivw_radial(RdatT2D3,0.05,3)

radEGGERT2D3<-egger_radial(RdatT2D3,0.05,1)

if(radIVWT2D3$outliers == "No significant outliers"){
  
  T2D_radplot3<-plot_radial(radIVWT2D3,F,F,F)
  
}else{
  
  T2D_radplot3<-plot_radial(c(radIVWT2D3,radEGGERT2D3),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006908",]

RdatT2D4<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWT2D4<-ivw_radial(RdatT2D4,0.05,3)

radEGGERT2D4<-egger_radial(RdatT2D4,0.05,1)

if(radIVWT2D4$outliers == "No significant outliers"){
  
  T2D_radplot4<-plot_radial(radIVWT2D4,F,F,F)
  
}else{
  
  T2D_radplot4<-plot_radial(c(radIVWT2D4,radEGGERT2D4),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006907",]

RdatT2D5<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWT2D5<-ivw_radial(RdatT2D5,0.05,3)

radEGGERT2D5<-egger_radial(RdatT2D5,0.05,1)

if(radIVWT2D5$outliers == "No significant outliers"){
  
  T2D_radplot5<-plot_radial(radIVWT2D5,F,F,F)
  
}else{
  
  T2D_radplot5<-plot_radial(c(radIVWT2D5,radEGGERT2D5),F,F,F)
  
}

# LDL

exposure_dat <- extract_instruments(c('ieu-a-300'))
exposure_dat <- clump_data(exposure_dat,clump_r2 = 0.01)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ukb-b-6358',
                                                        'ebi-a-GCST006910',
                                                        'ebi-a-GCST006909',
                                                        'ebi-a-GCST006908',
                                                        'ebi-a-GCST006907'),
                                    proxies = 0, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)
dat <- dat[dat$mr_keep == TRUE,]
mr_results <- mr(dat)

dat_temp<-dat[dat$id.outcome == "ukb-b-6358",]

RdatLDL1<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWLDL1<-ivw_radial(RdatLDL1,0.05/nrow(RdatLDL1),3)

radEGGERLDL1<-egger_radial(RdatLDL1,0.05,1)

if(radIVWLDL1$outliers == "No significant outliers"){
  
  LDL_radplot1<-plot_radial(radIVWLDL1,F,F,F)
  
}else{
  
  LDL_radplot1<-plot_radial(c(radIVWLDL1,radEGGERLDL1),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006910",]

RdatLDL2<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWLDL2<-ivw_radial(RdatLDL2,0.05,3)

radEGGERLDL2<-egger_radial(RdatLDL2,0.05,1)

if(radIVWLDL2$outliers == "No significant outliers"){
  
  LDL_radplot2<-plot_radial(radIVWLDL2,F,F,F)
  
}else{
  
  LDL_radplot2<-plot_radial(c(radIVWLDL2,radEGGERLDL2),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006909",]

RdatLDL3<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWLDL3<-ivw_radial(RdatLDL3,0.05,3)

radEGGERLDL3<-egger_radial(RdatLDL3,0.05,1)

if(radIVWLDL3$outliers == "No significant outliers"){
  
  LDL_radplot3<-plot_radial(radIVWLDL3,F,F,F)
  
}else{
  
  LDL_radplot3<-plot_radial(c(radIVWLDL3,radEGGERLDL3),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006908",]

RdatLDL4<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWLDL4<-ivw_radial(RdatLDL4,0.05,3)

radEGGERLDL4<-egger_radial(RdatLDL4,0.05,1)

if(radIVWLDL4$outliers == "No significant outliers"){
  
  LDL_radplot4<-plot_radial(radIVWLDL4,F,F,F)
  
}else{
  
  LDL_radplot4<-plot_radial(c(radIVWLDL4,radEGGERLDL4),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006907",]

RdatLDL5<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWLDL5<-ivw_radial(RdatLDL5,0.05,3)

radEGGERLDL5<-egger_radial(RdatLDL5,0.05,1)

if(radIVWLDL5$outliers == "No significant outliers"){
  
  LDL_radplot5<-plot_radial(radIVWLDL5,F,F,F)
  
}else{
  
  LDL_radplot5<-plot_radial(c(radIVWLDL5,radEGGERLDL5),F,F,F)
  
}

# HDL

exposure_dat <- extract_instruments(c('ieu-a-299'))
exposure_dat <- clump_data(exposure_dat,clump_r2 = 0.01)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ukb-b-6358',
                                                        'ebi-a-GCST006910',
                                                        'ebi-a-GCST006909',
                                                        'ebi-a-GCST006908',
                                                        'ebi-a-GCST006907'),
                                    proxies = 0, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)
dat <- dat[dat$mr_keep == TRUE,]
mr_results <- mr(dat)

dat_temp<-dat[dat$id.outcome == "ukb-b-6358",]

RdatHDL1<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWHDL1<-ivw_radial(RdatHDL1,0.05,3)

radEGGERHDL1<-egger_radial(RdatHDL1,0.05,1)

if(radIVWHDL1$outliers == "No significant outliers"){
  
  HDL_radplot1<-plot_radial(radIVWHDL1,F,F,F)
  
}else{
  
  HDL_radplot1<-plot_radial(c(radIVWHDL1,radEGGERHDL1),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006910",]

RdatHDL2<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWHDL2<-ivw_radial(RdatHDL2,0.05,3)

radEGGERHDL2<-egger_radial(RdatHDL2,0.05,1)

if(radIVWHDL2$outliers == "No significant outliers"){
  
  HDL_radplot2<-plot_radial(radIVWHDL2,F,F,F)
  
}else{
  
  HDL_radplot2<-plot_radial(c(radIVWHDL2,radEGGERHDL2),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006909",]

RdatHDL3<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWHDL3<-ivw_radial(RdatHDL3,0.05,3)

radEGGERHDL3<-egger_radial(RdatHDL3,0.05,1)

if(radIVWHDL3$outliers == "No significant outliers"){
  
  HDL_radplot3<-plot_radial(radIVWHDL3,F,F,F)
  
}else{
  
  HDL_radplot3<-plot_radial(c(radIVWHDL3,radEGGERHDL3),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006908",]

RdatHDL4<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWHDL4<-ivw_radial(RdatHDL4,0.05,3)

radEGGERHDL4<-egger_radial(RdatHDL4,0.05,1)

if(radIVWHDL4$outliers == "No significant outliers"){
  
  HDL_radplot4<-plot_radial(radIVWHDL4,F,F,F)
  
}else{
  
  HDL_radplot4<-plot_radial(c(radIVWHDL4,radEGGERHDL4),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006907",]

RdatHDL5<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWHDL5<-ivw_radial(RdatHDL5,0.05,3)

radEGGERHDL5<-egger_radial(RdatHDL5,0.05,1)

if(radIVWHDL5$outliers == "No significant outliers"){
  
  HDL_radplot5<-plot_radial(radIVWHDL5,F,F,F)
  
}else{
  
  HDL_radplot5<-plot_radial(c(radIVWHDL5,radEGGERHDL5),F,F,F)
  
}

# TG

exposure_dat <- extract_instruments(c('ieu-a-302'))
exposure_dat <- clump_data(exposure_dat,clump_r2 = 0.01)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, c('ukb-b-6358',
                                                        'ebi-a-GCST006910',
                                                        'ebi-a-GCST006909',
                                                        'ebi-a-GCST006908',
                                                        'ebi-a-GCST006907'),
                                    proxies = 0, rsq = 0.8, align_alleles = 1, palindromes = 1, maf_threshold = 0.3)
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)
dat <- dat[dat$mr_keep == TRUE,]
mr_results <- mr(dat)

dat_temp<-dat[dat$id.outcome == "ukb-b-6358",]

RdatTG1<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWTG1<-ivw_radial(RdatTG1,0.05,3)

radEGGERTG1<-egger_radial(RdatTG1,0.05,1)

if(radIVWTG1$outliers == "No significant outliers"){
  
  TG_radplot1<-plot_radial(radIVWTG1,F,F,F)
  
}else{
  
  TG_radplot1<-plot_radial(c(radIVWTG1,radEGGERTG1),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006910",]

RdatTG2<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWTG2<-ivw_radial(RdatTG2,0.05,3)

radEGGERTG2<-egger_radial(RdatTG2,0.05,1)

if(radIVWTG2$outliers == "No significant outliers"){
  
  TG_radplot2<-plot_radial(radIVWTG2,F,F,F)
  
}else{
  
  TG_radplot2<-plot_radial(c(radIVWTG2,radEGGERTG2),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006909",]

RdatTG3<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWTG3<-ivw_radial(RdatTG3,0.05,3)

radEGGERTG3<-egger_radial(RdatTG3,0.05,1)

if(radIVWTG3$outliers == "No significant outliers"){
  
  TG_radplot3<-plot_radial(radIVWTG3,F,F,F)
  
}else{
  
  TG_radplot3<-plot_radial(c(radIVWTG3,radEGGERTG3),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006908",]

RdatTG4<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWTG4<-ivw_radial(RdatTG4,0.05,3)

radEGGERTG4<-egger_radial(RdatTG4,0.05,1)

if(radIVWTG4$outliers == "No significant outliers"){
  
  TG_radplot4<-plot_radial(radIVWTG4,F,F,F)
  
}else{
  
  TG_radplot4<-plot_radial(c(radIVWTG4,radEGGERTG4),F,F,F)
  
}

###

dat_temp<-dat[dat$id.outcome == "ebi-a-GCST006907",]

RdatTG5<-format_radial(dat_temp$beta.exposure, dat_temp$beta.outcome,
                        dat_temp$se.exposure, dat_temp$se.outcome, dat_temp$SNP)

radIVWTG5<-ivw_radial(RdatTG5,0.05,3)

radEGGERTG5<-egger_radial(RdatTG5,0.05,1)

if(radIVWTG5$outliers == "No significant outliers"){
  
  TG_radplot5<-plot_radial(radIVWTG5,F,F,F)
  
}else{
  
  TG_radplot5<-plot_radial(c(radIVWTG5,radEGGERTG5),F,F,F)
  
}


### RESULTS ###

#Self-report stroke

radIVWSMK1$confint
radEGGERSMK1
ivw_radial(RdatVACT1)$meanF
SMK_radplot1

radIVWALC1$meanF
radEGGERALC1
ALC_radplot1

radIVWSBP1$coef
radEGGERSBP1
SBP_radplot1

radIVWBMI1$confint
radEGGERBMI1
BMI_radplot1

radIVWVACT1$coef
radEGGERVACT1
VACT_radplot1

radIVWT2D1$coef
radEGGERT2D1
T2D_radplot1

radIVWLDL1$coef
radEGGERLDL1
LDL_radplot1

radIVWHDL1
radEGGERHDL1
HDL_radplot1

radIVWTG1
radEGGERTG1
TG_radplot1


#Plot

RIVW_res1<-data.frame(c(radIVWALC1$coef[1,1],radIVWBMI1$coef[1,1],radIVWSBP1$coef[1,1],
                       radIVWSMK1$coef[1,1],radIVWVACT1$coef[1,1],radIVWT2D1$coef[1,1],
                       radIVWLDL1$coef[1,1],radIVWHDL1$coef[1,1],radIVWTG1$coef[1,1]),
                     c(radIVWALC1$confint[1],radIVWBMI1$confint[1],radIVWSBP1$confint[1],
                       radIVWSMK1$confint[1],radIVWVACT1$confint[1],radIVWT2D1$confint[1],
                       radIVWLDL1$confint[1],radIVWHDL1$confint[1],radIVWTG1$confint[1]),
                     c(radIVWALC1$confint[2],radIVWBMI1$confint[2],radIVWSBP1$confint[2],
                       radIVWSMK1$confint[2],radIVWVACT1$confint[2],radIVWT2D1$confint[2],
                       radIVWLDL1$confint[2],radIVWHDL1$confint[2],radIVWTG1$confint[2]),
                     c("Alcohol consumption",
                       "BMI",
                       "SBP",
                       "Smoking amount",
                       "Vigorous Physical Activity",
                       "Type-II Diabetes- Positive",
                       "LDL",
                       "HDL",
                       "Triglycerides"
                     ),
                     1:9)


names(RIVW_res1)<-c("effect","lci_95","uci_95","exposure","index")

RIVW_res1$index<-RIVW_res1$index * -1

uniplot6<-ggplot(RIVW_res1)+geom_point(aes(x=index, y=effect))+
  coord_flip()+geom_errorbar(aes(ymin=lci_95,ymax=uci_95,x=index),width=0,orientation = "x")+
  scale_y_continuous(limits = c(min(RIVW_res1$lci_95),max(RIVW_res1$uci_95)),name = "Effect (95% CI)")+geom_hline(yintercept=0,linetype = "dashed")+theme_bw()+
  scale_x_continuous(limits = c(-9,-1),breaks=RIVW_res1$index,labels= RIVW_res1$exposure,name = "")+
  scale_color_discrete(name = "Exposure")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

uniplot6


png("UMR_Forest1.png",width=5.33,height=4,units="in",res=500)
uniplot6
dev.off()

png("SBP_radial5.png",width=7,height=5.25,units="in",res=500)
SBP_radplot5
dev.off()









#Ischemic stroke (cardioembolic)

exp(radIVWSMK2$coef[1,1])
exp(radIVWSMK2$confint)
ivw_radial(RdatSMK2,0.05/nrow(RdatSMK2),3)$meanF
radEGGERSMK2
SMK_radplot2

radIVWALC2
radEGGERALC2
exp(radIVWALC2$coef[1,1])
exp(radIVWALC2$confint)
ivw_radial(RdatALC2,0.05/nrow(RdatALC2),3)$meanF
ALC_radplot2

radIVWSBP2
radEGGERSBP2
exp(radIVWSBP2$coef[1,1])
exp(radIVWSBP2$confint)
ivw_radial(RdatSBP2,0.05/nrow(RdatSBP2),3)$meanF
SBP_radplot2

radIVWBMI2
radEGGERBMI2
exp(radIVWBMI2$coef[1,1])
exp(radIVWBMI2$confint)
ivw_radial(RdatBMI2,0.05/nrow(RdatBMI2),3)$meanF
BMI_radplot2

radIVWVACT2
radEGGERVACT2
ivw_radial(RdatVACT2,0.05/nrow(RdatVACT2),3)$meanF
VACT_radplot2

radIVWT2D2
radEGGERT2D2
ivw_radial(RdatT2D2,0.05/nrow(RdatT2D2),3)$meanF
T2D_radplot2

radIVWLDL2
radEGGERLDL2
ivw_radial(RdatLDL2,0.05/nrow(RdatLDL2),3)$meanF
LDL_radplot2

radIVWHDL2
radEGGERHDL2
ivw_radial(RdatHDL2,0.05/nrow(RdatHDL2),3)$meanF
exp(radIVWHDL2$coef[1,1])
exp(radIVWHDL2$confint)
HDL_radplot2

radIVWTG2
radEGGERTG2
ivw_radial(RdatTG2,0.05/nrow(RdatTG2),3)$meanF
exp(radIVWTG2$coef[1,1])
exp(radIVWTG2$confint)
TG_radplot2

#Ischemic stroke (small-vessel)

radIVWSMK3
radEGGERSMK3
ivw_radial(RdatSMK3,0.05/nrow(RdatSMK3),3)$meanF
exp(radIVWSMK3$coef[1,1])
exp(radIVWSMK3$confint)
SMK_radplot3

radIVWALC3
radEGGERALC3
ivw_radial(RdatALC3,0.05/nrow(RdatALC3),3)$meanF
exp(radIVWALC3$coef[1,1])
exp(radIVWALC3$confint)
ALC_radplot3

radIVWSBP3
radEGGERSBP3
ivw_radial(RdatSBP3,0.05/nrow(RdatSBP3),3)$meanF
exp(radIVWSBP3$coef[1,1])
exp(radIVWSBP3$confint)
SBP_radplot3

radIVWBMI3
radEGGERBMI3
ivw_radial(RdatBMI3,0.05/nrow(RdatBMI3),3)$meanF
exp(radIVWBMI3$coef[1,1])
exp(radIVWBMI3$confint)
BMI_radplot3

radIVWVACT3
radEGGERVACT3
ivw_radial(RdatVACT3,0.05/nrow(RdatVACT3),3)$meanF
exp(radIVWVACT3$coef[1,1])
exp(radIVWVACT3$confint)
VACT_radplot3

radIVWT2D3
radEGGERT2D3
ivw_radial(RdatT2D3,0.05/nrow(RdatT2D3),3)$meanF
exp(radIVWT2D3$coef[1,1])
exp(radIVWT2D3$confint)
T2D_radplot3

radIVWLDL3
radEGGERLDL3
ivw_radial(RdatLDL3,0.05/nrow(RdatLDL3),3)$meanF
exp(radIVWLDL3$coef[1,1])
exp(radIVWLDL3$confint)
LDL_radplot3

radIVWHDL3
radEGGERHDL3
ivw_radial(RdatHDL3,0.05/nrow(RdatHDL3),3)$meanF
exp(radIVWHDL3$coef[1,1])
exp(radIVWHDL3$confint)
HDL_radplot3

radIVWTG3
radEGGERTG3
ivw_radial(RdatTG3,0.05/nrow(RdatTG3),3)$meanF
exp(radIVWTG3$coef[1,1])
exp(radIVWTG3$confint)
TG_radplot3

#Ischemic stroke

radIVWSMK4
radEGGERSMK4
ivw_radial(RdatSMK4,0.05/nrow(RdatSMK4),3)$meanF
exp(radIVWSMK4$coef[1,1])
exp(radIVWSMK4$confint)
SMK_radplot4

radIVWALC4
radEGGERALC4
ivw_radial(RdatALC4,0.05/nrow(RdatALC4),3)$meanF
exp(radIVWALC4$coef[1,1])
exp(radIVWALC4$confint)
ALC_radplot4

radIVWSBP4
radEGGERSBP4
ivw_radial(RdatSBP4,0.05/nrow(RdatSBP4),3)$meanF
exp(radIVWSBP4$coef[1,1])
exp(radIVWSBP4$confint)
SBP_radplot4

radIVWBMI4
radEGGERBMI4
ivw_radial(RdatBMI4,0.05/nrow(RdatBMI4),3)$meanF
exp(radIVWBMI4$coef[1,1])
exp(radIVWBMI4$confint)
BMI_radplot4

radIVWVACT4
radEGGERVACT4
ivw_radial(RdatVACT4,0.05/nrow(RdatVACT4),3)$meanF
exp(radIVWVACT4$coef[1,1])
exp(radIVWVACT4$confint)
VACT_radplot4

radIVWT2D4
radEGGERT2D4
ivw_radial(RdatT2D4,0.05/nrow(RdatT2D4),3)$meanF
exp(radIVWT2D4$coef[1,1])
exp(radIVWT2D4$confint)
T2D_radplot4

radIVWLDL4
radEGGERLDL4
ivw_radial(RdatLDL4,0.05/nrow(RdatLDL4),3)$meanF
exp(radIVWLDL4$coef[1,1])
exp(radIVWLDL4$confint)
LDL_radplot4

radIVWHDL4
radEGGERHDL4
ivw_radial(RdatHDL4,0.05/nrow(RdatHDL4),3)$meanF
exp(radIVWHDL4$coef[1,1])
exp(radIVWHDL4$confint)
HDL_radplot4

radIVWTG4
radEGGERTG4
ivw_radial(RdatTG4,0.05/nrow(RdatTG4),3)$meanF
exp(radIVWTG4$coef[1,1])
exp(radIVWTG4$confint)
TG_radplot4

#Ischemic stroke (large artery atherosclerosis)

radIVWSMK5
ivw_radial(RdatSMK5,0.05/nrow(RdatSMK5),3)$meanF
exp(radIVWSMK5$coef[1,1])
exp(radIVWSMK5$confint)
radEGGERSMK5

SMK_radplot5

radIVWALC5
radEGGERALC5
ivw_radial(RdatALC5,0.05/nrow(RdatALC5),3)$meanF
exp(radIVWALC5$coef[1,1])
exp(radIVWALC5$confint)
ALC_radplot5

radIVWSBP5
radEGGERSBP5
ivw_radial(RdatSBP5,0.05/nrow(RdatSBP5),3)$meanF
exp(radIVWSBP5$coef[1,1])
exp(radIVWSBP5$confint)
SBP_radplot5

radIVWBMI5
radEGGERBMI5
ivw_radial(RdatBMI5,0.05/nrow(RdatBMI5),3)$meanF
exp(radIVWBMI5$coef[1,1])
exp(radIVWBMI5$confint)
BMI_radplot5

radIVWVACT5
radEGGERVACT5
ivw_radial(RdatVACT5,0.05/nrow(RdatVACT5),3)$meanF
exp(radIVWVACT5$coef[1,1])
exp(radIVWVACT5$confint)
VACT_radplot5

radIVWT2D5
radEGGERT2D5
ivw_radial(RdatT2D5,0.05/nrow(RdatT2D5),3)$meanF
exp(radIVWT2D5$coef[1,1])
exp(radIVWT2D5$confint)
T2D_radplot5

radIVWLDL5
radEGGERLDL5
ivw_radial(RdatLDL5,0.05/nrow(RdatLDL5),3)$meanF
exp(radIVWLDL5$coef[1,1])
exp(radIVWLDL5$confint)
LDL_radplot5

radIVWHDL5
radEGGERHDL5
ivw_radial(RdatHDL5,0.05/nrow(RdatHDL5),3)$meanF
exp(radIVWHDL5$coef[1,1])
exp(radIVWHDL5$confint)
HDL_radplot5

radIVWTG5
radEGGERTG5
ivw_radial(RdatTG5,0.05/nrow(RdatTG5),3)$meanF
exp(radIVWTG5$coef[1,1])
exp(radIVWTG5$confint)
TG_radplot5



#Outlier removal IS (4)
radIVWBMI4$outliers


radIVWBMI4_outs<-ivw_radial(RdatTG5,0.05/nrow(RdatTG5),3)
radEGGERTG5
ivw_radial(RdatTG5,0.05/nrow(RdatTG5),3)$meanF
exp(radIVWTG5$coef[1,1])
exp(radIVWTG5$confint)











#Plot_output

png("BMI_Rad1.png",width=8,height=6,units="in",res=300)
BMI_radplot1
dev.off()

png("SBP_Rad1.png",width=8,height=6,units="in",res=300)
SBP_radplot1
dev.off()

LDL_radplot1<-plot_radial(radIVWLDL1,F,F,F)

png("LDL_Rad1.png",width=6,height=4,units="in",res=500)
LDL_radplot1
dev.off()

png("HDL_Rad1.png",width=8,height=6,units="in",res=300)
HDL_radplot1
dev.off()



#Outliers

Outliers<-unique(c(radIVWSBP2$outliers$SNP,
            radIVWSBP3$outliers$SNP,
            radIVWSBP4$outliers$SNP,
            radIVWSBP5$outliers$SNP))


#Outanalyses

A<-ivw_radial(RdatSMK3[!RdatSMK3$SNP %in% radIVWSMK3$outliers$SNP,])
A$meanF
exp(A$coef[1,1])
exp(A$confint)

B<-egger_radial(RdatSMK3[!RdatSMK3$SNP %in% radIVWSMK3$outliers$SNP,])
B$meanF
exp(B$coef[1,1])
exp(B$confint)

A<-ivw_radial(RdatSMK3)

plot_radial(A,F,F,F)
plotly_radial(A)
SMK_radplot3



ivw_radial(RdatSMK1,0.05,3)

png("SMK_Radouts.png",width=6,height=4,units="in",res=500)
plot_radial(A,F,F,F)
dev.off()

A<-ivw_radial(RdatLDL1,0.05/nrow(RdatLDL1),3)
ivw_radial(RdatLDL1[!RdatLDL1$SNP %in% A$outliers$SNP,],
           0.05/nrow(RdatLDL1[!RdatLDL1$SNP %in% A$outliers$SNP,]),3)


