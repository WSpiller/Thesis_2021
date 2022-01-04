library(RadialMR)
library(TwoSampleMR)

data_temp<-data_radial

data_temp<-data_temp[data_temp$ldlcp2< 5*10^-8,]

ExData<-format_data(dat=data_temp,type = "exposure", snp_col = "rsid" ,beta_col="ldlcbeta",se_col= "ldlcse",eaf_col = "eaf",
                     effect_allele_col = "a1")

OutData<-format_data(dat=data_temp,type = "outcome", snp_col = "rsid" ,beta_col="chdbeta",se_col= "chdse",eaf_col = "eaf",
                    effect_allele_col = "a1")

datacomb <- harmonise_data(
  exposure_dat = ExData, 
  outcome_dat = OutData,action = 1
)

res1<-mr(datacomb, method_list=c("mr_egger_regression", "mr_ivw","mr_weighted_median","mr_weighted_mode"))

p1 <- mr_scatter_plot(res1, datacomb)

ldl.dat <- data_radial[data_radial[,10]<5*10^-8,]
ldl.fdat <- format_radial(ldl.dat[,6], ldl.dat[,9],
                          ldl.dat[,15], ldl.dat[,21], ldl.dat[,1])
ivw.object <- ivw_radial(ldl.fdat, 0.05, 1)
plot_radial(ivw.object,F,F,F)

egger.object <- egger_radial(ldl.fdat, 0.05, 1)
plot_radial(egger.object)



