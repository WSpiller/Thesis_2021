library(AER)

rm(list=ls())

pheno_data<-read.csv("z_data.csv",header=T)

xy_data<-read.csv("xy_data.csv",header=T)

xy_data[,1]<-as.numeric(xy_data[,1])

xy_data[,2]<-as.numeric(xy_data[,2])

head(xy_data)

xy_data[,1]<-scale(xy_data[,1])

xy_data[,2]<-scale(xy_data[,2])

mean(xy_data[,1])

mean(xy_data[,2])

pheno_data<-pheno_data[,-c(1,1691)]

print(names(pheno_data)[1690])

phenovec<-length(names(pheno_data))-1

F_stat_pvalue<-rep(99,phenovec)

for(i in 1:3){

GZ<-pheno_data[,1690] * pheno_data[,i]

F_stat_pvalue[i]<-summary(ivreg(xy_data[,1]~ivreg(xy_data[,2]|pheno_data$SCORE),diagostics=T)$diagnostics[1,4]}


output<-data.frame(names(pheno_data)[1:1689],F_stat_pvalue)


write.csv(output,"output.csv",row.names=F)
































summary(ivreg(xy_data[,1]~xy_data[,2]|pheno_data$SCORE))

summary(ivreg(xy_data[,1]~xy_data[,2]|pheno_data$SCORE),diagnostics=T)

summary(ivreg(xy_data[,1]~xy_data[,2]|pheno_data$SCORE),diagnostics=T)$diagnostics[1,4]