library(ggplot2)

#Simulate data

N<-100

LDL<-rnorm(N,3.55593,0.8708)
HDL<-rnorm(N,1.45051,0.383054)
outcome<-5 + 1*LDL + (-2)*HDL + rnorm(N,0,1)

data<-data.frame(c(LDL,HDL),rep(outcome,2),c(rep("LDL",N),rep("HDL",N)))
names(data)<-c("mmol","outcome","lipid_type")

ldl.mod<-lm(outcome ~ mmol, data=data[data$lipid_type=="LDL",])
hdl.mod<-lm(outcome ~ mmol, data=data[data$lipid_type=="HDL",])

mregplot<-ggplot(data, aes(x=mmol, y=outcome,color = lipid_type)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=F)+
    xlab("Cholesterol (mmol/L)")+
    ylab("Outcome")+
    labs(color="Lipid fraction")+theme_bw()

png(filename = "Mreg_example.png",
    width = 1920 , height = 1500, units = "px", res=300,
    bg = "white")

mregplot

dev.off()
