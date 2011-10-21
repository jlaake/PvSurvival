setwd("C:\\Users\\JLaake\\git\\PvSurvival\\Analysis")
library(PvSurvival)
load(".Rdata")
pv=extract.pv()
pv.proc=process.data(pv,model="CJS",groups=c("Sex","Location","age"),age.var=3,initial.ages=as.numeric(levels(pv$age)),begin.time=1993)
pv.ddl=make.design.data(pv.proc,parameters=list(Phi=list(age.bins=c(0,1,2,4,6,10,50)),p=list(age.bins=c(1,2,3,5,50))),right=FALSE)
pv.ddl$p$adult=0
pv.ddl$p$adult[pv.ddl$p$Age>=5]=1
pv.ddl$p$male=0
pv.ddl$p$male[pv.ddl$p$Sex=="M"]=1
pv.ddl$Phi$nonpup=1
pv.ddl$Phi$nonpup[pv.ddl$Phi$Age==0]=0
pv.ddl$Phi$pup=1-pv.ddl$Phi$nonpup

do.pv=function()
{
#p.1=list(formula=~time)
#p.2=list(formula=~time+Location)
#p.3=list(formula=~time+Location+Sex)
#p.4=list(formula=~time+Sex)
#p.5=list(formula=~time+age)
p.6=list(formula=~time+Location+Sex+age)
p.7=list(formula=~time+Location+adult:male+age)
p.8=list(formula=~time+Location+adult:male+age+td)

#Phi.1=list(formula=~Weight)
#Phi.2=list(formula=~age)

pup:Weight + non-pup:weight  -- first year
pre-weaning mortality for last 5 years as covariate

	
#Phi.7=list(formula=~age +Time+Sex)
#Phi.8=list(formula=~age + age:Time+Sex)
#Phi.9=list(formula=~age+ Time+age*Sex)
#Phi.7=list(formula=~age +Time+Sex)
Phi.10=list(formula=~age+ Time+Sex +first:nonpup)
Phi.11=list(formula=~age+ pup:time+ Sex)

#Phi.11=list(formula=~age+ Time+Sex +first:nonpup+first:Weight)
cml=create.model.list("CJS")
return(mark.wrapper(cml,data=pv.proc,ddl=pv.ddl,output=FALSE))
}
results=do.pv()
summary(results[[1]],brief=TRUE)
