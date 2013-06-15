library(PvSurvival)
library(RMark)
setwd("C:\\Users\\JLaake\\git\\PvSurvival\\Analysis")
rm(list=ls())
load(".Rdata")

pv=extract.pv(dir="")
pv$digits[pv$SPENO==705]=1
pv=pv[!pv$Brand%in%c("229","229.1","232","232.1","473","473.1","564"),]
pv$digits2=ifelse(pv$digits==2,1,0)
pv$digits4=ifelse(pv$digits==4,1,0)
pv$digits6=ifelse(pv$digits==6,1,0)
# redefine td to exclude first capture event but create tdf covariate as well
#for(time in 1993:2010)
#{
#	pv[,paste("td",time+1,sep="")]=pv[,paste("td",time+1,sep="")]-pv[,paste("first",time,sep="")]
#    pv[,paste("tdf",time+1,sep="")]=pv[,paste("first",time,sep="")]
#}
pv$brandaspup=ifelse(pv$age==0&pv$AGEQ=="A",1,0)
pv$brandasnonpup=1-pv$brandaspup
pv$brandasyr=ifelse(pv$age==1&pv$AGEQ=="A",1,0)
pv.proc=process.data(pv,model="CJS",groups=c("Sex","Location","age"),age.var=3,initial.ages=as.numeric(levels(pv$age)),begin.time=1993)
pv.ddl=make.design.data(pv.proc)
# Add p design data
pv.ddl$p$adult=ifelse(pv.ddl$p$Age>=5,1,0)
#pv.ddl$p$EstAge=ifelse(pv.ddl$p$AGEQ=="E",1,0)
#pv.ddl$p$ActAge=1-pv.ddl$p$EstAge
pv.ddl$p$male=ifelse(pv.ddl$p$Sex=="M",1,0)
pv.ddl$p$ageclass=cut(pv.ddl$p$Age,c(1,2,4,10,50),right=FALSE)
# Add Phi design data
pv.ddl$Phi$nonpup=ifelse(pv.ddl$Phi$Age==0 ,0,1)
pv.ddl$Phi$pup=ifelse(pv.ddl$Phi$Age==0 ,1,0)
pv.ddl$Phi$yrlg=ifelse(pv.ddl$Phi$Age==1,1,0)
pv.ddl$Phi$py=ifelse(pv.ddl$Phi$Age<=5,1,0)
pv.ddl$Phi$twoyr=ifelse(pv.ddl$Phi$Age==2,1,0)
pv.ddl$Phi$twoplus=ifelse(pv.ddl$Phi$Age>=2,1,0)
pv.ddl$Phi$threeplus=ifelse(pv.ddl$Phi$Age>2,1,0)
pv.ddl$Phi$ageclass=cut(pv.ddl$Phi$Age,c(0,1,2,4,10,50),right=FALSE)
pv.ddl$Phi$ageclass1=cut(pv.ddl$Phi$Age,c(0,1,2,50),right=FALSE)
pv.ddl$Phi$timebin=cut(pv.ddl$Phi$Time,c(0,2,3:(2012-1993)),right=FALSE,labels=c("1993-1994",as.character(1995:2011)))
pv.ddl$Phi$timebin1=cut(pv.ddl$Phi$Time,c(0,10,19),right=FALSE)
#pv.ddl$Phi$EstAge=ifelse(pv.ddl$Phi$AGEQ=="E",1,0)
#pv.ddl$Phi$ActAge=1-pv.ddl$Phi$EstAge
# Model fitting function
do.pv=function()
{
# p models
p.1=list(formula=~time+Location+ageclass+td)
p.2=list(formula=~time+Location+ageclass+Sex+td)
p.3=list(formula=~time+Location+ageclass+adult:male+td)
p.4=list(formula=~time+Location+Sex*ageclass+td)
p.5=list(formula=~time+Location+ageclass)
p.6=list(formula=~time+Location+ageclass+Sex)
p.7=list(formula=~time+Location+ageclass+adult:male)
p.8=list(formula=~time+Location+Sex*ageclass)

#Phi models
Phi.1=list(formula=~ageclass)
Phi.2=list(formula=~Sex+ageclass)
Phi.3=list(formula=~Sex+ageclass+Time)
Phi.4=list(formula=~Sex+ageclass+time)
Phi.5=list(formula=~Sex+ageclass+Time)

Phi.6=list(formula=~Sex+ageclass+Time+brandaspup:pup:digits)
Phi.7=list(formula=~Sex+ageclass+Time+brandaspup:digits)
Phi.8=list(formula=~Sex+ageclass+Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6)
Phi.9=list(formula=~Sex+ageclass+Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6+ brandasnonpup:digits)
	
Phi.10=list(formula=~Sex+ageclass+time)
Phi.11=list(formula=~Sex+ageclass+time+brandaspup:pup:digits)
Phi.12=list(formula=~Sex+ageclass+time+brandaspup:digits)
Phi.13=list(formula=~Sex+ageclass+time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6)
Phi.14=list(formula=~Sex+ageclass+time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6+ brandasnonpup:digits)

Phi.15=list(formula=~Sex+ageclass1*Time)
Phi.16=list(formula=~Sex+ageclass1*Time+brandaspup:pup:digits)
Phi.17=list(formula=~Sex+ageclass1*Time+brandaspup:digits)
Phi.18=list(formula=~Sex+ageclass1*Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6)
Phi.19=list(formula=~Sex+ageclass1*Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6+ brandasnonpup:digits)

Phi.20=list(formula=~Sex+ageclass+pup:Time)
Phi.21=list(formula=~Sex+ageclass+pup:Time+brandaspup:pup:digits)
Phi.22=list(formula=~Sex+ageclass+pup:Time+brandaspup:digits)
Phi.23=list(formula=~Sex+ageclass+pup:Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6)
Phi.24=list(formula=~Sex+ageclass+pup:Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6+ brandasnonpup:digits)

Phi.25=list(formula=~Sex*ageclass+Time)
Phi.26=list(formula=~Sex*ageclass+Time+brandaspup:pup:digits)
Phi.27=list(formula=~Sex*ageclass+Time+brandaspup:digits)
Phi.28=list(formula=~Sex*ageclass+Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6)
Phi.29=list(formula=~Sex*ageclass+Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6+ brandasnonpup:digits)

Phi.30=list(formula=~Sex*ageclass+time)
Phi.31=list(formula=~Sex*ageclass+time+brandaspup:pup:digits)
Phi.32=list(formula=~Sex*ageclass+time+brandaspup:digits)
Phi.33=list(formula=~Sex*ageclass+time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6)
Phi.34=list(formula=~Sex*ageclass+time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6+ brandasnonpup:digits)

Phi.35=list(formula=~Sex*ageclass+ageclass1:Time)
Phi.36=list(formula=~Sex*ageclass+ageclass1:Time+brandaspup:pup:digits)
Phi.37=list(formula=~Sex*ageclass+ageclass1:Time+brandaspup:digits)
Phi.38=list(formula=~Sex*ageclass+ageclass1:Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6)
Phi.39=list(formula=~Sex*ageclass+ageclass1:Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6+ brandasnonpup:digits)

Phi.41=list(formula=~Sex*ageclass+pup:Time)
Phi.42=list(formula=~Sex*ageclass+pup:Time+brandaspup:pup:digits)
Phi.43=list(formula=~Sex*ageclass+pup:Time+brandaspup:digits)
Phi.44=list(formula=~Sex*ageclass+pup:Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6)
Phi.45=list(formula=~Sex*ageclass+pup:Time+brandaspup:digits2 +brandaspup:digits4 + brandaspup:digits6+ brandasnonpup:digits)
# create model list and run
cml=create.model.list("CJS")
return(mark.wrapper(cml,data=pv.proc,ddl=pv.ddl,output=FALSE))
}
results=do.pv()
head(results$model.table)

# Release gof test
pv.rel=pv
pv.rel$age=factor(cut(as.numeric(as.character(pv$age)),c(0,1,2,4,50),right=FALSE))
pv.proc.rel=process.data(pv.rel,model="CJS",groups=c("Sex","Location","age"),age.var=3,initial.ages=c(0,1,2,4),begin.time=1993)
release.gof(pv.proc.rel)



# natality data
library(CalcurData)
dir="C:\\Users\\JLaake\\Desktop\\Calcurr\\Master"
begin=0615
end=1015
Captures=getCalcurData("Pv","Captures",dir=dir)
Recaptures=getCalcurData("Pv","Recapture",dir=dir)
#   List of all branded seals and the year they were branded	
Brand=getCalcurData("Pv","All brands",dir=dir)
#   All resights of branded and tagged animals
Resight=getCalcurData("Pv","Resight",dir=dir)

names(Brand)[4]="Brand"
names(Captures)[12]="Brand"
names(Recaptures)[5]="Brand"
names(Brand)[6]="BrandYear"
names(Resight)[4]="Brand"
Captures$Year=as.POSIXlt(Captures$DAY)$year+1900
Recaptures$Year=as.POSIXlt(Recaptures$DAY)$year+1900
Resight$Year=as.POSIXlt(Resight$DAY)$year+1900
Resight$mday=as.POSIXlt(Resight$DAY)$mday+100*(as.POSIXlt(Resight$DAY)$mon+1)
LimitedResights=Resight[Resight$mday<=end&Resight$mday>=begin,c("SPENO","Year","STATUS","DAY","PUP","STATUS")]
BrandResightJoin=merge(LimitedResights,Brand[,names(Brand)[!names(Brand)=="DAY"]],by="SPENO",all.x=TRUE)
BrandResightJoin$Age=as.numeric(substr(BrandResightJoin$DAY,1,4))-BrandResightJoin$COHORT
#Resight=getCalcurData("Pv","Resight",dir=dir)
#nat=Resight[Resight$AGEQ=="A"&Resight$SEX=="F",]
nat=BrandResightJoin[BrandResightJoin$SEX=="F",]
nat=nat[!is.na(nat$SPENO),]
nat$natstatus="u"
haspreg=grep("PREG",toupper(nat$STATUS))
hasnot=grep("NOT",toupper(nat$STATUS))
nat$natstatus[haspreg[!haspreg%in%hasnot]]="P"
npindices=hasnot[hasnot%in%haspreg]
beforeAug=as.numeric(paste(substr(as.character(as.Date(nat$DAY)),6,7),substr(as.character(as.Date(nat$DAY)),9,10),sep=""))<801
nat$natstatus[npindices][beforeAug[npindices]]="N"
nat$natstatus[nat$PUP%in%c("Yes","Y","y","yes")]="P"
nat=nat[order(nat$SPENO,nat$DAY),]
nat=do.call("rbind",lapply(split(nat,list(nat$SPENO,substr(nat$DAY,1,4))),function(x) {
			if(nrow(x)!=0)
			if(toupper(x$STATUS[1])%in%c("PP","POSTPARTUM","POST PARTUM")) x$natstatus[1]="P"
            x}))
# mods to PostPartum
nat$natstatus[nat$SPENO=="741"&as.character(as.Date(nat$DAY))=="1997-08-14"]="P"
nat$natstatus[nat$SPENO=="887"&as.character(as.Date(nat$DAY))=="1998-08-25"]="P"
nat$natstatus[nat$SPENO=="1474"&as.character(as.Date(nat$DAY))=="2002-08-15"]="P"
nat$natstatus[nat$SPENO=="1635"&as.character(as.Date(nat$DAY))=="2010-07-05"]="P"
nat$natstatus[nat$SPENO=="1803"&as.character(as.Date(nat$DAY))=="2010-08-03"]="P"
nat$week=cut(as.Date(paste("2000-",substr(nat$DAY,6,10))),as.Date(paste("2000-",c("01-01","06-15","06-22","06-29","07-06","07-13","07-20","07-27","08-03","08-10","08-17","08-24","08-31","09-07","09-14","09-21","09-28","10-05","10-16","12-31"),sep="")))
nat=nat[as.numeric(nat$week)%in%2:18,]
nat=droplevels(nat)

natch=with(nat,tapply(natstatus,list(SPENO,substr(DAY,1,4)),function(x) ifelse(any(x=="P"),1,0)))
sum(natch,na.rm=T)

#write.table(nat,file="natality.txt",sep="\t")
natch=with(nat,tapply(natstatus,list(SPENO,week,substr(DAY,1,4)),function(x) ifelse(any(x=="P"),"P",ifelse(any(x=="N"),"N","u"))))
natch[is.na(natch)]=0
natsum=NULL
for(i in 1:19)
  natsum=rbind(natsum,data.frame(Year=rep(i+1992,length(rownames(natch))),SPENO=rownames(natch),ch=apply(natch[,,i],1,paste,collapse=""),stringsAsFactors=FALSE))
natsum=natsum[natsum$ch!="00000000000000000",]

natsum=merge(natsum,Brand[,c("SPENO","COHORT")],by="SPENO",all.x=TRUE)
natsum$Age=natsum$Year-natsum$COHORT

natsum$P=0
natsum$P[grep("P",natsum$ch)]=1


barplot(tapply(natsum$P,cut(natsum$Age,c(0:10,12,14,18,20,25)),mean),xlab="Age",ylab="Proportion with pup of those seen",main="Naive estimate of reproductive rate - all seals")

barplot(tapply(natsum$P,natsum$Year,mean),xlab="Year",ylab="Proportion with pup of those seen",main="Naive estimate of reproductive rate - all seals")

oops=natsum[grep("N",natsum$ch),]
oops=oops[grep("P",oops$ch),]
rownames(oops)=NULL
oops

plot(1994:2010,summary(results[[83]])$reals$Phi[[1]]$pim[1,-1],ylim=c(.5,1))
 points(1994:2010,summary(results[[83]])$reals$Phi[[5]]$pim[1,-1],pch=2)
 points(1994:2010,summary(results[[83]])$reals$Phi[[4]]$pim[1,-1],pch=3)




# Proportion observed by age for known aged animals
pv=extract.pv()
pv=pv[pv$AGEQ=="A",]
pv$Age=as.numeric(as.character(pv$age))
possible.F=matrix(0,nrow=30,ncol=2011-1993)
for(year in 1994:2011)
	possible.F[,year-1993]=with(pv[pv$Sex=="F",],table(factor((1-pv[pv$Sex=="F",paste("first",year,sep="")])*(Age+year-BrandYear),levels=1:30)))	
colnames(possible.F)=1994:2011
rownames(possible.F)=paste("Age:",1:30,sep="")

possible.M=matrix(0,nrow=30,ncol=2011-1993)
for(year in 1994:2011)
	possible.M[,year-1993]=with(pv[pv$Sex=="M",],table(factor((1-pv[pv$Sex=="M",paste("first",year,sep="")])*(Age+year-BrandYear),levels=1:30)))
colnames(possible.M)=1994:2011
rownames(possible.M)=paste("Age:",1:30,sep="")

chmat=t(sapply(pv$ch,function(x) as.numeric(strsplit(x,"")[[1]])))
observed.F=matrix(0,nrow=30,ncol=2011-1993)
chF=chmat[pv$Sex=="F",]
for(year in 1994:2011)
	observed.F[,year-1993]=with(pv[pv$Sex=="F",],table(factor((1-pv[pv$Sex=="F",paste("first",year,sep="")])*chF[,year-1992]*(Age+year-BrandYear),levels=1:30)))	
colnames(observed.F)=1994:2011
rownames(observed.F)=paste("Age:",1:30,sep="")

observed.M=matrix(0,nrow=30,ncol=2011-1993)
chM=chmat[pv$Sex=="M",]
for(year in 1994:2011)
	observed.M[,year-1993]=with(pv[pv$Sex=="M",],table(factor((1-pv[pv$Sex=="M",paste("first",year,sep="")])*chM[,year-1992]*(Age+year-BrandYear),levels=1:30)))	
colnames(observed.M)=1994:2011
rownames(observed.M)=paste("Age:",1:30,sep="")

par(mfrow=c(2,1))
obs=c(rowSums(observed.F)[1:17],sum(rowSums(observed.F)[18:30]))
names(obs)=paste("Age:",c(1:17,"18+"),sep="")
pos=c(rowSums(possible.F)[1:17],sum(rowSums(possible.F)[18:30]))
names(pos)=paste("Age:",c(1:17,"18+"),sep="")
barplot(obs/pos,main="Female")

obs=c(rowSums(observed.M)[1:17],sum(rowSums(observed.M)[18:30]))
names(obs)=paste("Age:",c(1:17,"18+"),sep="")
pos=c(rowSums(possible.M)[1:17],sum(rowSums(possible.M)[18:30]))
names(pos)=paste("Age:",c(1:17,"18+"),sep="")
barplot(obs/pos,main="Male")

