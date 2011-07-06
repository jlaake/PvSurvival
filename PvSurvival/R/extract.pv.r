extract.pv <-
function(file="PvObservations.mdb",dir="C:\\Users\\Jeff Laake\\Desktop\\Workspace\\PvSurvival",begin=615,end=1015)
{
	xx=require(RODBC,quietly=TRUE)
	fdir=file.path(dir,file)
	connection=odbcConnectAccess(fdir)
	Resight=sqlFetch(connection,"Resight")
	Captures=sqlFetch(connection,"Captures")
	Recaptures=sqlFetch(connection,"Recapture")
	Brand=sqlFetch(connection,"All brands")
	names(Brand)[4]="Brand"
	names(Captures)[12]="Brand"
	names(Recaptures)[5]="Brand"
	names(Brand)[6]="BrandYear"
	names(Resight)[4]="Brand"
	Captures$Year=as.POSIXlt(Captures$DAY)$year+1900
	Recaptures$Year=as.POSIXlt(Recaptures$DAY)$year+1900
	Resight$Year=as.POSIXlt(Resight$DAY)$year+1900
	Resight$mday=as.POSIXlt(Resight$DAY)$mday+100*(as.POSIXlt(Resight$DAY)$mon+1)
	LimitedResights=subset(Resight,subset=mday<=end&mday>=begin,select=c("Brand","SPENO","Year"))
	BrandResightJoin=merge(Brand,LimitedResights,by="SPENO",all.x=TRUE)
	resight.count.table=with(BrandResightJoin,table(SPENO,Year))
	cohort.count.table=with(BrandResightJoin,table(SPENO,BrandYear))
	capture.history=cohort.count.table+resight.count.table
	capture.history[capture.history>1]=1
	class(capture.history)="matrix"
	xx=subset(Brand,select=c("Brand","SPENO","SEX","COHORT","SITECODE","AGECLASS","BrandYear"))
	xx$key=paste(xx$SPENO,xx$BrandYear,sep="")
	Captures$key=paste(Captures$SPEN0,Captures$Year,sep="")
	xx=merge(xx,subset(Captures,select=c("key","WEIGHT")),by="key",all.x=TRUE)
	Recaptures$key=paste(Recaptures$SPENO,Recaptures$Year,sep="")
	xx=merge(xx,subset(Recaptures,select=c("key","WEIGHT")),by="key",all.x=TRUE)
	xx$AgeClass=factor(toupper(xx$AGECLASS),levels=c("P","Y","S","A"))
	xx$Sex=factor(toupper(xx$SEX))
	xx$Weight=xx$WEIGHT.x
	xx$Weight[is.na(xx$Weight)]=xx$WEIGHT.y[is.na(xx$Weight)]
    means=with(xx[!is.na(xx$Weight),],tapply(Weight,list(AgeClass,Sex),mean))
    indices=cbind(xx$AgeClass,xx$Sex)
	subtract=means[indices]
	xx$Wt=xx$Weight
	xx$Weight=xx$Wt-subtract
    xx$Cohort=xx$COHORT
	xx$WEIGHT.x=NULL
	xx$WEIGHT.y=NULL
	xx$SEX=NULL
	xx$Wt=NULL
	xx$AGECLASS=NULL
	xx$key=NULL
	xx$COHORT=NULL
#	xx$Brand=as.character(xx$SPENO)
	xx1=with(xx,table(SPENO,BrandYear))
	class(xx1)="matrix"
	zz=t(apply(xx1,1,cumsum))
    capture.history=zz*capture.history
	colnames(capture.history)=paste("td",as.numeric(colnames(capture.history))+1,sep="")
	colnames(xx1)=paste("first",colnames(xx1),sep="")
	CaptureHistory=as.data.frame(capture.history)
	CaptureHistory$SPENO=row.names(CaptureHistory)
	CaptureHistory=merge(xx,CaptureHistory,by="SPENO",all.x=TRUE)
	capture.history=CaptureHistory[,-(1:8)]
	CaptureHistory$TotalTimesResighted=rowSums(capture.history)-1
	CaptureHistory$recap=ifelse(CaptureHistory$TotalTimesResighted>0,1,0)
	CaptureHistory$Location=factor(ifelse(CaptureHistory$SITECODE<=10.06,"Gertrude","Eagle"),levels=c("Gertrude","Eagle"))
	MarkData=data.frame(ch=apply(capture.history,1,paste,collapse=""),stringsAsFactors=FALSE)
	MarkData=cbind(MarkData,CaptureHistory)
	MarkData$SITECODE=NULL
    MarkData=cbind(MarkData,xx1)
	MarkData$age=factor(MarkData$BrandYear-MarkData$Cohort)
	MarkData$digits=6
	brandnum=as.numeric(as.character(Brand$Brand))
	brandnum[is.na(brandnum)]=0
	MarkData$digits[brandnum>0&brandnum<=6]=1
	MarkData$digits[brandnum>6 & brandnum<=9]=2
	MarkData$digits[brandnum>9 & brandnum<=99]=4
	MarkData$digits[MarkData$Brand%in%
			c("v0",paste(">",0:9,sep=""),"0.01","0.02","0.03","0.04","0.05","0.06","0.07","0.08","0.09")]=4
	return(MarkData)
}


