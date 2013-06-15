#' Pv capture histories and covariates
#' 
#' Extracts data from ACCESS database and constructs the relevant capture histories from the
#' tables. It constructs all the queries that used to be done in ACCESS.
#' 
#' Uses CalcurData package to extract from ACCESS database and extracts initial tag, recapture and 
#' resightings information.  
#'
#' @import CalcurData 
#' @export
#' @param dir Directory containing ACCESS database; if NULL uses std location
#' @param begin month-day at beginning of resight period (615 = June 15)
#' @param end month-day at end of resight period (1015 = Oct 15)
#' @return dataframe containing following fields \item{ch}{capture history;
#'   character string} \item{Speno}{unnique identifier} \item{Brand}{brand identifier}
#'   \item{BrandYear}{year branded} \item{AgeClass}{Age class P,Y,S,A at time of initial marking}
#'   \item{Sex}{either M or F; factor variable} \item{Weight}{weight (kg) at time of branding minus age-class and sex-specific mean}
#'   \item{Cohort}{initial birth year}
#'   \item{td}{sequence of
#'   fields named tdyyyy with values 0/1; it is 1 if resighted in the prior year}
#'   \item{TotalTimesResighted}{number of years resighted}
#'   \item{recap}{0 if never resighted and 1 otherwise}
#'   \item{Location}{factor variable; Gertrude or Eagle location of animal at initial marking}
#'   \item{first}{sequence of
#'   fields named firstyyyy with values 0/1; it is 1 for year it was first branded}
#'   \item{age}{factor variable of age at first marking} \item{digits}{number of brand digits}
#' 
#' @author Jeff Laake
#' @examples 
#' pvdata=extract.pv()
extract.pv=function(dir=NULL,begin=615,end=1015)
{
#   Capture and recapture tables with baseline information
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
	LimitedResights=Resight[Resight$mday<=end&Resight$mday>=begin,c("Brand","SPENO","Year")]
	BrandResightJoin=merge(Brand,LimitedResights,by="SPENO",all.x=TRUE)
	
	resight.count.table=with(BrandResightJoin,table(SPENO,Year))
	cohort.count.table=with(BrandResightJoin,table(SPENO,BrandYear))
	if(ncol(cohort.count.table)<ncol(resight.count.table)) 
		cohort.count.table=cbind(cohort.count.table,matrix(0,nrow=nrow(cohort.count.table),
						ncol=ncol(resight.count.table)-ncol(cohort.count.table)))

	capture.history=cohort.count.table+resight.count.table
	capture.history[capture.history>1]=1
	class(capture.history)="matrix"

	xx=Brand[,c("Brand","SPENO","SEX","COHORT","SITECODE","AGECLASS","BrandYear","AGEQ")]
	xx$key=paste(xx$SPENO,xx$BrandYear,sep="")
	Captures$key=paste(Captures$SPEN0,Captures$Year,sep="")
	xx=merge(xx,Captures[,c("key","WEIGHT")],by="key",all.x=TRUE)
	Recaptures$key=paste(Recaptures$SPENO,Recaptures$Year,sep="")
	Recaptures=Recaptures[,c("key","WEIGHT")]
	xx=merge(xx,Recaptures[!duplicated(Recaptures$key),],by="key",all.x=TRUE)
	xx$AGECLASS=toupper(xx$AGECLASS)
	xx$AGECLASS[xx$AGECLASS=="W"]="P"
	xx$AgeClass=factor(toupper(xx$AGECLASS),levels=c("P","Y","S","A"))
	xx$Sex=factor(toupper(xx$SEX))
# compute number with unknown weight by sex/age
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
	if(ncol(xx1)<ncol(resight.count.table)) 
		xx1=cbind(xx1,matrix(0,nrow=nrow(xx1),
						ncol=ncol(resight.count.table)-ncol(xx1)))
	zz=t(apply(xx1,1,cumsum))
    capture.history=zz*capture.history
	colnames(capture.history)=paste("td",as.numeric(colnames(capture.history))+1,sep="")
	colnames(xx1)=paste("first",colnames(xx1),sep="")
	CaptureHistory=as.data.frame(capture.history)
	CaptureHistory$SPENO=row.names(CaptureHistory)
	CaptureHistory=merge(xx,CaptureHistory,by="SPENO",all.x=TRUE)
	capture.history=CaptureHistory[,-(1:9)]
	CaptureHistory$TotalTimesResighted=rowSums(capture.history)-1
	CaptureHistory$recap=ifelse(CaptureHistory$TotalTimesResighted>0,1,0)
	CaptureHistory$Location=factor(ifelse(CaptureHistory$SITECODE<=10.06,"Gertrude","Eagle"),levels=c("Gertrude","Eagle"))
	MarkData=data.frame(ch=apply(capture.history,1,paste,collapse=""),stringsAsFactors=FALSE)
	MarkData=cbind(MarkData,CaptureHistory)
	MarkData$SITECODE=NULL
    MarkData=cbind(MarkData,xx1)
	MarkData$age=factor(MarkData$BrandYear-MarkData$Cohort)
	MarkData$digits=6
	suppressWarnings(brandnum<- as.numeric(as.character(Brand$Brand)))
#   see if these are still correct with new one sided brands
	MarkData$digits[brandnum>0&brandnum<=6]=1
	MarkData$digits[brandnum>6 & brandnum<=9]=2
	MarkData$digits[brandnum>9 & brandnum<=99]=4
	MarkData$digits[MarkData$Brand%in%
			c("v0",paste(">",0:9,sep=""),
			"0.01","0.02","0.03","0.04","0.05","0.06","0.07","0.08","0.09",
			paste("C",0:9,sep=""),paste("U",0:9,sep=""),paste("n",0:9,sep=""),paste(")",0:9,sep=""))]=4
    MarkData$digits[MarkData$BrandYear%in%2010:2011&MarkData$age==0]=2
	return(MarkData)
}


