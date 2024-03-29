MOBITC_Stat_WLS<-function(distTDC,numdateTDC,incertitude,IC,datex,numdateprosp)
{

if (length(distTDC)>1)
{
	model=lm(distTDC ~ as.vector(numdateTDC),weights = (1/incertitude/2)^2)
	taux=round(coef(model)[2]*365.25*24*3600,digits=2)
	} else {
	taux=NA
}

if (length(IC)>0)
{
	for (ic in 1 : length(IC))
	{
		if (length(distTDC)>1)
		{
			icConf  <- predict(model,new=datex,interval="confidence",level=IC[ic]/100)
			if (ic==1)
			{
				res=icConf[,1]
			}
			res=cbind(res,icConf[,2:3])
		} else {
			if (ic==1)
			{
				res=NA*seq(1,1,length=length(datex[,1]))
			}
			res=cbind(res,NA*seq(1,1,length=length(datex[,1])),NA*seq(1,1,length=length(datex[,1])))
		}
			
		colnames(res)[2*ic]=paste("WLS",IC[ic],"lwr",sep="")
		colnames(res)[2*ic+1]=paste("WLS",IC[ic],"up",sep="")
	}
} else {
	if (length(distTDC)>1)
	{
		res=coef(model)[2]*datex+coef(model)[1]
	} else {
		res=cbind(NA*seq(1,1,length=length(datex[,1])))
	}
}

colnames(res)[1]="WLSfit"
return(list(taux,res))
}