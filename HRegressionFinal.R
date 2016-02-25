##################################################################################
###Ｓtinky tofu guilt reduction
##2015.3.15
##Author:GregTsai   Email:gregorytsai@gmail.com
##Developed on R Ver.3.1.2
##################################################################################

##Install packages before first time use
install.packages("psych")   # For correlation Test
install.packages("car")     
install.packages("ggplot2")
install.packages("pequod")  #For simple slope
install.packages("lmSupport") #For delta R square
install.packages("MBESS")     #For mediation
install.packages("gsl")       #For mediation


 
##Run after launching R
library("psych")
library("car") 
library("ggplot2")
library("pequod")
library("lmSupport")
library("MBESS")
library("gsl")




##
data=read.csv("~/Documents/碩論資料/Data anlysis/20150213/20150628.csv", header=T) 

names(data)
nrow(data)
ncol(data)


##
p=0.05
num=1



##############################################################
result = matrix(nrow=1, ncol=23)    #
result = c("DependentVar","Var1","Var2","Adj. R Square","Model p","Var1 b","Var1 p","Var2 b","Var2 p","Interaction b","Interaction p","Mod1","Mod1LowR","Mod1HighR","Mod1 p","Mod2","Mod2LowR","Mod2HighR","Mod2 p","LowIndeLowMod", "LowIndeHighMod", "HighIndeLowMod", "HighIndeHighMod")                     


##­
lmp <- function (modelobject) {
	if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
	f <- summary(modelobject)$fstatistic
	p <- pf(f[1],f[2],f[3],lower.tail=F)
	attributes(p) <- NULL
	return(p)
} 

##
corCalculate <- function (dep, pre, mod) {
	avg=mean(data[,mod],na.rm=T)                 #­
	group=matrix(ncol=1,nrow=nrow(data))       #
	for (r in 1:nrow(data)) {
		if (is.na(data[r,mod]))   {group[r]=NA}
		else if (data[r,mod]>avg) {group[r]=1} #
		else if (data[r,mod]<avg) {group[r]=0} #
		}

	#
	mydata = cbind(data[,pre],data[,dep],group)
	mydata0=subset(mydata, group==0)
	mydata1=subset(mydata, group==1)
	n0=nrow(mydata0)
	n1=nrow(mydata1)

	#­
	cor0=round(cor(mydata0[,1],mydata0[,2],use="pairwise.complete.obs"),digits=3)
	cor1=round(cor(mydata1[,1],mydata1[,2],use="pairwise.complete.obs"),digits=3)
	rTest=r.test(n=n0,cor0,cor1,n2=n1) #

	result=cbind(names(data[mod]),cor0,cor1,round(rTest$p,digits=4),deparse.level = 0)
	return(result)
} 



#
for (i in 28:35) 
{
	for (j in 8:10 )
	 {
		for (k in 49:57) 
		{
			
			model <- lm(data[,i] ~ scale(data[,3]) + scale(data[,4]) + scale(data[,j])*scale(data[,k]))
			modelP=lmp(model)
			moderatorP=summary.lm(model)$coefficients["scale(data[, j]):scale(data[, k])","Pr(>|t|)"]
	      	if (moderatorP<=p) {
				

				cor1=corCalculate(i, k, j) #
				cor2=corCalculate(i, j, k) #
				
				#simple slope
				modelSimple=paste0(names(data[i]),"~",names(data[j]),"*",names(data[k]))
				mod5=lmres(formula=modelSimple  , centered=c(names(data[j]),names(data[k])),data=data)
				slopedist=simpleSlope(mod5, pred=names(data[j]), mod1=names(data[k]))  
				chart=paste0(  "Chart",num,".jpg" )
				jpeg(file=chart,height=480,width=720,res = 72,units = "px",pointsize=16,quality=90)
				print(PlotSlope(slopedist)) 
				dev.off() 
				num=num+1
				


				
				
				
				
				modelR2=round(summary.lm(model)$adj.r.squared,digits=3)
				var1B=round(summary.lm(model)$coefficients["scale(data[, j])","Estimate"],digits=3)
				var2B=round(summary.lm(model)$coefficients["scale(data[, k])","Estimate"],digits=3)
				interB=round(summary.lm(model)$coefficients["scale(data[, j]):scale(data[, k])","Estimate"],digits=3)
				var1P=round(summary.lm(model)$coefficients["scale(data[, j])","Pr(>|t|)"],digits=4)
				var2P=round(summary.lm(model)$coefficients["scale(data[, k])","Pr(>|t|)"],digits=4)
				interP=round(summary.lm(model)$coefficients["scale(data[, j]):scale(data[, k])","Pr(>|t|)"],digits=4)
				regData=cbind(names(data[i]),names(data[j]),names(data[k]),modelR2,round(modelP,digits=4),var1B,var1P,var2B,var2P,interB,interP,deparse.level = 0)
				
				#
				resultAll=cbind(regData,cor1,cor2,slopedist$Point[1], slopedist$Point[2], slopedist$Point[3], slopedist$Point[4])
				result = rbind( result,resultAll)

				rm(var1B)
				rm(var2B)
				rm(interB)
				rm(var1P)
				rm(var2P)
				rm(interP)
				rm(regData)
				rm(cor1)
				rm(cor2)
				rm(resultAll)
			}
			rm(modelP)
			rm(moderatorP)
		}
	}

}

##simple slope  example
mod5=lmres(impulsive ~ gender + educationmoma + PCE*Teaching  , centered=c("PCE","Teaching"), data=data)
slopedist=simpleSlope(mod5, pred="PCE", mod1="Actx13r") 
summary(slopedist) 
PlotSlope(slopedist) 
chart=paste0(  "Chart",num,".jpg" )
jpeg(file=chart) 
num=num+1


##reg naming
			model <- lm(data[,i] ~ scale(data[,j])*scale(data[,k]))
			summary(model)
			model1 <- lm(data$ChildCentered ~ scale(data$PCE)*scale(data$Actx13r))
			summary(model1)
			model2 <- lm(ChildCentered ~ scale(PCE)*scale(Actx13r),data=data)
			summary(model2)
			modelCompare(model1, model2)

##HLM
			model1 <- lm(ChildCentered ~ scale(PCE)+scale(Actx13r),data=data)
			summary(model1)
			model2 <- lm(ChildCentered ~ scale(PCE)*scale(Actx13r),data=data)
			summary(model2)
			modelCompare(model1, model2)


##
write.csv("~/Documents/碩論資料/Data anlysis/20150213/20150712.csv", x=result, row.names = F,col.names = F)




##mediation
dataTemp = cbind(data1[,3:4],data1[,9:12],data1[,21],data1[,23],data1[,27:28],data1[,30],data1[,25],data1[,38])
mydata = dataTemp[complete.cases(dataTemp),]   #find completedata but i may need to rename the variable
mediation(x=data$PCE, mediator=data$PALSL, dv=data$QFactA, bootstrap = T, B = 1000)


##simple slope ChildCentered PCE*Actx13r
mod5=lmres(ChildCentered ~ gender + educationmoma + PCE* Actx13r  , centered=c("PCE","Actx13r"), data=data)
slopedist=simpleSlope(mod5, pred="PCE", mod1="Actx13r") 
slopedist2=simpleSlope(mod5, pred="Actx13r", mod1="PCE")  
summary(slopedist) 
summary(slopedist2) 


##simple slope LOG1pcenter EVE*PercepSensitive
mod5=lmres(LOG1pcenter ~ gender + educationmoma + EVE * PercepSensitive  , centered=c("EVE","PercepSensitive"), data=data)
slopedist=simpleSlope(mod5, pred="EVE", mod1="PercepSensitive") 
slopedist2=simpleSlope(mod5, pred="PercepSensitive", mod1="EVE")  
summary(slopedist) 
summary(slopedist2) 


##simple slope LOG1pcenter EVE* AttenionFocus
mod5=lmres(LOG1pcenter ~ gender + educationmoma + EVE * AttenionFocus  , centered=c("EVE","AttenionFocus"), data=data)
slopedist=simpleSlope(mod5, pred="EVE", mod1="AttenionFocus") 
slopedist2=simpleSlope(mod5, pred="AttenionFocus", mod1="EVE")  
summary(slopedist) 
summary(slopedist2) 

##simple slope LOG1teaching EVE* AttenionFocus
mod5=lmres(LOG1teaching ~ gender + educationmoma + EVE * AttenionFocus  , centered=c("EVE","AttenionFocus"), data=data)
slopedist=simpleSlope(mod5, pred="EVE", mod1="AttenionFocus") 
slopedist2=simpleSlope(mod5, pred="AttenionFocus", mod1="EVE")  
summary(slopedist) 
summary(slopedist2)


##simple slope ChildCentered EVI* impulsive
mod5=lmres(ChildCentered ~ gender + educationmoma + EVI * impulsive  , centered=c("EVI","impulsive"), data=data)
slopedist=simpleSlope(mod5, pred="EVI", mod1="impulsive") 
slopedist2=simpleSlope(mod5, pred="impulsive", mod1="EVI")  
summary(slopedist) 
summary(slopedist2) 

##simple slope LOG1teaching EVE* PercepSensitive
mod5=lmres(LOG1teaching ~ gender + educationmoma + EVE * PercepSensitive  , centered=c("EVE","PercepSensitive"), data=data)
slopedist=simpleSlope(mod5, pred="EVE", mod1="PercepSensitive") 
slopedist2=simpleSlope(mod5, pred="PercepSensitive", mod1="EVE")  
summary(slopedist) 
summary(slopedist2) 


##simple slope LOG1teaching PCE* ActivityLevel
mod5=lmres(LOG1teaching ~ gender + educationmoma + PCE * ActivityLevel  , centered=c("PCE","ActivityLevel"), data=data)
slopedist=simpleSlope(mod5, pred="PCE", mod1="ActivityLevel") 
slopedist2=simpleSlope(mod5, pred="ActivityLevel", mod1="PCE")  
summary(slopedist) 
summary(slopedist2) 


 
##simple slope LOG1teaching PCE* AttenionFocus
mod5=lmres(LOG1teaching ~ gender + educationmoma + PCE * AttenionFocus  , centered=c("PCE","AttenionFocus"), data=data)
slopedist=simpleSlope(mod5, pred="PCE", mod1="AttenionFocus") 
slopedist2=simpleSlope(mod5, pred="AttenionFocus", mod1="PCE")  
summary(slopedist) 
summary(slopedist2) 



##simple slope LOG1teaching PCE* inhibitory2
mod5=lmres(LOG1teaching ~ gender + educationmoma + PCE * inhibitory2  , centered=c("PCE","inhibitory2"), data=data)
slopedist=simpleSlope(mod5, pred="PCE", mod1="inhibitory2") 
slopedist2=simpleSlope(mod5, pred="inhibitory2", mod1="PCE")  
summary(slopedist) 
summary(slopedist2) 


##simple slope LOG1pcenter PCE* LIP
mod5=lmres(LOG1pcenter ~ gender + educationmoma + PCE * LIP  , centered=c("PCE","LIP"), data=data)
slopedist=simpleSlope(mod5, pred="PCE", mod1="LIP") 
slopedist2=simpleSlope(mod5, pred="LIP", mod1="PCE")  
summary(slopedist) 
summary(slopedist2) 
