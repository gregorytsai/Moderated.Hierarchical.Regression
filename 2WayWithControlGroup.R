##################################################################################
##2016.2.25
##Author:GregTsai   Email:gregorytsai@gmail.com
##Graduate student at National Taiwan University, department of Psychology
##Developed and tested on R Ver.3.2.3 Windows
##################################################################################

##Install if you wish to read or save as Excel file
install.packages("openxlsx")
install.packages("dplyr")
##Load the package when you open R
library(openxlsx)
library(dplyr)



####Defined by user
##Import  your data
##Reading csv by choosing which one
data=read.csv(file.choose(), header=T) 
##Reading by assigning file names or path
data=read.csv("FinalDataPool.csv", header=T) 
data=read.xlsx(".xlsx")  

###First assigning which variables to run, changes the values and run these part
##Dependent Vars
dependantVarStart=18   
dependantVarEnd=20 
##Contro variable
controlVarStart=3
controlVarEnd=4
##Predictor Variables
predictorVarStart=5
predictorVarEnd=7
##Moderator Variables
moderatorVarStart=8
moderatorVarEnd=17
###End of assigning

###About the result:
#You can type step1Result, step2Result, step3Result to see them in R
#The results will be automatically saved to working directory as 3 csv files
#Working directory is usually in your user document folder
#If you're not sure of where it is, use getwd() to check
#If you want to save as Excel, try the following code:
##Save as 3 sheet excel
##Needs to install Rtools: 
##https://github.com/stan-dev/rstan/wiki/Install-Rtools-for-Windows
##Remember to select "Edit the system path" while installing
wb <- createWorkbook()
addWorksheet(wb = wb, sheetName = "Step1: Control Var.", gridLines = FALSE)
writeData(wb = wb, sheet = 1, x = step1Result)
addWorksheet(wb = wb, sheetName = "Step2: Main Effect", gridLines = FALSE)
writeData(wb = wb, sheet = 2, x = step2Result)
addWorksheet(wb = wb, sheetName = "Step3: Interaction", gridLines = FALSE)
writeData(wb = wb, sheet = 3, x = step3Result)
saveWorkbook(wb, "Moderated stepwise regression.xlsx",  overwrite = TRUE)
#End of saving Excel


#After assigning which variables to run, just execute all the codes below 



#######################Don't mess with these codes, just execute#######################################
dataDependent <- data %>% dplyr::select_(
  paste(names(data)[dependantVarStart],names(data)[dependantVarEnd],sep=":") )
dataControl <- data %>% dplyr::select_(
  paste(names(data)[controlVarStart],names(data)[controlVarEnd],sep=":") )
dataPredictor <- data %>% dplyr::select_(
  paste(names(data)[predictorVarStart],names(data)[predictorVarEnd],sep=":") )
dataModerator <- data %>% dplyr::select_(
  paste(names(data)[moderatorVarStart],names(data)[moderatorVarEnd],sep=":") )
dataAll=data.frame(dataDependent,scale(dataControl),scale(dataPredictor),scale(dataModerator) ) %>%
  dplyr::filter(complete.cases(.))

##Function: Calculate regression model p
lmp <- function (modelobject) 
{
	if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
	f <- summary(modelobject)$fstatistic
	p <- pf(f[1],f[2],f[3],lower.tail=F)
	attributes(p) <- NULL
	return(p)
} 

##add significance stars, maybe doing vectorized calculation in the future
addStar <- function(name,pvalue)
{
	if (pvalue<0.001) {nameStar=paste0(name,"***")}
	else if (pvalue<0.01) {nameStar=paste0(name,"**")}
	else if (pvalue<=0.05) {nameStar=paste0(name,"*")}
	else if (pvalue<0.10) {nameStar=paste0(name,"†")}
	else {nameStar=name}

	return(nameStar)
}


#step 1 control var.
#set step 1 result format: dep, control1, control2,... , deltr R2, Adj R2, betas...
step1Result=matrix(nrow=1,ncol=(ncol(dataControl)*2+3))
step1Result[1]="Dep. Var."
step1Result[ncol(dataControl)+2]="Delta R2"
step1Result[ncol(dataControl)+3]="Adj. R2"
for (i in 1:ncol(dataControl)){
  step1Result[i+1]=paste("Control Var",i)
  step1Result[i+ncol(dataControl)+3]=paste("Control",i, "beta")
}
controlRegNames=names(dataControl[1])
if(ncol(dataControl)>1){
  for (i in 2:ncol(dataControl)){
    controlRegNames=paste(controlRegNames,names(dataControl[i]),sep="+")
  }
}

#Step2Result
step2Result=matrix(nrow=1,ncol=7)
step2Result=c("Dep. Var.","Pred. Var.","Mod. Var.","Delta R2","Adj.R2","Pred.Beta","Mod.Beta")

#Step3Result
step3Result=matrix(nrow=1,ncol=8)
step3Result=c("Dep. Var.","Pred. Var.","Mod. Var.","Delta R2","Adj.R2","Pred.Beta","Mod.Beta","Interaction b")

# i=1

#Starts Regressions
for (i in 1:ncol(dataPredictor)){
  ##Step 1
  result1Temp=matrix(nrow=1,ncol=ncol(step1Result))
  stpe1Regression=lm(formula=paste(names(dataDependent[i]),controlRegNames,sep="~"),data=dataAll)
  Step1Summary=summary(stpe1Regression)
  model1P=lmp(stpe1Regression)
  result1Temp[1]=names(dataDependent[i])                  #Dependent Var. name
  result1Temp[ncol(dataControl)+2]=Step1Summary$r.squared %>% round(.,2) #delta R2
  result1Temp[ncol(dataControl)+3]=Step1Summary$adj.r.squared %>%  #Adj. R2
    round(.,2) %>%
    addStar(.,model1P)
  for (j in 1:ncol(dataControl)){
    result1Temp[j+1]=names(dataControl[j])
    result1Temp[j+ncol(dataControl)+3]=
      Step1Summary$coefficients[names(dataControl[j]),"Estimate"] %>%
      round(.,2) %>%
      addStar(., Step1Summary$coefficients[names(dataControl[j]),"Pr(>|t|)"])
  }
  step1Result=rbind(step1Result,result1Temp)
  
  #Debugging
  # j=1
  # k=1
  
  #Step2 & 3
  for (j in 1:ncol(dataPredictor)){
    for (k in 1:ncol(dataModerator)){
      #Step2
      result2Temp=matrix(nrow=1,ncol=7)
      step2Formula=paste(names(dataDependent[i]),"~",
                         controlRegNames,"+",
                         names(dataPredictor[j]),"+",
                         names(dataModerator[k]))
      stpe2Regression=lm(formula=step2Formula,data=dataAll)
      Step2Summary=summary(stpe2Regression)
      model2P=lmp(stpe2Regression)
      step2ModelCompare=modelCompare(stpe1Regression, stpe2Regression)
      result2Temp[1]=names(dataDependent[i])                  #Dependent Var. name
      result2Temp[2]=names(dataPredictor[i])                  #Predictor Var. name
      result2Temp[3]=names(dataModerator[i])                  #Moderator Var. name
      result2Temp[4]=step2ModelCompare$DeltaR2 %>%            #delta R2
        round(.,2) %>% addStar(.,step2ModelCompare$p)
      result2Temp[5]=Step2Summary$adj.r.squared %>% #Adj. R2
        round(.,2) %>% addStar(.,model2P)
      result2Temp[6]=                                         #Predictor beta
        Step2Summary$coefficients[names(dataPredictor[j]),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,Step2Summary$coefficients[names(dataPredictor[j]),"Pr(>|t|)"])
      result2Temp[7]=                                         #Moderator beta
        Step2Summary$coefficients[names(dataModerator[k]),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,Step2Summary$coefficients[names(dataModerator[k]),"Pr(>|t|)"])
      step2Result=rbind(step2Result,result2Temp)
      
      #Step3
      result3Temp=matrix(nrow=1,ncol=8)
      step3Formula=paste(names(dataDependent[i]),"~",
                         controlRegNames,"+",
                         names(dataPredictor[j]),"*",
                         names(dataModerator[k]))
      stpe3Regression=lm(formula=step3Formula,data=dataAll)
      Step3Summary=summary(stpe3Regression)
      model3P=lmp(stpe3Regression)
      step3ModelCompare=modelCompare(stpe2Regression, stpe3Regression)
      result3Temp[1]=names(dataDependent[i])                  #Dependent Var. name
      result3Temp[2]=names(dataPredictor[i])                  #Predictor Var. name
      result3Temp[3]=names(dataModerator[i])                  #Moderator Var. name
      result3Temp[4]=step3ModelCompare$DeltaR2 %>%            #delta R2
        round(.,2) %>% addStar(.,step3ModelCompare$p)
      result3Temp[5]=Step3Summary$adj.r.squared %>% #Adj. R2
        round(.,2) %>% addStar(.,model3P)
      result3Temp[6]=                                         #Predictor beta
        Step3Summary$coefficients[names(dataPredictor[j]),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,Step3Summary$coefficients[names(dataPredictor[j]),"Pr(>|t|)"])
      result3Temp[7]=                                         #Moderator beta
        Step3Summary$coefficients[names(dataModerator[k]),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,Step3Summary$coefficients[names(dataModerator[k]),"Pr(>|t|)"])
      result3Temp[8]=                                         #Interaction beta
        Step3Summary$coefficients[paste0(names(dataPredictor[j]),":",names(dataModerator[k])),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,Step3Summary$coefficients[paste0(names(dataPredictor[j]),":",names(dataModerator[k])),"Pr(>|t|)"])
      
      step3Result=rbind(step3Result,result3Temp)
      
    }
  }
  ##Save as csv, after every predictor 
  write.csv(step1Result,"Step1.csv")
  write.csv(step2Result,"Step2.csv")
  write.csv(step3Result,"Step3.csv")
}















