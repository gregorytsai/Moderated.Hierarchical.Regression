##################################################################################
##2016.2.26
##Author:GregTsai   Email:gregorytsai@gmail.com
##Graduate student at National Taiwan University, department of Psychology
##Developed and tested on R Ver.3.2.3 Windows
##################################################################################

##Install if you wish to read or save as Excel file
install.packages("openxlsx")  #Very Obvious
install.packages("dplyr")     #For data manipulation
install.packages("pequod")    #for simpleSlope
install.packages("foreign")   #For spss file import
##Load the package when you open R
library(openxlsx)
library(dplyr)
library(pequod)
library(foreign)



####Defined by user
##Import  your data
##Reading csv by choosing which one
dataRaw=read.csv(file.choose(), header=T) 
##Reading by assigning file names or path
dataRaw=read.csv("FinalDataPool.csv", header=T) 
dataRaw=read.xlsx(".xlsx")  
dataRaw = read.spss("data.sav", to.data.frame=TRUE, use.value.labels = F)


###First assigning which variables to run, changes the values and run these part
##Don't change but do run this line of code
par=list()
##Dependent Vars column, assign the same number if there's only 1 control
par$dependantVarStart=4   
par$dependantVarEnd=7
##Contro variables column
par$controlVarStart=1
par$controlVarEnd=3
##Predictor Variables column
par$predictorVarStart=10
par$predictorVarEnd=ncol(dataRaw)
##Moderator Variables column
par$moderatorVarStart=8
par$moderatorVarEnd=9
#Missing data:Use listwise(0) or Pairwise(1)
par$missingMethod=1
#Coded Data(Categorical, won't convert to z score)
#If there's coded data, type in the names of coded var. 
#eg. =c("Gender") or =c("f1","f2")...
#Otherwise type =c("")
par$coded=c("")
###End of assigning


###About the result:
#You can type result$step1, result$step2, result$step3 to see them in R
#The results will be automatically saved to working directory as 4 csv files
#Working directory is usually in your user document folder, use getwd() to check
#If you want to save as Excel, try the following code(Highly recommended, in APA format):
##Save as 4 sheet excel
##Needs to install Rtools: (Or Xcode on Mac? haven't test it)
##https://github.com/stan-dev/rstan/wiki/Install-Rtools-for-Windows
##Remember to select "Edit the system path" while installing
wb <- createWorkbook()
addWorksheet(wb = wb, sheetName = "Step1_Control Var.", gridLines = T)
writeData(wb = wb, sheet = 1, x = result$step1,colNames = F)
addWorksheet(wb = wb, sheetName = "Step2_Main Effect", gridLines = T)
writeData(wb = wb, sheet = 2, x = result$step2,colNames = F)
addWorksheet(wb = wb, sheetName = "Step3_Interaction", gridLines = T)
writeData(wb = wb, sheet = 3, x = result$step3,colNames = F)
addWorksheet(wb = wb, sheetName = "PostHoc_SimpleSlope", gridLines = T)
writeData(wb = wb, sheet = 4, x = result$simpleSlope,colNames = T)
saveWorkbook(wb, "2 Way Moderation with Simple Slope.xlsx",  overwrite = TRUE)
#End of saving Excel
#Considering combining three sheets, maybe later~!


#After assigning which variables to run, just execute all the codes below 



#######################Don't mess with these codes, just execute#######################################
#Select data and convert to z score
data=list()
data$Dependent <- dataRaw %>% dplyr::select_(
  paste(names(dataRaw)[par$dependantVarStart],names(dataRaw)[par$dependantVarEnd],sep=":") ) 
data$Control <- dataRaw %>% dplyr::select_(
  paste(names(dataRaw)[par$controlVarStart],names(dataRaw)[par$controlVarEnd],sep=":") )
data$Predictor <- dataRaw %>% dplyr::select_(
  paste(names(dataRaw)[par$predictorVarStart],names(dataRaw)[par$predictorVarEnd],sep=":") )
data$Moderator <- dataRaw %>% dplyr::select_(
  paste(names(dataRaw)[par$moderatorVarStart],names(dataRaw)[par$moderatorVarEnd],sep=":") )
data$All.Original=data.frame(data$Dependent,data$Control,data$Predictor,data$Moderator ) 
if(par$missingMethod==0){
  data$All.scaled=data.frame(scale(data$Dependent),scale(data$Control),scale(data$Predictor),scale(data$Moderator) ) %>%
    dplyr::filter(complete.cases(.))
}else{
  data$All.scaled=data.frame(scale(data$Dependent),scale(data$Control),scale(data$Predictor),scale(data$Moderator) ) 
}


#Checking coded var. and revert back to original score, kind of dumb but works
if (nchar(par$coded[1])>0){              #to see if user assigned any coded var.
  for (i in 1:length(par$coded)) {       #Revert every scaled coded var.
    #Check to see if the coded var. name can be found in the data
    if (length(grep(paste0("^",par$coded[i],"$"),names(data$All.scaled)))==0){
      print(paste0("[Error]Coded Var.",i," Not Found"))
    }else{
      data$All.scaled[,par$coded[i]]=data$All.Original[,par$coded[i]]
    }
  }
}


##Function: Calculate regression model p
lmp <- function (modelobject) 
{
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
} 

#Function: APA style formatting
##add significance stars and format to APA style
##maybe doing vectorized calculation in the future
addStar <- function(name,pvalue)
{
  if (name==0) {name=".00"}
  else if (name<1 & name>0) {name=gsub("^.*?\\.",".",format(name,nsmall=2))}
  else if (name<0 & name>-1) {name=paste("-",gsub("^.*?\\.",".",format(name,nsmall=2)),sep="")}
  else {name=format(name,nsmall=2)}
  if (pvalue<0.001) {nameStar=paste0(name,"***")}
  else if (pvalue<0.01) {nameStar=paste0(name,"**")}
  else if (pvalue<=0.05) {nameStar=paste0(name,"*")}
  else if (pvalue<0.10) {nameStar=paste0(name,"†")}
  else {nameStar=name}
  
  return(nameStar)
}

#Function: Simple Slope Test
simpleSlopeTest = function(result,data, i,j,k){
  result$simpleTemp=matrix(nrow=2,ncol=(ncol(result$simpleSlope)) )
  colnames(result$simpleTemp)=colnames(result$simpleSlope)
  result$simpleFit=lmres(formula=formulas$step3,
                         centered=c(names(data$Predictor[j]),names(data$Moderator[k])),
                         data=data$All.Original)
  result$simpleTest=
    simpleSlope(result$simpleFit,pred=names(data$Predictor[j]),mod1=names(data$Moderator[k]),coded=par$coded )
  result$simpleSummary=summary(result$simpleTest) 
  # PlotSlope(result$simpleTest) 
  result$simpleTemp[1,"Dep. Var."]=names(data$Dependent[i])
  result$simpleTemp[1,"Pred. Var."]=names(data$Predictor[j])
  result$simpleTemp[1,"Mod. Var."]=names(data$Moderator[k])
  for (l in 1:2){
    result$simpleTemp[l,"Mod.Group"]=
      ifelse(l==1,paste("Low",names(data$Moderator[k]),"(-1SD)"),paste("High",names(data$Moderator[k]),"(+1SD)")   )
    result$simpleTemp[l,"Simple Slope"]=
      result$simpleSummary$simple_slope[l,"simple slope"] %>% 
      round(.,2) %>% addStar(.,result$simpleSummary$simple_slope[l,"p.value"])
    result$simpleTemp[l,"SE"]=
      result$simpleSummary$simple_slope[l,"standard error"] %>%
      round(.,2) %>% addStar(.,1)
    result$simpleTemp[l,"t-Value"]=
      result$simpleSummary$simple_slope[l,"t-value"] %>%
      round(.,2) %>% addStar(.,1)
    result$simpleTemp[l,"p-Value"]=
      result$simpleSummary$simple_slope[l,"p.value"] %>%
      round(.,3) %>% format(.,nsmall=3) %>% gsub("^.*?\\.",".",.)
    result$simpleTemp[l,"Low Pred.Point"]=
      result$simpleSummary$Points[l,1] %>% 
      round(.,2) %>% format(.,nsmall=2)
    result$simpleTemp[l,"High Pred.Point"]=
      result$simpleSummary$Points[l,2] %>% 
      round(.,2) %>% format(.,nsmall=2)
  }
  result$simpleSlope=rbind(result$simpleSlope,result$simpleTemp)
  return(result)
}



#step 1 control var.
#set step 1 result format: dep, control1, control2,... , deltr R2, Adj R2, betas...
result=list()
result$step1=matrix(nrow=1,ncol=(ncol(data$Control)*2+7))
result$step1[1]="Dep. Var."
result$step1[ncol(data$Control)+2]="Pred. Var"
result$step1[ncol(data$Control)+3]="Mod. Var."
result$step1[ncol(data$Control)+4]="Delta R2"
result$step1[ncol(data$Control)+5]="Adj. R2"
result$step1[ncol(data$Control)+6]="F value"
result$step1[ncol(data$Control)+7]="d.f."
for (i in 1:ncol(data$Control)){
  result$step1[i+1]=paste("Control Var",i)
  result$step1[i+ncol(data$Control)+7]=paste("Control",i, "beta")
}

#Step2Result
result$step2=matrix(nrow=1,ncol=(ncol(data$Control)+9))
result$step2[,1:9]=c("Dep. Var.","Pred. Var.","Mod. Var.","Delta R2","Adj.R2","F value","d.f","Pred.Beta","Mod.Beta")
for (i in 1:ncol(data$Control)){
  result$step2[9+i]=paste("Control",i, "beta")
}

#Step3Result
result$step3=matrix(nrow=1,ncol=(ncol(data$Control)+10))
result$step3[,1:10]=c("Dep. Var.","Pred. Var.","Mod. Var.","Delta R2","Adj.R2","F value","d.f","Pred.Beta","Mod.Beta","Interaction b")
for (i in 1:ncol(data$Control)){
  result$step3[10+i]=paste("Control",i, "beta")
}

#Simple Slope Result(Post-Hoc)
result$simpleSlope=matrix(nrow=1,ncol=10)
colnames(result$simpleSlope)=c("Dep. Var.","Pred. Var.","Mod. Var.",
                               "Mod.Group","Simple Slope","SE",
                               "t-Value","p-Value","Low Pred.Point","High Pred.Point")


#Regression formulas in 3 steps
formulas=list()
formulas$controlNames=names(data$Control[1])
if(ncol(data$Control)>1){
  for (i in 2:ncol(data$Control)){
    formulas$controlNames=paste(formulas$controlNames,names(data$Control[i]),sep="+")
  }
}

#Usful for debugging, skip loop testing codes inside 
#i=1
#j=2
#k=3


#Starts Regressions
for (i in 1:ncol(data$Dependent)){
  for (j in 1:ncol(data$Predictor)){
    for (k in 1:ncol(data$Moderator)){
      #Check missing data handling method
      #If: using Pairwise-missing-delete(par$missingMethod==1), resample every loop combination
      #If: using listwise-missing-delete(par$missingMethod==0), alway use all filtered data
      if (par$missingMethod==1){
        data$inLoop=data$All.scaled %>% 
          dplyr::select_(
            paste(names(data$Control[1]),names(data$Control[ncol(data$Control)]),sep=":"),
            names(data$Dependent[i]),names(data$Predictor[j]),names(data$Moderator[k])
          ) %>% dplyr::filter(complete.cases(.))
      }else if (par$missingMethod==0){data$inLoop=data$All.scaled }
      
      #Run Step 1 Regression
      #Only run in 2 conditions:
      #1.Use Pairwise-missing-delete(par$missingMethod==1):
      #  run in every i,j,k due to different n in diff. combinations
      #2.listwise-missing-delete(par$missingMethod==0):
      #  only run when j==1 & k==1(new dep. var.)
      if ( (par$missingMethod==1) | (par$missingMethod==0 & j==1 & k==1) ){
        result$step1Temp=matrix(nrow=1,ncol=ncol(result$step1))       #Temp result ready to row bind to final result
        result$stpe1Regression=lm(formula=paste(names(data$Dependent[i]),formulas$controlNames,sep="~"),data=data$inLoop) #running regression, save for later model p and model compare calculation
        result$Step1Summary=summary(result$stpe1Regression) #Get regression summary, betas and ps.
        result$model1P=lmp(result$stpe1Regression)          #Get model p, because you can't get it in summary
        #Writing results, considering using column names in the future
        result$step1Temp[1]=names(data$Dependent[i])        #Writing Dependent Var. name
        result$step1Temp[ncol(data$Control)+2] =            #Writing Pred. Var. name only when par$missingMethod==1
          ifelse(par$missingMethod==1,names(data$Predictor[j]),NA) 
        result$step1Temp[ncol(data$Control)+3] =            #Writing Mod. Var. name only when par$missingMethod==1
          ifelse(par$missingMethod==1,names(data$Moderator[k]),NA) 
        result$step1Temp[ncol(data$Control)+4]=result$Step1Summary$r.squared %>% 
          round(.,2) %>%    addStar(.,1)                    #Writing delta R2
        result$step1Temp[ncol(data$Control)+5]=result$Step1Summary$adj.r.squared %>%  
          round(.,2) %>%    addStar(.,result$model1P)       #Writing Adj. R2
        result$step1Temp[ncol(data$Control)+6]=             #Writing F value
          result$Step1Summary$fstatistic[1] %>% round(.,2) %>% addStar(.,1)
        result$step1Temp[ncol(data$Control)+7]=             #Writing df
          paste(result$Step1Summary$fstatistic[2],result$Step1Summary$fstatistic[3],sep=",")
        #Then writing control betas
        for (l in 1:ncol(data$Control)){
          result$step1Temp[l+1]=names(data$Control[l])
          result$step1Temp[l+ncol(data$Control)+7]=
            result$Step1Summary$coefficients[names(data$Control[l]),"Estimate"] %>%
            round(.,2) %>%
            addStar(., result$Step1Summary$coefficients[names(data$Control[l]),"Pr(>|t|)"])
        }
        result$step1=rbind(result$step1,result$step1Temp)
      }
      
      
      #Step2
      result$step2Temp=matrix(nrow=1,ncol=ncol(result$step2)) #Temp result ready to row bind to final result
      formulas$step2=paste(names(data$Dependent[i]),"~",
                           formulas$controlNames,"+",
                           names(data$Predictor[j]),"+",
                           names(data$Moderator[k]))                #Step 2 regression formula
      result$stpe2Regression=lm(formula=formulas$step2,data=data$inLoop)
      result$step2Summary=summary(result$stpe2Regression)
      result$model2P=lmp(result$stpe2Regression)
      result$step2ModelCompare=anova(result$stpe1Regression, result$stpe2Regression)
      result$step2Temp[1]=names(data$Dependent[i])                  #Dependent Var. name
      result$step2Temp[2]=names(data$Predictor[j])                  #Predictor Var. name
      result$step2Temp[3]=names(data$Moderator[k])                  #Moderator Var. name
      result$step2Temp[4]=(result$step2Summary$r.squared-result$Step1Summary$r.squared) %>%            #delta R2
        round(.,2) %>% addStar(.,result$step2ModelCompare[2,"Pr(>F)"])
      result$step2Temp[5]=result$step2Summary$adj.r.squared %>% #Adj. R2
        round(.,2) %>% addStar(.,result$model2P)
      result$step2Temp[6]=             #Writing F value
        result$step2Summary$fstatistic[1] %>% round(.,2) %>% addStar(.,1)
      result$step2Temp[7]=             #Writing df
        paste(result$step2Summary$fstatistic[2],result$step2Summary$fstatistic[3],sep=",")
      result$step2Temp[8]=                                         #Predictor beta
        result$step2Summary$coefficients[names(data$Predictor[j]),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,result$step2Summary$coefficients[names(data$Predictor[j]),"Pr(>|t|)"])
      result$step2Temp[9]=                                         #Moderator beta
        result$step2Summary$coefficients[names(data$Moderator[k]),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,result$step2Summary$coefficients[names(data$Moderator[k]),"Pr(>|t|)"])
      for (l in 1:ncol(data$Control)) {
        result$step2Temp[9+l]= 
          result$step2Summary$coefficients[names(data$Control[l]),"Estimate"] %>%
          round(.,2) %>%
          addStar(.,result$step2Summary$coefficients[names(data$Control[l]),"Pr(>|t|)"])
      }
      result$step2=rbind(result$step2,result$step2Temp)
      
      #Step3
      result$step3Temp=matrix(nrow=1,ncol=ncol(result$step3) )  #Temp result ready to row bind to final result
      formulas$step3=paste(names(data$Dependent[i]),"~",
                           formulas$controlNames,"+",
                           names(data$Predictor[j]),"*",
                           names(data$Moderator[k]))
      result$stpe3Regression=lm(formula=formulas$step3,data=data$inLoop)
      result$step3Summary=summary(result$stpe3Regression)
      result$model3P=lmp(result$stpe3Regression)
      result$step3ModelCompare=anova(result$stpe2Regression, result$stpe3Regression)
      result$step3Temp[1]=names(data$Dependent[i])                  #Dependent Var. name
      result$step3Temp[2]=names(data$Predictor[j])                  #Predictor Var. name
      result$step3Temp[3]=names(data$Moderator[k])                  #Moderator Var. name
      result$step3Temp[4]=(result$step3Summary$r.squared-result$step2Summary$r.squared) %>%            #delta R2
        round(.,2) %>% addStar(.,result$step3ModelCompare[2,"Pr(>F)"])
      result$step3Temp[5]=result$step3Summary$adj.r.squared %>% #Adj. R2
        round(.,2) %>% addStar(.,result$model3P)
      result$step3Temp[6]=             #Writing F value
        result$step3Summary$fstatistic[1] %>% round(.,2) %>% addStar(.,1)
      result$step3Temp[7]=             #Writing df
        paste(result$step3Summary$fstatistic[2],result$step3Summary$fstatistic[3],sep=",")
      result$step3Temp[8]=                                         #Predictor beta
        result$step3Summary$coefficients[names(data$Predictor[j]),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,result$step3Summary$coefficients[names(data$Predictor[j]),"Pr(>|t|)"])
      result$step3Temp[9]=                                         #Moderator beta
        result$step3Summary$coefficients[names(data$Moderator[k]),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,result$step3Summary$coefficients[names(data$Moderator[k]),"Pr(>|t|)"])
      result$step3Temp[10]=                                         #Interaction beta
        result$step3Summary$coefficients[paste0(names(data$Predictor[j]),":",names(data$Moderator[k])),"Estimate"] %>%
        round(.,2) %>%
        addStar(.,result$step3Summary$coefficients[paste0(names(data$Predictor[j]),":",names(data$Moderator[k])),"Pr(>|t|)"])
      for (l in 1:ncol(data$Control)) {
        result$step3Temp[10+l]= 
          result$step3Summary$coefficients[names(data$Control[l]),"Estimate"] %>%
          round(.,2) %>%
          addStar(.,result$step3Summary$coefficients[names(data$Control[l]),"Pr(>|t|)"])
      }
      result$step3=rbind(result$step3,result$step3Temp)
      
      #Doing post-hoc using Simple Slope Analysis
      if (result$step3Summary$coefficients[paste0(names(data$Predictor[j]),":",names(data$Moderator[k])),"Pr(>|t|)"]<0.1) {
        result=simpleSlopeTest(result,data,i,j,k)
        dataReverse=data
        dataReverse$Predictor=data$Moderator
        dataReverse$Moderator=data$Predictor
        result=simpleSlopeTest(result,dataReverse,i,k,j)
       }
    }
  }
  ##Save as csv, after every predictor 
  write.csv(result$step1,"Step1.csv")
  write.csv(result$step2,"Step2.csv")
  write.csv(result$step3,"Step3.csv")
  write.csv(result$simpleSlope,"SimpleSlope.csv")
}



