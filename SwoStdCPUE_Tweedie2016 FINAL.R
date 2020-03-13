#library(NbClust)
library(tweedie)
library(statmod)
library(fpc)

ls()

rm(list=ls())



################################################################################
#Clustering for 'tactic' 

#MY COMPUTER CANNOT RUN NBClust WITH MORE THAN 6000 DATA POINTS WITHOUT CRASHING. 
#Number of clusters determined by prediction.strength. Clustering of samples conducted in Cluster 3.0
#Could not load fpc  here so did prediction.strength in older version of R

#WEST Full area (<20)

DatasetWFC<-read.table("C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\Clustering\\SwoStdCPUE_ClusterWestFull.csv", ,header=T,sep=",")


###Predict number of clusters. Look up the concept of this method. Ran this in R3.1.0 since have issues with installing fpc package
??prediction.strength
Data_catch<- DatasetWFC[,1:19]
prediction.strength(Data_catch, Gmin=2, Gmax=10, classification="centroid", M=3, cutoff=0.8)

#Result = 2 clusters. Still 2 if cutoff set at 0.7

#NBclust package

#nb1 <- NbClust(DatasetWFC, diss=NULL, distance = "euclidean", 
 #             min.nc=2, max.nc=15, method = "kmeans", 
  #            index = "all", alphaBeale = 0.1)
#nb1$All.index
#nb1$All.CriticalValues
#nb1$Best.nc
#nb1$Best.partition
#hist(nb1$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
# Looks like 3 is the most frequently determined number of clusters


#Add clustering values to dataset as a categorical factor
#tactic <- as.matrix(nb1$Best.partition)
#DatasetWF1 <- cbind(DatasetWFC, tactic)
#write.csv(DatasetWF1,"C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\SWO_ClusterWESTFullresult.csv")


#WEST Half area (<=15). 

DatasetWHC<-read.table("C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\Clustering\\SwoStdCPUE_ClusterWestHalf.csv", ,header=T,sep=",")

###Predict number of clusters. Look up the concept of this method
??prediction.strength
Data_catch<- DatasetWHC[,1:19]
prediction.strength(Data_catch, Gmin=2, Gmax=10, classification="centroid", M=3, cutoff=0.8)

#Result = 3 clusters. Still 3 if cutoff set to 0.7

#EAST Full area (>=20)

DatasetEFC<-read.table("C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\Clustering\\SwoStdCPUE_ClusterEastFull.csv", ,header=T,sep=",")

###Predict number of clusters. Look up the concept of this method
??prediction.strength
Data_catch<- DatasetEFC[,1:19]
prediction.strength(Data_catch, Gmin=2, Gmax=10, classification="centroid", M=3, cutoff=0.8)

#Result = 2 clusters. 2 if cutoff is 0.7


#EAST Half area (>=27)

DatasetEHC<-read.table("C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\Clustering\\SwoStdCPUE_ClusterEastHalf.csv", ,header=T,sep=",")

###Predict number of clusters. Look up the concept of this method
??prediction.strength
Data_catch<- DatasetEHC[,1:19]
prediction.strength(Data_catch, Gmin=2, Gmax=10, classification="centroid", M=3, cutoff=0.7) #Changed cutoff to 0.7 to get >1 cluster

#Result = 2 clusters



################################################################################

#WEST FULL input dataset with 'tactic'
DatasetWF<-read.table("C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\SwoStdCPUE_WESTFULLinput.csv", ,header=T,sep=",")
names(DatasetWF)
View(DatasetWF)

#To remove a column in your dataset: 
#Dataset.features = Dataset
#Dataset.features$tactic <- NULL


#https://www.zoology.ubc.ca/~schluter/R/fit-model/

#Variables as factors.
Dataset_finalWF<-within(DatasetWF,{
  YEAR<- factor(YEAR)
  SEASON<- factor(SEASON)
  LAT<- factor(LAT)
  LONG<- factor(LONG)
  LOA<- factor (LOA)
  #HOOK<- factor(HOOK)
  SST<- factor(SST)
  TACTIC<- factor(TACTIC)
})



#Maximum likelihood estimation of the Tweedie index parameter p
out<-with(Dataset_finalWF, tweedie.profile(SWOCPUEno~+1,p.vec=seq(1.1,1.9,by=0.1),link.power=0, method="inversion",do.ci=TRUE,do.smooth=TRUE,do.plot=TRUE)) 
out$p.max
out$ci

#p.max = 1.47551

#Nominal model. Null-model, provides Null-deviance
glmnullWF<- glm(SWOCPUEno~+1, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF) ##link.power=0 indicates log link function


#Full model
glmfullWF<- glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+
                  SEASON*LAT+SEASON*LONG+SEASON*SST+offset(log(HOOKnr)), 
                family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)



#Order of variables in full model. From highest to lowest AIC score
#glmfull.step<- step(glmfull) 


#AICtweedie. Compare models to determine which model explains most of the variation. it offers a relative 
#estimate of the information lost when a given model is used to represent the process that generates the data. 
#In doing so, it deals with the trade-off between the goodness of fit of the model and the complexity of the model

glmcomplexWF0<-glm(SWOCPUEno~YEAR, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF1<-glm(SWOCPUEno~YEAR+SEASON, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF2<-glm(SWOCPUEno~YEAR+SEASON+SST, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF3<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF4<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF5<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF6<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF7<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF8<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
#glmcomplexWF9<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
#glmcomplexWF10<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF11<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG+SEASON*SST, family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)
glmcomplexWF12<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG+SEASON*SST+offset(log(HOOKnr)), family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)


AICtweedie(glmnullWF)
AICtweedie(glmcomplexWF0)
AICtweedie(glmcomplexWF1)
AICtweedie(glmcomplexWF2)
AICtweedie(glmcomplexWF3)
AICtweedie(glmcomplexWF4)
AICtweedie(glmcomplexWF5)
AICtweedie(glmcomplexWF6)
AICtweedie(glmcomplexWF7)
AICtweedie(glmcomplexWF8)
#AICtweedie(glmcomplexWF9)
#AICtweedie(glmcomplexWF10)
AICtweedie(glmcomplexWF11)
AICtweedie(glmcomplexWF12)


#Based on AIC results, create the final model

glmcomplexWFFINAL<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*SST+offset(log(HOOKnr)), family=tweedie(var.power=1.47551,link.power=0), data=Dataset_finalWF)


#If AIC score between two models differs by >2, then the smaller AIC is the better model. 

#Analysis of deviance
anova(glmcomplexWFFINAL,test="Chisq")  ##Change in deviance significant if including parameters
#OR
#install.packages("modEvA", repos="http://R-Forge.R-project.org")
#Dsquared(model = glmcomplex5) #Provides final %deviance explained (as a proportion)

summary(glmcomplexWFFINAL)        #parameter estimates and overall model fit
plot(glmcomplexWFFINAL)           # plots of deviance residuals, q-q, leverage.

#coef(glmcomplex5)          # model coefficients
#resid(glmcomplex5)         # deviance residuals
#predict(glmcomplex5)       # !!predicted values on the transformed scale
#predict(glmcomplex5, se.fit = TRUE)   # Includes SE's of predicted values
#fitted(glmcomplex5)        # !!predicted values on the original scale

anova(glmnullWF, glmcomplexWFFINAL, test = "Chisq") # compare fits of 2 models, "reduced" vs "full"

#Histogram of residuals need to look normal

windows()
hist(resid(glmcomplexWFFINAL))

#QQ plot (Quantiles). Need to follow line. 

windows() ##opens new window for each graph
qqnorm(resid(glmcomplexWFFINAL), main="qqplot")
qqline(resid(glmcomplexWFFINAL))

##########################################################################################################################################################################
######The Standardised WEST FULL CPUE!!!!!
library(lsmeans)
#Least Squares Means (LSMeans), by year, takes all variables in the model and finds the average of the dependent variable (CPUE) for each year (by min. SS).
#Includes CI and SE

#Had to run this in lsmeanstest.R with cut data. Even 8GB of ram won't get this model to run!!!

#StdCPUEWF<- lsmeans(glmcomplexWFFINAL, c("YEAR"))
#StdCPUEWF

#library(MASS)
#confint(glmcomplex5, level = 0.95) # approximate 95% confidence intervals. CIs of the predicted?


######Nominal CPUE!!!!!!
#(Includes normalised)
NomCPUE<- lsmeans(glmcomplexWF0, c("YEAR"))
NomCPUE

####The code can stop running here##########
####The code can stop running here##########
####The code can stop running here##########
####The code can stop running here##########

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
#WEST HALF input dataset with 'tactic'
DatasetWH<-read.table("C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\SwoStdCPUE_WESTHALFinput.csv", ,header=T,sep=",")
names(DatasetWH)
View(DatasetWH)

#To remove a column in your dataset: 
#Dataset.features = Dataset
#Dataset.features$tactic <- NULL


#https://www.zoology.ubc.ca/~schluter/R/fit-model/

#Variables as factors.
Dataset_finalWH<-within(DatasetWH,{
  YEAR<- factor(YEAR)
  SEASON<- factor(SEASON)
  LAT<- factor(LAT)
  LONG<- factor(LONG)
  LOA<- factor (LOA)
  #HOOK<- factor(HOOK)
  SST<- factor(SST)
  TACTIC<- factor(TACTIC)
})



#Maximum likelihood estimation of the Tweedie index parameter p
out<-with(Dataset_finalWH, tweedie.profile(SWOCPUEno~+1,p.vec=seq(1.1,1.9,by=0.1),link.power=0, method="inversion",do.ci=TRUE,do.smooth=TRUE,do.plot=TRUE)) 
out$p.max
out$ci

#p.max = 1.442857 Update the following models with this figure

#Nominal model. Null-model, provides Null-deviance
glmnullWH<- glm(SWOCPUEno~+1, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH) ##link.power=0 indicates log link function


#Full model
glmfullWH<- glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+
                  SEASON*LAT+SEASON*LONG+SEASON*SST+offset(log(HOOKnr)), family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)

#Order of variables in full model. From highest to lowest AIC score
#glmfull.step<- step(glmfull) 


#AICtweedie. Compare models to determine which model explains most of the variation. it offers a relative 
#estimate of the information lost when a given model is used to represent the process that generates the data. 
#In doing so, it deals with the trade-off between the goodness of fit of the model and the complexity of the model

glmcomplexWH0<-glm(SWOCPUEno~YEAR, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH1<-glm(SWOCPUEno~YEAR+SEASON, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH2<-glm(SWOCPUEno~YEAR+SEASON+SST, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH3<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH4<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH5<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH6<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH7<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH8<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
#glmcomplexWH9<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
#glmcomplexWH10<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH11<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG+SEASON*SST, family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)
glmcomplexWH12<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG+SEASON*SST+offset(log(HOOKnr)), family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH)


AICtweedie(glmnullWH)
AICtweedie(glmcomplexWH0)
AICtweedie(glmcomplexWH1)
AICtweedie(glmcomplexWH2)
AICtweedie(glmcomplexWH3)
AICtweedie(glmcomplexWH4)
AICtweedie(glmcomplexWH5)
AICtweedie(glmcomplexWH6)
AICtweedie(glmcomplexWH7)
AICtweedie(glmcomplexWH8)
AICtweedie(glmcomplexWH9)
AICtweedie(glmcomplexWH10)
AICtweedie(glmcomplexWH11)
AICtweedie(glmcomplexWH12)


#Based on AIC results, create the final model

glmcomplexWHFINAL<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*SST+offset(log(HOOKnr)), family=tweedie(var.power=1.442857,link.power=0), data=Dataset_finalWH, maxit=100)


#If AIC score between two models differs by >2, then the smaller AIC is the better model. 

#Analysis of deviance
anova(glmcomplexWHFINAL,test="Chisq")  ##Change in deviance significant if including parameters
#OR
#install.packages("modEvA", repos="http://R-Forge.R-project.org")
#Dsquared(model = glmcomplex5) #Provides final %deviance explained (as a proportion)

summary(glmcomplexWHFINAL)        #parameter estimates and overall model fit
plot(glmcomplexWHFINAL)           # plots of deviance residuals, q-q, leverage.
#coef(glmcomplex5)          # model coefficients
#resid(glmcomplex5)         # deviance residuals
#predict(glmcomplex5)       # !!predicted values on the transformed scale
#predict(glmcomplex5, se.fit = TRUE)   # Includes SE's of predicted values
#fitted(glmcomplex5)        # !!predicted values on the original scale

anova(glmnullWH, glmcomplexWHFINAL, test = "Chisq") # compare fits of 2 models, "reduced" vs "full"

#Histogram of residuals need to look normal

windows()
hist(resid(glmcomplexWHFINAL))

#QQ plot (Quantiles). Need to follow line. 

windows() ##opens new window for each graph
qqnorm(resid(glmcomplexWHFINAL), main="qqplot")
qqline(resid(glmcomplexWHFINAL))

##########################################################################################################################################################################
######The Standardised WEST HALF CPUE!!!!!
library(lsmeans)
#Least Squares Means (LSMeans), by year, takes all variables in the model and finds the average of the dependent variable (CPUE) for each year (by min. SS).

#The full model with more than 3000 sets and with the interaction terms cannot be run with 8GB RAM. 
#CUT model is in "lsmeanstest.R"

#Includes CI and SE
StdCPUEWH<- lsmeans(glmcomplexWHFINAL, c("YEAR"))
StdCPUEWH

#library(MASS)
#confint(glmcomplex5, level = 0.95) # approximate 95% confidence intervals. CIs of the predicted?


######Nominal CPUE!!!!!!
#(Includes normalised)
NomCPUE<- lsmeans(glmcomplexWH0, c("YEAR"))
NomCPUE

####The code can stop running here##########
####The code can stop running here##########
####The code can stop running here##########
####The code can stop running here##########
############################################################################################################################################

#EAST FULL input dataset with 'tactic'
DatasetEF<-read.table("C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\SwoStdCPUE_EASTFULLinput.csv", ,header=T,sep=",")
names(DatasetEF)
View(DatasetEF)

#To remove a column in your dataset: 
#Dataset.features = Dataset
#Dataset.features$tactic <- NULL


#https://www.zoology.ubc.ca/~schluter/R/fit-model/

#Variables as factors.
Dataset_finalEF<-within(DatasetEF,{
  YEAR<- factor(YEAR)
  SEASON<- factor(SEASON)
  LAT<- factor(LAT)
  LONG<- factor(LONG)
  LOA<- factor (LOA)
  #HOOK<- factor(HOOK)
  SST<- factor(SST)
  TACTIC<- factor(TACTIC)
})



#Maximum likelihood estimation of the Tweedie index parameter p
out<-with(Dataset_finalEF, tweedie.profile(SWOCPUEno~+1,p.vec=seq(1.1,1.9,by=0.1),link.power=0, method="inversion",do.ci=TRUE,do.smooth=TRUE,do.plot=TRUE)) 
out$p.max
out$ci

#p.max = 1.542857

#Nominal model. Null-model, provides Null-deviance
glmnullEF<- glm(SWOCPUEno~+1, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF) ##link.power=0 indicates log link function


#Full model
glmfullEF<- glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+VESSEL+LOA+offset(log(HOOKnr)), family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)

#Order of variables in full model. From highest to lowest AIC score
#glmfull.step<- step(glmfull) 


#AICtweedie. Compare models to determine which model explains most of the variation. it offers a relative 
#estimate of the information lost when a given model is used to represent the process that generates the data. 
#In doing so, it deals with the trade-off between the goodness of fit of the model and the complexity of the model
glmcomplexEF0<-glm(SWOCPUEno~YEAR, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF1<-glm(SWOCPUEno~YEAR+SEASON, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF2<-glm(SWOCPUEno~YEAR+SEASON+SST, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF3<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF4<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF5<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF6<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF7<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF8<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
#glmcomplexEF9<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
#glmcomplexEF10<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF11<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG+SEASON*SST, family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)
glmcomplexEF12<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG+SEASON*SST+offset(log(HOOKnr)), family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)


AICtweedie(glmnullEF)
AICtweedie(glmcomplexEF0)
AICtweedie(glmcomplexEF1)
AICtweedie(glmcomplexEF2)
AICtweedie(glmcomplexEF3)
AICtweedie(glmcomplexEF4)
AICtweedie(glmcomplexEF5)
AICtweedie(glmcomplexEF6)
AICtweedie(glmcomplexEF7)
AICtweedie(glmcomplexEF8)
AICtweedie(glmcomplexEF9)
AICtweedie(glmcomplexEF10)
AICtweedie(glmcomplexEF11)
AICtweedie(glmcomplexEF12)


#Based on AIC results, create the final model

glmcomplexEFFINAL<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LAT+SEASON*LONG+SEASON*SST+offset(log(HOOKnr)), family=tweedie(var.power=1.542857,link.power=0), data=Dataset_finalEF)

#If AIC score between two models differs by >2, then the smaller AIC is the better model. 

#Analysis of deviance
anova(glmcomplexEFFINAL,test="Chisq")  ##Change in deviance significant if including parameters
#OR
#install.packages("modEvA", repos="http://R-Forge.R-project.org")
#Dsquared(model = glmcomplex5) #Provides final %deviance explained (as a proportion)

summary(glmcomplexEFFINAL)        #parameter estimates and overall model fit
plot(glmcomplexEFFINAL)           # plots of deviance residuals, q-q, leverage.
#coef(glmcomplex5)          # model coefficients
#resid(glmcomplex5)         # deviance residuals
#predict(glmcomplex5)       # !!predicted values on the transformed scale
#predict(glmcomplex5, se.fit = TRUE)   # Includes SE's of predicted values
#fitted(glmcomplex5)        # !!predicted values on the original scale

anova(glmnullEF, glmcomplexEFFINAL, test = "Chisq") # compare fits of 2 models, "reduced" vs "full"

#Histogram of residuals need to look normal

windows()
hist(resid(glmcomplexEFFINAL))

#QQ plot (Quantiles). Need to follow line. 

windows() ##opens new window for each graph
qqnorm(resid(glmcomplexEFFINAL), main="qqplot")
qqline(resid(glmcomplexEFFINAL))

##########################################################################################################################################################################
######The Standardised EAST FULL CPUE!!!!!
library(lsmeans)
#Least Squares Means (LSMeans), by year, takes all variables in the model and finds the average of the dependent variable (CPUE) for each year (by min. SS).
#Includes CI and SE

#Had to run this in lsmeanstest.R with cut data. Even 8GB of ram won't get this model to run!!!

StdCPUE<- lsmeans(glmcomplexEFFINAL, c("YEAR"))
StdCPUE

#library(MASS)
#confint(glmcomplex5, level = 0.95) # approximate 95% confidence intervals. CIs of the predicted?


######Nominal CPUE!!!!!!
#(Includes normalised)
NomCPUE<- lsmeans(glmcomplexEF0, c("YEAR"))
NomCPUE

####The code can stop running here##########
####The code can stop running here##########
####The code can stop running here##########
####The code can stop running here##########

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
#EAST HALF input dataset with 'tactic'
DatasetEH<-read.table("C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\SwoStdCPUE_EASTHALFinput.csv", ,header=T,sep=",")
names(DatasetEH)
View(DatasetEH)

#To remove a column in your dataset: 
#Dataset.features = Dataset
#Dataset.features$tactic <- NULL


#https://www.zoology.ubc.ca/~schluter/R/fit-model/

#Variables as factors.
Dataset_finalEH<-within(DatasetEH,{
  YEAR<- factor(YEAR)
  SEASON<- factor(SEASON)
  LAT<- factor(LAT)
  LONG<- factor(LONG)
  LOA<- factor (LOA)
  #HOOK<- factor(HOOK)
  SST<- factor(SST)
  TACTIC<- factor(TACTIC)
})



#Maximum likelihood estimation of the Tweedie index parameter p
out<-with(Dataset_finalEH, tweedie.profile(SWOCPUEno~+1,p.vec=seq(1.1,1.9,by=0.1),link.power=0, method="inversion",do.ci=TRUE,do.smooth=TRUE,do.plot=TRUE)) 
out$p.max
out$ci

#p.max = 1.459184

#Nominal model. Null-model, provides Null-deviance
glmnullEH<- glm(SWOCPUEno~+1, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH) ##link.power=0 indicates log link function


#Full model
glmfullEH<- glm(SWOCPUEnr~YEAR+SEASON+SST+TACTIC+VESSEL+LOA+offset(log(HOOKnr)), family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)

#Order of variables in full model. From highest to lowest AIC score
#glmfull.step<- step(glmfull) 


#AICtweedie. Compare models to determine which model explains most of the variation. it offers a relative 
#estimate of the information lost when a given model is used to represent the process that generates the data. 
#In doing so, it deals with the trade-off between the goodness of fit of the model and the complexity of the model
glmcomplexEH0<-glm(SWOCPUEno~YEAR, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH1<-glm(SWOCPUEno~YEAR+SEASON, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH2<-glm(SWOCPUEno~YEAR+SEASON+SST, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH3<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH4<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH5<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH6<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH7<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH8<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
#glmcomplexEH10<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LONG, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH11<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LONG+SEASON*SST, family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)
glmcomplexEH12<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*LONG+SEASON*SST+offset(log(HOOKnr)), family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)


AICtweedie(glmnullEH)
AICtweedie(glmcomplexEH0)
AICtweedie(glmcomplexEH1)
AICtweedie(glmcomplexEH2)
AICtweedie(glmcomplexEH3)
AICtweedie(glmcomplexEH4)
AICtweedie(glmcomplexEH5)
AICtweedie(glmcomplexEH6)
AICtweedie(glmcomplexEH7)
AICtweedie(glmcomplexEH8)
#AICtweedie(glmcomplexEH9)
#AICtweedie(glmcomplexEH10)
AICtweedie(glmcomplexEH11)
AICtweedie(glmcomplexEH12)


#Based on AIC results, create the final model

glmcomplexEHFINAL<-glm(SWOCPUEno~YEAR+SEASON+SST+TACTIC+LOA+LAT+LONG+LOA*LAT+LOA*LONG+SEASON*SST+offset(log(HOOKnr)), family=tweedie(var.power=1.459184,link.power=0), data=Dataset_finalEH)


#If AIC score between two models differs by >2, then the smaller AIC is the better model. 

#Analysis of deviance
anova(glmcomplexEHFINAL,test="Chisq")  ##Change in deviance significant if including parameters
#OR
#install.packages("modEvA", repos="http://R-Forge.R-project.org")
#Dsquared(model = glmcomplex5) #Provides final %deviance explained (as a proportion)

summary(glmcomplexEHFINAL)        #parameter estimates and overall model fit
plot(glmcomplexEHFINAL)           # plots of deviance residuals, q-q, leverage.
#coef(glmcomplex5)          # model coefficients
#resid(glmcomplex5)         # deviance residuals
#predict(glmcomplex5)       # !!predicted values on the transformed scale
#predict(glmcomplex5, se.fit = TRUE)   # Includes SE's of predicted values
#fitted(glmcomplex5)        # !!predicted values on the original scale

anova(glmnullEH, glmcomplexEHFINAL, test = "Chisq") # compare fits of 2 models, "reduced" vs "full"

#Histogram of residuals need to look normal

windows()
hist(resid(glmcomplexEHFINAL))

#QQ plot (Quantiles). Need to follow line. 

windows() ##opens new window for each graph
qqnorm(resid(glmcomplexEHFINAL), main="qqplot")
qqline(resid(glmcomplexEHFINAL))

##########################################################################################################################################################################
######The Standardised EAST HALF CPUE!!!!!
library(lsmeans)
#Least Squares Means (LSMeans), by year, takes all variables in the model and finds the average of the dependent variable (CPUE) for each year (by min. SS).
#Includes CI and SE


#Had to run this in lsmeanstest.R with cut data. Even 8GB of ram won't get this model to run!!!

StdCPUEEH<- lsmeans(glmcomplexEHFINAL, c("YEAR"))
StdCPUEEH

#library(MASS)
#confint(glmcomplex5, level = 0.95) # approximate 95% confidence intervals. CIs of the predicted?


######Nominal CPUE!!!!!!
#(Includes normalised)
NomCPUE<- lsmeans(glmcomplexEH0, c("YEAR"))
NomCPUE

####The code can stop running here##########
####The code can stop running here##########
####The code can stop running here##########
####The code can stop running here##########










results <- data.frame(cbind(predict_dataset,CPUE=mCPUE,lcl=lCPUE,ucl=uCPUE,nominal=nom,lnCPUE,SE)) #Adjust to fit this code

results.norm <- data.frame(cbind(predict_dataset,nominal, CPUE=normCPUE,lcl=normLCI,ucl=normUCI)) #Adjust to fit this code

write.csv(results.norm,"C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\SWO_normalizedCPUEWEST.csv")
write.csv(results,"C:\\Users\\WendyW\\Desktop\\Masters_25Sep14\\Swordfish standardised cpue 2013\\2015\\SWO_CPUEWEST.csv")



##########################################################################################################################################################################
####Standardised from Henning code. Do not understand. Used above code to standardise

#zhat <- predict(z, se.fit = TRUE)       # result on logit or log scale

zhatfitted<- fitted(z)                  #predicted values on the original scale
zupper <- zhat$fit + 1.96 * zhat$se.fit #Also CIs? Same as confint? 
zlower <- zhat$fit - 1.96 * zhat$se.fit #Also CIs? Same as confint?

#Then convert to the original scale.Need this code*****
#yupper <- exp(zupper)                   # for log link
#ylower <- exp(zlower)

#How do I plot zhat, yupper and ylower. Plot in Excel? And plot null model: plot(SWOCPUEnr~YEAR, data=...)? Or also in Excel

####Nominal = average per year?

####Normalised
normCPUE <- mCPUE/exp(mean(log(mCPUE))) #predicted (zhatfitted) divided by the mean of fitted
normLCI <- lCPUE/exp(mean(log(mCPUE)))  #fitted upper (zupper) divided by the mean of fitted
normUCI <- uCPUE/exp(mean(log(mCPUE)))  #predicted lower (zlower) divided by the mean of fitted











