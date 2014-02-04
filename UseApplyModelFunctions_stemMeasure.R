# General Code for applying shrub nutrient models
# Try to avoid R-specific functions

library(spatstat)
library(maptools)


setwd( "C:/Users/elm26/Documents/SERDPproject")
source("SoilProbeSampling/CompassFunctions")
source("SoilProbeSampling/RasterGeometry")
source("SoilProbeSampling/ApplyModelFunctions_stemMeasure.R")


# PICK A DESERT
#DESERT="Sonoran"
DESERT="Mojave"


##########################################################
# Build Null Grid ------

if (DESERT=="Sonoran") { Xmin=327515.0; Xmax=327598.0; Ymin=3619121.0; Ymax=3619251.0 }
if (DESERT=="Mojave")  {  Xmin=510386; Xmax=510516; Ymin=3890308; Ymax=3890391 }
xcrds=seq(from=Xmin, to=Xmax, by=0.2)
ycrds=seq(from=Ymin, to=Ymax, by=0.2)

#windows(11,7)
windows(9,11)

xcrds=seq(from=Xmin, to=Xmax, by=0.2)
ycrds=seq(from=Ymin, to=Ymax, by=0.2)

GPredNut=matrix(data=NA, ncol=length(xcrds), nrow=length(ycrds))
GPredNut.im=im(GPredNut, xcrds, ycrds, unitname=c("meter","meters"))

GPredDens=matrix(data=NA, ncol=length(xcrds), nrow=length(ycrds))
GPredDens=im(GPredDens, xcrds, ycrds, unitname=c("meter","meters"))

##########################################################
# Get Shrub locations-------------
if (DESERT=="Sonoran") ThisShrubData=read.csv("ShrubSpatialAnalysis_Gibbs/AZLarreaVolume_Complete.csv",header=TRUE)
if (DESERT=="Mojave") ThisShrubData=read.csv("ShrubSpatialAnalysis_Gibbs/CALarreaVolume.csv",header=TRUE)

 ThisShrubData$Area_Bot = ThisShrubData$Area_Bot/1000
 ThisShrubData$Area_Top =   ThisShrubData$Area_Top/1000
 ThisShrubData$Area_Stem =   ThisShrubData$Area_Stem/1000
 ThisShrubData$Vol_Bot    =  ThisShrubData$Vol_Bot/100000  
 ThisShrubData$Vol_Stem    =  ThisShrubData$Vol_Stem/100000 
 ThisShrubData$Vol_Cent    =  ThisShrubData$Vol_Cent/100000  
 ThisShrubData$Vol_BotRing =   ThisShrubData$Vol_BotRing/100000 
 ThisShrubData$Vol_StemRing =  ThisShrubData$Vol_StemRing/100000 

#IntSpcParams=read.csv("SoilProbeSampling/InterspaceDistributionParameters.csv")


# Set up allometric relations between Area_Bot and Area_Stem
# For each desert, Area_Stem=a*Area_Bot^b
#Allom=data.frame(Desert=c("Mojave","Sonoran"), a=c(0.06050726, 0.04812063), b=c(1.102207 ,1.043064 ))
#ThisAllometry=Allom[Allom$Desert==DESERT,]
# Get Observed Nutrient Values

if (DESERT=="Mojave") AllNutData=read.table("SoilProbeSampling/PCA/MojavePCresults.txt", header=T)
if (DESERT=="Sonoran") AllNutData=read.table("SoilProbeSampling/PCA/SonoranPCresults.txt", header=T)



# PICK ONE NUTRIENT AT A TIME.  Pick the same name as the column heading
NUTRIENT="Total.N"

NutData=AllNutData[,c("ShrubNo", "Region", "Size",  "Direction", "Area_Bot", "SampleID", "Distance", 
			#"X.calc", "Y.calc", 		#Do we need this?  Do we ever use this?
			 NUTRIENT)]
names(NutData)[ncol(NutData)]="Nutrient"
#Shoudl we flip a PC axis? 
#NutData$Nutrient=-NutData$Nutrient


### Negative Exponential Models--------------
#PICK MODEL TO USE- Must match desert and nutrient
DESERT
NUTRIENT
ModelResults=read.csv("SoilProbeSampling/NegExpModelResults.csv",header=T)
ModelResults[, 1:5]
whichmodel=18			# 1 for SonK, 2 for SonN, 5 for MojN, 7 for MojK, 10 forMojPC1
ThisModel=ModelResults[whichmodel,]
ThisModel
ThisIntSpPars=subset(IntSpcParams, (Desert==DESERT)&( Nutrient==NUTRIENT))


Map.im=ApplyNegExpModel(ThisModel, ThisShrubData, ThisIntSpPars, GPredNut.im)

save(SonoranN1.im, file="SoilProbeSampling/SonoranN1imageObject")
cols=gray(seq(1,0,length.out=700))
windows(9,9)
plot(Map.im,col=cols,main=paste("estimated map for ", NUTRIENT, " in the ", DESERT, " Model ", ThisModel$Model))


### Negative Exponential QP Models--------
#PICK MODEL TO USE- Must match desert and nutrient
DESERT
NUTRIENT
ModelResults=read.csv("SoilProbeSampling/NegExpModelResults_QuadraticParameters.csv",header=T)
ModelResults[, 1:5]
whichmodel=2			# 1 for SonK, 2 for SonN, 5 for MojN, 7 for MojK, 10 forMojPC1
ThisModel=ModelResults[whichmodel,]
ThisModel
ThisIntSpPars=subset(IntSpcParams, (Desert==DESERT)&( Nutrient==NUTRIENT))

Result=ApplyNegExpModel(ThisModel, ThisShrubData, ThisIntSpPars, GPredNut.im)
#Result=ApplyNegExpQPModel(ThisModel, ThisShrubData, ThisIntSpPars, GPredNut.im)
Map.im=Result$image
SampleShrub=Result$table
#save(SonoranN1.im, file="SoilProbeSampling/SonoranN1imageObject")
cols=gray(seq(1,0,length.out=700))
#windows(9,9)
windows(7,5)
plot(Map.im,col=cols,main=paste("estimated map for ", NUTRIENT, " in the ", DESERT, " Model ", ThisModel$Model))
#rect(510490, 3890350, 510510, 3890370)
#sub.owin=owin(c(510490, 510510), c(3890350, 3890370))
#plot(Map.im[sub.owin],col=cols,main=paste("estimated map for ", NUTRIENT, " in the ", DESERT, " Model ", ThisModel$Model))


source("./VegSpatialModeling/PlotTransectGraphs.R")

### Show decay curves for these shrubs -----------
#windows(7,4) 
windows()
with(NutData, plot(Distance, Nutrient, pch=NA))
with(NutData, plot(Distance, Nutrient))
abline(h=BGcut, lty=2)
for (i in 1:nrow(SampleShrub)){
	new.x=1:500
	new.y.n=SampleShrub$a.n[i]*exp(-SampleShrub$b.n[i]*new.x)+SampleShrub$d.n[i]
	new.y.s=SampleShrub$a.s[i]*exp(-SampleShrub$b.s[i]*new.x)+SampleShrub$d.s[i]
	lines(new.x, new.y.n, lwd=3,col=rainbow(max(SampleShrub$Area_Bot/10000))[SampleShrub$Area_Bot[i]/10000])
	lines(new.x, new.y.s, lwd=1,col=rainbow(max(SampleShrub$Area_Bot/10000))[SampleShrub$Area_Bot[i]/10000])
	#Sys.sleep(1)
}
title(paste("Decay Curves for", NUTRIENT, " in the ", DESERT, " Model ", ThisModel$Model))

# Linear Models--------
#PICK MODEL TO USE- Must match desert and nutrient
DESERT
NUTRIENT
ModelResults=read.csv("SoilProbeSampling/LinearModelResults.csv",header=T)
ModelResults[, 1:5]
whichmodel=3			
ThisModel=ModelResults[whichmodel,]
ThisModel

Map.im=ApplyLinearModel(ThisModel, ThisShrubData, NutData, GPredNut.im, ThisAllometry)
MojaveN.im=Map.im
save(MojaveN.im, file="SoilProbeSampling/MojavePC21imageObject")
cols=gray(seq(1,0,length.out=700))
plot(Map.im,col=cols,main=paste("estimated map for ", NUTRIENT, " in the ", DESERT, " Model ", ThisModel$Model))

#  Regional Models---------
#PICK MODEL TO USE- Must match desert and nutrient
DESERT
NUTRIENT
ModelResults=read.csv("SoilProbeSampling/RegionalModelResults.csv",header=T)
ModelResults[, 1:5]
whichmodel=1			
ThisModel=ModelResults[whichmodel,]
ThisModel
InterspaceData=ThisIntSpPars

Map.im=ApplyRegionalModel(ThisModel,  GPredNut.im)

cols=gray(seq(1,0,length.out=700))
plot(Map.im,col=cols,main=paste("estimated map for ", NUTRIENT, " in the ", DESERT, " Model ", ThisModel$Model))

#  Polynomial Regional Models---------
#PICK MODEL TO USE- Must match desert and nutrient
DESERT
NUTRIENT
ModelResults=read.csv("SoilProbeSampling/RegionalPolynomialModelResults.csv",header=T)
ModelResults[, 1:5]
whichmodel=3			
ThisModel=ModelResults[whichmodel,]
ThisModel

Map.im=ApplyPolynomialRegionalModel(ThisModel,  GPredNut.im, DESERT)

cols=gray(seq(1,0,length.out=700))
plot(Map.im,col=cols,main=paste("estimated map for ", NUTRIENT, " in the ", DESERT, " Model ", ThisModel$Model))


### See how well this map matches the observed values-----

if (DESERT=="Sonoran") PRS2=read.csv("SoilProbeSampling/PostTrt/Sonoran PRSData_PostTrt_FinalData_all_wPCA.csv")
if (DESERT=="Mojave") PRS2=read.csv("SoilProbeSampling/PostTrt/Mojave PRSData_PostTrt_FinalData_all_wPCA.csv")
QuadsInfo=subset(PRS2, BurnTrt=="UB")
QuadsInfoPred=cbind(QuadsInfo, Predicted=NA)
names(QuadsInfoPred)=c(names(QuadsInfo), paste("rep",1:REPNUM, sep=""))
RangeInfo=data.frame(meanRange=NA, sdRange=NA)

	for ( i in 1:nrow(QuadsInfoPred)){
		thesepixels=topixel(Map.im, QuadsInfo$X_calc[i], QuadsInfo$Y_calc[i])
		QuadsInfoPred$Predicted[i]=Map.im$v[thesepixels[2],thesepixels[1]]
	} # end i loop
	RangeInfo=c(mean(Result$table$Range), sd(Result$table$Range))
QuadsInfoPred
#plot(QuadsInfoPred[,c(which(names(QuadsInfoPred)==NUTRIENT), (length(QuadsInfo)+1):length(QuadsInfoPred))])
RepCompareData=QuadsInfoPred[,c(which(names(QuadsInfoPred)==NUTRIENT), (length(QuadsInfo)+1):length(QuadsInfoPred))]
lmresults=summary(lm(RepCompareData[,1]~RepCompareData[,2]))
lmresults

windows()
plot(RepCompareData[,2], RepCompareData[,1], xlab="Predicted", ylab="Observed")
text(50, 250, paste("Slope = ", round(lmresults$coefficients[2,1],3),", R2 = ", round(lmresults$r.squared,3), ", P = ", round(lmresults$coefficients[2,4],3)))
abline(lmresults)
abline(0,1,lty=2)

################
Model=ThisModel
ShrubData=ThisShrubData
ObsNutData=NutData
BlankImage.im=GPredNut.im
BlankImage.im=Map.im
Allometry=ThisAllometry
Desert=DESERT
InterspaceData=ThisIntSpPars

remove(Model, ShrubData, ObsNutData, BlankImage.im, Allometry)


######## For Vegetation MOdeling -------------
GPredDens=matrix(data=NA, ncol=length(xcrds), nrow=length(ycrds))
GPredDens.im=im(GPredDens, xcrds, ycrds, unitname=c("meter","meters"))
BlankImage.im=GPredDens.im
ShrubData=ThisShrubData

### Quadratic Vertex Models--------------
#PICK MODEL TO USE- Must match desert and nutrient
DESERT
SPECIES
ModelResults=read.csv("VegSpatialModeling/ErodMojParams.csv",header=T)
ModelResults[, 1:5]
whichmodel=1    		
ThisModel=ModelResults[whichmodel,]
ThisModel
#ThisIntSpPars=subset(IntSpcParams, (Desert==DESERT)&( Nutrient==NUTRIENT))


Map.im=ApplyQuadVertexModel(ThisModel, ThisShrubData, ThisIntSpPars, GPredNut.im)

save(SonoranN1.im, file="SoilProbeSampling/SonoranN1imageObject")
cols=gray(seq(1,0,length.out=700))
windows(9,9)
plot(Map.im,col=cols,main=paste("estimated map for ", NUTRIENT, " in the ", DESERT, " Model ", ThisModel$Model))



