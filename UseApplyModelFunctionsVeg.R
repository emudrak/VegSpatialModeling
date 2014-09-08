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
YEAR="2013Xtra"

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

GPredDens=matrix(data=NA, ncol=length(xcrds), nrow=length(ycrds))
GPredDens=im(GPredDens, xcrds, ycrds, unitname=c("meter","meters"))

##########################################################
# Get Shrub locations-------------
if (DESERT=="Sonoran") ThisShrubData=read.csv("./AZLarreaVolume_Complete.csv",header=TRUE)
if (DESERT=="Mojave") ThisShrubData=read.csv("../CALarreaVolume.csv",header=TRUE)

 ThisShrubData$Area_Bot = ThisShrubData$Area_Bot/1000
 ThisShrubData$Area_Top =   ThisShrubData$Area_Top/1000
 ThisShrubData$Area_Stem =   ThisShrubData$Area_Stem/1000
 ThisShrubData$Vol_Bot    =  ThisShrubData$Vol_Bot/100000  
 ThisShrubData$Vol_Stem    =  ThisShrubData$Vol_Stem/100000 
 ThisShrubData$Vol_Cent    =  ThisShrubData$Vol_Cent/100000  
 ThisShrubData$Vol_BotRing =   ThisShrubData$Vol_BotRing/100000 
 ThisShrubData$Vol_StemRing =  ThisShrubData$Vol_StemRing/100000 


#Read in and munge data -----------------
# PICK A DESERT
#DESERT="Sonoran"
DESERT="Mojave"
YEAR="2013Xtra"


if((DESERT=="Mojave") & (YEAR=="2013Xtra")){
  CensusData=read.csv("../Mojave2013Intense24Shrubs.csv", blank.lines.skip=TRUE)
  CensusData$Invasives=CensusData$Erodium+CensusData$Schismus+CensusData$Bromus
  Shrubs=read.csv("../CALarreaVolume.csv")
  Shrubs=subset(Shrubs, ShrubID %in% as.character(1:168))
  Shrubs$Shrub=as.numeric(as.character(Shrubs$ShrubID)) #THIS MESSES UP!!!
}

Census=merge(Shrubs[,c("Shrub","Area_Bot","Vol_Bot","Vol_Stem")], CensusData,  by.y="Shrub", by.x="Shrub")

Census$Area_Bot=Census$Area_Bot/10000
Census$Vol_Bot=Census$Vol_Bot/1000000
Census$Vol_Stem=Census$Vol_Stem/1000000

Census=Census[order(Census[,"Shrub"], Census[,"MHcode"]), ]
names(Census)[1]="Shrub"

# Setup for elaborate plotting --------------
colorset=sample(rainbow(168))
firecols=c("gray40", "green")
watercols=c("lightblue", "tan")
dircols=c("red","blue")
firepch=c(16,1)
dirwds=c(1,2)

# Chose target species to model this time --------------
TargSpec="ErodCover"; ylabel="% Cover" 
#TargSpec="Erodium"; ylabel="# Plants" 

Census$Target=round(Census[,TargSpec]) #Round to deal with issue of 0.1 percent cover values
Census$LogitTarg=logit.transform(Census$Target/100)
Census$TargPres=ceiling(Census$Target/100) #Make a new binary variable showing zero or not

# table(Census[c("Fire", "Rain", "TranDir")])
# unique(Census[c("Fire", "Rain", "TranDir")])
# unique(Census[c( "Shrub", "Fire", "Rain", "TranDir")])

# Make training and Testing Data- ------------
# balance out Fire, Rain and TranDir, 7 of each shrub 
#  Withhold two in each category for testing
set.seed(81)
BurnAmbN=sample(unique(subset(Census, (Fire=="B")&(Rain=="A")&(TranDir=="N"))$Shrub), 2)
BurnAmbS=sample(unique(subset(Census, (Fire=="B")&(Rain=="A")&(TranDir=="S"))$Shrub), 2)
BurnDrtN=sample(unique(subset(Census, (Fire=="B")&(Rain=="D")&(TranDir=="N"))$Shrub), 2)
UnBurnAmbN=sample(unique(subset(Census, (Fire=="UB")&(Rain=="A")&(TranDir=="N"))$Shrub), 2)
UnBurnAmbS=sample(unique(subset(Census, (Fire=="UB")&(Rain=="A")&(TranDir=="S"))$Shrub), 2)
UnBurnDrtN=sample(unique(subset(Census, (Fire=="UB")&(Rain=="D")&(TranDir=="N"))$Shrub), 2)
TestShrubs=c(BurnAmbN, BurnAmbS, BurnDrtN, UnBurnAmbN,  UnBurnAmbS,  UnBurnDrtN)
TestShrubs
#58 131  99  14 124 120 122  80  93  63  69  26

Census.Test=Census[Census$Shrub %in% TestShrubs,]
Census.Train=Census[!(Census$Shrub %in% TestShrubs),]




# PICK ONE NUTRIENT AT A TIME.  Pick the same name as the column heading
SPECIES="ErodCover"; ylabel="% Cover" 

VegData=Census.Train[Census.Train$Target>0,c("Shrub",   "TranDir", "Area_Bot", "Fire","Rain", "Plot", "PlotDist",  SPECIES)]
VegData=Census.Test[Census.Test$Target>0,c("Shrub",   "TranDir", "Area_Bot", "Fire","Rain", "Plot", "PlotDist",  SPECIES)]

names(VegData)[ncol(VegData)]="Species"



source("./VegSpatialModeling/PlotTransectGraphs.R")




    # Sample Shrub is the list of shrubs to model against. 
SampleShrub=subset(ShrubData,(Northing<=Ymax)&(Northing>=Ymin)&(Easting<=Xmax)&(Easting>=Xmin),  #Only want shrubs within boundaries
            	select=c("ShrubID","Easting","Northing","Area_Bot","Area_Stem"))	#Only want certain info


SampleShrub=unique(VegData[c("Shrub","TranDir","Area_Bot","Fire","Rain")])
SS=SampleShrub  #just to shorten the crazy code below...
ModelResults=read.csv("../ErodMojParams.csv",header=T)
ModelResults[, 1:5]
whichmodel=1            
ThisModel=ModelResults[whichmodel,]
ThisModel
Model=ThisModel

    rownames(SS)=1:nrow(SS)
    nrow(SS)
    SS$TranDirDummy=as.numeric(SS$TranDir)-1  #Now N=0 and S=1
    SS$FireDummy=as.numeric(SS$Fire)-1  #Now B=0 and UB=1
    SS$RainDummy=as.numeric(SS$Rain)-1  #Now A=0 and D=1
    a.rand=rnorm(nrow(SS),0,Model$a..Intercept..1)
    h.rand=rnorm(nrow(SS),0,Model$h..Intercept..1)
    k.rand=rnorm(nrow(SS),0,Model$k..Intercept..1)
#Change the random process part to inside the loop???
    SS$a=Model$a..Intercept.+ Model$a.Area_Bot * SS$Area_Bot + Model$a.TranDirS*SS$TranDirDummy+ Model$a.Area_Bot.TranDirS*SS$Area_Bot*SS$TranDirDummy+a.rand
    SS$h=Model$h..Intercept.+ Model$h.Area_Bot * SS$Area_Bot + Model$h.TranDirS*SS$TranDirDummy+h.rand
    SS$k=Model$k..Intercept.+ Model$k.Area_Bot * SS$Area_Bot + Model$k.FireUB*SS$FireDummy+Model$k.RainD*SS$RainDummy + Model$k.Area_Bot.FireUB*SS$Area_Bot*SS$FireDummy+k.rand

SampleShrub=SS
    

#Stage 1:   

Stg1Model=data.frame(t(fixed.effects(hurdle6.lme)))

### Show  curves for these shrubs -----------
#windows(7,4) 
# Setup for elaborate plotting 
colorset=sample(rainbow(168))
firecols=c("gray40", "green")
watercols=c("lightblue", "tan")
dircols=c("red","blue")
firepch=c(16,1)
dirwds=c(1,2)

col=watercols[SampleShrub$Rain[i]]
col=firecols[SampleShrub$Fire[i]]
col=dircols[SampleShrub$TranDir[i]]

windows()
with(VegData, plot(PlotDist, Species/100, xlab="Distance from Shrub Stem", ylab="Cover"))
#Must divide species by 100 so logit transform will work. 
with(VegData, plot(PlotDist, logit.transform(Species/100), xlab="Distance from Shrub Stem", ylab="Cover", yaxt="n"))
axis(2, at=logit.transform(seq(0,0.35, by=0.05)), labels=seq(0,0.35, by=0.05))

#plot(c(0, 500), c(0,1), pch=NA, xlab="Distance from Shrub (cm)", ylab="P(plant present)")

for (i in 1:nrow(SampleShrub)){
	new.x=seq(0,260, by=20)
    #Stage 1- 
   logitp=Stg1Model$X.Intercept. + Stg1Model$RainD*SampleShrub$RainDummy[i]+Stg1Model$PlotDist*new.x + Stg1Model$TranDirS*SampleShrub$TranDirDummy[i]+ Stg1Model$I.PlotDist.2.*(new.x^2)+ Stg1Model$FireUB*SampleShrub$FireDummy[i] + Stg1Model$PlotDist.TranDirS*SampleShrub$TranDirDummy[i]*new.x+ Stg1Model$TranDirS.I.PlotDist.2.*SampleShrub$TranDirDummy[i]*(new.x^2)+ Stg1Model$PlotDist.FireUB*SampleShrub$FireDummy[i]*new.x+ Stg1Model$I.PlotDist.2..FireUB*SampleShrub$FireDummy[i]*(new.x^2)
    
    p=INVlogit.transform(logitp)
    #lines(new.x, p, col=dircols[SampleShrub$TranDir[i]], lwd=3)
    #Stage 2- if present, what is %Cover?
	cover=INVlogit.transform(SampleShrub$a[i]*((new.x/100)-SampleShrub$h[i])^2+SampleShrub$k[i])

    occurs=rbinom(length(new.x), 1, p)
    #logit.transform(occur)
    #Combine the two
    new.y=cover*occurs
    
    lines(new.x, new.y, lwd=3,col=watercols[SampleShrub$Rain[i]])

}
title(main=paste("Curves for", SPECIES, " in the ", DESERT, " Model ", ThisModel$Model), sub="Testing DataSet")







######## Map Making -------------
GPredDens=matrix(data=NA, ncol=length(xcrds), nrow=length(ycrds))
GPredDens.im=im(GPredDens, xcrds, ycrds, unitname=c("meter","meters"))
BlankImage.im=GPredDens.im
ShrubData=ThisShrubData

### Quadratic Vertex Models--------------
#PICK MODEL TO USE- Must match desert and nutrient
DESERT
SPECIES

#ThisIntSpPars=subset(IntSpcParams, (Desert==DESERT)&( Nutrient==NUTRIENT))


Map.im=ApplyQuadVertexModel(ThisModel, ThisShrubData, ThisIntSpPars, GPredNut.im)

save(SonoranN1.im, file="SoilProbeSampling/SonoranN1imageObject")
cols=gray(seq(1,0,length.out=700))
windows(9,9)
plot(Map.im,col=cols,main=paste("estimated map for ", NUTRIENT, " in the ", DESERT, " Model ", ThisModel$Model))

