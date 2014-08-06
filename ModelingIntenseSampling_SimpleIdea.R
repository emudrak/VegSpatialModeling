

library(nlme)
library(spatstat)

setwd("C:/Users/mudrak/Documents/SERDPproject")
setwd("C:/Users/elm26/Documents/SERDPproject")
setwd("C:/Users/Erika/Documents/Research_ISU/SERDP")
#source("SoilProbeSampling/CompassFunctions")
#source("SoilProbeSampling/RasterGeometry")
source("SoilProbeSampling/ApplyModelFunctions.R")
source("VegSpatialModeling/PlotTransectGraphs.R")

qlogis(seq(0,1,by=0.1))

# Make transformation functions############
logit.transform = function(x){  #This one looks better (Phil Dixon's suggestion)
    #  x is a vector of values between 0 and 1 inclusive
    eps=min(x[x!=0])
    x[x==0]=0.5*eps
    x[x==1]=1-0.5*eps
    return(log(x/(1-x)))
}
INVlogit.transform=function(alpha){
    #alpha is a vector of real number
    return(exp(alpha)/(exp(alpha)+1))
}
logit.transform.eps = function(x, eps){
    #  x is a vector of values between 0 and 1
    eps=min(x[x!=0])
    return(log((x+eps/(1-x+eps))))
    
}

#Read in and munge data -----------------
# PICK A DESERT
#DESERT="Sonoran"
DESERT="Mojave"
YEAR="2013Xtra"


if((DESERT=="Mojave") & (YEAR=="2013Xtra")){
  CensusData=read.csv("VegSpatialModeling/Mojave2013Intense24Shrubs.csv", blank.lines.skip=TRUE)
  CensusData$Invasives=CensusData$Erodium+CensusData$Schismus+CensusData$Bromus
  Shrubs=read.csv("VegSpatialModeling/CALarreaVolume.csv")
  Shrubs=subset(Shrubs, ShrubID %in% as.character(1:168))
  Shrubs$Shrub=as.numeric(as.character(Shrubs$ShrubID)) 
}

Shrubs[c("Shrub", "ShrubID")]

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
table(Census.Test[c("Fire", "Rain", "TranDir")])
Census.Train=Census[!(Census$Shrub %in% TestShrubs),]
table(Census.Train[c("Fire", "Rain", "TranDir")])

#  Plot Lines #######################################################

# Setup for elaborate plotting 
colorset=sample(rainbow(168))
firecols=c("gray40", "green")
watercols=c("lightblue", "tan")
dircols=c("red","blue")
firepch=c(16,1)
dirwds=c(1,2)


#Set scale ranges for this target
xplotmax=max(Census.Train[,"PlotDist"], na.rm=TRUE)
xplotmax=max(Census.Train[,"PlotDist"], na.rm=TRUE)
yplotmax=max(Census.Train[,TargSpec])


# Census.Train[,TargSpec]
# hist(Census.Train[,TargSpec], breaks= 400, xlab=ylabel, main=NULL)   #Zero Inflated?
# table(Census.Train[,TargSpec] )
# hist(round(Census.Train[,TargSpec]), breaks= 400, xlab=ylabel, main=NULL)
# table(round(Census.Train[,TargSpec]) )

# #ConsiderTransforming?
# hist(log(Census.Train$Target+1), breaks= 40, xlab=ylabel, main="Log(x+1)")  #STill zero inflated
# hist(asin(sqrt(Census.Train$Target/100))*2/pi, breaks= 40, xlab=ylabel, main="Arcsin(sqrt(x))*2/pi")
# 
# qlogis(seq(0,1,by=0.1))
# 
# test=seq(0,1,by=0.001)
# plot(range(test), range(log(test+1), asin(sqrt(test))*2/pi,logit.transform(test), logit.transform.eps(test,min(test[test!=0]) )), pch=NA, xlab="test", ylab="transformed test")
# plot(range(test), c(-5, 5), pch=NA, xlab="test", ylab="transformed test")
# lines(test, log(test+1))
# lines(test, asin(sqrt(test))*2/pi, col="red")
# lines(test, logit.transform(test), col="blue")  #this looks the best
# lines(test, logit.transform.eps(test,min(test[test!=0]) ), col="green")
# abline(h=0, lty=2)
# legend(0, 4.5, c("log(x+1", "Arcsin(sqrt(x))*2/pi","Emperical Logit (Dixon)", "Emperical Logit eps=lowest non-zero"), lty=1, col=c("black","red", "blue", "green"), cex=0.7 )
# alp=logit.transform(test)
# plot(alp, INVlogit.transform(alp))
# 
# # hist(logit.transform(Census.Train$Target/100), breaks= 40, xlab=ylabel, main="Emperical Logit (Dixon)")
# # hist(logit.transform.eps(Census.Train$Target/100, min((Census.Train$Target/100)[Census.Train$Target/100>0])), breaks= 40, xlab=ylabel, main="Emperical Logit eps=lowest non-zero")

# 
# # Consider dropping all drought treatment... to get rid of zero values. 
# with(subset(Census.Train, Rain=="A"), hist(log(Target/100+1), breaks= 40, xlab=ylabel, main="Ambient, Log(x+1)"))
# with(subset(Census.Train, Rain=="A"),hist(asin(sqrt(Target/100))*2/pi, breaks= 40, xlab=ylabel, main="Ambient, Arcsin(sqrt(x))*2/pi"))
# with(subset(Census.Train, Rain=="A"),hist(logit.transform(Target/100), breaks= 40, xlab=ylabel, main="Ambient, Emperical Logit (Dixon)"))
# with(subset(Census.Train, Rain=="A"),hist(logit.transform.eps(Target/100, min((Census.Train$Target/100)[Census.Train$Target/100>0])), breaks= 40, xlab=ylabel, main="Ambient, Emperical Logit eps=lowest non-zero"))
# 
# with(subset(Census.Train, Rain=="D"), hist(log(Target/100+1), breaks= 40, xlab=ylabel, main="Drought, Log(x+1)"))
# with(subset(Census.Train, Rain=="D"),hist(asin(sqrt(Target/100))*2/pi, breaks= 40, xlab=ylabel, main="Drought, Arcsin(sqrt(x))*2/pi"))
# with(subset(Census.Train, Rain=="D"),hist(logit.transform(Target/100), breaks= 40, xlab=ylabel, main="Drought, Emperical Logit (Dixon)"))
# with(subset(Census.Train, Rain=="D"),hist(logit.transform.eps(Target/100, min((Census.Train$Target/100)[Census.Train$Target/100>0])), breaks= 40, xlab=ylabel, main="Drought, Emperical Logit eps=lowest non-zero"))
# 
# #Ugh still looks zero inflated.  That is not worth it. 




#Plot With transformed response:##################################

yplotmax=max(Census.Train$LogitTarg)
hist(Census.Train$LogitTarg)

windows(7,10)
par(mfrow=c(3,1))
#With Fire Colors
with(Census.Train, plot(c(1,xplotmax), range(Census.Train$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("Logit transformmed ", ylabel), xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber   
  myburn=Census.Train$Fire[Census.Train$Shrub==i][1]  
  lines(Census.Train$PlotDist[(Census.Train$Shrub==i)],
        Census.Train$LogitTarg[Census.Train$Shrub==i],   
        pch=20,lwd=2, type="o", lty=1,
        col=firecols[myburn]
        
  )
  print(i)
  print(myburn)
} # end of i loop
legend(200,0.90* yplotmax, levels(Census.Train$Fire), col=firecols, lwd=2, lty=1, bty="n")  	#IF a 2012 Census.Train

#With Rain Colors
with(Census.Train, plot(c(1,xplotmax), range(Census.Train$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("Logit transformmed ", ylabel), xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber
  mywater=Census.Train$Rain[Census.Train$Shrub==i][1]  
  lines(Census.Train$PlotDist[(Census.Train$Shrub==i)],
        Census.Train$LogitTarg[Census.Train$Shrub==i],   
        pch=20,lwd=2, type="o", lty=1,
        col=watercols[mywater]
  )
} # end of i loop
legend(200,0.90* yplotmax, levels(Census.Train$Rain), col=watercols, lwd=2, lty=1, bty="n")  	#IF a 2012 Census.Train

#With Direction Colors
with(Census.Train, plot(c(1,xplotmax), range(Census.Train$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("Logit transformmed ", ylabel), xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber
  mydir=Census.Train$TranDir[Census.Train$Shrub==i][1]
  lines(Census.Train$PlotDist[Census.Train$Shrub==i],
        Census.Train$LogitTarg[Census.Train$Shrub==i], 
        pch=20,lwd=2, type="o", lty=1,
        col=dircols[mydir]
  )
} # end of i loop
legend(200,0.90* yplotmax, levels(Census.Train$TranDir), col=dircols, lwd=2, lty=1, bty="n")  	#IF a 2012 Census.Train


#Plot With % cover response:##################################

yplotmax=max(Census.Train$Target)
hist(Census.Train$Target)
windows(7,10)
par(mfrow=c(3,1))
#With Fire Colors
with(Census.Train, plot(c(1,xplotmax), range(Census.Train$Target/100) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("% Cover", ylabel), xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber   
  myburn=Census.Train$Fire[Census.Train$Shrub==i][1]  
  lines(Census.Train$PlotDist[(Census.Train$Shrub==i)],
        Census.Train$Target[Census.Train$Shrub==i]/100,   
        pch=20,lwd=2, type="o", lty=1,
        col=firecols[myburn]
        
  )
  print(i)
  print(myburn)
} # end of i loop
legend(200,0.90* yplotmax, levels(Census.Train$Fire), col=firecols, lwd=2, lty=1, bty="n")      #IF a 2012 Census.Train

#With Rain Colors
with(Census.Train, plot(c(1,xplotmax), range(Census.Train$Target/100) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=ylabel, xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber
  mywater=Census.Train$Rain[Census.Train$Shrub==i][1]  
  lines(Census.Train$PlotDist[(Census.Train$Shrub==i)],
        Census.Train$Target[Census.Train$Shrub==i]/100,   
        pch=20,lwd=2, type="o", lty=1,
        col=watercols[mywater]
  )
} # end of i loop
legend(200,0.90* yplotmax, levels(Census.Train$Rain), col=watercols, lwd=2, lty=1, bty="n")  	#IF a 2012 Census.Train

#With Direction Colors
with(Census.Train, plot(c(1,xplotmax), range(Census.Train$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("Logit transformmed ", ylabel), xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber
  mydir=Census.Train$TranDir[Census.Train$Shrub==i][1]
  lines(Census.Train$PlotDist[Census.Train$Shrub==i],
        Census.Train$LogitTarg[Census.Train$Shrub==i], 
        pch=20,lwd=2, type="o", lty=1,
        col=dircols[mydir]
  )
} # end of i loop
legend(200,0.90* yplotmax, levels(Census.Train$TranDir), col=dircols, lwd=2, lty=1, bty="n")  	#IF a 2012 Census.Train

     
#---------------------------------------------------------------------------------------------

# Try a simple linear decreasing model using only values from Canopy dripline

# Or, average annual biomass within a range of influence (how to get this, unless linear decreasing model...)
#Dripline in cm
Census.Train$Dripline=sqrt(Census.Train$Area_Bot/pi)*100
hist(Census.Train$Dripline)
    

Census.Train$Shrub[which.max(Census.Train$Dripline)]
Census.Train[Census.Train$Shrub==3,]


# Show only plots outside Canopy Dripline
Census.Train$Outer=Census.Train$PlotDist>Census.Train$Dripline

yplotmax=max(Census.Train$LogitTarg)
hist(Census.Train$LogitTarg)

Train.Outer=Census.Train[Census.Train$Outer==TRUE,]

windows(7,10)
par(mfrow=c(3,1))
with(Train.Outer, plot(c(1,xplotmax), range(Train.Outer$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("Logit transformmed ", ylabel), xlab="Distance from Shrub Stem" ))
for (i in unique(Train.Outer$Shrub)) {  # i=ShrubNumber   
  myburn=Train.Outer$Fire[Train.Outer$Shrub==i][1]  
  lines(Train.Outer$PlotDist[(Train.Outer$Shrub==i)],
        Train.Outer$LogitTarg[Train.Outer$Shrub==i],   
        pch=20,lwd=2, type="o", lty=1,
        col=firecols[myburn]
        
  )
} # end of i loop
legend(200,0.90* yplotmax, levels(Train.Outer$Fire), col=firecols, lwd=2, lty=1, bty="n")      #IF a 2012 Train.Outer

library(lme4)

linmod.lme=lmer(LogitTarg~PlotDist*Area_Bot*Fire*Rain+(1|Shrub), data=Census.Train[Census.Train$Outer==TRUE & Census.Train$Shrub!=166,])

summary(linmod.lme)
