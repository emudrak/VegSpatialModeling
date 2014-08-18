

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

# Try two-stage modeling- logistic for "is empty?" and then transformed value mixeff for the nonzero values
# or logit transformed linear regression (non linear?!!!?!)
  
# Issue:  TranNum is nested within Shrub, but only Shrub is Random (randomly selected from larger population)
# TranNum is just two replicates- the only two there are.   This should not be considered subsampling then? 
# Francoise:  Shrub, Transect, and Plot should all be random. 
##AAhh But for this intense sampling analysis, we don't have TranNum, because only was side was intensively sampled!

#Test non-zero distribution ###############

  hist (Census.Train$Target, breaks=200)
hist(Census.Train$LogitTarg, freq=FALSE)
hist(Census.Train$LogitTarg[Census.Train$Target>0], freq=FALSE)
summary(Census.Train$LogitTarg[Census.Train$Target>0])
lines(seq(-5, 0, 0.01), dnorm(seq(-5,0, 0.01),  mean=mean(Census.Train$LogitTarg[Census.Train$Target>0], sd=sd(Census.Train$LogitTarg[Census.Train$Target>0]))),type='l')
#so removing zeroes gives a somewhat close to normal fit. (??)
###########

# First, try two-stage hurdle models. 
# Plot With presence/absence response ##############

windows(7,7)
par(mfrow=c(3,1))
#With Fire Colors
with(Census.Train, plot(c(1,xplotmax), c(0,1) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab="Pres/Abs", xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber   
  myburn=Census.Train$Fire[Census.Train$Shrub==i][1]  
  points(Census.Train$PlotDist[(Census.Train$Shrub==i)],
        jitter(Census.Train$TargPres[Census.Train$Shrub==i], 0.2),   
        pch=19, 
        col=firecols[myburn]     
  )
} # end of i loop
legend(200,0.5, levels(Census.Train$Fire), col=firecols, lwd=2, lty=1, bty="n")      #IF a 2012 Census.Train

#With Rain Colors
with(Census.Train, plot(c(1,xplotmax), c(0,1) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab="Pres/Abs", xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber
  mywater=Census.Train$Rain[Census.Train$Shrub==i][1]  
  points(Census.Train$PlotDist[(Census.Train$Shrub==i)],
        jitter(Census.Train$TargPres[Census.Train$Shrub==i], 0.2),   
        pch=19,
        col=watercols[mywater]
  )
} # end of i loop
legend(200,0.5, levels(Census.Train$Rain), col=watercols, lwd=2, lty=1, bty="n")  	#IF a 2012 Census.Train

#With Direction Colors
with(Census.Train, plot(c(1,xplotmax), c(0,1), pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab="Pres/Abs", xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber
  mydir=Census.Train$TranDir[Census.Train$Shrub==i][1]
  points(Census.Train$PlotDist[Census.Train$Shrub==i],
        jitter(Census.Train$TargPres[Census.Train$Shrub==i], 0.2),   
        pch=19,
        col=dircols[mydir]
  )
} # end of i loop
legend(200,0.5, levels(Census.Train$TranDir), col=dircols, lwd=2, lty=1, bty="n")  	#IF a 2012 Census.Train


     
# presence/absense crosstabs ------
xtabs(~TargPres+MHcode+Rain+Fire+TranDir, data=Census.Train)
xtabs(~TargPres+Rain+Fire+TranDir, data=Census.Train)
xtabs(~Rain+Fire+TranDir, data=Census.Train)
xtabs(~TargPres+TranDir, data=Census.Train)
xtabs(~TargPres+Rain, data=Census.Train)
xtabs(~TargPres+Fire, data=Census.Train)

# Logistic modeling -----

library(lme4)
hurdle.lme=glmer(TargPres~Rain*PlotDist+Rain*I(PlotDist^2)
                        +TranDir*PlotDist+TranDir*I(PlotDist^2)
                        +Fire*PlotDist+Fire*I(PlotDist^2)
                        + TranDir:Fire + Rain:Fire                #No Dir/Rain interxn- no south shelters!
                 + (1|Shrub), family=binomial, data=Census.Train)
summary(hurdle.lme)



hurdle2.lme=glmer(TargPres~Rain*PlotDist+Rain*I(PlotDist^2)
                 +TranDir*PlotDist+TranDir*I(PlotDist^2)
                 +Fire*PlotDist+Fire*I(PlotDist^2)
                 + TranDir:Fire               #No Dir/Rain interxn- no south shelters!
                 + (1|Shrub), family=binomial, data=Census.Train)
summary(hurdle2.lme)


hurdle3.lme=glmer(TargPres~Rain*PlotDist
                  +TranDir*PlotDist+TranDir*I(PlotDist^2)
                  +Fire*PlotDist+Fire*I(PlotDist^2)
                  + TranDir:Fire               #No Dir/Rain interxn- no south shelters!
                  + (1|Shrub), family=binomial, data=Census.Train)
summary(hurdle3.lme)



hurdle4.lme=glmer(TargPres~Rain*PlotDist
                  +TranDir*PlotDist+TranDir*I(PlotDist^2)
                  +Fire*PlotDist+Fire*I(PlotDist^2)
                  + (1|Shrub), family=binomial, data=Census.Train)
summary(hurdle4.lme)



#Try adding Area_Bot


hurdle5.lme=glmer(TargPres~Rain*PlotDist
                  +TranDir*PlotDist+TranDir*I(PlotDist^2)
                  +Fire*PlotDist+Fire*I(PlotDist^2)
                  + (1|Shrub), family=binomial, data=Census.Train)
summary(hurdle5.lme)


hurdle6.lme=glmer(TargPres~Rain+PlotDist
                  +TranDir*PlotDist+TranDir*I(PlotDist^2)
                  +Fire*PlotDist+Fire*I(PlotDist^2)
                  + (1|Shrub), family=binomial, data=Census.Train)
summary(hurdle6.lme)

#Go with this one for now???

coef(hurdle6.lme)
fixed.effects(hurdle6.lme)

INVlogit.transform(fixed.effects(hurdle6.lme))

predict(hurdle6.lme, Census.Test, allow.new.levels=TRUE)  
#Not stochastic, uses population level data for previously unobserved levels
plot(INVlogit.transform(predict(hurdle6.lme, Census.Test, allow.new.levels=TRUE)), jitter(Census.Test$TargPres), xlab="Probability of presence", ylab="Observed presence (jittered)" ) 
#That looks pretty good

# Look at presence only crosstabs -------

xtabs(~MHcode+Rain+Fire+TranDir, data=Census.Train[Census.Train$Target>0,])
xtabs(~Rain+Fire+TranDir, data=Census.Train[Census.Train$Target>0,])
xtabs(~Shrub+Rain+Fire+TranDir, data=Census.Train[Census.Train$Target>0,])
xtabs(~Shrub, data=Census.Train[Census.Train$Target>0,])
##Yikes!  Shrub 166 only has one non-zero quadrat (of 8).  Don't include that!
xtabs(~TranDir, data=Census.Train[Census.Train$Target>0,])
xtabs(~Rain, data=Census.Train[Census.Train$Target>0,])
xtabs(~Fire, data=Census.Train[Census.Train$Target>0,])
 

# Presense only modeling- lme -----------
## Do non-zero part with tobit model in VGAM. No-  Tobits are for censored data , not truncated data
# Don't do glm:  rather, regression on transformed data- maybe can use vertex forumla then?
non.zero.lme=lme(LogitTarg~Rain*PlotDist+Rain*I(PlotDist^2)
                  +TranDir*PlotDist+TranDir*I(PlotDist^2)
                  +Fire*PlotDist+Fire*I(PlotDist^2)
                  + TranDir:Fire + Rain:Fire,  
                   random=~1|as.factor(Shrub),  data=Census.Train[Census.Train$Target>0,])
summary(non.zero.lme)
plot(non.zero.lme)

non.zero2.lme=lme(LogitTarg~Rain*PlotDist
                 +TranDir*PlotDist+TranDir*I(PlotDist^2)
                 +Fire*PlotDist+Fire*I(PlotDist^2)
                 + TranDir:Fire + Rain:Fire,  
                 random=~1|as.factor(Shrub),  data=Census.Train[Census.Train$Target>0,])
summary(non.zero2.lme)
plot(non.zero2.lme)

non.zero3.lme=lme(LogitTarg~Rain
                  +TranDir*PlotDist+TranDir*I(PlotDist^2)
                  +Fire*PlotDist+Fire*I(PlotDist^2)
                  + TranDir:Fire + Rain:Fire,  
                  random=~1|as.factor(Shrub),  data=Census.Train[Census.Train$Target>0,])
summary(non.zero3.lme)
plot(non.zero3.lme)

non.zero4.lme=lme(LogitTarg~Rain
                  +TranDir*PlotDist+TranDir*I(PlotDist^2)
                  +Fire*PlotDist+Fire*I(PlotDist^2)
                  + TranDir:Fire,  
                  random=~1|as.factor(Shrub),  data=Census.Train[Census.Train$Target>0,])
summary(non.zero4.lme)
plot(non.zero4.lme)

non.zero5.lme=lme(LogitTarg~Rain
                  +TranDir*PlotDist+TranDir*I(PlotDist^2)
                  +Fire*PlotDist
                  + TranDir:Fire,  
                  random=~1|as.factor(Shrub),  data=Census.Train[Census.Train$Target>0,])
summary(non.zero5.lme)
plot(non.zero5.lme)


non.zero6.lme=lme(LogitTarg~Rain
                  +TranDir*PlotDist+TranDir
                  +Fire*PlotDist
                  + TranDir:Fire,  
                  random=~1|as.factor(Shrub),  data=Census.Train[Census.Train$Target>0,])
summary(non.zero6.lme)
plot(non.zero6.lme)



non.zero7.lme=lme(LogitTarg~Rain
                  +TranDir*PlotDist+TranDir
                  +Fire
                  + TranDir:Fire,  
                  random=~1|as.factor(Shrub),  data=Census.Train[Census.Train$Target>0,])
summary(non.zero7.lme)
plot(non.zero7.lme)
#-------
#ugh do we really want to get rid of plotdist?
#Try vertex form, and non-linear modeling...

#Plot with Positive only data --------
Pos.Data=Census.Train[Census.Train$Target>0,]
Pos.Data=Census.Test[Census.Test$Target>0,]

yplotmax=max(Pos.Data$LogitTarg)
hist(Pos.Data$LogitTarg)

windows(7,10)
par(mfrow=c(3,1))
#With Fire Colors
with(Pos.Data, plot(c(1,xplotmax), range(Pos.Data$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=ylabel, xlab="Distance from Shrub Stem" , yaxt="n"))
axis(2, at=logit.transform(seq(0,0.35, by=0.05)), labels=seq(0,0.35, by=0.05))
for (i in unique(Pos.Data$Shrub)) {  # i=ShrubNumber   
    myburn=Pos.Data$Fire[Pos.Data$Shrub==i][1]  
    lines(Pos.Data$PlotDist[(Pos.Data$Shrub==i)],
          Pos.Data$LogitTarg[Pos.Data$Shrub==i],   
          pch=20,lwd=2, type="o", lty=1,
          col=firecols[myburn]
          
    )
} # end of i loop
legend(200,0.90* yplotmax, levels(Pos.Data$Fire), col=firecols, lwd=2, lty=1, bty="n")      #IF a 2012 Pos.Data

#With Rain Colors
with(Pos.Data, plot(c(1,xplotmax), range(Pos.Data$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=ylabel, xlab="Distance from Shrub Stem", yaxt="n"))
axis(2, at=logit.transform(seq(0,0.35, by=0.05)), labels=seq(0,0.35, by=0.05))
for (i in unique(Pos.Data$Shrub)) {  # i=ShrubNumber
    mywater=Pos.Data$Rain[Pos.Data$Shrub==i][1]  
    lines(Pos.Data$PlotDist[(Pos.Data$Shrub==i)],
          Pos.Data$LogitTarg[Pos.Data$Shrub==i],   
          pch=20,lwd=2, type="o", lty=1,
          col=watercols[mywater]
    )
} # end of i loop
legend(200,0.90* yplotmax, levels(Pos.Data$Rain), col=watercols, lwd=2, lty=1, bty="n")  	#IF a 2012 Pos.Data

#With Direction Colors
with(Pos.Data, plot(c(0,xplotmax/100), range(Pos.Data$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab= ylabel, xlab="Distance from Shrub Stem" , yaxt="n"))
axis(2, at=logit.transform(seq(0,0.35, by=0.05)), labels=seq(0,0.35, by=0.05))
for (i in unique(Pos.Data$Shrub)) {  # i=ShrubNumber
    mydir=Pos.Data$TranDir[Pos.Data$Shrub==i][1]
    lines((Pos.Data$PlotDist[Pos.Data$Shrub==i]/100),
          Pos.Data$LogitTarg[Pos.Data$Shrub==i], 
          pch=20,lwd=2, type="o", lty=1,
          col=dircols[mydir]
    )
} # end of i loop
legend(200,0.90* yplotmax, levels(Pos.Data$TranDir), col=dircols, lwd=2, lty=1, bty="n")  	#IF a 2012 Census.Train

# Get reasonable starting values for vertex model---------

a=-0.5
h=1.25
k=-2
a;h;k


with(Census.Train[Census.Train$Target>0,], plot(PlotDist/100, LogitTarg))

xvals=seq(0, 2.5, length.out=100)
#Get better values
lines(xvals, a*(xvals-h)^2+k, lwd=7)


#Step A     Full Model------- 
#full model (with two-way interactions)
i=-0.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=0
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl, j.sl),  k=c(m,m,m,m,m), c(m.sl, m.sl,m.sl, m.sl, m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme, random=a+h~1|Shrub, start=c(a=c(-0.5,-0.5,-0.5,-0.5,-0.5), c(2, 2,2, 2, 2), h=c(1.25, 1.25,1.25, 1.25,1.25), c(-2, -2,-2, -2, -2),  k=c(-2,-2,-2,-2,-2), c(0, 0,0, 0,0)))
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirahk.FireRainahk.nlme, random=k~1|Shrub)  
##----------
# Worst is  k.FireUB, or k.Rain but keep for higher orders.  remove k.TranDirS:FireUB     (3)
# Worst is h.FireUB:RainD (1)
# Worst is a.Area_Bot:TranDirS (1)
# Worst is k.Area_Bot:TranDirS (2)
# Next, remove k.TranDirS:FireUB    --> Step B
# And remove k.Area_Bot:TranDirS    --> Step  ?????????
# And remove a.Area_Bot:TranDirS    --> Step  ?????????
# And remove h.FireUB:RainD         --> Step  ?????????
# 

#Step B     From Step A, remove k.TranDirS:FireUB   -----        
i=-.25; i.sl=2; j=1; j.sl=2; m=-1.5; m.sl=0
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:Rain),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl, m.sl, m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme, random=h+k~1|Shrub)  
i=-.5; i.sl=0; j=1.25; j.sl=2; m=-2; m.sl=-2
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme, random=a~1|Shrub,          start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl, m.sl, m.sl)))
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainahk.FireDirah.FireRainahk.nlme, random=k~1|Shrub)  
#----------
# Worst is  k.Area_Bot:RainD  (2)
# Worst is  k.TranDirS, but keep for higher order terms.  remove k.Area_Bot:RainD (2)
# Worst is  h.Area_Bot:RainD (1)
# Worst is  h.FireUB, but keep for higher order terms.  h.Area_Bot:RainD  (2)
# Next, remove   k.Area_Bot:RainD   --> Step  C
# Next, remove   h.Area_Bot:RainD   --> Step   ????????? 
 

#Step C     From Step B, remove k.Area_Bot:RainD ---------
i=-.5; i.sl=0; j=1.5; j.sl=-2; m=-2; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:Rain),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme)
i=-.5; i.sl=0; j=1.5; j.sl=-2; m=-1.5; m.sl=0
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme, random=a+h~1|Shrub, start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl,m.sl)))
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme, random=h+k~1|Shrub)  
i=-.5; i.sl=0; j=1.5; j.sl=-2; m=-1.5; m.sl=2
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme, random=a~1|Shrub, start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl,m.sl)))
i=-.25; i.sl=0; j=1.5; j.sl=0; m=-1.5; m.sl=0
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme, random=h~1|Shrub, start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl,m.sl)))
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRainah.FireDirah.FireRainahk.nlme, random=k~1|Shrub)  
#----------
# Worst is  h.Area_Bot:RainD (7)
# Next, remove    h.Area_Bot:RainD  --> Step D


#Step D     From Step C, remove h.Area_Bot:RainD ---------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:TranDir + Fire:Rain,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:Rain),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=a+h~1|Shrub)
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-2; m.sl=-2
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=a+k~1|Shrub, start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl,m.sl)))
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirahk.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=k~1|Shrub)  
#----------
# Worst is   k.Area_Bot:TranDirS  (5)   a.Area_Bot:FireUB(2)
# Next, remove   k.Area_Bot:TranDirS   --> Step E
# Then, rmove    a.Area_Bot:FireUB     --> ?????????


#Step E     From Step D, remove k.Area_Bot:TranDirS-----
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:TranDir + Fire:Rain,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire + Fire:Rain),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j,j, j,j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDirah.FireRainahk.nlme, random=k~1|Shrub)  
#----------
# Worst is  h.TranDirS:FireUB  (7)   
# Next, remove  h.TranDirS:FireUB   --> Step F


#Step F     From Step E, remove h.TranDirS:FireUB  -------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:Rain,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire + Fire:Rain),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j, j,j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainahk.nlme, random=k~1|Shrub)  
#----------
# Worst is  k.FireUB:RainD (7 )   
# Next, remove k.FireUB:RainD   --> Step  G


#Step G     From Step F, remove k.FireUB:RainD  --------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:Rain,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j, j,j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRainah.nlme, random=k~1|Shrub)  
#----------
# Worst is  a.TranDirS:FireUB (3)  h.FireUB:RainD (4)
# Next, remove  h.FireUB:RainD  --> Step   H
# Then, remove  a.TranDirS:FireUB  --> Step  AW


#Step H     From Step G, remove h.FireUB:RainD  ------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:TranDir + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j, j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireDira.FireRaina.nlme, random=k~1|Shrub)  
#----------
# Worst is  a.TranDirS:FireUB (7)   
# Next, remove  a.TranDirS:FireUB  --> Step   I

#Step I     From Step H, remove a.TranDirS:FireUB  --------------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j, j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRaina.nlme, random=k~1|Shrub)  
#----------
# Worst is  a.Area_Bot:RainD (7)   
# Next, remove a.Area_Bot:RainD   --> Step   J 

#Step J     From Step I, remove a.Area_Bot:RainD  ----
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i), c(i.sl, i.sl,i.sl, i.sl), h=c(j, j, j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.FireRaina.nlme, random=k~1|Shrub)  
#----------
# Worst is  k.Fire, but save for interxn.  Next worst is h.Rain   (7)   
# Next, remove  h.Rain --> Step    K 

#Step K     From Step J, remove h.Rain   -------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:TranDir + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i), c(i.sl, i.sl,i.sl, i.sl), h=c(j, j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFireahk.FireRaina.nlme, random=k~1|Shrub)  
#----------
# Worst is  a.Area_Bot:FireUB  (7)   
# Next, remove a.Area_Bot:FireUB    --> Step  L   


#Step L     From Step K, remove a.Area_Bot:FireUB   ------------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:TranDir + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i), c(i.sl, i.sl,i.sl), h=c(j, j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.FireRaina.nlme, random=k~1|Shrub)  
#----------
# Worst is   h.(Intercept), then h.TranDirS (but interxn) then  a.FireUB:RainD  (7)   
# Next, remove  a.FireUB:RainD  --> Step    M

#Step M     From Step L, remove a.FireUB:RainD  -------------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:TranDir + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i), c(i.sl, i.sl,i.sl), h=c(j, j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainak.SzDirah.SzFirehk.nlme, random=k~1|Shrub)  
#----------
# Worst is   h.(Intercept), then h.TranDirS (but interxn) then  a.RainD   (7)   
# Next, remove  a.RainD   --> Step   N


#Step N     From Step M, remove a.RainD  ----------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Area_Bot:TranDir,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:TranDir + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i, i), c(i.sl, i.sl ), h=c(j, j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme)
vert.ah.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Raink.SzDirah.SzFirehk.nlme, random=k~1|Shrub)  
#----------
# Worst is    h.FireUB  (but interxn) then  a.FireUB   (6)  
# Worst is a.FireUB (1)
# Next, remove  a.FireUB     --> Step   O

#Step O     From Step N, remove a.FireUB  -------
i=-.5; i.sl=2; j=1.25; j.sl=2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Area_Bot:TranDir,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:TranDir + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i), c(i.sl, i.sl ), h=c(j, j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme)
vert.ah.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme, random=a+k~1|Shrub)
i=-.5; i.sl=0; j=1.25; j.sl=2; m=-2; m.sl=2
vert.hk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme, random=h+k~1|Shrub, start=c(a=c(i,i), c(i.sl, i.sl ), h=c(j, j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)))  
vert.a.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirehk.nlme, random=k~1|Shrub)  
#----------
# Worst is   h.TranDirS (but interxn) then  a.(Intercept)  then h.FireUB (interxn), then  h.Area_Bot:FireUB  (6)  
# Worst is a.Area_Bot but interxn, the h.Area_Bot:TranDirS (1)
# Next, remove   h.Area_Bot:FireUB   --> Step   P
# Next, remove   h.Area_Bot:TranDirS   --> Step AA


#Step P     From Step O, remove h.Area_Bot:FireUB  ---------
i=-.5; i.sl=2; j=1.25; j.sl=2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Area_Bot:TranDir,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:TranDir,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i), c(i.sl, i.sl ), h=c(j, j), c(j.sl, j.sl,j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme)
vert.ah.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDirah.SzFirek.nlme, random=k~1|Shrub)  
#----------
# Worst is   a.(Intercept), a.Area_Bot, h.TranDirS then h.Area_Bot:TranDirS (7)  
# Next, remove   h.Area_Bot:TranDirS  --> Step   Q


#Step Q     From Step P, remove h.Area_Bot:TranDirS  ----------
i=-.5; i.sl=-2; j=1; j.sl=0; m=-2; m.sl=-2
vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Area_Bot:TranDir,
                    h ~Area_Bot + TranDir + Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i), c(i.sl, i.sl ), h=c(j, j), c(j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme)
vert.ah.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirek.nlme, random=k~1|Shrub)  
#----------
# Worst is   h.FireUB   (7)  
# Next, remove  h.FireUB    --> Step  R

#Step R     From Step Q, remove h.FireUB  ----
i=-.5; i.sl=-2; j=1; j.sl=0; m=-2; m.sl=-2
vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Area_Bot:TranDir,
                    h ~Area_Bot + TranDir,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i), c(i.sl, i.sl ), h=c(j), c(j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme)
vert.ah.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzDira.SzFirek.nlme, random=k~1|Shrub)  
#----------
# Worst is  a.Area_Bot:TranDirS   (2)  
# worst is h.Intercept, then k.TranDirS    (1)
# worst is a.Intercept, a.Area_Bot, but interxn, then  k.TranDirS   (1)
# worst is a.Intercept, a.Area_Bot, but interxn, then  a.Area_Bot:TranDirS   (3)
# Next, remove a.Area_Bot:TranDirS  --> Step  S
# Then, remove k.TranDirS --> Step Z

#Step S     From Step R, remove a.Area_Bot:TranDirS  ---------
i=-.5; i.sl=-2; j=1; j.sl=0; m=-2; m.sl=-2
vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir,
                    h ~Area_Bot + TranDir,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub), 
         start=c(a=c(i,i), c(i.sl), h=c(j), c(j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme)
i=-.25; i.sl=-2; j=1.5; j.sl=2; m=-2; m.sl=2
vert.ah.Szahk.Dirahk.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme, random=a+h~1|Shrub, start=c(a=c(i,i), c(i.sl), h=c(j), c(j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)))
vert.ak.Szahk.Dirahk.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirahk.Firek.Raink.SzFirek.nlme, random=k~1|Shrub)  
#----------
# Worst is  a.intercept, then k.TranDirS  (1)  
# Worst is  h.intercept, then k.TranDirS  (6)  
# Next, remove k.TranDirS  --> Step T

#Step T     From Step S, remove k.TranDirS  ------
i=-.5; i.sl=2; j=1.25; j.sl=2; m=-2; m.sl=2
vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir,
                    h ~Area_Bot + TranDir,
                    k ~Area_Bot + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  
         start=c(a=c(i,i), c(i.sl), h=c(j), c(j.sl, j.sl),  k=c(m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme)
qqnorm(vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme)
vert.ah.Szahk.Dirah.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirah.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirah.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirah.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirah.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirah.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzFirek.nlme, random=k~1|Shrub)  
#----------
# Worst is  a.intercept, then k.FireUB (but interxn), then h.TranDirS  (7)  
# Next, remove h.TranDirS  --> Step U

#Step U     From Step T, remove h.TranDirS   ----------
i=-.5; i.sl=2; j=1.25; j.sl=2; m=-2; m.sl=2
vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir,
                    h ~Area_Bot,
                    k ~Area_Bot + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  
         start=c(a=c(i,i), c(i.sl),  c(j.sl, j.sl),  k=c(m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme)
qqnorm(vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme)
vert.ah.Szahk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme, random=a~1|Shrub)
vert.h.Szahk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme, random=h~1|Shrub)
vert.k.Szahk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szahk.Dira.Firek.Raink.SzFirek.nlme, random=k~1|Shrub)  
#----------
# Worst is   k.FireUB (but interxn), then h.Area_Bot  (7)  
# Next, remove h.Area_Bot  --> Step V

#Step V     From Step U, remove h.Area_Bot  ------  
i=-.25; i.sl=2; j=1; j.sl=0; m=-2; m.sl=-2
vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir,
                    h ~1,
                    k ~Area_Bot + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  
         start=c(a=c(i,i), c(i.sl),  c(j.sl),  k=c(m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme)
qqnorm(vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme)
i=-.25; i.sl=-2; j=1; j.sl=-2; m=-2.5; m.sl=-2
vert.ah.Szak.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme, random=a+h~1|Shrub, start=c(a=c(i,i), c(i.sl),  c(j.sl),  k=c(m,m), c(m.sl, m.sl,m.sl)))
i=-.5; i.sl=2; j=1; j.sl=0; m=-2; m.sl=2
vert.ak.Szak.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme, random=a+k~1|Shrub, start=c(a=c(i,i), c(i.sl),  c(j.sl),  k=c(m,m), c(m.sl, m.sl,m.sl)))
i=-.25; i.sl=-2; j=1; j.sl=0; m=-1.5; m.sl=-2
vert.hk.Szak.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme, random=h+k~1|Shrub, start=c(a=c(i,i), c(i.sl),  c(j.sl),  k=c(m,m), c(m.sl, m.sl,m.sl)))  
i=-.25; i.sl=0; j=1; j.sl=-2; m=-2.5; m.sl=2
vert.a.Szak.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme, random=a~1|Shrub, start=c(a=c(i,i), c(i.sl),  c(j.sl),  k=c(m,m), c(m.sl, m.sl,m.sl)))
vert.h.Szak.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme, random=h~1|Shrub)
vert.k.Szak.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Dira.Firek.Raink.SzFirek.nlme, random=k~1|Shrub)  
#----------
# Worst is   k.FireUB (but interxn), then a.Area_Bot  (2)  
# Worst is   k.FireUB (but interxn), then none  (1) 
# Worst is  k.FireUB (but interxn), then a.Intercept (but a.TransDir), then none  (2) 
# Worst is  k.FireUB (but interxn), then  a.TransDir (1) 
# Next, remove a.Area_Bot  --> Step W
# Next, remove a.TransDir  --> Step  X

#Step W     From Step V, remove a.Area_Bot  ------
i=-.25; i.sl=2; j=1; j.sl=0; m=-2; m.sl=2
vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~ TranDir,
                    h ~1,
                    k ~Area_Bot + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  
         start=c(a=c(i,i),  j,  k=c(m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme)
qqnorm(vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme)
i=-.25; j=1.25;  m=-2; m.sl=2
vert.ah.Szk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme, random=a+h~1|Shrub, start=c(a=c(i,i),  j,  k=c(m,m), c(m.sl, m.sl,m.sl)))
vert.ak.Szk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme, random=a+k~1|Shrub)
vert.hk.Szk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme, random=h+k~1|Shrub)    #CANDIDATE MODEL
vert.a.Szk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme, random=a~1|Shrub)  #CANDIDATE MODEL
vert.h.Szk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme, random=h~1|Shrub)  #CANDIDATE MODEL
vert.k.Szk.Dira.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szk.Dira.Firek.Raink.SzFirek.nlme, random=k~1|Shrub)  #CANDIDATE MODEL
#----------
# Worst is   a.Intercept (but a.TransDir),k.FireUB (but interxn), then none  (3)  
#   k.FireUB (but interxn), then none  (4)  
# STOP                          #CANDIDATE MODELS

#Step X     From Step V, remove a.TransDir  -------
i=-.25; i.sl=2; j=1; j.sl=0; m=-2; m.sl=-2
vert.ahk.Szak.Firek.Raink.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot,
                    h ~1,
                    k ~Area_Bot + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  
         start=c(a=c(i), c(i.sl),  j,  k=c(m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szak.Firek.Raink.SzFirek.nlme)
qqnorm(vert.ahk.Szak.Firek.Raink.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szak.Firek.Raink.SzFirek.nlme)
i=-.25; i.sl=0; j=1; j.sl=-2; m=-2; m.sl=-2
vert.ah.Szak.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Firek.Raink.SzFirek.nlme, random=a+h~1|Shrub, start=c(a=c(i), c(i.sl),  j,  k=c(m,m), c(m.sl, m.sl,m.sl)))
i=-.25; i.sl=-2; j=1; j.sl=0; m=-1.5; m.sl=-2
vert.ak.Szak.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Firek.Raink.SzFirek.nlme, random=a+k~1|Shrub, start=c(a=c(i), c(i.sl),  j,  k=c(m,m), c(m.sl, m.sl,m.sl)))
vert.hk.Szak.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Firek.Raink.SzFirek.nlme, random=h+k~1|Shrub)  
i=-.25; i.sl=-2; j=1.5; j.sl=-2; m=-2; m.sl=0
vert.a.Szak.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Firek.Raink.SzFirek.nlme, random=a~1|Shrub, start=c(a=c(i), c(i.sl),  j,  k=c(m,m), c(m.sl, m.sl,m.sl)))
i=-.25; i.sl=0; j=1; j.sl=-2; m=-2; m.sl=-2
vert.h.Szak.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Firek.Raink.SzFirek.nlme, random=h~1|Shrub)
vert.k.Szak.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Firek.Raink.SzFirek.nlme, random=k~1|Shrub)  
#----------
# Worst is k.FireUB, but interxn.  Then a.(Intercept) (7)    
# Next, remove  a.(Intercept)  --> Step  Y


#Step Y     From Step X, remove a.(Intercept)  -------
i=-.25; i.sl=-2; j=1.25; j.sl=0; m=-2; m.sl=0
vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot-1,
                    h ~1,
                    k ~Area_Bot + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  
         start=c( c(i.sl),  j,  k=c(m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme)
qqnorm(vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme)
vert.ah.Szak.Nointa.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme, random=a+h~1|Shrub)
vert.ak.Szak.Nointa.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme, random=a+k~1|Shrub)
vert.hk.Szak.Nointa.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme, random=h+k~1|Shrub)  
vert.a.Szak.Nointa.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme, random=a~1|Shrub)  #CANDIDATE MODEL
i=-.2; i.sl=-2; j=1; j.sl=1; m=-1.5; m.sl=-2
vert.h.Szak.Nointa.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme, random=h~1|Shrub)  #CANDIDATE MODEL
vert.k.Szak.Nointa.Firek.Raink.SzFirek.nlme=update(vert.ahk.Szak.Nointa.Firek.Raink.SzFirek.nlme, random=k~1|Shrub)   #CANDIDATE MODEL 
#----------
# Worst is k.FireUB, but interxn.  Then none (7)    
# STOP!                      # CANDIDATE MODELS

#Step Z     From Step R, remove k.TranDirS    ---------------
i=-.5; i.sl=-2; j=1; j.sl=0; m=-2; m.sl=2
vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Area_Bot:TranDir,
                    h ~Area_Bot + TranDir,
                    k ~Area_Bot + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i), c(i.sl, i.sl ), h=c(j), c(j.sl, j.sl),  k=c(m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme)
qqnorm(vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme)
vert.ah.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme, random=a+h~1|Shrub)  #CANDIDATE MODEL
vert.ak.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme, random=a+k~1|Shrub)  #CANDIDATE MODEL
vert.hk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme, random=h+k~1|Shrub)  #CANDIDATE MODEL
vert.a.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme, random=a~1|Shrub)     #CANDIDATE MODEL
vert.h.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme, random=h~1|Shrub)     #CANDIDATE MODEL
vert.k.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme=update(vert.ahk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme, random=k~1|Shrub)      #CANDIDATE MODEL 
#----------
# Worst is  None (7)                #CANDIDATE MODELS
# STOP! 


#Step AA     From Step O, remove h.Area_Bot:TranDirS  -------
i=-.5; i.sl=2; j=1.25; j.sl=2; m=-1.5; m.sl=-2
vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Area_Bot:TranDir,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i), c(i.sl, i.sl ), h=c(j, j, j), c(j.sl,  j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme)
vert.ah.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFirehk.nlme, random=k~1|Shrub)  
#----------
# Worst is  k.Area_Bot:FireUB (7)  
# Next, remove  k.Area_Bot:FireUB  --> Step  AB  

#Step AB     From Step AA, remove k.Area_Bot:FireUB  -------
i=-.25; i.sl=0; j=1; j.sl=-2; m=-2; m.sl=2
vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Area_Bot:TranDir,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain ),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i), c(i.sl, i.sl ), h=c(j, j, j), c(j.sl,  j.sl),  k=c(m,m,m), c(m.sl, m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme)
vert.ah.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzDira.SzFireh.nlme, random=k~1|Shrub)  
#----------
# Worst is   a.int/a.Area_Bot, then   a.TranDirS then a.Area_Bot:TranDirS (7)
# Next, remove   a.Area_Bot:TranDirS  --> Step    AC

#Step AC     From Step AB, remove a.Area_Bot:TranDirS  -------
i=-.25; i.sl=0; j=1; j.sl=-2; m=-2; m.sl=2
vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Fire + Rain ),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i), c(i.sl ), h=c(j, j, j), c(j.sl,  j.sl),  k=c(m,m,m), c(m.sl, m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme)
vert.ah.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Firehk.Raink.SzFireh.nlme, random=k~1|Shrub)  
#----------
# Worst is   k.FireUB,    (7)
# Next, remove  k.FireUB   --> Step   AD  

#Step AD     From Step AC, remove k.FireUB  -------
i=-.25; i.sl=2; j=1; j.sl=-2; m=-2.5; m.sl=2
vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i), c(i.sl ), h=c(j, j, j), c(j.sl,  j.sl),  k=c(m,m,m), c(m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme)
vert.ah.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirahk.Fireh.Raink.SzFireh.nlme, random=k~1|Shrub)  
#----------
# Worst is a.Int, then a.TranDirS (7)    
# Next, remove  a.TranDirS  --> Step  AE  

#Step AE     From Step AD, remove a.TranDirS  -------
i=-.25; i.sl=2; j=1; j.sl=-2; m=-2.5; m.sl=2
vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i), c(i.sl ), h=c(j, j, j), c(j.sl,  j.sl),  k=c(m,m,m), c(m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme)
qqnorm(vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme)
vert.ah.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szahk.Dirhk.Fireh.Raink.SzFireh.nlme, random=k~1|Shrub)  
#----------
# Worst is k.(Intercept), but others signif  then a.Area_Bot (7)    
# Next, remove a.Area_Bot    --> Step    AF

#Step AF     From Step AE, remove a.Area_Bot  -------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:Fire,
                    k ~Area_Bot + TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i),  h=c(j, j, j), c(j.sl,  j.sl),  k=c(m,m,m), c(m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme)
qqnorm(vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme, abline=c(0,1))
plot(vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme)
vert.ah.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme, random=a+h~1|Shrub)
vert.ak.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme, random=a+k~1|Shrub)
vert.hk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme, random=h+k~1|Shrub)  
vert.a.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme, random=a~1|Shrub)
vert.h.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme, random=h~1|Shrub)
vert.k.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szhk.Dirhk.Fireh.Raink.SzFireh.nlme, random=k~1|Shrub)  
#----------
# Worst is k.Area_Bot (7)    
# Next, emove k.Area_Bot  --> Step    AG 

#Step AG     From Step AF, remove k.Area_Bot  -------
i=-.5; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot + TranDir + Fire + Area_Bot:Fire,
                    k ~TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i),  h=c(j, j, j), c(j.sl,  j.sl),  k=c(m,m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme)
qqnorm(vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme, abline=c(0,1))
plot(vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme)
vert.ah.Szh.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme, random=a+h~1|Shrub)
vert.ak.Szh.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme, random=a+k~1|Shrub)
vert.hk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme, random=h+k~1|Shrub)  
vert.a.Szh.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme, random=a~1|Shrub)
vert.h.Szh.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme, random=h~1|Shrub)
vert.k.Szh.Dirhk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.SzFireh.nlme, random=k~1|Shrub)  
#----------
# Worst is a (nonsense), then   h.FireUB, but interxn, then h.TranDirS (6)
# Worst is   h.FireUB, but interxn, h.Area_Bot:FireUB (1)
# Next, remove h.TranDirS --> Step    AH 
# Next, remove h.Area_Bot:FireUB --> Step    AO

#Step AH     From Step AG, remove h.TranDirS  -------
i=-.25; i.sl=2; j=1.25; j.sl=-2; m=-2.5; m.sl=-2
vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot +  Fire + Area_Bot:Fire,
                    k ~TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i),  h=c(j, j), c(j.sl,  j.sl),  k=c(m,m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme)
qqnorm(vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme, abline=c(0,1))
plot(vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme)
i=-.25; i.sl=1; j=1.00; j.sl=2; m=-2.5; m.sl=-2
vert.ah.Szh.Dirk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme, random=a+h~1|Shrub, start=c(a=c(i),  h=c(j, j), c(j.sl,  j.sl),  k=c(m,m,m)))
i=-.50; i.sl=1; j=1.00; j.sl=0; m=-1.5; m.sl=-2
vert.ak.Szh.Dirk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme, random=a+k~1|Shrub, start=c(a=c(i),  h=c(j, j), c(j.sl,  j.sl),  k=c(m,m,m)))
vert.hk.Szh.Dirk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme, random=h+k~1|Shrub)  
vert.a.Szh.Dirk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme, random=a~1|Shrub)
vert.h.Szh.Dirk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme, random=h~1|Shrub)
vert.k.Szh.Dirk.Fireh.Raink.SzFireh.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.SzFireh.nlme, random=k~1|Shrub)  
#----------
# Worst is  h.FireUB , but must remove h.Area_Bot:FireUB first (also NS) (2)
# Worst is   h.Int, but would need to must remove h.Area_Bot:FireUB first (also NS) (1)
# worse is h.Area_Bot:FireUB (3)
# Next,  h.Area_Bot:FireUB   --> Step  AI  

#Step AI     From Step AH, remove h.Area_Bot:FireUB  -------
i=-.25; i.sl=2; j=1.00; j.sl=0; m=-1.5; m.sl=-2
vert.ahk.Szh.Dirk.Fireh.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot +  Fire,
                    k ~TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),   
         start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szh.Dirk.Fireh.Raink.nlme)
qqnorm(vert.ahk.Szh.Dirk.Fireh.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Szh.Dirk.Fireh.Raink.nlme)
i=-.5; i.sl=2; j=1.00; j.sl=2; m=-2; m.sl=-2
vert.ah.Szh.Dirk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.nlme, random=a+h~1|Shrub, start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m,m)))
vert.ak.Szh.Dirk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.nlme, random=a+k~1|Shrub)
i=-.25; i.sl=2; j=1.50; j.sl=-2; m=-2.5; m.sl=-2
vert.hk.Szh.Dirk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.nlme, random=h+k~1|Shrub, start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m,m)))  
i=-.25; i.sl=2; j=1.0; j.sl=0; m=-2; m.sl=-2
vert.a.Szh.Dirk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.nlme, random=a~1|Shrub, start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m,m)))
i=-.5; i.sl=2; j=1.25; j.sl=2; m=-2.5; m.sl=-2
vert.h.Szh.Dirk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.nlme, random=h~1|Shrub, start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m,m)))
i=-.5; i.sl=1; j=1; j.sl=0; m=-2; m.sl=-2
vert.k.Szh.Dirk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirk.Fireh.Raink.nlme, random=k~1|Shrub, start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m,m)))  
#----------
# Worst is   h.FireUB (6)
# Worst is   h.Area_Bot (1)
# Next, remove  h.FireUB     --> Step  AJ 
# Next, remove h.Area_Bot --> Step    AN

#Step AJ     From Step AI, remove h.FireUB  -------
i=-.25; i.sl=1; j=1.50; j.sl=0; m=-2; m.sl=-2
vert.ahk.Szh.Dirk.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot,
                    k ~TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),   
         start=c(a=c(i),  h=c(j), c(j.sl),  k=c(m,m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szh.Dirk.Raink.nlme)
qqnorm(vert.ahk.Szh.Dirk.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Szh.Dirk.Raink.nlme)
vert.ah.Szh.Dirk.Raink.nlme=update(vert.ahk.Szh.Dirk.Raink.nlme, random=a+h~1|Shrub)
vert.ak.Szh.Dirk.Raink.nlme=update(vert.ahk.Szh.Dirk.Raink.nlme, random=a+k~1|Shrub)
vert.hk.Szh.Dirk.Raink.nlme=update(vert.ahk.Szh.Dirk.Raink.nlme, random=h+k~1|Shrub)  
vert.a.Szh.Dirk.Raink.nlme=update(vert.ahk.Szh.Dirk.Raink.nlme, random=a~1|Shrub)
vert.h.Szh.Dirk.Raink.nlme=update(vert.ahk.Szh.Dirk.Raink.nlme, random=h~1|Shrub)
vert.k.Szh.Dirk.Raink.nlme=update(vert.ahk.Szh.Dirk.Raink.nlme, random=k~1|Shrub)  
#----------
# Worst is   k.TranDirS (5)
# Worst is    h.Area_Bot (2 )
# Next, remove   k.TranDirS     --> Step   AK   
# Next, remove  h.Area_Bot --> Step    AL

#Step AK     From Step AJ, remove  k.TranDirS  -------
i=-.25; i.sl=1; j=1.50; j.sl=0; m=-2; m.sl=-2
vert.ahk.Szh.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot,
                    k ~ Rain ),
         random=a+h+k~1|as.factor(Shrub),   
         start=c(a=c(i),  h=c(j), c(j.sl),  k=m, m.sl), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szh.Raink.nlme)
qqnorm(vert.ahk.Szh.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Szh.Raink.nlme)
vert.ah.Szh.Raink.nlme=update(vert.ahk.Szh.Raink.nlme, random=a+h~1|Shrub)  #CANDIDATE
vert.ak.Szh.Raink.nlme=update(vert.ahk.Szh.Raink.nlme, random=a+k~1|Shrub)
vert.hk.Szh.Raink.nlme=update(vert.ahk.Szh.Raink.nlme, random=h+k~1|Shrub)  
vert.a.Szh.Raink.nlme=update(vert.ahk.Szh.Raink.nlme, random=a~1|Shrub)  #CANDIDATE
vert.h.Szh.Raink.nlme=update(vert.ahk.Szh.Raink.nlme, random=h~1|Shrub)  #CANDIDATE
vert.k.Szh.Raink.nlme=update(vert.ahk.Szh.Raink.nlme, random=k~1|Shrub)   #CANDIDATE 
#----------
# Worst is  none  ( 7 )   Stop!                     #CANDIDATE 

#Step AL     From Step AK, remove h.Area_Bot  -------
i=-.25; i.sl=-0.25; j=1.25; j.sl=0; m=-2.5; m.sl=0
vert.ah.Dirk.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~1,
                    k ~TranDir + Rain ),
         random=a+h~1|as.factor(Shrub),   #ahk Wont Converge
         start=c(a=i,  h=j,   k=c(m,m), m.sl), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ah.Dirk.Raink.nlme)
qqnorm(vert.ah.Dirk.Raink.nlme, abline=c(0,1))
plot(vert.ah.Dirk.Raink.nlme)
#vert.ah.Dirk.Raink.nlme=update(vert.ahk.Dirk.Raink.nlme, random=a+h~1|Shrub)
vert.ak.Dirk.Raink.nlme=update(vert.ah.Dirk.Raink.nlme, random=a+k~1|Shrub) #Won't converge
i=-.25; i.sl=-0.25; j=1.00; j.sl=0; m=-2; m.sl=0
vert.hk.Dirk.Raink.nlme=update(vert.ah.Dirk.Raink.nlme, random=h+k~1|Shrub, start=c(a=i,  h=j,   k=c(m,m), m.sl))  
vert.a.Dirk.Raink.nlme=update(vert.ah.Dirk.Raink.nlme, random=a~1|Shrub)
vert.h.Dirk.Raink.nlme=update(vert.ah.Dirk.Raink.nlme, random=h~1|Shrub)  #CANDIDATE
vert.k.Dirk.Raink.nlme=update(vert.ah.Dirk.Raink.nlme, random=k~1|Shrub)  
#----------
# Worst is  none  ( 2 )
# Worst is   k.TranDirS   ( 2 )
# Next, remove   k.TranDirS     --> Step  AM        #CANDIDATE

#Step AM     From Step AL, remove k.TranDirS  -------
i=-.25; i.sl=-0.25; j=1.25; j.sl=0; m=-2.5; m.sl=-2
vert.ahk.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~1,
                    k ~ Rain ),
         random=a+h+k~1|as.factor(Shrub),  
         start=c(a=i,  h=j,   k=m, m.sl), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Raink.nlme)
qqnorm(vert.ahk.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Raink.nlme)
i=-.5; i.sl=-0.25; j=1.25; j.sl=0; m=-2.0; m.sl=-2
vert.ah.Raink.nlme=update(vert.ahk.Raink.nlme, random=a+h~1|Shrub, start=c(a=i,  h=j,   k=m, m.sl))  #CANDIDATE
vert.ak.Raink.nlme=update(vert.ahk.Raink.nlme, random=a+k~1|Shrub) 
i=-.25; i.sl=-0.25; j=1.5; j.sl=0; m=-1.5; m.sl=-2
vert.hk.Raink.nlme=update(vert.ahk.Raink.nlme, random=h+k~1|Shrub, start=c(a=i,  h=j,   k=m, m.sl))  
vert.a.Raink.nlme=update(vert.ahk.Raink.nlme, random=a~1|Shrub)  #CANDIDATE
vert.h.Raink.nlme=update(vert.ahk.Raink.nlme, random=h~1|Shrub)   #CANDIDATE
i=-.25; i.sl=-0.25; j=1.0; j.sl=0; m=-2.5; m.sl=-2
vert.k.Raink.nlme=update(vert.ahk.Raink.nlme, random=k~1|Shrub, start=c(a=i,  h=j,   k=m, m.sl))   #CANDIDATE
#----------
# Worst is  none  ( 7  )  STOP!!

#Step AN     From Step AI, remove h.Area_Bot  -------
i=-.50; i.sl=2; j=1.50; j.sl=0; m=-2.0; m.sl=-2
vert.ahk.Dirk.Fireh.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~ Fire,
                    k ~TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),   
         start=c(a=c(i),  h=c(j, j),   k=c(m,m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Dirk.Fireh.Raink.nlme)
qqnorm(vert.ahk.Dirk.Fireh.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Dirk.Fireh.Raink.nlme)
i=-.25; i.sl=2; j=1.25; j.sl=0; m=-1.5; m.sl=0
vert.ah.Dirk.Fireh.Raink.nlme=update(vert.ahk.Dirk.Fireh.Raink.nlme, random=a+h~1|Shrub, start=c(a=c(i),  h=c(j, j),   k=c(m,m,m)))
vert.ak.Dirk.Fireh.Raink.nlme=update(vert.ahk.Dirk.Fireh.Raink.nlme, random=a+k~1|Shrub)  #WONT CONVERGE
vert.hk.Dirk.Fireh.Raink.nlme=update(vert.ahk.Dirk.Fireh.Raink.nlme, random=h+k~1|Shrub)  
vert.a.Dirk.Fireh.Raink.nlme=update(vert.ahk.Dirk.Fireh.Raink.nlme, random=a~1|Shrub)
vert.h.Dirk.Fireh.Raink.nlme=update(vert.ahk.Dirk.Fireh.Raink.nlme, random=h~1|Shrub)
i=-.25; i.sl=2; j=1; j.sl=-2; m=-2.5; m.sl=0
vert.k.Dirk.Fireh.Raink.nlme=update(vert.ahk.Dirk.Fireh.Raink.nlme, random=k~1|Shrub, start=c(a=c(i),  h=c(j, j),   k=c(m,m,m)))  
#----------
# Worst is   h.FireUB (6)
# Next, remove h.FireUB  --> Step AL

#Step AO     From Step AG, remove h.Area_Bot:FireUB  -------
i=-.25; i.sl=2; j=1; j.sl=-2; m=-2; m.sl=-2
vert.ahk.Szh.Dirhk.Fireh.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot + TranDir + Fire,
                    k ~TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szh.Dirhk.Fireh.Raink.nlme)
qqnorm(vert.ahk.Szh.Dirhk.Fireh.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Szh.Dirhk.Fireh.Raink.nlme)
i=-.25; i.sl=2; j=1; j.sl=-2; m=-1.5; m.sl=-2
vert.ah.Szh.Dirhk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.nlme, random=a+h~1|Shrub,start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m,m)))
i=-.5; i.sl=2; j=1.5; j.sl=-2; m=-2.5; m.sl=-2
vert.ak.Szh.Dirhk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.nlme, random=a+k~1|Shrub,start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m,m)))
vert.hk.Szh.Dirhk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.nlme, random=h+k~1|Shrub)  
i=-.25; i.sl=2; j=1.25; j.sl=2; m=-2.5; m.sl=-2
vert.a.Szh.Dirhk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.nlme, random=a~1|Shrub,start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m,m)))
i=-.5; i.sl=2; j=1.00; j.sl=2; m=-1.5; m.sl=-2
vert.h.Szh.Dirhk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.nlme, random=h~1|Shrub,start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m,m)))
i=-.5; i.sl=1.5; j=1.5; j.sl=-2; m=-2.5; m.sl=-2
vert.k.Szh.Dirhk.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirhk.Fireh.Raink.nlme, random=k~1|Shrub,start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m,m)))  
#----------
# Worst is  h.FireUB (2)  h.TranDirS(4)  k.TranDirS(1)
 
# Next, remove h.TranDirS --> Step    AI 
# Next, remove  h.FireUB  --> Step  AP
# Next, remove k.TranDirS  --> Step  AQ

#Step AP     From Step AO, remove h.FireUB  -------
i=-.25; i.sl=2; j=1.5; j.sl=2; m=-2.5; m.sl=-2
vert.ahk.Szh.Dirhk.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot + TranDir,
                    k ~TranDir + Rain ),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szh.Dirhk.Raink.nlme)
qqnorm(vert.ahk.Szh.Dirhk.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Szh.Dirhk.Raink.nlme)
vert.ah.Szh.Dirhk.Raink.nlme=update(vert.ahk.Szh.Dirhk.Raink.nlme, random=a+h~1|Shrub)
vert.ak.Szh.Dirhk.Raink.nlme=update(vert.ahk.Szh.Dirhk.Raink.nlme, random=a+k~1|Shrub)
vert.hk.Szh.Dirhk.Raink.nlme=update(vert.ahk.Szh.Dirhk.Raink.nlme, random=h+k~1|Shrub)  
i=-.5; i.sl=2; j=1.25; j.sl=0; m=-2.0; m.sl=-2
vert.a.Szh.Dirhk.Raink.nlme=update(vert.ahk.Szh.Dirhk.Raink.nlme, random=a~1|Shrub,start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m,m)))
vert.h.Szh.Dirhk.Raink.nlme=update(vert.ahk.Szh.Dirhk.Raink.nlme, random=h~1|Shrub)
vert.k.Szh.Dirhk.Raink.nlme=update(vert.ahk.Szh.Dirhk.Raink.nlme, random=k~1|Shrub)  
#----------
# Worst is h.TranDirS (7)
# Next, remove h.TranDirS  --> Step  AJ    

#Step AQ     From Step AO, remove k.TranDirS  -------
i=-.25; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Szh.Dirh.Fireh.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot + TranDir + Fire,
                    k ~ Rain ),
         random=a+h+k~1|as.factor(Shrub),  
         start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szh.Dirh.Fireh.Raink.nlme)
qqnorm(vert.ahk.Szh.Dirh.Fireh.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Szh.Dirh.Fireh.Raink.nlme)
i=-.25; i.sl=1; j=1.25; j.sl=-2; m=-2; m.sl=-2
vert.ah.Szh.Dirh.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirh.Fireh.Raink.nlme, random=a+h~1|Shrub,         start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m)))
i=-.5; i.sl=1; j=1.5; j.sl=0; m=-2.5; m.sl=-2
vert.ak.Szh.Dirh.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirh.Fireh.Raink.nlme, random=a+k~1|Shrub, start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m)))
i=-.25; i.sl=1; j=1.5; j.sl=0; m=-2.5; m.sl=-2
vert.hk.Szh.Dirh.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirh.Fireh.Raink.nlme, random=h+k~1|Shrub, start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m)))  
i=-.25; i.sl=1; j=1.5; j.sl=0; m=-1.5; m.sl=-2
vert.a.Szh.Dirh.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirh.Fireh.Raink.nlme, random=a~1|Shrub, start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m)))
i=-.25; i.sl=1; j=1.25; j.sl=0; m=-1.5; m.sl=-2
vert.h.Szh.Dirh.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirh.Fireh.Raink.nlme, random=h~1|Shrub, start=c(a=c(i),  h=c(j, j, j), c(j.sl),  k=c(m,m)))
i=-.5; i.sl=1; j=1.5; j.sl=-2; m=-2.5; m.sl=-2
vert.k.Szh.Dirh.Fireh.Raink.nlme=update(vert.ahk.Szh.Dirh.Fireh.Raink.nlme, random=k~1|Shrub)  
#----------
# Worst is   h.TranDirS (3) none (1)  h.Area_Bot (2)  h.FireUB (1)
# Next, remove  h.TranDirS --> Step  AR 
# Next, remove  h.Area_Bot --> Step   AU
# Next, remove  h.FireUB  --> Step AV

#Step AR     From Step AQ, remove h.TranDirS  -------
i=-.25; i.sl=1; j=1; j.sl=-2; m=-1.5; m.sl=-2
vert.ah.Szh.Fireh.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot + Fire,
                    k ~ Rain ),
         random=a+h~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m)),    
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ah.Szh.Fireh.Raink.nlme)
qqnorm(vert.ah.Szh.Fireh.Raink.nlme, abline=c(0,1))
plot(vert.ah.Szh.Fireh.Raink.nlme)
#i=-.25; i.sl=1; j=1.25; j.sl=-2; m=-2; m.sl=-2
#vert.ah.Szh.Fireh.Raink.nlme=update(vert.ah.Szh.Fireh.Raink.nlme, random=a+h~1|Shrub)
i=-.5; i.sl=1; j=1; j.sl=2; m=-1.5; m.sl=-2
vert.ak.Szh.Fireh.Raink.nlme=update(vert.ah.Szh.Fireh.Raink.nlme, random=a+k~1|Shrub,start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m)))
i=-.5; i.sl=1; j=1.25; j.sl=2; m=-1.5; m.sl=-2
vert.hk.Szh.Fireh.Raink.nlme=update(vert.ah.Szh.Fireh.Raink.nlme, random=h+k~1|Shrub,start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m)))  
i=-.5; i.sl=1; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.a.Szh.Fireh.Raink.nlme=update(vert.ah.Szh.Fireh.Raink.nlme, random=a~1|Shrub,start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m)))
i=-.5; i.sl=1; j=1.25; j.sl=-2; m=-2.5; m.sl=-2
vert.h.Szh.Fireh.Raink.nlme=update(vert.ah.Szh.Fireh.Raink.nlme, random=h~1|Shrub,start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m)))
i=-.5; i.sl=1; j=1.5; j.sl=-2; m=-2.5; m.sl=-2
vert.k.Szh.Fireh.Raink.nlme=update(vert.ah.Szh.Fireh.Raink.nlme, random=k~1|Shrub)  
#----------
# Worst is    h.FireUB (3)  h.Area_Bot(3)
# Next, remove  h.FireUB  --> Step  AK 
# Next, remove  h.Area_Bot  --> Step  AS
  
#Step AS     From Step AR, remove h.Area_Bot -------
i=-.25; i.sl=1; j=1; j.sl=-2; m=-1.5; m.sl=-2
vert.ah.Fireh.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~ Fire,
                    k ~ Rain ),
         random=a+h~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i),  h=c(j, j),   k=c(m,m)),    
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 because it only had one non-zero quadrat
summary(vert.ah.Fireh.Raink.nlme)
qqnorm(vert.ah.Fireh.Raink.nlme, abline=c(0,1))
plot(vert.ah.Fireh.Raink.nlme)
#i=-.25; i.sl=1; j=1.25; j.sl=-2; m=-2; m.sl=-2
#vert.ah.Fireh.Raink.nlme=update(vert.ah.Fireh.Raink.nlme, random=a+h~1|Shrub)
vert.ak.Fireh.Raink.nlme=update(vert.ah.Fireh.Raink.nlme, random=a+k~1|Shrub) #WONT CONVERGE
i=-.5; i.sl=1; j=1.5; j.sl=-2; m=-2.5; m.sl=-2
vert.hk.Fireh.Raink.nlme=update(vert.ah.Fireh.Raink.nlme, random=h+k~1|Shrub,start=c(a=c(i),  h=c(j, j),   k=c(m,m)))  
i=-.25; i.sl=1; j=1; j.sl=-2; m=-2; m.sl=-2
vert.a.Fireh.Raink.nlme=update(vert.ah.Fireh.Raink.nlme, random=a~1|Shrub,start=c(a=c(i),  h=c(j, j),   k=c(m,m)))
vert.h.Fireh.Raink.nlme=update(vert.ah.Fireh.Raink.nlme, random=h~1|Shrub)   #CANDIDATE
vert.k.Fireh.Raink.nlme=update(vert.ah.Fireh.Raink.nlme, random=k~1|Shrub)  
#----------
# Worst is    h.FireUB(4) none(1)  -> Step AT 
# Next, remove  h.FireUB  --> Step     AM   STOP      1 CANDIDATE

#Step AT     From Step AS, remove h.FireUB -------  
i=-.5; i.sl=1; j=1.25; j.sl=-2; m=-2; m.sl=-2
vert.ah.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~ 1,
                    k ~ Rain ),
         random=a+h~1|as.factor(Shrub),    #ahk won't converge
         start=c(a=c(i),  h=c(j),   k=c(m,m)),    
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 because it only had one non-zero quadrat
summary(vert.ah.Raink.nlme)
qqnorm(vert.ah.Raink.nlme, abline=c(0,1))
plot(vert.ah.Raink.nlme)
#vert.ah.Raink.nlme=update(vert.ah.Raink.nlme, random=a+h~1|Shrub)
vert.ak.Raink.nlme=update(vert.ah.Raink.nlme, random=a+k~1|Shrub) 
i=-.25; i.sl=1; j=1.5; j.sl=-2; m=-2; m.sl=-2
vert.hk.Raink.nlme=update(vert.ah.Raink.nlme, random=h+k~1|Shrub, start=c(a=c(i),  h=c(j),   k=c(m,m)))  
vert.a.Raink.nlme=update(vert.ah.Raink.nlme, random=a~1|Shrub)  #CANDIDATE
vert.h.Raink.nlme=update(vert.ah.Raink.nlme, random=h~1|Shrub)    #CANDIDATE 
vert.k.Raink.nlme=update(vert.ah.Raink.nlme, random=k~1|Shrub)   #CANDIDATE 
#---------- 
# Worst is  none   STOP    3 CANDIDATES

#Step AU     From Step AQ, remove h.Area_Bot  -------
i=-.25; i.sl=2; j=1; j.sl=-2; m=-1.5; m.sl=-2
vert.ahk.Dirh.Fireh.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~ TranDir + Fire,
                    k ~ Rain ),
         random=a+h+k~1|as.factor(Shrub),  
         start=c(a=c(i),  h=c(j, j, j),  k=c(m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Dirh.Fireh.Raink.nlme)
qqnorm(vert.ahk.Dirh.Fireh.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Dirh.Fireh.Raink.nlme)
i=-.25; i.sl=2; j=1.25; j.sl=-2; m=-1.5; m.sl=-2
vert.ah.Dirh.Fireh.Raink.nlme=update(vert.ahk.Dirh.Fireh.Raink.nlme, random=a+h~1|Shrub, start=c(a=c(i),  h=c(j, j, j),  k=c(m,m)))
i=-.25; i.sl=2; j=1.5; j.sl=-2; m=-2.0; m.sl=-2
vert.ak.Dirh.Fireh.Raink.nlme=update(vert.ahk.Dirh.Fireh.Raink.nlme, random=a+k~1|Shrub, start=c(a=c(i),  h=c(j, j, j),  k=c(m,m)))
vert.hk.Dirh.Fireh.Raink.nlme=update(vert.ahk.Dirh.Fireh.Raink.nlme, random=h+k~1|Shrub)  
i=-.25; i.sl=2; j=1.5; j.sl=-2; m=-1.5; m.sl=-2
vert.a.Dirh.Fireh.Raink.nlme=update(vert.ahk.Dirh.Fireh.Raink.nlme, random=a~1|Shrub, start=c(a=c(i),  h=c(j, j, j),  k=c(m,m)))
vert.hk.Dirh.Fireh.Raink.nlme=update(vert.ahk.Dirh.Fireh.Raink.nlme, random=h+k~1|Shrub)  
i=-.25; i.sl=2; j=1; j.sl=-2; m=-2.5; m.sl=-2
vert.h.Dirh.Fireh.Raink.nlme=update(vert.ahk.Dirh.Fireh.Raink.nlme, random=h~1|Shrub, start=c(a=c(i),  h=c(j, j, j),  k=c(m,m)))
i=-.25; i.sl=2; j=1; j.sl=-2; m=-2.5; m.sl=-2
vert.k.Dirh.Fireh.Raink.nlme=update(vert.ahk.Dirh.Fireh.Raink.nlme, random=k~1|Shrub, start=c(a=c(i),  h=c(j, j, j),  k=c(m,m)))  
#----------
# Worst is   h.TranDirS (7)
# Next, remove  h.TranDirS  --> Step   AS   STOP 

#Step AV     From Step AQ, remove h.FireUB  -------
i=-.5; i.sl=2; j=1; j.sl=0; m=-1.5; m.sl=-2
vert.ahk.Szh.Dirh.Raink.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~1,
                    h ~Area_Bot + TranDir,
                    k ~ Rain ),
         random=a+h+k~1|as.factor(Shrub),  
         start=c(a=c(i),  h=c(j, j), c(j.sl),  k=c(m,m)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szh.Dirh.Raink.nlme)
qqnorm(vert.ahk.Szh.Dirh.Raink.nlme, abline=c(0,1))
plot(vert.ahk.Szh.Dirh.Raink.nlme)

vert.ah.Szh.Dirh.Raink.nlme=update(vert.ahk.Szh.Dirh.Raink.nlme, random=a+h~1|Shrub)
vert.ak.Szh.Dirh.Raink.nlme=update(vert.ahk.Szh.Dirh.Raink.nlme, random=a+k~1|Shrub)
vert.hk.Szh.Dirh.Raink.nlme=update(vert.ahk.Szh.Dirh.Raink.nlme, random=h+k~1|Shrub) 
i=-.25; i.sl=2; j=1; j.sl=0; m=-1.5; m.sl=-2
vert.a.Szh.Dirh.Raink.nlme=update(vert.ahk.Szh.Dirh.Raink.nlme, random=a~1|Shrub)
vert.h.Szh.Dirh.Raink.nlme=update(vert.ahk.Szh.Dirh.Raink.nlme, random=h~1|Shrub)
vert.k.Szh.Dirh.Raink.nlme=update(vert.ahk.Szh.Dirh.Raink.nlme, random=k~1|Shrub)  
#----------
# Worst is    h.TranDirS (7)
# Next, remove  h.TranDirS  --> Step   AK  STOP 


#Step AW     From Step G, remove a.TranDirS:FireUB  --------
i=-.5; i.sl=2; j=1.5; j.sl=-2; m=-2; m.sl=2
vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme=
    nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain  + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:Rain,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j, j,j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
         data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
summary(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme)
qqnorm(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme, abline=c(0,1))
plot(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme)
#START HERE
vert.ah.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme, random=a+h~1|Shrub)
vert.ak.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme, random=a+k~1|Shrub)
vert.hk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme, random=h+k~1|Shrub)  
vert.a.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme, random=a~1|Shrub)
vert.h.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme, random=h~1|Shrub)
vert.k.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme=update(vert.ahk.Szahk.Dirahk.Fireahk.Rainahk.SzDirah.SzFireahk.SzRaina.FireRainah.nlme, random=k~1|Shrub)  
#----------
# Worst is   
# Next, remove     --> Step    









#CANDIDATE MODELS

AIC(vert.ah.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme) #318.5451 
AIC(vert.ak.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme) #318.5451 
AIC(vert.hk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme) #319.7501
 AIC(vert.a.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme) # 314.5451
 AIC(vert.h.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme) #315.7497 
 AIC(vert.k.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme) #314.5451
#Did resid plot and qqnorm on all above and all were fine

#Higher AIC than 320--------
AIC(vert.hk.Szk.Dira.Firek.Raink.SzFirek.nlme)
AIC(vert.a.Szk.Dira.Firek.Raink.SzFirek.nlme)
AIC(vert.h.Szk.Dira.Firek.Raink.SzFirek.nlme)
AIC(vert.k.Szk.Dira.Firek.Raink.SzFirek.nlme)
AIC(vert.a.Szak.Nointa.Firek.Raink.SzFirek.nlme)
AIC(vert.h.Szak.Nointa.Firek.Raink.SzFirek.nlme)
AIC(vert.k.Szak.Nointa.Firek.Raink.SzFirek.nlme)
AIC(vert.ah.Szh.Raink.nlme)
AIC(vert.a.Szh.Raink.nlme)
AIC(vert.h.Szh.Raink.nlme)
AIC(vert.k.Szh.Raink.nlme)
AIC(vert.h.Dirk.Raink.nlme)
AIC(vert.ah.Raink.nlme)
AIC(vert.a.Raink.nlme)
AIC(vert.h.Raink.nlme)
AIC(vert.k.Raink.nlme)
AIC(vert.h.Fireh.Raink.nlme)
AIC(vert.a.Raink.nlme)
AIC(vert.h.Raink.nlme)
AIC(vert.k.Raink.nlme)
#----------

ErodMoj.models=list(
vert.ah.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme,
vert.ak.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme,
vert.hk.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme,
vert.a.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme,
vert.h.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme,
vert.k.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme)

save(ErodMoj.models,file="ErodMojCandidateModels")
load("ErodMojCandidateModels")
unlist(ErodMoj.models)


vert.ah.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme=ErodMoj.models[[1]]

lapply(ErodMoj.models, coef)
fixed.effects(vert.ah.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme) #Fixed effect parameters

VarCorr(vert.ak.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme)[,2]  # variance parameters

c(fixed.effects(vert.ak.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme), VarCorr(vert.ak.Szahk.Dirah.Firek.Raink.SzDira.SzFirek.nlme)[,2] )
fixef=lapply(ErodMoj.models, fixed.effects)
varpars=lapply(ErodMoj.models, function(x) VarCorr(x)[,2])

##Combine these somehow to make df

unlist(blurf)
class(blurf)
df=data.frame(matrix(unlist(blurf), ncol=length(blurf[[1]]), byrow=T))
names(df)=names(blurf[[1]])
write.csv(df, file="VegSpatialModeling/ErodMojfixed.csv", row.names=F)



#  Test with Census.Test---------- 


INVlogit.transform(predict(ErodMoj.models[[1]], newdata=Census.Test, level=0))


predict(ErodMoj.models[[1]], newdata=Census.Test, level=0)

plot(Census.Test$LogitTarg, predict(ErodMoj.models[[1]], newdata=Census.Test, level=0))
abline(0,1)
Census.Test  
Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,]$Target
Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,]$LogitTarg
=======
# Next, remove h.Area_Bot --> Step    ?????????

    
    
    