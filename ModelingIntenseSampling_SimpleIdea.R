

library(nlme)
library(spatstat)
library(devtools)
install_github("elmR", "emudrak")
library(elmR)

# must include transformation functions
# logit
# inv_logit
# logw0

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
  Shrubs$Shrub=as.numeric(as.character(Shrubs$ShrubID)) 
}

Shrubs[c("Shrub", "ShrubID")]

Census=merge(Shrubs[,c("Shrub","Area_Bot","Vol_Bot","Vol_Stem")], CensusData,  by.y="Shrub", by.x="Shrub")

Census$Area_Bot=Census$Area_Bot/10000
Census$Vol_Bot=Census$Vol_Bot/1000000
Census$Vol_Stem=Census$Vol_Stem/1000000

Census=Census[order(Census[,"Shrub"], Census[,"MHcode"]), ]
names(Census)[1]="Shrub"

# Chose target species to model this time --------------
TargSpec="ErodCover"; ylabel="% Cover" 
#TargSpec="Erodium"; ylabel="# Plants" 

Census$Target=round(Census[,TargSpec]) #Round to deal with issue of 0.1 percent cover values
Census$LogitTarg=logit(Census$Target/100)
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

#Plot With transformed response:##################################

# Setup for elaborate plotting 
firecols=c("gray40", "green")
watercols=c("lightblue", "tan")
dircols=c("red","blue")

#Set scale ranges for this target
xplotmax=max(Census.Train[,"PlotDist"], na.rm=TRUE)
yplotmax=max(Census.Train[,"LogitTarg"])

#yplotmax=max(Census.Train$LogitTarg)
hist(Census.Train$LogitTarg)


plotscheme="fire"
plotscheme="rain"
plotscheme="direction"

with(Census.Train, plot(c(1,xplotmax), range(Census.Train$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("Logit transformmed ", ylabel), xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber
    theseplots=Census.Train[Census.Train$Shrub==i,]
    if (plotscheme=="fire") mycol=firecols[theseplots$Fire[1] ]     
    else if (plotscheme=="rain") mycol=watercols[theseplots$Rain[1] ]    
    else if (plotscheme=="direction") mycol=firecols[theseplots$TranDir[1] ]
  lines(theseplots$PlotDist, theseplots$LogitTarg,   
        pch=20,lwd=2, type="o", lty=1,
        col=mycol  )
} # end of i loop
if (plotscheme=="fire") thesecols=firecols 
else if (plotscheme=="rain")  thesecols=watercols 
else if (plotscheme=="direction") thesecols=dircols   
legend(200,0.90* yplotmax, levels(Census.Train$Fire), col=thesecols, lwd=2, lty=1, bty="n") 


# 
# #Plot With % cover response:##################################
#Set scale ranges for this target
yplotmax=max(Census.Train[,"Target"])

plotscheme="fire"
plotscheme="rain"
plotscheme="direction"

with(Census.Train, plot(c(1,xplotmax), range(Census.Train$Target) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("Logit transformmed ", ylabel), xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber
    theseplots=Census.Train[Census.Train$Shrub==i,]
    if (plotscheme=="fire") mycol=firecols[theseplots$Fire[1] ]     else if (plotscheme=="rain") mycol=watercols[theseplots$Rain[1] ]    else if (plotscheme=="direction") mycol=dircols[theseplots$TranDir[1] ]
  lines(theseplots$PlotDist, theseplots$Target,   
        pch=20,lwd=2, type="o", lty=1,
        col=mycol  )
} # end of i loop
if (plotscheme=="fire") thesecols=firecols else if (plotscheme=="rain")  thesecols=watercols else if (plotscheme=="direction") thesecols=dircols   
legend(200,0.90* yplotmax, levels(Census.Train$Fire), col=thesecols, lwd=2, lty=1, bty="n") 

#---------------------------------------------------------------------------------------------

# Try a simple linear decreasing model using only values from Canopy dripline (i.e. MH Code >1.0)

# Or, average annual biomass within a range of influence (how to get this, unless linear decreasing model...)
#Dripline in cm

# Show only plots outside Canopy Dripline, (i.e. MH Code >1.0)----
# Choose one
plotscheme="fire"
plotscheme="rain"
plotscheme="direction"
with(Census.Train, plot(c(1,xplotmax), range(Census.Train$Target) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("Percent Cover ", ylabel), xlab="Distance from Shrub Stem" ))
for (i in unique(Census.Train$Shrub)) {  # i=ShrubNumber
    theseplots=Census.Train[Census.Train$Shrub==i & Census.Train$MHcode!=1.0,]
    if (plotscheme=="fire") mycol=firecols[theseplots$Fire[1] ]     else if (plotscheme=="rain") mycol=watercols[theseplots$Rain[1] ]    else if (plotscheme=="direction") mycol=dircols[theseplots$TranDir[1] ]
  lines(theseplots$PlotDist, theseplots$Target,   
        pch=20,lwd=2, type="o", lty=1,
        col=mycol  )
} # end of i loop
if (plotscheme=="fire") thesecols=firecols else if (plotscheme=="rain")  thesecols=watercols else if (plotscheme=="direction") thesecols=dircols   
legend(200,0.90* yplotmax, levels(Census.Train$Fire), col=thesecols, lwd=2, lty=1, bty="n") 



library(lme4)
library(lmerTest)

linmod1.lme=lmer(LogitTarg~PlotDist*Area_Bot*Fire*Rain+(1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod1.lme)
drop1(linmod1.lme)

linmod2.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^3+(1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod2.lme)
drop1(linmod2.lme)

linmod3.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2+PlotDist:Area_Bot:Fire+PlotDist:Area_Bot:Rain+PlotDist:Fire:Rain + (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod3.lme)
drop1(linmod3.lme)


linmod4.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2+PlotDist:Area_Bot:Fire+PlotDist:Area_Bot:Rain + (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod4.lme)
drop1(linmod4.lme, test="Chisq")


linmod5.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2+PlotDist:Area_Bot:Fire+ (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod5.lme)
drop1(linmod5.lme, test="Chisq")

linmod6.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2-Area_Bot:Rain+PlotDist:Area_Bot:Fire+ (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod6.lme)
drop1(linmod6.lme, test="Chisq")

linmod7.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2-Area_Bot:Rain - Fire:Rain+PlotDist:Area_Bot:Fire+ (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod7.lme)
drop1(linmod7.lme, test="Chisq")

linmod8.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2-Area_Bot:Rain - Fire:Rain-PlotDist:Rain+PlotDist:Area_Bot:Fire+ (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod8.lme)
drop1(linmod8.lme, test="Chisq")

linmod9.lme=lmer(LogitTarg~PlotDist + Area_Bot + Fire + Rain + PlotDist:Area_Bot+PlotDist:Fire+Area_Bot:Fire + (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod9.lme)
drop1(linmod9.lme, test="Chisq")

linmod10.lme=lmer(LogitTarg ~ PlotDist + Area_Bot + Fire + Rain + PlotDist:Area_Bot + Area_Bot:Fire + (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod10.lme)
drop1(linmod10.lme, test="Chisq")

linmod11.lme=lmer(LogitTarg ~ PlotDist + Area_Bot + Fire + Rain + PlotDist:Area_Bot + (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod11.lme)
drop1(linmod11.lme, test="Chisq")

linmod12.lme=lmer(LogitTarg ~ PlotDist + Area_Bot + Fire + Rain + (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod12.lme)
drop1(linmod12.lme, test="Chisq")

linmod13.lme=lmer(LogitTarg ~ PlotDist + Fire + Rain + (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod13.lme)
drop1(linmod13.lme, test="Chisq")

linmod14.lme=lmer(LogitTarg ~ Fire + Rain + (1|Shrub), data=Census.Train[Census.Train$MHcode>1,])
summary(linmod14.lme)
drop1(linmod14.lme, test="Chisq")
## FINAL - overly simple?  or easier because positive effect of PlotDist is weird?




#Try on Testing Data-----
Census.Test$Pred=NA
Census.Test[Census.Test$MHcode>1,"Pred"]=predict(linmod14.lme,  newdata=Census.Test[Census.Test$MHcode>1,], allow.new.levels=TRUE )


plot(inv_logit(Census.Test$LogitTarg), inv_logit(Census.Test$Pred))
#something is seriously wrong....

abline(0,1, lty=2)





# Try again with two-stage modeling------------ 
library(lme4)
library(lmerTest)


# presence/absense crosstabs ------
xtabs(~TargPres+MHcode+Rain+Fire+TranDir, data=Census.Train[Census.Train$MHcode>1,])
xtabs(~TargPres+Rain+Fire+TranDir, data=Census.Train[Census.Train$MHcode>1,])
xtabs(~Rain+Fire+TranDir, data=Census.Train[Census.Train$MHcode>1,])
xtabs(~TargPres+TranDir, data=Census.Train[Census.Train$MHcode>1,])
xtabs(~TargPres+Rain, data=Census.Train[Census.Train$MHcode>1,])
xtabs(~TargPres+Fire, data=Census.Train[Census.Train$MHcode>1,])

# Logistic modeling -----

hurdle.lme=glmer(TargPres~PlotDist*Area_Bot*Fire*Rain + (1|Shrub), family=binomial, data=Census.Train[Census.Train$MHcode>1,],control = glmerControl(optimizer = "bobyqa"))
summary(hurdle.lme)



hurdle2.lme=glmer(TargPres~(PlotDist+Area_Bot+Fire+Rain)^3 + (1|Shrub), family=binomial, data=Census.Train[Census.Train$MHcode>1,],control = glmerControl(optimizer = "bobyqa"))
summary(hurdle2.lme)
drop1(hurdle2.lme)


hurdle3.lme=glmer(TargPres~(PlotDist+Area_Bot+Fire+Rain)^2+Area_Bot:Fire:Rain+PlotDist:Area_Bot:Fire+PlotDist:Fire:Rain + (1|Shrub), family=binomial, data=Census.Train[Census.Train$MHcode>1,],control = glmerControl(optimizer = "bobyqa"))
summary(hurdle3.lme)
drop1(hurdle3.lme)


hurdle4.lme=glmer(TargPres~(PlotDist+Area_Bot+Fire+Rain)^2+Area_Bot:Fire:Rain+PlotDist:Fire:Rain + (1|Shrub), family=binomial, data=Census.Train[Census.Train$MHcode>1,],control = glmerControl(optimizer = "bobyqa"))
summary(hurdle4.lme)
drop1(hurdle4.lme)


hurdle5.lme=glmer(TargPres~(PlotDist+Area_Bot+Fire+Rain)^2-PlotDist:Area_Bot+Area_Bot:Fire:Rain+PlotDist:Fire:Rain + (1|Shrub), family=binomial, data=Census.Train[Census.Train$MHcode>1,],control = glmerControl(optimizer = "bobyqa"))
summary(hurdle5.lme)
drop1(hurdle5.lme)

hurdle6.lme=glmer(TargPres~(PlotDist+Area_Bot+Fire+Rain)^2-PlotDist:Area_Bot+PlotDist:Fire:Rain + (1|Shrub), family=binomial, data=Census.Train[Census.Train$MHcode>1,],control = glmerControl(optimizer = "bobyqa"))
summary(hurdle6.lme)
drop1(hurdle6.lme, test="Chisq")


hurdle7.lme=glmer(TargPres~PlotDist+Area_Bot+Fire+Rain+PlotDist:Fire+ PlotDist:Rain +Area_Bot:Rain+Fire:Rain+ PlotDist:Fire:Rain+ (1|Shrub), family=binomial, data=Census.Train[Census.Train$MHcode>1,],control = glmerControl(optimizer = "bobyqa"))
summary(hurdle7.lme)
drop1(hurdle7.lme, test="Chisq")


hurdle8.lme=glmer(TargPres~PlotDist+Area_Bot+Fire+Rain+PlotDist:Fire+ PlotDist:Rain +Fire:Rain+ PlotDist:Fire:Rain+ (1|Shrub), family=binomial, data=Census.Train[Census.Train$MHcode>1,],control = glmerControl(optimizer = "bobyqa"))
summary(hurdle8.lme)
drop1(hurdle8.lme, test="Chisq")


hurdle9.lme=glmer(TargPres~PlotDist+Fire+Rain+PlotDist:Fire+ PlotDist:Rain +Fire:Rain+ PlotDist:Fire:Rain+ (1|Shrub), family=binomial, data=Census.Train[Census.Train$MHcode>1,],control = glmerControl(optimizer = "bobyqa"))
summary(hurdle9.lme)
drop1(hurdle9.lme, test="Chisq")

#Go with this one for now???

coef(hurdle9.lme)
fixef(hurdle9.lme)

inv_logit(fixef(hurdle9.lme))

predict(hurdle9.lme, Census.Test, allow.new.levels=TRUE)  
#Not stochastic, uses population level data for previously unobserved levels
Census.Test$HurdPred=NA
Census.Test[Census.Test$MHcode>1,"HurdPred"]=predict(hurdle9.lme,  newdata=Census.Test[Census.Test$MHcode>1,], allow.new.levels=TRUE )

plot(jitter(TargPres,factor=0.2)~inv_logit(HurdPred), data=Census.Test[Census.Test$MHcode>1,], xlab="Probability of presence", ylab="Observed presence (jittered)" ) 


# Now model presence-only ------------
library(lme4)
library(lmerTest)


linmod1.lme=lmer(LogitTarg~PlotDist*Area_Bot*Fire*Rain+(1|Shrub), data=Census.Train[Census.Train$MHcode>1 & Census.Train$Target>0,])
summary(linmod1.lme)
drop1(linmod1.lme)

linmod2.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^3+(1|Shrub), data=Census.Train[Census.Train$MHcode>1 & Census.Train$Target>0,])
summary(linmod2.lme)
drop1(linmod2.lme)

linmod3.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^3-PlotDist:Area_Bot:Rain+(1|Shrub), data=Census.Train[Census.Train$MHcode>1 & Census.Train$Target>0,])
summary(linmod3.lme)
drop1(linmod3.lme)

linmod4.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^3-PlotDist:Area_Bot:Rain-PlotDist:Fire:Rain +(1|Shrub), data=Census.Train[Census.Train$MHcode>1 & Census.Train$Target>0,])
summary(linmod4.lme)
drop1(linmod4.lme)

linmod5.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^3-PlotDist:Rain-PlotDist:Area_Bot:Rain-PlotDist:Fire:Rain +(1|Shrub), data=Census.Train[Census.Train$MHcode>1 & Census.Train$Target>0,])
summary(linmod5.lme)
drop1(linmod5.lme)


linmod6.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2+Area_Bot:Fire:Rain+(1|Shrub), data=Census.Train[Census.Train$MHcode>1 & Census.Train$Target>0,])
summary(linmod6.lme)
drop1(linmod6.lme)


linmod7.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2-PlotDist:Rain+Area_Bot:Fire:Rain+(1|Shrub), data=Census.Train[Census.Train$MHcode>1 & Census.Train$Target>0,])
summary(linmod7.lme)
drop1(linmod7.lme)

linmod8.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2-PlotDist:Rain-PlotDist:Fire+Area_Bot:Fire:Rain+(1|Shrub), data=Census.Train[Census.Train$MHcode>1 & Census.Train$Target>0,])
summary(linmod8.lme)
drop1(linmod8.lme)

linmod9.lme=lmer(LogitTarg~(PlotDist+Area_Bot+Fire+Rain)^2-PlotDist:Rain-PlotDist:Fire+(1|Shrub), data=Census.Train[Census.Train$MHcode>1 & Census.Train$Target>0,])
summary(linmod9.lme)
drop1(linmod9.lme)
plot(linmod9.lme)
qqnorm(resid(linmod9.lme))
abline(c(0,1))
hist(resid(linmod9.lme), breaks=40, freq=F)
curve(dnorm(x, mean=mean(resid(linmod9.lme)), sd=sd(resid(linmod9.lme))), add=TRUE)
#Go with this one for now???

coef(linmod9.lme)
fixef(linmod9.lme)

inv_logit(fixef(linmod9.lme))


#Try on Testing Data
predict(linmod9.lme, Census.Test, allow.new.levels=TRUE)  
#Not stochastic, uses population level data for previously unobserved levels

Census.Test$PresOnlyPred=NA
Census.Test[Census.Test$MHcode>1 & Census.Test$Target>0,"PresOnlyPred"]=predict(linmod9.lme,  newdata=Census.Test[Census.Test$MHcode>1& Census.Test$Target>0,], allow.new.levels=TRUE )



plot(Target/100~inv_logit(PresOnlyPred), data=Census.Test[Census.Test$MHcode>1& Census.Test$Target>0,], xlab="Predicted Percent Cover", ylab="Observed percent Cover" ) 


