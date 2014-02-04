## Make functions out of apply nutrient models
source("SoilProbeSampling/CompassFunctions")
source("SoilProbeSampling/RasterGeometry")

###################################################################################
########################## Negative Exponential Model ############################
###################################################################################
ApplyNegExpModel=function(Model, ShrubData, InterspaceData, BlankImage.im) {
# Applies Negative Exponential Style Model to make a nutrient hotspot map image. 
# Model is a data frame with 1 row with columns   "Desert"     "Nutrient"   "Model"      "Use"        "AIC"       
# 	"PARA.mu.N"  "PARA.mu.S"  "PARA.slope" "PARA.sig"   "PARB.mu.N" 
# 	"PARB.mu.S"  "PARB.slope" "PARB.sig"   "PARD.mu.N"  "PARD.mu.S" 
# 	"PARD.slope" "PARD.sig"   "SIGMA"  
# ShrubData is a data frame containing information on shrubs in area to be mapped: 
#  	columns: "ShrubID","Easting","Northing","Area_Bot","Area_Stem"
# InterspaceData is a data frame with fitted lognormal models to interpace data (samples > 150cm from shrub stem)
# 	columns: Desert, Nutrient, BGDistrib, Bglogmean, Bglogsd
# BlankImage.im is an image pre-made with the geographic range that should be mapped, and pixel resolution desired
# Allometry is a data frame with 1 row showin conversion equation from area_bot to area_stem 
#	columns a, b, 

Ymin=BlankImage.im$yrange[1]
Ymax=BlankImage.im$yrange[2]
Xmin=BlankImage.im$xrange[1]
Xmax=BlankImage.im$xrange[2]
#xcoords=BlankImage.im$xcol
#ycoords=BlankImage.im$yrow

# Sample Shrub is the list of shrubs to model against. 
SampleShrub=subset(ShrubData,(Northing<=Ymax)&(Northing>=Ymin)&(Easting<=Xmax)&(Easting>=Xmin),  #Only want shrubs within boundaries
			select=c("ShrubID","Easting","Northing","Area_Bot","Area_Stem"))	#Only want certain info
rownames(SampleShrub)=1:nrow(SampleShrub)
nrow(SampleShrub)

A.rand=rnorm(nrow(SampleShrub),0,Model$PARA.sig)
B.rand=rnorm(nrow(SampleShrub),0,Model$PARB.sig)
D.rand=rnorm(nrow(SampleShrub),0,Model$PARD.sig)
SampleShrub$a.n=Model$PARA.mu.N+Model$PARA.slope*SampleShrub$Area_Bot/10000+A.rand
SampleShrub$a.s=Model$PARA.mu.S+Model$PARA.slope*SampleShrub$Area_Bot/10000+A.rand
SampleShrub$b.n=Model$PARB.mu.N+Model$PARB.slope*SampleShrub$Area_Bot/10000+B.rand
SampleShrub$b.s=Model$PARB.mu.S+Model$PARB.slope*SampleShrub$Area_Bot/10000+B.rand
SampleShrub$d.n=Model$PARD.mu.N+Model$PARD.slope*SampleShrub$Area_Bot/10000+D.rand
SampleShrub$d.s=Model$PARD.mu.S+Model$PARD.slope*SampleShrub$Area_Bot/10000+D.rand

SampleShrub$b.n[SampleShrub$b.n<0.005]=0.005 # THIS IS COMPLETELY ARBITRARY AND BASED ON VISUAL
SampleShrub$b.s[SampleShrub$b.s<0.005]=0.005 # THIS IS COMPLETELY ARBITRARY AND BASED ON VISUAL
SampleShrub$a.n[SampleShrub$a.n<0]=0 # Keep a non-negative
SampleShrub$a.s[SampleShrub$a.s<0]=0 # Keep a non-negative


#############################################################################
# initialize background (for points not under influence of any shrub) to be

PredNut=matrix(data=rlnorm(n=BlankImage.im$dim[2]*BlankImage.im$dim[1], 
		meanlog= InterspaceData$Bglogmean , 
		sdlog=InterspaceData$Bglogsd),
		ncol=BlankImage.im$dim[2], nrow=BlankImage.im$dim[1])

## Take out when going outside of R
PredNut.im=im(PredNut, BlankImage.im$xcol, BlankImage.im$yrow, unitname=c("meter","meters"))
summary(PredNut.im)

################# GET RANGE OF INFLUENCE FOR SHRUB
#Based on background noise.
#90th percentile of interspace area
BGcut=qlnorm(.90, meanlog= InterspaceData$Bglogmean , sdlog=InterspaceData$Bglogsd)
SampleShrub$Range.N=((log(BGcut-SampleShrub$d.n)-log(SampleShrub$a.n))/-SampleShrub$b.n)/100
SampleShrub$Range.S=((log(BGcut-SampleShrub$d.s)-log(SampleShrub$a.s))/-SampleShrub$b.s)/100
SampleShrub$Range=apply(rbind(SampleShrub$Range.N,SampleShrub$Range.S),2,max)
SampleShrub$Range[is.na(SampleShrub$Range)]=0
SampleShrub$Range[SampleShrub$Range==-Inf]=0
#For shrubs that get very small ranges, make them go at least as far out as the calculated radius. 
SampleShrub$Range[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]=sqrt((SampleShrub$Area_Bot/10000)/pi)[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]
###############	General Code	########################
pb <- txtProgressBar(min = 0, max = nrow(SampleShrub), style = 3)
for (k in 1:nrow(SampleShrub)){
	ShrubPixel=topixel(PredNut.im, SampleShrub$Easting[k], SampleShrub$Northing[k])
	# Use allometric relations to determine Area of Stem from Area of Bottom
	thisStemArea=NA
	thisStemArea=SampleShrub$Area_Stem[k]
	thisStemRad=sqrt((thisStemArea/10000)/pi)						#radius of stem for this shrub, converted to meters
	thisXrange=trunc((SampleShrub$Range[k]+thisStemRad)/PredNut.im$xstep) 	# Check if I used proper trunc/ceil/round
	thisYrange=trunc((SampleShrub$Range[k]+thisStemRad)/PredNut.im$ystep) 	# Check if I used proper trunc/ceil/round
	#Make sure pixels to check are within range
	xlist=(ShrubPixel[1]-thisXrange): (ShrubPixel[1]+thisXrange)
	ylist=(ShrubPixel[2]-thisYrange): (ShrubPixel[2]+thisYrange)
	xlist=xlist[(xlist>0)&(xlist<PredNut.im$dim[2]+1)]
	ylist=ylist[(ylist>0)&(ylist<PredNut.im$dim[1]+1)]
	for (i in xlist){
		for (j in ylist ){
			me=data.frame(Easting=BlankImage.im$xcol[i],Northing=BlankImage.im$yrow[j])
			thisdist=dist(rbind(me,SampleShrub[k,c("Easting","Northing")]))
			thisa.n=SampleShrub$a.n[k]
			thisa.s=SampleShrub$a.s[k]
			thisb.n=SampleShrub$b.n[k]
			thisb.s=SampleShrub$b.s[k]
			thisd.n=SampleShrub$d.n[k]
			thisd.s=SampleShrub$d.s[k]
			
			if (thisdist < thisStemRad){   #Point is within stem radius
				angle=LineAngle(SampleShrub[k,c("Easting")],SampleShrub[k,c("Northing")],
									 me$Easting, me$Northing, units="degrees")
				if(is.na(angle)) { 
					thisa=mean(thisa.n, thisa.s) 
					thisd=mean(thisd.n, thisd.s)
				} else{  	#shrub is on this cell 
				 	if(angle>180) angle=angle-2*(angle-180)   #Reflect across y=axis, scale from 0 to 180
					# Assign value of a proportional to angle between 0 and 180. 
					thisa=(angle/180)*thisa.s+(1-angle/180)*thisa.n
					thisd=(angle/180)*thisd.s+(1-angle/180)*thisd.n
				} #end angle check
				nutvalue=as.numeric(thisa+thisd+rnorm(1,0,Model$SIGMA))
				#If this shrub's influence is higher than current value, replace the value
				if(PredNut[j,i]<nutvalue) PredNut[j,i]=nutvalue
			}else
			if (thisdist < thisStemRad+SampleShrub$Range[k]){

				angle=LineAngle(SampleShrub[k,c("Easting")],SampleShrub[k,c("Northing")],
									 me$Easting, me$Northing, units="degrees")
				if(is.na(angle)) { 
					thisa=mean(thisa.n, thisa.s) 
					thisb=mean(thisb.n, thisb.s)
					thisd=mean(thisd.n, thisd.s)
				} else{  	#shrub is on this cell 
				 	if(angle>180) angle=angle-2*(angle-180)   #Reflect across y=axis, scale from 0 to 180
					# Assign value of a proportional to angle between 0 and 180. 
					thisa=(angle/180)*thisa.s+(1-angle/180)*thisa.n
					thisb=(angle/180)*thisb.s+(1-angle/180)*thisb.n
					thisd=(angle/180)*thisd.s+(1-angle/180)*thisd.n
				} #end angle check
				nutvalue=as.numeric(thisa*exp(-thisb*(thisdist-thisStemRad)*100)+thisd+rnorm(1,0,Model$SIGMA))
				#If this shrub's influence is higher than current value, replace the value
				if(PredNut[j,i]<nutvalue) PredNut[j,i]=nutvalue
			} #end thisdist check
		}# end j loop
	}# end i loop
	setTxtProgressBar(pb, k)
} #end k loop

close(pb)

PredNut.im=im(PredNut, xcol=BlankImage.im$xcol,yrow=BlankImage.im$yrow)

return(list(image=PredNut.im, table=SampleShrub))

} # End ApplyNegExpModel




###################################################################################
########################## Negative Exponential Model    ##########################
########################## with quadratic parameter eqns ##########################
###################################################################################
ApplyNegExpQPModel=function(Model, ShrubData, InterspaceData, BlankImage.im) {
# Applies Negative Exponential Style Model to make a nutrient hotspot map image. 
# Model is a data frame with 1 row with columns   "Desert"     "Nutrient"   "Model"      "Use"        "AIC"       
# 	PARA.mu.N	PARA.mu.S	PARA.slope	PARA.slope.S PARA.slope2 PARA.slope2.S PARA.sig	
#	PARB.mu.N	PARB.mu.S	PARB.slope	PARB.slope.S PARB.slope2 PARB.slope2.S PARB.sig	
#	PARD.mu.N	PARD.mu.S	PARD.slope	PARD.slope.S PARD.slope2 PARD.slope2.S PARD.sig	SIGMA
#  
# ShrubData is a data frame containing information on shrubs in area to be mapped: 
#  	columns: "ShrubID","Easting","Northing","Area_Bot","Area_Stem"
# InterspaceData is a data frame with fitted lognormal models to interpace data (samples > 150cm from shrub stem)
# 	columns: Desert, Nutrient, BGDistrib, Bglogmean, Bglogsd
# BlankImage.im is an image pre-made with the geographic range that should be mapped, and pixel resolution desired
# Allometry is a data frame with 1 row showin conversion equation from area_bot to area_stem 
#	columns a, b, 

Ymin=BlankImage.im$yrange[1]
Ymax=BlankImage.im$yrange[2]
Xmin=BlankImage.im$xrange[1]
Xmax=BlankImage.im$xrange[2]
#xcoords=BlankImage.im$xcol
#ycoords=BlankImage.im$yrow

# Sample Shrub is the list of shrubs to model against. 
SampleShrub=subset(ShrubData,(Northing<=Ymax)&(Northing>=Ymin)&(Easting<=Xmax)&(Easting>=Xmin),  #Only want shrubs within boundaries
			select=c("ShrubID","Easting","Northing","Area_Bot","Area_Stem"))	#Only want certain info
rownames(SampleShrub)=1:nrow(SampleShrub)
nrow(SampleShrub)

A.rand=rnorm(nrow(SampleShrub),0,Model$PARA.sig)
B.rand=rnorm(nrow(SampleShrub),0,Model$PARB.sig)
D.rand=rnorm(nrow(SampleShrub),0,Model$PARD.sig)
SampleShrub$a.n=Model$PARA.mu.N+Model$PARA.slope  *SampleShrub$Area_Bot/10000 + Model$PARA.slope2  *(SampleShrub$Area_Bot/10000)^2+A.rand
SampleShrub$a.s=Model$PARA.mu.S+Model$PARA.slope.S*SampleShrub$Area_Bot/10000 + Model$PARA.slope2.S*(SampleShrub$Area_Bot/10000)^2+A.rand
SampleShrub$b.n=Model$PARB.mu.N+Model$PARB.slope  *SampleShrub$Area_Bot/10000 + Model$PARB.slope2  *(SampleShrub$Area_Bot/10000)^2+B.rand
SampleShrub$b.s=Model$PARB.mu.S+Model$PARB.slope.S*SampleShrub$Area_Bot/10000 + Model$PARB.slope2.S*(SampleShrub$Area_Bot/10000)^2+B.rand
SampleShrub$d.n=Model$PARD.mu.N+Model$PARD.slope  *SampleShrub$Area_Bot/10000 + Model$PARD.slope2  *(SampleShrub$Area_Bot/10000)^2+D.rand
SampleShrub$d.s=Model$PARD.mu.S+Model$PARD.slope.S*SampleShrub$Area_Bot/10000 + Model$PARD.slope2.S*(SampleShrub$Area_Bot/10000)^2+D.rand

SampleShrub$b.n[SampleShrub$b.n<0.005]=0.005 # THIS IS COMPLETELY ARBITRARY AND BASED ON VISUAL
SampleShrub$b.s[SampleShrub$b.s<0.005]=0.005 # THIS IS COMPLETELY ARBITRARY AND BASED ON VISUAL
SampleShrub$a.n[SampleShrub$a.n<0]=0 # Keep a non-negative
SampleShrub$a.s[SampleShrub$a.s<0]=0 # Keep a non-negative


#############################################################################
# initialize background (for points not under influence of any shrub) to be

PredNut=matrix(data=rlnorm(n=BlankImage.im$dim[2]*BlankImage.im$dim[1], 
		meanlog= InterspaceData$Bglogmean , 
		sdlog=InterspaceData$Bglogsd),
		ncol=BlankImage.im$dim[2], nrow=BlankImage.im$dim[1])

## Take out when going outside of R
PredNut.im=im(PredNut, BlankImage.im$xcol, BlankImage.im$yrow, unitname=c("meter","meters"))
summary(PredNut.im)

################# GET RANGE OF INFLUENCE FOR SHRUB
#Based on background noise.
#90th percentile of interspace area
BGcut=qlnorm(.90, meanlog= InterspaceData$Bglogmean , sdlog=InterspaceData$Bglogsd)
SampleShrub$Range.N=((log(BGcut-SampleShrub$d.n)-log(SampleShrub$a.n))/-SampleShrub$b.n)/100
SampleShrub$Range.S=((log(BGcut-SampleShrub$d.s)-log(SampleShrub$a.s))/-SampleShrub$b.s)/100
SampleShrub$Range=apply(rbind(SampleShrub$Range.N,SampleShrub$Range.S),2,max)
SampleShrub$Range[is.na(SampleShrub$Range)]=0
SampleShrub$Range[SampleShrub$Range==-Inf]=0
#For shrubs that get very small ranges, make them go at least as far out as the calculated radius. 
SampleShrub$Range[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]=sqrt((SampleShrub$Area_Bot/10000)/pi)[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]
###############	General Code	########################
pb <- txtProgressBar(min = 0, max = nrow(SampleShrub), style = 3)
for (k in 1:nrow(SampleShrub)){
	ShrubPixel=topixel(PredNut.im, SampleShrub$Easting[k], SampleShrub$Northing[k])
	# Use allometric relations to determine Area of Stem from Area of Bottom
	thisStemArea=NA
	thisStemArea=SampleShrub$Area_Stem[k]
	thisStemRad=sqrt((thisStemArea/10000)/pi)						#radius of stem for this shrub, converted to meters
	thisXrange=trunc((SampleShrub$Range[k]+thisStemRad)/PredNut.im$xstep) 	# Check if I used proper trunc/ceil/round
	thisYrange=trunc((SampleShrub$Range[k]+thisStemRad)/PredNut.im$ystep) 	# Check if I used proper trunc/ceil/round
	#Make sure pixels to check are within range
	xlist=(ShrubPixel[1]-thisXrange): (ShrubPixel[1]+thisXrange)
	ylist=(ShrubPixel[2]-thisYrange): (ShrubPixel[2]+thisYrange)
	xlist=xlist[(xlist>0)&(xlist<PredNut.im$dim[2]+1)]
	ylist=ylist[(ylist>0)&(ylist<PredNut.im$dim[1]+1)]
	for (i in xlist){
		for (j in ylist ){
			me=data.frame(Easting=BlankImage.im$xcol[i],Northing=BlankImage.im$yrow[j])
			thisdist=dist(rbind(me,SampleShrub[k,c("Easting","Northing")]))
			thisa.n=SampleShrub$a.n[k]
			thisa.s=SampleShrub$a.s[k]
			thisb.n=SampleShrub$b.n[k]
			thisb.s=SampleShrub$b.s[k]
			thisd.n=SampleShrub$d.n[k]
			thisd.s=SampleShrub$d.s[k]
			
			if (thisdist < thisStemRad){   #Point is within stem radius
				angle=LineAngle(SampleShrub[k,c("Easting")],SampleShrub[k,c("Northing")],
									 me$Easting, me$Northing, units="degrees")
				if(is.na(angle)) { 
					thisa=mean(thisa.n, thisa.s) 
					thisd=mean(thisd.n, thisd.s)
				} else{  	#shrub is on this cell 
				 	if(angle>180) angle=angle-2*(angle-180)   #Reflect across y=axis, scale from 0 to 180
					# Assign value of a proportional to angle between 0 and 180. 
					thisa=(angle/180)*thisa.s+(1-angle/180)*thisa.n
					thisd=(angle/180)*thisd.s+(1-angle/180)*thisd.n
				} #end angle check
				nutvalue=as.numeric(thisa+thisd+rnorm(1,0,Model$SIGMA))
				#If this shrub's influence is higher than current value, replace the value
				if(PredNut[j,i]<nutvalue) PredNut[j,i]=nutvalue
			}else
			if (thisdist < thisStemRad+SampleShrub$Range[k]){

				angle=LineAngle(SampleShrub[k,c("Easting")],SampleShrub[k,c("Northing")],
									 me$Easting, me$Northing, units="degrees")
				if(is.na(angle)) { 
					thisa=mean(thisa.n, thisa.s) 
					thisb=mean(thisb.n, thisb.s)
					thisd=mean(thisd.n, thisd.s)
				} else{  	#shrub is on this cell 
				 	if(angle>180) angle=angle-2*(angle-180)   #Reflect across y=axis, scale from 0 to 180
					# Assign value of a proportional to angle between 0 and 180. 
					thisa=(angle/180)*thisa.s+(1-angle/180)*thisa.n
					thisb=(angle/180)*thisb.s+(1-angle/180)*thisb.n
					thisd=(angle/180)*thisd.s+(1-angle/180)*thisd.n
				} #end angle check
				nutvalue=as.numeric(thisa*exp(-thisb*(thisdist-thisStemRad)*100)+thisd+rnorm(1,0,Model$SIGMA))
				#If this shrub's influence is higher than current value, replace the value
				if(PredNut[j,i]<nutvalue) PredNut[j,i]=nutvalue
			} #end thisdist check
		}# end j loop
	}# end i loop
	setTxtProgressBar(pb, k)
} #end k loop

close(pb)

PredNut.im=im(PredNut, xcol=BlankImage.im$xcol,yrow=BlankImage.im$yrow)

return(list(image=PredNut.im, table=SampleShrub))

} # End ApplyNegExpModel




###################################################################################
########################## LINEAR MODEL ##########################################
###################################################################################
ApplyLinearModel=function(Model, ShrubData, InterspaceData, BlankImage.im, Allometry  ) {
# Applies Negative Exponential Style Model to make a nutrient hotspot map image. 
# Model is a data frame with 1 row with columns    "Desert"     "Nutrient"   "Model"  
# "AIC"        "PARM.mu.N"  "PARM.mu.S"  "PARM.slope" "PARM.sig"   "PARC.mu.N"  "PARC.mu.S" 
# "PARC.slope" "PARC.sig"   "SIGMA"    
# ShrubData is a data frame containing information on shrubs in area to be mapped: 
#  	columns: "ShrubID","Easting","Northing","Area_Bot","Area_Stem"
# ObsNutData is a data frame with the observed nutrient data 
# 	columns: Nutrient, Distance
# BlankImage.im is an image pre-made with the geographic range that should be mapped, and pixel resolution desired
# Allometry is a data frame with 1 row showin conversion equation from area_bot to area_stem 
#	columns a, b,

Ymin=BlankImage.im$yrange[1]
Ymax=BlankImage.im$yrange[2]
Xmin=BlankImage.im$xrange[1]
Xmax=BlankImage.im$xrange[2]


# Sample Shrub is the list of shrubs to model against. 
SampleShrub=subset(ShrubData,(Northing<=Ymax)&(Northing>=Ymin)&(Easting<=Xmax)&(Easting>=Xmin),  #Only want shrubs within boundaries
			select=c("ShrubID","Easting","Northing","Area_Bot", "Area_Stem"))	#Only want certain info
rownames(SampleShrub)=1:nrow(SampleShrub)

M.rand=rnorm(nrow(SampleShrub),0,Model$PARM.sig)
C.rand=rnorm(nrow(SampleShrub),0,Model$PARC.sig)
SampleShrub$m.n=Model$PARM.mu.N+Model$PARM.slope*SampleShrub$Area_Bot/10000+M.rand
SampleShrub$m.s=Model$PARM.mu.S+Model$PARM.slope*SampleShrub$Area_Bot/10000+M.rand
SampleShrub$c.n=Model$PARC.mu.N+Model$PARC.slope*SampleShrub$Area_Bot/10000+C.rand
SampleShrub$c.s=Model$PARC.mu.S+Model$PARC.slope*SampleShrub$Area_Bot/10000+C.rand

#For Sonoran Mg, make max slope 0.8
SampleShrub$m.n[SampleShrub$m.n>0.8]=0.8
SampleShrub$m.s[SampleShrub$m.s>0.8]=0.8

#############################################################################
# initialize background (for points not under influence of any shrub) to be
# Sampled from the the same distribution observed values of the "outer" samples 
# here, outer will be >150

PredNut=matrix(data=rlnorm(n=BlankImage.im$dim[2]*BlankImage.im$dim[1], 
		meanlog= InterspaceData$Bglogmean , 
		sdlog=InterspaceData$Bglogsd),
		ncol=BlankImage.im$dim[2], nrow=BlankImage.im$dim[1])

## Take out when going outside of R
PredNut.im=im(PredNut, BlankImage.im$xcol, BlankImage.im$yrow, unitname=c("meter","meters"))

################# GET RANGE OF INFLUENCE FOR SHRUB
#Based on background noise.  #For neg exponential, go out to 90% of background
BGcut=qlnorm(.90, meanlog= InterspaceData$Bglogmean , sdlog=InterspaceData$Bglogsd)
SampleShrub$Range.N=((BGcut-SampleShrub$c.n)/SampleShrub$m.n)/100
SampleShrub$Range.S=((BGcut-SampleShrub$c.s)/SampleShrub$m.s)/100
SampleShrub$Range=apply(rbind(SampleShrub$Range.N,SampleShrub$Range.S),2,max)
#For shrubs that get very small ranges, make them go at least as far out as the calculated radius. 
SampleShrub$Range[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]=sqrt((SampleShrub$Area_Bot/10000)/pi)[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]
#For Sonoran Mg: Make maximum 5 m
SampleShrub$Range[SampleShrub$Range>5]=5
###############	General Code	########################

pb <- txtProgressBar(min = 0, max = nrow(SampleShrub), style = 3)

for (k in 1:nrow(SampleShrub)){
	ShrubPixel=topixel(PredNut.im, SampleShrub$Easting[k], SampleShrub$Northing[k])
	# Use allometric relations to determine Area of Stem from Area of Bottom
	thisStemArea=NA
	thisStemArea=SampleShrub$Area_Stem[k]
	thisStemRad=sqrt((thisStemArea/10000)/pi)	
	thisXrange=trunc((SampleShrub$Range[k]+thisStemRad)/PredNut.im$xstep) 	# Check if I used proper trunc/ceil/round
	thisYrange=trunc((SampleShrub$Range[k]+thisStemRad)/PredNut.im$ystep) 	# Check if I used proper trunc/ceil/round
	#Make sure pixels to check are within range
	xlist=(ShrubPixel[1]-thisXrange): (ShrubPixel[1]+thisXrange)
	ylist=(ShrubPixel[2]-thisYrange): (ShrubPixel[2]+thisYrange)
	xlist=xlist[(xlist>0)&(xlist<PredNut.im$dim[2]+1)]
	ylist=ylist[(ylist>0)&(ylist<PredNut.im$dim[1]+1)]
	for (i in xlist){
		for (j in ylist ){
			me=data.frame(Easting=BlankImage.im$xcol[i],Northing=BlankImage.im$yrow[j])
			thisdist=dist(rbind(me,SampleShrub[k,c("Easting","Northing")]))
			thism.n=SampleShrub$m.n[k]
			thism.s=SampleShrub$m.s[k]
			thisc.n=SampleShrub$c.n[k]
			thisc.s=SampleShrub$c.s[k]
			if (thisdist< thisStemRad) { # Point is within stem radius 
				angle=LineAngle(SampleShrub[k,c("Easting")],SampleShrub[k,c("Northing")],
									 me$Easting, me$Northing, units="degrees")
				if(is.na(angle)) { #shrub is on this cell 
					thism=mean(thism.n, thism.s) 
					thisc=mean(thisc.n, thisc.s)
				} else{  	
				 	if(angle>180) angle=angle-2*(angle-180)   #Reflect across y=axis, scale from 0 to 180
					# Assign value of a proportional to angle between 0 and 180. 
					thism=(angle/180)*thism.s+(1-angle/180)*thism.n
					thisc=(angle/180)*thisc.s+(1-angle/180)*thisc.n
				} #end angle check
				nutvalue=-thism*0*100+thisc+rnorm(1,0,Model$SIGMA)
				#If this shrub's influence is higher than current value, replace the value
				if(PredNut[j,i]<nutvalue) PredNut[j,i]=nutvalue
				
			}else
			if (thisdist < SampleShrub$Range[k]){

				angle=LineAngle(SampleShrub[k,c("Easting")],SampleShrub[k,c("Northing")],
									 me$Easting, me$Northing, units="degrees")
				if(is.na(angle)) { #shrub is on this cell 
					thism=mean(thism.n, thism.s) 
					thisc=mean(thisc.n, thisc.s)
				} else{  	
				 	if(angle>180) angle=angle-2*(angle-180)   #Reflect across y=axis, scale from 0 to 180
					# Assign value of a proportional to angle between 0 and 180. 
					thism=(angle/180)*thism.s+(1-angle/180)*thism.n
					thisc=(angle/180)*thisc.s+(1-angle/180)*thisc.n
				} #end angle check
				nutvalue=-thism*(thisdist-thisStemRad)*100+thisc+rnorm(1,0,Model$SIGMA)
				#If this shrub's influence is higher than current value, replace the value
				if(PredNut[j,i]<nutvalue) PredNut[j,i]=nutvalue
			
			} #end thisdist check
		}# end j loop
	}# end i loop
	setTxtProgressBar(pb, k)
} #end k loop

close(pb)

PredNut.im=im(PredNut, xcol=BlankImage.im$xcol,yrow=BlankImage.im$yrow)


return(list(image=PredNut.im, table=SampleShrub))

} # End ApplyLinearModel
##################################################################################





###################################################################################
########################## REGIONAL MODEL ##########################################
###################################################################################
ApplyRegionalModel=function(Model, BlankImage.im  ) {
# Applies Regional Style Model to make a nutrient map image. 
# Model is a data frame with 1 row with columns  
# "Desert"    "Nutrient"  "Model"     "AIC"       "Intercept" "Y.calc"   
# "X.calc"    "sigeps"  
# BlankImage.im is an image pre-made with the geographic range that should be mapped, and pixel resolution desired
# Allometry is a data frame with 1 row showin conversion equation from area_bot to area_stem 
#	columns a, b,
#xcoords=BlankImage.im$xcol
#ycoords=BlankImage.im$yrow

PredNut=matrix(data=NA , ncol=BlankImage.im$dim[2], nrow=BlankImage.im$dim[1])
## Take out when going outside of R
PredNut.im=im(PredNut, BlankImage.im$xcol, BlankImage.im$yrow, unitname=c("meter","meters"))


total=length(BlankImage.im$xcol)*length(BlankImage.im$yrow)
pb <- txtProgressBar(min = 0, max = total, style = 3)

count=0
	for (i in BlankImage.im$xcol){		#all but the last in the series
		for (j in BlankImage.im$yrow){

			pixcoords=topixel(PredNut.im, i, j)
			PredNut[pixcoords[2],pixcoords[1]]=Model$Intercept+Model$Y.calc*j+Model$X.calc*i+rnorm(1, 0, Model$sigeps)
			if (PredNut[pixcoords[2],pixcoords[1]]<=0)  PredNut[pixcoords[2],pixcoords[1]]=0
		count=count+1
		#print(paste("did ", count , "out of ", total))
		setTxtProgressBar(pb, count)
		}# end j loop
	}# end i loop
close(pb)

PredNut.im=im(PredNut, xcol=BlankImage.im$xcol,yrow=BlankImage.im$yrow)
return(PredNut.im)

} #End Apply Regional Model







###################################################################################
##################### REGIONAL POLYNOMIAL MODEL ###################################
###################################################################################
ApplyPolynomialRegionalModel=function(Model, BlankImage.im  ) {
# Applies Negative Exponential Style Model to make a nutrient hotspot map image. 
# Model is a data frame with 1 row with columns  
# "Desert"    "Nutrient"  "Model"     "AIC"       "Intercept" 
# "Y2Local"	"YLocal"	"XY2Local"	"XYLocal"	"X2YLocal"	"XLocal"	"X2Local" "sigeps"  

# BlankImage.im is an image pre-made with the geographic range that should be mapped, and pixel resolution desired
# Allometry is a data frame with 1 row showin conversion equation from area_bot to area_stem 
#	columns a, b,
xcoords=BlankImage.im$xcol
ycoords=BlankImage.im$yrow
# UTM coordinates are too large to make the polynomial coordinates work,
# So subtract coordinates from lower left corner. 

localx=xcoords
localy=ycoords




PredNut=matrix(data=NA , ncol=length(localx), nrow=length(localy))
## Take out when going outside of R
PredNut.im=im(PredNut, localx, localy, unitname=c("meter","meters"))


total=length(localx)*length(localy)
pb <- txtProgressBar(min = 0, max = total, style = 3)

count=0
	for (i in localx){		#all but the last in the series
		for (j in localy){

			#pixcoords=topixel(PredNut.im, BlankImage.im$xcol[i], BlankImage.im$yrow[j])
			pixcoords=topixel(PredNut.im, i, j)
			PredNut[pixcoords[2],pixcoords[1]]=Model$Intercept+Model$Y2Local*(j^2)+Model$YLocal*j+
								Model$XYLocal*(i*j)+
								Model$XLocal*i+ 
								Model$X2Local*(i^2)+rnorm(1, 0, Model$sigeps)
			if (PredNut[pixcoords[2],pixcoords[1]]<=0)  PredNut[pixcoords[2],pixcoords[1]]=0
			count=count+1
			#print(paste("did ", count , "out of ", total))
			setTxtProgressBar(pb, count)
		}# end j loop
	}# end i loop
close(pb)

#Transpose PredNut back to original coordinates
PredNut.im=im(PredNut, xcol=BlankImage.im$xcol,yrow=BlankImage.im$yrow)
return(list(image=PredNut.im))


} #End Apply Regional Model


###################################################################################
########################## Negative Exponential Model ############################
###################################################################################
ApplyQuadVertexModel=function(Model, ShrubData, ObsNutData, BlankImage.im, Allometry  ) {
    # Applies Quadratic model written in vertex form to make a plant density hotspot map image. 
    # Model is a data frame with 1 row with columns   "Desert"     "Nutrient"   "Model"      a.(Intercept)    a.Area_Bot    a.TranDirS	a.Area_Bot:TranDirS	h.(Intercept)	h.Area_Bot	h.TranDirS	k.(Intercept)	k.Area_Bot	k.FireUB	k.RainD	k.Area_Bot:FireUB	a.(Intercept)	h.(Intercept)	k.(Intercept)	Residual"
    
    # ShrubData is a data frame containing information on shrubs in area to be mapped: 
    #  	columns: "ShrubID","Easting","Northing","Area_Bot","Area_Stem"
    # InterspaceData is a data frame with fitted lognormal models to interpace data (samples > 150cm from shrub stem)
    # 	columns: Desert, Nutrient, BGDistrib, Bglogmean, Bglogsd
    # BlankImage.im is an image pre-made with the geographic range that should be mapped, and pixel resolution desired
    # Allometry is a data frame with 1 row showin conversion equation from area_bot to area_stem 
    #	columns a, b, 
    
    Ymin=BlankImage.im$yrange[1]
    Ymax=BlankImage.im$yrange[2]
    Xmin=BlankImage.im$xrange[1]
    Xmax=BlankImage.im$xrange[2]
    #xcoords=BlankImage.im$xcol
    #ycoords=BlankImage.im$yrow
    
    # Sample Shrub is the list of shrubs to model against. 
    SampleShrub=subset(ShrubData,(Northing<=Ymax)&(Northing>=Ymin)&(Easting<=Xmax)&(Easting>=Xmin),  #Only want shrubs within boundaries
    			select=c("ShrubID","Easting","Northing","Area_Bot","Area_Stem"))	#Only want certain info
    rownames(SampleShrub)=1:nrow(SampleShrub)
    nrow(SampleShrub)
    
    A.rand=rnorm(nrow(SampleShrub),0,Model$a..Intercept..1)
    H.rand=rnorm(nrow(SampleShrub),0,Model$h..Intercept..1)
    K.rand=rnorm(nrow(SampleShrub),0,Model$k..Intercept..1)
    SampleShrub$a.n=Model$a..Intercept.+Model$a.Area_Bot*SampleShrub$Area_Bot+A.rand
    SampleShrub$a.s=Model$PARA.mu.S+Model$PARA.slope*SampleShrub$Area_Bot+A.rand
    SampleShrub$h.n=Model$PARB.mu.N+Model$PARB.slope*SampleShrub$Area_Bot+B.rand
    SampleShrub$h.s=Model$PARB.mu.S+Model$PARB.slope*SampleShrub$Area_Bot+B.rand
    SampleShrub$k.n=Model$PARD.mu.N+Model$PARD.slope*SampleShrub$Area_Bot+D.rand
    SampleShrub$k.s=Model$PARD.mu.S+Model$PARD.slope*SampleShrub$Area_Bot+D.rand
    
    SampleShrub$b.n[SampleShrub$b.n<0.005]=0.005 # THIS IS COMPLETELY ARBITRARY AND BASED ON VISUAL
    SampleShrub$b.s[SampleShrub$b.s<0.005]=0.005 # THIS IS COMPLETELY ARBITRARY AND BASED ON VISUAL
    SampleShrub$a.n[SampleShrub$a.n<0]=0 # Keep a non-negative
    SampleShrub$a.s[SampleShrub$a.s<0]=0 # Keep a non-negative
    
    
    #############################################################################
    # initialize background (for points not under influence of any shrub) to be
    
    PredNut=matrix(data=rlnorm(n=BlankImage.im$dim[2]*BlankImage.im$dim[1], 
    		meanlog= InterspaceData$Bglogmean , 
    		sdlog=InterspaceData$Bglogsd),
    		ncol=BlankImage.im$dim[2], nrow=BlankImage.im$dim[1])
    
    ## Take out when going outside of R
    PredNut.im=im(PredNut, BlankImage.im$xcol, BlankImage.im$yrow, unitname=c("meter","meters"))
    summary(PredNut.im)
    
    ################# GET RANGE OF INFLUENCE FOR SHRUB
    #Based on background noise.
    #90th percentile of interspace area
    BGcut=qlnorm(.90, meanlog= InterspaceData$Bglogmean , sdlog=InterspaceData$Bglogsd)
    SampleShrub$Range.N=((log(BGcut-SampleShrub$d.n)-log(SampleShrub$a.n))/-SampleShrub$b.n)/100
    SampleShrub$Range.S=((log(BGcut-SampleShrub$d.s)-log(SampleShrub$a.s))/-SampleShrub$b.s)/100
    SampleShrub$Range=apply(rbind(SampleShrub$Range.N,SampleShrub$Range.S),2,max)
    SampleShrub$Range[is.na(SampleShrub$Range)]=0
    SampleShrub$Range[SampleShrub$Range==-Inf]=0
    #For shrubs that get very small ranges, make them go at least as far out as the calculated radius. 
    SampleShrub$Range[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]=sqrt((SampleShrub$Area_Bot/10000)/pi)[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]
    ###############	General Code	########################
    pb <- txtProgressBar(min = 0, max = nrow(SampleShrub), style = 3)
    for (k in 1:nrow(SampleShrub)){
    	ShrubPixel=topixel(PredNut.im, SampleShrub$Easting[k], SampleShrub$Northing[k])
    	# Use allometric relations to determine Area of Stem from Area of Bottom
    	thisStemArea=NA
    	thisStemArea=SampleShrub$Area_Stem[k]
    	thisStemRad=sqrt((thisStemArea/10000)/pi)						#radius of stem for this shrub, converted to meters
    	thisXrange=trunc((SampleShrub$Range[k]+thisStemRad)/PredNut.im$xstep) 	# Check if I used proper trunc/ceil/round
    	thisYrange=trunc((SampleShrub$Range[k]+thisStemRad)/PredNut.im$ystep) 	# Check if I used proper trunc/ceil/round
    	#Make sure pixels to check are within range
    	xlist=(ShrubPixel[1]-thisXrange): (ShrubPixel[1]+thisXrange)
    	ylist=(ShrubPixel[2]-thisYrange): (ShrubPixel[2]+thisYrange)
    	xlist=xlist[(xlist>0)&(xlist<PredNut.im$dim[2]+1)]
    	ylist=ylist[(ylist>0)&(ylist<PredNut.im$dim[1]+1)]
    	for (i in xlist){
    		for (j in ylist ){
    			me=data.frame(Easting=BlankImage.im$xcol[i],Northing=BlankImage.im$yrow[j])
    			thisdist=dist(rbind(me,SampleShrub[k,c("Easting","Northing")]))
    			thisa.n=SampleShrub$a.n[k]
    			thisa.s=SampleShrub$a.s[k]
    			thisb.n=SampleShrub$b.n[k]
    			thisb.s=SampleShrub$b.s[k]
    			thisd.n=SampleShrub$d.n[k]
    			thisd.s=SampleShrub$d.s[k]
    			
    			if (thisdist < thisStemRad){   #Point is within stem radius
    				angle=LineAngle(SampleShrub[k,c("Easting")],SampleShrub[k,c("Northing")],
    									 me$Easting, me$Northing, units="degrees")
    				if(is.na(angle)) { 
    					thisa=mean(thisa.n, thisa.s) 
    					thisd=mean(thisd.n, thisd.s)
    				} else{  	#shrub is on this cell 
    				 	if(angle>180) angle=angle-2*(angle-180)   #Reflect across y=axis, scale from 0 to 180
    					# Assign value of a proportional to angle between 0 and 180. 
    					thisa=(angle/180)*thisa.s+(1-angle/180)*thisa.n
    					thisd=(angle/180)*thisd.s+(1-angle/180)*thisd.n
    				} #end angle check
    				nutvalue=as.numeric(thisa+thisd+rnorm(1,0,Model$SIGMA))
    				#If this shrub's influence is higher than current value, replace the value
    				if(PredNut[j,i]<nutvalue) PredNut[j,i]=nutvalue
    			}else
    			if (thisdist < thisStemRad+SampleShrub$Range[k]){
    
    				angle=LineAngle(SampleShrub[k,c("Easting")],SampleShrub[k,c("Northing")],
    									 me$Easting, me$Northing, units="degrees")
    				if(is.na(angle)) { 
    					thisa=mean(thisa.n, thisa.s) 
    					thisb=mean(thisb.n, thisb.s)
    					thisd=mean(thisd.n, thisd.s)
    				} else{  	#shrub is on this cell 
    				 	if(angle>180) angle=angle-2*(angle-180)   #Reflect across y=axis, scale from 0 to 180
    					# Assign value of a proportional to angle between 0 and 180. 
    					thisa=(angle/180)*thisa.s+(1-angle/180)*thisa.n
    					thisb=(angle/180)*thisb.s+(1-angle/180)*thisb.n
    					thisd=(angle/180)*thisd.s+(1-angle/180)*thisd.n
    				} #end angle check
    				nutvalue=as.numeric(thisa*exp(-thisb*(thisdist-thisStemRad)*100)+thisd+rnorm(1,0,Model$SIGMA))
    				#If this shrub's influence is higher than current value, replace the value
    				if(PredNut[j,i]<nutvalue) PredNut[j,i]=nutvalue
    			} #end thisdist check
    		}# end j loop
    	}# end i loop
    	setTxtProgressBar(pb, k)
    } #end k loop
    
    close(pb)
    
    PredNut.im=im(PredNut, xcol=BlankImage.im$xcol,yrow=BlankImage.im$yrow)
    
    return(list(image=PredNut.im, table=SampleShrub))

} # End ApplyNegExpModel


