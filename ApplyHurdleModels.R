

##################################################################################
########################## Hurdle MODEL ##########################################
##################################################################################
ApplyHurdleModel=function(PresModel, CovModel, ShrubData, InterspaceData, BlankImage.im  ) {
  # Applies Negative Exponential Style Model to make a nutrient hotspot map image. 
  # PresModel is a data frame with 1 row with columns    "Desert"     "Nutrient"   "Model"  
  # "AIC"        "PARM.mu.N"  "PARM.mu.S"  "PARM.slope" "PARM.sig"   "PARC.mu.N"  "PARC.mu.S" 
  # "PARC.slope" "PARC.sig"   "SIGMA"    
  # ShrubData is a data frame containing information on shrubs in area to be mapped: 
  #    columns: Easting","Northing","Area_Bot"
  # ObsNutData is a data frame with the observed nutrient data 
  # 	columns: Nutrient, Distance
  # BlankImage.im is an image pre-made with the geographic range that should be mapped, and pixel resolution desired
  
  Ymin=BlankImage.im$yrange[1]
  Ymax=BlankImage.im$yrange[2]
  Xmin=BlankImage.im$xrange[1]
  Xmax=BlankImage.im$xrange[2]
  
  
  # Sample Shrub is the list of shrubs to model against. 
  SampleShrub=subset(ShrubData,(Northing<=Ymax)&(Northing>=Ymin)&(Easting<=Xmax)&(Easting>=Xmin),  #Only want shrubs within boundaries
      select=c("Easting","Northing","Area_Bot","Rain","Fire"))	#Only want certain info
  rownames(SampleShrub)=1:nrow(SampleShrub)
  SampleShrub$Range=2.50  

  #############################################################################
  # initialize background (for points not under influence of any shrub) with something- for now make it 20%

PredCov=matrix(data=InterspaceData,
               ncol=BlankImage.im$dim[2], nrow=BlankImage.im$dim[1])
#   
#   ################# GET RANGE OF INFLUENCE FOR SHRUB
#   how? 


  ###############	General Code	########################
  
pb <- txtProgressBar(min = 0, max = nrow(SampleShrub), style = 3)

for (k in 1:nrow(SampleShrub)){
  thisShrub=SampleShrub[k,]
  ShrubPixel=topixel(BlankImage.im, thisShrub$Easting, thisShrub$Northing)
  thisShrubRad=sqrt(thisShrub$Area_Bot/pi)  # Use Area_Bot to get approx. radius	
  thisXrange=trunc((thisShrubRad + thisShrub$Range)/BlankImage.im$xstep) 	
  thisYrange=trunc((thisShrubRad + thisShrub$Range)/BlankImage.im$ystep) 	
  #Make sure pixels to check are within range
  xlist=(ShrubPixel[1]-thisXrange): (ShrubPixel[1]+thisXrange)
  ylist=(ShrubPixel[2]-thisYrange): (ShrubPixel[2]+thisYrange)
  xlist=xlist[(xlist>0)&(xlist<BlankImage.im$dim[2]+1)]
  ylist=ylist[(ylist>0)&(ylist<BlankImage.im$dim[1]+1)]
  for (i in xlist){
    for (j in ylist ){
      Cov=RangeCov=NA
      me=data.frame(Easting=BlankImage.im$xcol[i],Northing=BlankImage.im$yrow[j])
      thisdist=dist(rbind(me,thisShrub[c("Easting","Northing")]))
      if (thisdist< thisShrubRad) { # Point within shrub radius so is under shrub
        Cov=1  
      }else
        if (thisdist < thisShrubRad+ thisShrub$Range){  #within range 
          angle=LineAngle(thisShrub$"Easting",thisShrub$"Northing",
                          me$Easting, me$Northing, units="degrees")
          thisdist_cm=thisdist*100   #Note distances in cm for models.
          ProbN=inv_logit(PresModel$Int + PresModel$PlotDist*thisdist_cm +PresModel$Rain*(thisShrub$Rain=="D"))
          ProbS=inv_logit(PresModel$Int + PresModel$PlotDist*thisdist_cm + PresModel$Rain*(thisShrub$Rain=="D") + PresModel$TranDirS + PresModel$"Dist.DirS"*thisdist_cm)
          if(is.na(angle)) { #shrub is on this cell 
            Prob=mean(ProbN, ProbS) 
          } else{  	
            if(angle>180) angle=angle-2*(angle-180) #make angle between  0- 180
            Prob=(angle/180)*ProbS+(1-angle/180)*ProbN #Weighted avg
          } #end angle check
          Pres=rbinom(1,1,prob=Prob)  #Is plant even present?
          if(Pres){
            RangeCov=inv_logit(CovModel$Int+ CovModel$PlotDist*thisdist_cm + CovModel$Area_Bot*thisShrub$Area_Bot + CovModel$FireUB*(thisShrub$Fire=="UB")-CovModel$RainD*(thisShrub$Rain=="D") + CovModel$"Dist.Area"*thisShrub$Area_Bot*thisdist_cm + CovModel$"Area.FireUB"*thisShrub$Area_Bot*(thisShrub$Fire=="UB"))
          }else RangeCov=0
          if(PredCov[j,i]!=1) Cov=RangeCov
#           if(RangeCov<PredCov[j,i]&PredCov[j,i]!=1) Cov=RangeCov
        } #end thisdist check
      if(!is.na(Cov))  PredCov[j,i]=Cov  #Assign Cov value to the PredCov Matrix
    }# end j loop
  }# end i loop
  setTxtProgressBar(pb, k)
} #end k loop

close(pb)
  
PredCov
  
} # End ApplyHurdleModel