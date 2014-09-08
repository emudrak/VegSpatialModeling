

###################################################################################
########################## Hurdle MODEL ##########################################
###################################################################################
ApplyHurdleModel=function(PresModel, CovModel, ShrubData, InterspaceData, BlankImage.im, Allometry  ) {
  # Applies Negative Exponential Style Model to make a nutrient hotspot map image. 
  # Model is a data frame with 1 row with columns    "Desert"     "Nutrient"   "Model"  
  # "AIC"        "PARM.mu.N"  "PARM.mu.S"  "PARM.slope" "PARM.sig"   "PARC.mu.N"  "PARC.mu.S" 
  # "PARC.slope" "PARC.sig"   "SIGMA"    
  # ShrubData is a data frame containing information on shrubs in area to be mapped: 
  #    columns: Easting","Northing","Area_Bot"
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
      select=c("Easting","Northing","Area_Bot","Rain","Fire"))	#Only want certain info
  rownames(SampleShrub)=1:nrow(SampleShrub)
  SampleShrub$Range=2.50  

  
  #############################################################################
  # initialize background (for points not under influence of any shrub) to be
  # Sampled from the the same distribution observed values of the "outer" samples 
  # here, outer will be >150
#   
#   PredCov=matrix(data=rlnorm(n=BlankImage.im$dim[2]*BlankImage.im$dim[1], 
#                              meanlog= InterspaceData$Bglogmean , 
#                              sdlog=InterspaceData$Bglogsd),
#                  ncol=BlankImage.im$dim[2], nrow=BlankImage.im$dim[1])
PredCov=matrix(data=.20,
               ncol=BlankImage.im$dim[2], nrow=BlankImage.im$dim[1])

#   ## Take out when going outside of R
#   PredCov.im=im(PredCov, BlankImage.im$xcol, BlankImage.im$yrow, unitname=c("meter","meters"))
#   
#   ################# GET RANGE OF INFLUENCE FOR SHRUB
#   #Based on background noise.  #For neg exponential, go out to 90% of background
#   BGcut=qlnorm(.90, meanlog= InterspaceData$Bglogmean , sdlog=InterspaceData$Bglogsd)
#   SampleShrub$Range.N=((BGcut-SampleShrub$c.n)/SampleShrub$m.n)/100
#   SampleShrub$Range.S=((BGcut-SampleShrub$c.s)/SampleShrub$m.s)/100
#   SampleShrub$Range=apply(rbind(SampleShrub$Range.N,SampleShrub$Range.S),2,max)
#   #For shrubs that get very small ranges, make them go at least as far out as the calculated radius. 
#   SampleShrub$Range[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]=sqrt((SampleShrub$Area_Bot/10000)/pi)[SampleShrub$Range<sqrt((SampleShrub$Area_Bot/10000)/pi)]
#   #For Sonoran Mg: Make maximum 5 m
#   SampleShrub$Range[SampleShrub$Range>5]=5


  ###############	General Code	########################
  
  pb <- txtProgressBar(min = 0, max = nrow(SampleShrub), style = 3)
  
  for (k in 1:nrow(SampleShrub)){
    thisShrub=SampleShrub[k,]
    ShrubPixel=topixel(BlankImage.im, thisShrub$Easting, thisShrub$Northing)
    # Use Area_Bot to get approximate radius
    thisShrubRad=sqrt(thisShrub$Area_Bot/pi)	
    thisXrange=trunc((thisShrubRad + thisShrub$Range)/BlankImage.im$xstep) 	# Check if I used proper trunc/ceil/round
    thisYrange=trunc((thisShrubRad + thisShrub$Range)/BlankImage.im$ystep) 	# Check if I used proper trunc/ceil/round
    #Make sure pixels to check are within range
    xlist=(ShrubPixel[1]-thisXrange): (ShrubPixel[1]+thisXrange)
    ylist=(ShrubPixel[2]-thisYrange): (ShrubPixel[2]+thisYrange)
    xlist=xlist[(xlist>0)&(xlist<BlankImage.im$dim[2]+1)]
    ylist=ylist[(ylist>0)&(ylist<BlankImage.im$dim[1]+1)]
    for (i in xlist){
      for (j in ylist ){
        me=data.frame(Easting=BlankImage.im$xcol[i],Northing=BlankImage.im$yrow[j])
        thisdist=dist(rbind(me,thisShrub[c("Easting","Northing")]))

        ProbN=inv_logit(PresModel$"(Intercept)" +PresModel$PlotDist*thisdist +PresModel$Rain*(thisShrub$Rain=="D"))
        ProbS=inv_logit(PresModel$"(Intercept)" +PresModel$PlotDist*thisdist +PresModel$Rain*(thisShrub$Rain=="D") +PresModel$TranDirS + PresModel$"PlotDist:TranDirS"*thisdist)
        CalcCov=inv_logit(CovModel$"(Intercept)"+CovModel$PlotDist*thisdist + CovModel$Area_Bot*thisShrub$Area_Bot+CovModel$FireUB*(thisShrub$Fire=="UB")-CovModel$RainD*(thisShrub$Rain=="D")+CovModel$"PlotDist:Area_Bot"*thisShrub$Area_Bot*thisdist + CovModel$"Area_Bot:FireUB"*thisShrub$Area_Bot*(thisShrub$Fire=="UB"))
        if (thisdist< thisShrubRad) { # Point is within shrub radius pixel is under shrub
          Cov=1  
          #print("Shrub")
        }else
        if (thisdist < thisShrubRad+ thisShrub$Range){  #within range 
          angle=LineAngle(thisShrub$"Easting",thisShrub$"Northing",
                          me$Easting, me$Northing, units="degrees")
          if(is.na(angle)) { #shrub is on this cell 
            Prob=mean(ProbN, ProbS) 
          } else{  	
          if(angle>180) angle=angle-2*(angle-180)   #Reflect across y=axis, scale from 0 to 180
            Prob=(angle/180)*ProbS+(1-angle/180)*ProbN #Weighted avg
          } #end angle check

          Pres=rbinom(1,1,prob=Prob)
         # print(Pres)
          Cov=CalcCov*Pres # add rand effects? +rnorm(1,0,Model$SIGMA)
          #If this shrub's influence is higher than current value, replace the value
          #if(PredCov[j,i]<Cov) 
            PredCov[j,i]=Cov    
          } #end thisdist check
        
      }# end j loop
    }# end i loop
    setTxtProgressBar(pb, k)
  } #end k loop
  
  close(pb)
  
PredCov.im=im(PredCov, xcol=BlankImage.im$xcol,yrow=BlankImage.im$yrow)


plot(PredCov.im, col=gray(0:100/100))
  
  return(list(image=PredNut.im, table=SampleShrub))
  
} # End ApplyLinearModel