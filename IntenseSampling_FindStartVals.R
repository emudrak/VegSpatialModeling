

i=-0.5; i.sl=0; j=1.25; j.sl=0; m=-2; m.sl=0

#################################

meanPlotDist=mean(Census.Train$PlotDist)
i.seq=seq(-0.25, -0.5, length.out=2)
i.sl.seq= seq(-2, 2, length.out=3)
j.seq=seq(1, 1.5, length.out=3)
j.sl.seq=seq(-2, 2, length.out=3)
m.seq=seq(-1.5, -2.5, length.out=3)
m.sl.seq=seq(-2, 2, length.out=3)


#See if these land on top of data: -------

#Plot stuff
with(Pos.Train, plot(c(0,xplotmax/100), range(Pos.Train$LogitTarg) , pch=NA, main=paste(DESERT, YEAR, TargSpec), ylab=paste("Logit transformmed ", ylabel), xlab="Distance from Shrub Stem" ))
#Plot Lines
for (i in unique(Pos.Train$Shrub)) {  # i=ShrubNumber
    lines((Pos.Train$PlotDist[Pos.Train$Shrub==i]/100),
          Pos.Train$LogitTarg[Pos.Train$Shrub==i], 
          pch=20,lwd=2, type="o", lty=1, col=rainbow(168)[i]
    )
} # end of i loop

#Plot estimated curves
xvals=seq(0, 2.5, length.out=100)
for (a in i.seq){        #a  flattness parameter
    for (h in j.seq){    	#h x location of vertex
        for (k in m.seq){	#k y location of vertex
            lines(xvals, a*(xvals-h)^2+k)   
        }#end m loop
    }#end j loop
}#end i loop

# Find best starting values -------------

counter=0
ModelResults=data.frame(AIC=NA, a=NA, a.sl=NA, h=NA, h.sl=NA,  k=NA, k.sl=NA)
i=i.sl=j=j.sl=m=m.sl=NA
for (i in i.seq){    	#a  flattness parameter
    for (i.sl in i.sl.seq){
        for (j in j.seq){		#h x location of vertex
            for (j.sl in j.sl.seq){
                for (m in m.seq){	#k y location of vertex
                    for (m.sl in m.sl.seq){
                        vert.nlme=try(nlme(LogitTarg~a*((PlotDist/100)-h)^2+k, 
<<<<<<< HEAD
         fixed=list(a ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Area_Bot:Rain  + Fire:Rain,
                    h ~Area_Bot + TranDir + Fire + Rain + Area_Bot:TranDir + Area_Bot:Fire + Fire:Rain,
                    k ~Area_Bot + TranDir + Fire + Rain + Area_Bot:Fire),
         random=a+h+k~1|as.factor(Shrub),  #Three random effects wont converge...
         start=c(a=c(i,i,i,i), c(i.sl, i.sl,i.sl, j.sl, j.sl), h=c(j, j, j,j), c(j.sl, j.sl,j.sl, j.sl),  k=c(m,m,m), c(m.sl, m.sl,m.sl)), 
                                        data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 because it only had one non-zero quadrat
                        , silent=FALSE) #end try
=======
                                           fixed=list(a ~1,
                                                      h ~Area_Bot,
                                                      k ~TranDir + Rain ),
                                           random=a+h+k~1|as.factor(Shrub),   
                                           start=c(a=c(i),  h=c(j), c(j.sl),  k=c(m,m,m)), 
                                           data=Census.Train[(Census.Train$Target>0) & Census.Train$Shrub!=166,])  #Need to remove shrub 166 becasue it only had one non-zero quadrat
                                     , silent=FALSE) #end try
>>>>>>> 25451899e8757036681b5de6b67c7e537bf10016
                        try(print(summary(vert.nlme), silent=TRUE))
                        counter=counter+1
                        print(paste(i,i.sl, j, j.sl, m, m.sl))
                        print(paste("Run ",counter, " of ", length(i.seq)*length(i.sl.seq)*length(j.seq)*length(j.sl.seq)*length(m.seq)*length(m.sl.seq)))
                        aic=try(AIC(vert.nlme), silent=TRUE) 
                        if(class(aic)!="try-error") ModelResults=rbind(ModelResults, c(aic, i, i.sl, j, j.sl, m, m.sl))          
                    }#end m.sl loop
                }#end m loop
            }#end j.sl loop
        }#end j loop
    }#end i.sl loop
}#end i loop


ModelResults
ModelResults[with(ModelResults, order(AIC)),][1:10,]
ModelResults[which.min(ModelResults$AIC),]
ModelResults[which(ModelResults$AIC>0),]




