## General Geometry Functions for use in SERDP project 
## Stuff invovled with converting compass degrees to standard radius notation for use in 
## R trig functions. 

########### FUNCTION radians ########################
radians=function(degs){
# Converts angle given in compass-degrees (0 is North or (0,1) on unit circle) 
# to radians on unit circle (0 is (1,0) on unit circle)
	while(degs < -180) degs =degs +360
	while(degs >=90) 	degs =degs -360
	angle=(90-degs)*pi/180

	return(angle)
} #End radians function

########### FUNCTION degrees ########################
degrees=function(rads){
# Converts angle given in radians on unit circle (0 is (1,0) on unit circle) 
# to compass-degrees (0 is North or (0,1) on unit circle)
	angle=90-(rads*180/pi)
	while(angle < -180)   angle =angle +360
	while(angle >=360) 	angle =angle -360
	return (angle)
}  #End degrees function 

########### FUNCTION getEllipse########################
getEllipse=function(x0,y0,ax1,ax2,orient_deg=0, ...)  {
# Given a shrub centered at x0, y0 with measured axes ax1,ax2 (radii) and orientation in degrees, 
# Returns a set of XY coordinates defining the edge of an ellipse defined by the measured axes, centered at x0 y0
# 
#
theta <- seq(0, 2 * pi, length=(100))  #Set a list of angles to calculate edge points
#orient=(90-orient_deg)*pi/180		#Convert orientation from degrees to radians.
orient=radians(orient_deg)

X <- x0 + ax1 * cos(theta) * cos(orient) - ax2 * sin(theta) * sin(orient) 
Y <- y0 + ax1 * cos(theta) * sin(orient) + ax2 * sin(theta) * cos(orient) 

return(data.frame(X,Y))
}  #End getEllipse



########### FUNCTION getEllipseEdgePoint ########################
getEllipseEdgePoint=function(x0,y0,ax1,ax2,orient_deg=0, ptangle_deg, ...)  {
# Given a shrub centered at x0, y0 with measured axes ax1,ax2 (radii) and orientation in degrees, 
# Returns the XY location of a point on the edge of the ellipse(defined by the measured axes)
# corresponding to the angle given in the polar angle ptangle (degrees)
# ** Due to some sort of issue with atan functions, this will not work for the short axis of the ellipse
#
#Convert ptangle to be within [0,360]
while(ptangle_deg>360) ptangle_deg=ptangle_deg-360
while(ptangle_deg<0) ptangle_deg=ptangle_deg+360

#Convert orient_deg to be within [-90,90]
while(orient_deg < -90) orient_deg=orient_deg+180
while(orient_deg>90) 	orient_deg=orient_deg-180

if((ptangle_deg==orient_deg+90)||(ptangle_deg==orient_deg+270)) 
	{ print("This function will not work on that angle, sorry." ) 
		return(NULL)
	}else
	{
	orient=radians(orient_deg)	#Convert to radians for use in cos() and sin()
	ptangle=radians(ptangle_deg)

	paramt=atan(ax1/ax2*(tan(ptangle)*cos(orient)-sin(orient))/(tan(ptangle)*sin(orient)+cos(orient))) 
	if (ptangle_deg>(90+orient_deg)&&ptangle_deg<(270+orient_deg)) paramt=paramt+pi  #else
	X <- x0 + ax1 * cos(paramt) * cos(orient) - ax2 * sin(paramt) * sin(orient) 
	Y <- y0 + ax1 * cos(paramt) * sin(orient) + ax2 * sin(paramt) * cos(orient) 
	return(data.frame(X,Y))
	}
}
### End getEllipseEdgePoint



########### FUNCTION LineAngle ######################
# This function will take in coordinates of two points and return the angle (radians) of the line 
LineAngle=function(X1,Y1,X2,Y2, units="radians" ){
# X1 Y1 are coords for starting point, X2 Y2 are coords for ending point
#Find Quadrant: 
	if (X2>=X1  && Y2>=Y1) quadrant=1 else
	if (X2< X1  && Y2>=Y1) quadrant=2 else
	if (X2< X1  && Y2< Y1) quadrant=3 else
	if (X2>=X1  && Y2< Y1) quadrant=4 
	
	angle_rad=atan((Y2-Y1)/(X2-X1))

	if (quadrant==2) angle_rad=angle_rad-pi      else
	if (quadrant==3) angle_rad=angle_rad-pi  

	if (units=="degrees") 	return (90-(angle_rad*180/pi)) else   #convert to degrees
	if (units=="radians")  	return (atan((Y2-Y1)/(X2-X1))) else
	print ("Please enter either units='degrees' or units='radians'")
} #End Function LineAngle




########### FUNCTION MoveTurtle #####################
# This function takes starting coords, moves a given distance dist along an angle theta
# and returns ending coords
# theta must be in radians
MoveTurtle=function(X0,Y0, dist, theta){
# X0 Y0 are coords for starting point
# dist is distance to be moved, theta is angle of direction in radians
   	X1=X0+cos(theta)*dist
	Y1=Y0+sin(theta)*dist
      return(c(X1,Y1))
} # End MoveTurtle

#############











####################################################################################
###############################  TESTING PROCEDURES ################################
####################################################################################
if(FALSE){		# Don't run this when sourcing this code
### This code is messy. It is what I used to test each of the above functions
### And I don't want to lose these test bits


### Test degrees and radians
TestDegrees=c(0,30,45,60,90,120,135,150,180,210,225,240,270,300,315,330,360)
TestRadians=c(0,pi/6,pi/4,pi/3,pi/2,2*pi/3, 3*pi/4,5*pi/6,pi,7*pi/6,5*pi/4, 4*pi/3, 3*pi/2, 5*pi/3, 7*pi/4, 11*pi/6, 2*pi)

TestRadNames=c("0","pi/6","pi/4","pi/3","pi/2","2*pi/3", "3*pi/4","5*pi/6","pi",
			"7*pi/6","5*pi/4", "4*pi/3", "3*pi/2", "5*pi/3", "7*pi/4", "11*pi/6", "2*pi")

degtorad=rep(NA,length(TestDegrees))
for (i in 1:length(TestDegrees)){
degtorad[i]=radians(TestDegrees[i])

}
TestRadNamestoDegrees=TestRadNames[c(5:1,16:6)]
 cbind(TestDegrees[1:16], degtorad[1:16], TestRadians[c(5:1,16:6)], TestRadNamestoDegrees)


negdegtorad=rep(NA,length(TestDegrees))
for (i in 1:length(TestDegrees)){
negdegtorad[i]=radians(-1*TestDegrees[i])

}
TestRadNamestoDegrees=TestRadNames[c(5:1,16:6)]
 cbind(-1*TestDegrees[1:16], negdegtorad[1:16], TestRadians[c(5:16,1:4)], TestRadNamestoDegrees)



### Check other direction conversion
radtodeg=rep(NA,length(TestRadians))
for (i in 1:length(TestRadians)){
radtodeg[i]=degrees(TestRadians[i])

}
 cbind(TestRadians[1:16], TestRadNames[1:16], radtodeg[1:16], TestDegrees[c(5:1,16:6)])


# Test out getEllipse and getEllipseEdgePoint

# Test it out
Xc=4
Yc=4
AX1=8
AX2=8
DEG=3
for (i in 1:360){
DEG=i
jpeg(file=paste("EllipsePics/Deg",DEG,".jpg"))
#plot(getEllipse(x0=Xc,y0=Yc,ax1=AX1,ax2=AX2,orient_deg=DEG),type='l',asp=1)
plot(getEllipse(x0=Xc,y0=Yc,ax1=AX1,ax2=AX2,orient_deg=DEG),pch=NA,asp=1, main=paste("DEG= ", DEG))
abline(h=Yc,lty=2)
abline(v=Xc,lty=2)

ptangles=seq(from=0,to=360, by=1)
#ptangles=seq(from=120,to=150, by=1)
colors=colorRampPalette(c("red", "yellow", "blue"))
for (i in 1:length(ptangles)) {
	points(getEllipseEdgePoint(x0=Xc,y0=Yc,ax1=AX1,ax2=AX2,orient_deg=DEG,ptangle_deg=ptangles[i]),pch=4,cex=0.5,col=colors(length(ptangles))[i])
	#text(getEllipseEdgePoint(x0=Xc,y0=Yc,ax1=AX1,ax2=AX2,orient_deg=DEG,ptangle_deg=ptangles[i]),label=ptangles[i],cex=0.8,pch=19,col=colors(length(ptangles))[i])
	#pause()
	}

points(getEllipseEdgePoint(x0=Xc,y0=Yc,ax1=AX1,ax2=AX2,orient_deg=DEG,ptangle_deg=90+DEG),pch=19,col="green")
points(getEllipseEdgePoint(x0=Xc,y0=Yc,ax1=AX1,ax2=AX2,orient_deg=DEG,ptangle_deg=270+DEG),pch=19,col="red")

dev.off()
}

########################################
## Test LineAngle
X2s=c(1,sqrt(3)/2,sqrt(2)/2, 1/2, 0, -1/2, -sqrt(2)/2,-sqrt(3)/2,
	-1,-sqrt(3)/2,-sqrt(2)/2, -1/2, 0, 1/2, sqrt(2)/2,sqrt(3)/2)
Y2s= c(0, 1/2, sqrt(2)/2,sqrt(3)/2,1,sqrt(3)/2,sqrt(2)/2, 1/2, 
	0, -1/2, -sqrt(2)/2,-sqrt(3)/2,-1,-sqrt(3)/2,-sqrt(2)/2, -1/2 )
for(i in 1:length(X2s) ) print(LineAngle(0,0,X2s[i], Y2s[i], "degrees"))
LineAngle(0,0,-3,-3, "degrees")
LineAngle(0,0,0,-3, "degrees")
LineAngle(0,0,0,-3, "radians")

outs=cbind(TestDegrees, X=NA, Y=NA)
# Test MoveTurtle
for (i in 1:length(TestDegrees)){
outs[i,c(2,3)]=round(MoveTurtle(3,3,1, radians(TestDegrees[i])),3)
}

}  #End if FALSE  Testing block

