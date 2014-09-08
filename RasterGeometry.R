topixel=function(pixelim, coordx, coordy         ){
# pixelim of class "im"   MAYBE REWRITE TO INCLUDE MATRICES TOO                 9
# coordx and coordy are of the same units as the image's xrange and yrange
# (and hopefully with the ranges)
# This function identifies the coordinates of the pixel containing 
# coordx and coordy
if(class(pixelim)!="im") print("Please supply an image or a matrix") else
#Check to make sure pixx and pixy are in range
#if(class(pixelim)!="im"&&class(pixelim)!="matrix") print("Please supply an image or a matrix") else
#Check to make sure pixx and pixy are in range
if((coordx>pixelim$xrange[2])|(coordx<pixelim$xrange[1])|
	(coordy<pixelim$yrange[1])|(coordy>pixelim$yrange[2])) print ("Coordinates are outside range of image") else
	{
	# find difference between x coord and left x boundary
	xdist=coordx-pixelim$xrange[1]
	ydist=coordy-pixelim$yrange[1]
	#convert to pixel distances, not the units of the image
	pixx=ceiling(xdist/pixelim$xstep)
	pixy=ceiling(ydist/pixelim$ystep)
	#print(paste("pixx= ", pixx, "pixy=", pixy))
	return(c(pixx,pixy))	
	}
} #end topixel function



## Test this

#xs=seq(from=5, to=6, by=0.25)
#ys=seq(from=6, to=8, by=0.15)
#testvals=matrix(rnorm(length(xs)*length(ys), 4, 0.6), nrow=length(xs), ncol=length(ys))
#test.im=im(t(testvals), xs, ys)
#plot(test.im, axes=TRUE)


#topixel(test.im, 5, 6)

#testvals[1,14]=10
#test.im=im(t(testvals), xs, ys)
#plot(test.im, axes=TRUE)
#points(5, 8)