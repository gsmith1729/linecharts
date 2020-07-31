# could do with some rigourous testing

# define function
# x vector, list of y vectors, list of rgb triplet vectors entries in [0,1], 2d padding vectors, 2d ylim vector if needed
plotplus <- function(x,ys,rgbs=list(c(1,0,0)),ypadding=c(-0.05,0.05),xpadding=c(0,0.05),ylim=""){
	# define for use in next step
	shorten <- function(y,x){
		return (y[1:min(length(x),length(y))])
	}
	# find the maximal x and y values that will be plotted (assuming all x values will be plotted atm out of laziness)
	ymax <- max(sapply(sapply(ys,shorten, x), max))
	ymin <- min(sapply(sapply(ys,shorten, x), min))
	# adding the option aesthetic padding
	ypadding <- (ymax-ymin)*ypadding
	if (ylim==""){
		ylim <- c(ymin,ymax)+ypadding
	}
	xpadding <- (max(x)-min(x))*xpadding
	xlim <- c(min(x),max(x))+xpadding
	# a smaller circle but can be changed
	point=16
	# loop over y vectors
	for (i in 1:length(ys)){
		# take the colours in a loop reusing if they have all been used
		rgb=rgbs[[(i-1)%%length(rgbs)+1]]
		# take this y
		y=ys[[i]]
		# make sure the number of entries matches the number in x as appropriate
		if (length(y)>length(x)){
			print(paste("Length of y greater than x on loop", toString(i)))
			y <- y[1:length(x)]
		}
		# save xp for this loop only
		if (length(x)>length(y)){
			print(paste("Length of x greater than y on loop", toString(i)))
			xp <- x[1:length(y)]
		}
		else {
			xp <- x
		}
		# plot rather than point on first loop
		if (i==1){
			# x,y,ylim,xlim,point,colour, xaxs yaxs not sure but didn't work until added, line width, no axis labels (drawn later)
			plot(xp,y,ylim=ylim,xlim=xlim,pch=point,col=rgb(rgb[1],rgb[2],rgb[3]), xaxs="i", yaxs="i",lwd=2.5,yaxt="n",xaxt="n")
			# colour for grid
			grey=rgb(220/255, 220/255, 220/255)
			# remove end lines, axis ticks and axis labels: cex.axis shrinks font, mpg moves points closer to axis
			axis(1,  col=grey, col.ticks="white", cex.axis=0.7,mgp=c(3, 0, 0))
			axis(2,  col.ticks="white", cex.axis=0.7, mgp=c(3, 0.3, 0), las=1) # las rotates y axis labels
			axis(3, col="white", col.ticks="white", col.axis="white")
			axis(4, col="white", col.ticks="white", col.axis="white")
			# draw grid
			for (i in 1:length(axTicks(2))){
				abline(h=axTicks(2)[i],col=grey)
			}
			for (i in 1:length(axTicks(1))){
				abline(v=axTicks(1)[i],col=grey)
			}
			
			abline(v=xlim[2],col="white")
			abline(v=xlim[1],col="white")
			abline(h=ylim[2],col="white")
			abline(h=ylim[1],col="white")

			# draw black lines x=0, y=0 could be removed
			abline(h=0,col="black")
			abline(v=0,col="black")
			# redraw the points for the first y on top of the grid
			points(xp,y,ylim=ylim,pch=point,col=rgb(rgb[1],rgb[2],rgb[3]))
		}
		# obviously skip all that stuff after it has been done
		else {
			points(xp,y,col=rgb(rgb[1],rgb[2],rgb[3]),pch=point)
		}
		# lwd=line width
		lines(xp,y,col=rgb(rgb[1],rgb[2],rgb[3]),lwd=2.5)
	}
}

# sample usage
#data
x <- (0:105)/10
y1 <- (x^2)/10
y2 <- (x^2+1)/15
y3 <- sin(x)^2
y4 <- sin(x+0.3)^2+0.3
# colours
lightgold <- c(255, 233, 156)/255
gold <- c(255, 203, 23)/255
lightblue <- c(138, 142, 255)/255
blue <- c(20, 28, 255)/255

plotplus(x,list(y1,y2,y3,y4),ypadding=c(0,0.1),rgbs=list(blue,lightblue,gold,lightgold))



