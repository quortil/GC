# function to create a circle
circle <- function(center=c(0,0), radius=1, npoints=360)
{
  r = radius
  tt = seq(0, 2*pi, length=npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# function to get slices
slice2xy <- function(t, rad) 
{
  t2p = -t/180 * pi + pi/2
  list(x = rad * cos(t2p), y = rad * sin(t2p))
}

# function to draw slices
drawSlice <- function(Pstart=0, Pend=180, rad=1.0, Pcolor="red")
{
  S = slice2xy(seq.int(Pstart, Pend, length.out = 30), rad)
  polygon(c(S$x, 0), c(S$y, 0),
          border = Pcolor, col = Pcolor, lty = NULL)
  
}  

# function to get major and minor tick marks
ticks <- function(center=c(0,0), from=0, to=2*pi, radius=0.9, npoints=5)
{
  r = radius
  tt = seq(from, to, length=npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# function to label start and end points
labelPosition <- function(degrees=0, radius=0.9)
{
  v0 = -degrees / 180 * pi + pi/2
  Pos0 = list(x = radius * cos(v0), y = radius * sin(v0))
  text(Pos0, labels=degrees, col="gray50")
}  

# function to plot radial gauge chart
plotRGC <- function (Pstart = 350, Pend = 220, PPstart = 61, PPend = 120, Pedal = "Left")
{
  Pstart = floor(Pstart)
  Pend = floor(Pend)
  PPstart = floor(PPstart)
  PPend = floor(PPend)
    
  if (Pstart >  Pend) 
  {
    Pstart = Pstart - 360
  }
  
  # external circle (this will be used for the black border)
  border_cir = circle(c(0,0), radius=1, npoints = 360)
  

  # coordinates of major ticks (will be plotted as arrows)
  major_ticks_out = ticks(c(0,0), from=0, to=3*pi/2, radius=1.0, 4)
  major_ticks_in = ticks(c(0,0), from=0, to=3*pi/2, radius=0.8, 4)
  
  # open plot
  plot(border_cir$x, border_cir$y, type="n", asp=1, axes=FALSE,
       xlim=c(-1.05,1.05), ylim=c(-1.05,1.05),
       xlab="", ylab="")
  
  # draw slices
  drawSlice(Pstart, Pend, rad=1.0, Pcolor="#FF9900")
  drawSlice(Pstart, Pend, rad=0.8, Pcolor="white")
  drawSlice(PPstart, PPend, rad=0.8, Pcolor="#DC3912")
  drawSlice(Pstart, Pend, rad=0.6, Pcolor="white")
  
  # add external border
  lines(border_cir$x, border_cir$y, col="gray20", lwd=2)
  
  # add major ticks
  arrows(x0=major_ticks_out$x, y0=major_ticks_out$y,
         x1=major_ticks_in$x, y1=major_ticks_in$y, length=0, lwd=4)
  
  # add label of pedal
  text(0, 1.2, Pedal, cex=1.2,font=2)
  text(0.1, 0.0, "Peak", cex=1, col="#DC3912")
  
  # Postion labels
  labelPosition(Pstart, 0.7) 
  labelPosition(Pend, 0.7) 
  labelPosition(PPstart, 0.45) 
  labelPosition(PPend, 0.45) 
}

par(mfrow=c(1,2))
plotRGC(Pstart = 350, Pend = 220, PPstart = 61, PPend = 120, Pedal = "Left")
plotRGC(Pstart = 340, Pend = 210, PPstart = 71, PPend = 130, Pedal = "Right")

