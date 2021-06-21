spinWheel <- function(noSpin=FALSE){
  
  
## Parameters
DD <- 10 # Diameter of wheel
dd <- 4 ## wheel diameter where ball lands
Turns <- 2 ## Min number of wheel turns till we land on the number
numberAngle <- 360/37 # Angle of each of the 37 segments
deg2rad <- function(x) {x * pi / 180}


# Info from Wikipedia (European wheel):
## https://en.wikipedia.org/wiki/Roulette#Roulette_wheel_number_sequence
# (NOTE: 37 means "0")

numberLabels <-  c(0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 13, 36, 
                   11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 31, 9, 
                   22, 18, 29, 7, 28, 12, 35, 3, 26)
colRed <- c(
  seq(1, 10, by=2),
  seq(19, 28, by=2),
  seq(12, 18, by=2),
  seq(30, 36, by=2)
)
numberColours <- rep(NA, 38)
numberColours <- rep( rgb(0, 0, 0, max = 255, alpha = 125, names = "blackWheel"), 37)


numberColours[numberLabels%in%colRed] <-  rgb(255, 0, 0,     max = 255, alpha = 125, names = "redWheel")
numberColours[numberLabels%in%0] <-  rgb(0, 255, 0,     max = 255, alpha = 125, names = "greenWheel")
numberColours[38] <-  rgb(0, 255, 0,     max = 255, alpha = 125, names = "greenWheel")

coloursInWords <- rep("black", 38)
coloursInWords[numberLabels%in%colRed] <-  "red"
coloursInWords[numberLabels%in%0] <-  "green"
coloursInWords[38] <- "green"


##
XX <- sample(1:37, 1, replace=TRUE) # There are 37 numbers


## Start position for ball
Start <- c(0, D)

## Angles
phi <- c( rep( seq(0, 360, by=numberAngle), Turns),
          (0:XX) * numberAngle ) +
        numberAngle/2 # To get ball in the centre of the segments
phir <- deg2rad(phi) # Convert to radians

# Radius that changes
radius <- seq(0.8 * DD, dd, length=length(phi) )

delay <- seq( 0.01, 0.1, length=length(phi) )

# Grey that colours, to show "moving" ball
greyBall <- rgb(100, 100, 100,    
                max = 255, 
                alpha = seq(125, 0, length=length(phi)))





# Plot wheel
plotWheel <- function(DD, numberAngle, numberColours){
  
  #cat(".")
  plot( x=cos( deg2rad(seq(0, 360, by=1))) * DD,
        y=sin( deg2rad(seq(0, 360, by=1))) * DD,
        axes=FALSE,
        xlab="", 
        ylab="",
        type="n", # DO the outer lines later to define them properly
        asp=1)
  
  # Colours on the segments
  #cat(".")
  for (i in (1:37)){
    colAngle <- numberAngle*(i-1)
    
    polygon(x = c(0, seq( cos( deg2rad(colAngle) ) * DD, 
                          cos( deg2rad( colAngle + numberAngle) ) * DD, length=10), 0 ),
            y = c(0, seq( sin( deg2rad(colAngle) ) * DD, 
                          sin( deg2rad( colAngle + numberAngle) ) * DD, length=10), 0 ),
          col = numberColours[i+1]) # Because the first one is for 0
  }
  
  # Plot inner wheel2: Overwrites segment colours
  #cat(".")
  polygon(x=cos( deg2rad(seq(0, 360, by=0.2))) * dd * 1.5,
          y=sin( deg2rad(seq(0, 360, by=0.2))) * dd * 1.5,
          col="white")
  #cat(".")
  polygon(x=cos( deg2rad(seq(0, 360, by=0.2))) * dd,
          y=sin( deg2rad(seq(0, 360, by=0.2))) * dd,
          col="grey")
  
  
  # Plot "spokes": Overwrite segment colours
  #cat(".|")
  spokes <- seq(0, 360, by=numberAngle)
  for (i in (1:length(spokes))){
    segments( x0 = 0, 
              y0 = 0,
              x1 = DD * cos( deg2rad(spokes[i])), 
              y1 = DD * sin( deg2rad(spokes[i])),
              col="grey")
  }
  
  # Show numbers
  textAngle <- spokes - (numberAngle/2)
  text(x = cos( deg2rad( textAngle)) * 0.9 * DD,
       y = sin( deg2rad( textAngle)) * 0.9 * DD,
       labels=numberLabels,
       col="black")

  # Redo outside wheel outline
  lines( x=cos( deg2rad(seq(0, 360, by=0.05))) * DD,
         y=sin( deg2rad(seq(0, 360, by=0.05))) * DD,
         lwd=2)
}       
           
plotWheel(DD, numberAngle, numberColours)

# Plot wheel centre
if ( !noSpin) {
  for (i in (2:length(phir))){
  
    plotWheel(DD, numberAngle, numberColours)
    # Previous position
    if ( i != length(phir)) {
      points( x = cos(phir[i-1]) * radius[i-1],
            y = sin(phir[i-1]) * radius[i-1],
            pch=19,
            cex=2,
            col=greyBall[i])  
    }
    # Current position of ball
    points( x = cos(phir[i]) * radius[i],
            y = sin(phir[i]) * radius[i],
            pch=19,
            cex=2,
            col="black")  
    Sys.sleep( delay[i])
    
  }
}

if (noSpin) {
  roll <- NA
  colour <- NA
} else {
  roll <- numberLabels[XX+2]
  colour <- coloursInWords[XX+2]
}
return( list(roll = roll,
             colour = colour ) )
}


