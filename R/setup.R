library("kableExtra")  # For tables
library("NHANES")      # or NHANES data
library("knitr")
library("DT")          # For displaying tables
library("webshot")     # screen shot of HTML animations
library("GLMsData")    # For some data sets
library("webex") # For inline exercises
library("gifski") # Trying for animations: https://bookdown.org/yihui/rmarkdown-cookbook/animation.html


# set global chunk options
options(formatR.arrow=TRUE,width=90)

# Textbook colour for plots:
# blueTransparent <- rgb(0, 0, 1, 
#                       alpha = 0.2, 
#                       maxColorValue = 1)

# But change to colour on cover page:
yellowishTransparent <- rgb(169, 179, 145,
                          alpha = 0.2,
                          maxColorValue = 255)

plot.colour <- yellowishTransparent


# Environment defaults
foldLaTeXText <- "The answer is given in the online book."


knitr::opts_chunk$set(fig.pos = "hbtp")   # Place tables HERE and so on


# knitr:::is_latex_output()
# knitr:::is_html_output()
# 
# This example from: https://stackoverflow.com/questions/41745170/how-to-do-different-things-in-r-in-bookdown-if-output-is-html-or-latex
# 
# if( knitr:::is_latex_output() ) {
#     xlatex ...
# } else {
#     DT...
# }
# 







plot.norm <- function(mu, sd, xlab.name="Variable",
                      new=TRUE,
                      shade.lo.x=NA, shade.hi.x=NA,
                      shade.lo.z=NA, shade.hi.z=NA,
                      show.lo=NA, show.hi=NA,
                      round.dec=1,
                      shade.col="wheat",
                      main="",
                      width=6, # WAS 3.5
                      height=width,
                      type="z",
                      srt=0,
                      cex.tickmarks=1,
                      las=1,
                      xlim.hi = NA, xlim.lo = NA,
                      zlim.hi = 3.5, zlim.lo=-zlim.hi,
                      axis.labels=NULL){
  
  # mu  is the mean of the distn
  # sd  is the std dev of the distn
  # xlab.name  is the  xlab  label
  # new is TRUE by default: a new plot is drawn.  If FALSE, the plot is added to the current device 
  # shade.lo.x  is the lower shade limit (in terms of x, not z)
  # shade.hi.x  is the upper shade limit (in terms of x, not z)
  # shade.lo.z  is the lower shade limit (in terms of z, not x)
  # shade.hi.z  is the upper shade limit (in terms of z, not x)
  # show.lo  is a LOGICAL for showing the lower x-score 
  #   If it is a number, that number is placed at the lo position instead
  # show.hi  is a LOGICAL for showing the lower x-score explicitly
  #   If it is a number, that number is placed at the lo position instead
  # zlim.lo  /zlim.hi  is the lower (upper) limit of z on which to draw
  # round.dec  is the number of decimals to round to on the shown x-axis
  #    (full precision used in calculations)
  # type  is the the type of course, generally "z" or "t", placed as a label on the horizontal axis
  # las: The  las  parameter in par, for labelling horizontal axis
  # shade.col  is the shading colour, defaulting to "wheat" (see ?colours)
  # srt: String rotation of the x-axis labels.
  # cex.tickmarks: The value of  cex  for the tickmark labels
  # main  is the main title to use
  # width  and  height  specify the width and height of the x11 device window
  
  if ( is.na(shade.lo.x) & is.na(shade.lo.z) ) {
    warning("One of  shade.lo.x  and shade.lo.z  must be given.")
  }
  if ( is.na(shade.hi.x) & is.na(shade.hi.z) ) {
    warning("One of  shade.hi.x  and shade.hi.z  must be given.")
  }
  
  if ( new ) {
    par(mar=c(2,0,2,0) + 0.1  )
  }
  
  if (!is.na(xlim.hi) ){
    zlim.hi <- (xlim.hi-mu)/sd
  }
  if (!is.na(xlim.lo) ){
    zlim.lo <- (xlim.lo-mu)/sd
  }
  
  hor <- seq(zlim.lo, zlim.hi, length=250) # z-scores
  nc <- dnorm(hor, 0, 1) # Normal curve
  extra <- 0.25 # extra space at ends
  spacer <- -0.05 # space to other x-axis
  text.loc.z <- c(-3, -2, -1, 0, 1, 2, 3) 		# Where to place x-axis labels: In terms of z
  text.loc.x <- round(mu + text.loc.z * sd, round.dec) 		# Where to place x-axis labels: In terms of x 
  
  if ( is.na(shade.lo.z) ) {
    shade.lo.z <- (shade.lo.x - mu)/sd
  }
  if ( is.na(shade.hi.z) ) {
    shade.hi.z <- (shade.hi.x - mu)/sd
  }
  if (is.na(shade.lo.x) ) {
    shade.lo.x <- shade.lo.z * sd + mu
  }
  if (is.na(shade.hi.x) ) {
    shade.hi.x <- shade.hi.z * sd + mu
  }
  
  if (new) {
    plot( nc ~ hor, 
          axes=FALSE,
          ylim=c(-0.1, 0.4),
          xlim=c(zlim.lo-2*extra , zlim.hi+2*extra),
          lwd=2,
          xlab="",
          ylab="",
          main=main,
          type="l")
  }
  
  # Horizontal axis
  lines( c(zlim.lo-extra, zlim.hi+extra), 
         c(0,0),
         lwd=2 )
  
  # Add arrow to axis
  arrows(0, 0, 3.75, 0, 
         length=0.15, 
         angle=20, 
         lwd=2)
  
  # Add line corresponding to the mean (z=0)
  lines( c(0,0.4) ~ c(0, 0), 
         lwd=2,
         col="grey")
  
  # Titles
  title(sub=xlab.name, 
        line=0)
  
  # Label axis;  type  is usually "t" or "z" or is blank (i.e. type="")
  text(zlim.hi+extra, 0, 
       pos=3, 
       adj=0, 
       type)
  
  # Label horizontal axis
  if ( is.null(axis.labels)){
    text( text.loc.z, 0, 
        pos=1, 
        cex=cex.tickmarks,
        srt=srt,
        labels=as.character(text.loc.x) )
  } else {
    text( text.loc.z, 0, 
          pos=1, 
          cex=cex.tickmarks,
          srt=srt,
          labels=axis.labels )
  }
  
  # Add lines to demarcate shading
  lines(c(shade.lo.z, shade.lo.z), c(0, dnorm(shade.lo.z,0,1)), lwd=2)
  lines(c(shade.hi.z, shade.hi.z), c(0, dnorm(shade.hi.z,0,1)), lwd=2)
  
  # What to shade?
  if ( !is.na(show.lo) ) {
    if (is.logical(show.lo) ){
      if ( show.lo ) {
        lines( c(shade.lo.z, shade.lo.z), c(spacer, dnorm(shade.lo.x,0,1)))
        text(shade.lo.z, -0.11, as.character(shade.lo.x) )
      }
    } else { # Not logical:  a value/character is supplied
      lines( c(shade.lo.z, shade.lo.z), c(spacer, dnorm(shade.lo.x,0,1)))
      text(shade.lo.z, -0.11, as.character(show.lo) )
    }
  }
  
  if ( !is.na(show.hi) ) {
    if (is.logical(show.hi) ){
      if( show.hi ) {
        lines( c(shade.hi.z, shade.hi.z), c(spacer, dnorm(shade.hi.x,0,1)))
        text(shade.hi.z, -0.11, as.character(shade.hi.x) )
      }
    } else {
      lines( c(shade.hi.z, shade.hi.z), c(spacer, dnorm(shade.hi.x,0,1)))
      text(shade.hi.z, -0.11, as.character(show.hi) )
    }
  }
  
  # Add Shading
  x.poly <- seq( max(zlim.lo, shade.lo.z), 
                 min(zlim.hi, shade.hi.z), length=100)
  y.poly <- dnorm( x.poly, 0, 1)
  
  x.p <- c( x.poly, rev(x.poly) )
  y.p <- c( rep(0, length(x.poly)), rev(y.poly) )
  polygon(x.p, y.p, col=shade.col )
  
  # Rug on horizontal axis
  yrange <- max(nc) - 0
  for (i in 1:length(text.loc.z)) {
    lines( c(text.loc.z[i], text.loc.z[i]),
           c(0, 0.05*yrange),
           col="grey")
  } 
}





##############################################################
##############################################################
##############################################################
##############################################################






plot.normZ <- function(mu, sd, xlab.name="Variable",
                       new=TRUE,
                       shade.lo.x=NA, shade.hi.x=NA,
                       shade.lo.z=NA, shade.hi.z=NA,
                       show.lo=NA, show.hi=NA,
                       round.dec=1,
                       shade.col="wheat",
                       main="",
                       width=6, # WAS 3.5
                       height=width,
                       type="z",
                       las=1,
                       srt=0,
                       cex.tickmarks=1,
                       zlim.hi = 3.5, zlim.lo=-zlim.hi,
                       axis.labels=NULL){
  
  # mu  is the mean of the distn
  # sd  is the std dev of the distn
  # xlab.name  is the  xlab  label
  # new is TRUE by default: a new plot is drawn.  If FALSE, the plot is added to the current device 
  # shade.lo.z  is the lower shade limit (in terms of z, not x)
  # shade.hi.z  is the upper shade limit (in terms of z, not x)
  # show.lo  is a LOGICAL for showing the lower x-score 
  #   If it is a number, that number is placed at the lo position instead
  # show.hi  is a LOGICAL for showing the lower x-score explicitly
  #   If it is a number, that number is placed at the lo position instead
  # zlim.lo  /zlim.hi  is the lower (upper) limit of z on which to draw
  # round.dec  is the number of decimals to round to on the shown x-axis
  #    (full precision used in calculations)
  # type  is the the type of course, generally "z" or "t", placed as a label on the horizontal axis
  # las: The  las  parameter in par, for labelling horizontal axis
  # shade.col  is the shading colour, defaulting to "wheat" (see ?colours)
  # main  is the main title to use
  # width  and  height  specify the width and height of the x11 device window
  
  #if ( is.na(shade.lo.z) ) {
  #	warning(" shade.lo.z  must be given.")
  #}
  #if (  is.na(shade.hi.z) ) {
  #	warning(" shade.hi.z  must be given.")
  #}
  
  if ( new ) {
    #   quartz(width=width,height=height, bg="white")
    par(mar=c(2,0,2,0) + 0.1  )
  }
  
  if ( is.na(shade.lo.z) ) {
    shade.lo.z <- (shade.lo.x - mu)/sd
  }
  if ( is.na(shade.hi.z) ) {
    shade.hi.z <- (shade.hi.x - mu)/sd
  }
  if (is.na(shade.lo.x) ) {
    shade.lo.x <- shade.lo.z * sd + mu
  }
  if (is.na(shade.hi.x) ) {
    shade.hi.x <- shade.hi.z * sd + mu
  }
  
  
  hor <- seq(zlim.lo, zlim.hi, length=250) # z-scores
  nc <- dnorm(hor, 0, 1) # Normal curve
  extra <- 0.25 # extra space at ends
  spacer <- -0.05 # space to other x-axis
  text.loc <- c(-3, -2, -1, 0, 1, 2, 3) # In terms of z
  
  
  if (new) {
    plot( nc ~ hor, 
          axes=FALSE,
          ylim=c(-0.1, 0.4),
          xlim=c(zlim.lo-2*extra , zlim.hi+2*extra),
          lwd=2,
          xlab="",
          ylab="",
          main=main,
          type="l")
  }
  
  lines( c(zlim.lo-extra, zlim.hi+extra) , c(0,0), lwd=2 )
  #arrows(0, spacer, 3.75, spacer, length=0.15, angle=20, lwd=2)
  
  #lines( c(zlim.lo-extra, zlim.hi+extra) , c(spacer, spacer), lwd=2 )
  #arrows(0, 0, 3.75, 0, length=0.15, angle=20, lwd=2)
  
  lines( c(0,0.4) ~ c(0, 0), lwd=2 )
  
  # text(zlim.hi+1.5*extra, spacer, adj=0,"z")
  #text(zlim.hi-extra, 2*spacer, pos=1, adj=0,xlab.name)
  title(sub=xlab.name, line=0)
  
  #text(zlim.hi+1.5*extra, 0, adj=0,xlab.name)
  text(zlim.hi+extra, 0, pos=3, adj=0, "z")

  # Label horizontal axis
  if ( is.null(axis.labels)){
    text( text.loc, 0, 
          pos=1, 
          cex=cex.tickmarks,
          srt=srt,
          labels=as.character(text.loc) )
  } else {
    text( text.loc, 0, 
          pos=1, 
          cex=cex.tickmarks,
          srt=srt,
          labels=axis.labels )
  }
#  text( text.loc, 0, pos=1, labels=as.character(text.loc), las=las )
  #text( text.loc, spacer, pos=1, 
  #   labels=as.character(round(text.loc*sd + mu, round.dec)) )
  
  
  lines(c(shade.lo.z, shade.lo.z), c(0, dnorm(shade.lo.z,0,1)), lwd=2)
  lines(c(shade.hi.z, shade.hi.z), c(0, dnorm(shade.hi.z,0,1)), lwd=2)
  
  if ( !is.na(show.lo) ) {
    if (is.logical(show.lo) ){
      if ( show.lo ) {
        lines( c(shade.lo.z, shade.lo.z), c(0, dnorm(shade.lo.z,0,1)))
        text(shade.lo, -0.11, as.character(shade.lo.z) )
        #	      text(shade.lo, spacer-150, as.character(shade.lo.z) )
      }
    } else { # Not logical:  a value/character is supplied
      lines( c(shade.lo.z, shade.lo.z), c(0, dnorm(shade.lo.z,0,1)))
      #      text(shade.lo, spacer-0.05, as.character(show.lo) )
      #      text(shade.lo, -0.11, as.character(show.lo) )
    }
  }
  
  if ( !is.na(show.hi) ) {
    if (is.logical(show.hi) ){
      if( show.hi ) {
        lines( c(shade.hi.z, shade.hi.z), c(0, dnorm(shade.hi.z,0,1)))
        #text(shade.hi.z, -0.11, as.character(shade.hi.z) )
      }
    } else {
      lines( c(shade.hi.z, shade.hi.z), c(0, dnorm(shade.hi.z,0,1)))
      #text(shade.hi.z, -0.11, as.character(show.hi) )
    }
  }
  
  # Shading
  x.poly <- seq( max(zlim.lo, shade.lo.z), 
                 min(zlim.hi, shade.hi.z), length=100)
  y.poly <- dnorm( x.poly, 0, 1)
  
  x.p <- c( x.poly, rev(x.poly) )
  y.p <- c( rep(0, length(x.poly)), rev(y.poly) )
  polygon(x.p, y.p, col=shade.col )
  
  # Rug on horizontal axis
  yrange <- max(nc) - 0
  for (i in 1:length(text.loc)) {
    lines( c(text.loc[i], text.loc[i]),
           c(0, 0.05*yrange),
           col="grey")
  } 
}








##############################################################
##############################################################
##############################################################
##############################################################


# Define the six steps to use repeatedly
SixSteps <- function( Flag=NA, Text=NA, Labs="Long", Arrows=TRUE, ...){
  # FLAG is the box flagged for adding additional text.
  # We count from the top down, so Flag=1 corresponds to Step 1: Ask
  #
  # TEXT is the extra text that it contains
  
  
  
  
  # Colours
  col.Flag <- "slategray4"
  col.Default <- "slategray1"  

  
#  blueTransparent <- rgb(0, 0, 1, alpha=0.2, maxColorValue = 1)
#  plot.colour <- blueTransparent
#  col.Default <- blueTransparent 
  
  
    
  
  BoxWidth <- 1      # Width of a box
  BoxHeight <- 0.4  # Height of a standard box (one line of text)
  BoxGap <- 0.4     # Gap *between* boxed
  
  # To use  Text  in expressions, we need to use tildes rather than spaces
  Labels.Long <- c(
    "Ask~the~question",
    "Design~the~study",
    "Collect~the~data",
    "Describe~and~summarise~the~data",
    "Analyse~the~data",
    "Report~the~results")
  
  Labels.Short <- c("Ask",
                    "Design",
                    "Collect",
                    "Describe",
                    "Analyse",
                    "Report")
  
  if (Labs=="Short") Labels <- Labels.Short
  if (Labs=="Long") Labels <-  Labels.Long
  if (Labs=="None") Labels <- rep("", 6)
  if (Labs=="Random") Labels <- Labels.Long[ sample(1:length(Labels.Long))]
  if (Labs=="ShortWhenWithText") {
    Labels <- Labels.Long
    if ( !is.na(Text) ) {
      Labels[Flag] <- Labels.Short[Flag]
    }
  }
  
  # To use  Text  in expressions, we need to replace the spaces with tildes
  library(stringr)
  #if (!is.na(Text)) Text <- str_replace_all(Text," ", "~")
  if (is.na(Flag) ) Flag <- 0
  
  
  # Some fooling about to make the main step in bold.
  if ( !(Labs=="None")){ # So if we are adding labels...
    for (j in (1:6)){    # Treat each step in the six-step process one at a time..
      Labels[j] <- paste("expression(bold(", Labels[j], "))", sep="")
    }
  }
  
  
  
  
  par( mar=c(0, 0, 0, 0) + 0.1)
  plot( c(-BoxWidth/2, BoxWidth/2),
        c(-BoxHeight/2, 7*BoxHeight + 6*BoxGap),
        type="n",
        xlab="",
        ylab=",",
        axes=FALSE)

  Box.TopCorners <- (2:7) * BoxHeight + (0:5) * BoxGap  # NOTE: Box.TopCorners[1] is the lowest. So reverse
  Box.TopCorners <- rev(Box.TopCorners)                 # NOW: Box.TopCorners[1] is the *top* box, which is Step 1
  Box.BottomCorners <- Box.TopCorners - BoxHeight

    # Now adjust if one of the boxes is two lines of text high
  if ( Flag>0 ) {# If a box is flagged...
    if (  !is.na(Text) ) {   # *AND(* there is text to put there....
      if ( Flag == 6 ){ 
        Box.TopCorners[ 6 ] <- Box.TopCorners[ 6]
      } else {
        Box.TopCorners[ (Flag+1):6] <- Box.TopCorners[ (Flag+1):6] - BoxHeight
      }
      Box.BottomCorners[ Flag:6] <- Box.BottomCorners[ Flag:6 ] - BoxHeight
    }
  }

  
  DrawBox <- function(Box.TCorner, Box.BCorner, BoxW=BoxWidth, which.box, flag=FALSE, Locations=Locations){
    
    polygon( c(-BoxW/2, BoxW/2, BoxW/2, -BoxW/2),
             c(Box.TCorner, Box.TCorner, Box.BCorner, Box.BCorner),
             col=col.Default, # All boxes same colour
             lwd = 4, # Thick borders
             border=ifelse(flag, col.Flag, NA) ) # Thick, dark borders on flagged boxes
  }
  
  DrawLine <- function(Top, Bottom){
    
    arrows(0, Top,
           0, Bottom,
           length=0.15,
           lwd=2,
           angle=15)  
  }
  
  for (i in (1:6)){
    if (Arrows) if (i > 1 ) DrawLine( Box.BottomCorners[i-1], Box.TopCorners[i])
    
    if ( i == Flag){
      DrawBox(Box.TopCorners[i], Box.BottomCorners[i], BoxWidth, 1, TRUE)
    } else{
      DrawBox(Box.TopCorners[i], Box.BottomCorners[i], BoxWidth, 1, FALSE)
    }
  }
  
  # Add main text (the names of each step) into boxes
  Text.StepsLocation <- Box.TopCorners - BoxGap/2 
  for (i in (1:length(Labels))){
    Text.StepsLocation.Thisone <- Text.StepsLocation[i]
    text.col <- grey(0.3)
    if (i == Flag ) text.col <- "black"
    text.string <- paste("text(0, Text.StepsLocation.Thisone, ", Labels[i], ", col=text.col)")
    eval( parse(text=text.string))
    #    text(0, Loc, rev(Labels)[i])
  }
  
  # Add the additional text
  Text.AddedTextLocation <- Box.TopCorners[Flag] - BoxHeight*1.33
  text(0, Text.AddedTextLocation, Text)
  
}


