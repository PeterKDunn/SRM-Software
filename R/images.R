### Normal: Generic IQ

z <- seq(-4, 4, length=100)
fz <- dnorm( z )


dev.new(width=6, height=3, noRStudioGD = TRUE)

par(mar=c(4, 2, 2, 1))
plot( fz ~ z,
      type="l",
      lwd=3,
      axes=FALSE,
      col="blue",
      xlab="IQ score",
      ylab="")

abline(h=0)

abline(v=seq(-3, 3, by=1),
       lwd=2,
       col="grey")

dev.print(png, 
          filename="IQ-Normal.png",
          units="in",
          res=72,
          width=6,
          height=3)




### Normal: IQ, with IQ labelled at 130

z <- seq(-4, 4, length=100)
fz <- dnorm( z )


dev.new(width=6, height=3, noRStudioGD = TRUE)

par(mar=c(4, 2, 2, 1))
plot( fz ~ z,
      type="l",
      lwd=3,
      axes=FALSE,
      col="blue",
      xlab="IQ score",
      ylab="")

abline(h=0)

axis(side=1,
     at=seq(-3, 3, by=1),
     labels=c(55, 70, 85, 100, 115, 130, 145))


xx <- seq(2, 4, length=100)
fxx <- dnorm( xx )
polygon( x = c(2, xx, rev(xx) ),
         y = c(0, fxx, rep(0, length(xx)) ),
         col="wheat")

xx <- seq(-4, 2, length=100)
fxx <- dnorm( xx )
polygon( x = c(-4, xx, rev(xx) ),
         y = c(0, fxx, rep(0, length(xx)) ),
         col="wheat3")

abline(v=2,
       lwd=2,
       col="grey")


dev.print(png, 
          filename="IQ-Normal-130.png",
          units="in",
          res=72,
          width=6,
          height=3)



### Normal: IQ, with IQ at 85

z <- seq(-4, 4, length=100)
fz <- dnorm( z )


dev.new(width=6, height=3, noRStudioGD = TRUE)

par(mar=c(4, 2, 2, 1))

plot( fz ~ z,
      type="l",
      lwd=3,
      axes=FALSE,
      col="blue",
      xlab="IQ score",
      ylab="")

abline(h=0)

axis(side=1,
     at=seq(-3, 3, by=1),
     labels=c(55, 70, 85, 100, 115, 130, 145))


xx <- seq(-1, 4, length=100)
fxx <- dnorm( xx )
polygon( x = c(-1, xx, rev(xx) ),
         y = c(0, fxx, rep(0, length(xx)) ),
         col="wheat")

xx <- seq(-4, -1, length=100)
fxx <- dnorm( xx )
polygon( x = c(-4, xx, rev(xx) ),
         y = c(0, fxx, rep(0, length(xx)) ),
         col="wheat3")

abline(v=-1,
       lwd=2,
       col="grey")


dev.print(png, 
          filename="IQ-Normal-85.png",
          units="in",
          res=72,
          width=6,
          height=3)

