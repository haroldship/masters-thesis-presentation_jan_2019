library(lattice)
library(latticeExtra)
library(dkd)

n <- 100

set.seed(1)

Xc <- c(rnorm(n/2), rnorm(n/2, mean=3))
Yc <- c(rnorm(n/2), rnorm(n/2, mean=3))

pts <- data.frame(X=Xc,Y=Yc)

Xd <- seq(-2, 5, 0.1)
Yd <- seq(-2, 5, 0.1)
pts.c <- expand.grid(X=Xd, Y=Yd)
Dx <- dnorm(pts.c$X) + dnorm(pts.c$X, mean=3)
Dy <- dnorm(pts.c$Y) + dnorm(pts.c$Y, mean=3)
pts.c$Z <- sqrt(Dx^2 + Dy^2)
d1 <- sqrt(pts.c$X^2 + pts.c$Y^2)
d2 <- sqrt((pts.c$X-3)^2 + (pts.c$Y-3)^2)
D1 <- dnorm(d1)
D2 <- dnorm(d2)
pts.c$Z <- sqrt(D1^2 + D2^2)

fhatU <- gen.kde(pts, bandwidth=0.3, kernel="gaussian")
fhatM <- gen.kde(pts, bandwidth=1, kernel="gaussian")
fhatO <- gen.kde(pts, bandwidth=5, kernel="gaussian")

DF <- expand.grid(X=seq(-2,5,0.1), Y=seq(-2,5,0.1))
DF$ZU <- fhatU(DF$X,DF$Y)
DF$ZM <- fhatM(DF$X,DF$Y)
DF$ZO <- fhatO(DF$X,DF$Y)

pdf(file="img/bandwidth-undersmooth.pdf")
under.l <- levelplot(ZU ~ X*Y, data=DF, col.regions=terrain.colors(100), xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
under.c <- contourplot(Z ~ X*Y, data=pts.c, cuts=10, labels=FALSE, lty=5, xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
under <- (under.l + under.c)
print(under)
dev.off()

pdf(file="img/bandwidth-good.pdf")
good.l <- levelplot(ZM ~ X*Y, data=DF, col.regions=terrain.colors(100), xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
good.c <- contourplot(Z ~ X*Y, data=pts.c, cuts=10, labels=FALSE, lty=5, xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
good <- (good.l + good.c)
print(good)
dev.off()

pdf(file="img/bandwidth-oversmooth.pdf")
over.l <- levelplot(ZO ~ X*Y, data=DF, col.regions=terrain.colors(100), xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
over.c <- contourplot(Z ~ X*Y, data=pts.c, cuts=10, labels=FALSE, lty=5, xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
over <- (over.l + over.c)
print(over)
dev.off()

pdf(file="img/bandwidth-points.pdf")
scatter.p <- xyplot(Y ~ X, data=pts, pch=8, xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
scatter.c <- contourplot(Z ~ X*Y, data=pts.c, cuts=10, labels=FALSE, lty=5, xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
scatter <- (scatter.p + scatter.c)
print(scatter)
dev.off()

pdf(file="img/example-incidents.pdf")
scatter <- xyplot(Y ~ X, data=pts, pch=8, xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
print(scatter)
dev.off()

pdf(file="img/example-incidents-smoothed.pdf")
good <- levelplot(ZM ~ X*Y, data=DF, col.regions=terrain.colors(100), xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
print(good)
dev.off()

pdf(file="img/example-incidents-undersmoothed.pdf")
good <- levelplot(ZU ~ X*Y, data=DF, col.regions=terrain.colors(100), xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
print(good)
dev.off()

pdf(file="img/example-incidents-oversmoothed.pdf")
good <- levelplot(ZO ~ X*Y, data=DF, col.regions=terrain.colors(100), xlim=c(-1.5,4.5), ylim=c(-1.5,4.5))
print(good)
dev.off()
