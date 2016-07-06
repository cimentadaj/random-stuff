setwd("/Users/ggmhf/Desktop/Teaching/Multilevel Short Course")

set.seed(999)
a <- 0
Xb <- rep(1:5, each=5)
Xw <- rep(-2:2, 5)/5
x <- Xw + Xb
u <- rep(rnorm(5), each=5)/5
e <- rnorm(25)
y <- a + Xb - Xw + u + e

summary(lm(y~x))

pdf("BW1.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, xlim=c(-1,6), ylim=c(-1,6), pch=19)
dev.off()

pdf("BW2.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, xlim=c(-1,6), ylim=c(-1,6), pch=19)
segments(x0=min(x), y0=min(range(predict(lm(y~x)))), x1=max(x), y1=max(range(predict(lm(y~x)))), lwd=2)
dev.off()

pdf("BW3.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
dev.off()

pdf("BW4.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$y~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$y~Z$x)))), col=which(unique(Z$u)==unique(u))))
dev.off()

pdf("BW5.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$y~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$y~Z$x)))), col=which(unique(Z$u)==unique(u))))
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
dev.off()

pdf("BW5b.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
dev.off()

pdf("BW5c.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19, type="n")
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
dev.off()

pdf("BW6.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$y~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$y~Z$x)))), col=which(unique(Z$u)==unique(u))))
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
segments(x0=min(unique(Xb)), y0=min(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), x1=max(unique(Xb)), y1=max(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), lwd=2)
dev.off()

pdf("BW6b.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19, type="n")
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
segments(x0=min(unique(Xb)), y0=min(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), x1=max(unique(Xb)), y1=max(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), lwd=2)
dev.off()


pdf("BW62.pdf", height=4, width=9)
par(cex.lab=1.5)
par(cex.axis=1.5)
par(mfrow=c(1,2))
plot(x, y, xlim=c(-1,6), ylim=c(-1,6), pch=19)
segments(x0=min(x), y0=min(range(predict(lm(y~x)))), x1=max(x), y1=max(range(predict(lm(y~x)))), lwd=2)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$y~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$y~Z$x)))), col=which(unique(Z$u)==unique(u))))
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
segments(x0=min(unique(Xb)), y0=min(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), x1=max(unique(Xb)), y1=max(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), lwd=2)
dev.off()

dat <- data.frame(a, Xb, Xw, x, u, e, y)
by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))
means <- by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))
means <- data.frame(Xb=names(means), do.call(rbind, means))
names(means)[2:3] <- c("xM", "yM")
dat <- merge(dat, means)
dat$xD <- dat$x-dat$xM
dat$yD <- dat$y-dat$yM

summary(lm(y ~ x + as.factor(Xb), dat))

summary(lm(yD ~ xD, dat)) # same beta coefficient

pdf("BW7.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
abline(h=0, col="lightgrey")
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
dev.off()

pdf("BW8.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
abline(h=0, col="lightgrey")
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
by(dat, dat$Xb, function(Z) arrows(x0=min(Z$Xb), x1=min(Z$Xb), y0=min(Z$yM), y1=0, col=min(Z$Xb), lwd=2))
dev.off()

pdf("BW9.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(dat$x, dat$yD, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-3.5,3.5), pch=19, ylab="y", xlab="x")
abline(h=0, col="lightgrey")
points(do.call(rbind, by(dat, dat$Xb, function(Z) c(mean(Z$x), mean(Z$yD)))), col=unique(Xb), pch=19, cex=2)
by(dat, dat$Xb, function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$yD~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$yD~Z$x)))), col=which(unique(Z$u)==unique(u))))
dev.off()

pdf("BW10.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(dat$x, dat$yD, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-3.5,3.5), pch=19, ylab="y", xlab="x")
abline(h=0, col="lightgrey")
points(do.call(rbind, by(dat, dat$Xb, function(Z) c(mean(Z$x), mean(Z$yD)))), col=unique(Xb), pch=19, cex=2)
by(dat, dat$Xb, function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$yD~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$yD~Z$x)))), col=which(unique(Z$u)==unique(u))))
by(dat, dat$Xb, function(Z) arrows(x0=min(Z$Xb), x1=0, y0=(3-min(Z$Xb))/5, y1=(3-min(Z$Xb))/5, col=min(Z$Xb), lwd=2))
dev.off()

pdf("BW11.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(dat$xD, dat$yD, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-3.5,3.5), pch=19, ylab="y", xlab="x")
abline(h=0, col="lightgrey")
by(dat, dat$Xb, function(Z) segments(x0=min(Z$x)-min(Z$Xb), y0=max(range(predict(lm(Z$yD~Z$x)))), x1=max(Z$x)-min(Z$Xb), y1=min(range(predict(lm(Z$yD~Z$x)))), col=which(unique(Z$u)==unique(u))))
points(do.call(rbind, by(dat, dat$Xb, function(Z) c(mean(Z$xD), mean(Z$yD)))), pch=19, cex=2)
dev.off()

pdf("BW12.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(dat$xD, dat$yD, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-3.5,3.5), pch=19, ylab="y", xlab="x")
abline(h=0, col="lightgrey")
by(dat, dat$Xb, function(Z) segments(x0=min(Z$x)-min(Z$Xb), y0=max(range(predict(lm(Z$yD~Z$x)))), x1=max(Z$x)-min(Z$Xb), y1=min(range(predict(lm(Z$yD~Z$x)))), col=which(unique(Z$u)==unique(u))))
points(do.call(rbind, by(dat, dat$Xb, function(Z) c(mean(Z$xD), mean(Z$yD)))), pch=19, cex=2)
abline(a=0, b=-1.3614, lwd=2)
dev.off()

pdf("BW13.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(dat$xD, dat$yD, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-3.5,3.5), pch=19, ylab="y", xlab="x")
abline(h=0, col="lightgrey")
points(do.call(rbind, by(dat, dat$Xb, function(Z) c(mean(Z$xD), mean(Z$yD)))), pch=19, cex=2)
abline(a=0, b=-1.3614, lwd=2)
dev.off()

pdf("BW132.pdf", height=4, width=9)
par(cex.lab=1.5)
par(cex.axis=1.5)
par(mfrow=c(1,2))
plot(dat$xD, dat$yD, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-3.5,3.5), pch=19, ylab="y", xlab="x")
abline(h=0, col="lightgrey")
points(do.call(rbind, by(dat, dat$Xb, function(Z) c(mean(Z$xD), mean(Z$yD)))), pch=19, cex=2)
abline(a=0, b=-1.3614, lwd=2)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
abline(h=0, col="lightgrey")
by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$y~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$y~Z$x)))), col=which(unique(Z$u)==unique(u))))
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
segments(x0=min(unique(Xb)), y0=min(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), x1=max(unique(Xb)), y1=max(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), lwd=2)
dev.off()

pdf("BW14.pdf", height=4, width=9)
par(cex.lab=1.5)
par(cex.axis=1.5)
par(mfrow=c(1,2))
plot(dat$xD, dat$yD, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-3.5,3.5), pch=19, ylab="y", xlab="x")
abline(h=0, col="lightgrey")
points(do.call(rbind, by(dat, dat$Xb, function(Z) c(mean(Z$xD), mean(Z$yD)))), pch=19, cex=2)
abline(a=0, b=-1.3614, lwd=2)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
abline(h=0, col="lightgrey")
by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$y~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$y~Z$x)))), col=which(unique(Z$u)==unique(u))))
points(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) c(mean(Z$x), mean(Z$y)))), col=unique(Xb), pch=19, cex=2)
segments(x0=min(unique(Xb)), y0=min(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), x1=max(unique(Xb)), y1=max(predict(lm(as.numeric(by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) mean(Z$y)))~unique(Xb)))), lwd=2)
abline(a=(3+1.3614*3), b=-1.3614, lwd=2)
dev.off()



# Trento Lecture 3

lm(yD~xD, dat)$coefficients 
lm(y~x + as.factor(Xb), dat)$coefficients # coefficient on x matches...
by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) lm(Z$y~Z$x)$coefficients)
colMeans(do.call(rbind, by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) lm(Z$y~Z$x)$coefficients))) # MEAN coefficient matches

pdf("BW15.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
by(dat, dat$Xb, function(Z) segments(x0=min(Z$x), y0=mean(Z$y)+max(range(predict(lm(dat$yD~dat$xD)))), x1=max(Z$x), y1=mean(Z$y)+min(range(predict(lm(dat$yD~dat$xD)))), col=which(unique(Z$u)==unique(u))))
dev.off()

pdf("BW16.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
by(dat, dat$Xb, function(Z) segments(x0=min(Z$x), y0=mean(Z$y)+max(range(predict(lm(dat$yD~dat$xD)))), x1=max(Z$x), y1=mean(Z$y)+min(range(predict(lm(dat$yD~dat$xD)))), col=which(unique(Z$u)==unique(u))))
by(data.frame(a, Xb, Xw, x, u, e, y), Xb, function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$y~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$y~Z$x)))), col=which(unique(Z$u)==unique(u))))
dev.off()

pdf("BW17.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
plot(x, y, col=rep(1:5, each=5), xlim=c(-1,6), ylim=c(-1,6), pch=19)
by(dat, dat$Xb, function(Z) abline(mean(Z$y)-lm(Z$yD~Z$xD)$coefficients[2]*Z$xM, lm(Z$yD~Z$xD)$coefficients[2], col=which(unique(Z$u)==unique(u))))
by(dat, dat$Xb, function(Z) abline(mean(Z$y)-lm(dat$yD~dat$xD)$coefficients[2]*Z$xM, lm(dat$yD~dat$xD)$coefficients[2], col=which(unique(Z$u)==unique(u))))
dev.off()


####### here we start a new series, illustrating random intercepts

set.seed(999)
u <- rep(rnorm(5), each=5)/5
e <- rnorm(25)
set.seed(123)
Xb <- rep(rep(1:5, each=5), 4)
Xw <- rep(rep(-2:2, 5)/5, 4)
x <- Xw + Xb
u <- c(u, rep(rnorm(15), each=5)/5)
e <- c(e, rnorm(75))
y <- a + Xb - Xw + u + e
grp <- rep(1:20, each=5)
dat <- data.frame(a, Xb, Xw, x, u, e, y, grp)
means <- by(dat, dat$grp, function(Z) c(mean(Z$x), mean(Z$y)))
means <- data.frame(grp=names(means), do.call(rbind, means))
names(means)[2:3] <- c("xM", "yM")
dat <- merge(dat, means)
dat$xD <- dat$x-dat$xM
dat$yD <- dat$y-dat$yM

palette(c(palette()[1:5], rainbow(15)))

pdf("BW18.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
dev.off()

pdf("BW19.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat, dat$grp, function(Z) abline(lm(y~x, Z), col=Z$grp))
dev.off()

mods <- do.call(rbind, by(dat, dat$grp, function(Z) lm(y ~ x, Z)$coefficients))
mods <- data.frame(mods, matrix(t(do.call(rbind, by(dat, dat$grp, function(Z) confint(lm(y ~ x, Z))))), ncol=4, byrow=T))
names(mods) <- c("Intercept.est", "Slope.est", "Intercept.lo", "Intercept.hi", "Slope.lo", "Slope.hi")
mods$col <- c(palette()[1:5], rainbow(15))
mods <- mods[order(mods$Intercept.est),]

pdf("BW20.pdf")
par(cex.lab=1.25)
par(cex.axis=1.25)
par(mfrow=c(1,2))
plot(mods$Intercept.est, 1:20, pch=19, type="n", xlim=c(min(mods$Intercept.est)-10, max(mods$Intercept.est)+10), ylab="Group", xlab="Intercept", axes=F)
box("plot")
axis(1)
abline(h=seq(20), col="lightgray", lwd=0.5)
abline(v=seq(-20, 30, 10), col="lightgray", lwd=0.5)
segments(y0=1:20, y1=1:20, x0=mods$Intercept.lo, x1=mods$Intercept.hi, lwd=2, col=mods$col)
points(mods$Intercept.est, 1:20, pch=19, col=mods$col)
plot(mods$Slope.est, 1:20, pch=19, type="n", xlim=c(min(mods$Slope.est)-2, max(mods$Slope.est)+2), ylab="", xlab="Slope", axes=F)
box("plot")
axis(1)
abline(h=seq(20), col="lightgray", lwd=0.5)
abline(v=seq(-6, 6, 2), col="lightgray", lwd=0.5)
segments(y0=1:20, y1=1:20, x0=mods$Slope.lo, x1=mods$Slope.hi, lwd=2, col=mods$col)
points(mods$Slope.est, 1:20, pch=19, col=mods$col)
dev.off()

pdf("BW21.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
abline(a=(3-with(dat, lm(y ~ x + as.factor(grp)))$coefficients[2]*3), b=with(dat, lm(y ~ x + as.factor(grp)))$coefficients[2], lwd=5)
dev.off()

pdf("BW22.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat, dat$grp, function(Z) abline(lm(y~x, Z), col=Z$grp))
abline(a=(3-with(dat, lm(y ~ x + as.factor(grp)))$coefficients[2]*3), b=with(dat, lm(y ~ x + as.factor(grp)))$coefficients[2], lwd=5)
dev.off()

dat$yMLM <- predict(lmer(y ~ xD + (1 | grp), dat))

pdf("BW23.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat, dat$grp, function(Z) abline(lm(yMLM ~ x, Z), col=Z$grp))
dev.off()

pdf("BW24.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat[dat$grp<6,], plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat[dat$grp<6,], dat$grp[dat$grp<6], function(Z) abline(lm(yMLM ~ x, Z), col=Z$grp))
dev.off()

pdf("BW25.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat[dat$grp<6,], plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat[dat$grp<6,], dat$grp[dat$grp<6], function(Z) abline(lm(yMLM ~ x, Z), col=Z$grp))
by(dat[dat$grp<6,], dat$grp[dat$grp<6], function(Z) segments(x0=min(Z$x), y0=max(range(predict(lm(Z$y~Z$x)))), x1=max(Z$x), y1=min(range(predict(lm(Z$y~Z$x)))), col=which(unique(Z$u)==unique(u))))
dev.off()

pdf("BW26.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat[dat$grp<6,], plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat[dat$grp<6,], dat$grp[dat$grp<6], function(Z) abline(lm(yMLM ~ x, Z), col=Z$grp))
by(dat[dat$grp<6,], dat$grp[dat$grp<6], function(Z) segments(x0=min(Z$x), y0=mean(Z$y)+max(range(predict(lm(dat$yD~dat$xD)))), x1=max(Z$x), y1=mean(Z$y)+min(range(predict(lm(dat$yD~dat$xD)))), col=which(unique(Z$u)==unique(u))))
dev.off()

pdf("BW27.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat, dat$grp, function(Z) abline(lm(yMLM ~ x, Z), col=Z$grp))
abline(c(fixef(lmer(y ~ xD + (1 | grp), dat))[1]-3*fixef(lmer(y ~ xD + (1 | grp), dat))[2], fixef(lmer(y ~ xD + (1 | grp), dat))[2]), lwd=5)
dev.off()

pdf("BW28.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat, dat$grp, function(Z) abline(lm(yMLM ~ x, Z), col=Z$grp))
by(dat, dat$grp, function(Z) points(mean(Z$x), mean(Z$y), col=Z$grp, pch=19, cex=2))
abline(c(fixef(lmer(y ~ xD + (1 | grp), dat))[1]-3*fixef(lmer(y ~ xD + (1 | grp), dat))[2], fixef(lmer(y ~ xD + (1 | grp), dat))[2]), lwd=5)
abline(fixef(lmer(y ~ xM + (1 | grp), dat)), lwd=5)
dev.off()

###### now show random slopes too

library(MASS)

a <- 0
set.seed(999)
u <- rep(rnorm(5), each=5)/5
e <- rnorm(25)
set.seed(123)
Xb <- rep(rep(1:5, each=5), 4)
Xw <- rep(rep(-2:2, 5)/5, 4)
x <- Xw + Xb
u <- c(u, rep(rnorm(15), each=5)/5)
e <- c(e, rnorm(75))
y <- a + Xb - Xw + u + e
grp <- rep(1:20, each=5)
dat <- data.frame(a, Xb, Xw, x, u, e, y, grp)
means <- by(dat, dat$grp, function(Z) c(mean(Z$x), mean(Z$y)))
means <- data.frame(grp=names(means), do.call(rbind, means))
names(means)[2:3] <- c("xM", "yM")
dat <- merge(dat, means)
dat$xD <- dat$x-dat$xM
dat$yD <- dat$y-dat$yM
set.seed(123)
U0U1 <- mvrnorm(n=20, mu=c(0,0), Sigma=matrix(c(0.2^2, 0.01, 0.01, 0.2^2), ncol=2)) # 0.5 correlation
dat <- data.frame(dat, U0U1[rep(1:nrow(U0U1), each=5),])
names(dat)[13:14] <- c("U0", "U1")
dat <- within(dat, y <- y - u + U0 + U1*Xw)

dat$yMLM  <- predict(lmer(y ~ xD + (1 | grp), dat))
dat$yMLMs <- predict(lmer(y ~ xD + (xD | grp), dat))

palette(c(palette()[1:5], rainbow(15)))

pdf("BW29a.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
dev.off()

pdf("BW29.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat, dat$grp, function(Z) abline(lm(yMLMs ~ x, Z), col=Z$grp))
dev.off()

pdf("BW30.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat[dat$grp<6,], plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat[dat$grp<6,], dat$grp[dat$grp<6], function(Z) abline(lm(yMLMs ~ x, Z), col=Z$grp))
dev.off()

pdf("BW31.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat, dat$grp, function(Z) abline(lm(y~x, Z), col=Z$grp))
dev.off()

pdf("BW32.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat, dat$grp, function(Z) abline(lm(yMLM ~ x, Z), col=Z$grp))
dev.off()

pdf("BW33.pdf")
par(cex.lab=1.5)
par(cex.axis=1.5)
with(dat, plot(x, y, col=grp, xlim=c(-1,6), ylim=c(-1,6), pch=19))
by(dat, dat$grp, function(Z) points(mean(Z$x), mean(Z$y), col=Z$grp, pch=19, cex=2))
by(dat, dat$grp, function(Z) abline(lm(yMLMs ~ x, Z), col=Z$grp))
abline(fixef(lmer(y ~ xM + (1 | grp), dat)), lwd=5)
abline(c(fixef(lmer(y ~ xD + (1 | grp), dat))[1]-3*fixef(lmer(y ~ xD + (1 | grp), dat))[2], fixef(lmer(y ~ xD + (1 | grp), dat))[2]), lwd=5)
dev.off()



# fixed effects not only "zero out" the differences in the means of x and y, but also "zero out" the slopes...

# show next:
#	random intercept models
#	random slope models
#	shrinkage of random intercepts
#	shrinkage of random slopes
#	look at SEs (how they change with random slopes)
#	look at variances (think about number of parameters, and the variances)

# show pictures to illustrate... overall shrinkage (use Produc data?)

-> fit models with lmer, and MCMCglmm

# use the Produc data to show shrinkage

# then use EVS data to show what all this gets you (use GODIMP)
# add slides on the application... why AUTH, GODIMP?
# use MCMCglmm
# work with a binary outcome, and apply the binomial trick

