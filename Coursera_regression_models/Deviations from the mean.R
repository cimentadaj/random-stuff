library(UsingR); data(galton); library(reshape); long <- melt(galton)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour="black", binwidth = 1)
g <- g + facet_grid(. ~variable)
g

## With this graph you can actually see the MARGINAL distribution of height for parents and childs
## Marginal in this case simply means separate distribution.


library(manipulate)
myHist<-function(mu){
    hist(galton$child,col="blue",breaks=100)
    lines(c(mu,mu),c(0,150),col="red",lwd=5)
    mse<-mean((galton$child-mu)^2)
    text(63,150,paste("mu=",mu))
    text(63,140,paste("MSE=",round(mse,2)))
}
manipulate(myHist(mu),mu=slider(62,74,step=0.5))

## Here you can see the frequency of childrens height. Mu is the overall mean
## and the MSE is the sum of the difference of each point from the overla mean
## squared. So the bigger the MSE, the bigger the distance of the overall points
## from the mean


## Now les take it to regression

myPlot<-function(beta){
    y<-galton$child-mean(galton$child) # subtract each point from its means
    x<-galton$parent-mean(galton$parent) # subtract each point from its means
    freqData<-as.data.frame(table(x,y)) # generate a dataframe que a frequency column
    names(freqData)<-c("child","parent","freq")
    plot( # a simply scatterplot of both variables
        as.numeric(as.vector(freqData$parent)),
        as.numeric(as.vector(freqData$child)),
        pch=21,col="black",bg="lightblue",
        cex=.15*freqData$freq,
        xlab="parent",
        ylab="child"
    )
    abline(0,beta,lwd=3)
    points(0,0,cex=2,pch=19)
    mse<-mean((y-x*beta)^2) # calculate the MSE by subtracting each point from 
                            # the fit line. Because you don't have the fit line,
                            # you multiply your chosen beta by X, so the MSE
                            # will change according to your chosen beta.
    title(paste("beta=",beta,"mse=",round(mse,3)))
}
## After you plot this, you will get a scatter plot with all the data points.
## Also you will get a regression line that will vary according to your beta.

## Remember what the MSE is. Mean Squared Errors is simply the difference of each data points from a summarizing line
## Naturally, if it's a high number it means that the difference between each data point and the summarizing line is big.
## Which is why MSE is main criteria to see if a line actually fits the data better. If you have two lines, the one with 
## the lowest MSE will always explain the data better.

manipulate(myPlot(beta),beta=slider(0.01,1.2,step=0.02))
## You'll get the plot with a beta slider. Note that the beta argument is passed
## to the slider. 

## This is a great example on which you can see which is the best slope. If you play around, you'll see that there is a slope
## that captures the LEAST MSE. That is the slope of the line.
