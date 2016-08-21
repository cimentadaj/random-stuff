## ggplot2 tutorial

## This tutorial consists of reproducing a graph by The Economist.
## The tutorial is originally from a class at Harvard and can be accessed here
# http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

library(ggplot2)
library(downloader)

dir.create(paste0(getwd(),"/ggplot2tutorial"))
setwd(paste0(getwd(),"/ggplot2tutorial"))
download("http://tutorials.iq.harvard.edu/R/Rgraphics.zip", destfile="Rgraphics.zip", mode="wb")
unzip("Rgraphics.zip")

list.files()
list.files(paste0(getwd(),"/Rgraphics"))


## Let's start. ggplot2 is a great package for producing high quality graphs. But more importantly, ggplot2 offers
## consistency in the structure of its syntax by using something called the grammar of graphics.
# Soon you'll learn what this is.

## Some things ggplot2 can not do:

# 3-dimensional graphics (see the rgl package)
# Graph-theory type graphs (nodes/edges layout; see the igraph package)
# Interactive graphics (see the ggvis package)

housing <- read.csv("./Rgraphics/dataSets/landdata-states.csv")
head(housing[1:5])

## The grammar of graphics is a system that by combining a number of basic pilars allows you to produce any graph.
## The basic building blocks of ggplot2 are

# data = The dataset containing the information
# aesthetic mapping = the variables that will be used(x,y,z(fill))
# geometric object = encompasses the objects used to define the type of graph: scatterplot, histogram, barplot
# statistical transformations
# scales = scales manages the units of the aesthetics. It transforms things like the scale of the X and Y axis.
# coordinate system
# position adjustments
# faceting = this block allows to disaggregate the full graph to subsets of the data


## Every geom has a default stat transformation and every stat transformation has a default geom.

ggplot(housing, aes(x = Home.Value, y= Structure.Cost, color=region)) + geom_smooth()

# is the same as: 

ggplot(housing, aes(x = Home.Value, y= Structure.Cost, color=region)) + stat_smooth()

# When I say both are the same they are actually not. It simply means that each have each other as defaults if the other is not specified.

# Histograms

ggplot(housing, aes(Home.Value)) + geom_histogram() # The option bins allows you to specify the number of bins in the plot
ggplot(housing, aes(Home.Value)) + geom_histogram(bins=100)
# There is also the option of using binwidth which specifies, as the name conveys, the width of each bin.

ggplot(housing, aes(Home.Value)) + geom_histogram(binwidth=1000)
# Please note that binwidth overrides bins
ggplot(housing, aes(Home.Value)) + geom_histogram(binwidth=1000, bins=30) ## Will give the same plot

# Barplots

ggplot(housing, aes(region)) + geom_bar() # Will give you the counts for each region(better than table)
ggplot(housing, aes(region,y=..count../sum(..count..))) + geom_bar() ## Will give the percentage of each category
ggplot(housing, aes(region)) + geom_bar(stat="identity") # Will give the mean for each group
## Ops! Of course, we need to specify a variable to calculate the mean for
ggplot(housing, aes(region, Home.Value)) + geom_bar(stat="identity")

## If you input a continuous variable in the X axis the output will automatically be a histogram

## You can change the fill color with the fill option
ggplot(housing, aes(region, Home.Value)) + geom_bar(stat="identity",fill="lightblue")

# You can also use another variable to fill the bars
ggplot(housing, aes(reorder(State, -Home.Value), Home.Value, fill=region)) + geom_bar(stat="identity")

# Random subset to exemplify
housing2 <- subset(housing, region %in% c("Midwest","South") & State %in% c("AL","AR","KS","WV"))

# While using the fill option with a categorical variable, you'll obtain a stacked barplot. Using position="dodge", you can put it side by side
ggplot(housing, aes(region, Home.Value, fill=State)) +
    geom_bar(stat="identity")

ggplot(housing2, aes(region, Home.Value, fill=State)) +
    geom_bar(stat="identity", position="dodge")

# To change the colors manually use the scale_fill_manual() option.
ggplot(housing2, aes(region, Home.Value, fill=State)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = c('#669933', '#FFCC66', "235543","#231543"))

# Finally, you can color the outlines of each bar with the color option in geom_bar()
ggplot(housing2, aes(region, Home.Value, fill=State)) +
    geom_bar(stat="identity", position="dodge", color="black") +
    scale_fill_manual(values = c('#669933', '#FFCC66', "235543","#231543"))

# Options in geom_bar()
# Setting the width between bars with width:
ggplot(housing2, aes(region, Home.Value, fill=State)) +
    geom_bar(stat="identity", position="dodge", color="black", width = 0.2)

# Changing width for bars within a group
ggplot(housing2, aes(region, Home.Value, fill=State)) +
    geom_bar(stat="identity", position=position_dodge(width=0.5), color="black", width = 0.2)

##       The Economist graph         ##

dat <- read.csv("./Rgraphics/dataSets/EconomistData.csv")

# 1st exercises
# Create a scatter plot with CPI on the x axis and HDI on the y axis.
# Color the points in the previous plot blue.
# Color the points in the previous plot according to Region.
# Create boxplots of CPI by Region
# Overlay points on top of the box plots

# 1.
(p1 <- ggplot(dat, aes(CPI, HDI)) + geom_point())
# 2.
(p1 + geom_point(colour="blue"))
# 3.
(p1 + geom_point(aes(colour=Region)))
# 4.
(p2 <- ggplot(dat, aes(Region, CPI)) + geom_boxplot())
# 5.
(p2 + geom_point())

# 2nd exercises
# Re-create a scatter plot with CPI on the x axis and HDI on the y axis (as you did in the previous exercise).
# Overlay a smoothing line on top of the scatter plot using the lm method. Hint: see ?stat_smooth.
# Overlay a smoothing line on top of the scatter plot using the default method.
# BONUS (optional): Overlay a smoothing line on top of the scatter plot using the default loess method, but make it less smooth. Hint: see ?loess.

# 1.
(p1 <- ggplot(dat, aes(CPI, HDI)) + geom_point())
# 2.
(p1 + geom_smooth(method = "lm"))
# 3.
(p1 + geom_smooth(method = "lm") + geom_smooth())
# 4.
(p1 + geom_smooth(span=0.2))


###
p3 <- ggplot(housing, aes(x = reorder(State, -Home.Price.Index), y = Home.Price.Index)) + 
    theme(legend.position="top", axis.text=element_text(size = 6))

(p4 <- p3 + geom_point(aes(color = Date),
                       alpha = 0.5, # to control for the transparency of the dots
                       size = 1.5, # set the size of the dot
                       position = position_jitter(width = 0.25, height = 0))) # set the jitter of the dot

p4 + scale_x_discrete(name="State Abbreviation") + # set the name of the x scale (really you use this to name the categories in the discrete outcome)
    scale_color_continuous(name="",
                           breaks = c(19751, 19941, 20131),
                           labels = c(1971, 1994, 2013))

p4 + scale_x_discrete(name="State Abbreviation") +
     scale_color_continuous(name="",
                           breaks = c(19751, 19941, 20131),
                           labels = c(1971, 1994, 2013),
                           low = "blue", high = "red")


## Each geom has its own scale. For example, you might specify a color, shape or whatever but you can't
# specify which colors or shapes you want. For that you use the scale function.

# Here we identified points with a certain color
ggplot(housing, aes(Home.Value, Land.Value, color=region)) + geom_point(alpha=0.5)
# Let's change the colors and rewrite the name of the legend
ggplot(housing, aes(Home.Value, Land.Value, color=region)) + geom_point(alpha=0.5) + scale_colour_manual(name="Region",
                                                                                    values=c("blue","green","red","brown"))
# Alternatively, you can provide a named vector with each category assigned to a color.

# Let's change the labels as well:
ggplot(housing, aes(Home.Value, Land.Value, color=region)) + geom_point(alpha=0.5) + scale_colour_manual(name="Region",
                                                                                    values=c("blue","green","red","brown"),
                                                                                    labels=c("Mid west","North East","South","West"))

# Each of these options has their own scale:
# position
# color and fill
# size
# shape
# line type

# Remember the Rstudio cheat sheet. It contains each scale with a detailed description for each geom and scale function.
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

# Exercise Three

# 1. Create a scatter plot with CPI on the x axis and HDI on the y axis. Color the points to indicate region.

# 2. Modify the x, y, and color scales so that they have more easily-understood names (e.g., spell out "Human development Index"
# instead of "HDI").

# 3. Modify the color scale to use specific values of your choosing. Hint: see ?scale_color_manual.

# 1.
p1 <- ggplot(dat, aes(CPI, HDI, color=Region)) + geom_point()
# 2.
p1 <- p1 + scale_y_continuous(name="Human Development Index") + scale_x_continuous(name="CPI Index") + scale_color_discrete(name="Regions")
# 3.
p1 <- p1 + scale_color_manual(name="Regions of the world",values=sample(colors(),6))

# Facets

# Facets are used to disaggregate one plot with many groupings into several plots

ggplot(housing, aes(Date, Home.Value, color=State)) + geom_line() # This plots is not easy to understand

p2 <- ggplot(housing, aes(Date, Home.Value)) + geom_line() + facet_wrap(~State) # this is better
ggplot(housing, aes(Date, Home.Value)) + geom_line() + facet_grid(. ~ State) # you can also use facet_grid; check the cheat sheet for
# how it works.

# Themes

# You can adjust and change the background color, legend position and many other things with the theme option.
p2 + theme_linedraw()

# Let's reproduce this graph: http://www.economist.com/node/21541178

# [ ] add a trend line
# [ ] change the point shape to open circle
# [ ] change the order and labels of Region
# [ ] label select points
# [ ] fix up the tick marks and labels
# [ ] move color legend to the top
# [ ] title, label axes, remove legend title
# [ ] theme the graph with no vertical guides
# [ ] add model R2 (hard)
# [ ] add sources note (hard)
# [ ] final touches to make it perfect (use image editor for this)

dat <- read.csv("./Rgraphics/dataSets/EconomistData.csv")
p1 <- ggplot(dat, aes(x = CPI,y = HDI, color=Region))
(p2 <- p1 + geom_smooth(
                       method="lm",
                       se=FALSE,
                       formula = y ~ log(x),
                       color="red")) +
                       geom_point()
# Note that here the only object being save is ONLY the smooth line (look at the parenthesis)
p3 <- p2 + geom_point(shape = 1, size=4) # Here I can add the points(inhereted by the aes()) and set a shape

# Make lines of the points bigger
p4 <- p3 +
    geom_point(size = 4.5, shape = 1) +
    geom_point(size = 4, shape = 1) +
    geom_point(size = 3.5, shape = 1)

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

library(ggrepel)
p5 <- p4 + geom_text_repel(aes(label = Country),
                    color = "gray20",
                    data = subset(dat, Country %in% pointsToLabel),
                    force = 10)

dat$Region <- factor(dat$Region,
                     levels = c("EU W. Europe",
                                "Americas",
                                "Asia Pacific",
                                "East EU Cemt Asia",
                                "MENA",
                                "SSA"),
                     labels = c("OECD",
                                "Americas",
                                "Asia &\nOceania",
                                "Central &\nEastern Europe",
                                "Middle East &\nnorth Africa",
                                "Sub-Saharan\nAfrica"))

# Let's update the data
p5$data <- dat
p5

library(grid)
p5 <- p5 +
    scale_x_continuous("Corruption Perception Index, 2011 (10=least corrupt)", breaks = seq(1,10,by=1), limits=c(.9,10.5)) +
    scale_y_continuous("Human Development Index, 2011(1=Best)", breaks=seq(0.2,1,0.1), limits=c(0.2,1)) +
    scale_color_manual(name = "", values = c("#24576D",
                                  "#099DD7",
                                  "#28AADC",
                                  "#248E84",
                                  "#F2583F",
                                  "#96503F")) +
    ggtitle("Corruption and Human Development")

(p6 <- p5 +
     # start with a minimal theme and add what we need
    theme(text = element_text(color = "gray20"),
          panel.background=element_rect(fill="white"),
          legend.position = c("top"), # position the legend in the upper left 
          legend.direction = "horizontal",
          legend.justification = 0.1, # anchor point for legend.position.
          legend.text = element_text(size = 11, color = "gray10"),
          axis.text = element_text(face = "italic"),
          axis.title.x = element_text(vjust = -1), # move title away from axis
          axis.title.y = element_text(vjust = 2), # move away for axis
          axis.ticks.y = element_blank(), # element_blank() is how we remove elements
          axis.line = element_line(color = "gray40", size = 0.5),
          axis.line.y = element_blank(),
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank()
    ))
