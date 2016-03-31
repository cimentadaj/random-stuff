## On this script I take the SERCE dataset collected by the UNESCO and compute the mean scores in mathematics
## for most Latin American countries. I explore the differences between the top and bottom performers 
## and the difference between private and public schools.

library(foreign)
library(car)
library(survey)
library(ggplot2)
library(dplyr)
library(tidyr)

## download the zip file containing the data and saving as serce.zip in your working directory
download.file("http://www.unesco.org/new/fileadmin/MULTIMEDIA/FIELD/Santiago/zip/bcf362e6.zip", destfile = "serce.zip")

## unziping the data the folders
unzip("serce.zip")
####################################################################################################################################

## Now you have a folder name SERCE in your working directory. This folder has 3 compressed files, one .rar, one .zip and a document
## We want the .rar file. Through R you can extract the .rar file automatically, however, you need to tell R which program
## you use to extract .rar files. Since that may vary between users who run this script or, similar to me, you don't even know which program
## you use to extract files, I suggest we do this step manually. Simply go to your working directory(getwd())  go into the SERCE folder and extract
## the Bases_paises_SERCE.rar.  Now you should have a new folder names Bases países.

## Now go into Bases países and then into Aprendizaje. Now you have several more .rar files. We are interested in third graders, so
## extract the m3.rar file. You should have a new file in the Aprendizaje folder named m3.sav. Let's read it and begin with the analysis.

## :)


##################################################################################################################################

## Read the data using the haven package for SPSS datasets
matematica3 <- read.spss(paste0(getwd(),"/SERCE/Bases países/Aprendizaje/m3.sav"),to.data.frame = T)

## The data contains these countries but the names are written incorrectly. I rewrote them
levels(matematica3$pais_num)[[9]] <- "Nuevo Leon"
levels(matematica3$pais_num)[[11]] <- "Mexico"
levels(matematica3$pais_num)[[13]] <- "Panama"
levels(matematica3$pais_num)[[15]] <- "Peru"

## Setting the complex survey design
## First stratification variable is admrur which has public and private urbans and rurals.
## peso_estudiante is the sampling weight.

matematica3.design <- svydesign(
    ids = ~1,
    strata = ~admrur,
    weights = ~peso_estudiante,
    data = matematica3
)

## Geting each countries mean score on mathematics
t4 <- svyby(~PUNTAJE_ESTANDAR_FINAL,~pais_num,matematica3.design,svymean)

## barplot of the scores of all countries. Sorted by top performers
fourth <- ggplot(data=t4, aes(x=reorder(pais_num,-PUNTAJE_ESTANDAR_FINAL), y=PUNTAJE_ESTANDAR_FINAL)) +
    geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=16))

## Mean scores for private and publich school for each country
t <- svyby(~PUNTAJE_ESTANDAR_FINAL, ~interaction(pais_num,admrur), matematica3.design, svymean)
## Row names of the data frames had country names, change it to numbers
row.names(t) <- 1:nrow(t)

## I wanted to separate the interaction column into two columns, one specifying the country, the second the type of school

##Convert to character
t$`interaction(pais_num, admrur)` <- as.character(t$`interaction(pais_num, admrur)`)

## Create a list containing a vector for each country. Inside each vector the country name is now separated from the type of school
list <- strsplit(t$`interaction(pais_num, admrur)`,"\\.")
## run list to see the results

## create two empty vectors that will be filled up with the country name and the type of school
pais <- NULL
escuela <- NULL

for (i in 1:50) {
pais <- c(pais, list[[i]][1]) ## loop through the each vector inside the list and add the country name to the "pais" vector
escuela <- c(escuela, list[[i]][2]) ## loop through the each vector inside the list and add the type of school to the "escuela" vector
}

## adding the new vectors as factors to the existin data frame
t$pais <- as.factor(pais)
t$escuela <- as.factor(escuela)

## deleting the initial vector
t$`interaction(pais_num, admrur)` <- NULL

## barplot for the mean score of each country in urban private and public schools and public rural schools
first <- ggplot(data=t, aes(x=reorder(pais,-PUNTAJE_ESTANDAR_FINAL), y=PUNTAJE_ESTANDAR_FINAL, fill=escuela)) +
    geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=16))

## Obtaining the achievement scores for the 90th and 10th percentile
t2 <- svyby(~PUNTAJE_ESTANDAR_FINAL, ~pais_num, matematica3.design, svyquantile, quantiles=c(.10,.90), ci=T)

## replace country row names with numbers
row.names(t2) <- 1:nrow(t2)

## computing the achievement gap between top and bottom performers
t3 <- t2 %>%
        mutate(diff=abs(`0.1`-`0.9`))

## graphing the achievement difference by countries
third <- ggplot(data=t3, aes(x=reorder(pais_num,-diff), y=diff)) +
    geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=16))

## transposing the data so the the 90th and 10th percentile columns are now one column
t2 <-  t2 %>%
    select(pais_num, `0.1`, `0.9`) %>%
    gather(quantiles, puntaje, -pais_num)
t2 <- t2[order(t2$pais_num,t2$quantiles),]

## graphing mean performance for the 90th and 10th percentile for each country.
second <- ggplot(data=t2, aes(x=reorder(pais_num,-puntaje), y=puntaje, fill=quantiles)) +
    geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=16))

## function that combines as many graphs as you want into a single unified graph
## credits to Winston Chang: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

## four graphs combined
multiplot(first,third,second,fourth, cols=2)
