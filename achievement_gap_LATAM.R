## Using the TERCE database collected by UNESCO, I explore the average achievement for each country and
## the gap between top and bottom performers. I finish off by plotting some of these results. 
## This analysis was made to write an article on achievement gaps in Latin America.


library(data.table)
library(calibrate)
library(ggplot2)
library(directlabels)
library(plotly)
library(dplyr)
library(ggrepel)

thirdmath2 <- fread("/Users/cimentadaj/Google Drive/Estudio comparativo Latinoamerica/TERCE/Logro de aprendizaje/PM3_all_TERCE.csv", header = TRUE)
thirdmath <- as.data.table(thirdmath2)

## Difference variance of test scores for each country
thirdmath[, .(Puntaje = var(na.omit(Puntaje_estandar))), by=.(Country)]
variance <- thirdmath[, var(Puntaje_estandar, na.rm = T), by=Country] 
variance <- variance[order(variance$V1),]
barplot(variance$V1, names.arg = variance$Country, las = 2)

## Difference between the top and bottom quartile
variance2 <- thirdmath[, IQR(Puntaje_estandar, na.rm = T), by=Country]
variance2 <- variance2[order(variance2$V1),]
barplot(variance2$V1, names.arg = variance2$Country, las = 2)

cor <- thirdmath[, .(Puntaje = mean(Puntaje_estandar,na.rm = T), Disp = IQR(Puntaje_estandar,na.rm = T),
                     top=quantile(Puntaje_estandar, probs=0.75, na.rm=T),
                     bottom=quantile(Puntaje_estandar, probs=0.25, na.rm=T)), by=Country]

correlation <- variance2[order(variance$V1),]
barplot(variance2$V1, names.arg = variance2$Country, las = 2)

fit <- cor[,lm(Disp ~ Puntaje, data=cor)]
cor[,plot(Puntaje, Disp)]
abline(fit)

countries <- c("Argentina","Brasil","Chile","Colombia","Costa Rica", "Ecuador","Guatemala","Honduras","Mexico","Nicaragua",
               "Nuevo León","Panama","Paraguay","Perú","República Dominicana","Uruguay")
for (i in 1:16) cor[i,1] <- countries[i]

cor[,1] <- as.character(cor$Country)

## ggplot2 
meanY <- mean(cor$Puntaje)

graph <- ggplot(cor, aes(cor$Puntaje,cor$Disp))

t2<-theme(                              
    axis.title.x = element_text(face="bold", color="black", size=15),
    axis.title.y = element_text(face="bold", color="black", size=15),
    plot.title = element_text(face="bold", color = "black", size=20),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15)
)

graph <- graph + geom_point(aes(colour=Country), size=3) +
    xlab("Puntaje promedio en Matemàticas") + ylab("Brecha de desempeño entre los mejores y peores estudiantes") +
    ggtitle("Relación entre desempeño educativo y desigualdad") +
    geom_vline(xintercept=meanY, color="red", size=1, linetype=2) + theme_bw() +
    t2 + geom_text(x=720,y=100, label="Promedio en Latinoamérica", size=4.5); graph

direct.label(graph, list(cex=0.8, "extreme.grid"))

###################

ggplot(thirdmath[complete.cases(thirdmath$Puntaje_estandar), ],
       aes(reorder(Country, Puntaje_estandar), Puntaje_estandar)) + geom_boxplot(outlier.shape = NA) +
    stat_boxplot(geom ='errorbar')

cor <- cor %>%
    mutate(diff=top-bottom)

ggplot(cor, aes(x=reorder(Country, Puntaje), y=Puntaje)) +
    geom_point(aes(colour=diff,size=diff)) + scale_colour_gradient(low="blue",high="red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    guides(color=guide_legend(), size = guide_legend())

ggplot(cor[cor$Country %in% c("República Dominicana","Paraguay"), ],
       aes(x=reorder(Country, Puntaje), y=Puntaje, ymin=bottom, ymax=top)) + geom_pointrange()

dr <- cor[cor$Country == "República Dominicana",]$dif
all <- cor[cor$Country != "República Dominicana",]$dif
div <- round((all/dr)-1,2)
names(div) <- cor[cor$Country != "República Dominicana",]$Country


difference <- data.table(Country=c("Republica Dominicana","Argentina","Costa Rica", "Brasil"),
                         y=c(5,5,5,5),
                         diff=c(cor[Country=="República Dominicana",diff], cor[Country=="Argentina",diff], cor[Country=="Costa Rica",diff], cor[Country=="Brasil",diff]),
                         Label=c(NA, div[which(names(div) == "Argentina")], div[which(names(div) == "Costa Rica")], div[which(names(div) == "Brasil")]))

zoomtheme <- theme(legend.position="none",axis.line=element_blank(),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.y=element_blank(),axis.title.x=element_blank(),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_rect(fill="white"), plot.margin = unit(c(40,20,40,20),"mm"))

ggplot(difference, aes(x=reorder(Country, -diff), y=y, size=diff)) +
    geom_point(aes(colour=diff,alpha=0.05)) +
    scale_colour_gradient(low="blue",high="red") +
    zoomtheme +
    scale_size_continuous(range = c(20,50)) + 
    annotate("text", x=1, y=5, label= "48%") +
    annotate("text", x=2, y=5, label= "42%") +
    annotate("text", x=3, y=5, label= "6%")
