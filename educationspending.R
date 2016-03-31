
## On this script I graph the evolution of GDP spending of education for most Latin American countries and some other
## non-Latin American countries.
## I did this to learn a little about dplyr and ggvis

library(dplyr)
library(ggvis)
library(tidyr)

latam <- read.csv("/Users/cimentadaj/Downloads/EducacacionLATAM2/EducacionLATAM2.csv", sep = ",")
latam2 <- tbl_df(latam)

## Selecting only the countries I'm interested in
latam2 <- filter(latam2, Country.Name %in% c("Argentina",
                                             "Brazil",
                                             "Chile",
                                             "Colombia",
                                             "Costa Rica",
                                             "El Salvador",
                                             "Dominican Republic",
                                             "Guatemala",
                                             "Honduras",
                                             "Mexico",
                                             "Peru",
                                             "Uruguay"))

## I want it as a factor
latam2$Country.Name <- factor(latam2$Country.Name)

## Selecting year variables which start with X
latam2 <- select(latam2, Country.Name, starts_with("X"))

##This column was in the dataframe and I wanted to remove it
latam2$X <- NULL

## Each year is a separate column. I transposed it to be a single variable
latam2 <- gather(latam2,Year,Spending,-Country.Name)
## View(latam2) to see what the change

## I browse through the year variable and when it encounters and X it replaces it with nothing ""
latam2$Year <- sapply(latam2$Year, gsub, pattern="X",replacement="")

## Similarly, I browse through each observation of Year and add the month and day. Now I can convert it to a date object.
latam2$Year <- sapply(latam2$Year, paste0,"-01-01")

## Only gonna use years before 2013, including it.
latam2 <- latam2[latam2$Year != "2014-01-01" & latam2$Year != "2015-01-01" &
                !is.na(latam2$Spending) & latam2$Country.Name != "Dominican Republic",]
## I'm specifically interested in the Dominican data, however the data here is incomplete.


## Data taken from here: https://economiadominicana.files.wordpress.com/2010/11/grafico-educacion.jpg
latam2 <- rbind(latam2, c("Dominican Republic", "1991-01-01",0.76))
latam2 <- rbind(latam2, c("Dominican Republic", "1992-01-01",1.04))
latam2 <- rbind(latam2, c("Dominican Republic", "1993-01-01",1.24))
latam2 <- rbind(latam2, c("Dominican Republic", "1994-01-01",0.95))
latam2 <- rbind(latam2, c("Dominican Republic", "1996-01-01",1.29))
latam2 <- rbind(latam2, c("Dominican Republic", "1997-01-01",1.33))
latam2 <- rbind(latam2, c("Dominican Republic", "1998-01-01",1.74))
latam2 <- rbind(latam2, c("Dominican Republic", "1999-01-01",2.03))
latam2 <- rbind(latam2, c("Dominican Republic", "2000-01-01",2.00))
latam2 <- rbind(latam2, c("Dominican Republic", "2001-01-01",2.41))
latam2 <- rbind(latam2, c("Dominican Republic", "2002-01-01",2.54))
latam2 <- rbind(latam2, c("Dominican Republic", "2003-01-01",1.53))
latam2 <- rbind(latam2, c("Dominican Republic", "2004-01-01",1.30))
latam2 <- rbind(latam2, c("Dominican Republic", "2005-01-01",1.56))
latam2 <- rbind(latam2, c("Dominican Republic", "2006-01-01",1.51))
latam2 <- rbind(latam2, c("Dominican Republic", "2007-01-01",1.78))
latam2 <- rbind(latam2, c("Dominican Republic", "2008-01-01",1.83))
latam2 <- rbind(latam2, c("Dominican Republic", "2009-01-01",1.99))
latam2 <- rbind(latam2, c("Dominican Republic", "2010-01-01",1.99))
latam2 <- rbind(latam2, c("Dominican Republic", "2011-01-01",1.98))
latam2 <- rbind(latam2, c("Dominican Republic", "2013-01-01",3.7))

## Rounding the spending to two digits
latam2$Spending <- round(as.numeric(latam2$Spending),2)

## First graph shows the Latin American mean spending per year
latam2 %>%
    group_by(Year) %>%
    filter(Country.Name != "Cuba") %>%
    summarise(mean=mean(Spending)) %>%
    ggvis(~as.Date(Year),~mean) %>%
    layer_points() %>%
    layer_lines()

## I transform year to a numeric instead of a date vector.
latam2$Year <- sapply(latam2$Year, gsub, pattern="-01-01",replacement="")
latam2$Year <- as.numeric(latam2$Year)

## When I'm gonna do now you'll understand specifically with the graph in line 110.

## I create a new variable for country names
latam2$Country.Name2 <- latam2$Country.Name
latam2$Country.Name2 <- as.character(latam2$Country.Name2)

## I replace every single country name for an empty character vector with the exception of the countries that have 2013 data.
latam2$Country.Name2[latam2$Year != 2013] <- ""

## View(latam2) will show that the only countries with names in the new column are the ones which have data for 2013

## I manually name some specific countries which don't have data on 2013 but have data on 2010
latam2$Country.Name2[latam2$Country.Name == "Mexico" & latam2$Year == 2010] <- "Mexico"
latam2$Country.Name2[latam2$Country.Name == "El Salvador" & latam2$Year == 2010] <- "El Salvador"
latam2$Country.Name2[latam2$Country.Name == "Chile" & latam2$Year == 2010] <- "Chile"
latam2$Country.Name2[latam2$Country.Name == "Brazil" & latam2$Year == 2010] <- "Brazil"
latam2$Country.Name2[latam2$Country.Name == "Argentina" & latam2$Year == 2010] <- "Argentina"

## Now the variable has country names for year 2013 and some specific countries in 2010. run View(latam2) to understand it better.

latam2 %>%
    filter(Year %in% c(1980,1990,1995,2000,2005,2010,2013)) %>%
    group_by(Country.Name) %>%
    ggvis(~Year, ~Spending) %>%
    layer_smooths(stroke=~Country.Name, strokeDash = ~Country.Name,strokeWidth := 5, strokeOpacity := 0.5) %>%
    add_axis("y", title="Gasto publico en educación (% del PIB)", properties = axis_props(labels = list(fontSize = 15))) %>%
    add_axis("x", title="Años", format = "####", properties = axis_props(labels = list(fontSize = 15))) %>%
    layer_text(text:=~Country.Name2, fontSize:=12) %>%
    hide_legend(c("stroke","strokeDash"))

## I graph the public spenditure on education for each of the selected countries. I wanted each line
## to have the name of the country so I could identify it. As you can see from the graph, all countries which
## line reaches 2013 have a name in the end. In addition, I manually added the name of countries like
## Brazil, Mexico, Argentina, Chile, etc.. which didn't have data on 2013 but did in 2010. 
## This was a simple exercise on how to include country names for each line.

#################################################

## This is quite similar to the past exercise but I included other non-Latin American countries.

latam3 <- tbl_df(latam)

# Select countries
latam3 <- filter(latam3, Country.Name %in% c("Argentina",
                                             "Brazil",
                                             "Chile",
                                             "Colombia",
                                             "Costa Rica",
                                             "El Salvador",
                                             "Dominican Republic",
                                             "Guatemala",
                                             "Honduras",
                                             "Mexico",
                                             "Peru",
                                             "Uruguay",
                                             "Denmark",
                                             "United States",
                                             "Singapore"
                                             ))

# Convert country names to factors.
latam3$Country.Name <- factor(latam3$Country.Name)

# Select variables that start with X which are the years.
latam3 <- select(latam3, Country.Name, starts_with("X"))

# remove this additional variable
latam3$X <- NULL


## Transposing the dataset so years are stacked into one column.
latam3 <- gather(latam3,Year,Spending,-Country.Name)

# Browse the year vector and replace each X in the beginning for nothing ""
latam3$Year <- sapply(latam3$Year, gsub, pattern="X",replacement="")

## Year is now numeric
latam3$Year <- as.numeric(latam3$Year)

## Exclude years 2014 and 2015 and missing values.
latam3 <- latam3[latam3$Year != 2014 & latam3$Year != 2015 &
                     !is.na(latam3$Spending) & latam3$Country.Name != "Dominican Republic",]

## I'm specifically interested in Dominican Republic and the data was incomplete.
## I obtained this one from here: https://economiadominicana.files.wordpress.com/2010/11/grafico-educacion.jpg
latam3 <- rbind(latam3, c("Dominican Republic", 1991,0.76))
latam3 <- rbind(latam3, c("Dominican Republic", 1992,1.04))
latam3 <- rbind(latam3, c("Dominican Republic", 1993,1.24))
latam3 <- rbind(latam3, c("Dominican Republic", 1994,0.95))
latam3 <- rbind(latam3, c("Dominican Republic", 1996,1.29))
latam3 <- rbind(latam3, c("Dominican Republic", 1997,1.33))
latam3 <- rbind(latam3, c("Dominican Republic", 1998,1.74))
latam3 <- rbind(latam3, c("Dominican Republic", 1999,2.03))
latam3 <- rbind(latam3, c("Dominican Republic", 2000,2.00))
latam3 <- rbind(latam3, c("Dominican Republic", 2001,2.41))
latam3 <- rbind(latam3, c("Dominican Republic", 2002,2.54))
latam3 <- rbind(latam3, c("Dominican Republic", 2003,1.53))
latam3 <- rbind(latam3, c("Dominican Republic", 2004,1.30))
latam3 <- rbind(latam3, c("Dominican Republic", 2005,1.56))
latam3 <- rbind(latam3, c("Dominican Republic", 2006,1.51))
latam3 <- rbind(latam3, c("Dominican Republic", 2007,1.78))
latam3 <- rbind(latam3, c("Dominican Republic", 2008,1.83))
latam3 <- rbind(latam3, c("Dominican Republic", 2009,1.99))
latam3 <- rbind(latam3, c("Dominican Republic", 2010,1.99))
latam3 <- rbind(latam3, c("Dominican Republic", 2011,1.98))
latam3 <- rbind(latam3, c("Dominican Republic", 2013,3.7))

## See from line 94 to 124 for a description of the next steps.
latam3$Country.Name2 <- latam3$Country.Name
latam3$Country.Name2 <- as.character(latam3$Country.Name2)
latam3$Country.Name2[latam3$Year != 2013] <- ""


latam3$Country.Name2[latam3$Country.Name == "Mexico" & latam3$Year == 2010] <- "Mexico"
latam3$Country.Name2[latam3$Country.Name == "El Salvador" & latam3$Year == 2010] <- "El Salvador"
latam3$Country.Name2[latam3$Country.Name == "Chile" & latam3$Year == 2010] <- "Chile"
latam3$Country.Name2[latam3$Country.Name == "Brazil" & latam3$Year == 2010] <- "Brazil"
latam3$Country.Name2[latam3$Country.Name == "Argentina" & latam3$Year == 2010] <- "Argentina"
latam3$Country.Name2[latam3$Country.Name == "United States" & latam3$Year == 2010] <- "USA"
latam3$Country.Name2[latam3$Country.Name == "Denmark" & latam3$Year == 2010] <- "Denmark"

latam3$Year <- as.numeric(latam3$Year)
latam3$Spending <- as.numeric(latam3$Spending)
latam3$Spending <- round(latam3$Spending,2)

## Repeat the same graph as the one before with additional countries.
latam3 %>%
    filter(Year %in% c(1980,1990,1995,2000,2005,2010,2013)) %>%
    group_by(Country.Name) %>%
    arrange(Country.Name, Year) %>%
    ggvis(~Year, ~Spending) %>%
    layer_smooths(stroke=~Country.Name, strokeDash = ~Country.Name,strokeWidth := 5, strokeOpacity := 0.5) %>%
    add_axis("y", title="Gasto publico en educación (% del PIB)", properties = axis_props(labels = list(fontSize = 15))) %>%
    add_axis("x", title="Años", format = "####", properties = axis_props(labels = list(fontSize = 15))) %>%
    layer_text(text:=~Country.Name2, fontSize:=12) %>%
    hide_legend(c("stroke","strokeDash"))



