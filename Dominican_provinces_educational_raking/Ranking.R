dir.create(paste0(getwd(),"/RD_Provinces"))
setwd(paste0(getwd(),"/RD_Provinces"))

temp <- tempfile()
download.file("https://github.com/cimentadaj/random-stuff/blob/master/Dominican_provinces_educational_raking/Archive.zip?raw=true",
              temp)
unzip(temp)
unlink(temp)

## original dataset: http://one.gob.do/Multimedia/Download?ObjId=1355
analfabeto <- read.csv("analfabeto-provincia.csv", skip=4, nrow=33, stringsAsFactors = F)[-1,c("X.1","Total","Hombre","Mujer")]
names(analfabeto)[1] <- c("Province") ## Make sure all columns are identifiable

## The goal is to estimate how big is a gap for each province, regardless of whether women or men are higher.
analfabeto$gender_gap_analfa <- round((abs(analfabeto$Hombre - analfabeto$Mujer)/analfabeto$Total) * 100, 1) # Percentage of men which are analfabetos relative to women
analfabeto[order(analfabeto$gender_gap_analfa), ]
## This will work as a rank. The higher the worst.

## original dataset: http://one.gob.do/Multimedia/Download?ObjId=1349
yrsedu <- read.csv("anoseducacion.csv", skip=5, header=TRUE, nrow=32, stringsAsFactors = F)[2:3]
yrsedu[2] <- round(yrsedu[2], 2)
names(yrsedu)[1:2] <- c("Province","avg_yrs_education") ## Make sure all columns are identifiable

## original dataset: http://one.gob.do/Multimedia/Download?ObjId=5985
## Let's match the number of enrolled students per province with the province name. This
## works because the provinces in yrsedu are ordered, otherwise the names would get messed up.
total_students <- setNames(read.csv("estudiantesmatriculados.csv", skip=9, nrow=32, stringsAsFactors = F)[,2], yrsedu[,1])


## original dataset: http://one.gob.do/Multimedia/Download?ObjId=5403
prof_province <- read.csv("prof-provincia.csv", skip=8, nrow=32, stringsAsFactors = F)[,1:2]
names(prof_province) <- c("Province","total_teachers") ## Make sure all columns are identifiable

## To obtain an estimate of how many students per teacher are in each province let's
## append the total students to the teacher data frame and divide total students by teachers
prof_province$total_students <- total_students
prof_province$ratio_student_teacher <- round(prof_province$total_students/prof_province$total_teachers,0)

## original dataset: http://one.gob.do/Multimedia/Download?ObjId=1383
repetition <- read.csv("repeticion.csv", skip=8, nrow=42, stringsAsFactors = F)[-c(1,4,8,12,17,22,27,31,35,38),-4]
names(repetition) <- c("Province", "Inic_dropout","Inic_promoted","Basic_dropout","Basic_promoted",
                       "Basic_repetition","Middle_dropout","Middle_promoted","Middle_repetition")
## Make sure all columns are identifiable

## Delete unwanted columns
delete_cols <- function(x) {
    repetition <- repetition[,-x]
    return(repetition)
}

repetition <- delete_cols(which(names(repetition) %in% c("Inic_promoted","Basic_promoted","Middle_promoted")))

## compile list with all data frames
ourlist <- list(analfabeto, yrsedu, prof_province, repetition)

## Let's sort all Province columns from the data frames
ourlist <- lapply(ourlist, function(x) x[order(x$Province),])
ourlist[[4]]$Province <- gsub("? $", "", ourlist[[4]]$Province) # Delete white space at the end

provinces <- do.call(cbind, lapply(ourlist, function(x) x$Province))


# Check to see if the names of each province are the same
provinces[,1] == provinces[,2:4] # row 25 is different
provinces[,2] == provinces[,3:4] # row 25 is different
provinces[,3] == provinces[,4, drop=F] # row 25

# The difference is because of a capital letter(something common in Spanish). Confirm it:
provinces[25,]
# Fix it:
ourlist[[3]][25,1] <- ourlist[[1]][25,1]
lapply(ourlist, function(x) x[25,1]) # confirmation

## Let's bind all data frame together, with only the province variable of the first one
complete_data <- cbind(ourlist[[1]][c(1,5)],ourlist[[2]][-1],ourlist[[3]][-(1:3)],ourlist[[4]][,-1])

## Let's loop through the data frame, sort each column and create a rank variable for that column
for(i in names(complete_data)[-1]){
    if (i == "avg_yrs_education") { ## this variable the ranking should be decreasing, as more years of education positive
        complete_data[order(complete_data[,i], decreasing = T), paste0("rank_", i)] <- 1:nrow(complete_data)
    } else 
        complete_data[order(complete_data[,i]),paste0("rank_", i)] <- 1:nrow(complete_data)
}

## Identify all rank variables to create a separate data frame
columns <- grepl("rank", names(complete_data))
columns[1] <- TRUE ## But keep the province column

## New data frame with only rank columns
complete_data2 <- complete_data[,columns]

## Sum it all up to create an overall ranking
complete_data2$total_ranking <- rowSums(complete_data2[-1])

cl <- complete_data2[order(complete_data2$total_ranking),1 , drop=F]
row.names(cl) <- 1:nrow(cl)
cl ## Final Ranking
