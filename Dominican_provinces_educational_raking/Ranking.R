setwd("/Users/cimentadaj/Downloads/DR Try/")

unzip(list.files()[1])

## original dataset: http://one.gob.do/Multimedia/Download?ObjId=1355
analfabeto <- read.csv(list.files()[2], skip=4, nrow=33, stringsAsFactors = F)[-1,c("X.1","Total","Hombre","Mujer")]
names(analfabeto)[1] <- c("Province","")

## The goal is to estimate how big is a gap, regardless of wether women or men are higher.
analfabeto$gender_gap_analfa <- round((abs(analfabeto$Hombre - analfabeto$Mujer)/analfabeto$Total) * 100,1)

analfabeto[order(analfabeto$gender_gap_analfa), ]
## This will work as a rank. The higher the worst.

## original dataset: http://one.gob.do/Multimedia/Download?ObjId=1349
yrsedu <- read.csv(list.files()[4], skip=5, header=TRUE, nrow=32, stringsAsFactors = F)[2:3]
yrsedu[2] <- lapply(yrsedu[2], round, 2)
names(yrsedu)[1:2] <- c("Province","avg_yrs_education")

## original dataset: http://one.gob.do/Multimedia/Download?ObjId=5985
total_students <- setNames(read.csv(list.files()[6], skip=9, nrow=32, stringsAsFactors = F)[,2], yrsedu[,1])

## original dataset: http://one.gob.do/Multimedia/Download?ObjId=5403
prof_province <- read.csv(list.files()[10], skip=8, nrow=32, stringsAsFactors = F)[,1:2]
names(prof_province) <- c("Province","total_teachers")
prof_province$total_students <- total_students
prof_province$ratio_student_teacher <- round(prof_province$total_students/prof_province$total_teachers,0)

## original dataset: http://one.gob.do/Multimedia/Download?ObjId=1383
repetition <- read.csv(list.files()[12], skip=8, nrow=42, stringsAsFactors = F)[-c(1,4,8,12,17,22,27,31,35,38),-4]
names(repetition) <- c("Province", "Inic_dropout","Inic_promoted","Basic_dropout","Basic_promoted",
                       "Basic_repetition","Middle_dropout","Middle_promoted","Middle_repetition")

repetition$Inic_promoted <- NULL
repetition$Basic_promoted <- NULL
repetition$Middle_promoted <- NULL

ourlist <- list(analfabeto, yrsedu, prof_province, repetition)

for (i in 1:length(ourlist)) {
    ourlist[[i]] <- ourlist[[i]][order(ourlist[[i]][,1]),]
}

for (i in 1:length(ourlist[[1]][,1])) {
    ourlist[[4]][,1][i] <- ourlist[[1]][,1][i] 
}

complete_data <- cbind(ourlist[[1]][c(1,5)],ourlist[[2]][-1],ourlist[[3]][-(1:3)],ourlist[[4]][,-1])

for(i in names(complete_data)[-1]){
    if (i == "avg_yrs_education") {
        complete_data[order(complete_data[,i], decreasing = T), paste0("rank_", i)] <- 1:nrow(complete_data)
    } else 
        complete_data[order(complete_data[,i]),paste0("rank_", i)] <- 1:nrow(complete_data)
}

columns <- grepl("rank", names(complete_data))
columns[1] <- TRUE
complete_data2 <- complete_data[,columns]
complete_data2$total_ranking <- rowSums(complete_data2[-1])

cl <- complete_data2[order(complete_data2$total_ranking),1, drop=F]
row.names(cl) <- 1:nrow(cl)
cl ## Final Ranking

