## Reliability -- Coding Trauma Items

require(pacman)
p_load(tidyverse, irr)

setwd("~/Google Drive/01 - Research/McNally Lab Qualitative Coding Projects/Payton's Project/Qualitative Coding/Data/Codesheets/")

claire <- read_csv("Codesheets - Events - Claire.csv")
margot <- read_csv("Codesheets - Events - Margot.csv")
melissa <- read_csv("Codesheets - Events - Melissa.csv")
nicole <- read_csv("Codesheets - Events - Nicole.csv")
noah <- read_csv("Codesheets - Events - Noah.csv")
silvana <- read_csv("Codesheets - Events - Silvana.csv")

colnames(claire)[4:18] <- paste(colnames(claire)[4:18], "claire", sep="_")
colnames(margot) <- paste(colnames(margot), "margot", sep="_")
colnames(melissa) <- paste(colnames(melissa), "melissa", sep="_")
colnames(nicole) <- paste(colnames(nicole), "nicole", sep="_")
colnames(noah) <- paste(colnames(noah), "noah", sep="_")
colnames(silvana) <- paste(colnames(silvana), "silvana", sep="_")

ratings <- cbind(claire, 
                 margot[,4:18], 
                 melissa[,4:18], 
                 nicole[,4:18], 
                 noah[,4:18], 
                 silvana[,4:18])

rm(claire, margot, melissa, nicole, noah, silvana)

### ---- Clean ----

ratings$independent_codes <- c(rep(FALSE, 216), rep(TRUE, 600-216))

ratings[,c(grep("Comments", colnames(ratings)))] <- NULL
ratings$OrigRow <- NULL
ratings$OrderNum <- NULL

### ---- Save ----

setwd("~/Google Drive/01 - Research/McNally Lab Qualitative Coding Projects/Payton's Project/Qualitative Coding/Data")
write.csv(ratings, file = "qualitative_ratings.csv", row.names = FALSE)

