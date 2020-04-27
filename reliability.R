## Reliability -- Coding Trauma Items

require(pacman)
p_load(tidyverse, irr)

claire <- read_csv("Codesheets - Events - Claire.csv")
margot <- read_csv("Codesheets - Events - Margot.csv")
melissa <- read_csv("Codesheets - Events - Melissa.csv")
nicole <- read_csv("Codesheets - Events - Nicole.csv")
noah <- read_csv("Codesheets - Events - Noah.csv")
payton <- read_csv("Codesheets - Events - Payton.csv")
silvana <- read_csv("Codesheets - Events - Silvana.csv")

colnames(claire)[4:18] <- paste(colnames(claire)[4:18], "claire", sep="_")
colnames(margot) <- paste(colnames(margot), "margot", sep="_")
colnames(melissa) <- paste(colnames(melissa), "melissa", sep="_")
colnames(nicole) <- paste(colnames(nicole), "nicole", sep="_")
colnames(noah) <- paste(colnames(noah), "noah", sep="_")
colnames(payton) <- paste(colnames(payton), "payton", sep="_")
colnames(silvana) <- paste(colnames(silvana), "silvana", sep="_")

ratings <- cbind(claire, 
                 margot[,4:18], 
                 melissa[,4:18], 
                 nicole[,4:18], 
                 #noah[,4:18], 
                 #payton[,4:18], 
                 silvana[,4:18])
ratings$validItem <- c(rep(FALSE, 15), rep(TRUE, 585))

rm(claire, margot, melissa, nicole, noah, payton, silvana)

### ---- Read comments ----

comments <- ratings[ratings$validItem,c(3, grep("Comments", colnames(ratings)))]
comments <- comments[rowSums(is.na(comments)) < 7 ,]
View(comments)


### ---- Calculate reliability ----

krip <- function(grepName){
  irrList <- ratings[ratings$validItem,grep(grepName, colnames(ratings))] %>%
    na.omit %>%
    as.matrix %>% t %>%
    kripp.alpha()
  irrList$value
}

agr <- function(grepName){
  irrList <- ratings[ratings$validItem,grep(grepName, colnames(ratings))] %>%
    na.omit %>%
    as.matrix %>% 
    agree
  irrList$value
}

# Krippendorff's alpha
categories <- c("Threat of physical injury",
                "Actual physical injury",
                "Threat of death",
                "Actual death",
                "Sexual content",
                "Physical pain",
                "Human perpetrator",
                "Close int. perpetrator",
                "Close int. victim",
                "Loss of possessions",
                "Threat to moral identity",
                "Threat to social status or other identity",
                "Threat to moral worldview",
                "Threat to trust in interpersonal relationships")


# ratings <- ratings[217:400,]

kalpha <- sapply(categories, krip)
agreement <- sapply(categories, agr)

# ---- Explore what's going wrong ----

exploreSums <- function(grepName){
  ratings[ratings$validItem, grepl(grepName, colnames(ratings)) & 
            !(colnames(ratings) %in% c("OrigRow","OrderNum","EVENTS","validItem")) & 
            !(grepl("Comments", colnames(ratings)))] %>%
    na.omit %>%
    as.matrix %>% 
    colSums
}


sapply(categories, exploreSums) %>% rowSums
sumTable <- sapply(categories, exploreSums)
write.csv(sumTable, "Sum Table.csv")

View(ratings[ratings$validItem, c(3, grep("Threat of physical injury", colnames(ratings)))])
View(ratings[ratings$validItem, c(3, grep("Actual physical injury", colnames(ratings)))])
View(ratings[ratings$validItem, c(3, grep("Threat to moral identity", colnames(ratings)))])
View(ratings[ratings$validItem, c(3, grep("Threat to moral worldview", colnames(ratings)))])
View(ratings[ratings$validItem, c(3, grep("Threat to trust in interpersonal relationships", colnames(ratings)))])

worstItems <- function(number){
  worst <- ratings
  worst$variance <- 0
  for(i in 1:length(categories)){
    worst$variance <- worst$variance + 
      apply(worst[,grep(categories[i], colnames(worst))], 1, var)
  }
  worst <- worst[order(worst$variance, decreasing=TRUE),c(1:3, order(colnames(worst)[4:109]))]
  return(head(worst, number))
}

worst20 <- worstItems(20)
view(worst20)
worst20$EVENTS

