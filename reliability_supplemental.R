## Reliability -- Coding Trauma Items

require(pacman)
p_load(tidyverse, irr)

ratings <- read_csv("qualitative_ratings.csv")

### ---- Calculate reliability ----

krip <- function(grepName){
  irrList <- ratings[ratings$independent_codes,
                     grep(grepName, colnames(ratings))] %>%
    na.omit %>%
    as.matrix %>% t %>%
    kripp.alpha()
  irrList$value
}

agr <- function(grepName, independent = TRUE){
  irrList <- ratings[ratings$independent_codes,
                     grep(grepName, colnames(ratings))] %>%
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

kalpha <- sapply(categories, krip)
agreement <- sapply(categories, agr)

rel_table <- data.frame(kalpha = round(kalpha, 2), agreement = round(agreement, 1))
write.csv(rel_table, file="category_reliabilities.csv")

# ---- Identify example items ----

for(category in categories){
  temp <- ratings[, grep(category, colnames(ratings))]
  tempOrder <- order(rowSums(temp), decreasing = TRUE)
  print(category)
  print(head(ratings$EVENTS[tempOrder], 8))
}

ratings$EVENTS[grep("learned", ratings$EVENTS)]
ratings$EVENTS[grep("child", ratings$EVENTS)]

