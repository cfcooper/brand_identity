
library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(reshape)
library(broom)



rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

## read in data ----------------------------------------------------------------

COdat <- read.csv("genderedmarketing_CO.csv")
COdat_numeric <- read.csv("genderedmarketing_CO_numeric.csv")
brand <- read.csv("brands.csv")
names(COdat_numeric) <- gsub("\\.", "", names(COdat_numeric))

COdemo <- subset(COdat_numeric, select=c(1:2,55:77))

##attention check
COdat$wrong <- (COdat$Q23_1 + COdat$Q23_2 + COdat$Q23_3 + COdat$Q23_4)
COwrong <- COdat[COdat$wrong == 20, ]
COdat <- COdat[!(COdat$responseId %in% COwrong$responseId), ]

COdat <- COdat[!COdat$responseId == "R_3FG01KhRwYFENPI", ]
COdat <- COdat[!COdat$responseId == "R_3vTqfAFg88icNcl", ]
COdat <- COdat[!COdat$responseId == "R_3CJBtfxAgWLUcaU", ]


COdat_numeric$wrong <- rowSums(COdat_numeric[, c(3:17)])
noalc <- COdat_numeric[COdat_numeric$wrong < 16, ]
noalc <- na.omit(noalc)



##COdat$female <- if_else(COdemo$Q17 == 2, 1, 0)

COdat$gender <- (COdat$Q21_1 + COdat$Q21_2 + COdat$Q21_3 - COdat$Q21_4) + 2
COdat$sex <- (COdat$Q23_3 + COdat$Q23_4 - COdat$Q23_1 - COdat$Q23_2) + 8






CObrand <- subset(COdat, select=c(2,12:63))
summary(CObrand)

COlong <- CObrand %>% 
  pivot_longer(cols = -c(responseId), names_to = 'brand', values_to = 'value') %>% 
  type.convert(as.is = T) %>% 
  select(responseId, brand, value)

COlong <- COlong[complete.cases(COlong), ]

COlong <- merge(COlong, brand, by="brand")
awayhome <- COlong[COlong$category %in% c("awayhome"),]


COdemo$female <- if_else(COdemo$Q17 == 2, 1, 0)

awayhome <- merge(awayhome, COdemo, by="responseId")

#awayhome_reg <- lm(value ~ Q13 + Q14 + Q16 + Q19_1 + Q17 + gender + sex + subcategory + type + scope, data = awayhome)
#summary(awayhome_reg)


alcohol <- COlong[COlong$category %in% c("alcohol"),]
alcohol <- merge(alcohol, COdemo, by="responseId")


alcohol_reg <- lm(value ~ Q13 + Q14 + Q16 + Q19_1 + Q17 + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4  + type + scope, data = alcohol)
summary(alcohol_reg)

alcohol_reg <- tidy(alcohol_reg)


## specific brands -----------------------------------------------------------------------------------

COdemo$female <- if_else(COdemo$Q17 == 2, 1, 0)



whiteclaw <- COlong[COlong$brand %in% c("whiteclaw"),]
whiteclaw <- merge(whiteclaw, COdemo, by="responseId")

whiteclaw_reg <- lm(value ~ Q13 + Q14 + Q16 + Q17 + Q19_1 + female + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4, data = whiteclaw)
summary(whiteclaw_reg)




chipotle <- COlong[COlong$brand %in% c("chipotle"),]
chipotle <- merge(chipotle, COdemo, by="responseId")

chipotle_reg <- lm(value ~ Q13 + Q14 + Q16 + Q17 + Q19_1 + female + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4, data = chipotle)
summary(chipotle_reg)




chickfila <- COlong[COlong$brand %in% c("chickfila"),]
chickfila <- merge(chickfila, COdemo, by="responseId")

chickfila_reg <- lm(value ~ Q13 + Q14 + Q16 + Q17 + Q19_1 + female + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4, data = chickfila)
summary(chickfila_reg)





sweetgreen <- COlong[COlong$brand %in% c("sweetgreen"),]
sweetgreen <- merge(sweetgreen, COdemo, by="responseId")
mean(sweetgreen$value)
sweetgreen_reg <- lm(value ~ Q13 + Q14 + Q16 + Q17 + Q19_1 + female + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4, data = sweetgreen)
summary(sweetgreen_reg)





popeyes <- COlong[COlong$brand %in% c("popeyes"),]
popeyes <- merge(popeyes, COdemo, by="responseId")
mean(popeyes$value)
popeyes_reg <- lm(value ~ Q13 + Q14 + Q16 + Q17 + Q19_1 + female + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4, data = popeyes)
summary(popeyes_reg)




## subset groups ----------------------------------------------------------

objects <- ls()
datasets_to_keep <- c("COlong", "COdat","COdat_numeric","CObrand","brand","COdemo")
objects_to_remove <- setdiff(objects, datasets_to_keep)
rm(list = objects_to_remove)



male_conservative <- COdemo[COdemo$Q17 %in% c("1"),]
male_conservative <- male_conservative[male_conservative$Q19_1 > 5,]

alcohol <- COlong[COlong$category %in% c("alcohol"),]
alcohol_malecons <- merge(alcohol, male_conservative, by="responseId")

alcohol_reg_mc <- lm(value ~ Q13 + Q14 + Q16 + Q19_1 + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4  + type + scope, data = alcohol_malecons)
summary(alcohol_reg_mc)


male_center <- COdemo[COdemo$Q17 %in% c("1"),]
male_center <- male_center[(male_center[, 17] < 6) & (male_center[, 17] > 2), ]

alcohol <- COlong[COlong$category %in% c("alcohol"),]
alcohol_malecent <- merge(alcohol, male_center, by="responseId")

alcohol_reg_mce <- lm(value ~ Q13 + Q14 + Q16 + Q19_1 + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4  + type + scope, data = alcohol_malecent)
summary(alcohol_reg_mce)



male_liberal <- COdemo[COdemo$Q17 %in% c("1"),]
male_liberal <- male_liberal[male_liberal$Q19_1 < 3,]

alcohol <- COlong[COlong$category %in% c("alcohol"),]
alcohol_malelib <- merge(alcohol, male_liberal, by="responseId")

alcohol_reg_ml <- lm(value ~ Q13 + Q14 + Q16 + Q19_1 + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4  + type + scope, data = alcohol_malelib)
summary(alcohol_reg_ml)



female_center <- COdemo[COdemo$Q17 %in% c("2"),]
female_center <- female_center[(female_center[, 17] < 6) & (female_center[, 17] > 2), ]

alcohol <- COlong[COlong$category %in% c("alcohol"),]
alcohol_femalecent <- merge(alcohol, female_center, by="responseId")

alcohol_reg_fce <- lm(value ~ Q13 + Q14 + Q16 + Q19_1 + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4  + type + scope, data = alcohol_femalecent)
summary(alcohol_reg_fce)




female_conservative <- COdemo[COdemo$Q17 %in% c("2"),]
female_conservative <- female_conservative[female_conservative$Q19_1 > 5,]

alcohol <- COlong[COlong$category %in% c("alcohol"),]
alcohol_femalecons <- merge(alcohol, female_conservative, by="responseId")

alcohol_reg_fc <- lm(value ~ Q13 + Q14 + Q16 + Q19_1 + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4  + type + scope, data = alcohol_femalecons)
summary(alcohol_reg_fc)




female_liberal <- COdemo[COdemo$Q17 %in% c("2"),]
female_liberal <- female_liberal[female_liberal$Q19_1 < 3,]

alcohol <- COlong[COlong$category %in% c("alcohol"),]
alcohol_femalelib <- merge(alcohol, female_liberal, by="responseId")

alcohol_reg_fl <- lm(value ~ Q13 + Q14 + Q16 + Q19_1 + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4  + type + scope, data = alcohol_femalelib)
summary(alcohol_reg_fl)



##groups for food ---------------------------

young <- COdemo[COdemo$Q16 < 4,]
chickfila <- COlong[COlong$brand %in% c("chickfila"),]
chickfila_young <- merge(chickfila, young, by="responseId")

chickfila_young_reg <- lm(value ~ Q13 + Q14 + Q16 + Q17 + Q19_1 + Q18 + Q21_1 + Q21_2 + Q21_3 + Q21_4 + Q23_1 + Q23_2 + Q23_3 + Q23_4, data = chickfila_young)
summary(chickfila_young_reg)



