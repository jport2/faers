# Checks AUC against 132/399 of the Ryan Set

setwd("/home/jportanova/Downloads")
library(readr)
library(dplyr)
sdoct<- read.table("standard_drug_outcome_contingency_table.tsv", sep="\t")
# sdoct<- read_tsv("standard_drug_outcome_contingency_table.tsv", col_names = FALSE) change 'V' to 'X'
sdoct <- sdoct %>% rename(drug_concept_id = V1, outcome_concept_id = V2, a = V3, b = V4, c = V5, d = V6)
sdoct$b <- as.integer(as.character(sdoct$b))
sdoct$c <- as.integer(as.character(sdoct$c))
# why are NA's introduced by coercion

# Calculate PRR
sdoct <- sdoct %>% mutate(prr = (a/(a+b))/(c/(c+d)))

# Calculate ROR
sdoct <- sdoct %>% mutate(ror = (a/c)/(b/d))

# map concepts
concept <- read_tsv("concept.tsv", col_names = FALSE)
concept1 <- concept %>% rename(standard_concept_id = X1)
concept1$standard_concept_id <- as.factor(concept1$standard_concept_id)
concept1 <- concept1 %>% select(one_of("standard_concept_id", "X2"))
sdoct <- sdoct %>% rename(standard_concept_id = drug_concept_id)
sdoct$standard_concept_id <- as.factor(as.character(sdoct$standard_concept_id))
sdoct <- left_join(sdoct, concept1, by="standard_concept_id")
sdoct <- sdoct %>% rename(drug_concept_id = standard_concept_id, standard_concept_id = outcome_concept_id)
sdoct$standard_concept_id <- as.factor(as.character(sdoct$standard_concept_id))
sdoct <- left_join(sdoct, concept1, by="standard_concept_id")

sdoct$X2.x <- tolower(sdoct$X2.x)
sdoct$X2.y <- tolower(sdoct$X2.y)
sdoct$drugoutcome <- paste0(sdoct$X2.x, sdoct$X2.y)
ryan <- read.csv("40264_2013_97_MOESM1_ESM.csv") 
ryan <- ryan %>% rename(drug_concept_id = Drug.Concept.ID)

# to lower
ryan$Condition.Name <- tolower(ryan$Condition.Name)
ryan$Drug.Concept.Name <- tolower(ryan$Drug.Concept.Name)
ryan$drugoutcome <- paste0(ryan$Drug.Concept.Name,ryan$Condition.Name)
merge <- left_join(sdoct, ryan, by="drugoutcome")
merge <- merge[complete.cases(merge$Condition.Concept.ID),]

# basicplot <- ggplot(merge, aes(d = Ground.Truth..1.postive.control..0.negative.control., m = prr)) + geom_roc()
library(pROC)
plot <-plot.roc(merge$Ground.Truth..1.postive.control..0.negative.control., merge$prr, type="b", pch=21, col="blue", bg="grey")
plot
plot <-plot.roc(merge$Ground.Truth..1.postive.control..0.negative.control., merge$ror, type="b", pch=21, col="blue", bg="grey")
plot
