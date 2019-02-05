
sdoct<- read.table("standard_drug_outcome_contingency_table.tsv", sep="\t")
sdoct <- sdoct %>% rename(drug_concept_id = V1, outcome_concept_id = V2, a = V3, b = V4, c = V5, d = V6)
sdoct$b <- as.integer(as.character(sdoct$b))
sdoct$c <- as.integer(as.character(sdoct$c))
# why are NA's introduced by coercion

# Calculate PRR
sdoct <- sdoct %>% mutate(prr = (a/(a+b))/(c/(c+d)))

# Calculate ROR
sdoct <- sdoct %>% mutate(ror = (a/c)/(b/d))
ryan <- ryan %>% rename(drug_concept_id = Drug.Concept.ID)
merge <- left_join(sdoct, ryan, by="drug_concept_id")

merge <- merge[complete.cases(merge$Condition.Concept.ID),]

# basicplot <- ggplot(merge, aes(d = Ground.Truth..1.postive.control..0.negative.control., m = prr)) + geom_roc()
library(pROC)
plot <-plot.roc(merge$Ground.Truth..1.postive.control..0.negative.control., merge$prr, type="b", pch=21, col="blue", bg="grey")
