# FAERS NORMALIZED (BANDA) MERGED FOR DATA SET CREATION

### Banda to FAERS ################################### 
#1 Read in Standard Drug Case
#2 Read in Standard Outcome Case
#3 Merge both with concept
#4 Left Join Drug and Outcome

setwd("/home/jportanova/Downloads")
library(dplyr)
library(readr)
library(stringr)

standard_case_drug <- read_tsv("standard_case_drug.tsv", col_names =FALSE)
standard_case_drug <- standard_case_drug %>% rename(primaryid = X1, isr = X2, drug_seq = X3, role_cod = X4, 
                                                    standard_concept_id = X5)
standard_case_drug$standard_concept_id <- as.factor(standard_case_drug$standard_concept_id)
concept <- read_tsv("concept.tsv", col_names = FALSE)
concept1 <- concept %>% rename(standard_concept_id = X1)
concept1$standard_concept_id <- as.factor(concept1$standard_concept_id)
concept1 <- concept1 %>% select(one_of("standard_concept_id", "X2"))
drug_code <- left_join(standard_case_drug, concept1, by="standard_concept_id")

drug_code$id <- paste0(drug_code$primaryid, drug_code$isr)
drug_code$id <- str_replace_all(drug_code$id, "NA","")
drug_code$X2 <- str_replace_all(drug_code$X2," ","_")
drug_code$X2 <- tolower(drug_code$X2)
drug_code$iddrug <- paste0(drug_code$id, sub('_.*','', drug_code$X2))

# Outcome 
standard_case_outcome <- read_tsv("standard_case_outcome.tsv", col_names =FALSE)
standard_case_outcome <- standard_case_outcome %>% rename(primaryid = X1, isr = X2, pt = X3, standard_concept_id = X4, 
                                                    snomed_id = X5)
standard_case_outcome$standard_concept_id <- as.factor(standard_case_outcome$standard_concept_id)
outcome_code <- left_join(standard_case_outcome, concept1, by="standard_concept_id")


# outcome_code$primaryid <- gsub("\\N","NA", outcome_code$primaryid)
outcome_code$id <- paste0(outcome_code$primaryid, outcome_code$isr)
outcome_code$id <- gsub("[\r\\N]","", outcome_code$id)
outcome_code$X2 <- str_replace_all(outcome_code$X2," ","_")
outcome_code$X2 <- tolower(outcome_code$X2)
# outcome_code$idoutcome <- paste0(outcome_code$id, sub('_.*','', outcome_code$X2)) # this line is both wrong (adds A) and irrelevant
outcome_code$id <- str_replace_all(outcome_code$id, "A","")
drugoutcome <- left_join(drug_code, outcome_code, by="id")

# outcomes are not reading in for primary id! Take out the A
drugoutcome <- drugoutcome %>% rename(drug_names = X2.x, pt_names = X2.y)
drugoutcome <- drugoutcome[complete.cases(drugoutcome$pt_names),]
drugoutcome <- drugoutcome[complete.cases(drugoutcome$drug_names),]
ps <- drugoutcome %>% filter(role_cod == "PS")
psswitch <- ps
banda <- drugoutcome
bandaswitched <- drugoutcome

### banda regular ps #####################################################################
ps$drug_names <- paste0(">", ps$drug_names)
ps <- ps %>% 
  group_by(id) %>% 
  mutate(drug_names = paste0(unique(drug_names), collapse = " "),
         pt_names = paste0(unique(pt_names), collapse=" ")) 

ps <- ps %>% distinct(id, .keep_all = TRUE)
ps <- ps[,c("drug_names", "pt_names")]
setwd("/home/data/jportanova/FAERS/ascii/psonly")
write.table(ps,file="ps_only_standardized.txt", row.names=FALSE, quote=FALSE, sep=" ")

# Note: Now there is no psonly data set from faers use psonly for banda psonly 
# Note: unique1, unique2d, and psswithced are all related to FAERS, there is currently no psonly for FAERS

### banda ps switch #####################################################################
psswitch$pt_names <- paste0(">", psswitch$pt_names)
psswitch <- psswitch %>% 
  group_by(id) %>% 
  mutate(drug_names = paste0(unique(drug_names), collapse = " "),
         pt_names = paste0(unique(pt_names), collapse=" ")) 

psswitch <- psswitch %>% distinct(id, .keep_all = TRUE)
psswitch2 <- psswitch[,c("drug_names", "pt_names")]
setwd("/home/data/jportanova/FAERS/ascii/psswitchbanda")
write.table(psswitch2,file="psswitch_only_standardized.txt", row.names=FALSE, quote=FALSE, sep=" ")

### banda regular #####################################################################
banda$drug_names <- paste0(">", banda$drug_names)
banda <- banda %>% 
  group_by(id) %>% 
  mutate(drug_names = paste0(unique(drug_names), collapse = " "),
         pt_names = paste0(unique(pt_names), collapse=" ")) 

banda <- banda %>% distinct(id, .keep_all = TRUE)
banda <- banda[,c("drug_names", "pt_names")]
setwd("/home/data/jportanova/FAERS/ascii/banda")
write.table(banda,file="banda_full_standardized.txt", row.names=FALSE, quote=FALSE, sep=" ")

### banda regular switch #####################################################################
bandaswitched$pt_names <- paste0(">", bandaswitched$pt_names)
bandaswitched <- bandaswitched %>% 
  group_by(id) %>% 
  mutate(drug_names = paste0(unique(drug_names), collapse = " "),
         pt_names = paste0(unique(pt_names), collapse=" ")) 

bandaswitched <- bandaswitched %>% distinct(id, .keep_all = TRUE)
bandaswitched <- bandaswitched[,c("drug_names", "pt_names")]
setwd("/home/data/jportanova/FAERS/ascii/bandaswitch")
write.table(bandaswitched,file="bandaswitch_full_standardized.txt", row.names=FALSE, quote=FALSE, sep=" ")


