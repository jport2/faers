library(dplyr)
library(readr)
library(stringr)
setwd("../../../../jportanova/Downloads")
#setwd("Downloads")
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
# drug_code$iddrug <- str_replace_all(drug_code$iddrug,",","")


setwd("/home/data/jportanova/FAERS/ascii")

# REAC PRE 13  
files <- dir()
reac <- files[grep("REAC", files)]
df <- data.frame()
for(i in reac){
  print(i)
  a <- read.delim(i, header=TRUE, sep="$", row.names=NULL)
  a <- a %>% rename(ISR_id=row.names, PT1=ISR)
  a <- a %>% select(ISR_id, PT1)
  df <- rbind(df, a)
}


# Read in Drug files
files <- dir()
drug <- files[grep("DRUG", files)]

# Drug pre 13
df2 <- data.frame()
for(i in drug){
  print(i)
  b <- read.delim(i, header=TRUE, sep="$", row.names=NULL)
  b <- b %>% rename(ISR_id=row.names, ROLE_CODE=DRUG_SEQ, DRUGNAME1=ROLE_COD)
  b <- b %>% select(ISR_id, DRUGNAME1)
  df2 <- rbind(df2, b)
}


drug_reac_b13 <- full_join(df2, df, by="ISR_id")

# REAC POST 13  
reac <- files[grep("REAC", files)]
reac2 <- reac[grep("txt", reac)]
dfr13 <- data.frame()
for(i in reac2){
  print(i)
  a <- read.delim(i, header=TRUE, sep="$")
  a <- a %>% select(primaryid, caseid, pt)
  dfr13 <- rbind(dfr13, a)
}

dfr13 <- dfr13 %>% select(primaryid, pt)
# Drug post 13
drug <- drug[grep("txt", drug)]
dfd13 <- data.frame()
for(i in drug){
  print(i)
  b <- read.delim(i, header=TRUE, sep="$", row.names=NULL)
  b <- b %>% select(primaryid, drugname)
  dfd13 <- rbind(dfd13, b)
}

dfdr13 <- full_join(dfr13,dfd13, by="primaryid")

# remove space for both
dfdr13$drugname <- str_replace_all(dfdr13$drugname,
                                   pattern=" ", repl="_")
dfdr13$pt <- str_replace_all(dfdr13$pt,
                             pattern=" ", repl="_")


drug_reac_b13 <- drug_reac_b13 %>% rename(id = ISR_id, drugname = DRUGNAME1, pt=PT1)
dfdr13 <- dfdr13 %>% rename(id = primaryid)
dfdr13 <- dfdr13 %>% select(id,drugname,pt)
data <- rbind(drug_reac_b13, dfdr13)

# replace with underscore
# to lower 

#data$drugname <- str_replace_all(data$drugname," ","_")
data$drugname <- tolower(data$drugname)
data$iddrug <- paste0(data$id, sub(' .*','', data$drugname))
# data$iddrug <- str_replace_all(data$iddrug,",","")

drug_code <- drug_code %>% select(id, X2, role_cod, iddrug)
drug_code <- drug_code %>% filter(role_cod == "PS")
psdrugs <- left_join(data, drug_code, by="iddrug")
psdrugs <- psdrugs[complete.cases(psdrugs$role_cod),]
#write.csv(psdrugs, file="drug_ps_basic.csv")
# csv dimensions 3,145,334 and 7
# csv stored indata/jportanova/FAERS/ascii
# drug_ps3 <- left_join(data, drug_code, by="id")
# drug_ps3 <- drug_ps3[complete.cases(drug_ps3$role_cod),] this doesn't work

# Drugs

psdrugs$drugname <- paste0(">", psdrugs$drugname)
psdrugs$pt <- tolower(psdrugs$pt)
psdrugs$drugname <- str_replace_all(psdrugs$drugname, " ","_")
psdrugs$pt <- str_replace_all(psdrugs$pt," ","_")
psdrugs <- psdrugs %>% select(id.x,drugname, pt)

psdrugs <- psdrugs %>%
  group_by(id.x) %>%
  mutate(drug_names = paste0(unique(drugname), collapse = " "),
         pt_names = paste0(unique(pt), collapse=" "))

psdrugs <- psdrugs %>% select(id.x, drug_names, pt_names)
psdrugs <- psdrugs %>% distinct(id.x, .keep_all=TRUE)
psdrugs <- psdrugs[,2:3]
write.table(psdrugs,file="ps_only.txt", row.names=FALSE, quote=FALSE, sep=" ")


# switched

psdrugs$pt <- tolower(psdrugs$pt)
psdrugs$drugname <- paste0(">", psdrugs$pt)
psdrugs$drugname <- str_replace_all(psdrugs$drugname, " ","_")
psdrugs$pt <- str_replace_all(psdrugs$pt," ","_")
psdrugs <- psdrugs %>% select(id.x,drugname, pt)


psdrugs <- psdrugs %>% 
  group_by(id.x) %>% 
  mutate(drug_names = paste0(unique(drugname), collapse = " "),
         pt_names = paste0(unique(pt), collapse=" ")) 

psdrugs <- psdrugs %>% select(id.x, drug_names, pt_names)
psdrugs <- psdrugs %>% distinct(id.x, .keep_all=TRUE)
psdrugs <- psdrugs[,2:3]
write.table(psdrugs,file="ps_only_switched.txt", row.names=FALSE, quote=FALSE, sep=" ")
dim(psdrugs) # 647902

