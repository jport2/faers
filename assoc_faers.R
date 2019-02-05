rm(list = ls())
# association analysis for faers prediction
setwd("/home/data/jportanova/FAERS/ascii")

library(dplyr)
library(stringr)
library(arules)
library(readr)
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

# remove space for both
drug_reac_b13$DRUGNAME1 <- str_replace_all(drug_reac_b13$DRUGNAME1,
                                           pattern=" ", repl="_")
drug_reac_b13$PT1 <- str_replace_all(drug_reac_b13$PT1,
                                     pattern=" ", repl="_")

# add >
# drug_reac_b13$DRUGNAME1 <- paste(">",drug_reac_b13$DRUGNAME1, sep="")

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

# add >
# dfdr13$drugname <- paste(">",dfdr13$drugname, sep="")

drug_reac_b13 <- drug_reac_b13 %>% rename(drugname = DRUGNAME1, pt = PT1)

drug_reac_b13$drugnamept <- paste0(drug_reac_b13$drugname, drug_reac_b13$pt)
dfdr13$drugnamept <- paste0(dfdr13$drugname, dfdr13$pt)

# here

# Combine data sets
a <- drug_reac_b13 %>% rename(primaryid = ISR_id) %>% select(primaryid, drugname, pt)
b <- dfdr13 %>% select(primaryid, pt, drugname)

data.use <- rbind(a, b)

# write.csv(data.use, file="ebgm_data.csv")

# Change Data around
data.use$drugname <- as.factor(data.use$drugname)
data.use$pt <- as.factor(data.use$pt)

write.csv(data.use, file="assoc_data.csv")


# data.use$pt <- str_replace_all(data.use$pt, pattern=">", repl="")
# data.use$drugname <- paste0(">", data.use$drugname)
setwd("../../data/jportanova/FAERS/ascii")
assoc <- read.csv("assoc_data.csv",stringsAsFactors = TRUE, nrows=1000)
# Association Analysis
# rules <- apriori(data.use, parameter=list(supp=0.5, conf=1))
#assoc$drugname <- as.factor(as.character(assoc$drugname))
#assoc$pt <- as.factor(as.character(assoc$pt))
# assoc$drugname <- as.logical(as.character(assoc$drugname))
# assoc$pt <- as.logical(as.character(assoc$pt))
assoc <- assoc[,2:3]
assoc <- sapply(assoc, as.factor)
trans <- as(assoc,"transactions")
rules <- apriori(assoc, parameter=list(supp=0.5, conf=0.5, minlen=2,maxlen=2))

# rules_lift <- sort(rules, by="lift", decreasing = TRUE)
# inspect(head(rules_lift))
ls <- ls()
ls <- ls[-dat]
# Compare to ryan
setwd("../../../../jportanova/Downloads")
ryan <- read.csv("40264_2013_97_MOESM1_ESM.csv")
drugs <- ryan$Drug.Concept.Name
outcome <- ryan$Condition.Name

basket_rules <- apriori(assoc,parameter = list(sup = 0.5, conf = 0.5,target="rules", minlen = 2, maxlen = 2),
                        appearance = list(lhs=drugs, rhs=outcome))ls()
