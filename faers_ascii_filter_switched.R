cd ../data/jportanova/FAERS/ascii

setwd("/home/data/jportanova/FAERS/ascii")

library(dplyr)
library(stringr)

# a <- read.table("REAC12Q3.TXT", sep="$", header = TRUE, row.names=NULL, quote="")
# a <- read.delim("DRUG12Q3.TXT", header=TRUE, sep="$", row.names = NULL)
# a <- read.delim("REAC14Q4.txt", header=TRUE, sep="$")


# change column titles for previous merge
# add new merge 
# 1 line for
# add identifier for drugs >



# Usage and Integration of Reporting System Data for Post-Marketing Drug Surveillance
# 3 ml, integrating data sources, combining signals, ehr , nlp

# drug's wont have spaces
# spaces between them in df
# > sign 

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
drug_reac_b13$PT1 <- paste(">",drug_reac_b13$PT1, sep="")

# pt 2
# dr_b13 <- drug_reac_b13
# dr_b13$DRUGNAME1 <- gsub("^(.*?),.*", "\\1", dr_b13$DRUGNAME1)

# remove duplicates
# library(data.table) ## >= v1.9.6
# z <- drug_reac_b13[1:100,]
# x <- setDT(z)[, .(count = unique(DRUGNAME1)), by = ISR_id]
drug_reac_b13a <- drug_reac_b13 %>% 
  group_by(ISR_id) %>% 
  mutate(drug_names = paste0(unique(DRUGNAME1), collapse = " "),
         pt_names = paste0(unique(PT1), collapse=" ")) 

# drug_reac_b13a$drug_names <- unique(drug_reac_b13a$drug_names)
drug_reac_b13a <- drug_reac_b13a %>% select(drug_names, pt_names)
drug_reac_b13b <- drug_reac_b13a %>% distinct(ISR_id, .keep_all=TRUE)
drug_reac_b13c <- drug_reac_b13b[,2:3]
# drug_reac_b13c <- drug_reac_b13c[1:100,]
# write.table(drug_reac_b13c, file="drug_reac_b13_full_unique.txt", row.names=FALSE, quote=FALSE, sep=" ")
# write.csv(drug_reac_b13c, file="drug_reac_b13_full_unique.csv", row.names=FALSE, quote=FALSE)


# c <- read.delim("DRUG12Q3.TXT", header=TRUE, sep="$", row.names=NULL)
# d <- read.delim("DRUG13Q2.txt", header=TRUE, sep="$", row.names=NULL)
# e <- read.delim("REAC13Q2.txt", header=TRUE, sep="$", row.names=NULL)
# f <- read.delim("REAC12Q3.TXT", header=TRUE, sep="$", row.names = NULL)


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
dfdr13$pt <- paste(">",dfdr13$pt, sep="")

dfdr13a <- dfdr13 %>% 
  group_by(primaryid) %>% 
  mutate(drug_names = paste0(drugname, collapse = " "),
         pt_names = paste0(pt, collapse=" ")) 

#dfdr13a <- dfdr13a %>% select(drug_names, pt_names)
dfdr13b <- dfdr13a %>% distinct(primaryid, .keep_all=TRUE)
dfdr13c <- dfdr13b[,4:5]
#dfdr13c <- dfdr13c[1:100,]
# write.table(dfdr13c, file="drug_reac_a13_full_unique.txt", row.names=FALSE, quote=FALSE, sep=" ")
# write.csv(dfdr13c, file="drug_reac_a13_full_unique.csv", row.names=FALSE, quote=FALSE)


# see if there are any same ID's 
drug_reac_b13bc <- drug_reac_b13b %>% rename(primaryid = ISR_id)
drug_reac_b13bc$primaryid <- as.numeric(as.character(drug_reac_b13bc$primaryid))
drug_reac <- full_join(drug_reac_b13bc, dfdr13b, by="primaryid")
dfdr13b <- dfdr13b[,c("primaryid", "drug_names","pt_names")]
drug_reac2 <- rbind(drug_reac_b13bc, dfdr13b)
drug_reac2 <- drug_reac2[,2:3]
write.table(drug_reac2,file="drug_reac_full_switched.txt", row.names=FALSE, quote=FALSE, sep=" ")
#write.csv(drug_reac2, file="drug_reac_full2.csv", row.names = FALSE, quote=FALSE)

# regular
## drug_reac_full.txt

# unique regular
## drug_reac_full_unique.txt

# unique switched
## drug_reac_full2.txt

# switched
## drug_reac_full_switched