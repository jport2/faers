setwd("/home/data/faersIndex")
input <- read.table("inputweightvectors.txt", sep="|", fill=NA)
output <- read.table("outputweightvectors.txt", sep="|", fill=NA)

# a <- input
# b <- input
input <- input[-1,]
input <- input[complete.cases(input$V1),]
input <- t(input)
colnames(input) <- input[1,]
input <- input[-1,]
# input <- as.data.frame(input)
input2 <- input[,grep("Acute_myocardial_infarction", colnames(input))]
input3 <- input[,grep("Acute_kidney_injury", colnames(input))]
input4 <- input[,grep("Liver", colnames(input))]
input5 <- input[,grep("liver", colnames(input))]
input6 <- input[,grep("Gastrointestinal", colnames(input))]
input7 <- rbind(input2,input3,input4,input5,input6)

dim(input7)
write.csv(input, file="ryaninputweightvectors.csv")

input2 <- input[,index]
#row.names(input) <- as.character(input[,1])
#input <- input[,-1]

output <- output[-1,]
row.names(output) <- as.character(output[,1])
output <- output[,-1]

input<- t(input)
output<-t(output)
# 
# sp <- input[,1]*t(output[,1])
# output[,1]*t(input[,1])
# input <- t(input)
# input[,1]*t(output[,1])

input1 <- input[1,]
output1 <- output[1,]
sigmoid(dot(as.numeric(input1), as.numeric(output1)))


setwd("/home/jportanova/Downloads")
ryan <- read.csv("40264_2013_97_MOESM1_ESM.csv")
drug_name <- unique(ryan$Drug.Concept.Name)
data <- data.frame()
for(i in drug_name){
  z <- output[grep(i,output$V1),]
  data <- rbind(data, z)
}



input <- t(input)
l <- list(ryan$Drug.Concept.Name)
df <- data.frame()

# dot(input, output[,2])
input <- as.data.frame(input)
input <- input %>% select(one_of("Acute myocardial infarction", "Acute kindey injury", "Acute liver injury", "GI bleed"))
# 
input <- input %>% select(one_of("Acute_myocardial_infarction", "Renal_failure_acute", "Acute_kindey_injury", 
                                 "Acute_hepatic_failure","Gastrointestinal_haemorrhage"))

setwd("/home/data/faersIndex")
drugvec <- read.table("drugvectors_loop.txt", fill=NA, sep="|")
hoivec <- read.table("hoivectors.txt", sep="|")

hoivect <- t(hoivec)
colnames(hoivect) <- hoivect[1,]
hoivect <- hoivect[-1,]

drugvect <- t(drugvec)
colnames(drugvect) <- drugvect[1,]
drugvect <- drugvect[-1,]
# drugvec1 <- drugvec[2,]
# hoivec1 <- hoivec[2,]
df <- data.frame()
# a <- data.frame()
# df$drug <- 1
# df$hoi <- 2
# df$p <- 3
library(pracma)
for(i in 1:ncol(drugvect)){
  # print(i)
  # print(colnames(drugvect)[i])
  for(j in 1:ncol(hoivect)){
    # print(j)
    # print(colnames(hoivect)[j])
    a <- colnames(drugvect)[i]
    b <- colnames(hoivect)[j]
    c <- sigmoid(dot(as.numeric(drugvect[,i]), as.numeric(hoivect[,j])))
    d <- cbind(a,b,c)
    df <- rbind(df, d)
    # a <- data.frame()
    # a$drug <- colnames(drugvect)[i]
    # a$hoi <- colnames(hoivect)[j]
    # print(sigmoid(dot(as.numeric(drugvect[,i]), as.numeric(hoivect[,j]))))
    # df <- rbind(df,a)
  }
}

# write.csv(df, file="faers2vec_ryan.csv")
# faers2vec <- read.csv("faers2vec_ryan.csv")
# df <- faers2vec
# read in ryan
setwd("/home/jportanova/Downloads")
ryan <- read.csv("40264_2013_97_MOESM1_ESM.csv")
library(dplyr)
# Change ryan names
ryan <- ryan %>% mutate(a = Drug.Concept.Name, b=Condition.Name)
ryan$a <- paste0(">",ryan$a)
library(stringr)
# ryan$a <- str_replace_all(ryan$a,
#                                    pattern=" ", repl="_")
ryan$b <- str_replace_all(ryan$b,
                             pattern=" ", repl="_")
df$a <- sub('_.*','',df$a)
#df <- df[complete.cases(df$b),]
#df <- df %>% mutate(b = replace(b,b=="Gastrointestinal_haemorrhage","GI_bleed"))
df <- df %>% mutate(b = as.character(ifelse(b == "Gastrointestinal_haemorrhage", "GI_bleed", b)))
df[df=="Gastrointestinal_haemorrhage"] <- "GI_bleed"
df$d <- paste0(df$a,df$b)
ryan$d <- paste0(ryan$a, ryan$b)

a <- inner_join(ryan, df, by="d")
b <- a %>% group_by(d) %>% summarise(c2=max(as.numeric(as.character(c))))
merge <- left_join(ryan,b, by="d")
library(pROC)
plot <-plot.roc(merge$Ground.Truth..1.postive.control..0.negative.control., merge$c2, type="b", pch=21, col="blue", bg="grey")

# Methods
