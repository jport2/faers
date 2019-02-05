# openEBGM

library(openEBGM)
library(tidyr)
data(caers_raw)
head(caers_raw, 4)

dat <- tidyr::separate_rows(caers_raw, SYM_One.Row.Coded.Symptoms, sep = ", ")
dat[1:4, c("RA_Report..", "PRI_Reported.Brand.Product.Name", 
           "SYM_One.Row.Coded.Symptoms")]

dat$id   <- dat$RA_Report..
dat$var1 <- dat$PRI_Reported.Brand.Product.Name
dat$var2 <- dat$SYM_One.Row.Coded.Symptoms

dat$strat_gender <- dat$CI_Gender
table(dat$strat_gender, useNA = "always")

data = processRaw(data = dat, stratify = FALSE, zeroes = FALSE)

# 
setwd("../data/jportanova/FAERS/ascii")
dat <- read_csv("assoc_data.csv") # the id's on this are wrong, but do for practice
dat$id <- dat$X1
dat$var1 <- dat$drugname
dat$var2 <- dat$pt

a <- dat[complete.cases(dat),] # 189,424,164 and only 143 more
b <- a[1:100000,]
data = processRaw(data = b, stratify = FALSE, zeroes = FALSE)

squashed <- squashData(data, bin_size = 2, keep_pts = 0)
squashed <- squashData(squashed, count = 2, bin_size = 12)
hyperEM_ests <- hyperEM(squashed, theta_init_vec = c(1, 1, 2, 2, .3),
                        conf_int = TRUE, track = TRUE)
library(ggplot2)
library(tidyr)
pdat <- gather(hyperEM_ests$tracking, key = "metric", value = "value", logL:P)
pdat$metric <- factor(pdat$metric, levels = unique(pdat$metric), ordered = TRUE)
ggplot(pdat, aes(x = iter, y = value)) +
  geom_line(size = 1.1, col = "blue") +
  facet_grid(metric ~ ., scales = "free") +
  ggtitle("Convergence Assessment",
          sub = "Dashed red line indicates accelerated estimate") +
  labs(x = "Iteration Count", y = "Estimate") +
  geom_vline(xintercept = c(100, 200), size = 1, linetype = 2, col = "red")


# 
setwd("../data/jportanova/FAERS/ascii")
dat <- read_csv("ebgm_data.csv") # the id's on this are wrong, but do for practice
dat$id <- dat$primaryid
dat$var1 <- dat$drugname
dat$var2 <- dat$pt

a <- dat[complete.cases(dat),] # 189,424,164 and only 143 more
a <- a[1:10000,]
data = processRaw(data = a, stratify = FALSE, zeroes = FALSE)

squashed <- squashData(data, bin_size = 5, keep_pts = 0)
squashed <- squashData(squashed, count = 2, bin_size = 12)
hyperEM_ests <- hyperEM(squashed, theta_init_vec = c(1, 1, 2, 2, .3),
                        conf_int = TRUE, track = TRUE)
library(ggplot2)
library(tidyr)
pdat <- gather(hyperEM_ests$tracking, key = "metric", value = "value", logL:P)
pdat$metric <- factor(pdat$metric, levels = unique(pdat$metric), ordered = TRUE)
ggplot(pdat, aes(x = iter, y = value)) +
  geom_line(size = 1.1, col = "blue") +
  facet_grid(metric ~ ., scales = "free") +
  ggtitle("Convergence Assessment",
          sub = "Dashed red line indicates accelerated estimate") +
  labs(x = "Iteration Count", y = "Estimate") +
  geom_vline(xintercept = c(100, 200), size = 1, linetype = 2, col = "red")


