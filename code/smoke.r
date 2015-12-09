## ---- chunk1 ----
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/rawdata/smoke_df.csv", 
              destfile = "../rawdata/smoke_df.csv")

download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/rawdata/lung_cancer_df.csv",
              destfile = "../rawdata/lung_cancer_df.csv")

## ---- chunk2 ----
smoke_df <- read.csv(file = "../rawdata/smoke_df.csv", header = TRUE, stringsAsFactors = FALSE)
lung_cancer_df <- read.csv(file = "../rawdata/lung_cancer_df.csv", header = TRUE, stringsAsFactors = FALSE)

smoke_df <- smoke_df[ , c(1, 2)]
colnames(smoke_df) <- c("state", "smokers(%)")

lung_cancer_df <- lung_cancer_df[ , c(1, 3)]
lung_cancer_df[ , 2] <- as.numeric(lung_cancer_df[ , 2])
lung_cancer_df[lung_cancer_df$state == "NV", 2] <- round((1683/2685000) * 100000, digits = 1)
lung_cancer_df[ , 2] <- lung_cancer_df[ , 2] / 1000
colnames(lung_cancer_df) <- c("state", "cancer(%)")

write.table(smoke_df, file = "../data/smoke_cdf.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(lung_cancer_df, file = "../data/lung_cancer_cdf.csv", sep = ",", row.names = FALSE, col.names = TRUE)

## ---- chunk3 ----
smoke_cdf <- read.csv(file = "../data/smoke_cdf.csv", header = TRUE, stringsAsFactors = FALSE)
str(smoke_cdf)
summary(smoke_cdf)
head(smoke_cdf)
tail(smoke_cdf)

## ---- chunk4 ----
lung_cancer_cdf <- read.csv(file = "../data/lung_cancer_cdf.csv", header = TRUE, stringsAsFactors = FALSE)
str(lung_cancer_cdf)
summary(lung_cancer_cdf)
head(lung_cancer_cdf)
tail(lung_cancer_cdf)

## ---- chunk5 ----
smoke_cancer_df <- cbind(smoke_cdf, lung_cancer_cdf)
smoke_cancer_df[ , 3] <- NULL 

write.table(smoke_cancer_df, file = "../data/smoke_cancer_cdf.csv", row.names = FALSE, col.names = TRUE, sep = ",")

## ---- chunk6 ----
smoke_cancer_cdf <- read.csv(file = "../data/smoke_cancer_cdf.csv", header = TRUE, stringsAsFactors = FALSE)
smoke_cancer_cdf$state[which.max(smoke_cancer_cdf$smokers...)]

## ---- chunk7 ----
smoke_cancer_cdf$state[which.min(smoke_cancer_cdf$smokers...)]

## ---- chunk8 ----
smoke_cancer_cdf$state[which.max(smoke_cancer_cdf$cancer...)]

## ---- chunk9 ----
smoke_cancer_cdf$state[which.min(smoke_cancer_cdf$cancer...)]

## ---- chunk10 ----
smoker_perc <- smoke_cancer_cdf$smokers...
names(smoker_perc) <- smoke_cdf[ , 1]
barplot(sort(smoker_perc), main = "Smoker Population in USA by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,20,147, maxColorValue = 255),rgb(30,144,254, maxColorValue = 255),
              rgb(254,215,0, maxColorValue = 255), rgb(0,254,0, maxColorValue = 255)))

## ---- chunk11 ----
lung_cancer_perc <- smoke_cancer_cdf$cancer...
names(lung_cancer_perc) <- smoke_cdf[ , 1]
barplot(sort(lung_cancer_perc), 
        main = "Lung Cancer Patients in USA by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,255,200, maxColorValue = 255),rgb(221,160, 221, maxColorValue = 255),
              rgb(255,250,205, maxColorValue = 255), rgb(230,230,250, maxColorValue = 255)))

## ---- chunk12 ----
ggplot(smoke_cancer_cdf) +
  geom_point(aes(x = names(smoker_perc), y = smoke_cancer_cdf$smokers..., col = "red")) +
  geom_line(aes(x = names(smoker_perc), y = smoke_cancer_cdf$smokers..., col = "red", group = 1)) +
  geom_point(aes(x = names(smoker_perc), y = smoke_cancer_cdf$cancer... * 1000, col = "green")) +
  geom_line(aes(x = names(smoker_perc), y = smoke_cancer_cdf$cancer... * 1000, col = "green", group = 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("#AA3939", "#73DB1D"), name = "type", labels = c("lung cancer patients per 100000", "percentage of smokers")) +
  theme(legend.position = "top") +
  xlab("States in the US") +
  ylab("")

## ---- chunk13 ----
smoke_cancer_fit <- lm(smoke_cancer_cdf$cancer... ~ smoke_cancer_cdf$smokers...)
smoke_cancer_fit

plot(smoke_cancer_cdf$smokers..., smoke_cancer_cdf$cancer..., 
     pch = 16, cex = 1.3, col = "#063BB6",
     main = "cigarette smoking population -vs- lung cancer patients", las = 2, 
     xlab = "number of cigarrete smokers per 100,000", ylab = "number of lung cancer patients per 100,000")
abline(smoke_cancer_fit, col = "#FF0000", lwd = 2)

## ---- chunk14 ----
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/rawdata/lung_cancer_male.txt",
              destfile = "../rawdata/lung_cancer_male.csv")

download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/rawdata/lung_cacner_%20female.txt",
              destfile = "../rawdata/lung_cancer_female.csv")

## ---- chunk15 ----
male_df <- read.csv("../rawdata/lung_cancer_male.csv", header = TRUE, sep = "\t", 
                    col.names = c("X", "male_age", "male_all", "white", "black", "asian", 
                                  "native_american", "hispanic"), stringsAsFactors = FALSE)
male_df[ , 1] <- NULL

female_df <- read.csv("../rawdata/lung_cancer_female.csv", header = TRUE, sep = "\t",
                      col.names = c("X", "female_age", "female_all", "white", "black", "asian", 
                                    "native_american", "hispanic"), stringsAsFactors = FALSE)
female_df[ , 1] <- NULL

male_df[male_df == "~"] <- NA
female_df[female_df == "~"] <- NA

for (i in 2:length(colnames(male_df))) {
  male_df[ , i] <- as.numeric(male_df[ , i])
  female_df[ , i] <- as.numeric(female_df[ , i])
}

write.table(male_df, file = "../data/male_cdf.csv", row.names = FALSE, col.names = TRUE, sep = ",")
write.table(female_df, file = "../data/female_cdf.csv", row.names = FALSE, col.names = TRUE, sep = ",")

## ---- chunk16 ----
male_cdf <- read.csv(file = "../data/male_cdf.csv", header = TRUE, stringsAsFactors = FALSE)
female_cdf <- read.csv(file = "../data/female_cdf.csv", header = TRUE, stringsAsFactors = FALSE)

male_fifty_df <- male_cdf[12:19, ]

total_rate_male <- c()
for (i in 1:5){
  total_rate_male[i] <- round(sum(male_fifty_df[ , (i+2)]) / 8, digit = 1)  
}
names(total_rate_male) <- colnames(male_fifty_df)[3:7]

female_fortyfive_df <- female_cdf[11:19, ]

total_rate_female <- c()
for (i in 1:5) {
  total_rate_female[i] <- round(sum(female_fortyfive_df[ , (i+2)]) / 9, digit = 1)
}
names(total_rate_female) <- colnames(female_fortyfive_df)[3:7]

## ---- chunk17 ----
total_rate_combined <- cbind("Male" = total_rate_male, "Female" = total_rate_female)
barplot(total_rate_combined, col = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), 
        main = "Lung Cancer Patients by Race", ylab = "frequency per 100,000", beside = TRUE)
legend("topright", 
       title = "Race",
       legend = c("white", "black", "native american", "asian", "hispanic"), 
       fill = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), cex = 0.8)

## ---- chunk18 ----
both_gender_df <- cbind(male_cdf, female_cdf)
both_gender_df <- both_gender_df[-c(1:5), ]
ggplot (both_gender_df) +
  geom_bar(aes(x = both_gender_df$male_age, y = both_gender_df$male_all), 
           stat = "identity", col= "#0033CC") +
  geom_bar(aes(x = both_gender_df$female_age, y = both_gender_df$female_all), 
           stat = "identity", col= "#FF6699") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Age") + ylab("lung cancer patients per 100,000") +
  ggtitle("Lung Cancer Patients by Age")

## ---- chunk19 ----
png(filename = "../images/smokepop.png")
barplot(sort(smoker_perc), main = "Smoker Population in USA by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,20,147, maxColorValue = 255),rgb(30,144,254, maxColorValue = 255),
              rgb(254,215,0, maxColorValue = 255), rgb(0,254,0, maxColorValue = 255)))
dev.off()

png(filename = "../images/lungcancer.png")
barplot(sort(lung_cancer_perc), 
        main = "Lung Cancer Patients in USA by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,255,200, maxColorValue = 255),rgb(221,160, 221, maxColorValue = 255),
              rgb(255,250,205, maxColorValue = 255), rgb(230,230,250, maxColorValue = 255)))
dev.off()

png(filename = "../images/comp.png")
figure1 <- ggplot(smoke_cancer_cdf) +
  geom_point(aes(x = names(smoker_perc), y = smoke_cancer_cdf$smokers..., col = "red")) +
  geom_line(aes(x = names(smoker_perc), y = smoke_cancer_cdf$smokers..., col = "red", group = 1)) +
  geom_point(aes(x = names(smoker_perc), y = smoke_cancer_cdf$cancer... * 1000, col = "green")) +
  geom_line(aes(x = names(smoker_perc), y = smoke_cancer_cdf$cancer... * 1000, col = "green", group = 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("#AA3939", "#73DB1D"), name = "type", labels = c("lung cancer patients per 100000", "percentage of smokers")) +
  theme(legend.position = "top") +
  xlab("States in the US") +
  ylab("")
plot(figure1)
dev.off()

png(filename = "../images/reg.png")
plot(smoke_cancer_cdf$smokers..., smoke_cancer_cdf$cancer..., 
     pch = 16, cex = 1.3, col = "#063BB6",
     main = "cigartte smoking population -vs- lung cancer patients", las = 2, 
     xlab = "number of cigarette smokers per 100,000", ylab = "number of lung cancer patients per 100,000") +
  abline(smoke_cancer_fit, col = "#FF0000", lwd = 2)
dev.off()

png(filename = "../images/race.png")
barplot(total_rate_combined, col = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), 
        main = "Lung Cancer Patients by Race", ylab = "frequency per 100,000", beside = TRUE)
legend("topright", 
       title = "Race",
       legend = c("white", "black", "native american", "asian", "hispanic"), 
       fill = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), cex = 0.8)
dev.off()

png(filename = "../images/age.png")
figure2 <- ggplot (both_gender_df) +
  geom_bar(aes(x = both_gender_df$male_age, y = both_gender_df$male_all), 
           stat = "identity", col= "#0033CC") +
  geom_bar(aes(x = both_gender_df$female_age, y = both_gender_df$female_all), 
           stat = "identity", col= "#FF6699") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Age") + ylab("lung cancer patients per 100,000") +
  ggtitle("Lung Cancer Patients by Age")
plot(figure2)
dev.off()

# -------------- The End -----------------

## We first look at the population of cigarette smokers and those diagnosed with lung cancer in each state. 

# Downloading the necessary data
# Data on smoker population in percentage
library(readr)
library(ggplot2)

#### Include this in the code but do not run it!
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/rawdata/smoke_df.csv", 
              destfile = "../rawdata/smoke_df.csv")

smoke_df <- read.csv(file = "../rawdata/smoke_df.csv", header = TRUE, stringsAsFactors = FALSE)

# Data on lung cancer patients

#### Include this in the code but do not run it!
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/rawdata/lung_cancer_df.csv",
              destfile = "../rawdata/lung_cancer_df.csv")

lung_cancer_df <- read.csv(file = "../rawdata/lung_cancer_df.csv", header = TRUE, stringsAsFactors = FALSE)

# Extracting only the necessary data from 'smoke_df' data frame
smoke_df <- smoke_df[ , c(1, 2)]
colnames(smoke_df) <- c("state", "cigarette smokers(%)")

# Extracting only the necessary data from 'lung_cancer_df' data frame
lung_cancer_df <- lung_cancer_df[ , c(1, 3)]
colnames(lung_cancer_df) <- c("state", "lung cancer patients(per 100,000)")
lung_cancer_df[ , 2] <- as.numeric(lung_cancer_df[ , 2])

# since the data doesn't have Neveda data, we have to manually look it up in the Neveda cancer webpage
# source from : Lung cancer in Neveda
# In 2009, there were 1,683 people diagnosed with lung cancer in Neveda
lung_cancer_df[lung_cancer_df$state == "NV", 2] <- round((1683/2685000) * 100000, digits = 1)

# Changing the number of patients with lung cancer(per 100,000) into the percentage of lung cancer patients
lung_cancer_df[ , 2] <- lung_cancer_df[ , 2] / 1000
colnames(lung_cancer_df) <- c("state", "lung cancer patients(%)")

# Checking smoke_df data frame
str(smoke_df)
summary(smoke_df)
head(smoke_df)
tail(smoke_df)

# Checking lung_cancer_df
str(lung_cancer_df)
summary(lung_cancer_df)
head(lung_cancer_df)
tail(lung_cancer_df)

# Merging the two data frames
smoke_cancer_df <- cbind(smoke_df, lung_cancer_df)
smoke_cancer_df[ , 3] <- NULL 

# State with highest percentage of smokers
smoke_cancer_df$state[which.max(smoke_cancer_df$`cigarette smokers(%)`)]

# State with lowest percentage of smokers
smoke_cancer_df$state[which.min(smoke_cancer_df$`cigarette smokers(%)`)]

# State with highest percentage of lung cancer patients
smoke_cancer_df$state[which.max(smoke_cancer_df$`lung cancer patients(%)`)]

# State with lowest percentage of lung cancer patients
smoke_cancer_df$state[which.min(smoke_cancer_df$`lung cancer patients(%)`)]

# Visual representation of the percentage of smokers in each state
smoker_perc <- smoke_cancer_df$`cigarette smokers(%)`
names(smoker_perc) <- smoke_df[ , 1]
barplot(sort(smoker_perc), main = "Smoker Population in USA by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,20,147, maxColorValue = 255),rgb(30,144,254, maxColorValue = 255),
              rgb(254,215,0, maxColorValue = 255), rgb(0,254,0, maxColorValue = 255)))

# Visual representation of the percentage of people with lung cancer in each state
lung_cancer_perc <- smoke_cancer_df$`lung cancer patients(%)`
names(lung_cancer_perc) <- smoke_df[ , 1]
barplot(sort(lung_cancer_perc), 
        main = "Lung Cancer Patients in USA by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,255,200, maxColorValue = 255),rgb(221,160, 221, maxColorValue = 255),
              rgb(255,250,205, maxColorValue = 255), rgb(230,230,250, maxColorValue = 255)))

# Comparing the percentage of smokers and number of patients with lung cancer(per 100,000) in each state
ggplot(smoke_cancer_df) +
  geom_point(aes(x = names(smoker_perc), y = smoke_cancer_df$`cigarette smokers(%)`, col = "red")) +
  geom_line(aes(x = names(smoker_perc), y = smoke_cancer_df$`cigarette smokers(%)`, col = "red", group = 1)) +
  geom_point(aes(x = names(smoker_perc), y = smoke_cancer_df$`lung cancer patients(%)` * 1000, col = "green")) +
  geom_line(aes(x = names(smoker_perc), y = smoke_cancer_df$`lung cancer patients(%)` * 1000, col = "green", group = 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("#AA3939", "#73DB1D"), name = "type", labels = c("percentage of smokers", "number of lung cancer patients per 100000")) +
  theme(legend.position = "top") +
  xlab("States in the US") +
  ylab("")

# Linear Model (Regression Line): smoking population vs lung cancer patients
smoke_cancer_fit <- lm(smoke_cancer_df$`lung cancer patients(%)` ~ smoke_cancer_df$`cigarette smokers(%)`)
smoke_cancer_fit

plot(smoke_cancer_df$`cigarette smokers(%)`, smoke_cancer_df$`lung cancer patients(%)`, 
     pch = 16, cex = 1.3, col = "#063BB6",
     main = "cigartte smoking population -vs- lung cancer patients", las = 2, 
     xlab = "number of cigarrete smokers per 100,000", ylab = "number of lung cancer patients per 100,000")
abline(smoke_cancer_fit, col = "#FF0000", lwd = 2)

# We see that in general, states with higher percentage of smokers have greater number of lung cancer patients

## Secondly, we look at the relationship between lung cancer and one's race with gender. 
## In addition, we look at the relationship between lung cancer and one's age with gender.

# Downloading necessary files

#### Include this in the code but do not run it!  
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/rawdata/lung_cancer_male.txt",
              destfile = "../rawdata/lung_cancer_male.csv")

male_df <- read.csv("../rawdata/lung_cancer_male.csv", header = TRUE, sep = "\t", 
                    col.names = c("X", "male_age", "male_all", "white", "black", "asian", 
                                 "native_american", "hispanic"), stringsAsFactors = FALSE)
male_df[ , 1] <- NULL

#### Include this in the code but do not run it!
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/rawdata/lung_cacner_%20female.txt",
              destfile = "../rawdata/lung_cancer_female.csv")

female_df <- read.csv("../rawdata/lung_cancer_female.csv", header = TRUE, sep = "\t",
                      col.names = c("X", "female_age", "female_all", "white", "black", "asian", 
                                    "native_american", "hispanic"), stringsAsFactors = FALSE)
female_df[ , 1] <- NULL

# Assigning NA to values that are not available in the data frames
male_df[male_df == "~"] <- NA
female_df[female_df == "~"] <- NA

# Changing the values in the data frames into numbers
for (i in 2:length(colnames(male_df))) {
  male_df[ , i] <- as.numeric(male_df[ , i])
  female_df[ , i] <- as.numeric(female_df[ , i])
}

# Comparing rate of lung cancer in patients over 50 years old by gender
male_fifty_df <- male_df[12:19, ]

total_rate_male <- c()
for (i in 1:5){
  total_rate_male[i] <- round(sum(male_fifty_df[ , (i+2)]) / 8, digit = 1)  
}
names(total_rate_male) <- colnames(male_fifty_df)[3:7]

female_fortyfive_df <- female_df[11:19, ]

total_rate_female <- c()
for (i in 1:5) {
  total_rate_female[i] <- round(sum(female_fortyfive_df[ , (i+2)]) / 9, digit = 1)
}
names(total_rate_female) <- colnames(female_fortyfive_df)[3:7]

# Visual representation of the rate of lung cancer patients by race and gender
total_rate_combined <- cbind("Male" = total_rate_male, "Female" = total_rate_female)
barplot(total_rate_combined, col = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), 
        main = "Lung Cancer Patients by Race", ylab = "frequency per 100,000", beside = TRUE)
legend("topright", 
       title = "Race",
       legend = c("white", "black", "native american", "asian", "hispanic"), 
       fill = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), cex = 0.8)

# Visual representation of the rate of lung cancer patients by ages
both_gender_df <- cbind(male_df, female_df)
both_gender_df <- both_gender_df[-c(1:5), ]
ggplot (both_gender_df) +
  geom_bar(aes(x = both_gender_df$male_age, y = both_gender_df$male_all), 
           stat = "identity", col= "#0033CC") +
  geom_bar(aes(x = both_gender_df$female_age, y = both_gender_df$female_all), 
           stat = "identity", col= "#FF6699") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Age") + ylab("Number of patients with lung cancer per 100,000") +
  ggtitle("Lung Cancer Patients by Age")
