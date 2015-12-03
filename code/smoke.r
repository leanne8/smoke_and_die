# =====================================
# Stat 133 Final Project
# Project: Smoke and Die
# By: Leanne Lee and Tony Son
# =====================================

## We first look at the population of cigarette smokers and those diagnosed with lung cancer in each state. 

# Downloading the necessary data
# Data on smoker population in percentage
library(readr)
library(ggplot2)
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/smoke_df.csv", 
              destfile = "../rawdata/smoke_df.csv")
smoke_df <- read.csv(file = "../rawdata/smoke_df.csv", header = TRUE, stringsAsFactors = FALSE)

# Data on lung cancer patients
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/lung_cancer_df.csv",
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
barplot(sort(smoker_perc), main = "Smoking Population in 51 States across the United States", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,20,147, maxColorValue = 255),rgb(30,144,254, maxColorValue = 255),
              rgb(254,215,0, maxColorValue = 255), rgb(0,254,0, maxColorValue = 255)))

# Visual representation of the percentage of people with lung cancer in each state
lung_cancer_perc <- smoke_cancer_df$`lung cancer patients(%)`
names(lung_cancer_perc) <- smoke_df[ , 1]
barplot(sort(lung_cancer_perc), 
        main = "Lung Cancer Patients in 51 States across the United States", 
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
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/lung_cancer_male.txt",
              destfile = "../rawdata/lung_cancer_male.csv")
male_df <- read.csv("../rawdata/lung_cancer_male.csv", header = TRUE, sep = "\t", 
                    col.names = c("X", "male_age", "male_all", "white", "black", "asian", 
                                 "native_american", "hispanic"), stringsAsFactors = FALSE)
male_df[ , 1] <- NULL

download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/lung_cacner_%20female.txt",
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
       fill = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"))

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
  ggtitle("Do older people have a high schance of getting lung cancer?")
