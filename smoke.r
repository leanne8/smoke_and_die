# =====================================
# Stat 133 Final Project
# Project: Smoke and Die
# By: Leanne Lee and Tony Son
# =====================================

# We first look at the population of cigarette smokers and those diagnosed with lung cancer in each state. 

# Downloading the necessary data
# Data on smoker population in percentage
library(readr)
library(ggplot2)
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/smoking%20population%20in%20diff%20states.csv", 
              destfile = "smoke_df.csv")
smoke_df <- read.csv(file = "smoke_df.csv", header = TRUE, stringsAsFactors = FALSE)

# Data on lung cancer patients
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/lung_map_incidence.csv",
              destfile = "lung_cancer_df.csv")
lung_cancer_df <- read.csv(file = "lung_cancer_df.csv", header = TRUE, stringsAsFactors = FALSE)


# Extracting only the necessary data from 'smoke_df' data frame
smoke_df <- smoke_df[ , c(1, 2)]
colnames(smoke_df) <- c("state", "cigarette smokers(%)")

# Checking smoke_df data frame
str(smoke_df)
summary(smoke_df)
head(smoke_df)
tail(smoke_df)

lung_cancer_df <- lung_cancer_df[ , c(1, 3)]
colnames(lung_cancer_df) <- c("state", "lung cancer patients(per 100,000)")
lung_cancer_df[ , 2] <- as.numeric(lung_cancer_df[ , 2])

# Extracting only the necessary data from 'lung_cancer_df' data frame
# since the data doesn't have Neveda data, we have to manually look it up in the Neveda cancer webpage
# source from : Lung cancer in Neveda
# In 2009, there were 1,683 people diagnosed with lung cancer in Neveda
lung_cancer_df[lung_cancer_df$state == "NV", 2] <- round((1683/2685000) * 100000, digits = 1)

# Changing the number of patients with lung cancer(per 100,000) into the percentage of lung cancer patients
lung_cancer_df[ , 2] <- lung_cancer_df[ , 2] / 1000
colnames(lung_cancer_df) <- c("state", "lung cancer patients(%)")

# Checking lung_cancer_df
str(lung_cancer_df)
summary(lung_cancer_df)
head(lung_cancer_df)
tail(lung_cancer_df)

# Merging the two data frames
smoke_cancer_df <- cbind(smoke_df, lung_cancer_df)
smoke_cancer_df[ , 3] <- NULL 

# state with highest percentage of smokers
smoke_cancer_df$state[which.max(smoke_cancer_df$`cigarette smokers(%)`)]

# state with lowest percentage of smokers
smoke_cancer_df$state[which.min(smoke_cancer_df$`cigarette smokers(%)`)]

# state with highest percentage of lung cancer patients
smoke_cancer_df$state[which.max(smoke_cancer_df$`lung cancer patients(%)`)]

# state with lowest percentage of lung cancer patients
smoke_cancer_df$state[which.min(smoke_cancer_df$`lung cancer patients(%)`)]

# visual representation of the percentage of smokers in each state
smoker_perc <- smoke_cancer_df$`cigarette smokers(%)`
names(smoker_perc) <- smoke_df[ , 1]
barplot(smoker_perc, main = "Smoking Population in 51 States across the United States", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,20,147, maxColorValue = 255),rgb(30,144,254, maxColorValue = 255),
              rgb(254,215,0, maxColorValue = 255), rgb(0,254,0, maxColorValue = 255)))

# visual representation of the percentage of people with lung cancer in each state
lung_cancer_perc <- smoke_cancer_df$`lung cancer patients(%)`
names(lung_cancer_perc) <- smoke_df[ , 1]
barplot(lung_cancer_perc, 
        main = "Lung Cancer Patients in 51 States across the United States", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,255,200, maxColorValue = 255),rgb(221,160, 221, maxColorValue = 255),
              rgb(255,250,205, maxColorValue = 255), rgb(230,230,250, maxColorValue = 255)))

# comparing the percentage of smokers and number of patients with lung cancer(per 100,000) in each state
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
  
# We see that in general, states with higher percentage of smokers have greater number of lung cancer patients


download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/lung_cancer_male.txt",
              destfile = "lung_cancer_male.csv")
male_df <- read.csv("lung_cancer_male.csv", header = TRUE, sep = "\t")
male_df[ , 1] <- NULL

download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/lung_cacner_%20female.txt",
              destfile = "lung_cancer_female.csv")
female_df <- read.csv("lung_cancer_female.csv", header = TRUE, sep = "\t")
female_df[ , 1] <- NULL

male_df[male_df == "~"] <- NA
female_df[female_df == "~"] <- NA
