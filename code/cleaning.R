## ---- chunk2 ----
smoke_df <- read.csv(file = "../rawdata/smoke_df.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
lung_cancer_df <- read.csv(file = "../rawdata/lung_cancer_df.csv", 
                           header = TRUE, stringsAsFactors = FALSE)

smoke_df <- smoke_df[ , c(1, 2)]
colnames(smoke_df) <- c("state", "smokers(%)")

lung_cancer_df <- lung_cancer_df[ , c(1, 3)]
colnames(lung_cancer_df) <- c("state", "cancer(%)")
lung_cancer_df[ , 2] <- as.numeric(lung_cancer_df[ , 2])
lung_cancer_df[lung_cancer_df$state == "NV", 2] <- 
  round((1683/2685000) * 100000, digits = 1)
lung_cancer_df[ , 2] <- lung_cancer_df[ , 2] / 1000

smoke_cancer_df <- cbind(smoke_df, lung_cancer_df)
smoke_cancer_df[ , 3] <- NULL 

write.table(smoke_df, file = "../data/smoke_cdf.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)
write.table(lung_cancer_df, file = "../data/lung_cancer_cdf.csv", sep = ",",
            row.names = FALSE, col.names = TRUE)
write.table(smoke_cancer_df, file = "../data/smoke_cancer_cdf.csv",
            row.names = FALSE, col.names = TRUE, sep = ",")

## ---- chunk14 ----
male_df <- read.csv("../rawdata/lung_cancer_male.csv", header = TRUE, 
                    sep = "\t", col.names = c("X", "male_age", "male_all", 
                                              "white", "black", "asian", 
                                              "native_american", "hispanic"), 
                    stringsAsFactors = FALSE)
male_df[ , 1] <- NULL

female_df <- read.csv("../rawdata/lung_cancer_female.csv", header = TRUE, 
                      sep = "\t", col.names = c("X", "female_age", 
                                                "female_all", "white", "black", 
                                                "asian", "native_american", 
                                                "hispanic"), 
                      stringsAsFactors = FALSE)
female_df[ , 1] <- NULL

male_df[male_df == "~"] <- NA
female_df[female_df == "~"] <- NA

for (i in 2:length(colnames(male_df))) {
  male_df[ , i] <- as.numeric(male_df[ , i])
  female_df[ , i] <- as.numeric(female_df[ , i])
}

write.table(male_df, file = "../data/male_cdf.csv", 
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(female_df, file = "../data/female_cdf.csv", 
            row.names = FALSE, col.names = TRUE, sep = ",")

## ---- chunk15 ----
male_cdf <- read.csv(file = "../data/male_cdf.csv", 
                     header = TRUE, stringsAsFactors = FALSE)
female_cdf <- read.csv(file = "../data/female_cdf.csv", 
                       header = TRUE, stringsAsFactors = FALSE)

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
