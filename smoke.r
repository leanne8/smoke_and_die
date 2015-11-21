#download the data
library(readr)
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/smoking%20population%20in%20diff%20states.csv", 
              destfile = "smoke_df.csv")
smoke_df <- read.csv(file = "smoke_df.csv", header = TRUE, stringsAsFactors = FALSE)

#The state where the maximum population smoke
smoke_df$LocationDesc[which.max(smoke_df$Data_Value)]
#The state where the minimum population smoke
smoke_df$LocationDesc[which.min(smoke_df$Data_Value)]

#extract the first two columns and plot 
smoke_pop_percentage <- smoke_df[ , 2]
names(smoke_pop_percentage) <- smoke_df[ , 1]
barplot(smoke_pop_percentage, main="Smoking Population in 51 States across the United States", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,20,147, maxColorValue = 255),rgb(30,144,254, maxColorValue = 255),
        rgb(254,215,0, maxColorValue = 255), rgb(0,254,0, maxColorValue = 255)))

#download lung cancer file
download.file(url = "https://raw.githubusercontent.com/leanne8/smoke_and_die/master/lung_map_incidence.csv",
              destfile = "lung_cancer_df.csv")
lung_cancer_df <- read.csv(file = "lung_cancer_df.csv", header = TRUE, stringsAsFactors = FALSE)

#since the data doesn't have Neveda data, we have to manually look it up in the Neveda cancer webpage
#source from : Lung cancer in Neveda
#In 2009, there is 1,683 people diagnoised with lung cancer in Neveda
(1683/2685000) * 100000

#replacing the missing value in Neveda
lung_cancer_df$Rate <- gsub("Data not available", 62.7, lung_cancer_df$Rate)

#changing the rate into percentage 
lung_cancer_df$Percent <- as.numeric(lung_cancer_df$Rate) / 1000 

#joint the two tables together
smoke_df
lung_cancer_df
smoke2_df <- smoke_df[ , c(1, 2)]
lung_cancer2_df <- lung_cancer_df[ , c(3, 4)]
lung_and_smoke_df <- cbind(smoke2_df, lung_cancer2_df)
col_heading <- c("state", "percentage of smokers","lung cancer rate" , 
                 "percentage of lung cancer")
names(lung_and_smoke_df) <- col_heading

#plot lung cacner in different states
lung_pop_percentage <- lung_and_smoke_df[ ,4]
names(lung_pop_percentage) <- lung_and_smoke_df[ , 1]
barplot(lung_pop_percentage, 
        main="Lung Cancer in 51 States across the United States", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,255,200, maxColorValue = 255),rgb(221,160, 221, maxColorValue = 255),
              rgb(255,250,205, maxColorValue = 255), rgb(230,230,250, maxColorValue = 255)))




