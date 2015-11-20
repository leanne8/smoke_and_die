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

download.file(url )
