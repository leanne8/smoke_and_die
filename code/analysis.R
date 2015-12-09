## ---- chunk3 ----
smoke_cdf <- read.csv(file = "../data/smoke_cdf.csv", 
                      header = TRUE, stringsAsFactors = FALSE)
str(smoke_cdf)
summary(smoke_cdf)

## ---- chunk4 ----
lung_cancer_cdf <- read.csv(file = "../data/lung_cancer_cdf.csv",
                            header = TRUE, stringsAsFactors = FALSE)
str(lung_cancer_cdf)
summary(lung_cancer_cdf)

## ---- chunk5 ----
smoke_cancer_cdf <- read.csv(file = "../data/smoke_cancer_cdf.csv",
                             header = TRUE, stringsAsFactors = FALSE)
smoke_cancer_cdf$state[which.max(smoke_cancer_cdf$smokers...)]

## ---- chunk6 ----
smoke_cancer_cdf$state[which.min(smoke_cancer_cdf$smokers...)]

## ---- chunk7 ----
smoke_cancer_cdf$state[which.max(smoke_cancer_cdf$cancer...)]

## ---- chunk8 ----
smoke_cancer_cdf$state[which.min(smoke_cancer_cdf$cancer...)]

## ---- chunk9 ----
smoker_perc <- smoke_cancer_cdf$smokers...
names(smoker_perc) <- smoke_cdf[ , 1]
barplot(sort(smoker_perc), main = "Smoker Population in the US by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,20,147, maxColorValue = 255),
              rgb(30,144,254, maxColorValue = 255),
              rgb(254,215,0, maxColorValue = 255), 
              rgb(0,254,0, maxColorValue = 255)))

## ---- chunk10 ----
lung_cancer_perc <- smoke_cancer_cdf$cancer...
names(lung_cancer_perc) <- smoke_cdf[ , 1]
barplot(sort(lung_cancer_perc), 
        main = "Lung Cancer Patients in the US by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of lung cancer patients",
        col=c(rgb(255,255,200, maxColorValue = 255),
              rgb(221,160, 221, maxColorValue = 255),
              rgb(255,250,205, maxColorValue = 255),
              rgb(230,230,250, maxColorValue = 255)))

## ---- chunk11 ----
ggplot(smoke_cancer_cdf) +
  geom_point(aes(x = names(smoker_perc), 
                 y = smoke_cancer_cdf$smokers..., col = "red")) +
  geom_line(aes(x = names(smoker_perc), 
                y = smoke_cancer_cdf$smokers..., col = "red", group = 1)) +
  geom_point(aes(x = names(smoker_perc), 
                 y = smoke_cancer_cdf$cancer... * 1000, col = "green")) +
  geom_line(aes(x = names(smoker_perc), 
                y = smoke_cancer_cdf$cancer... * 1000, col = "green", group = 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("#AA3939", "#73DB1D"), name = "type", 
                     labels = c("lung cancer patients per 100000", "percentage of smokers")) +
  theme(legend.position = "top") +
  xlab("States in the US") +
  ylab("")

## ---- chunk12 ----
smoke_cancer_fit <- lm(smoke_cancer_cdf$cancer... ~ smoke_cancer_cdf$smokers...)
plot(smoke_cancer_cdf$smokers..., smoke_cancer_cdf$cancer..., 
     pch = 16, cex = 1.3, col = "#063BB6",
     main = "cigarette smoking population -vs- lung cancer patients", las = 1, 
     xlab = "number of cigarrete smokers per 100,000", 
     ylab = "number of lung cancer patients per 100,000")
abline(smoke_cancer_fit, col = "#FF0000", lwd = 2)

## ---- chunk16 ----
total_rate_combined <- cbind("Male" = total_rate_male, 
                             "Female" = total_rate_female)
barplot(total_rate_combined, 
        col = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), 
        main = "Lung Cancer Patients by Race", las = 1,
        ylab = "frequency per 100,000", beside = TRUE)
legend("topright", 
       title = "Race",
       legend = c("white", "black", "native american", "asian", "hispanic"), 
       fill = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), 
       cex = 0.8)

## ---- chunk17 ----
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

## ---- chunk18 ----
png(filename = "../images/smokepop.png")
barplot(sort(smoker_perc), main = "Smoker Population in the US by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of smokers",
        col=c(rgb(255,20,147, maxColorValue = 255),
              rgb(30,144,254, maxColorValue = 255),
              rgb(254,215,0, maxColorValue = 255), 
              rgb(0,254,0, maxColorValue = 255)))
dev.off()

png(filename = "../images/lungcancer.png")
barplot(sort(lung_cancer_perc), 
        main = "Lung Cancer Patients in the US by State", 
        cex.names = 0.6, las = 2, ylab = "percentage of lung cancer patients",
        col=c(rgb(255,255,200, maxColorValue = 255),
              rgb(221,160, 221, maxColorValue = 255),
              rgb(255,250,205, maxColorValue = 255), 
              rgb(230,230,250, maxColorValue = 255)))
dev.off()

png(filename = "../images/comp.png")
figure1 <- ggplot(smoke_cancer_cdf) +
  geom_point(aes(x = names(smoker_perc), 
                 y = smoke_cancer_cdf$smokers..., col = "red")) +
  geom_line(aes(x = names(smoker_perc), 
                y = smoke_cancer_cdf$smokers..., col = "red", group = 1)) +
  geom_point(aes(x = names(smoker_perc),
                 y = smoke_cancer_cdf$cancer... * 1000, col = "green")) +
  geom_line(aes(x = names(smoker_perc), 
                y = smoke_cancer_cdf$cancer... * 1000, col = "green", group = 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("#AA3939", "#73DB1D"), name = "type", 
                     labels = c("lung cancer patients per 100000", 
                                "percentage of smokers")) +
  theme(legend.position = "top") +
  xlab("States in the US") +
  ylab("")
plot(figure1)
dev.off()

png(filename = "../images/reg.png")
plot(smoke_cancer_cdf$smokers..., smoke_cancer_cdf$cancer..., 
     pch = 16, cex = 1.3, col = "#063BB6",
     main = "cigartte smoking population -vs- lung cancer patients", las = 2, 
     xlab = "number of cigarette smokers per 100,000", 
     ylab = "number of lung cancer patients per 100,000") +
  abline(smoke_cancer_fit, col = "#FF0000", lwd = 2)
dev.off()

png(filename = "../images/race.png")
barplot(total_rate_combined, col = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), 
        main = "Lung Cancer Patients by Race", 
        ylab = "frequency per 100,000", beside = TRUE)
legend("topright", 
       title = "Race",
       legend = c("white", "black", "native american", "asian", "hispanic"), 
       fill = c("#FFFFFF", "#000000", "#984126", "#FFFF00", "#E5A470"), 
       cex = 0.8)
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
