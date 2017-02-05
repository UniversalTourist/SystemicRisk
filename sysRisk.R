# Quantile Regression & VaR - CoVaR Applications on Turkey's Banks 

install.packages("PerformanceAnalytics")
install.packages('readxl')
install.packages("xts")
install.packages("zoo")
install.packages("quantmod")
install.packages('lubridate')
install.packages('broom')
install.packages('imputeTS')
install.packages('reshape2')
install.packages("quantreg")

library(PerformanceAnalytics)
library(readxl)
library(xts)
library(zoo)
library(quantmod)
library(lubridate)
library(broom)
library(imputeTS)
library(reshape2)
library(quantreg)

setwd('/Users/hazelkavili/Desktop')

#Read data and set date/time column as row names
banks <- read_excel("~/banks.xlsx")
View(banks2)
banks <- banks[-1,]
banks2 <- data.frame(banks[,-1], row.names = banks$tarih)


#Impute missing values for bist100 and volatilie
na.kalman(bist100$imkb100) 
na.interpolation(bist100$volatilite, option = "spline")
na.interpolation(banks$halkbank)
  
#Look at the data
summary(banks2)
hist(banks$isbankasi, main = "Is Bankasi icin Aktif Getiri Orani Degerlerinin Dagilimi", xlab = "Aktif Getiri Oranlari", ylab = "Frekans")
hist(banks$akbank, main = "Akbank icin Aktif Getiri Orani Degerlerinin Dagilimi", xlab = "Aktif Getiri Oranlari", ylab = "Frekans")
hist(banks$halkbank, main = "Halkbank icin Aktif Getiri Orani Degerlerinin Dagilimi", xlab = "Aktif Getiri Oranlari", ylab = "Frekans")
hist(banks$garanti, main = "Garanti icin Aktif Getiri Orani Degerlerinin Dagilimi", xlab = "Aktif Getiri Oranlari", ylab = "Frekans")
hist(banks$yapikredi, main = "Yapi Kredi icin Aktif Getiri Orani Degerlerinin Dagilimi", xlab = "Aktif Getiri Oranlari", ylab = "Frekans")

# Weekly plots of Rate of Returns
banks$tarih <- as.Date(banks$tarih, 'yyyy-mm-dd')
isbankasi_line <- banks %>% ggplot(aes(x = tarih, y = isbankasi)) + 
  geom_line() + theme_bw() + ggtitle("2010-2015 Yillari Arasinda Is Bankasi Aktiflerin Getiri Orani") + 
  labs(x = "Tarih", y = "Aktif Getiri Oranlari")


akbank_line <- banks %>% ggplot(aes(x = tarih, y = akbank)) + 
  geom_line() + theme_bw() + ggtitle("2010-2015 Yillari Arasinda Akbank Aktif Getiri Orani") + 
  labs(x = "Tarih", y = "Aktif Getiri Oranlari")

garanti_line <- banks %>% ggplot(aes(x = tarih, y = garanti)) + 
  geom_line() + theme_bw() + ggtitle("2010-2015 Yillari Arasinda Garanti Bankasi Aktif Getiri Orani") + 
  labs(x = "Tarih", y = "Aktif Getiri Oranlari")

halkbank_line <- banks %>% ggplot(aes(x = tarih, y = halkbank)) + 
  geom_line() + theme_bw() + ggtitle("2010-2015 Yillari Arasinda Halkbank Aktif Getiri Orani") + 
  labs(x = "Tarih", y = "Aktif Getiri Oranlari")

yapikredi_line <- banks %>% ggplot(aes(x = tarih, y = yapikredi)) + 
  geom_line() + theme_bw() + ggtitle("2010-2015 Yillari Arasinda Yapi Kredi Aktif Getiri Orani") + 
  labs(x = "Tarih", y = "Aktif Getiri Oranlari")


allbanks <- select(banks, -7,-8)
allbanks_melted <- melt(allbanks, id = "tarih")
allbanks_plot <- ggplot(data=allbanks_melted, aes(x=tarih, y=value, colour=variable)) + geom_line() + theme_bw() +
  ggtitle("2010-2015 Yillari Arasinda Bankalarin Aktif Getiri Oranlari") + labs(x = "Tarih", y = "Aktif Getiri Oranlari")


#Var and CVar Calculations
banks_50 <- CVaR(banks2, clean = "none", method = "historical", p = .50)
banks_95 <- CVaR(banks2, clean = "none", method = "historical", p = .95)
banks_99 <- CVaR(banks2, clean = "none", method = "historical", p = .99)

#banks_var_50 <- VaR(banks2, clean = "none", method = "historical", p = .50)
banks_var_95 <- VaR(banks2, clean = "none", method = "historical", p = .95)
banks_var_99 <- VaR(banks2, clean = "none", method = "historical", p = .99)


#Create a chart of Value-at-Risk and/or Expected Shortfall estimates by confidence interval for multiple methods.
chart.VaRSensitivity(banks2, lwd = 2, main = "Risk Confidence Sensitivity of Banks")

#table.CAMP
#table.DownsideRisk. SemiDeviation: downside volatility

