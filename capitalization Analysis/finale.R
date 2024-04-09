install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("cowplot")
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(cowplot)

#import dataset
finale <- read_excel("C:/Users/Lenovo/Downloads/finale.xlsx", sheet = 2)
na = sum(is.na(finale))
ru =which(rowSums(is.na(finale))<10)
bank = finale[-ru,]
data = finale[ru,]

#banks by size 
total_assets <- data.frame(data[,7], data[,8], data[,9], data[,10], data[,11], data[,12])

for (i in 1:ncol(total_assets)) {
  if (any(is.na(total_assets[, i]) == T)) {
    total_assets[is.na(total_assets[, i]), i] <- total_assets[-i,]
    total_assets[, i] <- as.numeric(total_assets[, i])
  } else {
    total_assets[, i] <- as.numeric(total_assets[, i]) 
  }
}
total_assets[,7] <- rowMeans(total_assets)

q1 <- quantile(total_assets$V7, 0.25)
q2 <- quantile(total_assets$V7, 0.5)
q3 <- quantile(total_assets$V7, 0.75)
q4 <- quantile(total_assets$V7, 1)

categories <- rep(NA, nrow(total_assets))
quantiles <- c("q1", "q2", "q3", "q4")

for (i in 1:nrow(total_assets)) {
  
  if (total_assets[i, 7] <= q1){
    categories[i] <- "Small"
  } 
  if (total_assets[i, 7] > q1 & total_assets[i, 7] < q2){
    categories[i] <- "Medium"
  } 
  if (total_assets[i, 7] > q2 & total_assets[i, 7] < q3){
    categories[i] <- "Large"
  }
  if (total_assets[i, 7] >= q3){
    categories[i] <- "Huge"
  }
}
categories <- as.factor(categories)
data[,79] <- categories
data <- rename(data, size = ...79)

#profitability over countries comparing the two categories contained in the dataset 
data_country <- as.factor(data$Country)

#since the RWA data was not available we computed it by the information already provided  
RWA <- c()
for (i in 1:6){
    RWA <- (data[,36:31] * 1/data[,18:13])^-1
  }

#as for the RWA data we computed the capital adequacy ratio 
matrix <- matrix(ncol = 6, nrow = nrow(data))
CAR <- data.frame(matrix)

d <- data.frame(data[30:25])
CAR <- d/RWA
CAR <- cbind(CAR, data$Specialisation, data$Country, data$size)
CAR <- na.omit(CAR)
colnames(CAR) <- c("CAR_2016", "CAR_2017", "CAR_2018", "CAR_2019", "CAR_2020", "CAR_2021", 
                   "Specialisation", "Country", "Size")
mean_by_years <- rowMeans(CAR[,1:6])
CAR <- cbind(CAR, mean_by_years)

g <- aggregate(CAR, by = list(CAR$Country,CAR$Specialisation),FUN = mean)
g <- g[-c(9:11)]
g <- g[-1,]

#CAR plot by countries taking into account the two groups 
CAR_plot <- ggplot(g, aes(x = g[,1], y = g[,9], fill = g[,2])) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(x = "Country", y = "CAR percentage", fill = "Bank type") +
  scale_fill_manual(values = c("blue", "skyblue")) +
  geom_hline(yintercept = 8, color = "black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(CAR_plot)

#TIER over the years 
TIER1 <- data.frame(data[,48:43], data$Specialisation)
for (i in 1:ncol(TIER1)){
  if (any(is.na(TIER1[,i]) == T)){
    TIER1[is.na(TIER1[,i]), i] <- TIER1[-1,]
  }
}

#TIER1 for commercial banks 
commercial_tier = subset(TIER1, data.Specialisation == 'Commercial bank')
commercial_tier <- na.omit(commercial_tier)
colnames(commercial_tier) <- c(2016:2021)
mean_tier_c <- colMeans(commercial_tier[,1:6])
commercial_tier <- commercial_tier[,-7]
commercial_tier <- rbind(commercial_tier, mean_tier_c)
commercial_tier <- data.frame(t(commercial_tier))

#TIER1 for investment banks 
investment_tier = subset(TIER1, data.Specialisation == 'Investment bank')
investment_tier <- na.omit(investment_tier)
colnames(investment_tier) <- c(2016:2021)
mean_tier_i <- colMeans(investment_tier[,1:6])
investment_tier <- investment_tier[,-7]
investment_tier <- rbind(investment_tier, mean_tier_i)
investment_tier <- data.frame(t(investment_tier))

matrix_tier <- matrix(ncol = 3, nrow = 6)
TIER_data <- data.frame(matrix) 

TIER_data[,1] <- c(2016:2021)
TIER_data[,2] <- commercial_tier[1:6, 166]
TIER_data[,3] <- investment_tier[1:6, 22]

#average tier 1 per country
TIER_country <- data.frame(data$Specialisation, data$Country, data[,48:43])
TIER_country <- na.omit(TIER_country)
TIER_country <- cbind(TIER_country, rowMeans(TIER_country[,3:8]))
TIER_country <- TIER_country[-c(3:8)]

t <- aggregate(TIER_country, by = list(TIER_country$data.Country,TIER_country$data.Specialisation), FUN = mean)
t <- t[-c(3:4)]

TIER_country_plot <- ggplot(t, aes(x = t[,1], y = t[,3], fill = t[,2])) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("blue", "skyblue")) + 
  labs(x = "Country", y = "TIER1 ratio", fill = "Bank type") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(TIER_country_plot)



#TIER1 plot over years for the two categories 
TIER_plot <- ggplot(data = TIER_data) + 
  geom_line(aes(x = X1, y = X2, color = "Commercial")) +
  geom_line(aes(x = X1, y = X3, color = "Investment")) + 
  scale_color_manual(values = c("Commercial" = "blue",
                                "Investment" = "skyblue")) +
  guides(color = guide_legend(title = "Legend")) + 
  labs(x = "years", y = "Average TIER1 ratio")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(TIER_plot)

#liquidity analysis 
#liquidity coverage ratio 
LCR <- data.frame(data[,49:54], data$Specialisation, data$Country, )

for (i in 1:ncol(LCR)){
  if (any(is.na(LCR[,i]) == T)){
    LCR[is.na(LCR[,i]), i] <- LCR[-1,]
  }
}

commercialBank = subset(data, Specialisation == 'Commercial bank')
investmentBank = subset(data, Specialisation == 'Investment bank')

#LCR for investment
investment_data <- subset(investmentBank, select = Basel_LCR_2021:Basel_LCR_2016)

investment_data <- na.omit(investment_data)
colnames(investment_data) <- c(2021:2016)
mean_LCR <- colMeans(investment_data)
investment_data <- rbind(investment_data, mean_LCR)
investment_data <- data.frame(t(investment_data))

#LCR commercial
commercial_data <- subset(commercialBank, select = Basel_LCR_2021:Basel_LCR_2016)

commercial_data <- na.omit(commercial_data)
colnames(commercial_data) <- c(2021:2016)
mean_LCR_commercial <- colMeans(commercial_data)
commercial_data <- rbind(commercial_data, mean_LCR_commercial)
commercial_data <- data.frame(t(commercial_data))

matrix <- matrix(ncol = 3, nrow = 6)
LCR_data <- data.frame(matrix) 

LCR_data[,1] <- c(2016:2021)
LCR_data[,2] <- commercial_data[6:1, 50]
LCR_data[,3] <- investment_data[6:1, 15]

#LCR plot over years for the two different categories 
LCR_plot <- ggplot(data = LCR_data) + 
  geom_line(aes(x = X1, y = X2, color = "Commercial")) +
  geom_line(aes(x = X1, y = X3, color = "Investment")) + 
  scale_color_manual(values = c("Commercial" = "red",
                                "Investment" = "blue")) +
  guides(color = guide_legend(title = "Legend")) + 
  labs(x = "years", y = "Average LCR")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(LCR_plot)

#pursuing the liquidity analysis: Leverage ratio 
leverage_data <- data[,60:55]
leverage_data <- na.omit(leverage_data)
mean <- colMeans(leverage_data[1:6,])
c <- c(2016:2021)

leverage_data <- rbind(leverage_data, c)
leverage_data <- rbind(leverage_data, mean)
leverage_data <- t(leverage_data)
leverage_data <- data.frame(leverage_data)

leverage_plot <- ggplot(data = leverage_data, aes(x = X128, y = X129)) + 
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3)+
  labs(x = "Years", y = "Leverage ratio %")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(leverage_plot)

#profitability analysis 
#ROE computation for bank size using the average roe over years 
roe_data <- data[,78:73]

roe_data <- apply(roe_data[,1:6], 2, as.numeric)
mean_by_years <- rowMeans(roe_data[,1:6])
roe_data <- cbind(roe_data, mean_by_years)

roe_data <- cbind(roe_data, data$size)
roe_data <- cbind(roe_data, data$Specialisation)
roe_data <- data.frame(roe_data)

mean_by_size_ <- aggregate(as.numeric(roe_data$mean_by_years), by = list(roe_data$V8, roe_data$V9), FUN = mean)
mean_by_size_ <- data.frame(mean_by_size_)
colnames(mean_by_size_) <- c("Bank_size", "Bank_type", "Average_ROE_")

roe_plot <- ggplot(data = mean_by_size_, aes(x = Bank_size, y = Average_ROE_, fill = Bank_type)) +
geom_bar(stat = "identity", position = "dodge", ) +
  scale_fill_manual(values = c("blue", "skyblue")) +
  labs(x = "Bank Size", y = "Average ROE %") 
print(roe_plot)

#profitability based on RWA levels 
RWA <- c()
for (i in 1:6){
  RWA <- (data[,36:31] * 1/data[,18:13])^-1
}
ROE_tot <- data[,78:73]
ROE_tot <- apply(ROE_tot, 2, as.numeric)
mean_roe <- rowMeans(ROE_tot)
ROE_tot <- cbind(ROE_tot, mean_roe)

RWA <- apply(RWA, 2, as.numeric)
mean_rwa <- rowMeans(RWA)
RWA <- cbind(RWA, mean_rwa)

p <- data.frame(RWA[,7], ROE_tot[,7])

for (i in nrow(p):1) {
  if (any(is.na(p[i,]))) {
    p <- p[-i,]
  }
}
colnames(p) <- c("rwa", "roe")

ols <- lm(roe ~ rwa, data = p )
plot(ols)

#how many banks belong to the same quantile of risk ?
#RWA divided in quantiles 
mean_rwa <- na.omit(mean_rwa)
q1_ <- quantile(mean_rwa, 0.25)
q2_ <- quantile(mean_rwa, 0.5)
q3_ <- quantile(mean_rwa, 0.75)
q4_ <- quantile(mean_rwa, 1)

categories_ <- rep(NA, nrow(p))
quantiles_rwa <- c("q1", "q2", "q3", "q4")
p <- data.frame(p)

for (i in 1:nrow(p)) {
  
  if (p[i, 1] <= q1_){
    categories_[i] <- "1st quartile"
  } 
  if (p[i, 1] > q1_ & p[i, 1] < q2_){
    categories_[i] <- "2nd quartile"
  } 
  if (p[i, 1] > q2_ & p[i, 1] < q3_){
    categories_[i] <- "3th quartile"
  }
  if (p[i, 1] >= q3_){
    categories_[i] <- "4th quartile"
  }
}
categories_
p <- cbind(p, categories_)

rwa_plot <- ggplot(p, aes(x = categories_, y = roe)) + 
  geom_boxplot(outlier.shape = 21, outlier.size = 3, outlier.color = "blue", fill = 'skyblue') +
  xlab("RWA quantiles") + ylab("ROE percentage")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(rwa_plot)
 
#total assets vs ROE
k <- data.frame(total_assets$V7, roe_data$mean_by_years)
colnames(k) <- c("assets", "roe")

roe <- ggplot(k, aes(x = as.numeric(assets), y = as.numeric(roe))) + 
  geom_point(alpha = 0.5, size = 2, color = "blue") +
  labs(x = "Total assets", y = "ROE percentage") 

print(roe)

#Capital structure analysis 
dataset <- read_excel("C:/Users/Lenovo/Downloads/Dajee.xlsx", sheet = 2)

na = sum(is.na(dataset))
ru =which(rowSums(is.na(dataset))<10)
dataset = dataset[ru,]
data[,80:85] <- dataset[,36:31]

#assets debt to equity
#computation of total assets mean for commercial banks 

years <- 2016:2021
media_a <- numeric(length(years))
for (i in seq_along(years)) {
  media_a[i] <- sum(commercialBank[[paste0("tot_assets_", years[i])]]) / 165
}

#computation of debt to equity mean for commercial banks 
years <- 2016:2021
media_dte <- numeric(length(years))
for (i in seq_along(years)) {
  media_dte[i] <- sum(commercialBank[[paste0("debt_to_equity_", years[i])]]) / 165
}

andamento = data.frame(years, media_dte, media_a) 

#computation of total assets mean for investment banks
years <- 2016:2021
media_a_I <- numeric(length(years))
for (i in seq_along(years)) {
  media_a_I[i] <- sum(investmentBank[[paste0("tot_assets_", years[i])]]) / 165
}

#computation of debt to equity mean for investment banks 
years <- 2016:2021
media_dte_I <- numeric(length(years))
for (i in seq_along(years)) {
  media_dte_I[i] <- sum(investmentBank[[paste0("debt_to_equity_", years[i])]]) / 165
}
andamentoI = data.frame(years, media_dte_I, media_a_I) 

#plot for investment bank 
assets_debt_investment <- ggplot(andamentoI, aes(x = years)) +
  geom_line(aes(y = media_a_I, color = "Assets"), size = 1, color = "red") + 
  geom_point(aes(x = years, y = media_a_I), size = 2, color = "black")+
  geom_line(aes(y = media_dte_I*1000, color = "Debt-to-Equity"),  size = 1, color = "blue") +
  geom_point(aes(x = years, y = media_dte_I*1000), size = 2, color = "black")+
  ggtitle("Assets and Debt-to-Equity between years for INVESTMENT BANKS")+
  scale_y_continuous(
    name = 'Assets',
    sec.axis = sec_axis(~ ./1000, name = "Debt-to-Equity"))
print(assets_debt_investment)

#plot for commercial banks
assets_debt_commercial <- ggplot(andamento, aes(x = years)) +
  geom_line(aes(y = media_a, color = "Assets"), size = 1, color = "red") + 
  geom_point(aes(x = years, y = media_a), size = 2, color = "black")+
  geom_line(aes(y = media_dte*8000, color = "Debt-to-Equity"), size = 1, color = "blue" ) +
  geom_point(aes(x = years, y = media_dte*8000), size = 2, color = "black")+
  ggtitle("Assets and Debt-to-Equity between years for COMMERCIAL BANKS")+
  scale_y_continuous(
    name = 'Assets',
    sec.axis = sec_axis(~ ./8000, name = "Debt-to-Equity"))
print(assets_debt_commercial)

#loans analysis 
Loans <- read_excel("C:/Users/Lenovo/Downloads/Loans.xlsx", sheet = 2)
na = sum(is.na(Loans))
ru =which(rowSums(is.na(Loans))<10)
data_loans = Loans[ru,]

CommercialBank = subset(data_loans, Specialisation == 'Commercial bank')
CommercialBank <- na.omit(CommercialBank)
data_loans <- data.frame(data_loans)

#computation of loan mean for commercial bank 
media_NLoans <- rep(NA, 6)
for (i in 1:6) {
  year <- 2016 + i - 1  
  media_NLoans[i] <- mean(CommercialBank[[paste0("Loans_", year)]])
}
print(media_NLoans)
year = c(2016:2021)
VNL = data.frame(year, media_NLoans)

p0 <- ggplot(VNL, aes(x = year, y = media_NLoans)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "NLoans Over the Year",
       x = "Year",
       y = "Media Nloans")
print(p0)

#computation of consumer loan mean for commercial bank  
media_CLoans <- rep(NA, 6)
for (i in 1:6) {
  year <- 2016 + i - 1  
  media_CLoans[i] <- mean(CommercialBank[[paste0("Consumer_loans_", year)]])
}
print(media_CLoans)
year = c(2016:2021)
VCR = data.frame(year, media_CLoans)

p1 <- ggplot(VCR, aes(x = year, y = media_CLoans)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Consumer Loan Over the Years",
       x = "Year",
       y = "Consumer Loan")
print(p1)

#computation of corporate loan mean for commercial bank 
media_CorLoans <- rep(NA, 6)
for (i in 1:6) {
  year <- 2016 + i - 1  
  media_CorLoans[i] <- mean(CommercialBank[[paste0("Corporate_loans_", year)]])
}
print(media_CorLoans)
year <- c(2016:2021)

VCE = data.frame(year, media_CorLoans)

p2 <- ggplot(VCE, aes(x = year, y = media_CorLoans)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Corporate Loan Over the Years",
       x = "Year",
       y = "Corporate Loan")
print(p2)


#computation of other loan mean for commercial bank 
media_OLoans <- rep(NA, 6)
for (i in 1:6) {
  year <- 2016 + i - 1  
  media_OLoans[i] <- mean(CommercialBank[[paste0("Other_loans_", year)]])
}
print(media_OLoans)
year <- c(2016:2021)

VOL = data.frame(year, media_OLoans)

p3 <- ggplot(VOL, aes(x = year, y = media_OLoans)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Other Loan Over the Years",
       x = "Year",
       y = "Other Loan")
print(p3)

#plot of all the loans 
p_final <- plot_grid(p0, p1, p2, p3, ncol = 2)
print(p_final)

#plot of loans composition over the years 
Loans <- c(rep("Loan", 6), rep("Corporate", 6), rep("Consumer", 6), rep("Other", 6))
Time <- rep(c("2021", "2020", "2019", "2018", "2017", "2016"), 4)
Category <- rep(c("A", "B", "C", "D", "E", "F"), 4)
names(Time) <- Category
value <- c(media_OLoans, media_CLoans, media_CorLoans, media_OLoans)
data <- data.frame(Loans, Time, Category, value)

loans_plot <- ggplot(data, aes(fill=Time, y=value, x=Loans)) + 
  geom_bar(position="dodge", stat="identity" ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(loans_plot)

#NPL analysis 
a = sum(is.na(finalenoloans))
ru =which(rowSums(is.na(finalenoloans))<10)
NplData = finalenoloans[ru,]
NplData
NpLDataOnly <- na.omit(NpLDataOnly)
media_NPLRatio2021 = sum(NpLDataOnly$NPL_ratio_2021)/43
media_NPLRatio2020 = sum(NpLDataOnly$NPL_ratio_2020)/43
media_NPLRatio2019 = sum(NpLDataOnly$NPL_ratio_2019)/43
media_NPLRatio2018 = sum(NpLDataOnly$NPL_ratio_2018)/43
media_NPLRatio2017 = sum(NpLDataOnly$NPL_ratio_2017)/43
media_NPLRatio2016 = sum(NpLDataOnly$NPL_ratio_2016)/43

vector_mediaNPL <- c(media_NPLRatio2021, media_NPLRatio2020, media_NPLRatio2019, media_NPLRatio2018, media_NPLRatio2017, media_NPLRatio2016)

year <- c(2021:2016)

VNPL = data.frame(year, vector_mediaNPL)

ggplot(VNPL, aes(x = year, y = vector_mediaNPL, col = "red")) +
  geom_line() +
  geom_point() +
  labs(title = "NPL Ratio Over the Years",
       x = "Year",
       y = "NPL Ratio")

#end