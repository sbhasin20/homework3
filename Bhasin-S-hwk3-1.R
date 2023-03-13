if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)
library(modelr)

#1 

tax_change <- final.data %>%
  filter(Year >= 1970 & Year <= 1985) %>%
  group_by(Year) %>%
  summarize(proportion_changed = length(unique(state[tax_dollar != lag(tax_dollar)]))/ length(unique(state)))
print(tax_change)

graph_1 <- ggplot(tax_change, aes(x = Year, y = proportion_changed)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Proportion of States with a Change in Cigarette Tax from 1970 to 1985", x = "Year", y = "Proportion of States with a Change") +
  theme_bw()
graph_1


#2 

final.data <- filter(final.data, Year >= 1970 & Year <= 2018)

average_tax <- final.data %>% group_by(Year) %>% summarize(avg_tax = mean(tax_dollar)) 
average_price <- final.data %>% group_by(Year) %>% summarize(avg_price = mean(cost_per_pack)) 

graph_2 <- ggplot() +
  geom_line(data = average_tax, aes(x = Year, y = avg_tax, color = "red")) +
  geom_line(data = average_price, aes(x = Year, y = avg_price, color = "blue")) +
  labs(x = "Year", y = "Average Tax/Price", 
       title = "Average Tax and Price of Cigarettes (1970-2018)",
       color = "Legend") +
  scale_color_manual(values = c("red", "blue"), labels = c("Price", "Tax")) +
  theme_minimal()

graph_2

#3 

difference <- final.data %>%
  group_by(state) %>%
  summarize(price_change = cost_per_pack[Year == 2018] - cost_per_pack[Year == 1970])

sort_data <- difference %>%
  arrange(desc(price_change))

top_5 <- sort_data %>%
  head(5)

final.data.five <- final.data %>%
  filter(state %in% top_5$state) %>%
  group_by(Year) %>%
  summarize(sales_per_capita_five = mean (sales_per_capita)) 
  
graph_3 <- ggplot(final.data.five, aes(x = Year, y = sales_per_capita_five)) + 
  geom_line() +
  labs(x = "State", y = "Average Cigarette Packs Sold Per Capita", title = "Average Cigarette Packs Sold Per Capita in Five States with Largest Increase in Price from 1970 to 2018")
graph_3

#4 

difference <- final.data %>%
  group_by(state) %>%
  summarize(price_change = cost_per_pack[Year == 2018] - cost_per_pack[Year == 1970])

sort_data <- difference %>%
  arrange(desc(price_change))

bottom_5 <- sort_data %>%
  tail(5)

final.data.bfive <- final.data %>%
  filter(state %in% bottom_5$state) %>%
  group_by(Year) %>%
  summarize(sales_per_capita_fiveb = mean (sales_per_capita)) 

graph_4 <- ggplot(final.data.bfive, aes(x = Year, y = sales_per_capita_fiveb)) + 
  geom_line() + 
  labs(x = "State", y = "Average Cigarette Packs Sold Per Capita", title = "Average Cigarette Packs Sold Per Capita in Five States with Smallest Increase in Price from 1970 to 2018")
graph_4


#5 

#Sales from the 5 states with the highest price increase has a steep decline in average cigarette packs sold per capita compared to the 5 states with the lowest price increase. As seen from question 3, there is a sharp decline from 1990 to 2000 whereas the graph from 4 had a more gradual decline.  

#6 

subset_data <- subset(final.data, Year >= 1970 & Year <= 1990)
model <- lm(log(sales_per_capita) ~ log(cost_per_pack), data = subset_data)
summary(model)

#An 1% increase in the cost of a cigarette pack is estimated to decrease sales per capita by 0.17 percent on average. It is an inelastic relationship. 


#7

if (!requireNamespace("AER", quietly = TRUE)) {
  install.packages("AER")
}
library(AER)

subset_data <- subset(final.data, Year >= 1970 & Year <= 1990)

subset_data$LogSales <- log(subset_data$sales_per_capita)
subset_data$LogPrices <- log(subset_data$cost_per_pack)
subset_data$TotalCigaretteTax <- log(subset_data$tax_dollar)

iv_regression <- ivreg(LogSales ~ LogPrices | TotalCigaretteTax, data = subset_data)

number <- summary(iv_regression)

number


#An 1% increase in the cost of a cigarette pack is estimated to decrease sales per capita by 0.28%. The estimates of those with an instrument are different and shows that a change in cost has a greater impact on sales, making it slightly more elastic. This is because the estimates with an instrument are accounting for the total cigarette tax has on the cost of cigarette packs.

#8

step1 <- lm(log(cost_per_pack) ~  log(tax_dollar), data=subset_data)
cost_hat <- predict(step1)
summary(cost_hat)

step2 <- lm(log(sales_per_capita) ~ cost_hat, data=subset_data)
summary(step2)

#9

#a
subset_data2 <- subset(final.data, Year >= 1991 & Year <= 2015)
model2 <- lm(log(sales_per_capita) ~ log(cost_per_pack), data = subset_data)
summary(model2)

#An 1% increase in the cost of a cigarette pack is estimated to decrease sales per capita by 0.17 percent on average. It is an inelastic relationship. 

#b
subset_data2$LogSales <- log(subset_data2$sales_per_capita)
subset_data2$LogPrices <- log(subset_data2$cost_per_pack)
subset_data2$TotalCigaretteTax <- log(subset_data2$tax_dollar)

iv_regression2 <- ivreg(LogSales ~ LogPrices | TotalCigaretteTax, data = subset_data2)

summary(iv_regression2)

# An 1% increase in cost per cigarette pack is estimated to decrease sales per capita by 0.76%. The estimates of those with an instrument are different and show that a change in cost has a greater impact on sales, making it more elastic. This may be due to the fact that the cigarette tax is accounted for and its inflence on the cost per pack. 

#c
step1 <- lm(log(sales_per_capita) ~  log(tax_dollar), data=subset_data2)
pricehat <- predict(step1)
summary(step1)

step2 <- lm(log(sales_per_capita) ~ pricehat, data=subset_data2)
summary(step2)

#10 

# Yes, they are different. The elasticity estimates from 1991-2015 are more elastic compared to those from 1970-1990. This may be because taxes on cigarette packs increased and the CDC emphasized the harmful health effects of smoking, making people more sensitive to the change in prices on cigarettes. 

save.image("Hwk3_workspace.Rdata")   

