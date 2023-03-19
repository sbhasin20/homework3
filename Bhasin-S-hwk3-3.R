if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, modelsummary)
install.packages("fixest")
ilibrary(fixest)


#1 

final.data <- final.data %>% group_by(state) %>% arrange(state, Year) %>%
  mutate(tax_change = tax_state - lag(tax_state),
         tax_change_d = ifelse(tax_change == 0,0,1),
         price_cpi_2012 = cost_per_pack*(229.5939/index),
         total_tax_cpi_2012=tax_dollar*(229.5939/index),
         ln_sales=log(sales_per_capita),
         ln_price_2012=log(price_cpi_2012))

tax.change.plot <- final.data %>% group_by(Year) %>% filter(Year<1986, Year> 1970) %>%
  summarize(mean_change=mean(tax_change_d)) %>% 
  ggplot(aes(x=as.factor(Year), y=mean_change)) +
  geom_bar(stat= "identity") +
  labs(
    x="Year",
    y = "Share of States",
    title = ""
  ) + ylim(0,1) +
  theme_bw()

tax.change.plot

#2 

tax.price.data <- final.data %>% select(Year, state, total_tax_cpi_2012, price_cpi_2012)  %>%
  group_by(Year)  %>% summarize(mean_tax=mean(total_tax_cpi_2012, na.rm = TRUE),
                                mean_price =mean(price_cpi_2012, na.rm = TRUE))  %>%
  pivot_longer(cols=c("mean_tax", "mean_price"),
               names_to="var", values_to = "dollars")

tax.price.plot <- tax.price.data  %>%
  ggplot(aes(x=Year, y=dollars, color=var)) +
  geom_line() +
  labs(
    x="Year",
    y= "Price per Pack in 2012 Dollars"
  ) + ylim(0,10) +
  geom_text(data = tax.price.data %>% filter(Year == 2015),
            aes(label = c("Mean Price", "Mean Tax"),
                x= Year,
                Y= dollars-.3)) +
  scale_color_manual(values=c("black", "black")) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1970, 2020, 5))

tax.price.plot


#3 
cig.data.change <- final.data %>% ungroup() %>%
  filter(Year==1970)  %>% select(state, price_1970= price_cpi_2012)  %>%
  left_join(final.data  %>% filter(Year== 2018)  %>% select(state, price_2018 = price_cpi_2012),
            by=c("state"))  %>%
  mutate(price_change = price_2018-price_1970)


high.change <- cig.data.change  %>% slice_max(price_change, n=5)  %>% mutate(change_group = "high")
low.change <- cig.data.change  %>% slice_min(price_change, n=5) %>% mutate(change_group = "low")
change.group  <- rbind(high.change, low.change)

top.bottom.price <- final.data %>%
  inner_join(change.group %>% select(state, change_group),
             by=c("state"))

high.price.plot <- top.bottom.price %>% filter(change_group=="high") %>%
  ggplot(aes(x=Year, y=sales_per_capita, color= state)) +
  stat_summary(fun= "mean", geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    title = "",
    color= "State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970,2019,5))

high.price.plot

#4 

low.price.plot <- top.bottom.price %>% filter(change_group=="low") %>%
  ggplot(aes(x=Year, y=sales_per_capita, color= state)) +
  stat_summary(fun= "mean", geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    title = "",
    color= "State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970,2019,5))

low.price.plot

#5 

difference <- final.data %>%
  group_by(state) %>%
  summarize(price_change = price_cpi_2012[Year == 2018] - price_cpi_2012[Year == 1970])

sort_data <- difference %>%
  arrange(desc(price_change))

top_5 <- sort_data %>%
  head(5)

final.data.five <- final.data %>%
  filter(state %in% top_5$state) %>%
  group_by(Year) %>%
  summarize(sales_per_capita_five = mean (sales_per_capita)) 

difference <- final.data %>%
  group_by(state) %>%
  summarize(price_change = price_cpi_2012[Year == 2018] - price_cpi_2012[Year == 1970])

sort_data <- difference %>%
  arrange(desc(price_change))

bottom_5 <- sort_data %>%
  tail(5)

final.data.bfive <- final.data %>%
  filter(state %in% bottom_5$state) %>%
  group_by(Year) %>%
  summarize(sales_per_capita_fiveb = mean (sales_per_capita)) 

graph_5 <- ggplot() +
  geom_line(data = final.data.five, aes(x = Year, y = sales_per_capita_five, color = "red")) +
  geom_line(data = final.data.bfive, aes(x = Year, y = sales_per_capita_fiveb , color = "blue")) +
  labs(x = "Year", y = "Average Cigarette Packs Sold Per Capita", 
       title = "",
       color = "Legend") +
  scale_color_manual(values = c("red", "blue"), labels = c("Low","High")) +
  theme_minimal()

graph_5 

#Sales from the 5 states with the highest price increase had a steeper decline in average cigarette packs sold per capita compared to the 5 states with the lowest price increase. After 1973 till 2018, the five states with the largest price increase had an average lower sales per capita compared to five states with the lowest price increase.


#6 

ln_sales <- log(final.data$sales_per_capita)
ln_price_2012 <-log(final.data$price_cpi_2012)

ols <- feols(ln_sales~ln_price_2012, data = final.data %>% filter(Year<1991))

summary(ols)

#An 1% increase in the cost of a cigarette pack is estimated to decrease sales per capita by 0.80 percent on average. It is an inelastic relationship. 


#7

iv <- feols(ln_sales ~1 | ln_price_2012 ~ total_tax_cpi_2012, data = final.data %>% filter(Year<1991))

summary(iv)

#An 1% increase in the cost of a cigarette pack is estimated to decrease sales per capita by 0.74%. The estimates of those with an instrument are different and show that a change in cost has a slightly smaller impact on sales, making it slightly less elastic. This is because the estimates with an instrument are accounting for the total cigarette tax has on the cost of cigarette packs.

#8

first.stage <- feols(ln_price_2012~total_tax_cpi_2012, data= final.data %>% filter(Year<1991))

summary(first.stage)

reduced.form <- feols(ln_sales~total_tax_cpi_2012, data= final.data %>% filter(Year<1991))

summary(reduced.form)


#9

ols_part2 <- feols(ln_sales~ln_price_2012, data = final.data %>% filter(Year<=2015 & Year>=1991))

summary(ols_part2)

#An 1% increase in the cost of a cigarette pack is estimated to decrease sales per capita by 0.997 percent on average. It is an inleastic relationship but very close to an elastic relationship. 

iv_part2 <- feols(ln_sales ~1 | ln_price_2012 ~ total_tax_cpi_2012, data = final.data %>% filter(Year>=1991 & Year<= 2015))

summary(iv_part2)

#An 1% increase in cost per cigarette pack is estimated to decrease sales per capita by 1.17%. The estimates of those with an instrument are different and show that a change in cost has a greater impact on sales, making it more elastic. This may be due to the fact that the cigarette tax is accounted for and its influence on the cost per pack. 


first.stage_part2 <- feols(ln_price_2012~total_tax_cpi_2012,data= final.data %>% filter(Year>= 1991 & Year <= 2015))

summary(first.stage_part2)

reduced.form_part2 <- feols(ln_sales~total_tax_cpi_2012, data= final.data %>% filter(Year>= 1991 & Year <= 2015))

summary(reduced.form_part2)

#10 


#Yes, they are different. The estimates from 1991 to 2015 are more elastic compared to those from 1970 t0 1990. This may be because taxes on cigarette packs increased and the Center for Disease Control emphasized the harmful health effects of smoking, making people more sensitive to the price changes on cigarettes.

save.image("Hwk3_workspace.Rdata")   

