#Seyit Mustafa Turhan 
#Econ303 Project




#required packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("knitr")

library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)


#setting our directory
setwd("C:/Users/tseyi/Desktop")

#importing data
trade_dataset <- read_xlsx("trade_dataset.xlsx")


#looking for data structure and size

dim(trade_dataset)
glimpse(trade_dataset)

trade_dataset %>%
  filter(flowDesc == "Import") %>%
  select(cifvalue, primaryValue) %>%
  head(10)


#preparing data for analysis

trade_dataset %>%
  count(refYear, flowDesc, cmdCode) %>%
  pivot_wider(names_from = flowDesc, values_from = n)

df <- trade_dataset %>%
  select(refYear, flowDesc, cmdCode, cmdDesc, primaryValue) %>%
  mutate(primaryValue = as.numeric(primaryValue)) %>% 
  filter(!is.na(primaryValue))


#total EX%IM annually

EXIM_annually <- df %>%
  group_by(refYear, flowDesc) %>%
  summarise(total = sum(primaryValue), .groups = "drop") %>%
  pivot_wider(names_from = flowDesc, values_from = total) %>%
  mutate(trade_volume = Import + Export)

kable(EXIM_annually)

#VISUALISATION
EXIM_annually_long <- df %>%
  group_by(refYear, flowDesc) %>%
  summarise(total = sum(primaryValue), .groups = "drop")

#Llyold

ggplot(EXIM_annually_long, aes(x = refYear, y = total / 1e9, color = flowDesc)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Annual Development of JP&SP Trade",
       x = "Year", y = "Value (Billion $)",
color = "Flow Type") +
  theme_minimal()



#Trends

df %>%
  filter(cmdCode == 7) %>%
  ggplot(aes(x = refYear, y = primaryValue / 1e9, color = flowDesc)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Trade in Machinery and Transportation Equipment (SITC 7)",
x = "Year", y = "Value (Billion $)") +
  theme_minimal()


#trade balance

trade_balance <- df %>%
  group_by(refYear, flowDesc) %>%
  summarise(total = sum(primaryValue), .groups = "drop") %>%
  pivot_wider(names_from = flowDesc, values_from = total) %>%
  mutate(net_trade = Export - Import)

kable(trade_balance)



#VISUALISATION
ggplot(trade_balance, aes(x = refYear, y = net_trade / 1e9)) +
  geom_col(fill = "steelblue") + 
  labs(title = "JP's Trade Balance With SP",
       X = "Year", y = "Net Export ( Billion $)") + 
  theme_minimal()

gl_index <- df %>%
  group_by(cmdCode, cmdDesc, refYear) %>%
  summarise(Ex = sum(primaryValue[flowDesc == "Export"]),
            Im = sum(primaryValue[flowDesc == "Import"]),
            .groups = "drop") %>%
  mutate(GL = 1 - abs(Ex - Im) / (Ex + Im)) %>%
  group_by(cmdCode, cmdDesc) %>%
  summarise(Avg_GL = mean(GL, na.rm = TRUE), .groups = "drop")



#advanced analysis

df %>%
  group_by(refYear, cmdDesc) %>%
  summarise(total = sum(primaryValue), .groups = "drop") %>%
  group_by(refYear) %>%
  mutate(share = total / sum(total) * 100) %>%
  ggplot(aes(x = refYear, y = share, fill = cmdDesc)) +
  geom_area(alpha = 0.7) +
  labs(title = "Share of Product Categories in Total Trade (%)",
       x = "Year", y = "Share (%)") +
  theme_minimal() + 
  theme(legend.position = "bottom")


#correlation
correlation_df <- EXIM_annually_long %>%
  pivot_wider(names_from = flowDesc, values_from = total) %>%
  select(-refYear)

cor(correlation_df$Import, correlation_df$Export)

#Summary

summary_final <- df %>%
  group_by(cmdCode, cmdDesc, flowDesc) %>%
  summarise(average = mean(primaryValue / 1e6), .groups = "drop") %>% 
  pivot_wider(names_from = flowDesc, values_from = average) %>%
  arrange(cmdCode)

kable(summary_final, digits = 2, col.names = c("Code", "Product", "Avr. Im. (Million $)", "Avr. Ex. (Million $"))





































































































































