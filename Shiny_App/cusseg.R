library(tidyr) # data transformations
library(dplyr) # data transformations
library(ggplot2) # data visualization
library(lubridate) #for working with dates

#loading data
data <- read_csv("pikachu.csv")

summary(data)

#feature engineering
data$TotalPrice <- data$Quantity * data$UnitPrice

#making datetime variable
data$InvoiceDateTime <- lubridate::mdy_hm(data$InvoiceDate) #make datetime variable
data$InvoiceDate <- lubridate::ymd(date(data$InvoiceDateTime)) #make date variable


data$InvoiceYear <- year(data$InvoiceDate)
data <- data %>% filter(InvoiceYear==2017)

data$Recency <-  as.numeric(mdy("01-01-2018") - data$InvoiceDate)

r_data <- data %>% 
  group_by(CustomerID) %>% 
  summarize(Recency=min(Recency))

f_data <- data %>% 
  group_by(CustomerID) %>% 
  count(Frequency=n_distinct(InvoiceNo)) %>% 
  select(CustomerID, Frequency)

m_data <- data %>% 
  group_by(CustomerID) %>% 
  summarise(mon_value=sum(TotalPrice))

new_data <- r_data %>% 
  full_join(m_data, by="CustomerID") %>% 
  full_join(f_data, by="CustomerID")

new_data$Recency_group <- cut(new_data$Recency, 
                              quantile(new_data$Recency, 
                                       probs=seq(0,1,0.25)), 
                              ordered_result=T, 
                              include.lowest=T) # segment data into groups

new_data$Recency_group <- factor(new_data$Recency_group, 
                                 labels=c("very recent", "recent", "old", "oldest")) #rename 
table(new_data$Frequency)

new_data$Frequency_group <- cut(new_data$Frequency, 
                                c(0,1,3,10,188), 
                                ordered_result=T) #segment into four groups

new_data$Frequency_group <- factor(new_data$Frequency_group, 
                                   labels=c("very rare", "rare", "frequent", "very frequent"))

#akxak
new_data$mon_value_group <- cut(new_data$mon_value, 
                                quantile(new_data$mon_value, probs=seq(0,1,0.25)), 
                                ordered_result=T, include.lowest=T) #segment into groups

new_data$mon_value_group <- factor(new_data$mon_value_group, 
                                   labels=c("small", "medium", "large", "very large")) #rename ggplot(new_data, aes(Frequency_group)) +
geom_bar()


saveRDS(new_data, "campaign.rds")

new_data_updated <- read.csv('campaign.csv')   




plot_code <- ggplot(new_data, aes(Recency_group, Frequency_group)) +
  geom_count() +
  facet_grid(mon_value_group ~ .) +
  labs(x="Recency", y="Frequency", title="RFM analysis since 2017")  

library("plotly")
plotly::ggplotly(plot_code)

