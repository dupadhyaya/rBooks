# MA - Chapter-1
# 

pareto = read.csv("./mabook/pareto.csv")
sales = pareto # make a copy
summary(sales)
head(sales)
#sales has $ and is character type
gsub('$','', '$3')
sales$Price = gsub('\\$','', sales$Price)
summary(sales)
head(sales)
sales$Price = as.numeric(sales$Price)
colSums(is.na(sales))   # missing values
str(sales)
aggregate(sales$Price,by=list(sales$Store), sum)

library(dplyr)
names(sales)
sales %>% 
   group_by(Store) %>% 
   summarise(sum_allow=sum(Price)) %>% 
   mutate(pct_allow=sum_allow/sum(sum_allow))

sales %>% 
  group_by(Month) %>%
  summarise(sum_allow=sum(Price)) %>% 
  mutate(pct_allow=sum_allow/sum(sum_allow)) %>%
#  arrange(pct_allow) %>%
  data.frame()
# Months not in the order

month.name[1:12]
month = factor(month.name[1:12], ordered=T, labels=month.name[1:12])
month
sales$Month

sales$Month = factor(sales$Month,levels =month.name[1:12] )
sales$Month

sales %>% 
  group_by(Month) %>%
  summarise(sum_allow=sum(Price)) %>% 
  mutate(pct_allow=sum_allow/sum(sum_allow)) %>%
  data.frame()
# Months now in the order

names(sales)
# Pareto Chart

aggregate(sales$Price, by=list(sales$Product), sum)
prodsales= setNames(aggregate(sales$Price, by=list(sales$Product), sum),c('Product' ,'TotalSales'))

prodsales
names(prodsales)
head(prodsales[order(prodsales$TotalSales, decreasing = TRUE),])



# Pg 11 Report Filter & Slicers
names(sales)
aggregate(sales$Price, by=list(sales$Product), sum, mean)
aggregate(cbind(val1, val2) ~ id1 + id2, data = x, FUN = plyr::each(avg = mean, n = length))
aggregate(Price ~ Store , data = sales, FUN = plyr::each(avg = mean, n = length))
aggregate(Price ~ Product , data = sales, FUN = plyr::each(avg = mean, n = length))
aggregate(Price ~ Month + Store , data = sales, FUN = plyr::each(avg = mean, n = length))


