#Chapter -1 : Bakery

# Sales Sales at Bakery
bakery = read.csv('./mabook/BakeryData.csv', stringsAsFactors = F)
summary(bakery)
dim(bakery)
colSums(is.na(bakery))
sapply(bakery, class)
names(bakery)
sum(is.na(bakery$Date))
summary(bakery$Date)

library(lubridate)
bakery$Date <- as.Date(bakery$Date,'%m/%d/%Y')
summary(bakery$Date)
wday(df$date, label=TRUE)

strftime(bakery$Date,'%A')
weekdays(bakery$Date)
wday(bakery$Date, label=TRUE)
as.POSIXlt(bakery$Date)$wday
names(bakery)
(df=aggregate(bakery[c('Cakes', 'Pies', 'Cookies', 'Smoothies', 'Coffee')], by=list(weekday = weekdays(bakery$Date)), FUN= mean))
matplot(df)
aggregate( cbind(Cakes, Pies, Cookies, Smoothies, Coffee) ~ weekdays(bakery$Date) ,data=bakery, FUN=mean)


library(ggplot2)

ggplot(data = df, aes(x = weekday)) + 
  geom_line(aes(y = Cakes)) + 
  geom_line(aes(y = Pies)) +
  geom_line(aes(y = Cookies)) +
  geom_line(aes(y = Smoothies)) +
  geom_line(aes(y = Coffee)) +
  labs(y = "Avg Sales") # Delete or change y axis title if desired.

matplot(as.matrix(df[,1]),as.matrix(df[,-1]),type='l')
df2= as.matrix(df[,-1])
?matplot
matplot(as.matrix(df[,1]),df2,type='l')