setwd("自己打 working directory")

# 只要跑一次存好檔，讀檔比較快
# all_merge <- read.csv("all_merge5.csv")
# library(dplyr)
# all_merge <- distinct(all_merge)
# all_merge <- subset(all_merge, select = -c(2,3,5,6,12) )
# saveRDS(all_merge, file = "all_merge_withNames.rds")
# all_merge <- subset(all_merge, select = -c(3) ) # names
# coffee <- subset(all_merge, (all_merge$special_type=="normal"))
# coffee <- subset(coffee, (coffee$uniprice<=200))
# coffee <- subset(coffee, select = -c(12) ) # 刪 special_type
# saveRDS(coffee, file = "coffee.rds")
# coffee <- subset(coffee, select = -c(15) ) # 刪 number
# coffee[which(coffee$tep_type =="non"&coffee$channel =="星巴克"),"tep_type"] <- "熱"
# coffee <- subset(coffee,coffee$quant*coffee$uniprice==coffee$totprice)
# saveRDS(coffee, file = "coffee_withoutNum.rds")

coffee <- readRDS("coffee_withoutNum.rds")


pairs(coffee[,c(6,4,5,7,15,17,18)], pch=19, cex=0.5)




coffee[which(coffee$tep_type =="non" & channel =="星巴克"),]
coffee[which(coffee$tep_type =="non"&channel =="星巴克"),'tep_type'] <- "熱"



# 不同 channel
summary(all_merge$channel)
channel_711 <- subset(all_merge, (all_merge$channel=="7-11"))
channel_louisa <- subset(all_merge, (all_merge$channel=="路易莎"))
channel_family <- subset(all_merge, (all_merge$channel=="全家"))
channel_starbucks <- subset(all_merge, (all_merge$channel=="星巴克"))
channel_cama <- subset(all_merge, (all_merge$channel=="cama"))

# EDA
library(ggplot2)
groupcoffe_type<-group_by(coffee,channel,coffe_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),
            meantotprice=mean(totprice))
qplot(groupcoffe_type$coffe_type, groupcoffe_type$sumquant, data = groupcoffe_type, 
      color=channel,xlab="coffee_type",ylab ="quant", 
      main ="不同咖啡類型的銷售量")+theme(plot.title=element_text(hjust = 0.5))
qplot(groupcoffe_type$coffe_type, groupcoffe_type$sumtotprice, data = groupcoffe_type, 
      color=channel,xlab="coffee_type",ylab ="totalprice", 
      main ="不同咖啡類型的銷售總額")+theme(plot.title=element_text(hjust = 0.5))

groupmonth_type<-group_by(coffee,channel,month_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),
            meantotprice=mean(totprice))
qplot(groupmonth_type$month_type, groupmonth_type$sumquant, data = groupmonth_type, 
      color=channel,xlab="month",ylab ="quant", 
      main ="不同月份的銷售量")+theme(plot.title=element_text(hjust = 0.5))

groupclock_type<-group_by(coffee,channel,clock_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),
            meantotprice=mean(totprice))
qplot(groupclock_type$clock_type, groupmonth_type$sumquant, data = groupclock_type, 
      color=channel,xlab="clock",ylab ="quant", 
      main ="不同時點的銷售量")+theme(plot.title=element_text(hjust = 0.5))

# 先把size為non的拿掉
size_nonon <-  subset(coffee, size_type!=c("non"))
groupsize <- group_by(size_nonon, channel, size_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),
            meantotprice=mean(totprice),sumquant=sum(quant),
            meanquant=mean(quant))
qplot(groupsize$size_type,groupsize$sumquant, data = size, color=channel,xlab="Size Type",ylab ="Quantity", 
                 main ="不同咖啡類型的銷售總額")+theme(plot.title=element_text(hjust = 0.5))


# Model
hist(coffee$totprice, xlab="totprice", main="Histogram of totprice", breaks=10)
coffee$recipTotprice <- 1 / coffee$totprice
hist(coffee$recipTotprice, xlab="recipTotprice", main="Histogram of recipTotprice", breaks=10)


