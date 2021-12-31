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
coffee <- subset(coffee,coffee$size_type!="non"&coffee$tep_type!="non")


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
library(dplyr)

grouptepsize<- group_by(coffee, size_type, tep_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),
            meantotprice=mean(totprice),sumquant=sum(quant),
            meanquant=mean(quant))
qplot(grouptepsize$tep_type,grouptepsize$sumquant, data = grouptepsize, color=size_type,xlab="Temperature",ylab ="Quantity", 
      main ="溫度和大小的關係")+theme(plot.title=element_text(hjust = 0.5))

grouptepclock<- group_by(coffee, clock_type, tep_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),
            meantotprice=mean(totprice),sumquant=sum(quant),
            meanquant=mean(quant))
options(scipen = 999)
qplot(grouptepclock$tep_type,grouptepclock$sumtotprice, data = grouptepclock, color=clock_type,xlab="Temperature",ylab ="Totol Price", 
      main ="溫度和時間的關係")+theme(plot.title=element_text(hjust = 0.5))

options(scipen = 999)
grouparea<- group_by(coffee, channel, area_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),
            meantotprice=mean(totprice),sumquant=sum(quant),
            meanquant=mean(quant))
qplot(grouparea$area_type,grouparea$sumtotprice, data = grouparea, color=channel,xlab="Area",ylab ="Totol Price", 
      main ="地區與總銷售額關係")+theme(plot.title=element_text(hjust = 0.5),axis.text.x = element_text(angle = 270, vjust = 0.5))

groupuni<- group_by(coffee, channel, uniprice)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),
            meantotprice=mean(totprice),sumquant=sum(quant),
            meanquant=mean(quant))
qplot(groupuni$uniprice,groupuni$sumtotprice, data = groupuni, color=channel,xlab="Unit Price",ylab ="Totol Price", 
      main ="單價與總銷售額關係")+theme(plot.title=element_text(hjust = 0.5))

groupcoffe_type<-group_by(coffee,channel,coffe_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),
            meantotprice=mean(totprice))
qplot(groupcoffe_type$coffe_type, groupcoffe_type$sumquant, data = groupcoffe_type, 
      color=channel,xlab="Coffee Type",ylab ="Quantity", 
      main ="不同咖啡類型的銷售量")+theme(plot.title=element_text(hjust = 0.5))
qplot(groupcoffe_type$coffe_type, groupcoffe_type$sumtotprice, data = groupcoffe_type, 
      color=channel,xlab="Coffee Type",ylab ="Total Price", 
      main ="不同咖啡類型的銷售總額")+theme(plot.title=element_text(hjust = 0.5))

groupmonth_type<-group_by(coffee,channel,month_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),
            meantotprice=mean(totprice))
qplot(groupmonth_type$month_type, groupmonth_type$sumquant, data = groupmonth_type, 
      color=channel,xlab="Month",ylab ="Quantity", 
      main ="不同月份的銷售量")+theme(plot.title=element_text(hjust = 0.5))
qplot(groupmonth_type$month_type, groupmonth_type$sumtotprice, data = groupmonth_type, 
      color=channel,xlab="Month",ylab ="Total Price", 
      main ="不同月份的銷售總額")+theme(plot.title=element_text(hjust = 0.5))

groupclock_type<-group_by(coffee,channel,clock_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),
            meantotprice=mean(totprice))
qplot(groupclock_type$clock_type, groupmonth_type$sumtotprice, data = groupclock_type, 
      color=channel,xlab="Clock",ylab ="Total Price", 
      main ="不同時點的銷售總額")+theme(plot.title=element_text(hjust = 0.5))
qplot(groupclock_type$clock_type, groupmonth_type$sumquant, data = groupclock_type, 
      color=channel,xlab="Clock",ylab ="Quantity", 
      main ="不同時點的銷售量")+theme(plot.title=element_text(hjust = 0.5))

groupsize <- group_by(coffee, channel, size_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),
            meantotprice=mean(totprice),sumquant=sum(quant),
            meanquant=mean(quant))
qplot(groupsize$size_type,groupsize$sumquant, data = groupsize, color=channel,xlab="Size Type",ylab ="Quantity", 
      main ="不同咖啡大小的銷售量")+theme(plot.title=element_text(hjust = 0.5))
qplot(groupsize$size_type,groupsize$sumtotprice, data = groupsize, color=channel,xlab="Size Type",ylab ="Total Price", 
      main ="不同咖啡大小的銷售總額")+theme(plot.title=element_text(hjust = 0.5))

grouptep_type<-group_by(coffee,channel,tep_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),
            meantotprice=mean(totprice))
qplot(grouptep_type$tep_type,grouptep_type$sumquant, data = grouptep_type, color=channel,xlab="Temperature",ylab ="Quantity", 
      main ="不同咖啡溫度的銷售量")+theme(plot.title=element_text(hjust = 0.5))
qplot(grouptep_type$tep_type,grouptep_type$sumtotprice, data = grouptep_type, color=channel,xlab="Temperature",ylab ="Total Price", 
      main ="不同咖啡溫度的銷售總額")+theme(plot.title=element_text(hjust = 0.5))

groupweek <- group_by(coffee, channel, week_type)%>%
  summarise(transactions=n(),sumtotprice=sum(totprice),
            meantotprice=mean(totprice),sumquant=sum(quant),
            meanquant=mean(quant))
qplot(groupweek$week_type,groupweek$sumquant, data = groupweek, color=channel,xlab="Week Type",ylab ="Quantity", 
      main ="平日、假日的銷售量")+theme(plot.title=element_text(hjust = 0.5))
qplot(groupweek$week_type,groupweek$sumtotprice, data = groupweek, color=channel,xlab="Week Type",ylab ="Total Price", 
      main ="平日、假日的銷售總額")+theme(plot.title=element_text(hjust = 0.5))

# Model
par(mfrow=c(1,2))
hist(coffee$totprice, xlab="totprice", main="Histogram of totprice", breaks=10)
coffee$recipTotprice <- 1 / coffee$totprice
hist(coffee$recipTotprice, xlab="recipTotprice", main="Histogram of recipTotprice", breaks=10)

# model1
lm1 <- lm(recipTotprice ~ area_type + channel + quant + uniprice + invo_price + coffe_type + tep_type + size_type + month_type + clock_type + week_type, data = coffee)
summary(lm1)

par(mfrow=c(1,3))
plot(fitted(lm1), residuals(lm1), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lm1), main="Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals(lm1), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lm1))
