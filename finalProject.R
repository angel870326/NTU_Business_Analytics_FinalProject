setwd("...") # 自己的 Working Directory
#---------------------------------------------------------------------#
#                             資料整理（1）                             #
#---------------------------------------------------------------------#
# 讀進原始資料
data <- read_csv("NTU_Coffee.csv")
raw_data <- data.table(data)
# 將原始資料的變數進行分類
coffe_type <- str_extract(raw_data$name,pattern="拿鐵|鮮奶咖啡|雲朵冰搖咖啡|咖啡星冰樂|那堤|美式|冷萃咖啡|曼巴咖啡|氣泡通寧|濃縮|冰咖啡|西西里|咖啡豆")
tep_type <- str_extract(raw_data$name,pattern="冰|熱")
size_type <- str_extract(raw_data$name,pattern = "特大|大|中|小")
month_type <- month(raw_data$datetime_UTC_8)
clock_type <- hour(raw_data$datetime_UTC_8)
week <- wday(raw_data$datetime_UTC_8)
area_type <- str_extract(raw_data$county_district,pattern="新北市|臺北市|臺中市|臺南市|桃園市|高雄市|基隆市|新竹市|嘉義市|嘉義縣|新竹縣|苗栗縣|彰化縣|南投縣|雲林縣|屏東縣|宜蘭縣|花蓮縣|臺東縣|澎湖縣|連江縣|金門縣")
special_type <-str_extract(raw_data$name,pattern="隨時取|優惠|隨")
afternoon <- str_replace(new_data$clock_type, pattern ="12|13|14|15|16|17|18|19",replacement = "afternoon")
night <- str_replace(afternoon,pattern = "20|21|22|23|24|1|2|3",replacement = "night")
clock_type <- str_replace(night,pattern = "4|5|6|7|8|9|10|11",replacement = "morning")
weekday <- str_replace(week,pattern ="1|2|3|4|5",replacement = "weekday")
week_type <- str_replace(weekday,pattern="6|7",replacement = "weekend")
discount_number <- raw_data$invo_price-raw_data$totprice
new_data <- cbind(raw_data,coffe_type,tep_type,size_type,month_type,clock_type,area_type,special_type,discount_number,week_type)
# 處理空值
new_data$size_type[is.na(new_data$size_type)] <- "non"
new_data$tep_type[is.na(new_data$tep_type)] <- "non"
new_data$coffe_type[is.na(new_data$coffe_type)] <- "other"
new_data$special_type[is.na(new_data$special_type)]<- "normal"
# month_type 由數字改成文字 & clock_type 0:00 改成 24:00
new_data$month_type <- replace(new_data$month_type,new_data$month_type == 4,"April")
new_data$month_type <- replace(new_data$month_type,new_data$month_type == 5,"May")
new_data$month_type <- replace(new_data$month_type,new_data$month_type == 6,"June")
new_data$clock_type <- replace(new_data$clock_type,new_data$clock_type == 0,24)
# coffe_type 分類
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="其他","Other")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="那堤","Latte")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="咖啡星冰樂","Latte")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="雲朵冰搖咖啡","Latte")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="鮮奶咖啡","Latte")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="冷萃咖啡","Americano")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="曼巴咖啡","Americano")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="氣泡通寧","Americano")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="濃縮","Americano")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="冰咖啡","Americano")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="西西里","Americano")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="美式","Americano")
new_data$coffe_type <- replace(new_data$coffe_type ,new_data$coffe_type =="拿鐵","Latte")
# 匯出資料
export(new_data,"all_merge4.csv")

#---------------------------------------------------------------------#
#                             資料整理（2）                             #
#---------------------------------------------------------------------#
# 因發現7-11和全家的資料中仍有其他優惠組合，故另手動抓取關鍵字排除 詳見附件
special <- read.csv("咖啡品項與價格 - 需排除品項.csv") # 匯入整理好需排除的咖啡品項csv
special <- data.table(special)
coffee <- read_csv("all_merge4.csv")
coffee$special_type <- special$name[match(coffee$name, special$name)] # fill and match 品項 name，並標上優惠代碼
coffee$special_type <- coffee$special_type %>% replace_na('normal') # 將非優惠品項設為 normal
# 匯出資料
write.csv(coffee,file="all_merge5.csv",row.names = FALSE)

#---------------------------------------------------------------------#
#                             資料整理（3）                             #
#---------------------------------------------------------------------#
all_merge <- read.csv("all_merge5.csv")
library(dplyr)
all_merge <- distinct(all_merge)  # 刪除重複的 observaitons
all_merge <- subset(all_merge, select = -c(datetime_UTC_8, deviceid, invo_idx, id, county_district, names))  # 刪除部分變數
# 取出 special_type 為 "normal" 的 observations
coffee <- subset(all_merge, (all_merge$special_type=="normal"))
coffee <- subset(coffee, select = -special_type )
# 刪除 uniprice > 200 的 observaitons
coffee <- subset(coffee, (coffee$uniprice<=200))
saveRDS(coffee, file = "coffee.rds")
# 由於星巴克未標注冷熱的飲品皆為熱飲（冷飲才會額外標注），因此將星巴克 tep_type 為 "non" 者改為 "熱"
coffee[which(coffee$tep_type =="non"&coffee$channel =="星巴克"),"tep_type"] <- "熱"
# 只取出 quant*uniprice=totprice 的 observations，其他視為有誤之資料
coffee <- subset(coffee,coffee$quant*coffee$uniprice==coffee$totprice)
# 匯出檔案
# saveRDS(coffee, file = "coffee.rds")  # 節省後續分析的讀檔時間
write.csv(coffee, file = "coffee.csv")  # 最終版資料集

#---------------------------------------------------------------------#
#                      Preprocessing for Analysis                     #
#---------------------------------------------------------------------#
# coffee <- readRDS("coffee.rds")
coffee <- read.csv("coffee.csv")
coffee <- subset(coffee,coffee$size_type!="non"&coffee$tep_type!="non")  # 刪除 size_type 為 non 或是 tep_type 為 non 的資料
# 觀察五間不同銷售通路的總交易筆數、平均單價、總銷售額、總銷售量
groupsort <- group_by(coffee, channel)%>%summarise(transactions=n(),mean_unitprice=mean(uniprice),sum_totalprice=sum(totprice),mean_totalprice=mean(totprice),sum_quantity=sum(quant),mean_quantity=mean(quant))

#---------------------------------------------------------------------#
#                                 EDA                                 #
#---------------------------------------------------------------------#
library(ggplot2)
library(dplyr)
# (1) 通路與單價的關係
groupchuni <- group_by(coffee,channel,uniprice)%>%summarise(transactions=n(),sumtotprice=sum(totprice),meantotprice=mean(totprice),sumquant=sum(quant),meanquant=mean(quant))
plot(groupchuni$channel,groupchuni$uniprice,ylab="Unit Price",xlab="Channel",main="通路與單價的關係")

#-----------------1. 與銷售總額的關係（以不同銷售通路區分）-----------------#
# (1) 不同咖啡溫度
grouptep_type <- group_by(coffee,channel,tep_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),meantotprice=mean(totprice))
qplot(grouptep_type$tep_type,grouptep_type$sumtotprice,data=grouptep_type,color=channel,xlab="Temperature",ylab="Total Price",main="不同咖啡溫度的銷售總額")+theme(plot.title=element_text(hjust=0.5))
# (2) 不同咖啡大小
groupsize <- group_by(coffee,channel,size_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),meantotprice=mean(totprice),sumquant=sum(quant),meanquant=mean(quant))
qplot(groupsize$size_type,groupsize$sumtotprice,data=groupsize,color=channel,xlab="Size Type",ylab="Total Price",main="不同咖啡大小的銷售總額")+theme(plot.title=element_text(hjust=0.5))
# (3) 不同銷售月份
groupmonth_type <-group_by(coffee,channel,month_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),meantotprice=mean(totprice))
qplot(groupmonth_type$month_type,groupmonth_type$sumtotprice,data=groupmonth_type,color=channel,xlab="Month",ylab="Total Price",main="不同月份的銷售總額")+theme(plot.title=element_text(hjust=0.5))
# (4) 平日或假日
groupweek <- group_by(coffee,channel,week_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),meantotprice=mean(totprice),sumquant=sum(quant),meanquant=mean(quant))
qplot(groupweek$week_type,groupweek$sumtotprice,data=groupweek,color=channel,xlab="Week Type",ylab="Total Price",main="平日、假日的銷售總額")+theme(plot.title=element_text(hjust=0.5))
# (5) 不同咖啡類型
groupcoffe_type <-group_by(coffee,channel,coffe_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),meantotprice=mean(totprice))
qplot(groupcoffe_type$coffe_type,groupcoffe_type$sumtotprice,data=groupcoffe_type,color=channel,xlab="Coffee Type",ylab="Total Price",main="不同咖啡類型的銷售總額")+theme(plot.title=element_text(hjust=0.5))
# (6) 不同時點（早、中、晚）
groupclock_type <- group_by(coffee,channel,clock_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),sumquant=sum(quant),meantotprice=mean(totprice))
qplot(groupclock_type$clock_type,groupclock_type$sumtotprice,data=groupclock_type,color=channel,xlab="Clock",ylab="Total Price",main="不同時點的銷售總額")+theme(plot.title=element_text(hjust=0.5))
# (7) 不同地區
grouparea <- group_by(coffee,channel,area_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),meantotprice=mean(totprice),sumquant=sum(quant),meanquant=mean(quant))
qplot(grouparea$area_type,grouparea$sumtotprice,data=grouparea,color=channel,xlab="Area",ylab="Totol Price",main="地區與總銷售額關係")+theme(plot.title=element_text(hjust=0.5),axis.text.x=element_text(angle=270,vjust=0.5))
# (8) 不同單價
groupuni <- group_by(coffee,channel,uniprice)%>%summarise(transactions=n(),sumtotprice=sum(totprice),meantotprice=mean(totprice),sumquant=sum(quant),meanquant=mean(quant))
qplot(groupuni$uniprice,groupuni$sumtotprice,data=groupuni,color=channel,xlab="Unit Price",ylab="Totol Price",main="單價與總銷售額關係")+theme(plot.title=element_text(hjust=0.5))

#-------------------2. 與銷售總額的關係（以不同時點區分）------------------#
# (1) 溫度與總銷售總額關係
grouptepclock <- group_by(coffee,clock_type,tep_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),meantotprice=mean(totprice),sumquant=sum(quant),meanquant=mean(quant))
qplot(grouptepclock$tep_type,grouptepclock$sumtotprice,data=grouptepclock,color=clock_type,xlab="Temperature",ylab="Totol Price",main ="溫度和時間的關係")+theme(plot.title=element_text(hjust=0.5))

#------------------3. 與銷售量的關係（以不同咖啡大小區分）------------------#
# (1) 溫度與總銷售量關係
grouptepsize <- group_by(coffee,size_type,tep_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),meantotprice=mean(totprice),sumquant=sum(quant),meanquant=mean(quant))
gplot(grouptepsize,aes(x=factor(grouptepsize$size_type),y=grouptepsize$sumquant,colour=tep_type,group=grouptepsize$tep_type))+geom_line(size=1)+labs(x="Size Type",y="Quantity",title="溫度和大小的關係")+theme(plot.title=element_text(hjust=0.5)) 
grouptepsize$size_type=factor(grouptepsize$size_type,levels=c("小","中","大","特大"))

#---------------------------------------------------------------------#
#                             Model Building                          #
#---------------------------------------------------------------------#
# 依照月份合併
library(dplyr)
group1 <- group_by(coffee,area_type,month_type,week_type,clock_type,channel,coffe_type,size_type,tep_type)%>%summarise(transactions=n(),sumtotprice=sum(totprice),meantotprice=mean(totprice),sumquant=sum(quant),meanquant=mean(quant))

# Model 1
lm1 <- lm(sumtotprice ~ area_type+month_type+week_type+clock_type+channel+coffe_type+size_type+tep_type, data = group1)
summary(lm1)
par(mfrow=c(1,3))
plot(fitted(lm1), residuals(lm1), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lm1), main="Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals(lm1), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lm1))

# Studentized residuals
group1$stuout3 <- rstandard(lm1)
coffee2 <- subset(group1, group1$stuout3>(-3) & group1$stuout3<3)

# Model 2 (刪除異常值)
lm2 <- lm(sumtotprice ~ area_type+month_type+week_type+clock_type+channel+coffe_type+size_type+tep_type, data = coffee2)
summary(lm2)
par(mfrow=c(1,3))
plot(fitted(lm2), residuals(lm2), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lm2), main="Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals(lm2), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lm2))

# log(Y) & Relevel
coffee2$logTotprice <- log(coffee2$sumtotprice)
coffee2$area_type <- relevel(factor(coffee2$area_type), ref="臺北市")
coffee2$channel <- relevel(factor(coffee2$channel), ref="seven")
par(mfrow=c(1,2))
hist(coffee2$sumtotprice, xlab="totprice", main="Histogram of totprice", breaks=10)
hist(coffee2$logTotprice, xlab="logTotprice", main="Histogram of recipTotprice", breaks=10)

# Model 3 (取 log)
lm3 <- lm(logTotprice ~ area_type+month_type+week_type+clock_type+channel+coffe_type+size_type+tep_type, data = coffee2)
summary(lm3)
par(mfrow=c(1,3))
plot(fitted(lm3), residuals(lm3), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lm3), main="Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals(lm3), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lm3))

#---------------------------------------------------------------------#
#                         Channel Model Building                      #
#---------------------------------------------------------------------#
group1$logTotprice <- log(group1$sumtotprice)
# Reference group relevel
group1$area_type <- relevel(factor(group1$area_type), ref="臺北市")
group1$channel <- relevel(factor(group1$channel), ref="seven")

# 分不同 channel
groupseven <- subset(group1, (group1$channel=="seven"))
groupfmart <- subset(group1, (group1$channel=="全家"))
groupstarbucks <- subset(group1, (group1$channel=="星巴克"))
grouplouisa <- subset(group1, (group1$channel=="路易莎"))
groupcama <- subset(group1, (group1$channel=="cama"))

#--------------------------------7-11---------------------------------#
# Model
lmseven <- lm(logTotprice ~ area_type + month_type + week_type + clock_type + coffe_type + size_type + tep_type, data = groupseven)
summary(groupseven)
# Residual plots
par(mfrow=c(1,3))
plot(fitted(lmseven), residuals(lmseven), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmseven), main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals(lmseven), main="QQ-plot of Residuals", ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmseven))
# 刪除異常值
groupseven$stuout3 <- rstandard(lmseven)
groupseven2 <- subset(groupseven, groupseven$stuout3>(-3) & groupseven$stuout3<3)
# Model 2
lmseven2 < -lm(logTotprice ~ area_type+month_type+week_type+clock_type+coffe_type+size_type+tep_type, data = groupseven2)
summary(lmseven2)
# ANOVA table
anova(lmseven2)
# Residual plots
par(mfrow=c(1,3))
plot(fitted(lmseven2), residuals(lmseven2), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmseven2), main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals(lmseven2), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmseven2))

#---------------------------------全家---------------------------------#
# Model
lmfmart <- lm(logTotprice ~ area_type + month_type + week_type + clock_type + coffe_type + size_type + tep_type, data = groupfmart)
summary(lmfmart)
# Residual Plots
par(mfrow=c(1,3))
plot(fitted(lmfmart), residuals(lmfmart), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmfmart), main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals(lmfmart), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmfmart))
# 刪除異常值
groupfmart$stuout3 <- rstandard(lmfmart)
groupfmart2 <- subset(groupfmart, groupfmart$stuout3>(-3) & groupfmart$stuout3<3)
# Model 2
lmfmart2 <- lm(logTotprice ~ area_type+month_type+week_type+clock_type+coffe_type+size_type+tep_type, data = groupfmart2)
summary(lmfmart2)
# ANOVA table
anova(lmfmart2)
# Residual Plots
par(mfrow=c(1,3))
plot(fitted(lmfmart2), residuals(lmfmart2), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmfmart2), main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals(lmfmart2), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmfmart2))

#--------------------------------星巴克--------------------------------#
# Model
lmstar<-lm(logTotprice ~ area_type + month_type + week_type + clock_type + coffe_type + size_type + tep_type, data = groupstarbucks)
summary(lmstar)
# Residual plots
par(mfrow=c(1,3))
plot(fitted(lmstar), residuals(lmstar), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmstar), main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals(lmstar), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmstar))
# 刪除異常值
groupstarbucks$stuout3 <- rstandard(lmstar)
groupstarbucks2 <- subset(groupstarbucks, groupstarbucks$stuout3>(-3) & groupstarbucks$stuout3<3)
# Model 2
lmstar2<-lm(logTotprice ~ area_type + coffe_type + tep_type + size_type + month_type + clock_type + week_type, data = groupstarbucks2)
summary(lmstar2)
# ANOVA table
anova(lmstar2)
# Residual plots
par(mfrow=c(1,3))
plot(fitted(lmstar2), residuals(lmstar2), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmstar2), main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals(lmstar2), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmstar2))

#--------------------------------路易莎--------------------------------#
# Model
lmlouisa<-lm(logTotprice ~ area_type + month_type + week_type + clock_type + coffe_type + size_type + tep_type, data = grouplouisa)
summary(lmlouisa)
# Residual plots
par(mfrow=c(1,3))
plot(fitted(lmlouisa), residuals(lmlouisa), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmlouisa), main="Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals(lmlouisa), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmlouisa))
# 刪除異常值
grouplouisa$stuout3 <- rstandard(lmstar)
grouplouisa2 <- subset(grouplouisa, grouplouisa$stuout3>(-3) & grouplouisa$stuout3<3)
# Model 2
lmlouisa2<-lm(logTotprice ~ area_type + month_type + week_type + clock_type + coffe_type + size_type + tep_type, data = grouplouisa2)
summary(lmlouisa2)
# ANOVA table
anova(lmlouisa2)
# Residual plots
par(mfrow=c(1,3))
plot(fitted(lmlouisa2), residuals(lmlouisa2), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmlouisa2), main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals(lmlouisa2), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmlouisa2))

#--------------------------------Cama---------------------------------#
# Model
lmcama<-lm(logTotprice ~ area_type + month_type + week_type + clock_type + coffe_type + size_type + tep_type, data = groupcama)
summary(lmcama)
# Residual Plots
par(mfrow=c(1,3))
plot(fitted(lmcama), residuals(lmcama), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmcama), main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals(lmcama), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmcama))
# 刪除異常值
groupcama$stuout3 <- rstandard(lmcama)
groupcama2 <- subset(groupcama, groupcama$stuout3>(-3) & groupcama$stuout3<3)
# Model 2
lmcama2<-lm(logTotprice ~  area_type + coffe_type + tep_type + size_type + month_type + clock_type + week_type, data = groupcama2)
summary(lmcama2)
# ANOVA table
anova(lmcama2)
# Residual plots
par(mfrow=c(1,3))
plot(fitted(lmcama2), residuals(lmcama2), main="Residual Plots", xlab="fitted", ylab="Residuals", cex=0.4, pch=19) 
abline(h=0)
hist(residuals(lmcama2), main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals(lmcama2), main="QQ-plot of Residuals",ylab="Residuals", cex=0.4, pch=19)
qqline(residuals(lmcama2))

