setwd("自己打 working directory")

# 只要跑一次存好檔，讀檔比較快
# all_merge <- read.csv("all_merge4.csv")
# library(dplyr)
# all_merge <- distinct(all_merge)
# all_merge <- subset(all_merge, select = -c(2,3,5,6) )
all_merge <- subset(all_merge, select = -c(3,8) )
# saveRDS(all_merge, file = "all_merge.rds")

all_merge <- readRDS("all_merge.rds")
coffee <- subset(all_merge, (all_merge$special_type=="normal"))

pairs(coffee[,c(6,4,5,7,15,17,18)], pch=19, cex=0.5)

# name、county_district 拿掉
lm1 <- lm(totprice ~ area_type + channel + quant + uniprice + invo_price + coffe_type + tep_type + size_type + month_type + clock_type + Controlable_income + number, data = coffee)

lm0.1 <- lm(totprice ~ area_type, data = coffee)
summary(lm0.1)
lm0.2 <- lm(totprice ~ channel, data = coffee)
summary(lm0.2)
lm0.3 <- lm(totprice ~ name, data = coffee)
summary(lm0.3) # name 要刪掉


lm0.1 <- lm(totprice ~ month_type, data = coffee)
summary(lm0.1)






# 不同 channel
summary(all_merge$channel)
channel_711 <- subset(all_merge, (all_merge$channel=="7-11"))
channel_louisa <- subset(all_merge, (all_merge$channel=="路易莎"))
channel_family <- subset(all_merge, (all_merge$channel=="全家"))
channel_starbucks <- subset(all_merge, (all_merge$channel=="星巴克"))
channel_cama <- subset(all_merge, (all_merge$channel=="cama"))
