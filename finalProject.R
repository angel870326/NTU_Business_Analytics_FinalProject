setwd("自己打 working directory")

# 只要跑一次存好檔，讀檔比較快
# all_merge <- read.csv("all_merge4.csv")
# library(dplyr)
# all_merge <- distinct(all_merge)
# all_merge <- subset(all_merge, select = -c(2,3,5,6) )
# saveRDS(all_merge, file = "all_merge.rds")

all_merge <- readRDS("all_merge.rds")
coffee <- subset(all_merge, (all_merge$special_type=="normal"))

pairs(coffee[,c(6,4,5,7,15,17,18)], pch=19, cex=0.5)







# 不同 channel
summary(all_merge$channel)
channel_711 <- subset(all_merge, (all_merge$channel=="7-11"))
channel_louisa <- subset(all_merge, (all_merge$channel=="路易莎"))
channel_family <- subset(all_merge, (all_merge$channel=="全家"))
channel_starbucks <- subset(all_merge, (all_merge$channel=="星巴克"))
channel_cama <- subset(all_merge, (all_merge$channel=="cama"))
