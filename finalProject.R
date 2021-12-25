setwd("自己打 working directory")

# 只要跑一次存好檔，讀檔比較快
# coffee <- read.csv("NTU_Coffee.csv")
# saveRDS(coffee, file = "NTU_Coffee.rds")

coffee <- readRDS("NTU_Coffee.rds")

# 不同 channel
summary(coffee$channel)
channel_711 <- subset(coffee, (coffee$channel=="7-11"))
channel_louisa <- subset(coffee, (coffee$channel=="路易莎"))
channel_family <- subset(coffee, (coffee$channel=="全家"))
channel_starbucks <- subset(coffee, (coffee$channel=="星巴克"))
channel_cama <- subset(coffee, (coffee$channel=="cama"))
