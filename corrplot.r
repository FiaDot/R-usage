
library(data.table)
library(ggplot2)
library(corrplot) 

# install.packages("corrplot")
# install.packages("data.table")

setwd("D:/data")
getwd()

# d = read.csv("DEF_TICK_CHECK_result.csv") 
#rawData1 = read.csv("DEF_BULLET_POWER_result.csv") 
#rawData1 = read.csv("DEF_GOTO_DIST_result.csv") 
#rawData1 = read.csv("DEF_RANDOM_MIN_result.csv") 
#rawData1 = read.csv("DEF_RANDOM_POINT_result.csv") 
#rawData1 = read.csv("DEF_RANDOM_RANGE_result.csv") 
#rawData1 = read.csv("DEF_RISK_BULLET_result.csv") 
#rawData1 = read.csv("DEF_RISK_ENEMY_result.csv") 
#rawData1 = read.csv("DEF_RISK_FUEL_result.csv") 


#eMean <- aggregate(d[,6], list(d$type, d$val), mean)
# 위와 동일한 처리(20배 빠름)



show_me_data <- function(filename) {
  
  d = read.csv(filename) 
  
  dt <- data.table(d)
  #dt1 <- dt[, mean(score), by="type,val"]
  #setnames(dt1, "V1", "mean")
  
  dt$key = paste(dt$type, dt$val, sep="-")
  
  
  dt1 <- dt[, mean(score), by="key"]
  setnames(dt1, "V1", "mean")
  
  dt2 <- dt[, median(score), by="key"]
  setnames(dt2, "V1", "median")  
  
  dt3 <- dt[, max(score), by="key"]
  setnames(dt3, "V1", "max")
  
  dt4 <- dt[, min(score), by="key"]
  setnames(dt4, "V1", "min")
  
  dt5 <- dt[, sd(score), by="key"]
  setnames(dt5, "V1", "sd")
  
  dt6 <- dt[, sum(score), by="key"]
  setnames(dt6, "V1", "sum")  
  
  setkey(dt1, key)
  setkey(dt2, key)
  setkey(dt3, key)
  setkey(dt4, key)
  setkey(dt5, key)
  setkey(dt6, key)
  
  dt12 <- merge(dt1, dt2)
  dt34 <- merge(dt3, dt4)
  dt56 <- merge(dt5, dt6)
  
  dtall <- merge(dt12, dt34)
  dtall <- merge(dtall, dt56)
  
  dtall$type = as.character(lapply(strsplit(as.character(dtall$key), split="-"), "[", 1))
  dtall$val = as.numeric(lapply(strsplit(as.character(dtall$key), split="-"), "[", 2))
  
  # remove cols
  #dtall[, c('key','type') := NULL]
  
  #pairs(~mean+max+min+sd+val,data=dtall, main=filename)  
 
  png_filename <- paste(filename, ".png")
  png(png_filename,
      ,width=1280,
      height=768      
      )
  pairs(~mean+max+min+sd+val,data=dtall, main=filename)  
  dev.off()
 
  
  print(filename)
  
  dtall[, c('key') := NULL]
  
  return(dtall)
}





dd <- show_me_data("DEF_TICK_CHECK_result.csv")
with(dd, cor.test(mean, val))
total <- dd[order(val)] 

dd <- show_me_data("DEF_BULLET_POWER_result.csv") 
with(dd, cor.test(mean, val))
total <- rbind(total, dd[order(val)] )

dd <- show_me_data("DEF_RANDOM_POINT_result.csv") 
with(dd, cor.test(mean, val))
total <- rbind(total, dd[order(val)] )

dd <- show_me_data("DEF_RANDOM_MIN_result.csv") 
with(dd, cor.test(mean, val))
total <- rbind(total, dd[order(val)] )

dd <- show_me_data("DEF_RANDOM_RANGE_result.csv") 
with(dd, cor.test(mean, val))
total <- rbind(total, dd[order(val)] )

dd <- show_me_data("DEF_RISK_ENEMY_result.csv") 
with(dd, cor.test(mean, val))
total <- rbind(total, dd[order(val)] )

dd <- show_me_data("DEF_RISK_BULLET_result.csv") 
with(dd, cor.test(mean, val))
total <- rbind(total, dd[order(val)] )


dd <- show_me_data("DEF_RISK_FUEL_result.csv") 
with(dd, cor.test(mean, val))
total <- rbind(total, dd[order(val)] )

dd <- show_me_data("DEF_GOTO_DIST_result.csv") 
with(dd, cor.test(mean, val))
total <- rbind(total, dd[order(val)] )


write.csv(total, file="total.csv")









# test code

filename <- "DEF_TICK_CHECK_result.csv"


d = read.csv(filename) 

dt <- data.table(d)
#dt1 <- dt[, mean(score), by="type,val"]
#setnames(dt1, "V1", "mean")

dt$key = paste(dt$type, dt$val, sep="-")

dt1 <- dt[, mean(score), by="key"]
setnames(dt1, "V1", "mean")

dt2 <- dt[, median(score), by="key"]
setnames(dt5, "V1", "median")  

dt3 <- dt[, max(score), by="key"]
setnames(dt2, "V1", "max")

dt4 <- dt[, min(score), by="key"]
setnames(dt3, "V1", "min")

dt5 <- dt[, sd(score), by="key"]
setnames(dt4, "V1", "sd")

dt6 <- dt[, sum(score), by="key"]
setnames(dt6, "V1", "sum")  

setkey(dt1, key)
setkey(dt2, key)
setkey(dt3, key)
setkey(dt4, key)
setkey(dt5, key)
setkey(dt6, key)

dt12 <- merge(dt1, dt2)
dt34 <- merge(dt3, dt4)
dt56 <- merge(dt5, dt6)

dtall <- merge(dt12, dt34)
dtall <- merge(dtall, dt56)

dtall$type = as.character(lapply(strsplit(as.character(dtall$key), split="-"), "[", 1))
dtall$val = as.numeric(lapply(strsplit(as.character(dtall$key), split="-"), "[", 2))



# correlation
#cor(dtall$mean, dtall$val)

#with(dtall, plot(mean, val))

print(filename)

# 유의수준
with(dtall, cor.test(mean, val))



# remove cols
#dtall[, c('key','type') := NULL]

#pairs(~mean+max+min+sd+val,data=dtall, main=filename)  

png_filename <- paste(filename, ".png")
png(png_filename,
    ,width=1280,
    height=768      
)
pairs(~mean+max+min+sd+val,data=dtall, main=filename)  
dev.off()

dtall







