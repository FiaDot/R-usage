
library(data.table)
library(ggplot2)
library(corrplot) 

# install.packages("corrplot")
# install.packages("data.table")

setwd("D:/data")
getwd()


show_me_data <- function(filename) {
  
  d = read.csv(filename) 
  
  dt <- data.table(d)
  
  dt1 <- dt[, mean(score), by="group"]
  setnames(dt1, "V1", "mean")
  
  dt2 <- dt[, median(score), by="group"]
  setnames(dt2, "V1", "median")  
  
  dt3 <- dt[, max(score), by="group"]
  setnames(dt3, "V1", "max")
  
  dt4 <- dt[, min(score), by="group"]
  setnames(dt4, "V1", "min")
  
  dt5 <- dt[, sd(score), by="group"]
  setnames(dt5, "V1", "sd")
  
  dt6 <- dt[, sum(score), by="group"]
  setnames(dt6, "V1", "sum")  
  
  setkey(dt1, group)
  setkey(dt2, group)
  setkey(dt3, group)
  setkey(dt4, group)
  setkey(dt5, group)
  setkey(dt6, group)
  
  dt12 <- merge(dt1, dt2)
  dt34 <- merge(dt3, dt4)
  dt56 <- merge(dt5, dt6)
  
  dtall <- merge(dt12, dt34)
  dtall <- merge(dtall, dt56)
  
  
  print(filename)
  
  dtall[, c('key') := NULL]
  
  return(dtall)
}



cnt <- 1

filename <- paste(cnt, "_result.csv", sep="")

dd <- show_me_data(filename)


save_filename <- paste(cnt, "_stat.csv", sep="")

write.csv(dd, file=save_filename)









# test code


filename <- "1_result.csv"

d = read.csv(filename) 

dt <- data.table(d)

dt1 <- dt[, mean(score), by="group"]
setnames(dt1, "V1", "mean")

dt2 <- dt[, median(score), by="group"]
setnames(dt2, "V1", "median")  

dt3 <- dt[, max(score), by="group"]
setnames(dt3, "V1", "max")

dt4 <- dt[, min(score), by="group"]
setnames(dt4, "V1", "min")

dt5 <- dt[, sd(score), by="group"]
setnames(dt5, "V1", "sd")

dt6 <- dt[, sum(score), by="group"]
setnames(dt6, "V1", "sum")  

# options
dt7 <- dt[, mean(tick), by="group"]
setnames(dt7, "V1", "tick")

dt8 <- dt[, mean(bulpw), by="group"]
setnames(dt8, "V1", "bulpw")

dt9 <- dt[, mean(pt), by="group"]
setnames(dt9, "V1", "pt")

dt10 <- dt[, mean(min), by="group"]
setnames(dt10, "V1", "mini")

dt11 <- dt[, mean(range), by="group"]
setnames(dt11, "V1", "range")

dt12 <- dt[, mean(enemy), by="group"]
setnames(dt12, "V1", "enemy")

dt13 <- dt[, mean(bullet), by="group"]
setnames(dt13, "V1", "risk")

dt14 <- dt[, mean(fuel), by="group"]
setnames(dt14, "V1", "fuel")

dt15 <- dt[, mean(dist), by="group"]
setnames(dt15, "V1", "dist")




setkey(dt1, group)
setkey(dt2, group)
setkey(dt3, group)
setkey(dt4, group)
setkey(dt5, group)
setkey(dt6, group)

setkey(dt7, group)
setkey(dt8, group)
setkey(dt9, group)
setkey(dt10, group)
setkey(dt11, group)
setkey(dt12, group)
setkey(dt13, group)
setkey(dt14, group)
setkey(dt15, group)

dtall <- dt1
dtall <- merge(dtall, dt2)
dtall <- merge(dtall, dt3)
dtall <- merge(dtall, dt4)
dtall <- merge(dtall, dt5)
dtall <- merge(dtall, dt6)
dtall <- merge(dtall, dt7)
dtall <- merge(dtall, dt8)
dtall <- merge(dtall, dt9)
dtall <- merge(dtall, dt10)
dtall <- merge(dtall, dt11)
dtall <- merge(dtall, dt12)
dtall <- merge(dtall, dt13)
dtall <- merge(dtall, dt14)
dtall <- merge(dtall, dt15)


save_filename <- paste(filename, "_stat.csv", sep="")

write.csv(dtall, file=save_filename)


png_filename <- paste(filename, ".png")
png(png_filename,
    ,width=1920,
    height=1080      
)


# sampling
ind <- sample(2, nrow(d), replace = TRUE, prob = c(0.95, 0.05))
trainData <- d[ind == 2, ]
plot(trainData)




plot(dtall)
#pairs(~mean+max+min+sd+sum+tick+bullet+pt+mini+range+enemy+fuel+risk+dist,data=ddd, main=filename)  
dev.off()


print(filename)

dtall[, c('key') := NULL]


