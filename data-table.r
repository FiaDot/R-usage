
library(data.table)
library(ggplot2)
library(corrplot) 

# install.packages("corrplot")
# install.packages("data.table")

setwd("D:/data")
getwd()




# test code


filename <- "1_result.csv_stat.csv"

d = read.csv(filename) 

dt <- data.table(d)



plot(dt)

