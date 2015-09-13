# install.packages("party")
# install.packages("rpart")

library(rpart)

library(party)
library(Matrix)
library(arules)
library(arulesSequences)
library(arulesViz)

library(ggplot2)
library(Cairo)



#rawFileName <- "action-player600-game10000"
rawFileName <- "action-player600-game10000-190425"
#rawFileName <- "action-player600-game10000-151235"

################################################################################
# Initialize
################################################################################

initWindows <- function() { 
  setwd("D:/research-paper/source/log_generator_java/output_20141211")
  getwd()
  
  fileName <- paste(rawFileName, ".csv", sep="")
  print(fileName)
  return(fileName)
}

initMac <- function() {
  setwd("/Users/fiadot/research-paper/source/log_generator_java/output_20141211")
  getwd()
  
  fileName <- paste(rawFileName, "UTF8.csv", sep="")
  print(fileName)
  par(family="Apple SD Gothic Neo")  
  
  # install.packages("extrafont")
  # library(extrafont)
  # font_import()
  # fonts()  
  
  #family="AppleMyungjo"
  # AppleGothic
  
  #par(family="AppleGothic")
  #plot(0, main="한글제목")
  #warnings()  
  return(fileName)
}

initLinux <- function() {
  print("Not supported.")  
}


switch(Sys.info()[['sysname']],
       Windows= {print("Windows detected...") 
                 rawFileName <- initWindows()},
       Linux  = {print("Linux detected...")
                 rawFileName <- initLinux()},
       Darwin = {print("Mac detected...") 
                 rawFileName <- initMac() })

################################################################################
# Loader 
################################################################################

rawData = read.csv(rawFileName) 
dfRawData <- data.frame(rawData)
#dfRawData$comb <- as.integer(dfRawData$char_comb)

winData <- subset(dfRawData, result == T)
loseData <- subset(dfRawData, result == F)


# 샘플 추출 
#set.seed(1234)
#ind <- sample(2, nrow(dfRawData), replace = TRUE, prob = c(0.7, 0.3))
#dfRawData <- rawData[ind == 1, ]
dfRawData <- dfRawData[,-1]


summary(dfRawData)

# scatter 
plot(dfRawData)


# another option
makePairs <- function(data) 
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}

draw_final_scatter <- function(df) {
  # expand iris data frame for pairs plot (species에 대해 제외)
  gg1 = makePairs(df[,-5])
  
  # new data frame mega iris
  mega_data = data.frame(gg1$all, result=rep(df$result, length=nrow(gg1$all)))
  
  # pairs plot 
  p <- ggplot(mega_data, aes_string(x = "x", y = "y")) + 
    facet_grid(xvar ~ yvar, scales = "free") +
    geom_point(aes(colour=result), na.rm = TRUE, alpha=0.8) +
    theme(text = element_text(size = 20, family="Arial"))
    
  ggsave(width=20, height=14, filename="scatter.png", plot=p, dpi=72)  
  
  
  # density
  p2 <- ggplot(mega_data, aes_string(x = "x", y = "y")) + 
    facet_grid(xvar ~ yvar, scales = "free") + 
    stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                 data = gg1$densities, position = "identity", 
                 colour = "grey20", geom = "line") +
    theme(text = element_text(size = 20, family="Arial"))
  
  ggsave(width=20, height=14, filename="scatter_density.png", plot=p2, dpi=72)    
}

# draw_final_scatter(dfRawData)


draw_scatter_set <- function() {
  # scatter + jitter
  ggplot(mega_data, aes_string(x = "x", y = "y")) + 
    facet_grid(xvar ~ yvar, scales = "free") +
    geom_jitter(aes(colour=result), na.rm = TRUE, alpha=0.8, position = position_jitter(width=.005,height=.005))
  
  # lm
  ggplot(mega_data, aes_string(x = "x", y = "y")) + 
    facet_grid(xvar ~ yvar, scales = "free") + 
    geom_smooth(method = "lm") +
    geom_point(aes(colour=result), na.rm = TRUE, alpha=0.8)
    
  
  #dfRawData$col <- NULL
  dfRawData$col <- ifelse(dfRawData$result == T, 1, 2)
  pairs(dfRawData[,1:4],col = dfRawData$col)
}






winTree <- ctree(resultfac ~ char1 + char2 + char3, data=dfRawData)
print(winTree)

plot(winTree)
plot(winTree, type="simple")
# text(winTree, use.n = TRUE)
plot(winTree$model)

# 이 함수가 하는 일은 predictor와 predicted value의 행렬을 만들어 
# raw R-square와 cross-validated R-square를 구해준다.
shrinkage <- function(fit, k=10){ 
  require(bootstrap)
  
  theta.fit <- function(x,y){lsfit(x,y)} 
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  
  x <- fit$model[,2:ncol(fit$model)] 
  y <- fit$model[,1]
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k) 
  r2 <- cor(y, fit$fitted.values)^2 
  r2cv <- cor(y, results$cv.fit)^2 
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n") 
  cat("Change =", r2-r2cv, "\n")
}


library(rpart)
fit <- rpart(resultfac ~ char1 + char2 + char3, data=dfRawData)
plot(fit)
printcp(fit)
text(fit)

library(MASS)

library(tree)
# install.packages("tree")

dt <- tree(result ~ elo + comb, data=dfRawData)
 

dt <- tree(elo ~ win + lose, data=dfRawData)
plot(dt)
summary(dt)
dt


# How to convert factor to integer ?
dfRawData$comb <- NULL
#dfRawData$comb <- as.numeric(as.character(dfRawData$char_comb))
#dfRawData$comb <- as.numeric(levels(dfRawData$char_comb))
dfRawData$comb <- as.integer(dfRawData$char_comb)


winData$comb <- as.integer(winData$char_comb)

summary(tree(comb ~ win + lose + elo, data=winData))
summary(tree(win ~elo, data=winData))






# Classification Tree with rpart
library(rpart)

# grow tree 
fit <- rpart(resultfac~ char1 + char2 + char3,
             data=dfRawData)
#plot(fit, compress = TRUE)
#text(fit, use.n = TRUE)
#text(fit, use.n=TRUE, all=TRUE, cex=.8)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit)
#text(fit, use.n = TRUE)
text(fit, pretty = 0, use.n=TRUE, all=TRUE, cex=.8)



# prune the tree 
pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE)
text(pfit, pretty = 0, use.n=TRUE, all=TRUE, cex=.8)


printcp(pfit)






# reference : http://rstudio-pubs-static.s3.amazonaws.com/4942_5f21cb99518244b98c358376f0acd8ca.html

## 데이터를 7:3의 비율로 trainData와 testData로 샘플링
set.seed(1234)
ind <- sample(2, nrow(dfRawData), replace = TRUE, prob = c(0.7, 0.3))
trainData <- dfRawData[ind == 1, ]
testData <- dfRawData[ind == 2, ]


rawdata_ctree <- ctree(resultfac ~ char1 + char2 + char3, data = trainData)
#summary(rawdata_ctree)
#print(rawdata_ctree)
#plot(rawdata_ctree)
#plot(rawdata_ctree, type="simple")

# trainData에 대하여 설정한 Tree Model을 이용하여 예측한 값을 출력
trainPred <- predict(rawdata_ctree, newdata = trainData)
#trainPred
summary(trainPred)

library(caret)
# confusionMatrix(trainPred, trainData$resultfac)

## 실제 trainData값과 예측값을 비교하는 교차표를 출력
trainResult <- table(predict(rawdata_ctree), trainData$resultfac)
trainAccuracy <- (trainResult[1] + trainResult[4]) / sum(trainResult) * 100
sprintf("train accuracy=%4.3f", trainAccuracy)

## trainData에 대하여 설정한 Tree Model을 이용하여 예측한 값을 출력
testPred <- predict(rawdata_ctree, newdata = testData)
#testPred

## 실제 trainData값과 예측값을 비교하는 교차표를 출력한다.
testResult <- table(testPred, testData$result)
testAccuracy <- (testResult[1] + testResult[4]) / sum(testResult) * 100
sprintf(" test accuracy=%4.3f", testAccuracy)











# http://www.statmethods.net/advstats/cart.html

#Classification tree:
#  tree(formula = Species ~ Sepal.Width + Petal.Width, data = iris)
#Number of terminal nodes:  5 
#Residual mean deviance:  0.204 = 29.57 / 145 
#Misclassification error rate: 0.03333 = 5 / 150

# convert logical to factor
dfRawData$resultfac <- as.factor(dfRawData$result)

# tree는 output이 factor 일 때만 misclassification error rate 를 보여준다
library(tree)
#tree1 <- tree(resultfac ~ char1 + char2 + char3 + elo + win + lose + draw- result, data = dfRawData)
treeTotal <- tree(resultfac ~ char1 + char2 + char3, data = dfRawData)
summary(treeTotal)
plot(treeTotal)
text(treeTotal)
treeTotal # print  of the tree 


#set.seed(1234)
#ind <- sample(2, nrow(dfRawData), replace = TRUE, prob = c(0.7, 0.3))
#trainData <- dfRawData[ind == 1, ]
#testData <- dfRawData[ind == 2, ]


treeTrain <- tree(resultfac ~ char1 + char2 + char3, data = trainData)
summary(treeTrain)
plot(treeTrain)
text(treeTrain, pretty=0)


treePredict <- predict(treeTrain, testData, type="class")
testResult <- with(testData, table(treePredict, resultfac))

testAccuracy <- (testResult[1] + testResult[4]) / sum(testResult) 
testAccuracy


# pruning
pruneTree <- cv.tree(treeTrain, FUN=prune.misclass)
plot(pruneTree)

pruneTree2 = prune.misclass(treeTrain, best = 3)
plot(pruneTree2)
text(pruneTree2, pretty = 0)

treePredict <- predict(pruneTree2, testData, type="class")
testResult <- with(testData, table(treePredict, resultfac))

testAccuracy <- (testResult[1] + testResult[4]) / sum(testResult) 
testAccuracy



kc <- kmeans(dfRawData$elo, 3)
plot(dfRawData[c("win", "elo")], col=kc$cluster)



