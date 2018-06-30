library(fpc)
library(tidyverse)

#-----------------Read Data
# NOTE
# Data is taken from Fangraphs 2018 batters leaderboard
# qualified hitters only and added zone%, contact%, z-contact%, o-contact%,
# swing%, O-swing%, and Z-swing%


datatest <- read.csv('datatest.csv')

#----------------Clean Data

datatest <- na.omit(data) # listwise deletion of missing

datatest$BB. <- as.numeric(sub("%", "", datatest$BB.)) * .01
datatest$K. <- as.numeric(sub("%", "", datatest$K.)) * .01

for(i in 22:29){
  datatest[,i] <- as.numeric(sub("%", "", datatest[,i])) * .01
}

eye <- datatest[,c(26,27,28)]


#--------------------Determine number of clusters
wss <- (nrow(eye)-1)*sum(apply(eye,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(eye, 
                                     centers=i)$withinss)
# PLOT ELBOW
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#-------------------Generate Final dataframe eye.final

eyetest.fit <- kmeans(eye, 5)
aggregate(data,by=list(eyetest.fit$cluster),FUN=mean)
eyetest.final <- data.frame(datatest, eyetest.fit$cluster)
eyetest.final <- eyetest.final[,c(1,9,10,22,23,24,25,26,27,28,30)]

#View(eyetest.final)

#-------------------Look at swing rates by group

#View(eyetest.fit$centers)

# -------------------get walk and k averages by group in dataframe outcome.data

walk.avg <- NULL
k.avg <- NULL

for(i in 1:5){
  temp <- eye.final %>% 
    filter(eye.fit.cluster == i)
  walk.avg[i] <- mean(temp$BB.)
  k.avg[i] <- mean(temp$K.)
}
outcome.data <- data.frame(cbind(walk.avg,k.avg))
colnames(outcome.data) <- c('BB%','K%')

#View(outcome.data)

#---------------- get swing and contact rates by group in process.data

ballsw.avg <- NULL
ksw.avg <- NULL
ballc <- NULL
kc <- NULL
c <- NULL

for(i in 1:5){
  temp <- eye.final %>% 
    filter(eye.fit.cluster == i)
  ballsw.avg[i] <- mean(temp$O.Swing...pi.)
  ksw.avg[i] <- mean(temp$Z.Swing...pi.)
  ballc[i] <- mean(temp$O.Contact...pi.)
  kc[i] <- mean(temp$Z.Contact...pi.)
  c[i] <- mean(temp$Contact...pi.)
}
process.data <- data.frame(cbind(ballsw.avg,ksw.avg,ballc,kc,c))
colnames(process.data) <- c('O-Swing%','Z-Swing%','O-Contact%','Z-Contact%','Contact%')

#View(process.data)

#-------------- Correlation Tests and plots for swing rates and K% and BB%

cor.test(eye.final$Z.Swing...pi.,eye.final$K.)
cor.test(eye.final$Z.Swing...pi.,eye.final$BB.)
cor.test(eye.final$Z.Contact...pi.,eye.final$K.)


plot(eye.final$Z.Contact...pi.,eye.final$K.)
plot(eye.final$O.Contact...pi.,eye.final$K.)

# Histogram

eye.final %>% 
  ggplot(aes(x=eye.fit.cluster)) + geom_histogram(bins = 5)


fit <- kmeans(data[,-c(1,2)], 5) # 5 cluster solution

# get cluster means 
aggregate(data,by=list(fit$cluster),FUN=mean)

# append cluster assignment
yaa <- data.frame(data, fit$cluster)
