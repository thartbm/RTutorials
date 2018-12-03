setwd("~/Desktop/R tutorials/2017/Tutorial 2")

load('Tutorial_2A_IQscores.rda')
IQscores <- IQscores$IQscores 

par(mfrow=c(1,3))
boxplot(IQscores, main='Boxplot of IQ scores', ylab='IQ scores')
hist(IQscores, xlab='IQ scores')
qqnorm(IQscores)

print(t.test(IQscores, alternative='t', mu=100))
