rm(list = ls())
#setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")
setwd("/Users/jingli/Desktop/Eyetracking/R/eyetracking_git")

df <- read.csv("./studyResults_without_Sandra.csv", header=T)

boxplot(leak~Difficulty, data = AoiFixdur)

a <- AoiFixdur
a <- a[(a$leak == 0) == F,]
a$leak <- log(a$leak)
a$leakNodes <- log(a$leakNodes)
a$otherNodes <- log(a$otherNodes)

g <- lm(otherNodes~Difficulty, data = a)

plot(g, which = 1)
plot(g, which = 2)

library(MASS)
boxcox(otherNodes ~ ., data = AoiFixdur)
par(mfrow = c(3,2))
boxplot(leak~Difficulty, data = a)
boxplot(leak~Difficulty, data = AoiFixdur)
boxplot(leakNodes~Difficulty, data = a)
boxplot(leakNodes~Difficulty, data = AoiFixdur)
boxplot(otherNodes~Difficulty, data = a)
boxplot(otherNodes~Difficulty, data = AoiFixdur)
par(mfrow = c(1,1))


## Jia Kai data
b <- df
b$Time.in.page <- log(b$Time.in.page)

gl <- lm(Time.in.page ~ Difficulty, data = b)

plot(gl, which = 1)
plot(gl, which = 2)

par(mfrow = c(3,2))
boxplot(Accuracy~Difficulty, data = df)
boxplot(Accuracy~Difficulty, data = b)
boxplot(Time.in.page~Difficulty, data = df)
boxplot(Time.in.page~Difficulty, data = b)
boxplot(Last.click.time~Difficulty, data = df)
boxplot(Last.click.time~Difficulty, data = b)
par(mfrow = c(1,1))

boxcox(Last.click.time ~ Difficulty, data = df)
boxcox(Time.in.page ~ Difficulty, data = df)

plot(df$Name[which(df$Difficulty == 0)], df$Time.in.page[which(df$Difficulty == 0)])
