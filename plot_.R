rm(list = ls())
#setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")
setwd("/Users/jingli/Desktop/Eyetracking/R/eyetracking_git")

library(ggplot2)

df <- read.csv("./studyResults_without_Sandra.csv", header=T)

boxplot(leak~Difficulty, data = AoiFixdur)

a <- AoiFixdur
#a <- a[(a$leak == 0) == F,]
a$leak <- log(a$leak +1)
a$leakNodes <- log(a$leakNodes)
a$otherNodes <- (a$otherNodes)^(0.5)
Diffm <- c("None", "S1", "S2", "S3", "S4")

leakm <- aggregate(a$leak, by=list(a$Difficulty), mean)
leaknm <- aggregate(a$leakNodes, by=list(a$Difficulty), mean)
onm <- aggregate(a$otherNodes, by=list(a$Difficulty), mean)

plot(leakm$x, pch = "-", cex = 3, col = "red")

g <- lm(leak~Difficulty, data = a)

plot(g, which = 1)
plot(g, which = 2)

library(MASS)
boxcox(otherNodes ~ ., data = AoiFixdur)
par(mfrow = c(3,2))
boxplot(leak~Difficulty, data = a, names = c("None", "S1", "S2", "S3", "S4"), ylab = "Fixation Duration on Privacy Leaking Nodes", xlab = "Difficulty Levels")
points(leakm$x, pch = "-", cex = 3, col = "red")

boxplot(leak~Difficulty, data = AoiFixdur, ylab = "Fixation Duration for Leaking Nodes", xlab = "Difficulty Levels")

boxplot(leakNodes~Difficulty, data = a, names = Diffm, ylab = "Fixation Duration on Privacy Leaking Ontology Nodes", xlab = "Difficulty Levels")
points(leaknm$x, pch = "-", cex = 3, col = "red")
boxplot(leakNodes~Difficulty, data = AoiFixdur)
boxplot(otherNodes~Difficulty, data = a, names = Diffm, ylab = "Fixation Duration on Other Ontology Nodes", xlab = "Difficulty Levels")
points(onm$x, pch = "-", cex = 3, col = "red")

boxplot(otherNodes~Difficulty, data = AoiFixdur)
par(mfrow = c(1,1))


## Jia Kai data
b <- df
b$Time.in.page <- (b$Time.in.page)/1000
b$Last.click.time <- (b$Last.click.time)/1000

bb <- aggregate(b$Last.click.time, by=list(b$Difficulty), mean)

gl <- lm(Time.in.page ~ Difficulty, data = b)

plot(gl, which = 1)
plot(gl, which = 2)

par(mfrow = c(3,2))
boxplot(Accuracy~Difficulty, data = df, names = Diffm, ylab = "Correctness", xlab = "Difficulty Levels")
points(bb$x, pch = "-", cex = 3, col = "red")

boxplot(Accuracy~Difficulty, data = b)
boxplot(Time.in.page~Difficulty, data = df)
boxplot(Time.in.page~Difficulty, data = b, names = Diffm, ylab = "Total Time Spent (s)", xlab = "Difficulty Levels")
points(bb$x, pch = "-", cex = 3, col = "red")

boxplot(Last.click.time~Difficulty, data = df)
boxplot(Last.click.time~Difficulty, data = b, names = Diffm, ylab = "Last Selection Time (s)", xlab = "Difficulty Levels")
points(bb$x, pch = "-", cex = 3, col = "red")

par(mfrow = c(1,1))

boxcox(Last.click.time ~ Difficulty, data = df)
boxcox(Time.in.page ~ Difficulty, data = df)

plot(df$Name[which(df$Difficulty == 0)], df$Time.in.page[which(df$Difficulty == 0)])
