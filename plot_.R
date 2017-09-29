rm(list = ls())
#setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")
setwd("/Users/jingli/Desktop/Eyetracking/R/eyetracking_git")

library(ggplot2)

df <- read.csv("./studyResults_without_Sandra.csv", header=T)

a <- AoiFixdur
#a <- a[(a$leak == 0) == F,]
a$leak <- log(a$leak +1)
a$leakNodes <- log(a$leakNodes)
a$otherNodes <- (a$otherNodes)^(0.5)
Diffm <- c("None", "S1", "S2", "S3", "S4")

leakm <- aggregate(a$leak, by=list(a$Difficulty), mean)
leaknm <- aggregate(a$leakNodes, by=list(a$Difficulty), mean)
onm <- aggregate(a$otherNodes, by=list(a$Difficulty), mean)


g <- lm(leak~Difficulty, data = a)

plot(g, which = 1)
plot(g, which = 2)

library(MASS)
boxcox(otherNodes ~ ., data = AoiFixdur)
par(mfrow = c(3,2))
boxplot(leak~Difficulty, data = a, names = c("None", "S1", "S2", "S3", "S4"), main = "Fixation Duration on PLNs", xlab = "Difficulty Levels", ylab = "Time (log(s))",
               cex.lab = 1.3, cex.axis = 1.3)
points(leakm$x, pch = "-", cex = 3, col = "red")

boxplot(leak~Difficulty, data = AoiFixdur, ylab = "Fixation Duration for Leaking Nodes", xlab = "Difficulty Levels")

boxplot(leakNodes~Difficulty, data = a, names = Diffm, main = "Fixation Duration on PLONs", ylab = "Time (log(s))",
        xlab = "Difficulty Levels", cex.lab = 1.3, cex.axis = 1.3)
points(leaknm$x, pch = "-", cex = 3, col = "red")

boxplot(leakNodes~Difficulty, data = AoiFixdur)

boxplot(otherNodes~Difficulty, data = a, names = Diffm, main = "Fixation Duration on OONs", xlab = "Difficulty Levels", ylab = "Time (sqrt(s))",
        cex.lab = 1.3, cex.axis = 1.3)
points(onm$x, pch = "-", cex = 3, col = "red")

boxplot(otherNodes~Difficulty, data = AoiFixdur)
par(mfrow = c(1,1))


## Jia Kai data
b <- df
b$Time.in.page <- (b$Time.in.page)/1000
b$Last.click.time <- (b$Last.click.time)/1000

bl <- aggregate(b$Last.click.time, by=list(b$Difficulty), mean)
bt <- aggregate(b$Time.in.page, by=list(b$Difficulty), mean)
ba <- aggregate(b$Accuracy, by=list(b$Difficulty), mean)

gl <- lm(Time.in.page ~ Difficulty, data = b)

plot(gl, which = 1)
plot(gl, which = 2)

par(mfrow = c(3,2))
boxplot(Accuracy~Difficulty, data = df, names = Diffm, main = "Correctness", ylab = "Correctness", xlab = "Difficulty Levels",
        cex.lab = 1.3, cex.axis = 1.3)
points(ba$x, pch = "-", cex = 3, col = "red")

boxplot(Accuracy~Difficulty, data = b)
boxplot(Time.in.page~Difficulty, data = df)
boxplot(Time.in.page~Difficulty, data = b, names = Diffm, main = "Total Time Spent", xlab = "Difficulty Levels", ylab = "Time (s)",
        cex.lab = 1.3, cex.axis = 1.3)
points(bt$x, pch = "-", cex = 3, col = "red")

boxplot(Last.click.time~Difficulty, data = df)
boxplot(Last.click.time~Difficulty, data = b, names = Diffm, main = "Last Selection Time", xlab = "Difficulty Levels", ylab = "Time (s)",
        cex.lab = 1.3, cex.axis = 1.3)
points(bl$x, pch = "-", cex = 3, col = "red")

par(mfrow = c(1,1))

boxcox(Last.click.time ~ Difficulty, data = df)
boxcox(Time.in.page ~ Difficulty, data = df)

plot(df$Name[which(df$Difficulty == 0)], df$Time.in.page[which(df$Difficulty == 0)])


## plot AOI sequence
AOI.plot.data <- AoiVisitTime
AOI.plot.data <- 
t <- seq(as.POSIXct("2017-09-01 00:00"), by = 1, length.out = nrow(AOI.plot.data))
AOI.plot.data$Date <- t
write.csv(AOI.plot.data, file = "AoiData_all_date.csv", row.names = FALSE, quote = FALSE)


## plot error bar
library(psych)

b <- rbindlist(trimmed.data.noAoi.list)
fd <- AvgfixDur.lvl[-nrow(AvgfixDur.lvl),]
sc <- AvglenSac.lvl[-nrow(AvglenSac.lvl),]
fd_m <- colMeans(fd)
fd_s <- apply(fd, 1, sd)
sc_s <- apply(sc, 1, sd)
df1 <- data.frame(mean = rowMeans(fd), sd = fd_s)
df2 <- data.frame(mean = rowMeans(sc), sd = sc_s)
error.crosses(df2, df1, sd=TRUE, main = "", xlab = "Average Saccade Length", ylab = "Average Fixation Duration")




