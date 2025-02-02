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
difficulty <- c(0, 4, 5, 6, 7)
names(difficulty) <- c("None","S1","S2","S3","S4")
AOI.plot.data <- AoiVisitTime #AoiVisitTime[nrow(AoiVisitTime):1]#apply(AoiVisitTime,2,rev)#AoiVisitTime[rev(rownames(AoiVisitTime)),]
AOI.plot.data$Date <- rep(as.POSIXct("2017-09-01"), nrow(AOI.plot.data))
AOI.plot.data$EndDate <- rep(as.POSIXct("2017-09-01"), nrow(AOI.plot.data))
AOI.plot.data$Correctness <- "NA"
a <- AOI.plot.data$Correctness
for(i in 2:nrow(AOI.plot.data)) {
  if(AOI.plot.data$Participant[i] != AOI.plot.data$Participant[i-1]) {
    AOI.plot.data[i-1]$EndDate <- AOI.plot.data$Date[i-1] + AOI.plot.data$FixationDuration[i-1] 
  }
  else {
    AOI.plot.data$Date[i] <- AOI.plot.data$Date[i-1] + AOI.plot.data$FixationDuration[i-1]
    AOI.plot.data[i-1]$EndDate <- AOI.plot.data$Date[i-1] + AOI.plot.data$FixationDuration[i-1]
  }
}

for(i in 1:nrow(AOI.plot.data)) {
  #AOI.plot.data[i]$Correctness <- df$Accuracy[((paste(df$Name[i]) == AOI.plot.data$Participant[i]) & (df$Difficulty[i] == difficulty[AOI.plot.data$Difficulty[i]]))]
  #a[i] <- df$Accuracy[(grepl(AOI.plot.data$Participant[i], paste(df$Name), fixed = TRUE) & (df$Difficulty == difficulty[AOI.plot.data$Difficulty[i]]))]
  if(a[i] >= 1) AOI.plot.data$Correctness[i] <- "100%"
  else if (a[i] <= 0) AOI.plot.data$Correctness[i] <- "0%"
  else AOI.plot.data$Correctness[i] <- "1%-99%"
}
AOI.plot.data$Correctness <- a

AOI.plot.data$EndDate[nrow(AOI.plot.data)] <- AOI.plot.data$Date[nrow(AOI.plot.data)]
write.csv(AOI.plot.data, file = "AoiData_all_date_correct.csv", row.names = FALSE, quote = FALSE)


## plot error bar
library(psych)
pi <- read.csv("./userInfo.csv", header=T)

b <- rbindlist(trimmed.data.noAoi.list)
fd <- AvgfixDur.lvl[-nrow(AvgfixDur.lvl),]
sc <- AvglenSac.lvl[-nrow(AvglenSac.lvl),]
fd_s <- apply(fd, 2, sd)
sc_s <- apply(sc, 2, sd)
df1 <- data.frame(mean = colMeans(fd), sd = fd_s)
df2 <- data.frame(mean = colMeans(sc), sd = sc_s)
error.crosses(df2, df1, sd=TRUE, main = "", xlab = "Average Saccade Length", ylab = "Average Fixation Duration", cex.lab = 1.3)


fd$Gender <- "NA"
for(i in 1:nrow(fd)) {
  fd$Gender[i] <- paste(pi[pi$First_Name == rownames(fd)[i],"Gender"])
}

fdd <- fd[1:5]
fd_s <- apply(fdd, 1, sd)
sc_s <- apply(sc, 1, sd)
df11 <- data.frame(mean = rowMeans(fdd), sd = fd_s)
df22 <- data.frame(mean = rowMeans(sc), sd = sc_s)
colors <- c("blue", "darkorange")
names(colors) <- c("M", "F")
error.crosses(df22, df11, sd=TRUE, main = "", xlab = "Average Saccade Length", ylab = "Average Fixation Duration", 
              colors = colors[fd$Gender], labels = "", cex.lab = 1.3)

## plot error bar for dataset
ds <- c("DS5", "DS2", "DS3", "DS1", "DS4")

aa <- cbind(AvglenSacMedia[AvglenSacMedia$MediaName == "dis"]$`length of saccadic run (px)`, 
       AvglenSacMedia[AvglenSacMedia$MediaName == "sex"]$`length of saccadic run (px)`,
       AvglenSacMedia[AvglenSacMedia$MediaName == "mit"]$`length of saccadic run (px)`, 
       AvglenSacMedia[AvglenSacMedia$MediaName == "inc"]$`length of saccadic run (px)`,
       AvglenSacMedia[AvglenSacMedia$MediaName == "job"]$`length of saccadic run (px)`)
aa <- data.frame(aa)
names(aa) <- ds

bb <- cbind(AvgfixDurMedia[AvgfixDurMedia$media == "dis",][1],
            AvgfixDurMedia[AvgfixDurMedia$media == "sex",][1],
            AvgfixDurMedia[AvgfixDurMedia$media == "mit",][1],
            AvgfixDurMedia[AvgfixDurMedia$media == "inc",][1],
            AvgfixDurMedia[AvgfixDurMedia$media == "job",][1])
names(bb) <- ds

df111 <- data.frame(mean = colMeans(aa), sd = apply(aa, 2, sd))
df222 <- data.frame(mean = colMeans(bb), sd = apply(bb, 2, sd))
error.crosses(df111, df222, sd=TRUE, main = "", xlab = "Average Saccade Length", 
              ylab = "Average Fixation Duration", cex.lab = 1.3)




