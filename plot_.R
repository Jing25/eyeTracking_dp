rm(list = ls())
setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")
#setwd("/Users/jingli/Desktop/Eyetracking/R/eyetracking_git")

df <- read.csv("./studyResults_without_Sandra.csv", header=T)

boxplot(Accuracy~Difficulty, data = df)
boxplot(Time.in.page~Difficulty, data = df)

plot(df$Name[which(df$Difficulty == 0)], df$Time.in.page[which(df$Difficulty == 0)])
