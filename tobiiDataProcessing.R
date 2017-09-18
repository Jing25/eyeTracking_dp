rm(list = ls())

#setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")
setwd("/Users/jingli/Desktop/Eyetracking/R/eyetracking_git")

recordings <- c("sex0")
numCol <- 9;
xCol <- 7;
yCol <- 8;
aoi.names <- c("color key", "L1", "L2", "L3-leak", "L4", "R1-leak", "R2", "R3", "R4", "S1", "S2-sen_leak", "S3", "S4", "leak")

source("TobiiTrim.R")


trimmed.data <- TobiiTrim(recs = recordings, fileroot = "./Jing_EyetrackingData/Perceptual_Masking_test_test1_Rec ", n.aoi = 14)

Sumfix <- SumFixDur(trimmed.data)
Numfix <- NumFixDur(trimmed.data)
Avgfix <- Sumfix/Numfix

SumfixAoi <- SumFixDurAoi(trimmed.data)


AoiData <- trimmed.data[(trimmed.data$aoi.tag == "NA") ==F,]
AoiData <- AoiData[, c("aoi.tag", "GazeEventDuration", "MediaName", "ParticipantName")]
names(AoiData) <- c("AOIName", "FixationDuration", "Stimulus", "Participant")
write.csv(AoiData, file = "AoiData.csv", row.names = FALSE, quote = FALSE)

AoiTotFixDur <- aggregate(AoiData$FixationDuration, by=list(AoiData$AOIName), sum)
names(AoiTotFixDur) <- c("AOIName", "FixationDuration")

AoiVisitTime <- VisFixDurAoi(AoiData)
AoiVisitTime <- AoiVisitTime[(is.na(AoiVisitTime$AOIName)) == F,]
write.csv(AoiVisitTime[,1:4], file = "AoiVisitData.csv", row.names = FALSE, quote = FALSE)
sacrun <- SacRun(trimmed.data)


## Avg saccadic amplitude
avgSacAmp.aoi <- AvgSacAmpAOI(trimmed.data)
avgSacAmp.all <- AvgSacAmp(trimmed.data)


## Avg length of saccades
avglenSac.all <- LenSac(trimmed.data)
avglenSac.aoi <- LenSacAOI(trimmed.data)
