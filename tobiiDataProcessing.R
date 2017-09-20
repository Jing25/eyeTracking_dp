rm(list = ls())
setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")
#setwd("/Users/jingli/Desktop/Eyetracking/R/eyetracking_git")

recordings <- c("sex0", "mit0")
num.aois <- c(14, 16)
numCol <- 9;
xCol <- 7;
yCol <- 8;

source("TobiiTrim.R")

rm(trimmed.data)
trimmed.data.list <- lapply(1:length(recordings), function(x) TobiiTrim(recs = recordings[x], 
                               fileroot = "./Jing_EyetrackingData/Perceptual_Masking_test_test1_Rec ", n.aoi = num.aois[x]))
names(trimmed.data.list) <- recordings

trimmed.data.noAoi <- TobiiTrim.noAOIs(recs = recordings, fileroot = "./Jing_EyetrackingData/Perceptual_Masking_test_test1_Rec ")

## Total fixation duration
Sumfix.all <- SumFixDur(trimmed.data)
Numfix <- NumFixDur(trimmed.data)
Avgfix.all <- Sumfix.all/Numfix

## Total fixation duration per aoi
Sumfix.aoi <- SumFixDurAoi(trimmed.data)

## Avg saccadic amplitude
avgSacAmp.aoi <- AvgSacAmpAOI(trimmed.data)
avgSacAmp.all <- AvgSacAmp(trimmed.data)


## Avg length of saccades
avglenSac.all <- LenSac(trimmed.data)
avglenSac.aoi <- LenSacAOI(trimmed.data)


#AoiData <- trimmed.data[(trimmed.data$aoi.tag == "NA") ==F,]
AoiData <- trimmed.data_sex0[, c("aoi.tag", "GazeEventDuration", "MediaName", "ParticipantName")]
names(AoiData) <- c("AOIName", "FixationDuration", "Stimulus", "Participant")
write.csv(AoiData, file = "AoiData.csv", row.names = FALSE, quote = FALSE)


## Fixation duration per aoi
AoiTotFixDur <- aggregate(AoiData$FixationDuration, by=list(AoiData$AOIName), sum)
names(AoiTotFixDur) <- c("AOIName", "FixationDuration")



AoiVisitTime <- VisFixDurAoi(AoiData)
AoiVisitTime <- AoiVisitTime[(is.na(AoiVisitTime$AOIName)) == F,]
write.csv(AoiVisitTime[,1:4], file = "AoiVisitData.csv", row.names = FALSE, quote = FALSE)

AoiVisitTime.group <- GroupAois(AoiVisitTime)
write.csv(AoiVisitTime.group[,1:4], file = "AoiVisitData_group.csv", row.names = FALSE, quote = FALSE)



