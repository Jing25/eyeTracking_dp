rm(list = ls())
setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")
#setwd("/Users/jingli/Desktop/Eyetracking/R/eyetracking_git")


numCol <- 9;
xCol <- 7;
yCol <- 8;

source("TobiiTrim.R")


## all data aoi numbers and image names
num.aois.0 <- c(14, 16)
names(num.aois.0) <- c("sex0", "mit0")
num.aois.1 <- c(16,14)
names(num.aois.1) <- c("mit0", "sex0")

level.list <- list(num.aois.0, num.aois.1)

names(level.list) <- c("level.0", "level.1")

## list of all data
trimmed.data.list <- lapply(level.list, function(l) {
  lapply(l, function(x) { 
    TobiiTrim(recs = names(l)[which(l == x)], fileroot = "./Jing_EyetrackingData/Perceptual_Masking_test_test1_Rec ", n.aoi = x)
  })
})

## data without aois list
trimmed.data.noAoi.list <- lapply(level.list, function(l) 
  TobiiTrim.noAOIs(recs = names(l), fileroot = "./Jing_EyetrackingData/Perceptual_Masking_test_test1_Rec "))

## get data
data.01 <- trimmed.data.noAoi
data.02 <- trimmed.data.list$level.0$sex0

### whole image
rm(NumFixDur.all)
NumFixDur.list <- lapply(trimmed.data.noAoi.list, function(l) NumFixDur(l))
NumFixDur.lvl <- data.frame(matrix(unlist(NumFixDur.list), ncol = length(NumFixDur.list$level.0), byrow = T), stringsAsFactors = FALSE) 
## Total fixation duration
SumfixDur.all <- SumFixDur(data.01)
AvgfixDur.all <- SumfixDur.all/NumfixDur.all
## Avg saccade info
avgSacAmp.all <- AvgSacAmp(trimmed.data)
avglenSac.all <- LenSac(trimmed.data)


### per aoi
## fixation duration
SumfixDur.aoi <- SumFixDurAoi(data.02)
## Avg saccadic amplitude
avgSacAmp.aoi <- AvgSacAmpAOI(data.02)
## Avg length of saccades
avglenSac.aoi <- LenSacAOI(data.02)


## individual AOI data
AoiData.indiv <- data.02[(data.02$aoi.tag == "NA") ==F,]
AoiData.indiv <- data.02[, c("aoi.tag", "GazeEventDuration", "MediaName", "ParticipantName")]
names(AoiData.indiv) <- c("AOIName", "FixationDuration", "Stimulus", "Participant")
#write.csv(AoiData.indiv, file = "AoiData_indiv.csv", row.names = FALSE, quote = FALSE)

## Fixation duration per aoi
AoiTotFixDur <- aggregate(AoiData$FixationDuration, by=list(AoiData$AOIName), sum)
names(AoiTotFixDur) <- c("AOIName", "FixationDuration")



AoiVisitTime <- VisFixDurAoi(AoiData)
AoiVisitTime <- AoiVisitTime[(is.na(AoiVisitTime$AOIName)) == F,]
write.csv(AoiVisitTime[,1:4], file = "AoiVisitData.csv", row.names = FALSE, quote = FALSE)

AoiVisitTime.group <- GroupAois(AoiVisitTime)
write.csv(AoiVisitTime.group[,1:4], file = "AoiVisitData_group.csv", row.names = FALSE, quote = FALSE)



