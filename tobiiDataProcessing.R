rm(list = ls())
#setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")
setwd("/Users/jingli/Desktop/Eyetracking/R/eyetracking_git")


numCol <- 9;
xCol <- 7;
yCol <- 8;

library(data.table)
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


### different levels
## number of fixation 
NumFixDur.list <- lapply(trimmed.data.noAoi.list, function(l) NumFixDur(l))
  #write.table(as.data.frame(NumFixDur.list), file = "mylist.csv", quote = F, sep = ",")
NumFixDur.lvl <- data.frame(matrix(unlist(NumFixDur.list), ncol = length(NumFixDur.list)), stringsAsFactors = FALSE) 
  names(NumFixDur.lvl) <- names(NumFixDur.list)
  row.names(NumFixDur.lvl) <- row.names(NumFixDur.list$level.0)
## fixation duration
SumFixDur.list <- lapply(trimmed.data.noAoi.list, function(l) SumFixDur(l))
SumFixDur.lvl <- data.frame(matrix(unlist(SumFixDur.list), ncol = length(SumFixDur.list)), stringsAsFactors = FALSE) 
  names(SumFixDur.lvl) <- names(SumFixDur.list)
  row.names(SumFixDur.lvl) <- row.names(SumFixDur.list$level.0)
## Avg fixation duration
AvgfixDur.lvl <- SumFixDur.lvl/NumFixDur.lvl
## Avg saccadic amplitude
AvgSacAmp.list <- lapply(trimmed.data.noAoi.list, function(l) AvgSacAmp(l))
AvgSacAmp.lvl <- data.frame(matrix(unlist(AvgSacAmp.list), ncol = length(AvgSacAmp.list)), stringsAsFactors = FALSE) 
  names(AvgSacAmp.lvl) <- names(AvgSacAmp.list)
  row.names(AvgSacAmp.lvl) <- row.names(AvgSacAmp.list$level.0)
## Avg saccadic length
AvglenSac.list <- lapply(trimmed.data.noAoi.list, function(l) LenSac(l))
AvglenSac.lvl <- data.frame(matrix(unlist(AvglenSac.list), ncol = length(AvglenSac.list)), stringsAsFactors = FALSE) 
  names(AvglenSac.lvl) <- names(AvglenSac.list)
  row.names(AvglenSac.lvl) <- row.names(AvglenSac.list$level.0)
  

## different images
## number of fixation 
NumFixDur.img.list <- lapply(trimmed.data.list, function(l) lapply(l, function(x) NumFixDur(x)))
  #write.table(as.data.frame(NumFixDur.img.list), file = "mylist.csv", quote = F, sep = ",")
NumFixDur.img <- data.frame(matrix(unlist(NumFixDur.img.list), nrow = length(NumFixDur.img.list$level.0$sex0[,1])), stringsAsFactors = FALSE) 
  names(NumFixDur.img) <- as.vector(sapply(NumFixDur.img.list, function(l) names(l)))
  row.names(NumFixDur.img) <- c(as.character(1:(length(NumFixDur.img[,1])-1)), "SUMMATION")
## fixation duration
SumFixDur.img.list <- lapply(trimmed.data.list, function(l) lapply(l, function(x) SumFixDur(x)))
SumFixDur.img <- data.frame(matrix(unlist(SumFixDur.img.list), nrow = length(SumFixDur.img.list$level.0$sex0[,1])), stringsAsFactors = FALSE) 
  names(SumFixDur.img) <- as.vector(sapply(SumFixDur.img.list, function(l) names(l)))
  row.names(SumFixDur.img) <- c(as.character(1:(length(SumFixDur.img[,1])-1)), "SUMMATION")
## Avg fixation duration
AvgfixDur.img <- SumFixDur.img/NumFixDur.img
## Avg saccadic amplitude
AvgSacAmp.img.list <- lapply(trimmed.data.list, function(l) lapply(l, function(x) AvgSacAmp(x)))
AvgSacAmp.img <- data.frame(matrix(unlist(AvgSacAmp.img.list), nrow = length(AvgSacAmp.img.list$level.0$sex0[,1])), stringsAsFactors = FALSE) 
  names(AvgSacAmp.img) <- as.vector(sapply(AvgSacAmp.img.list, function(l) names(l)))
  row.names(AvgSacAmp.img) <- c(as.character(1:(length(AvgSacAmp.img[,1])-1)), "SUMMATION")
## Avg saccadic length
AvglenSac.img.list <- lapply(trimmed.data.list, function(l) lapply(l, function(x) LenSac(x)))
AvglenSac.img <- data.frame(matrix(unlist(AvglenSac.img.list), nrow = length(AvglenSac.img.list$level.0$sex0[,1])), stringsAsFactors = FALSE) 
  names(AvglenSac.img) <- as.vector(sapply(AvglenSac.img.list, function(l) names(l)))
  row.names(AvglenSac.img) <- c(as.character(1:(length(AvglenSac.img[,1])-1)), "SUMMATION")


### per aoi
## fixation duration
SumfixDur.aoi.list <- lapply(trimmed.data.list, function(l) lapply(l, function(x) SumFixDurAoi(x)))
## Avg saccadic amplitude
AvgSacAmp.aoi.list <- lapply(trimmed.data.list, function(l) lapply(l, function(x) AvgSacAmpAOI(x)))
## Avg length of saccades
AvglenSac.aoi <- lapply(trimmed.data.list, function(l) lapply(l, function(x) LenSacAOI(x)))


## AOI sequence data
AoiData.list <- lapply(trimmed.data.list, function(l) lapply(l, function(x) {
  dat <- x[(x$aoi.tag == "NA") == F,]
  dat <- dat[, c("aoi.tag", "GazeEventDuration", "MediaName", "ParticipantName")]
  names(dat) <- c("AOIName", "FixationDuration", "Stimulus", "Participant")
  return(dat)
  }))

a <- AoiData.list$level.0$sex0

AoiVisitTime.list <- lapply(AoiData.list, function(l) lapply(l, function(x) {
     dat <- VisFixDurAoi(x)
     dat <- dat[(is.na(dat$AOIName)) == F,]
     return(dat)
  }))
  #write.csv(AoiData.indiv, file = "AoiData_indiv.csv", row.names = FALSE, quote = FALSE)

## group AOIs
AoiVisitTime.list.group <- lapply(AoiVisitTime.list, function(l) lapply(l, function(x) GroupAois(x)))

AoiVisitTime.lvl.list.group <- lapply(seq_along(AoiVisitTime.list.group), function(l, n, i) {
    dat <- rbindlist(l[[i]])
    dat[["Participant"]] <- n[i]
    #write.csv(dat[,1:4], file = paste("AoiData_group",n[i], ".csv"), row.names = FALSE, quote = FALSE)
    return(dat)
  }, l = AoiVisitTime.list.group, n = names(AoiVisitTime.list.group))
  names(AoiVisitTime.lvl.list.group) <- names(AoiVisitTime.list.group)
AoiVisitTime.lvl <- rbindlist(AoiVisitTime.lvl.list.group)
AoiVisitTime.lvl$Stimulus <- "dp"
write.csv(AoiVisitTime.lvl[,1:4], file = "AoiData_levels.csv", row.names = FALSE, quote = FALSE)


  # b <- AoiVisitTime.lvl.list.group$level.0
  # b <- AoiVisitTime.list.group$level.0$sex0
  # 
  # ## Fixation duration per aoi
  # AoiTotFixDur <- aggregate(AoiData$FixationDuration, by=list(AoiData$AOIName), sum)
  # names(AoiTotFixDur) <- c("AOIName", "FixationDuration")
  # 
  # 
  # write.csv(AoiVisitTime[,1:4], file = "AoiVisitData.csv", row.names = FALSE, quote = FALSE)
  # 
  # AoiVisitTime.group <- GroupAois(AoiVisitTime)
  # write.csv(AoiVisitTime.group[,1:4], file = "AoiVisitData_group.csv", row.names = FALSE, quote = FALSE)


## AOIs percentage

AoiPercent.list <- lapply(AoiVisitTime.list.group, function(l) lapply(l, function(x) AoiPercnt(x)))
AoiPercent.lvl.list <- lapply(AoiPercent.list, function(l) {
   dat <- rbindlist(l)
   dat <- dat[order(Participant)]
   #row.names(dat) <- unlist(lapply(l, function(x) row.names(x)))
   return(dat)
  })
  #a <- AoiPercent.lvl.list$level.0
AoiPercent.lvl <- rbindlist(AoiPercent.lvl.list)
AoiPercent.lvl <- AoiPercent.lvl[order(Participant)]





