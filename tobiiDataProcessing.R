rm(list = ls())
#setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")
setwd("/Users/jingli/Desktop/Eyetracking/R/eyetracking_git")


numCol <- 9;
xCol <- 7;
yCol <- 8;

library(data.table)
source("TobiiTrim.R")


## all data aoi numbers and image names
num.aois.0 <- c(15,12,15,16,14)
names(num.aois.0) <- c("dis0","inc0","job0","mit0","sex0")
num.aois.4 <- c(15,12,15,16,14)
names(num.aois.4) <- c("dis4","inc4","job4","mit4","sex4")
num.aois.5 <- c(15,12,15,16,14)
names(num.aois.5) <- c("dis5","inc5","job5","mit5","sex5")
num.aois.6 <- c(15,12,15,16,14)
names(num.aois.6) <- c("dis6","inc6","job6","mit6","sex6")
num.aois.7 <- c(15,12,15,16,14)
names(num.aois.7) <- c("dis7","inc7","job7","mit7","sex7")

level.list <- list(num.aois.0, num.aois.4, num.aois.5, num.aois.6, num.aois.7)
names(level.list) <- c("None","S1","S2","S3","S4")

## write to .csv
nameV <- c(names(num.aois.0), names(num.aois.4), names(num.aois.5), names(num.aois.6), names(num.aois.7))
DelUselessAois(recs = nameV, fileroot = "./Jing_EyetrackingData/new/Perceptual_Masking_test_test1_", numCol)

#TobiiTrim(recs = "mit0", fileroot = "./Jing_EyetrackingData/Perceptual_Masking_test_test1_", n.aoi = 16)
## list of all data

trimmed.data.list <- lapply(level.list, function(l) { 
  dat <- lapply(seq_along(l), function(x, n, i) 
  TobiiTrim(recs = n[i], fileroot = "./Jing_EyetrackingData/", n.aoi = x[i]), x = l, n = names(l))
  names(dat) <- names(l)
  return(dat)
})

## data without aois list
trimmed.data.noAoi.list <- lapply(level.list, function(l) 
  TobiiTrim.noAOIs(recs = names(l), fileroot = "./Jing_EyetrackingData/"))


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

a <- AoiData.list$None$dis0

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
    dat[["Stimulus"]] <- n[i]
    #write.csv(dat[,1:4], file = paste("AoiData_group",n[i], ".csv"), row.names = FALSE, quote = FALSE)
    return(dat)
  }, l = AoiVisitTime.list.group, n = names(AoiVisitTime.list.group))

  names(AoiVisitTime.lvl.list.group) <- names(AoiVisitTime.list.group)
  a <- AoiVisitTime.lvl.list.group$level.0

AoiVisitTime.lvl.list <- lapply(seq_along(AoiVisitTime.list.group), function(l, n, i) {
  dat <- rbindlist(l[[i]])
  dat[["Difficulty"]] <- n[i]
  #write.csv(dat[,1:4], file = paste("AoiData_group",n[i], ".csv"), row.names = FALSE, quote = FALSE)
  return(dat)
}, l = AoiVisitTime.list.group, n = names(AoiVisitTime.list.group))
  names(AoiVisitTime.lvl.list) <- names(AoiVisitTime.list.group)
  a <- AoiVisitTime.lvl.list$None

  
AoiVisitTime <- rbindlist(AoiVisitTime.lvl.list)
AoiVisitTime$Stimulus <- AoiVisitTime$Difficulty
AoiVisitTime <- AoiVisitTime[(AoiVisitTime$AOIName == "colorkey") == F,]
write.csv(AoiVisitTime[,1:4], file = "AoiData_all.csv", row.names = FALSE, quote = FALSE)

AoiVisitTime.lvl <- rbindlist(AoiVisitTime.lvl.list.group)
AoiVisitTime.lvl$Participant <- "All participants"
AoiVisitTime.lvl <- AoiVisitTime.lvl[(AoiVisitTime.lvl$AOIName == "colorkey") == F,]
AoiVisitTime.lvl.named <- ChangeAoiNames(AoiVisitTime.lvl)
write.csv(AoiVisitTime.lvl.named[,1:4], file = "AoiData_levels.csv", row.names = FALSE, quote = FALSE)


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

## AOIs fixation duration

AoiFixdur.list <- lapply(AoiVisitTime.list.group, function(l) lapply(l, function(x) AoiFDur(x)))
AoiFixdur.lvl.list <- lapply(AoiFixdur.list, function(l) {
  dat <- rbindlist(l)
  dat <- dat[order(Participant)]
  #row.names(dat) <- unlist(lapply(l, function(x) row.names(x)))
  return(dat)
})

for(i in 1:length(AoiFixdur.lvl.list)) AoiFixdur.lvl.list[[i]]$Difficulty <- 
  rep(names(AoiFixdur.lvl.list)[i], nrow(AoiFixdur.lvl.list[[i]]))

#a <- AoiPercent.lvl.list$level.0
AoiFixdur <- rbindlist(AoiFixdur.lvl.list)
AoiFixdur <- AoiFixdur[order(Participant)]


a <- lapply(AoiData.list, function(l) a <- rbindlist(l))

b <- sapply(a, function(x)  {
  a <- paste(unique(x$Participant)) 
  sort(a) })







