rm(list = ls())

setwd("/Users/jing/Desktop/Eyetracking/eyetracking_git/eyeTracking_dp")

recordings <- c("sex0")
numCol <- 9;
xCol <- 7;
yCol <- 8;

source("TobiiTrim.R")


output_fixation <- TobiiTrim(recs = recordings, fileroot = "./Jing_EyetrackingData/Perceptual_Masking_test_test1_Rec ", n.aoi = 14)

aoi.names <- c("color key", "L1", "L2", "L3-leak", "L4", "R1-leak", "R2", "R3", "R4", "S1", "S2-sen_leak", "S3", "S4", "leak")
# 
# recs = recordings
# fileroot = "./TobiiData/Jing_test01_test01_Rec "
# dat<-read.table(paste(fileroot, recs[1] , ".tsv", sep=""),sep="\t", header=TRUE)

