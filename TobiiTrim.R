DelUselessAois <- function(recs, fileroot, nc) {
  
  numAoi <- 0
  for(i in 1:length(recs)) {
    print(paste("Analysing Recording", paste(fileroot, recs[i])))
    
    dat <- read.table(paste(fileroot, recs[i] , ".tsv", sep=""),sep="\t", header=TRUE)
    
    dat <- dat[!(dat$ParticipantName == "Jing" | dat$ParticipantName == "Summer" |dat$ParticipantName == "Sandra"),
               colSums(is.na(dat)) < nrow(dat)]
    names(dat) <- sub("(\\.Hit).*", "\\1", names(dat))
    
    if(i == 1) numAoi <- c(recs[i], (ncol(dat) - nc))
    else numAoi <- rbind(numAoi, c(recs[i], (ncol(dat) - nc)))
    
    #write.xlsx(dat, file = paste("./Jing_EyetrackingData/new2/Perceptual_Masking_test_test1_", recs[i],".xlsx"))
    write.table(dat, file = paste("./Jing_EyetrackingData/new2/", recs[i], ".csv", sep = ""), row.names = FALSE, quote = FALSE, sep = "\t")
  }
  
  #write.csv(numAoi, file = "./Jing_EyetrackingData/AoiNums.csv", row.names = FALSE)
}# end of DelUselessAois()

## This function turns data into a more manageable format
TobiiTrim<-function(recs, fileroot, n.aoi){
  
  # this sets up the first line (which will be deleted) of a matrix to store all the data.  This is done so we can rbind().
  
  first.line<-matrix(nrow=1, ncol=(numCol+(n.aoi)+2+1), NA)
  
  #This part checks that the recodrings that you say are there are actually there taking time now but saving pain later on.  Fix this to just real 1 line.
  
  #for (j in 1:length(recs)){dat<-read.table(paste(fileroot, recs[j] , ".tsv", sep=""),sep="\t", header=TRUE) ; print(paste("Checking file", recs[j]))}
  
  # this is a 'loop' that cycles through each for the recs in turn
  for (i in 1:length(recs)){
    
    # progress bar
    
    print(paste("Analysing Recording", fileroot, recs[i], sep = ""))
    
    # this loads the data
    dat<-read.table(paste(fileroot, recs[i], ".csv", sep=""),sep="\t", header=TRUE)
    #dat=dat[,-length(dat[1,])]
    
    
    # this reduces the size of the data because it is too big and that slows things down later on
    dat <- unique(dat)
    dat<-dat[dat$GazeEventType == "Fixation",]
    #dat<-dat[dat$GazeEventType != "Unclassified",]
    
    # fill the NAs with 0s or the next bit won't work
    for (j in (numCol+1):(numCol+n.aoi)){dat[is.na(dat[,j]),j]<-0}
    
    
    ### This is the main bit
    
    # this bit extracts the saccades/regressions
    # here we create the additional variables to hold the saccades/regressions
    
    # This bit creates an AOI tag for later analysis
    
    rm(list=grep(x= ls(), pattern="AOI.", value=TRUE))
    
    for(j in (numCol+1):(numCol+n.aoi)){assign(paste(names(dat)[j]), matrix(ncol=1, nrow=length(dat[,1]), NA))}
    
    aoi.tag <- as.data.frame(matrix(ncol=1, nrow=length(dat[,1])))
    names(aoi.tag)<-"aoi.tag"
    aoi.names <- grep(x = ls(), pattern="AOI.", value=TRUE)
    rm(list=grep(x= ls(), pattern="AOI.", value=TRUE))   
    
  
    
    for (j in 1:length(dat[,1])) {

      if (any(dat[j,(numCol+1):(numCol+n.aoi)] == 1))    
      {
        if (length(which(dat[j,(numCol+1):(numCol+n.aoi)] == 1)) > 1)
          aoi.tag[j,] <- names(dat[(which(dat[j,(numCol+1):(numCol+n.aoi)] == 1)[1])+numCol])
        else
          aoi.tag[j,] <- names(dat[(which(dat[j,(numCol+1):(numCol+n.aoi)] == 1))+numCol])
      }
    }

    
    
    for (j in 1:length(dat[,1])){
      if(is.na(aoi.tag[j,])==FALSE)    {aoi.tag[j,] <- substr(aoi.tag[j,], start = 5, stop = (nchar(aoi.tag[j,])-4))}
    }
    
    aoi.tag[is.na(aoi.tag)]<-"NA"
    
    ## This extracts all x axis movement       
    
    comb.x.movement<-matrix(ncol=1, nrow=length(dat[,1]), NA)
    
    for (k in 2:(length(dat[,1])))    {comb.x.movement[k]<-(dat[k,xCol]-dat[k-1,xCol])}                               
    
    ## This loop extracts the Y-axis movement.  We need this for checking whether a saccade is a reading saccade or not.
    
    comb.y.movement<-matrix(ncol=1, nrow=length(dat[,1]), NA)
    
    for (k in 2:(length(dat[,1])))    {comb.y.movement[k]<-dat[k,yCol]-dat[k-1,yCol]}       
    
    ## pop all the data togheter for later                        }
    
    dat<-cbind(dat, comb.x.movement ,comb.y.movement, aoi.tag)
    
    ## ok we have the measures
    ## add a name tag to the person
    #tag<-matrix(ncol=1, nrow=length(dat[,1]), paste("Rec",recs[i]))
    #dat2<-cbind(tag, dat)
    ## put all the data together in 1 dataframe
    
    first.line<-as.data.frame(first.line)
    names(first.line)<-names(dat)
    if (i==1) {dat3<-rbind(dat, first.line)}
    #if (i>1) {dat3<-rbind(dat3, dat, first.line)}
  }
  
  #########  This is where the main loop ends, below is tidying up
  
  dat<-dat3
  dat<-dat[is.na(dat$ParticipantName)==F,]
  
  return(dat)
}

## This function turns data into a more manageable format
TobiiTrim.noAOIs<-function(recs, fileroot){
  
  # this sets up the first line (which will be deleted) of a matrix to store all the data.  This is done so we can rbind().
  
  first.line<-matrix(nrow=1, ncol=(numCol+2), NA)
  
  #This part checks that the recodrings that you say are there are actually there taking time now but saving pain later on.  Fix this to just real 1 line.
  
  #for (j in 1:length(recs)){dat<-read.table(paste(fileroot, recs[j] , ".tsv", sep=""),sep="\t", header=TRUE) ; print(paste("Checking file", recs[j]))}
  
  # this is a 'loop' that cycles through each for the recs in turn
  for (i in 1:length(recs)){
    
    # progress bar
    
    print(paste("Analysing Recording", recs[i]))
    
    # this loads the data
    dat<-read.table(paste(fileroot, recs[i] , ".csv", sep=""),sep="\t", header=TRUE)
    dat=dat[,1:numCol]
    
    
    # this reduces the size of the data because it is too big and that slows things down later on
    dat <- unique(dat)
    dat<-dat[dat$GazeEventType == "Fixation",]
    #dat<-dat[dat$GazeEventType != "Unclassified",]
    
    
    ### This is the main bit
    
    # this bit extracts the saccades/regressions
    # here we create the additional variables to hold the saccades/regressions
    
    
    ## This extracts all x axis movement       
    
    comb.x.movement<-matrix(ncol=1, nrow=length(dat[,1]), NA)
    
    for (k in 2:(length(dat[,1])))    {comb.x.movement[k]<-(dat[k,xCol]-dat[k-1,xCol])}                               
    
    ## This loop extracts the Y-axis movement.  We need this for checking whether a saccade is a reading saccade or not.
    
    comb.y.movement<-matrix(ncol=1, nrow=length(dat[,1]), NA)
    
    for (k in 2:(length(dat[,1])))    {comb.y.movement[k]<-dat[k,yCol]-dat[k-1,yCol]}       
    
    ## pop all the data togheter for later                        }
    
    dat<-cbind(dat, comb.x.movement ,comb.y.movement)
    
    ## ok we have the measures
    ## add a name tag to the person
    #tag<-matrix(ncol=1, nrow=length(dat[,1]), paste("Rec",recs[i]))
    #dat2<-cbind(tag, dat)
    ## put all the data together in 1 dataframe
    
    first.line<-as.data.frame(first.line)
    names(first.line)<-names(dat)
    if (i==1) {dat3<-rbind(dat, first.line)}
    if (i>1) {dat3<-rbind(dat3, dat, first.line)}
  }
  
  #########  This is where the main loop ends, below is tidying up
  
  dat<-dat3
  dat<-dat[is.na(dat$ParticipantName)==F,]
  
  return(dat)
}


## This function turns data into a more manageable grouped format
GroupAois <- function(trimmed.data) {
  
  dat <- trimmed.data
  dat$AOIName[grep(".leak", dat$AOIName)] <- "leaknodes"
  dat$AOIName[grep(".key", dat$AOIName)] <- "colorkey"
  dat$AOIName[grep("\\. ", dat$AOIName)] <- "othernodes"
  dat$AOIName[grep("[[:upper:]]", dat$AOIName)] <- "othernodes"
  dat$AOIName[grep("NA", dat$AOIName)] <- "others"
  
  return(dat)
}

## This function change AOI names
ChangeAoiNames <- function(trimmed.data) {
  
  dat <- trimmed.data
  dat$AOIName[grep("^leak$", dat$AOIName)] <- "Leaking Nodes"
  dat$AOIName[grep("^leaknodes$", dat$AOIName)] <- "Leaking Ontology"
  dat$AOIName[grep("^othernodes$", dat$AOIName)] <- "Other Ontology"
  
  return(dat)
}


## This function extracts the number of fixations
NumFixDur <- function(trimmed.data , recs = unique(trimmed.data$ParticipantName)){
  
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  
  if(missing(trimmed.data)) {stop("You need to enter a data frame")}
  
  num.fix.dur <- as.data.frame(matrix(nrow=length(recs), ncol=1, NA))
  names(num.fix.dur) <- "Number of fixations"
  #row.names(num.fix.dur) <- (recs)
  
  
  for (i in 1:length(num.fix.dur[,1])){
    num.fix.dur[i,1] <- max(trimmed.data$FixationIndex[
      (trimmed.data$ParticipantName == paste(recs[i]))
      ], na.rm=TRUE)
  }
  
  num.fix.dur <- rbind(num.fix.dur, colSums(num.fix.dur))
  row.names(num.fix.dur) <- c(paste(recs), "SUMMATION")
  
  return(num.fix.dur)
  
} # end NumFixDur()


## This function extracts the sum fixation duration

SumFixDur <- function(trimmed.data , recs = sort(unique(sort(trimmed.data$ParticipantName)))) {
  
  if(missing(trimmed.data)) {stop("You need to enter a data frame")}
  
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  sum.fix.dur.out<-as.data.frame(matrix(nrow=length(recs), ncol=1, NA))
  names(sum.fix.dur.out)<-"Sum of fixations"
  #row.names(sum.fix.dur.out)<-(recs)
  
  for (i in 1:length(sum.fix.dur.out[,1])){
    sum.fix.dur.out[i,1]<-sum(trimmed.data$GazeEventDuration[
      (trimmed.data$ParticipantName == paste(recs[i]))], na.rm=TRUE)
  }
  
  sum.fix.dur.out <- rbind(sum.fix.dur.out, colSums(sum.fix.dur.out))
  row.names(sum.fix.dur.out) <- c(paste(recs), "SUMMATION")
  
  return(sum.fix.dur.out)
  
} # end SumFixDur()



## This function extracts the sum fixation duration per AOI

SumFixDurAoi <- function(trimmed.data , recs = unique(trimmed.data$ParticipantName) , aois = unique(trimmed.data$aoi.tag[trimmed.data$aoi.tag != "NA"])){
  
  if(missing(trimmed.data)) {stop("You need to enter a data frame")}
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  sum.fix.dur.aoi<-as.data.frame(matrix(nrow=length(recs), ncol=length(aois), NA))
  names(sum.fix.dur.aoi)<-(aois)
  row.names(sum.fix.dur.aoi)<-(recs)
  
  
  for (i in 1:length(sum.fix.dur.aoi[,1])) {
    for(j in 1:length(sum.fix.dur.aoi[1,])) {
      sum.fix.dur.aoi[i, j] <- sum(trimmed.data$GazeEventDuration[
        (trimmed.data$ParticipantName == paste(recs[i])) & 
        (trimmed.data$aoi.tag == paste(aois[j]))], na.rm = TRUE)
    }
  }
  return(sum.fix.dur.aoi)
}



## This function extracts the length of saccadic run in pixles

LenSac <- function(trimmed.data , recs = unique(trimmed.data$ParticipantName)) {
  
  sac.run<-matrix(nrow=length(trimmed.data[,1]),ncol=1,NA)
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  for (i in 1:length(trimmed.data[,1])){
    if((is.na(trimmed.data$comb.x.movement[i]) == FALSE)) {
      sac.run[i] <- ((trimmed.data$comb.x.movement[i])^2 + (trimmed.data$comb.y.movement[i])^2)^0.5
    } 
  }
  
  len.sac.run<-as.data.frame(matrix(nrow=length(recs), ncol=1, NA))
  names(len.sac.run)<-"length of saccadic run (px)"
  #row.names(len.sac.run)<-(recs)
  
  for (i in 1:length(recs)){
    len.sac.run[i,]<-mean(sac.run[(trimmed.data$ParticipantName == paste(recs[i]))], na.rm=T)
  }
  
  len.sac.run <- rbind(len.sac.run, colSums(len.sac.run))
  row.names(len.sac.run) <- c(paste(recs), "SUMMATION")
  
  return(len.sac.run)
  
}#end LenSac()


## This function extracts the length of saccadic run in pixles per AOI

LenSacAOI <- function(trimmed.data , recs = unique(trimmed.data$ParticipantName) , aois = unique(trimmed.data$aoi.tag[trimmed.data$aoi.tag != "NA"])) {
  
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  sac.run<-matrix(nrow=length(trimmed.data[,1]),ncol=1,NA)
  
  for (i in 1:length(trimmed.data[,1])){
    if((is.na(trimmed.data$comb.x.movement[i]) == FALSE)) {
      sac.run[i] <- ((trimmed.data$comb.x.movement[i])^2 + (trimmed.data$comb.y.movement[i])^2)^0.5
    } 
  }
  
  len.sac.run<-as.data.frame(matrix(nrow=length(recs), ncol=length(aois), NA))
  names(len.sac.run)<-(aois)
  row.names(len.sac.run)<-(recs)
  
  for (i in 1:length(recs)){
    for (j in 1:length(aois)) {
      
      len.sac.run[i, j]<-mean(sac.run[(trimmed.data$ParticipantName == paste(recs[i])) &
                                      (trimmed.data$aoi.tag == paste(aois[j]))], na.rm=T)
      
    }
    
  }
  
  return(len.sac.run)
  
} # end LenSacAOI()



## This function extracts the average saccadic amplitude per AOI
AvgSacAmpAOI <- function(trimmed.data , recs = unique(trimmed.data$ParticipantName) , aois = unique(trimmed.data$aoi.tag[trimmed.data$aoi.tag != "NA"])){
  
  if(missing(trimmed.data)) {stop("You need to enter a data frame")}
  
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  avg.sac.amp <- as.data.frame(matrix(nrow=length(recs), ncol=length(aois), NA))
  names(avg.sac.amp)<-(aois)
  row.names(avg.sac.amp)<-(recs)
  
  
  for (i in 1:length(avg.sac.amp[,1])) {
    for(j in 1:length(avg.sac.amp[1,])) {
      avg.sac.amp[i, j] <- mean(trimmed.data$SaccadicAmplitude[
        (trimmed.data$ParticipantName == paste(recs[i])) & 
          (trimmed.data$aoi.tag == paste(aois[j]))], na.rm = TRUE)
    }
  }
  
  return(avg.sac.amp)
  
} # end AvgSacAmpAOI()

## This function extracts the average saccadic amplitude
AvgSacAmp <- function(trimmed.data , recs = unique(trimmed.data$ParticipantName)){
  
  if(missing(trimmed.data)) {stop("You need to enter a data frame")}
  
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  avg.sac.amp <- as.data.frame(matrix(nrow=length(recs), ncol=1, NA))
  names(avg.sac.amp)<-"Average Saccadic Amplitude"
  #row.names(avg.sac.amp)<-(recs)
  
  
  for (i in 1:length(avg.sac.amp[,1])) {

    avg.sac.amp[i,] <- mean(trimmed.data$SaccadicAmplitude[
      (trimmed.data$ParticipantName == paste(recs[i]))], na.rm = TRUE)
    
  }
  
  avg.sac.amp <- rbind(avg.sac.amp, colSums(avg.sac.amp))
  row.names(avg.sac.amp) <- c(paste(recs), "SUMMATION")
  
  return(avg.sac.amp)
  
} # end AvgSacAmp()


## This function extracts visit time for AOIs

VisFixDurAoi <- function(AoiData){
  
  sum.fix.dur.aoi <- as.data.frame(matrix(nrow=length(AoiData[,1]), ncol=length(AoiData[1,])+1, NA))
  names(sum.fix.dur.aoi)<-c(names(AoiData), "Times")
  sum.fix.dur.aoi[1,1:2] <- AoiData[1,1:2]
  sum.fix.dur.aoi[1,"Stimulus"] <- paste(AoiData[1, "Stimulus"])
  sum.fix.dur.aoi[1,"Participant"] <- paste(AoiData[1, "Participant"])
  sum.fix.dur.aoi[1, 5] <- 1
  k <- 0
  
  for (i in 2:length(sum.fix.dur.aoi[,1])) {
    
    if(AoiData$AOIName[i] == AoiData$AOIName[i-1]) {
      k <- k + 1;
      sum.fix.dur.aoi[i-k,"FixationDuration"] = sum.fix.dur.aoi[i-k,"FixationDuration"] + AoiData$FixationDuration[i]
      sum.fix.dur.aoi[i-k, 5] = sum.fix.dur.aoi[i-k, 5] + 1
    }
    else {
      k <- 0
      sum.fix.dur.aoi[i,1:2] <- AoiData[i,1:2]
      sum.fix.dur.aoi[i,"Stimulus"] <- paste(AoiData[i, "Stimulus"])
      sum.fix.dur.aoi[i,"Participant"] <- paste(AoiData[i, "Participant"])
      sum.fix.dur.aoi[i, 5] <- 1
    }
    
  }
  
  return(sum.fix.dur.aoi)
}

## This function calculate the precentage of leak fixation duration per person

AoiPercnt <- function(Aoidata, recs = unique(Aoidata$Participant)) {
  
  recs <- sort(recs)
  
  percnt <- as.data.frame(matrix(nrow=length(recs), ncol=5, NA))
  names(percnt) <- c("Participant","MediaName","leak.%", "leakNodes.%", "otherNodes.%")
  percnt[,"MediaName"] <- rep(Aoidata$Stimulus[1], length(recs))
  percnt[,"Participant"] <- (recs)
  
  for (i in 1:length(percnt[,1])) {
    totfixdur <- aggregate(Aoidata$FixationDuration[Aoidata$Participant == percnt[i,"Participant"]], 
                           by=list(Aoidata$AOIName[Aoidata$Participant == percnt[i,"Participant"]]), sum)
    names(totfixdur) <- c("AOI", "fixD")
    
    if(any(totfixdur$AOI == "leak")) 
      percnt[i, "leak.%"] <- totfixdur[totfixdur$AOI == "leak", "fixD"] / sum(totfixdur[, "fixD"]) * 100
    else
      percnt[i, "leak.%"] <- 0
    
    if(any(totfixdur$AOI == "leaknodes")) 
      percnt[i, "leakNodes.%"] <- totfixdur[totfixdur$AOI == "leaknodes", "fixD"] / sum(totfixdur[, "fixD"]) * 100
    else
      percnt[i, "leakNodes.%"] <- 0
    
    if(any(totfixdur$AOI == "othernodes")) 
      percnt[i, "otherNodes.%"] <- totfixdur[totfixdur$AOI == "othernodes", "fixD"] / sum(totfixdur[, "fixD"]) * 100
    else
      percnt[i, "otherNodes.%"] <- 0
    
  }
  
  return(percnt)
}

AoiFDur <- function(Aoidata, recs = unique(Aoidata$Participant)) {
  
  recs <- sort(recs)
  
  fixdur <- as.data.frame(matrix(nrow=length(recs), ncol=5, NA))
  names(fixdur) <- c("Participant","MediaName","leak", "leakNodes", "otherNodes")
  fixdur[,"MediaName"] <- rep(Aoidata$Stimulus[1], length(recs))
  fixdur[,"Participant"] <- (recs)
  
  for (i in 1:length(fixdur[,1])) {
    totfixdur <- aggregate(Aoidata$FixationDuration[Aoidata$Participant == fixdur[i,"Participant"]], 
                           by=list(Aoidata$AOIName[Aoidata$Participant == fixdur[i,"Participant"]]), sum)
    names(totfixdur) <- c("AOI", "fixD")
    
    if(any(totfixdur$AOI == "leak")) 
      fixdur[i, "leak"] <- totfixdur[totfixdur$AOI == "leak", "fixD"]
    else
      fixdur[i, "leak"] <- 0
    
    if(any(totfixdur$AOI == "leaknodes")) 
      fixdur[i, "leakNodes"] <- totfixdur[totfixdur$AOI == "leaknodes", "fixD"]
    else
      fixdur[i, "leakNodes"] <- 0
    
    if(any(totfixdur$AOI == "othernodes")) 
      fixdur[i, "otherNodes"] <- totfixdur[totfixdur$AOI == "othernodes", "fixD"]
    else
      fixdur[i, "otherNodes"] <- 0
    
  }
  
  return(fixdur)
}

## This function extracts the length of saccadic run in pixles

LenSacMedia <- function(trimmed.data , recs = unique(trimmed.data$ParticipantName)) {
  
  sac.run<-matrix(nrow=length(trimmed.data[,1]),ncol=2,NA)
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  for (i in 1:length(trimmed.data[,1])){
    if((is.na(trimmed.data$comb.x.movement[i]) == FALSE)) {
      sac.run[i,1] <- ((trimmed.data$comb.x.movement[i])^2 + (trimmed.data$comb.y.movement[i])^2)^0.5
      sac.run[i,2] <- paste(trimmed.data$MediaName[i]) 
    } 
  }
  
  len.sac.run<-as.data.frame(matrix(nrow=length(recs), ncol=2, NA))
  names(len.sac.run)<-c("length of saccadic run (px)", "MediaName")
  row.names(len.sac.run)<-(recs)
  
  for (i in 1:length(recs)){
    len.sac.run[i,1] <- mean(as.numeric(sac.run[(trimmed.data$ParticipantName == paste(recs[i])), 1]), na.rm=T)
    a <- sac.run[(trimmed.data$ParticipantName == paste(recs[i])),2]
    len.sac.run[i,2] <- paste(unique(a[!(is.na(a))]))
  }
  
  #len.sac.run <- rbind(len.sac.run, colSums(len.sac.run))
  #row.names(len.sac.run) <- c(paste(recs), "SUMMATION")
  
  return(len.sac.run)
  
}#end LenSacMedia()

SumFixDurMedia <- function(trimmed.data , recs = sort(unique(sort(trimmed.data$ParticipantName)))) {
  
  if(missing(trimmed.data)) {stop("You need to enter a data frame")}
  
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  sum.fix.dur.out<-as.data.frame(matrix(nrow=length(recs), ncol=2, NA))
  names(sum.fix.dur.out)<- c("Sum of fixations", "MediaName")
  row.names(sum.fix.dur.out)<-(recs)
  
  for (i in 1:length(sum.fix.dur.out[,1])){
    sum.fix.dur.out[i,1]<-sum(trimmed.data$GazeEventDuration[
      (trimmed.data$ParticipantName == paste(recs[i]))], na.rm=TRUE)
    a <- trimmed.data$MediaName[(trimmed.data$ParticipantName == paste(recs[i]))]
    sum.fix.dur.out[i,2] <- paste(unique(a[!(is.na(a))]))
  }
  
  #sum.fix.dur.out <- rbind(sum.fix.dur.out, colSums(sum.fix.dur.out))
  #row.names(sum.fix.dur.out) <- c(paste(recs), "SUMMATION")
  
  return(sum.fix.dur.out)
  
} # end SumFixDurMedia()

## This function extracts the number of fixations
NumFixDurMedia <- function(trimmed.data , recs = unique(trimmed.data$ParticipantName)){
  
  ## sort factor level
  old.lvl <- levels(recs)
  recs <- sort(factor(recs, levels=c(sort(old.lvl))))
  
  
  if(missing(trimmed.data)) {stop("You need to enter a data frame")}
  
  num.fix.dur <- as.data.frame(matrix(nrow=length(recs), ncol=2, NA))
  names(num.fix.dur) <- c("Number of fixations", "MediaName")
  row.names(num.fix.dur) <- (recs)
  
  
  for (i in 1:length(num.fix.dur[,1])){
    num.fix.dur[i,1] <- max(trimmed.data$FixationIndex[
      (trimmed.data$ParticipantName == paste(recs[i]))
      ], na.rm=TRUE)
    a <- trimmed.data$MediaName[(trimmed.data$ParticipantName == paste(recs[i]))]
    num.fix.dur[i,2] <- paste(unique(a[!(is.na(a))]))
  }
  
  #num.fix.dur <- rbind(num.fix.dur, colSums(num.fix.dur))
  #row.names(num.fix.dur) <- c(paste(recs), "SUMMATION")
  
  return(num.fix.dur)
  
} # end NumFixDur()

