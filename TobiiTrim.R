## This function turns data into a more manageable format
TobiiTrim<-function(recs, fileroot, n.aoi){
  
  # this sets up the first line (which will be deleted) of a matrix to store all the data.  This is done so we can rbind().
  
  first.line<-matrix(nrow=1, ncol=(numCol+(n.aoi)+2+1), NA)
  
  #This part checks that the recodrings that you say are there are actually there taking time now but saving pain later on.  Fix this to just real 1 line.
  
  #for (j in 1:length(recs)){dat<-read.table(paste(fileroot, recs[j] , ".tsv", sep=""),sep="\t", header=TRUE) ; print(paste("Checking file", recs[j]))}
  
  # this is a 'loop' that cycles through each for the recs in turn
  for (i in 1:length(recs)){
    
    # progress bar
    
    print(paste("Analysing Recording", recs[i]))
    
    # this loads the data
    dat<-read.table(paste(fileroot, recs[i] , ".tsv", sep=""),sep="\t", header=TRUE)
    dat=dat[,-length(dat[1,])]
    
    
    # this reduces the size of the data because it is too big and that slows things down later on
    dat <- unique(dat)
    dat<-dat[dat$GazeEventType == "Fixation",]
    #dat<-dat[dat$GazeEventType != "Unclassified",]
    
    # fill the NAs with 0s or the next bit won't work
    for (j in (numCol+1):(numCol+n.aoi)){dat[is.na(dat[,j]),j]<-0}
    
    
    # delete missing data
   # for (j in 1:length(dat[,1])){
    #  if(is.na(dat[j, xCol]) == TRUE)
     # {
     #   dat <- dat[-j,]
    #  }
    #}
    
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
    
    #aoi.tag[is.na(aoi.tag)]<-"NA"
    
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
  #dat<-dat[is.na(dat$tag)==F,]
  
  return(dat)
}