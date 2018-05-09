
##### Load required libraries
library(tuneR)
library(seewave)
library(MASS)
library(e1071)
library(stringr)
library(ggplot2)
library(viridis)
library(plyr)
##### Download all required files into a single location on your desktop
##### Use find and replace function to change /Users/denasmacbook/Desktop/MFCC-Vocal-Fingerprinting-master 
##### to local file location


##### Part 1 Two methods of MFCC feature extraction: averaging across all time windows
##### and creating a standardized number of time windows for each call


##### Save all .wav files and set working directory to access the files
##### There are nine .wav files, three from three different females (SAFA, SAFBA and VJRN_01)
setwd("F:\\mid eval minor 2018\\testing_wav")

####Set the input directory to loop over the wave files
input.dir <-
  "F:\\mid eval minor 2018\\testing_wav"

####List all .wav files in directory
L = list.files(input.dir, pattern = "*.wav", full.names = FALSE)
L
loc <- NULL
####Extract file names
filehandles <- str_split_fixed(L, pattern = ".wav", n = 2)[, 1]

####MFCC Feature Extraction Method 1: Averaging over time windows ###

####Create empty list to hold MFCC values
mfcc.vector.list = list()


####Loop to calculate MFCC for each .wav file in the directory
for (i in 1:length(filehandles)) {
  filehandle <-  L[i]
  filename <- paste(input.dir, "/", filehandle, sep = "")
  print(paste("processing", filehandle))
  
  # Read in wav file
  w <- readWave(filename)
  if(str_detect( L[i],"-hu.wav")==TRUE)
    
  {
    
    loc <- c(loc,6)
    
  } else  if(str_detect( L[i],"-bu.wav")==TRUE){
    
    loc <- c(loc,2)
    
  } else  if(str_detect( L[i],"-bp.wav")==TRUE) {
    
    loc <- c(loc,1)
  } else  if(str_detect( L[i],"-dc.wav")==TRUE) {
    
    loc <- c(loc,5)
  } else  if(str_detect( L[i],"-ch.wav")==TRUE){
    
    loc <- c(loc,4)
  } else  if(str_detect( L[i],"-lo.wav")==TRUE){
    
    loc <- c(loc,7)
  } else  if(str_detect( L[i],"-sc.wav")==TRUE) {
    
    loc <- c(loc,8)
  } else  if(str_detect( L[i],"-dk.wav")==TRUE) {
    
    loc <- c(loc,"cant_say")
  } else  if(str_detect( L[i],"-ti.wav")==TRUE) {
    
    loc <- c(loc,9)
  }
  # Calculate 12 MFCCs for each 0.25 ms window
  melfcc.output <-
    melfcc(
      w,
      minfreq = 400,
      maxfreq = 2000,
      wintime = 0.25,
      fbtype = "mel",
      numcep = 12
    )
  dfr <- data.frame(melfcc.output)
  dfr[is.na(dfr)] <- 0
  
  
  
  melfcc.output<- matrix(as.numeric(unlist(dfr)),nrow=nrow(dfr))
  
  #Add MFCC values to list
  mfcc.vector.list[[i]] <- melfcc.output
}


####Check structure of MFCC list; for each call there is matrix with 12 MFCC for each 0.25 s frame index
####Calls of different length will have different number of frames
str(mfcc.vector.list)


mean.sd.list = list()

####Loop to calculate MFCC mean and sd
for (j in 1:length(mfcc.vector.list)) {
  tmp.list <- mfcc.vector.list[[j]]
  list.elements <- lapply(1:ncol(tmp.list), function(x) {
    vec.temp <- tmp.list[, x]
    vec.mean <- mean(vec.temp)
    vec.sd <- sd(vec.temp)
    data.vec <- c(vec.mean, vec.sd)
    data.vec
  })
  mean.sd.list[[j]] <- list.elements
}



####Convert the list to a vector
vec <- unlist(mean.sd.list)

####Convert the vector into a matrix
data.matrix.all <-
  matrix(vec,
         nrow = length(filehandles),
         ncol = 24,
         byrow = T)
data.matrix.all <- as.data.frame(data.matrix.all)



####Create unique column names for the mean and sd of each Mel-frequency bin
colnames <- rep(c("mean", "sd"), 12)
nums <- rep(seq(1, 12), 2)
nums <- sort(nums)
colnames <- paste(colnames, nums)
colnames(data.matrix.all) <- colnames
loc<-data.frame(loc)

mfcc.data.all <-
  cbind.data.frame(data.matrix.all, loc, filehandles)

str(mfcc.data.all)

write.table(mfcc.data.all, file = paste("F:\\mid eval minor 2018\\testing_dataset_svm\\test_mean_and_sd_of_coff.csv"),
            row.names = FALSE,  na="",col.names=TRUE, sep=",")



