library(tuneR)
library(audio)
library(seewave)
#install.packages("wrassp")
library(wrassp)
#install.packages("phonTools")
library(phonTools)
library(stringr)


vec <- NULL

wavPath ="F:/mid eval minor 2018/training_wav"
wavFiles = list.files(wavPath, pattern=glob2rx('*.wav'), full.names=TRUE)



for(i in 1:433)
{
  w<- readWave(wavFiles[i])  
  
  
  meli <- melfcc(w,numcep=12)
  df <- data.frame(meli)
  
  write.table(df, file = paste("F:/mid eval minor 2018/mfcc_train_svm/mfcc_train" , as.numeric(i) , ".csv",sep=''),
              row.names = FALSE,  na="",col.names=TRUE, sep=",")
  
  df= NULL
  if(str_detect( wavFiles[i],"-hu.wav")==TRUE)
    
  {
    
    vec <- c(vec,"hungry")
    
  } else  if(str_detect( wavFiles[i],"-bu.wav")==TRUE){
    
    vec <- c(vec,"burping")
    
  } else  if(str_detect( wavFiles[i],"-bp.wav")==TRUE) {
    
    vec <- c(vec,"belly_pain")
  } else  if(str_detect( wavFiles[i],"-dc.wav")==TRUE) {
    
    vec <- c(vec,"discomfort")
  } else  if(str_detect( wavFiles[i],"-ch.wav")==TRUE){
    
    vec <- c(vec,"cold/hot")
  } else  if(str_detect( wavFiles[i],"-lo.wav")==TRUE){
    
    vec <- c(vec,"lonely")
  } else  if(str_detect( wavFiles[i],"-sc.wav")==TRUE) {
    
    vec <- c(vec,"scared")
  } else  if(str_detect( wavFiles[i],"-dk.wav")==TRUE) {
    
    vec <- c(vec,"cant_say")
  } else  if(str_detect( wavFiles[i],"-ti.wav")==TRUE) {
    
    vec <- c(vec,"tired")
  }
  
  
}

labe <- data.frame(vec)
write.table(labe, file = paste("F:/mid eval minor 2018/training_dataset_svm/training_labels.csv"),
            row.names = FALSE,  na="",col.names=TRUE, sep=",")


