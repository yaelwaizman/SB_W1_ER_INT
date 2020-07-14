########################################################################################## 
# Emotion Regulation Questionnaire (ERQ) scoring
# Script written by Yael H. Waizman
########################################################################################## 

ERQ <- function(dataframe){
  # calculate Cognitive Reappraisal usage total score
  dataframe$ERQ_Reapp_TOTAL <- rowSums(dataframe[,paste("ERQ",c(1,3,5,7,8,10),sep="")], na.rm = FALSE)
  
  # calculate Expressive Suppression usage total score
  dataframe$ERQ_Supp_TOTAL <- rowSums(dataframe[,paste("ERQ",c(2,4,6,9),sep="")], na.rm = FALSE) 
  
  return(dataframe)
}