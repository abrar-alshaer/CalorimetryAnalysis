#Abrar Al-Shaer
#Shaikh Lab, Sept 2019
#Calorimetry Data Analysis Program
#Total parameters calculated for each mouse: 18

#set directory to a folder with ONLY your input data files in it
setwd("C:/Users/Abrar/Dropbox/UNC_OneDrive/Shaikh Lab/ALX_FPR2 KO Study/Calorimetry/Calorimetry Preprocessed Files")
#clear working environemnt
rm(list=ls())

#load in all files from your directory (make sure you have a directory with ONLY the data files you wish to analyze)
fileNames <- Sys.glob("*.csv")

#create empty dataframe with column names for all 18 parameters calculated & 3 mouse identifiers (mouse ID, genotype, diet) - 21 total data points for each mouse
#as we iterate through each file this dataframe will get populated with each row corresponding to a mouse
all_params <- setNames(data.frame(matrix(ncol = 21, nrow = 0)), c("Mouse ID", "Genotype", "Diet", "RMR", "Resting Gox", "Resting Lox", "Total VO2", "Total VCO2", "Total EE", "Total Gox", 
                                                                  "Total Lox", "VO2 Night", "VO2 Day", "VCO2 Night", "VCO2 Day", "EE Night", "EE Day",
                                                                  "Gox Night", "Gox Day", "Lox Night", "Lox Day"))

#iterate through each file
i = 1 #initializing counter
for (fileName in fileNames) {
  
  #loading in data
  calData <- na.omit(read.csv(fileName, header = TRUE, fill = TRUE))
  #extracting mouse identifiers from file name (mouse ID, genotype, diet)
  MouseID <- substr(fileName, start = 7, stop = 9) #start & stop correspond to the character locations of the mouse ID, or genotype, or diet etc in the file name
  Genotype <- substr(fileName, start = 23, stop = 24)
  Diet <- substr(fileName, start = 26, stop = 28)
  
  #Extracting all instances where the Xt is 0 (mouse is not moving)
  Xt_zero <- calData[calData$XT == 0,]
  
  #calculating mean resting metabolic rate (average of normalized energy expenditure at all instances Xt = 0)
  RMR <- mean(Xt_zero$EE.cal.min.kg.lbm)
  
  #calculating Gox & Lox at resting state (glucose & lipid metabolism when Xt = 0)
  Gox_rest <- mean(Xt_zero$Gox.normalized)
  Lox_rest <- mean(Xt_zero$Lox.normalized)
  
  #averages for normalized total measurements (not subsetted by day/night cycles)
  total_VO2 <- mean(calData$VO2.norm) #total average for all VO2 measures
  total_VCO2 <- mean(calData$VCO2.norm) #total average for all VCO2 measures
  total_EE <- mean(calData$EE.cal.min.kg.lbm) #total average for all EE measures
  total_Gox <- mean(calData$Gox.normalized) #total average for all Gox measures
  total_Lox <- mean(calData$Lox.normalized) #total average for all Lox measures
  
  #hours 19:06-6:57 is the night cycle, 7:06-18:57 is the day cycle
  #subsetting data based on 2 night cycles and 2 day cycles
  #every 80 data points there is a transition from a night cycle to a day cycle & vice versa
  night <- calData[c(1:80,161:240),] #19:06-6:57 (7:06pm - 6:57am) night 1 & night 2
  day <- calData[c(81:160, 241:320),] #19:06-6:57 (7:06pm - 6:57am) day 1 & day 2
  
  #calculating VO2, VCO2, energy expenditure (EE), & Glucose/lipid metabolism based on day/night cycles
  VO2_night <- mean(night$VO2.norm)
  VCO2_night <- mean(night$VCO2.norm)
  EE_night <- mean(night$EE.cal.min.kg.lbm)
  Gox_night <- mean(night$Gox.normalized)
  Lox_night <- mean(night$Lox.normalized)
  
  VO2_day <- mean(day$VO2.norm)
  VCO2_day <- mean(day$VCO2.norm)
  EE_day <- mean(day$EE.cal.min.kg.lbm)
  Gox_day <- mean(day$Gox.normalized)
  Lox_day <- mean(day$Lox.normalized)
  
  #add all the mouse info & calculated parameters to a dataframe
  all_params[i,] <- c(MouseID, Genotype, Diet, RMR, Gox_rest, Lox_rest, total_VO2, total_VCO2, total_EE, total_Gox, total_Lox, VO2_night, VO2_day, VCO2_night, VCO2_day, EE_night, EE_day, Gox_night, Gox_day, Lox_night, Lox_day)
  i = i+1 #iterate to the next row in the dataframe
}

#write the final dataframe (all_params) to a csv file
write.csv(all_params, "Calorimtery_ALX-FPR2_Calculated_Analysis.csv")

