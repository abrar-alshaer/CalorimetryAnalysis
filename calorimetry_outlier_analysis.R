setwd("C:/Users/Abrar/Dropbox/UNC_OneDrive/Shaikh Lab/ALX_FPR2 KO Study/Calorimetry/Calorimetry Preprocessed Files/")
rm(list=ls())

#read in calorimetry file that was outputted from the Calorimetry Analysis.R program (but add a "Group" column to your file!)
cal <- read.csv("Calorimtery_ALX-FPR2_Calculated_Analysis_ALL_Mice_12Weeks-Old_v2.csv", header = TRUE) #load in OCR data 
file_name <- "Calorimtery_ALX-FPR2_Calculated_Analysis_ALL_Mice_12Weeks-Old_v2"

#####OUTLIER ANALYSIS

groups = levels(cal$Group) #create a variable called groups for all levels (row groups) of the dataframe
columns = colnames(cal)[5:length(colnames(cal))] #store columns in variable

datalist_cal = list()
outliers_data1 = list()
c = 1

#loop through each group (rows) for COMBO data
for(i in 1:length(groups)) {
  print(groups[i]) #print group
  sub_cal <- subset(cal, cal$Group %in% groups[i]) #subset dataframe by the biological group
  #loop through each column within each group
  for(j in 1:length(columns)){
    print(paste("COLUMN:",columns[j]))
    out1 = quantile(sub_cal[,columns[j]], c(1)/4, na.rm = TRUE)[[1]]-1.5*IQR(sub_cal[,columns[j]], na.rm = TRUE)
    out1 = floor(out1*10)/10 #needed due to rounding error (this statement prevents rounding of the outlier cutoff so we don't detect very small deviations from the IQR)
    out2 = quantile(sub_cal[,columns[j]], c(3)/4, na.rm = TRUE)[[1]]+1.5*IQR(sub_cal[,columns[j]], na.rm = TRUE)
    #out2 = floor(out2*10)/10 #not necessary because it reduces the threshold for a high outlier (makes it so that we are too sensitive and detect very small deviations from the IQR)
    n = 1
    outliers_data1 = list()
    for(k in sub_cal[,columns[j]]){
      if(is.nan(k) == FALSE && (k < out1 | k > out2)){
        outliers_data1[[n]] = k
        print(columns[j])
        print(k)
        n = n + 1
      }
    }
    #if there are outliers (list isn't empty)
    if(length(outliers_data1)>0)
    {
      datalist_cal[[c]] <- c(groups[i],columns[j],outliers_data1)
      c = c+1
      #outliers_data1 = list()
    }
  }
}

#big_data_cal = do.call(rbind, datalist_cal) #combines all previous dataframes from for loop
#list_data = data.frame(datalist = unlist(datalist_cal))

#If big_data_data1 from rbind function fails
matrix_data = as.matrix(datalist_cal)
write.csv(matrix_data, paste(file_name,"Outliers_11.26.19.csv"))

#Check to see if the list is empty (no outliers) - if not, add column headers & send dataframe to a file
# if(length(big_data_cal) < 1){
#   print("There are no cal outliers")
# } else {
#   colnames(big_data_cal) <- c("Sample", "Measurement", "Value")
#   write.csv(big_data_cal, "seahorsecal_outliers.csv")
# }

#write.csv(matrix_data, "seahorsecal_outliers_11.10.18.csv")

#######replacing outliers with NaN

#extracting all data1 values from the outliers list and placing them in a new list (removing first 2 elements for group & timepoint)
#for the second list it's appending only element 2 - which is the column the outlier belongs to - useful for the next for loop when replacing outliers with NaNs
datalist_cal_new <- list()
datalist_cal_newNames <- list()
count <- 1
for (i in 1:length(datalist_cal)){
  datalist_cal_new[[count]] <- datalist_cal[[i]][c(3:length(datalist_cal[[i]]))]
  datalist_cal_newNames[[count]] <- datalist_cal[[i]][2] #list of column names that contained outliers
  count = count + 1
}

#converting the list of lists into a single one level list
datalist_cal_new <- unlist(datalist_cal_new)
datalist_cal_newNames <- unlist(datalist_cal_newNames)

#replacing the outliers step
#only loop through the list of outlier columns
for (row in datalist_cal_newNames) { 
  #print(row)
  #print(cal[[row]])
  for (i in 1:length(cal[[row]])){
    #print(cal[[row]][i])
    for (j in 1:length(datalist_cal_new))
      if(is.nan(cal[[row]][i]) == FALSE && (cal[[row]][i] == datalist_cal_new[j])) {
        cal[[row]][i] <- "NaN" }
  }
}

#new dataframe without outliers
write.csv(cal, paste(file_name,"_RMoutliers_11.26.19.csv"))


#calculate outlier values based on the IQR formula MANUALLY
#quantile(sub$X1.413664)[2]-1.5*IQR(sub$X1.413664) Q1-1.5*IQR
#quantile(sub$X1.413664)[4]+1.5*IQR(sub$X1.413664) Q3+1.5*IQR
