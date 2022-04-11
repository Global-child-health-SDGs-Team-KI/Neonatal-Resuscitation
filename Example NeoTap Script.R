## Read Me!
# This is an example of R script that allows for custom made collection of data from the CSV files generated from the NeoTap application
# There are multiple ways to import data from CSV files and structure it in a tabular format for analysis, this is an example 

## 1. Ensure that the CSV files of the data collection is in a repository and that the working dirctory is set to the folder where all the data files are

## 2. Packages (if needed)
install.packages("dplyr")
library(dplyr)

temp <- sort(list.files(path = "."))
str.to.sec <- function(string) {
  a <- strsplit(string, ':')
  out <- as.double(a[[1]][1]) * 60 + as.double(a[[1]][2])
  return(out)
}
## 3. Create custom made functions that collect data as appropriate
# Here, we make two functions focusing on ventilation and stimulation
# For each resuscitation two different CSV files were created, one with the pattern .1 and the other with .2. For ease of understanding, we recommend that one create one custom made function for each type of CSV file. 
# The coding is specific to the CSV files created, so when writing the functions one must take the specifics of the CSV files and the outcome variables of interest into account

#First function: Ventilation
ventilation <- function(filename){
  #Make it into a manageble dataframe from all CSV files
  data <- read.table(file = filename, sep = ',', stringsAsFactors = F, header = F)
  ventilation <-  data[data$V2 == 'VENTILATION',-1]
  colnames(ventilation) <- c('V1','duration', 'end')
  
  #Specifics of what we are interested in (this depends on the variables of interest)
  ventilation[,2] <-  unlist(lapply(ventilation[,2], str.to.sec)) # 
  ventilation[,3] <-  unlist(lapply(ventilation[,3], str.to.sec))
  ventilation['start'] <- ventilation[,3] -ventilation[,2]  
  TVS <- ventilation$start[1] # When the ventilation started
  TVT5 <- sum(ventilation$duration) # Here we collected data for the first 5 minutes, hence TVT5 represents the total duration during the first 5 minutes

  #Specifics of ventilation duration and fractions
  a <- rep(0, 700)
  for(i in 1:dim(ventilation)[1]) {
    a[ventilation[i,4]:(ventilation[i,3]-1)] <- 1
  }
  VT1 <- sum(a[1:59]) # Number of seconds of ventilation during the first minute
  VT1fraction <- (VT1/60)
  VT2 <-  sum(a[60:119]) # Number of seconds of ventilation during second minute
  TV2 <- VT1 + VT2 # Total number of seconds of ventilation during first two minutes
  TV2fraction <- TV2/120
  
  #Collect all variables and return them
  return(c('TVT5'=TVT5, 'TVS'=TVS,'VT1'= VT1, "VT1fraction" = VT1fraction, 'VT2'= VT2, 'TV2'=TV2, "TV2fraction"=TV2fraction))
}

stimulation <- function(filename){
  #Make it into a manageble dataframe from all CSV files
  data <- read.table(file = filename, sep = ',', stringsAsFactors = F, header = F)
  stimulation <-  data[data$V2 == 'STIMULATION',-1]
  colnames(stimulation) <- c('V1','duration', 'end')
  
  #Specifics of what we are interested in (this depends on the variables of interest)
  if(dim(stimulation)[1] !=0){
  stimulation[,2] <-  unlist(lapply(stimulation[,2], str.to.sec))
  stimulation[,3] <-  unlist(lapply(stimulation[,3], str.to.sec))
  stimulation['start'] <- stimulation[,3] -stimulation[,2]  
  TStT <- sum(stimulation$duration) # Total duration of stimulation
  
  #Specifics of ventilation duration and fractions
  a <- rep(0, 700)
  for(i in 1:dim(stimulation)[1]) {
    a[stimulation[i,4]:(stimulation[i,3]-1)] <- 1
  }
  StT1 <- sum(a[1:59]) # Number of seconds of stimulation during the first minute
  StT2 <-  sum(a[60:119]) # Number of seconds of stimulation during the second minute
  TSt2 <- StT1 + StT2 # Total number of seconds of ventilation during first two minutes
  }
  #If stimulation is not present then the above should be a zero
  else{
    StT1 <- 0
    StT2 <- 0
    TSt2 <- 0
    TStT <- 0
  }

  # If we have a yes or no option this can be added easily
  # For instance here we press the "Team" button once on the NeoTap application if more than one birth attendant were present for stimulation 
  
  team1 <-  '0'
  if(sum(grepl('TEAM', data[,2])) == 1) team1 <- '1'
  
  #Collect all variables and return them
  return(c("TStT"= TStT, 'StT1'=StT1, 'StT2'= StT2, 'TSt2'=TSt2, 
           'Team1' = team1))
  }

 ## 4. Run the functions on the csv files and collect the data (note that ventilation function is applied to CSV files ending with ".1.csv" etc)

gather.data <- function(){
  temp1 <- sort(list.files(path = ".",pattern = ".1.csv"))
  temp2 <- sort(list.files(path = ".",pattern = ".2.csv"))

  
  df1 <- sapply(temp1, ventilation)
  df2 <- sapply(temp2, stimulation)

  
  total.df <- cbind(data.frame('video'= sub('.1.csv', '', temp1)),t(df1),t(df2))
  
  return(total.df)
  
}

## 5. Create dataset that can be used to analyse in the R environment
data <- gather.data()

## 5.1 Export dataset to excel for analysis or viewing
write.table(gather.data(), file = 'test.csv', sep = ';', row.names = F)


