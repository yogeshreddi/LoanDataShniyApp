# title: "Capitalone HMDA Data Challenge"
# author: "Yogesh"
# date: "December 09, 2016"
# output: html_document


requiredPackages <- c('data.table','dplyr','rjson','ggplot2','shinydashboard','shiny')

for (i in requiredPackages){
  ifelse(i %in% rownames(installed.packages()),NA,install.packages(i) )
}

# to supress warnings globally
options(warn=-1)

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))


library(data.table)
library(dplyr)
library(rjson)
library(ggplot2)
library(shiny)
library(shinydashboard)




rm(i,requiredPackages)

projectDir<-'C:/Users/gvykr/Desktop/interviews/CapitalOneDataChallenge/Data'
if(getwd()!=projectDir){
  setwd(projectDir)  
}

rm(projectDir)

loanData<-suppressWarnings(data.frame(fread('./2012_to_2014_loans_data.csv',na.strings=c(NA,""," "))))
instData<-suppressWarnings(data.frame(fread('./2012_to_2014_institutions_data.csv',na.strings=c(NA,""," "))))
# hardly takes any time to load the data compared to read.table and read.csv 


hmdaData <-inner_join(loanData,instData,c("Respondent_ID","Agency_Code","As_of_Year"))

hmdaData <- hmdaData%>%
  filter(Respondent_Name_TS != 'MEGACHANGE MORTGAGE')

# removing the loanData and instution data once we have the join data
rm(loanData,instData)

# Looking at the structure of the data
# str(hmdaData)

# Converting the continous varibale to its class type
hmdaData$Loan_Amount_000<-as.numeric(hmdaData$Loan_Amount_000)
hmdaData$Applicant_Income_000<-as.numeric(hmdaData$Applicant_Income_000)
hmdaData$FFIEC_Median_Family_Income<-as.numeric(hmdaData$FFIEC_Median_Family_Income)
hmdaData$Number_of_Owner_Occupied_Units <- as.integer(hmdaData$Number_of_Owner_Occupied_Units)

# Converting the coded variables as factor varibales
hmdaData$Agency_Code <- as.factor(hmdaData$Agency_Code)
hmdaData$As_of_Year <- as.factor(hmdaData$As_of_Year)
hmdaData$County_Code <- as.factor(hmdaData$County_Code)
hmdaData$State_Code <- as.factor(hmdaData$State_Code)


# global function to filter the data based on the selected geographic county and competitor

filtered_data <- function(
  county = c(unique(hmdaData$County_Name)),
  Respondent = c(unique(hmdaData$Respondent_Name_TS))
){
return(hmdaData%>%
         filter(County_Name == county & 
                  Respondent_Name_TS == Respondent))
}

############### Function to look at the missing value and unique values distribution in the dataset ############

descriptive <- function(dataset){
  
  # Using sapply to get the number of missing value count
  result<-stack(sapply(colnames(dataset), function(x) sum(is.na(dataset[,x]))))
  
  # Renaming column names
  colnames(result)<-c('MissingCount','Column_Name')
  
  # reordering the column names
  result<-result[,c('Column_Name','MissingCount')]
  
  #type of class
  result[,'Column_Type']<-stack(sapply(hmdaData, function(x) class(x)))[,'values']
  
  # calculating for missing values percentages in the dataset for each variable
  result[,'PercentageMissing'] = round(result[,'MissingCount']/nrow(dataset)*100,2)
  
  # calculating unique values 
  result[,'UniqueCOunt'] = stack(sapply(colnames(dataset), function(x) length(unique(dataset[,x]))))['values']
  
  # calculating for unique values percentages in the dataset for each variable
  result[,'PercentageUniqe'] = round(result[,'UniqueCOunt']/nrow(dataset)*100,2)
  
  result <- result[,c('Column_Name','Column_Type','MissingCount','PercentageMissing','UniqueCOunt','PercentageUniqe')]

  
  # returning the resultant dataframe
  return(result)
}


descData <- descriptive(hmdaData)

##################-----------Derciptive Statistics for continous columns---------#########################

descForCont <- function(dataset){
  
  result <- stack(sapply(colnames(dataset), function(x) round(mean(dataset[,x],na.rm =T),2)))
  
  # Renaming column names
  colnames(result)<-c('Mean','Column_Name')
  
  # reordering the column names
  result<-result[,c('Column_Name','Mean')]
  
  # evaluating standard deviation
  result[,'StdDeviation'] <- stack(sapply(colnames(dataset), function(x) round(sd(dataset[,x],na.rm =T),2)))['values']
  
  #evaluating median
  result[,'Median'] <- stack(sapply(colnames(dataset), function(x) round(median(dataset[,x],na.rm =T),2)))['values']
  
  #evaluating max
  result[,'Maximum'] <- stack(sapply(colnames(dataset), function(x) round(max(dataset[,x],na.rm =T),2)))['values']
  
  #evaluating max
  result[,'Minimum'] <- stack(sapply(colnames(dataset), function(x) round(min(dataset[,x],na.rm =T),2)))['values']
  
  #evaluating range
  result[,'Range'] <- result$Maximum - result$Minimum
  
  return(result)
}


