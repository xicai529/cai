#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### DQ Testing Module - HeartPath Version - Aug2017 ###
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

### 0- Environment Setup
setwd("C:/Users/xicai/Desktop")
#
#setwd("~/Desktop/DQ Module Dev 15Aug15/") #Mac Version

#Future version - library(RODBC)
#library(toxboot)
#library(lubridate)
library(chron)
library(ggplot2)

# dbscan: added by Cai
library(mice)
library(cluster)
library(dbscan)
library(mclust)
library(readr)

source('DQModuleFunctions.R') # Imported DQ query generation Functions as a library (below)

#0 - Script parameters
###Future Version : Import Config file with Target table and DQ req issue
# Config parameters: 
# TdataFile <- 'HPDataset-1Aug17.csv'
# reqsFile <- 'filename.csv'
# preprocFile <- 'filename.csv'

### Table reading from DB - To be used later
# HeartpathConn <- odbcConnect("HeartPathSS", believeNRows=FALSE)
# TableExtract<-sqlFetch(HeartpathConn, "dbo.WF_HeartPathWay_Index_Data2") 
# nullToNA(TableExtract) # find conversion 



### 1- Import data and Requirements
## Import Data
ImportData <- function(TdataFile){
  #TdataFile <- 'HPDataset-1Aug17.csv'
  Tdata <- read.csv(file=(TdataFile), header=TRUE, na.strings = c("NA", 'NULL'), stringsAsFactors=FALSE)
  is.na(Tdata) <- Tdata == "NULL" # Update NULL to NA
  ## Variable names handling 
  names(Tdata) <- toupper(names(Tdata)) #Make all names upper case - More robust in R
  return(Tdata)
}

##Import DQ Requirements File
ImportReqs <- function(DQReqsFile){
  reqs <- read.csv(file=(DQReqsFile), header=TRUE, stringsAsFactors=FALSE)
  # Check for empty lines  in reqs at this point
  reqs$Variable <- toupper(reqs$Variable) #Make all Variable names upper case - More robust in R
  return(reqs)
}


## Custom Data Pre-processing from external file
#fileName <- 'preproc.r' # Find usual types of data manipulations to add structure in this file
PreProc <- function(preprocFile){
  preProc <- readLines(preprocFile)
  eval(parse (text = preProc ))
  return(preProc)
  #return(eval(parse (text = preProc )))
}


#!!!!!!! Add variable name validation code here -- Use 2 variable name validation checked
# Done manually for this round
### 2- Generate Delinquent lines table
# Could append queries to reqs data frame for next version...
GenerateDelinquents <- function(reqs, Tdata){
  source('DQModuleFunctions.R') # Imported DQ query generation Functions as a library (below)
  #View(reqs)
  reqs <- GenerateQueriesAtReqs(reqs)
  cat("\n Please review Queries for newly-added requirements!!!!! \n\n")
  Sys.sleep(1)
  
  #Running Queries and Recording Results and Details
  i<-1
  j<-1
  ResultsDetail<- data.frame("ReqID"=numeric(0), "Variable"=character(0), "ReqType"=character(0), "Parameter"=character(0),"PAT_MRN_ID"=numeric(0), "PAT_ENC_CSN_ID"=numeric(0) , stringsAsFactors=FALSE)
  ResultOverview <- data.frame("ReqID"=numeric(0), "Variable"=character(0), "ReqType"=character(0), "Parameter"=character(0),"FlawsFound"=logical(0), "FlawsCnt"=numeric(0), stringsAsFactors=FALSE)
  for (i in 1:nrow(reqs)){
    ReqLine <- reqs[i,] #Get Corresponding row @ reqs
    cat ('\n Working on Req: ', ReqLine$ReqID)
    
    
    if(!is.null(reqs$Query)){
      DlqRows <- tryCatch(
        { # Try to...
        eval(parse(text=reqs$Query[i])) # ...Pull delinquent rows
      }, 
      warning = function(w) {
        cat('Warning Returned: \n ')
        cat(as.character(w))# Warnings are not critical at this point
        cat('\n ')
      }, 
      error = function(e) {
        
        # Print Error
        cat('Error Returned: \n ')
        cat(as.character(e))# Warnings are not critical at this point
        cat('\n ')
        
        #Print Requirement line and query
        cat('Requirement to be fixed: \n ')
        reqs[i,]
        
        #Ask for a replacement requirement (parametr)
        cat('Please attempt to fix the requirement: \n ')
        #reqs <- edit(reqs)
        i <- i-1
        
      }, 
      finally = {
        #Stuff to run not matter what
        cat('\n ')
      }
      )
    } else {
      DlqRows <- cat('\n ')
    }
    
    cat ('  ReqEvaluated! \n')
    
    if (is.null(DlqRows)!=T){
      cat ('DQ Errors found: ', nrow(DlqRows),'\n')
      if(is.logical(DlqRows)==TRUE) { #if (population-level query) only add 1 line to overview w/ result
        ResultOverview <- rbind(ResultOverview, c(ReqLine, "FlawsFound"=DlqRows, "FlawsCnt"=NA), stringsAsFactors=FALSE) # Population-level Reqs, which return TRUE or FALSE
      } else { # else requirement is not population-level and will return dataset delinquent lines
        if(nrow(DlqRows)<=0){ #don't add anything if no bad rows 
          ResultOverview <- rbind(ResultOverview, c(ReqLine, "FlawsFound"=FALSE, "FlawsCnt"=0), stringsAsFactors=FALSE)
        } else {# Add 1 line to overview
          ResultOverview <- rbind(ResultOverview, c(ReqLine, "FlawsFound"=TRUE, "FlawsCnt"=nrow(DlqRows)), stringsAsFactors=FALSE)
          for (j in 1:nrow(DlqRows)){ # Add n lines to detailed results 
            ResultsDetail <- rbind(ResultsDetail, c(ReqLine, "PAT_MRN_ID"=DlqRows$PAT_MRN_ID[j],  "PAT_ENC_CSN_ID"=DlqRows$PAT_ENC_CSN_ID[j]), stringsAsFactors=FALSE) # Attach test data primary keys to Req definition line
            cat('AddedLine')
          }
        }
      }
    }
  }
  ### Break here and enter the Validation loop (Validation code goes HERE/could be also called at this point)
  Results <- list("Overview" = ResultOverview, "Detail" = ResultsDetail)
  #Results <- c(ResultOverview, ResultsDetail)
  return(Results)
}


######testing with real reqs and test data########
test_data <- ImportData('DQModuleTestDatasetJan18.csv')
reqs <- ImportReqs('DQReqs.csv')
results <- GenerateDelinquents(reqs,test_data)
Flaws_overview_report <- data.frame(b[1])
Flaws_details <- data.frame(b[2])


