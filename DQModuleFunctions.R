### 0- Functions for TEST code generation
## To be containded in file (DQModuleFunctions.R) 
## Imported to main DQ module 
## Initial version 14Jun17

# 0 - Pre-requisite functions
'%!in%'<- function(x,y)!('%in%'(x,y))


# 0 - Variable name validation Function
VarNamesTest <- function ( reqs, Tdata ) { # Returns a vector with variable names in requirements not found in the dataset
  test <- data.frame( 'ReqVariable'=unique (reqs$VARIABLE))
  test$Matches <- ifelse (test$ReqVariable %in% names(Tdata), TRUE, FALSE)
  result <- test [test$Matches==FALSE,]
  if (nrow(result==0)) { return(0) }
  else { return(as.vector(result$ReqVariable)) }
}


# 1- Individual Query Generators
UniqueTestGen <- function(var){ #0- Unique - Pull non-unique variables in var
  var <- toupper(var)
  testCode <- paste ('Tdata [which(Tdata$',var, ' %in% duplicated(Tdata$', var, ')),] ', sep='')
  return(testCode)
}

NoOtherValuesGen <- function(var, parameter){ #2- No other Values - Pull line with categorical values outside GOOD range
  var <- toupper(var)
  testCode <-  paste( 'Tdata[which(Tdata$', var, ' %!in% c(', parameter , ',NA)), ]', sep='')
  return(testCode)
}

NoNULLGen <- function(var){ #3- No Nulls # Pull lines with NULL values within variable var
  var <- toupper(var)
  testCode <-   paste ('Tdata[which(is.na(Tdata$', var, ')),]', sep='')
  return(testCode)
}


ValueRangeGen <- function(var, parameter){ #4- ValueRange # Pull line outside the GOOD Value Range
  var <- toupper(var)
  testCode <-   paste ('Tdata[which(!(Tdata$',var,parameter ,')), ]', sep='')
  return(testCode)
}

FormulaGen <- function(parameter){  #5- Formulas # Pull lines that do not comply with GOOD FORMULA
  testCode <-   paste ('Tdata[which(!(', parameter ,')), ]', sep='')
  return(testCode)
}

ConditionGen <- function(parameter){  #6- Condition -- Pull rows with BAD CONDITION met
  testCode <-   paste ('Tdata[which(', parameter ,'), ]')
  return(testCode)
}

IfConditionGen <- function(parameter){  #7- IfCondition -- Pull rows respond to IF clause, but NOT THEN clause
  
  ifPart <- gsub('\\) *then *(.+)','', trimws(parameter),ignore.case=T, perl=T) # gets 'if ([Condition]' -- Note the missing right parethesis
  ifPart <- gsub('if *\\(', '', trimws(ifPart),ignore.case=T) # Get rid of everything up to the first parethesis
  thenPart <-  gsub('(.*)then *','', trimws(parameter),ignore.case=T, perl=T) # (.+) Multiple wildcard
  
  testCode <-   paste ('Tdata[which(', ifPart ,' && !(',thenPart,') )',', ]', sep='')
  return(testCode)
}

#4- ValidDate 

DateValidGen <- function(var){ # Expects list of string values to be converted to POSIXct class
  
  #Tdata[which( class( try( as.POSIXct(Tdata$var) ) )  == "try-error" ) ,  ]
  
  testCode <-   paste ('Tdata[which( class( try( as.POSIXct(Tdata$',var
                       ,') ) )  == "try-error" ) ,  ]', sep='')
  return(testCode)
  
}



#Possibility to add flexibility - Check multiple types of formats
# IsDate <- function(mydate) { # Check is in any of the prescribed formats
#   # All possible date formats here 
#   date.format = c( "%d/%m/%y", "%d/%m/%y") 
#   for (i in length(date.format)) {
#       t[i] <- tryCatch(!is.na(as.Date(mydate, date.format [i])),  
#            error = function(err) {FALSE})
#   }
#   testRes <- any(t=TRUE) #If any TRUE return TRUE, else FALSE
#   return(testRes)
# }

#5- Duration



DateDurationGen <- function(var, parameter){ # Expects parameter such as ' as.POSIXct(Tdata$ED_ARRIVAL_TM), 'EDT', units = "mins" )> 20'
  #Base code
  #Tdata[which ( difftime(as.POSIXct(Tdata$INDEX_DATE), as.POSIXct(Tdata$ED_ARRIVAL_TM), 'EDT', units = "mins" )> 20 ) ,  ]
  
  #Code generator string
  #'Tdata[which ( difftime(as.POSIXct(Tdata$',var,'), ', parameter, ') ,  ]'
  
  testCode <-   paste ('Tdata[which ( difftime(as.POSIXct(Tdata$',var
                       ,'), ', parameter, ') ,  ]', sep='')
  return(testCode)
  
}




#6- DateBefore 

DateBeforeGen <- function(var, parameter){ # Expects base date and parameter as comparsion time, returns all values where var data comes after parameter date
  
  #Tdata[which ( as.POSIXct(Tdata$var)>as.POSIXct(Tdata$parameter) ) ,  ]
  
  testCode <-   paste ('Tdata[which ( as.POSIXct(strptime(Tdata$',var 
                       , ', "%Y-%m-%d %H:%M:%S"))>as.POSIXct(strptime(Tdata$', parameter
                       , ', "%Y-%m-%d %H:%M:%S")) ) ,  ]', sep='')
  return(testCode)
  
}


#7 - BlackList 
BlackListGen <- function(var, ListFilename){ # Expects list of values to be screened, returns values in list and in dataset, NO HEADER, sep='\n'
  
  #Tdata[Tdata$Var %in% read.csv(ListFilename),  ]
  
  testCode <-   paste ('Tdata[Tdata$', Var
                       ,'%in% read.csv(', 
                       ListFilename, '),  ]', sep='')
  return(testCode)
  
}

#--- Popularion-level checks -- Return True or False for whole requirement
#10- AllValuesPresent
AllValuesGen <- function(var, parameter){  #10- All Values Present -- Generates Query That returns TRUE or FALSE to all expected values present
  var <- toupper(var)
  
  # character, to variable conversion
  testCode <-   paste ('!(all( as.vector( na.omit( as.character( ', 
                       parameter,') ) ) == as.vector( na.omit( subset( Tdata$', var
                       , ', !duplicated(Tdata$', var
                       ,'))))))', sep='')
  return(testCode)
}

#11- Normality
NormalityGen <- function(var){  #11 - NormalityQuery Generator -- Returns TRUE for Normal, FALSE for not normal and NA if the sample size is out of range
  var <- toupper(var)
  # character, to variable conversion
  testCode <-   paste (' if (length(Tdata$', var 
                       ,')<5000 && length(Tdata$', var, 
                       ')>3 ) {c <- shapiro.test(as.numeric(Tdata$', var
                       ,')) } else  { return(NA) } \n if (c[2]<=0.05) {return (FALSE)} else {return (FALSE)}', sep='')
  return(testCode)
}



# # Code Used
# c <- numeric(0)
# if (length(Tdata$WEIGHT_LB)<5000 && length(Tdata$WEIGHT_LB)>3 ) { 
#   c <- shapiro.test(as.numeric(Tdata$WEIGHT_LB)) 
#     } else  { return(NA) } # Function not executed 
#  if (c[2]<=0.05) {return (FALSE)} else {return (FALSE)}


#FUTURE VERSIONS: #12- Skewness #13- Kurtosis

#14- DBSCAN: added by Cai, leave nmaxit and epsvar as parameters that could be tuned by users
DBSCAN <- function(Tdata, nmaxit,epsvar){ #default nmaxit=5, epsvar=0.05
  num_index <- rep(0, ncol(Tdata))
  for (i in 1:ncol(Tdata)){
    if (is.numeric(Tdata[,i])&(length(unique(Tdata[,i]))>10)
        &(!(is.integer(Tdata[,i])&(length(unique(Tdata[,i]))==nrow(Tdata))))){
      num_index[i] <- 1
    }
  }
  data_new <- Tdata[,num_index==1]
  
  imputed_data <- mice(data_new, maxit=nmaxit, method= 'pmm',seed=500)
  completeData <- mice::complete(imputed_data,4)
  df <- scale(completeData)
  
  d_clust <- Mclust(as.matrix(df), G=1:15, modelNames = mclust.options("emModelNames"))
  nkmeans <- min(parse_number(names(pickBIC(d_clust$BIC,5))))
  
  kNNdist <- sort(kNNdist(df,nkmeans))
  diffKNNdist <- diff(kNNdist)
  db <- dbscan(df,eps=kNNdist[which(diffKNNdist>epsvar)][1], minPts =nkmeans)
  
  # define outliers 
  merged_data <- data.frame(Tdata, db$cluster)
  outliers <- merged_data[merged_data$db.cluster !='1',]
  return(outliers)
}

#15  Then, like other query generators, we define a function called DBscanGen to generate a query
DBSCANGen <- function(parameter){ 
  testCode <- paste ('DBSCAN(Tdata,', parameter,')', sep='')
  return(testCode)
}


#20- Query Table Generator Function

GenerateQueries <- function (reqs){
  Queries<-data.frame("ReqID"=numeric(0), "QCode"=character(0), stringsAsFactors=FALSE)
  i <- 1 
  for (i in 1:nrow(reqs)){
    cat(reqs$ReqID[i], '...\n')
    if (reqs$ReqType[i]=='Unique') Queries[nrow(Queries)+1,] <- c(ReqID=reqs$ReqID[i], QCode=as.character(UniqueTestGen(reqs$Variable[i])))
    if (reqs$ReqType[i]=='CatChecks') Queries[nrow(Queries)+1,]  <- c(ReqID=reqs$ReqID[i], QCode=as.character(NoOtherValuesGen(reqs$Variable[i], reqs$Parameter[i])))
    if (reqs$ReqType[i]=='NoNULLS') Queries[nrow(Queries)+1,]  <- c(ReqID=reqs$ReqID[i], QCode=as.character(NoNULLGen(reqs$Variable[i]))) 
    
    if (reqs$ReqType[i]=='ValueRange') Queries[nrow(Queries)+1,]  <- c(ReqID=reqs$ReqID[i], QCode=as.character(ValueRangeGen(reqs$Variable[i], reqs$Parameter[i])))   
    if (reqs$ReqType[i]=='Formula') Queries[nrow(Queries)+1,]  <- c(ReqID=reqs$ReqID[i], QCode=as.character(FormulaGen(reqs$Parameter[i]))) 
    if (reqs$ReqType[i]=='Condition') Queries[nrow(Queries)+1,]  <-  c(ReqID=reqs$ReqID[i],  QCode=as.character(ConditionGen(reqs$Parameter[i]))) 
    if (reqs$ReqType[i]=='IfCondition') Queries[nrow(Queries)+1,]  <-  c(ReqID=reqs$ReqID[i],  QCode=as.character(IfConditionGen(reqs$Parameter[i]))) 
    
    if (reqs$ReqType[i]=='ValidDate') Queries[nrow(Queries)+1,]  <- c(ReqID=reqs$ReqID[i], QCode=as.character(DateValidGen(reqs$Variable[i])))  
    if (reqs$ReqType[i]=='DateDuration') Queries[nrow(Queries)+1,]  <- c(ReqID=reqs$ReqID[i], QCode=as.character(DateDurationGen(reqs$Variable[i], reqs$Parameter[i])))  
    if (reqs$ReqType[i]=='DateBefore') Queries[nrow(Queries)+1,]  <- c(ReqID=reqs$ReqID[i], QCode=as.character(DateBeforeGen(reqs$Variable[i], reqs$Parameter[i])))  
    
    if (reqs$ReqType[i]=='BlackListValues') Queries[nrow(Queries)+1,]  <- c(ReqID=reqs$ReqID[i], QCode=as.character(BlackListGen(reqs$Variable[i], reqs$Parameter[i])))  
    
    if (reqs$ReqType[i]=='AllValues') Queries[nrow(Queries)+1,]  <- c(ReqID=reqs$ReqID[i], QCode=as.character(AllValuesGen(reqs$Variable[i], reqs$Parameter[i])))  #Population Req.
    if (reqs$ReqType[i]=='Normality') Queries[nrow(Queries)+1,]  <-  c(ReqID=reqs$ReqID[i],  QCode=as.character(NormalityGen(reqs$Variable[i])))   #Population Req.
  }    
  return(Queries)
}

#21- Query Table Generator Function

GenerateQueriesAtReqs <- function (reqs){
  if (is.null(reqs$Query)!=T) reqs$Query[]<-''
  i <- 1 
  for (i in 1:nrow(reqs)){
    cat(reqs$ReqID[i], '...\n')
    if (reqs$ReqType[i]=='Unique') reqs$Query[i] <- as.character(UniqueTestGen(reqs$Variable[i]))
    if (reqs$ReqType[i]=='CatChecks') reqs$Query[i]  <- as.character(NoOtherValuesGen(reqs$Variable[i], reqs$Parameter[i]))
    if (reqs$ReqType[i]=='NoNULLS') reqs$Query[i]  <- as.character(NoNULLGen(reqs$Variable[i])) 
    
    if (reqs$ReqType[i]=='ValueRange') reqs$Query[i]  <- as.character(ValueRangeGen(reqs$Variable[i], reqs$Parameter[i]))   
    if (reqs$ReqType[i]=='Formula') reqs$Query[i]  <- as.character(FormulaGen(reqs$Parameter[i])) 
    if (reqs$ReqType[i]=='Condition') reqs$Query[i]  <- as.character(ConditionGen(reqs$Parameter[i])) 
    if (reqs$ReqType[i]=='IfCondition') reqs$Query[i]  <- as.character(IfConditionGen(reqs$Parameter[i])) 
    
    if (reqs$ReqType[i]=='ValidDate') reqs$Query[i]  <- as.character(DateValidGen(reqs$Variable[i]))  
    if (reqs$ReqType[i]=='DateDuration') reqs$Query[i]  <- as.character(DateDurationGen(reqs$Variable[i], reqs$Parameter[i]))  
    if (reqs$ReqType[i]=='DateBefore') reqs$Query[i]  <- as.character(DateBeforeGen(reqs$Variable[i], reqs$Parameter[i]))  
    
    if (reqs$ReqType[i]=='BlackListValues') reqs$Query[i]  <- as.character(BlackListGen(reqs$Variable[i], reqs$Parameter[i]))  
    
    if (reqs$ReqType[i]=='AllValues') reqs$Query[i]  <- as.character(AllValuesGen(reqs$Variable[i], reqs$Parameter[i]))  #Population Req.
    if (reqs$ReqType[i]=='Normality') reqs$Query[i]  <-  as.character(NormalityGen(reqs$Variable[i]))   #Population Req.
    
    if (reqs$ReqType[i]=='DBSCAN') reqs$Query[i]  <-  as.character(DBSCANGen(reqs$Parameter[i]))
    
  }
  return(reqs)
}