# cai
Introduction

Data Quality Module applies to data-preprocessing and data quality screening.
It assesses data quality and minimizes bias between data inputs and user's specific requiements.
Here are outlines of main goals about the module.
1)	Simple data manipulation according to user’s specific requirements.
2)	Monitor if data inputs are in correct formats and conditions, 
    automatically return a flaws report that contains unqualified data needing further adjustments. 
3)	Fit Machine Learning Methods (DBSCAN) into outlier detection, 
    ensuring data inputs not only meet user’s requirements but also in reasonable ranges.



Prerequiements
This module runs on R, need to install R and R studio.

This module requires the following modules:
* chron
* ggplot2
* mice
* cluster
* dbscan
* mclust
* readr


Module Contents

DQModuleFunctions:
#0 Variable name validation Function, when fed requirment and test dataset. 
will implement comparison then return a list of varibles that are required but not found in test dataset.

#1 Individual Query Generators, examines if the inputting variable is duplicated. 

#2 NoOtherValuesGen,  pull lines with categorical values outside good ranges when given required parameter(range). 

#3 NoNULLGen, pull lines in which the inputting variable has NULL values.

#4 valueRangeGen, pull lines in which the inputting variable has bad value range regarding to defined parameter(range).

#5 FormulaGen, pull lines that do not comply with good formula. 

#6 ConditionGen, pull lines that do not meet defined conditions. 

#7 IfConditionGen, derive new row that meets the IfCondition from exsiting row. 

#8 DateValidGen, convert list of string type Date into DateTime type.

#9 DateDurationGen, generate the date duration between datetimestamps.

#10 DateBeforeGen, examine if the variable date after the base date 

#11 NomalityGen, examines the normality of all variables in dataset, if the number of variable greater than 3 and less than 5000 will 
    return Nomality = True, otherwise will return False and NA,which is for the sample size out of range. 
    
#12 Skewbess 

#13 Kurtosis 

#14 DBSCAN, fit DBSCAN clustering method here, leave nmaxit and epsvar as parameters that could be tunned by user. Nmaxit value stands for the
    number of interaction of missing value imputation. Epsvar value determines the density of cluster and its expanding area. (The 
    preferred eplison values are 0.05 or 0.1, prior one is in a rigorous way to detect outliers whereas later one is in a loose way.
    
#15 DBSCANGen, invoke bunch of codes in #14 according to defined parameters.

#21 GenerateQueriesAtReqs, create a query column in which executable codes of each function are fitted next to the corresponding request type. 

all codes in #1-#15 are converted to character strings by paste function,and embedded in #21 


DQ preprocessing function:

Based on 'DQModuleFunctions.R'

#1 import both request file and test data. 

#2 GenerateDelinquents, this function will invoke the GenerateQueriesAtReqs function defined previously,
   and compare request file and test data to assess the data quality, then return a flaws report indicating all 
   unqualified data inputs that need close attentions and further adjustments.



