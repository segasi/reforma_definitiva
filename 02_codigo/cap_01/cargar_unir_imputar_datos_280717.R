##################################################################################
####   
####  Chunk 1: Cleaning the working space and loading the data
####   
##################################################################################

### Cleaning

rm(list=ls(all=TRUE))


#######
### Loads GWF SP, that is the part of the GWF data set that corresponds to single-party regimes 
### (previously divided using Excel) 
#######

gwfsp09 <- read.csv("01_datos/gwfparty09.csv",na.strings="NA")

dim(gwfsp09) # Check dimensions
names(gwfsp09) # Check names
str(gwfsp09)


#######
### Loads PWT 5.6 data set
#######

pwt5.6 <- read.csv("01_datos/PWT_5.6.csv", na.strings="NA")

dim(pwt5.6) # Check dimensions
names(pwt5.6) # Check names
str(pwt5.6) # Check structure of the database


#######
### Loads PWT 7.0 data set
#######

pwt7.0 <- read.csv("01_datos/PWT_7.0.csv", na.strings="NA", header = TRUE, dec = ".", sep =",")

dim(pwt7.0) # Check dimensions
names(pwt7.0) # Check names
str(pwt7.0) # Check structure of the database



#######
### Loads Ross' data on oil and gas
#######

ross  <- read.csv("01_datos/Ross_Oil_Gas_value_2009.csv", na.strings="NA")

dim(ross) # Check dimensions
names(ross) # Check names
str(ross) # Check structure of the database




##################################################################################
####   
####  Chunk 2: Merging data sets to create sp09
####   
##################################################################################



######
### Merge of gwfparty and pwt7.0
######

sp09  <- merge(gwfsp09,pwt7.0,by="id")

dim(sp09) 
names(sp09)
str(sp09) 

### Addition of ross data to sp09

sp09  <- merge(sp09,ross,by="id")

dim(sp09) # Check dimensions: Rows: 1725, Colums: 75
names(sp09)
str(sp09)


############
#### Creation log1p(ross.oil_gas_valuePOP_2009) and addition to the database with the name of "ross.logogvp09". I use log1p(x) instead of log(x) because there are many 0s in Ross' data. log1p(x) computes log(1+x) 
############

# Creation

ross.logogvp09  <- log1p(sp09$ross.oil_gas_valuePOP_2009)


# Addition to sp09

sp09$ross.logogvp09 <- ross.logogvp09



### Savinga .csv of "sp09"

write.csv(sp09,file="03_datos_generados/sp09.csv",na="NA",row.names=TRUE)



##################################################################################
####   
####   Chunk 3: IMPUTATION of Missing Data
####   
##################################################################################




#####
##### Imputation of missing values of GDP per capita in PWT 7.0
#####


## In order to create dataframes of equal length for imputation,  gwfsp09 defined the universe of regime-years observations. Then, I checked how many of these regime-years were missing in pwt5.6.imp, pwt.7.imp and oil.imp. If any, I added the corresponding missing regime-years, the name of the country and the year, leaving all variables empty. 

# More regime-years had to be added to pwt5.6.imp because along with whole regimes that are missing, I also had to add the regime-years of those autocracies that ended after 1992 or that were still in power as 2009.



#######
### Loads PWT 5.6 data set for IMPUTATION
#######

pwt5.6.imp  <- read.csv("01_datos/PWT_5.6_para_imputacion.csv", na.strings="NA")

dim(pwt5.6.imp) # Check dimensions
names(pwt5.6.imp) # Check names
str(pwt5.6.imp) # Check structure of the database


#######
### Loads PWT 7.0 data set for IMPUTATION
#######


pwt7.0.imp  <- read.csv("01_datos/PWT_7.0_para_imputacion.csv", na.strings="NA")

dim(pwt7.0.imp) # Check dimensions
names(pwt7.0.imp) # Check names
str(pwt7.0.imp) # Check structure of the database


#######
### Loads Ross' data for IMPUTATION
#######

ross.imp  <- read.csv("01_datos/Ross_Oil_Gas_value_2009_para_imputacion.csv",na.strings="NA")

dim(ross.imp) # Check dimensions
names(ross.imp) # Check names
str(ross.imp) # Check structure of the database




######
### Creation of "sp09.imp" merging data from gwfparty and pwt5.6.imp
######


sp09.imp  <- merge(gwfsp09,pwt5.6.imp,by="id")

dim(sp09.imp) 
names(sp09.imp)
str(sp09.imp)


### Addition of pwt7.0.imp data

sp09.imp  <- merge(sp09.imp,pwt7.0.imp,by="id")

dim(sp09.imp) 
names(sp09.imp)
str(sp09.imp) 


### Addition of oil.imp data to sp09

sp09.imp  <- merge(sp09.imp,ross.imp,by="id")

dim(sp09.imp) 
names(sp09.imp)
str(sp09.imp)



#############
### I. Imputing the missing data
#############

##### First step: Selection of the variables from the data frame sp09.imp that need to be loaded in Amelia for the imputation.

names(sp09.imp)

amelia.sp09.imp <- sp09.imp[,c(1,3,4,13,14,19,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,71,72,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,93,95,97,98,99,100,104,105,107,109,110,111,112,114,115,117,138)]

names(amelia.sp09.imp)

##### Second step: Imputing the data. The command idvars defines "state.id", s identification variable in order to not include it in the replication but to have it remain in the imputed datasets.
library(Amelia)
sp09_imputado <- amelia(amelia.sp09.imp, m = 5, ts = "gwf.year", cs = "gwf.casename", p2s=1, idvars=c("id"))

sp09.imp$gwf.casename
### Saving the imputations for replication

save(imputed.sp09, file = "sp09imputations.RData")


##### Third step: Diagnostic analysis of the quality of the imputation

missmap(imputed.sp09) ### Map that helps visualize the missignes in the dataset.

compare.density(imputed.sp09, var = "pwt7.rgdpch") ### Diagnostic plot. The imputed  
### curve (in red) plots the density of the  												      ### mean imputation over the m datasets.  												      ### That is, for each cell that is missing  												      ### in the variable, the diagnostic will  												      ### find the mean of that cell across each  												      ### of them data sets and use that value  												      ### for the density plot. The black 												      ### distributions are those of the 												      ### observed data. 
### 
### In this case the plot shows that the
### distribution of the imputed data is 												      ### pretty similar to the distribution of 												      ### the observed data, and, therefore, 												      ### their means are also relatively close												     
overimpute(imputed.sp09, var = "pwt7.rgdpch") ### Overimputation diagnostic graph. Here 												      ### ninety per cent conﬁdence intervals are 												      ### constructed that detail where an 												      ### observed value would have been imputed 												      ### had it been missing from the data set, 												      ### given the imputation model. The dots 												      ### represent the mean imputation. Around 												      ### ninety per cent of these conﬁdence 												      ### intervals containthe y =x line, which 												      ### means that the true observed value 												      ### falls withinthis range.
### 
### On this graph, a y = x line indicates 												      ### the line of perfect agreement; that is, 												      ### if the imputation model was a perfect 												      ### predictor of the true value, all the 												      ### imputations would fall on this line. 												      ### For each observation, Amelia also plots 												      ### 90% conﬁdence intervals that allows the 												      ### user to visually inspect the behavior 												      ### of the imputation model.
### 												      ### In this case all the conﬁdence 
### intervals contain the y =x line, 												      ### which means that the true observed 												      ### value falls within this range.

disperse(imputed.sp09, dims = 1, m = 5) ### Overdispersion diagnostic to make 												      ### sure that our imputations do not depend 												      ### on our starting values. The plot shows 												      ### a well behaved likelihood, as the 												      ### starting values all converge to the 												      ### same point. 
### 
### In this case the plot all starting 												      ### values converge to the same point.



### Savinga .csv of "sp09.imp"

write.csv(sp09.imp,file="sp09.imp.csv",na="NA",row.names=TRUE)







