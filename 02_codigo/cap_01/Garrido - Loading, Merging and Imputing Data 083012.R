

library(Amelia)
library(car)
library(effects)
library(foreign)
library(ggplot2)
library(KMsurv)
library(MASS)
library(memisc)
library(splines)
library(statnet)
library(survival)
library(Zelig)

#### Programs needed to export results to LaTeX

source("/Users/segasi/Documents/Google/UCLA/Personal_Research/R to LaTeX/mtable-ext.R")
source("/Users/segasi/Documents/Google/UCLA/Personal_Research/R to LaTeX/ergmTable.r")
source("/Users/segasi/Documents/Google/UCLA/Personal_Research/R to LaTeX/relogitTable.r")


##################################################################################
####   
####   Chunk 1: Cleaning the working space and loading the data
####   
##################################################################################


### Cleaning

rm(list=ls(all=TRUE))

  
### Define working directory



setwd("/Users/segasi/Documents/Google/UCLA/Personal_Research/Regime_survival/Data/Merged databases/From R/")


#######
### Loads GWF SP from 1945 to 2010 , that is the part of the GWF 
### data set that corresponds to single-party regimes (previously divided 
### using Excel) 
#######

gwfsp <- read.csv("/Users/segasi/Documents/Google/UCLA/Personal_Research/Regime_survival/Data/GWF Global Regimes/gwfparty.csv",na.strings="NA")


#######
### Loads GWF SP from 1945 to 2009 (EXCLUDING 2010), that is the part of the GWF 
### data set that corresponds to single-party regimes (previously divided 
### using Excel) 
#######

gwfsp09 <- read.csv("/Users/segasi/Documents/Google/UCLA/Personal_Research/Regime_survival/Data/GWF Global Regimes/gwfparty09.csv",na.strings="NA")

#######
### Loads PWT 5.6 data set
#######

pwt5.6 <- read.csv("/Users/segasi/Documents/Google/UCLA/Personal_Research/Regime_survival/Data/Penn Tables/PWT 5.6.csv", na.strings="NA")

#######
### Loads PWT 7.0 data set
#######

pwt7.0 <- read.csv("/Users/segasi/Documents/Google/UCLA/Personal_Research/Regime_survival/Data/Penn Tables/PWT 7.0.csv", na.strings="NA", header = TRUE, dec = ".", sep =",")


#######
### Loads Maddison data set
#######

mad <- read.csv("/Users/segasi/Documents/Google/UCLA/Personal_Research/Regime_survival/Data/Maddison/Maddison GDP per capita with extra variables.csv", na.strings="NA", header = TRUE, dec = ".", sep =",")


#######
### Loads Ross' data on oil and gas
#######

ross <- read.csv("/Users/segasi/Documents/Google/UCLA/Personal_Research/Regime_survival/Data/Ross - Oil and Gas 1932-2010/Ross Oil & Gas value 2009.csv",na.strings="NA")



#######
###
### Check dimensions, names and structure of each database
###
#######


dim(gwfsp09) # Check dimensions
names(gwfsp09) # Check names
str(gwfsp09) # Check structure of the database

dim(pwt5.6) # Check dimensions
names(pwt5.6) # Check names
str(pwt5.6) # Check structure of the database

dim(pwt7.0) # Check dimensions
names(pwt7.0) # Check names
str(pwt7.0) # Check structure of the database

dim(mad) # Check dimensions
names(mad) # Check names
str(mad) # Check structure of the database

dim(ross) # Check dimensions
names(ross) # Check names
str(ross) # Check structure of the database



##################################################################################
####   
####   Chunk 2: Merging data sets to create sp09
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
### Creation of ''pure.party'', a merge of ''gwf.party'' and ''gwf.partyoli'' 
### that assigns a 1 to all PURE dominant-party regimes AND  
### dominant party Oligarchies, and 0 otherwise.  
############

### Creation

pure.party  <- sp09$gwf.party==1 | sp09$gwf.partyol==1 # I create a new logical variable that assigns a TRUE if either ``sp09$gwf.party'' or ``sp09$gwf.partyol'' are equal to 1, and 0 otherwise. 

pure.party[pure.party]  <- 1 # This command replaces the TRUE's for 1s and the FALSE's for 0s

### Verification

summary(sp09$gwf.regimetype)

count(pure.party)

### Addition to sp09

sp09$pure.party <- pure.party

names(sp09)




############
### Creation log1p(ross.oil_gas_valuePOP_2009) and addition to the 
### database with the name of "ross.logogvp09". I use log1p(x) instead 
### of log(x) because there are many 0s in Ross' data. log1p(x) computes
### log(1+x) 
############

# Creation

ross.logogvp09  <- log1p(sp09$ross.oil_gas_valuePOP_2009)

# Addition to sp09

sp09$ross.logogvp09 <- ross.logogvp09

names(sp09)




### Savinga .csv of "sp09"

write.csv(sp09,file="sp09.csv",na="NA",row.names=TRUE)





##################################################################################
####   
####   Chunk 2: Merging data sets to create spm
####   
##################################################################################

######
### Merge of gwfparty and pwt7.0
######

spm  <- merge(gwfsp,mad,by="id")

dim(spm) 
names(spm)
str(spm) 

### Addition of ross data to sp09

spm  <- merge(spm,ross,by="id")

dim(spm) # Check dimensions: Rows: 1725, Colums: 75
names(spm)
str(spm)





### Savinga .csv of "sp09"

write.csv(spm,file="spm.csv",na="NA",row.names=TRUE)










##################################################################################
####   
####   Chunk 4: IMPUTATION of Missing Data
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

pwt5.6.imp <- read.csv("/Users/Policarpo/Documents/UCLA/Personal_Research/Regime_survival/Data/Penn Tables/PWT 5.6.imp.csv", na.strings="NA")

dim(pwt5.6.imp) # Check dimensions
names(pwt5.6.imp) # Check names
str(pwt5.6.imp) # Check structure of the database


#######
### Loads PWT 7.0 data set for IMPUTATION
#######


pwt7.0.imp <- read.csv("/Users/Policarpo/Documents/UCLA/Personal_Research/Regime_survival/Data/Penn Tables/PWT 7.0.imp.csv", na.strings="NA", header = TRUE, dec = ".", sep =",")

dim(pwt7.0.imp) # Check dimensions
names(pwt7.0.imp) # Check names
str(pwt7.0.imp) # Check structure of the database


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


### Elimination of the (already empty) country-years of North Korea, Serbia and South Yemen


sp09.imp  <- subset(sp09.imp,gwf.casename!="Korea North 48-NA")

sp09.imp  <- subset(sp09.imp,gwf.casename!="Serbia 91-00")

sp09.imp  <- subset(sp09.imp,gwf.casename!="South Yemen 67-90")

dim(sp09.imp)


#############
### I. Imputing the missing data
#############


#########
### First step: Selection of the variables from the data frame pricol 
### that need to be loaded in Amelia for the imputation.

names(sp09.imp)

amelia.sp09.imp <- sp09.imp[,c(1,3,4,7,8,9,10,11,12,13,14,19,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,70,71,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,93,94,95,108,109,110,111,112,113,114,119)]

names(amelia.sp09.imp)


#########
### Second step: Imputing the data. The command idvars defines 
### "state.id", s identification variable in order to not include 
### it in the replication but to have it remain in the imputed datasets.

imputed.sp09 <- amelia(amelia.sp09.imp, m = 10, ts = "gwf.year", cs = "gwf.casename", p2s=1, logs=c("pwt5.rgdpch","pwt7.rgdpch.imp"), idvars=c("id"))

### Saving the imputations for replication

save(imputed.sp09, file = "sp09imputations.RData")



#########
### Third step: Diagnostic analysis of the quality of the imputation
###

missmap(imputed.sp09) ### Map that helps visualize the missignes in the dataset.


par(mfrow=c(1,1), mar=c(4, 6, 1, 0.5), oma=c(1.5,1.5,0,1.5))

compare.density(imputed.sp09, var = "pwt7.rgdpch.imp")



compare.density(imputed.sp09, var = "logpwt7.rgdpch")

### Diagnostic plot. The imputed curve (in red) plots the density of the mean imputation over the m datasets. That is, for each cell that is missing in the variable, the diagnostic will  												      find the mean of that cell across each of them data sets and use that value for the density plot. The black distributions are those of the observed data. In this case the plot shows that the distribution of the imputed data is similar to the distribution of the observed data, and, therefore, their means are also relatively close												     


overimpute(imputed.sp09, var = "pwt7.rgdpch") 

### Overimputation diagnostic graph. Here ninety per cent conﬁdence intervals are constructed that detail where an observed value would have been imputed had it been missing from the data set, given the imputation model. The dots represent the mean imputation. Around ninety per cent of these conﬁdence intervals contain the y =x line, which means that the true observed value falls withinthis range.

# On this graph, a y = x line indicates the line of perfect agreement; that is, if the imputation model was a perfect predictor of the true value, all the imputations would fall on this line. For each observation, Amelia also plots 90% conﬁdence intervals that allows the 												     user to visually inspect the behavior of the imputation model. In this case all the conﬁdence intervals contain the y =x line, which means that the true observed value falls within this range.

# The color of the line (as coded in the legend) represents the fraction of missing observations in the pattern of missingness for that observation.

disperse(imputed.sp09, dims = 1, m = 5) 

### Overdispersion diagnostic to make sure that our imputations do not depend on our starting values. The plot should show a well behaved likelihood, as the starting values all converge to the same point. In this case all starting values converge to the same point.





#########
##### Fourth step: Taking the imputations and defining them as 
###   a data.frame called "imputed.sp09.all" 

imputed.sp09.all  <- as.data.frame(imputed.sp09[[1]])

##### Fifth step: Create separeted objects for each imputation 

imputed.sp09.1  <- imputed.sp09.all[,1:71]
imputed.sp09.2  <- imputed.sp09.all[,72:142]
imputed.sp09.3  <- imputed.sp09.all[,143:213]
imputed.sp09.4  <- imputed.sp09.all[,214:284]
imputed.sp09.5  <- imputed.sp09.all[,285:355]
imputed.sp09.6  <- imputed.sp09.all[,356:426]
imputed.sp09.7  <- imputed.sp09.all[,427:497]
imputed.sp09.8  <- imputed.sp09.all[,498:568]
imputed.sp09.9  <- imputed.sp09.all[,569:639]
imputed.sp09.10  <- imputed.sp09.all[,640:710]



dim(imputed.sp09.2)


#########
### Sixth step: Compute the average value of each variable 
### for the 10 imputations and store it for later use

imputed.sp09.avg <- matrix(NA,2035,71)  #


# Creates a matrix full of NA with 2035 rows (the number of 
# observations) and 71 columns (the number of variables 
# included in the imputation)

mean.imputed <-  matrix(NA,2035,71)    

### Creates a second matrix full of NA with 2035 rows and 71 columns


for(i in 1:71){             
  
  ### This loop takes the value of each [j,i] 
  ### observation in the ten imputations                                    
  ### generated and calculates the average in                                    
  ### each case. Then, it stores the value in the 
  ### [j,i] cell of the "mean.imputed" matrix                                                                     
  ### previously created, to later store it in                                    
  ### the "imputed.pricol.avg" matrix which will 
  ### be later used to run the models again
  
  
  imputed.sp09.avg[,i] <- mean.imputed[,i]
  
  for (j in 1:2035){                       		                                      
    mean.imputed[j,i]<-mean(c(imputed.sp09.1[j,i],
                              
                              imputed.sp09.2[j,i],
                              imputed.sp09.3[j,i],
                              imputed.sp09.4[j,i],
                              imputed.sp09.5[j,i],
                              imputed.sp09.6[j,i],
                              imputed.sp09.7[j,i],
                              imputed.sp09.8[j,i],
                              imputed.sp09.9[j,i],
                              imputed.sp09.10[j,i]))
  } # end j loop
} # end i loop




colnames(mean.imputed) <- c("id","gwf.year","gwf.casename","gwf.ssafrica","gwf.samerica","gwf.nafrica","gwf.meast","gwf.easia","gwf.ceeurope","gwf.casia","gwf.cacar","gwf.duration","pwt5.pop","pwt5.rgdpch","pwt5.rgdpl","pwt5.c","pwt5.i","pwt5.g","pwt5.rgdptt","pwt5.y","pwt5.cgdp","pwt5.cc","pwt5.ci","pwt5.cg","pwt5.p","pwt5.pc","pwt5.pi","pwt5.pg","pwt5.xr","pwt5.rgdpea","pwt5.rgdpw","pwt5.kapw","pwt5.kdur","pwt5.knres","pwt5.kother","pwt5.kres","pwt5.ktranp","pwt5.open","pwt5.rgnp","pwt7.pop","pwt7.xrat","pwt7.ppp","pwt7.tcgdp","pwt7.cgdp","pwt7.cgdp2","pwt7.cda2","pwt7.cc","pwt7.cg","pwt7.ci","pwt7.p","pwt7.p2","pwt7.pc","pwt7.pg","pwt7.pi","pwt7.openc","pwt7.cgnp","pwt7.y","pwt7.y2","pwt7.rgdpl","pwt7.rgdpl2","pwt7.rgdpch.imp","logpwt7.rgdpch","logpwt7.rgdpch.imp","pwt7.kc","pwt7.kg","pwt7.ki","pwt7.openk","pwt7.rgdpeqa","pwt7.rgdpwok","pwt7.rgdpl2wok","incgrp")




mean.imputed.data  <- as.data.frame(mean.imputed) ### Define mean.imputed as a                                     												                 ### data frame 

names(amelia.sp09.imp)         ### Checking if the names of the imputed variables 
names(mean.imputed.data)      ### match the names of the variables originally 
### inlcuded for the imputation in "amelia.pricol". 





#########
### Seventh step: Adding the imputed versions of pwt7.rgdpch.imp
### in to sp09.imp

### Taking the imputed variable pwt7.rgdpch.imp (an exact copy of pwt7.rgdpch previously renamed as "pwt7.rgdpch.imp" in Excel) and pasting it in sp09.imp


sp09.imp$ampwt7.rgdpch  <- mean.imputed.data[,61] 


### Making sure pwt7.rgdpch.imp only differs from pwt7.rgdpch 
### in the imputed values. This is confirmed by the string 
### of 0s and NAs.

sp09.imp$pwt7.rgdpch - sp09.imp$pwt7.rgdpch.imp


dim(sp09.imp)


### Savinga .csv of "sp09.imp"

write.csv(sp09.imp,file="sp09.imp.csv",na="NA",row.names=TRUE)







##################################################################################
####   
####   Chunk 3: Merge of a new database with that includes all the variables of sp09, sp09.imp and several new variables that combine changes in GDP per capita from PWT 5.6 and PWT 7.0
####   
##################################################################################


#######
### Loading the dataset that includes all the variables in sp09, sp09.imp
### plus
#######

# pwt7.rgdpch.imp2  = A new version of pwt7.rgdpch.imp in which I erased the imputed values of those country-years that precede the first year for which any of the two versions of the PWT database included data.

# imputed.obs.original = Dummy variable that indicates the country-years for which Amelia originally imputed the data in pwt7.rgdpch.imp

# imputed.obs.final = Dummy variable that indicates the country-years for which I kept the imputed data calculated by Amelia

# comb.rgdpch.ch = This variable combines the yearly change in GDP per capita calculated from either PWT 5.6 or PWT 7. The data of PWT 7 was used for most regimes except Czechoslovakia 948-89 (years 1960-89), Germany, East 1949-90	(years 1970-88), Liberia 1944-80	(years 1960-69), Soviet Union 1917-91	(years 1960-89) and Yugoslavia 1945-90 (years 1960-90). In this five cases I used the GDP per capita data from PWT 5.6 to calculate the value of comb.rgdpch.ch 

# comb.rgdpch.ch.1lag = Same as comb.rgdpch.ch but with one year lag

# comb.rgdpch.ch.ma5a = Five-Years Moving Average constructed using comb.rgdpch.ch

# comb.rgdpch.ma7a = Seven-Years Moving Average constructed using comb.rgdpch.ch

# comb.rgdpch.dev7 =  Deviation from Seven-Years Moving Average constructed using comb.rgdpch.ch



#######
###
### Check dimensions, names and structure of each database
###
#######

sp09.comb <- read.csv("/Users/Policarpo/Documents/UCLA/Personal_Research/Regime_survival/Data/Merged databases/From R/sp09.comb.csv",na.strings="NA")

dim(sp09.comb) # Check dimensions
names(sp09.comb) # Check names
str(sp09.comb) # Check structure of the database


######
### Merge of sp09.comb with Ross data
######


sp09.comb  <- merge(sp09.comb,ross,by="id")

dim(sp09.comb) # Check dimensions
names(sp09.comb) # Check names
str(sp09.comb) # Check structure of the database


############
### Creation log1p(ross.oil_gas_valuePOP_2009) and addition to the 
### database with the name of "ross.logogvp09". I use log1p(x) instead 
### of log(x) because there are many 0s in Ross' data. log1p(x) computes
### log(1+x)
###
### Creation of log(sp09.comb$ampwt7.rgdpch) and addition to the database
### with the name "log.ampwt7rgdpch"
###
###
############

# Creation

ross.logogvp09  <- log1p(sp09.comb$ross.oil_gas_valuePOP_2009)

logpwt7rgdpch.imp2  <- log(sp09.comb$pwt7.rgdpch.imp2)


# Addition of the variables to sp09.comb

sp09.comb$ross.logogvp09 <- ross.logogvp09

sp09.comb$log.ampwt7rgdpch <- log.ampwt7rgdpch



### Saving the new file as 

write.csv(sp09.comb,file="sp09.comb.analysis.csv",na="NA",row.names=TRUE)




