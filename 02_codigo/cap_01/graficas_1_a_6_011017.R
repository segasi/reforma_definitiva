################
################
#####
#####  Chunk 1: Loading required libraries
#####
################
################

library(foreign)
library(splines)
library(car)
library(MASS)
library(sna)
library(grid)
library(graphics)


################
################
#####
#####  Chunk 2: Cleaning the working space and loading the different data bases
#####
################
################

####

### Cleaning

rm(list=ls(all=TRUE))



### Loading data on DPRs's end between 1945 and 2010

dpend  <- read.csv("/Users/segasi 1/Google/CIDE/Investigación/La reforma definitiva/Análisis/Cap_02/bd/sp09.ended.csv",na.strings="NA")


### Loading a version of SP09 that erases the observations of year 0 in 
### gwf.duration.inv in order to make Figure 4


ecodata <- read.csv("/Users/segasi 1/Google/CIDE/Investigación/La reforma definitiva/Análisis/Cap_02/bd/sp09.5yrma.csv",na.strings="NA")


#######
### Loads GWF SP from 1945 to 2010 , that is the part of the GWF 
### data set that corresponds to single-party regimes (previously divided 
### using Excel) 
#######

gwfsp <- read.csv("/Users/segasi 1/Google/CIDE/Investigación/La reforma definitiva/Análisis/Cap_02/bd/gwfparty.csv",na.strings="NA")


#######
### Loads GWF SP from 1945 to 2009 (EXCLUDING 2010), that is the part of the GWF 
### data set that corresponds to single-party regimes (previously divided 
### using Excel) 
#######

gwfsp09 <- read.csv("/Users/segasi 1/Google/CIDE/Investigación/La reforma definitiva/Análisis/Cap_02/bd/gwfparty09.csv",na.strings="NA")

#######
### Loads PWT 5.6 data set
#######

pwt5.6 <- read.csv("/Users/segasi 1/Google/CIDE/Investigación/La reforma definitiva/Análisis/Cap_02/bd/PWT 5.6.csv", na.strings="NA")

#######
### Loads PWT 7.0 data set
#######

pwt7.0 <- read.csv("/Users/segasi 1/Google/CIDE/Investigación/La reforma definitiva/Análisis/Cap_02/bd/PWT 7.0.csv", na.strings="NA", header = TRUE, dec = ".", sep =",")


#######
### Loads Maddison data set
#######

mad <- read.csv("/Users/segasi 1/Google/UCLA/Personal_Research/Regime_survival/Data/Maddison/Maddison GDP per capita with extra variables.csv", na.strings="NA", header = TRUE, dec = ".", sep =",")


#######
### Loads Ross' data on oil and gas
#######

ross <- read.csv("/Users/segasi 1/Google/UCLA/Personal_Research/Regime_survival/Data/Ross - Oil and Gas 1932-2010/Ross Oil & Gas value 2009.csv",na.strings="NA")

##################################################################################
####   
####   Chunk 2: Merging data sets to create sp09
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

sp09[1:30,1:50]





################
################
#####
#####  Chunk 3: Figure 1 Frequencies of DPRs that ended b/w 1945-2010
#####
################
################

par(mfrow=c(1,1), mar=c(4.1, 5.1, 0.5, 2.1), oma=c(2,2,1,2))

plot(dpend$year, dpend$Ended, type ="l", lty=3, lwd=5, col="grey", xlab="\n Año", ylab="Número", main="",frame=F, xlim=c(1940,2010), ylim=c(0,60), las=1, cex.main = 2, cex.axis = 1.8, cex.lab = 1.8)

par(new=T)

plot(dpend$year, dpend$Cummulative.Ends, type ="l", lty=1, lwd=5, col="black", xlab="", ylab="", axes=F, frame=F, xlim=c(1940,2010), ylim=c(0,60)) 

legend(1938,62, c("Frecuencia anual"), cex=1.8, col=c("grey"), lty=c(3), lwd=5, bty="n")

legend(1938,59, c("Frecuencia acumulada"), cex=1.8, col=c("black"), lty=c(1), lwd=5, bty="n")


### Kathy's proposal

plot(dpend$year, dpend$Ended, type ="l", lty=1, lwd=3, col="grey", xlab="Year", ylab="Number of regimes ended per year", frame=F, xlim=c(1940,2010), ylim=c(0,8), las=1, cex.axis = 1.5, cex.lab = 1.5)





################
################
#####
#####  Chunk 4: Figure 2. Duration in power of the DPRs
#####
################
################


### Subset of last country-year for all DPR

gwfsplastyr <- subset(gwfsp, gwf.duration.inv== "0")


par(mfrow=c(1,1), mar=c(4.2, 6, 1, 0.5), oma=c(1.5,1.5,0,1.5))

# Duration of Dominant-Party Regimes, GWF SP09

plot(density(gwfsplastyr$gwf.duration), xlim=c(3.5,100), ylim=c(0,0.025), las=1, lwd=5, frame.plot=F, xlab="Años en el poder", ylab="Densidad \n", main="", cex.axis = 1.5, cex.lab = 1.5)

par(new=T)

plot(density(gwfsplastyr$gwf.duration[ gwfsplastyr$collapse==1]), xlim=c(3.5,100), ylim=c(0,0.025),  xlab="", ylab="", xaxt="n", yaxt="n", main="", col="dimgrey", lty=3, lwd=5, frame.plot=F)

par(new=T)

plot(density(gwfsplastyr$gwf.duration[ gwfsplastyr$collapse==0]), xlim=c(3.5,100), ylim=c(0,0.025),  xlab="", ylab="", xaxt="n", yaxt="n", main="", col="grey", lty=5, lwd=5, frame.plot=F)

legend(70.5,0.026, c("Todos los regímenes (n = 76)"), cex=1.3, col=c("black"), lty=c(1), bty="n", lwd=5)

legend(70.5,0.024, c("Concluidos (n = 52)"), cex=1.3, col=c("dimgrey"), lty=c(3), bty="n", lwd=5)

legend(70.5,0.022, c("Continuaban en 2010 (n = 24)" ), cex=1.3, col=c("grey"), lty=c(5), bty="n", lwd=5)


segments(29.5, 0, 29.5, 0.0148, col="black", lwd=5)

segments(23, 0, 23, 0.0166,lty=3, col="dimgrey", lwd=5)

segments(39.5, 0, 39.5, 0.017,lty=5, col="grey", lwd=5)






################
################
#####
#####  Chunk 5: Figure 3. Three Alternative Measurements of Economic Performance 
#####           of the Universe of Dominant-Party Regimes, All Based on the 
#####           Real GDP per capita (2005 prices)
#####
################
################


## Division of the screen

split.screen( figs = c( 2, 1 ) ) ##  Split the screen into two rows and one column, defining screens 1 and 2.

split.screen( figs = c( 1, 2 ), screen = 1 ) ##  Split screen 1 into one row and two columns, defining screens 3 and 4

split.screen( figs = c( 1, 1 ), screen = 2 ) ##  Split screen 2 into one row and one column, defining screen 5


##  The boxplots of Cambio porcentual in GDP per capita are located in screen 3:

screen( 3 )

par(mar=c(4,4,2.5,1.5))

boxplot(sp09$pwt7.rgdpch.ch[sp09$collapse==1]~sp09$gwf.duration.inv[sp09$collapse==1], frame.plot=F, axes=T, xlab="Número de años antes de la transición", ylab="Cambio porcentual", ylim=c(-20,20), xaxt="n", cex.axis=.71, cex.lab=.95, las=1)
axis(1, at=c(0,5,10,15,20,25,30,35,40,45,50), lab=c("-50","-45","-40","-35","-30","-25","-20","-15","-10","-5","0"), cex.axis=.71)        
title(main="(a) Cambio en PIB per capita (46 regímenes)", cex.main=.95)
segments(-1, 0, 50.8, 0, col="grey", lty=3, lwd=3)


##  The boxplots of Five-Years Moving Avg. are located in screen 4:

screen( 4 )

par(mar=c(4,4,2.5,1.5))

boxplot(sp09$pwt7.rgdpch.ch.ma5a[sp09$collapse==1]~sp09$gwf.duration.inv[sp09$collapse==1], axes=T, xlab="Número de años antes de la transición", ylab="Cambio porcentual", frame.plot=F, ylim=c(-20,20),xaxt="n", cex.axis=.71, cex.lab=.95, las=1)
axis(1, at=c(1,6,11,16,21,26,31,36,41,46), lab=c("-45","-40","-35","-30","-25","-20","-15","-10","-5","0"), cex.axis=.71)
title(main="(b) Promedio móvil de cinco años (44 regímenes)", cex.main=.95)
segments(-1, 0, 46.8, 0, col="grey", lty=3, lwd=3)


##  The Boxplots of Deviations from the Seven-Years Moving Avg. are located in screen 5:

screen( 5 )

par(mar=c(4,14,3.5,14))

names(sp09)

boxplot(sp09$pwt7.rgdpch.dev7[sp09$collapse==1]~sp09$gwf.duration.inv[sp09$collapse==1], axes=T, xlab="Número de años antes de la transición", ylab="Cambio porcentual", frame.plot=F, ylim=c(-20,20),xaxt="n", cex.axis=.71, cex.lab=.95, las=1)
axis(1, at=c(-6,-1,4,9,14,19,24,29,34,39,44), lab=c("-50","-45","-40","-35","-30","-25","-20","-15","-10","-5","0"), cex.axis=.71)
title(main="(c) Desviación del promedio móvil de siete años  (43 regímenes)", cex.main=.95)
segments(-7, 0, 44.8, 0, col="grey", lty=3, lwd=3)


##  Close all screens.

close.screen( all = TRUE )


################
################
#####           
#####   Same graphs, now separated 
#####
################
################

# Graph a: Cambio porcentual in GDP per capita


par(mfrow=c(1,1), mar=c(3.9, 4.1, 2.1, 2.1), oma=c(2,2,1,2), family="Baskerville Old Face")

boxplot(sp09$pwt7.rgdpch.ch[sp09$collapse==1]~sp09$gwf.duration.inv[sp09$collapse==1], frame.plot=F, axes=T, xlab="Número de años antes de la transición", ylab="Cambio porcentual", ylim=c(-20,20), xaxt="n", cex.lab=1.3, cex.axis=1.3, las=1)
axis(1, at=c(0,5,10,15,20,25,30,35,40,45,50), lab=c("-50","-45","-40","-35","-30","-25","-20","-15","-10","-5","0"), cex.axis=1.3)        
title(main="(a) Short-Term Growth (46 regimes)", cex.main=1.5)
segments(-1, 0, 50.8, 0, col="grey", lty=3, lwd=3)


# Graph b: Five-Years Moving Avg.

par(mfrow=c(1,1), mar=c(3.9, 4.1, 2.1, 2.1), oma=c(2,2,1,2), family="Helvetica")

boxplot(sp09$pwt7.rgdpch.ch.ma5a[sp09$collapse==1]~sp09$gwf.duration.inv[sp09$collapse==1], axes=T, xlab="Número de años antes de la transición", ylab="Cambio porcentual", frame.plot=F, ylim=c(-20,20),xaxt="n", cex.axis=1.3, cex.lab=1.3, las=1)
axis(1, at=c(1,6,11,16,21,26,31,36,41,46), lab=c("-45","-40","-35","-30","-25","-20","-15","-10","-5","0"), cex.axis=1.3)
title(main="(b) Promedio móvil de cinco años (44 regímenes)", cex.main=1.5)
segments(-1, 0, 46.8, 0, col="grey", lty=3, lwd=3)


# Graph c: Seven-Years Moving Avg. 

par(mfrow=c(1,1), mar=c(3.9, 4.1, 2.1, 2.1), oma=c(2,2,1,2), family="Baskerville Old Face")

boxplot(sp09$pwt7.rgdpch.dev7[sp09$collapse==1]~sp09$gwf.duration.inv[sp09$collapse==1], axes=T, xlab="Número de años antes de la transición", ylab="Cambio porcentual", frame.plot=F, ylim=c(-20,20),xaxt="n", cex.axis=1.3, cex.lab=1.3, las=1)
axis(1, at=c(-6,-1,4,9,14,19,24,29,34,39,44), lab=c("-50","-45","-40","-35","-30","-25","-20","-15","-10","-5","0"), cex.axis=1.3)
title(main="(c) Desviación del promedio móvil de siete años  (43 regímenes)", cex.main=1.5)
segments(-7, 0, 44.8, 0, col="grey", lty=3, lwd=3)





################
################
#####           
#####   Same graphs, now in one single column AQUI
#####
################
################

# Graph a: Cambio porcentual in GDP per capita

par(mfrow=c(3,1), mar=c(5.1, 4.1, 4.1, 2.1), oma=c(2,2,1,2))

boxplot(sp09$pwt7.rgdpch.ch[sp09$collapse==1]~sp09$gwf.duration.inv[sp09$collapse==1], frame.plot=F, axes=T, xlab="Número de años antes de la transición", ylab="Cambio porcentual", ylim=c(-20,20), xaxt="n", cex.axis=.71, cex.lab=.95, las=1)
axis(1, at=c(0,5,10,15,20,25,30,35,40,45,50), lab=c("-50","-45","-40","-35","-30","-25","-20","-15","-10","-5","0"), cex.axis=.71)        
title(main="(a) Cambio en PIB per capita (46 regímenes)", cex.main=.95)
segments(-1, 0, 50.8, 0, col="darkgrey", lty=3, lwd=2)



# Graph b: Five-Years Moving Avg.

boxplot(sp09$pwt7.rgdpch.ch.ma5a[sp09$collapse==1]~sp09$gwf.duration.inv[sp09$collapse==1], axes=T, xlab="Número de años antes de la transición", ylab="Cambio porcentual", frame.plot=F, ylim=c(-20,20),xaxt="n", cex.axis=.71, cex.lab=.95, las=1)
axis(1, at=c(1,6,11,16,21,26,31,36,41,46), lab=c("-45","-40","-35","-30","-25","-20","-15","-10","-5","0"), cex.axis=.71)
title(main="(b) Promedio móvil de cinco años (44 regímenes)", cex.main=.95)
segments(-1, 0, 46.8, 0, col="darkgrey", lty=3, lwd=2)


# Graph c:Seven-Years Moving Avg. 


boxplot(sp09$pwt7.rgdpch.dev7[sp09$collapse==1]~sp09$gwf.duration.inv[sp09$collapse==1], axes=T, xlab="Número de años antes de la transición", ylab="Cambio porcentual", frame.plot=F, ylim=c(-20,20),xaxt="n", cex.axis=.71, cex.lab=.95, las=1)
axis(1, at=c(-1,4,9,14,19,24,29,34,39,44), lab=c("-45","-40","-35","-30","-25","-20","-15","-10","-5","0"), cex.axis=.71)
title(main="(c) Desviación del promedio móvil de siete años  (43 regímenes)", cex.main=.95)
segments(-7, 0, 44.8, 0, col="darkgrey", lty=3, lwd=2)




################
################
#####
#####  Chunk 6: Figure 4. 
#####
################
################


## Division of the screen

split.screen( figs = c( 2, 1 ) ) ##  Split the screen into two rows and one column, defining screens 1 and 2.

split.screen( figs = c( 1, 2 ), screen = 1 ) ##  Split screen 1 into one row and two columns, defining screens 3 and 4

split.screen( figs = c( 1, 1 ), screen = 2 ) ##  Split screen 2 into one row and one column, defining screen 5


##  The boxplots of Cambio porcentual in GDP per capita are located in screen 3:

screen( 3 )

par(mar=c(4.2,4,2.5,1.5))


boxplot(ecodata$pwt7.rgdpch.ch~ecodata$gwf.failalt2, axes=T, xlab="Regime Status", ylab="Cambio porcentual", ylim=c(-15,15), las=1, xaxt="n", frame.plot=F)

axis(1, tick=TRUE, labels=FALSE,at=seq(0.,4,1))

abline(h=0, lty=3, lwd=3, col="grey")

mtext("Ended, year", side=1, line=0.7, adj=.15, cex=.78)
mtext("before transition", side=1, line=1.7, adj=0.13, cex=.78)

mtext("Ended, previous", side=1, line=0.7, adj=.52, cex=.78)
mtext("years", side=1, line=1.7, adj=0.50, cex=.78)

mtext("Continued", side=1, line=0.7, adj=.87, cex=.78)
mtext("as of 2009", side=1, line=1.7, adj=0.87, cex=.78)

title(main="(a) Change in GDP per capita", cex.main=1)



##  The boxplots of Five-Years Moving Avg. are located in screen 4:

screen( 4 )

par(mar=c(4.2,4,2.5,1.5))


boxplot(ecodata$pwt7.rgdpch.ch.ma5a~ecodata$gwf.failalt2, axes=T, xlab="Regime Status", ylab="Cambio porcentual", ylim=c(-15,15), las=1, xaxt="n", frame.plot=F)

axis(1, tick=TRUE, labels=FALSE,at=seq(0.,4,1))

abline(h=0, lty=3, lwd=3, col="grey")

mtext("Ended, year", side=1, line=0.7, adj=.15, cex=.78)
mtext("before transition", side=1, line=1.7, adj=0.13, cex=.78)

mtext("Ended, previous", side=1, line=0.7, adj=.52, cex=.78)
mtext("years", side=1, line=1.7, adj=0.50, cex=.78)

mtext("Continued", side=1, line=0.7, adj=.87, cex=.78)
mtext("as of 2009", side=1, line=1.7, adj=0.87, cex=.78)

title(main="(b) Five-Years Moving Avgerage", cex.main=1)



##  The Boxplots of Deviations from the Seven-Years Moving Avg. are located in screen 5:

screen( 5 )

par(mar=c(4.2,16,3.5,16))

boxplot(ecodata$pwt7.rgdpch.dev7~ecodata$gwf.failalt2, axes=T, xlab="Regime Status", ylab="Cambio porcentual", ylim=c(-15,15), las=1, xaxt="n", frame.plot=F)

axis(1, tick=TRUE, labels=FALSE,at=seq(0.,4,1))

abline(h=0, lty=3, lwd=3, col="grey")

mtext("Ended, year", side=1, line=0.7, adj=.15, cex=.78)
mtext("before transition", side=1, line=1.7, adj=0.13, cex=.78)

mtext("Ended, previous", side=1, line=0.7, adj=.52, cex=.78)
mtext("years", side=1, line=1.7, adj=0.50, cex=.78)

mtext("Continued", side=1, line=0.7, adj=.87, cex=.78)
mtext("as of 2009", side=1, line=1.7, adj=0.87, cex=.78)

title(main="(c) Deviation from Seven-Years Moving Avg.", cex.main=1)


##  Close all screens.

close.screen( all = TRUE )






################
################
#####           
#####   Same graphs, now in one column
#####
################
################

# Graph a: Cambio porcentual in GDP per capita

par(mfrow=c(3,1), mar=c(5.1, 4.1, 4.1, 0.5), oma=c(2,2,1,2), family="Baskerville Old Face")

boxplot(ecodata$pwt7.rgdpch.ch~ecodata$gwf.failalt2, axes=T, xlab="", ylab="Cambio porcentual", ylim=c(-15,15), las=1, xaxt="n", frame.plot=F, cex.lab=1.5, cex.axis = 1.3)
axis(1, tick=TRUE, labels=FALSE,at=seq(0.,4,1))
abline(h=0, lty=3, lwd=3, col="grey")
mtext("Ended, year", side=1, line=0.7, adj=0.15, cex=1.3)
mtext("before transition", side=1, line=1.9, adj=0.14, cex=1.3)
mtext("Ended, all years", side=1, line=0.7, adj=.50, cex=1.3)
mtext("before year -1", side=1, line=1.9, adj=0.50, cex=1.3)
mtext("Continued as of", side=1, line=0.7, adj=0.85, cex=1.3)
mtext("2009, all years", side=1, line=1.9, adj=0.85, cex=1.3)
mtext("Regime Status", side=1, line=3.7, adj=.5, cex=1.5)
title(main="(a) Short-Term Growth", cex.main=1.5)


#Graph b: Five-Years Moving Avg.

boxplot(ecodata$pwt7.rgdpch.ch.ma5a~ecodata$gwf.failalt2, axes=T, xlab="", ylab="Cambio porcentual", ylim=c(-15,15), las=1, xaxt="n", frame.plot=F, cex.lab=1.5, cex.axis = 1.3)
axis(1, tick=TRUE, labels=FALSE,at=seq(0.,4,1))
abline(h=0, lty=3, lwd=3, col="grey")
mtext("Ended, year", side=1, line=0.7, adj=0.15, cex=1.3)
mtext("before transition", side=1, line=1.9, adj=0.14, cex=1.3)
mtext("Ended, all years", side=1, line=0.7, adj=.50, cex=1.3)
mtext("before year -1", side=1, line=1.9, adj=0.50, cex=1.3)
mtext("Continued as of", side=1, line=0.7, adj=0.85, cex=1.3)
mtext("2009, all years", side=1, line=1.9, adj=0.85, cex=1.3)
mtext("Regime Status", side=1, line=3.7, adj=.5, cex=1.5)
title(main="(b) Five-Years Moving Avgerage", cex.main=1.5)


# Graph c: Deviations from the Seven-Years Moving Avg. 

boxplot(ecodata$pwt7.rgdpch.dev7~ecodata$gwf.failalt2, axes=T, xlab="", ylab="Cambio porcentual", ylim=c(-15,15), las=1, xaxt="n", frame.plot=F, cex.lab=1.5, cex.axis = 1.3)
axis(1, tick=TRUE, labels=FALSE,at=seq(0.,4,1))
abline(h=0, lty=3, lwd=3, col="grey")
mtext("Ended, year", side=1, line=0.7, adj=0.15, cex=1.3)
mtext("before transition", side=1, line=1.9, adj=0.14, cex=1.3)
mtext("Ended, all years", side=1, line=0.7, adj=.50, cex=1.3)
mtext("before year -1", side=1, line=1.9, adj=0.50, cex=1.3)
mtext("Continued as of", side=1, line=0.7, adj=0.85, cex=1.3)
mtext("2009, all years", side=1, line=1.9, adj=0.85, cex=1.3)
mtext("Regime Status", side=1, line=3.7, adj=.5, cex=1.5)
title(main="(c) Deviation from Seven-Years Moving Avg.", cex.main=1.5)






################
################
#####
#####  Chunk 6: Figure 7. Five-Years Moving Average Density 
#####
################
################



summary(sp09$pwt7.rgdpch.ch.ma5a)

hist(sp09$pwt7.rgdpch.ch.ma5a, breaks=100)

plot(density(sp09$pwt7.rgdpch.ch.ma5a, na.rm = TRUE))

?hist


sd(sp09$pwt7.rgdpch.ch.ma5a, na.rm = TRUE)

?sd



summary(sp09$pwt7.rgdpch.ch.ma5a)

hist(sp09$pwt7.rgdpch.ch.ma5a, breaks=100)



plot(density(sp09$pwt7.rgdpch.ch.ma5a, na.rm = TRUE), xlim=c(-15,15), ylim=c(0,0.15), las=1, lwd=2, frame.plot=F, xlab="Years", ylab="Density \n", main="", cex.axis = 1.5, cex.lab = 1.5)

abline(v=2.491, lty=3, col="grey",lwd=2)

abline(v=-1.315306, lty=3, col="grey",lwd=2)

abline(v=6.297306, lty=3, col="grey",lwd=2)



sp09$pwt7.rgdpch.ch.ma5a,pmin(y1,y2),col="gray")
