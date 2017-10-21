### Cargar paquetes ----
# install.packages(pacman) En caso de no tener pacamn instalado previamente
library(pacman)
p_load(car, foreign, graphics, grid, gridExtra, 
       lattice, MASS, splines, sna, tidyverse)


### Importar bases de datos ----

## Importar datos de resultados electorales del PRI en elecciones de diputados federales

m1.pred <- read.table("01_datos/cap_04/Predicted M 1 (pripfshr) 91-00 033013.txt", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
m3.pred <- read.table("01_datos/cap_04/Predicted M 3 (pripfmarg) 91-00 033013.txt", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Analizar estructura
dim(m1.pred)
dim(m3.pred)
str(m1.pred)
str(m3.pred)

# Nombres y resúmen estadístico de data frames
names(m1.pred)
names(m3.pred)
summary(m1.pred)
summary(m3.pred)


### Gráficas de probabilidad predicha con modelo 1 de la tabla 2 del capítulo 4 ----

# En este mdoelo la principal variable explicativa es el porcentaje de financiamiento público recibido anualmente por el PRI

mod1 <- m1.pred %>% 
  ggplot() +
  geom_line(aes(pripfshr, predicted), size = 1.1) +
  geom_line(aes(pripfshr, upper), col = "grey", linetype = 2, size = 1.5) +
  geom_line(aes(pripfshr, lower), col = "grey", linetype = 2, size = 1.5) +
  annotate("text", label = "Probabilidad predicha", x = 50, y = .9, size = 5, colour = "#666666") +
  geom_segment(aes(x = 42.4, y = .895, xend = 44.4, yend = .895), colour = "#000000", alpha = 0.9, size = 1.5) +
  annotate("text", label = "Int. de confianza 95%", x = 49.9, y = .82, size = 5, colour = "#666666") +
  geom_segment(aes(x = 42.4, y = .815, xend = 44.4, yend = .815), colour = "grey", alpha = 0.9, size = 1.5, linetype = 2) +
  labs(title = "Modelo 1", 
       x = "Porcentaje del financiamiento público recibido por el PRI",
       y = "Probabilidad de deserción") +
  coord_cartesian(xlim = c(25, 55)) +
  scale_x_continuous(breaks = seq(25, 55, 5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), axis.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(margin = margin(10,0,0,0)),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        axis.text = element_text(size = 12))



pred_m5 <- data_frame(momento = c("Antes de\nla reforma", "Después de\nla reforma"),
                      valores = c(0.05, 0.62))

mod5 <- pred_m5 %>% 
  ggplot()+
  geom_col(aes(momento, valores), fill = "grey") +
  geom_text(aes(x=momento,y=valores,label=(valores)), fontface="bold",  size=5, colour = "#000000", position = position_dodge(width = 0.8), vjust=-0.4) +
  labs(title = "Modelo 5",
       x = "",
       y = "") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        axis.text = element_text(size = 12))

png("04_graficas/fig_4.2.png", width = 1080, units = "px")
grid.arrange(mod1, mod5, ncol=2)
dev.off()  

##################################################################################
####   
####   Chunk 4: Predicted probabilities using Model 1, Table 2
####            In this model PRI's MARGIN of Public Funds is the main
####            explanatory variable
####   
##################################################################################

par(mfrow=c(1,1))
par(mar=c(4.5, 5.1, 3.1, 2.1))

plot(m3.pred$pripfmarg, m3.pred$predicted, type ="l", lty=1, lwd=2, col="black", axes=F,  xlim=c(0,35), ylim=c(0,1), xlab="PRI's Margin of Public Funds", ylab="Predicted Probability", cex.lab=1.5)

axis(1, at=seq(0,35,5), las=1, cex.axis=1.5)
axis(2, at=seq(0,1,.2), las=2, cex.axis=1.5)

par(new=T)

plot(m3.pred$pripfmarg, m3.pred$upper, type ="l", lty=2, lwd=2, col="red", axes=F,  xlim=c(0,35), ylim=c(0,1), xlab="", ylab="", cex.lab=1.5)

par(new=T)

plot(m3.pred$pripfmarg, m3.pred$lower, type ="l", lty=2, lwd=2, col="red", axes=F,  xlim=c(0,35), ylim=c(0,1), xlab="", ylab="", cex.lab=1.5)




##################################################################################
####   
####   Chunk 5: Putting the previous two graphs together
####   
##################################################################################


### Setting the figure's format

par(mfrow=c(1,2), mar=c(4,4,4,3), oma=c(2.5,0,0,0))

#### PRI's SHARE of public funds   

plot(m1.pred$pripfshr, m1.pred$predicted, type ="l", lty=1, lwd=2, col="black", axes=F,  xlim=c(25,55), ylim=c(0,1), main ="(a)", xlab="PRI's Share of Public Funds", ylab="Predicted Probability", cex.lab=1.5)

axis(1, at=seq(25,55,5), las=1, cex.axis=1.3)
axis(2, at=seq(0,1,.2), las=2, cex.axis=1.3)

par(new=T)

plot(m1.pred$pripfshr, m1.pred$upper, type ="l", lty=2, lwd=2, col="red", axes=F,  xlim=c(25,55), ylim=c(0,1), xlab="", ylab="", cex.lab=1.5)

par(new=T)

plot(m1.pred$pripfshr, m1.pred$lower, type ="l", lty=2, lwd=2, col="red", axes=F,  xlim=c(25,55), ylim=c(0,1), xlab="", ylab="", cex.lab=1.5)



#### PRI's MARGIN of public funds   

plot(m3.pred$pripfmarg, m3.pred$predicted, type ="l", lty=1, lwd=2, col="black", axes=F,  xlim=c(0,35), ylim=c(0,1), main ="(b)", xlab="PRI's Margin of Public Funds", ylab="Predicted Probability", cex.lab=1.5)

axis(1, at=seq(0,35,5), las=1, cex.axis=1.3)
axis(2, at=seq(0,1,.2), las=2, cex.axis=1.3)

par(new=T)

plot(m3.pred$pripfmarg, m3.pred$upper, type ="l", lty=2, lwd=2, col="red", axes=F,  xlim=c(0,35), ylim=c(0,1), xlab="", ylab="", cex.lab=1.5)

par(new=T)

plot(m3.pred$pripfmarg, m3.pred$lower, type ="l", lty=2, lwd=2, col="red", axes=F,  xlim=c(0,35), ylim=c(0,1), xlab="", ylab="", cex.lab=1.5)





### Frequency Line

par(mar=c(5,4,1,2)+0.1)

par(oma=c(3,0,0,0)) 

plot(defect$year, defect$left, type ="n", lwd=1.5, col="blue", axes=F, xlab="", ylab="% of Elections with Defections", main="",pch=20, cex=4, ylim=c(0,70))

points(defect$year[defect$year<=1996], defect$left[defect$year<=1996], type ="p",  col="grey46", xlab="", ylab="% of Elections with Defections", main="",pch=20, cex=3)

points(defect$year[defect$year>=1996], defect$left[defect$year>=1996], type ="p",  col="darkblue", xlab="", ylab="% of Elections with Defections", main="",pch=20, cex=3)

fit1  <- lm(defect$left[defect$year<=1996] ~ defect$year[defect$year<=1996])
abline(fit1, col="grey46", lwd=3)

fit2  <- lm(defect$left[defect$year>=1996] ~ defect$year[defect$year>=1996])
abline(fit2, col="darkblue", lwd=3)


axis(1, at=seq(1987,2006,1), las=2)
axis(2, at=seq(0,100,10))
abline(v=1996, lty=3, col="red")

text(1998.5,10,labels="1996 Electoral")
text(1998.5,6.8,labels="Reform")
arrows(1997.2,9, 1996.3, 9, lenght = 1, angle = 0)
par(new=T)

### Smoother

par(new=T)

lines(predict(smooth.spline(defect$year[defect$year<=1996], defect$left[defect$year<=1996], df=3), x=seq(1987,2006,1)), lwd=3, type="l", lty=2, col="grey46")

lines(predict(smooth.spline(defect$year[defect$year>=1996], defect$left[defect$year>=1996], df=3), x=seq(1987,2006,1)), lwd=3, type="l", lty=2, col="darkblue")



######
### Labels for number of elections per year
######

mtext("  No. of", side=1, line=3.2, adj=-0.08, cex=1)
mtext("elections", side=1, line=4.2, adj=-0.08, cex=1)


mtext("6", side=1, line=3.7, adj=0.035, cex=1)
mtext("5", side=1, line=3.7, adj=0.035, cex=1)
mtext("1", side=1, line=3.7, adj=0.035, cex=1)
mtext("0", side=1, line=3.7, adj=0.087, cex=1)
mtext("7", side=1, line=3.7, adj=0.142, cex=1)
mtext("11", side=1, line=3.7, adj=0.195, cex=1)
mtext("9", side=1, line=3.7, adj=0.253, cex=1)
mtext("3", side=1, line=3.7, adj=0.308, cex=1)
mtext("5", side=1, line=3.7, adj=0.363, cex=1)
mtext("0", side=1, line=3.7, adj=0.419, cex=1)
mtext("7", side=1, line=3.7, adj=0.475, cex=1)
mtext("10", side=1, line=3.7, adj=0.528, cex=1)
mtext("7", side=1, line=3.7, adj=0.583, cex=1)
mtext("6", side=1, line=3.7, adj=0.638, cex=1)
mtext("4", side=1, line=3.7, adj=0.695, cex=1)
mtext("0", side=1, line=3.7, adj=0.748, cex=1)
mtext("7", side=1, line=3.7, adj=0.803, cex=1)
mtext("10", side=1, line=3.7, adj=0.861, cex=1)
mtext("8", side=1, line=3.7, adj=0.914, cex=1)
mtext("6", side=1, line=3.7, adj=0.97, cex=1)

mtext("Year", side=1, line=5.5, adj=0.5, cex=1)

######
### Legend
######

legend(2003,10, c("Percentage","Smoother"), cex=1.1, col=c("black", "black"), lty=c(1,2), bty="n")


######
### Source
######

source  <- "Source: Own elaboration"
note  <- "Note:"
footnote <- paste(source, sep=" \ ")

makeFootnote <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(0.01))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(1,"mm"),
             y= unit(2, "mm"),
             just=c("left", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}

makeFootnote(footnote)



#################################################################################
####   
####   Chunk 5.a: Figure 5. Defections of PRI's governor contenders, 1987-2006 
####   
#################################################################################


### Frequency Line

par(mar=c(5,4,1,2)+0.1)

par(oma=c(4,1,0,0))

plot(defect1$year, defect1$left1, type ="l", lwd=1.5, col="black", axes=F, xlab="", ylab="% of Elections with Defections", main="", cex.lab=1.5)

axis(1, at=seq(1987,2006,1), las=2, cex.axis=1.5)
axis(2, at=seq(0,70,10), cex.axis=1.5)
abline(v=1996, lty=3, col="red",lwd=2)

text(1999,10,labels="1996 Electoral", cex=1.5)
text(1999,7,labels="Reform", cex=1.5)
arrows(1997.2,9, 1996.3, 9, lenght = 1, angle = 0)
par(new=T)

### Smoother

#plot(defect1$year, defect1$left, type ="n", lwd=1.5, col="black", axes=F, xlab="", ylab="")

#par(new=T)

# Whole period
#lines(predict(smooth.spline(defect1$year, defect1$left, df=8), x=seq(1988,2006,1)), lwd=1.5, type="l", lty=2)

# 1987-1996
#lines(predict(smooth.spline(defect1$year[defect1$year>=1987&defect1$year<1996], defect1$left[defect1$year>=1987&defect1$year<1996], df=8), x=seq(1988,2006,1)), lwd=1.5, type="l", lty=2)

#lines(predict(smooth.spline(defect1$year[defect1$year>=1996], defect1$left[defect1$year>=1996], df=8), x=seq(1988,2006,1)), lwd=1.5, type="l", lty=2)



######
### Labels for number of elections per year
######

mtext("  No. of", side=1, line=4.5, adj=-0.07, cex=1.5)
mtext("elections", side=1, line=5.7, adj=-0.07, cex=1.5)

mtext("6", side=1, line=4.7, adj=0.033, cex=1.5)
mtext("4", side=1, line=4.7, adj=0.083, cex=1.5)
mtext("1", side=1, line=4.7, adj=0.132, cex=1.5)
mtext("0", side=1, line=4.7, adj=0.182, cex=1.5)
mtext("7", side=1, line=4.7, adj=0.231, cex=1.5)
mtext("11", side=1, line=4.7, adj=0.276, cex=1.5)
mtext("9", side=1, line=4.7, adj=0.330, cex=1.5)
mtext("3", side=1, line=4.7, adj=0.380, cex=1.5)
mtext("5", side=1, line=4.7, adj=0.428, cex=1.5)
mtext("0", side=1, line=4.7, adj=0.477, cex=1.5)
mtext("7", side=1, line=4.7, adj=0.526, cex=1.5)
mtext("10", side=1, line=4.7, adj=0.574, cex=1.5)
mtext("7", side=1, line=4.7, adj=0.625, cex=1.5)
mtext("6", side=1, line=4.7, adj=0.675, cex=1.5)
mtext("4", side=1, line=4.7, adj=0.724, cex=1.5)
mtext("0", side=1, line=4.7, adj=0.773, cex=1.5)
mtext("7", side=1, line=4.7, adj=0.821, cex=1.5)
mtext("10", side=1, line=4.7, adj=0.873, cex=1.5)
mtext("8", side=1, line=4.7, adj=0.919, cex=1.5)
mtext("6", side=1, line=4.7, adj=0.969, cex=1.5)

mtext("Year", side=1, line=7.5, adj=0.5, cex=1.5)

######
### Legend
######

#legend(2003,10, c("Percentage","Smoother"), cex=1.1, col=c("black", "black"), lty=c(1,2), bty="n")


######
### Source
######

source  <- "Source: Own elaboration"
note  <- "Note:"
footnote <- paste(source, sep=" \ ")

makeFootnote <- function(footnoteText=
  format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(0.01))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"mm"),
            y= unit(2, "mm"),
            just=c("left", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

makeFootnote(footnote)







#################################################################################
####   
####   Chunk 5.b: Figure 5. Defections of PRI's governor contenders, 1987-2006 
####   
#################################################################################
library(ggplot2)
library(reshape2)

qplot(x=factor(Year),y = value, data=defect2, geom="bar", fill=variable)

defect

ggplot(defect2, aes(x=year, y=variable, fill=variable)) + 
  geom_bar(stat="identity") 

x <- data.frame(
  Period = c(1,1,2,2,3,3,4,4),
  Sample = c("A","B","A","B","A","B","A","B"),
  Value1 = c(3,2,6,7,3,2,1,2),
  Value2 = c(1,0,5,2,2,0,2,5)
)
mx <- melt(x, id.vars=1:2)




### Frequency Line

par(mar=c(5,4,1,2)+0.1)

par(oma=c(4,1,0,0))

barplot(defect3, main="Car Distribution by Gears and VS", xlab="Years", col=c("darkblue","red"),legend = rownames(defect2))



plot(defect1$year, defect1$left1, type ="l", lwd=1.5, col="black", axes=F, xlab="", ylab="% of Elections with Defections", main="", cex.lab=1.5)

axis(1, at=seq(1987,2006,1), las=2, cex.axis=1.5)
axis(2, at=seq(0,70,10), cex.axis=1.5)
abline(v=1996, lty=3, col="red",lwd=2)

text(1999,10,labels="1996 Electoral", cex=1.5)
text(1999,7,labels="Reform", cex=1.5)
arrows(1997.2,9, 1996.3, 9, lenght = 1, angle = 0)
par(new=T)

### Smoother

#plot(defect1$year, defect1$left, type ="n", lwd=1.5, col="black", axes=F, xlab="", ylab="")

#par(new=T)

# Whole period
#lines(predict(smooth.spline(defect1$year, defect1$left, df=8), x=seq(1988,2006,1)), lwd=1.5, type="l", lty=2)

# 1987-1996
#lines(predict(smooth.spline(defect1$year[defect1$year>=1987&defect1$year<1996], defect1$left[defect1$year>=1987&defect1$year<1996], df=8), x=seq(1988,2006,1)), lwd=1.5, type="l", lty=2)

#lines(predict(smooth.spline(defect1$year[defect1$year>=1996], defect1$left[defect1$year>=1996], df=8), x=seq(1988,2006,1)), lwd=1.5, type="l", lty=2)





mtext("Year", side=1, line=7.5, adj=0.5, cex=1.5)

######
### Legend
######

#legend(2003,10, c("Percentage","Smoother"), cex=1.1, col=c("black", "black"), lty=c(1,2), bty="n")


######
### Source
######

source  <- "Source: Own elaboration"
note  <- "Note:"
footnote <- paste(source, sep=" \ ")

makeFootnote <- function(footnoteText=
  format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(0.01))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"mm"),
            y= unit(2, "mm"),
            just=c("left", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

makeFootnote(footnote)










##################################################################################
####   
####   Chunk 6: Figure X. Number of SOE's, 1985-2006 
####   
##################################################################################

plot(soes$year, soes$soe, type ="l", lty=1, lwd=1.5, col="red", axes=F, xlab="Year", ylab="Number of SOEs")
axis(1, at=seq(1955,2006,5), las=2)
axis(2, at=seq(0,1300,200))


barplot(soes$soe,border="black", axes=F,col="black")
par(new=T)
barplot(soesz$soe, axes=F, main= "Number of State Owned Enterprises in Mexico, 1955-2006", xlab="Year", ylab="Number of SOEs", border="black", col="grey",names.arg=c("1955","1970","1975","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006"),las=2)

axis(2, at=seq(0,1400,200), las=2)


#########
##Source
########

source  <- "Source: Own elaboration with data from Chong and López de Silanes 2005: 351; Lustig 1992: 104-105; MacLeod 2004; Salinas 1990, 1994; Zedillo 1995, 1997."
note  <- "Note:"
footnote <- paste(source, sep=" \ ")

makeFootnote <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(0.01))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(1,"mm"),
             y= unit(2, "mm"),
             just=c("left", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}

makeFootnote(footnote)