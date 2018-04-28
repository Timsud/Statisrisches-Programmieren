
#Aufgabe1
install.packages("mvtnorm")
library(mvtnorm)
sim_dat = rmvnorm(1000, mean= c(100,100), sigma= matrix(c(15^(2),0.7*15^(2),0.7*15^(2),15^(2)),ncol=2),  method = "chol")

#Aufgabe2
var1 <- sim_dat[,1]
var2 <- sim_dat[,2]
cor(var1, var2)

ts_sim <- rep(rmvnorm(n = 1000, mean = c(100, 100), sigma = matrix(c(15^(2),0.7*15^(2),0.7*15^(2),15^(2)),ncol=2), method = "chol"), 1000)
ts_sim

# werte standardisieren für die dnorm-funktion
ts_sim <- (ts_sim - mean(ts_sim)) / sd(ts_sim)

hist(ts_sim, freq = FALSE, main = "Histogram mit der Dichtefunktion", xlab = "Vektor1")

xx <- seq(min(ts_sim), max(ts_sim), length = 1000)
xx
yy <- dnorm(xx)
yy
lines(xx, yy, col = 4, lwd = 2)

#Aufgabe3
#A,B
plot(sim_dat,pch=20, xlab="vektor1", ylab="vektor2")
#C
tmp <- par()$usr
tmp
 abline(a=0, b=1)     ##lines(c(0,tmp[2]), c(0,tmp[4]))

#D
par(cex=2)
unter_a_zig = sim_dat[sim_dat[,1] < 80,]
bool = unter_a_zig[,1] > unter_a_zig[,2]
plot(sim_dat,pch=20, xlab="vektor1", ylab="vektor2", type="n")
 points(sim_dat[sim_dat[,1] < 80,][bool,], pch=17, col="black")
 points(sim_dat[sim_dat[,1] < 80,][!bool,], pch=17, col="blue")
 points(sim_dat[sim_dat[,1] >= 80,], pch=20, col="red")
abline(a=0, b=1)
#E
ant = length(sim_dat[sim_dat[,1] < 80,][!bool,][,1])/length(sim_dat[sim_dat[,1] < 80,][,1]) #0.8076923
text(x=90, y=140, labels = paste(" Anteil von oberen X < 80 ist ", ant,sep= "\n"))
 
#f
X <- sim_dat[sim_dat[,1] < 80,]
chull(X)
hpts <- chull(X)
hpts <- c(hpts, hpts[1])
lines(X[hpts, ])

#Aufgabe4
 rho = seq(-1, 1, 0.1)
 nur_unt_80 = NULL
 nur_unt_80 = as.list(nur_unt_80)
 ober_ger=NULL
 ober_ger=as.list(ober_ger)
 for(i in 1:length(rho)) {
       sim_dat = matrix(rmvnorm(1000, mean= c(100,100), sigma= matrix(c(15^(2),rho[i]*15^(2),rho[i]*15^(2),15^(2)), ncol=2), method="chol"), ncol=2)
       unter_a_zig = sim_dat[sim_dat[,1] < 80,]
       nur_unt_80[[i]] = unter_a_zig
       bool = unter_a_zig[,1] > unter_a_zig[,2]
       ober_ger[[i]] =unter_a_zig[!bool,]
  }
  ant = sapply(ober_ger,length)/sapply(nur_unt_80,length) 
  plot(rho, ant, type="l")
  # Als rho von -1 zu 0 geht fällt anteil auch, wiel die Punkte dann mehr gestreuet sind. und dass fällt bis rho 1 ist dann kriegen wir anteil wieder 1, weil alle Punkte auf eine Gerade liegen.


#Aufgabe5
install.packages("xlsx")
install.packages("rJava")
library(xlsx)
HDI = read.xlsx(file="exdaten.xls", sheetName="Table 1", header=TRUE, startRow = 3, encoding="UTF-8", endRow = 197, colNames=FALSE, check.names=FALSE)
bool = 1:194 %in% 1:3
HDI = HDI[!bool,]
bool1 = (names(HDI)=="NA")
HDI = cbind(HDI[,2], HDI[,!bool1])
HDI = HDI[,!(names(HDI)== "colNames")]
new_names =c("Country","HDI","Life expectancy at birth", "Expected years of schooling", "Mean years of schooling", "GNI", "GNI minus HDI rank", "HDI rank")
names(HDI) = new_names
wo_na = apply(HDI,2, is.na)
new_HDI= na.omit(HDI)
        
#Aufgabe6
#Plot 1 
plot(as.numeric(as.character(new_HDI[,2])),as.numeric(as.character(new_HDI[,"Expected years of schooling"])), xlab="HDI", ylab="Years of schooling",pch=17,col=f(nrow(HDI)))
title(main="Dependence between school years and HDI")
f =  colorRampPalette(c("black", "red", "blue", "yellow"))
legend("topleft", legend=c("Very High.dev","High.dev", "Med.Hum.Dev", "Low.Hum.Dev"), pch = 17, col =c("black", "red", "blue", "yellow"), cex=0.7)
# Es gibt eine lineare Abhängigkeit. Je höher Jahren desto höher HDI. 
dev.new()
#Plot 2
new_HDI[,"GNI"]
plot(as.numeric(as.character(new_HDI[,2])),as.numeric(as.character(new_HDI[,"GNI"])), xlab="HDI", ylab="GNI",pch=17,col=f(nrow(HDI)))
title(main="Dependence between GNI and HDI")
legend("topleft", legend=c("Very High.dev","High.dev", "Med.Hum.Dev", "Low.Hum.Dev"), pch = 17, col =c("black", "red", "blue", "yellow"), cex=0.7)
# Hier gibt es auch eine Abhängigkeit. Je höher GNI, desto höher HDI.
dev.new()
# Plot 3
plot(as.numeric(as.character(new_HDI[, "Life expectancy at birth"])),as.numeric(as.character(new_HDI[,"GNI"])), xlab="HDI", ylab="GNI",pch=17,col=f(nrow(HDI)))
title(main="Dependence between GNI and Life Expectancy")
legend("topleft", legend=c("Very High.dev","High.dev", "Med.Hum.Dev", "Low.Hum.Dev"), pch = 17, col =c("black", "red", "blue", "yellow"), cex=0.7)
#Hier sehen wir auch generell, dass je höher GNI desto höher HDI ist.


 

















































