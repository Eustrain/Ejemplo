()library(agridat)
datos<-carlson.germination
str(datos)
#grafica 1
boxplot(datos$germ~datos$gen,xlab="Genotipos",ylab="Germinacion",varwith=TRUE,cex.lab=1.25,cex.axis=0.75,main="",outcol="grey",boxcol="red",boxfill="orange",whiskcol="red",)
##grafica 2
library(ggplot2)
g<-ggplot(datos$germ,datos$gen,aes(x=season, y=o3))
g+geom_boxplot(fill="darkseagreen4")  
##Regresion
plot(reg1)
attach(datos)
reg1<-lm(germ~gen+nacl)
anova(reg1)
reg1
##regresion 2 + linea
modelo<-(lm(datos$germ~datos$nacl,data=datos))
par(bg="656")
plot(datos$germ~datos$nacl,xlab = "Concentraciones de Nacl", ylab = "Germinacion",col="white",col.axis="white",col.lab="white")
abline(modelo,col="black")
##Prediccion
a<-predict(modelo,0.3)
mean(a)
dev.off()
##Polinomial 
par(bg="white")
m1<-(datos$germ~datos$nacl)
plot(m1,xlab = "Concentraciones de Nacl", ylab = "Germinacion")
m2<-lm(datos$germ~poly(datos$nacl,2,raw=TRUE,data=datos)
lines(datos$nacl,predict(m2),col=2)
b<-predict(m2,0.3)
mean(b)
#Xyplot
xyplot(datos$germ~datos$nac|factor(datos$gen),data=datos,xlab = "Concentraciones de Nacl", ylab ="Germinacion")

str(datos)
print(strtable(datos), na.print='')
nacl
re2<-lm(datos$germ~datos$gen)
plot(re2)
library(lattice)
library(swirl)
Sys.setlocale("LC_ALL", "en_US.UTF-8")
swirl()
eust
s<-HSD.test(anv,"Tratamiento",group = TRUE)
mmlimmli
library(agricolae)
v<-HSD.test(anv1,"A",1200697, 44470, group = TRUE)
x<-1:10
matrix(x,nrow=2,ncol=5)
matrix(x,2,5,byrow=T)
x<-1:4
matrix(x,3,8)

R.sci <- readMoments(names=c("read","write","math","science","T",))
1
0.5967765 1
0.6622801 0.6174493 1
0.6301579 0.5704416 0.6307332 1
R.sci <- readMoments(names=c("read","write","sciencie"))
1
0.5967765 1
0.6301579  0.6307332 1


mod.sci <- specifyModel(text ="
read -> math, gam31
write -> math, gam32
read -> science, gam41
write -> science, gam42
math -> science, beta43
math <-> math, phi33
science <-> science, phi44
")

sem.out <- sem(mod.sci, R.sci, N=200, fixed.x=c("read","write"))

sem.dhp <- sem(model.dhp, R.DHP, 329,
               fixed.x=c("RParAsp", "RIQ", "RSES", "FSES", "FIQ", "FParAsp"))
