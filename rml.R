##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo Final         -----------##
##-- Nombre: Arias Navarrete Rubi
#2.1 Leer los archivos poblacion1.xlsx y poblacion2.xlsx, y analizar sus dimensiones.
dir <- "C:/Users/Rubi Arias/Desktop/Programa R/Regresion-Lineal-Multiple"
list.files()
library(readxl)
poblacion1 <- read_excel("poblacion1.xlsx",sheet = 1,col_names = TRUE,na = "")
poblacion2<-read_excel("poblacion2.xlsx",sheet=1,col_name =TRUE, na= "")
str(poblacion1)
str(poblacion2)
dim1 <- dim(poblacion1)
dim2 <- dim(poblacion2)

#2.2 Una los archivos leídos en un mismo objeto llamado poblacion
poblacion<-merge(x = poblacion1 ,y = poblacion2, by = "identificador", suffixes = c("","")) 
View(poblacion)
#2.3 Cree un código que identifique la clase de cada variable y genere diagramos
#de cajas para variables continuas y diagramas de barra para variables discretas
for(i in 2:ncol(poblacion))
{
  print(names(poblacion)[i])
  print(class(poblacion[,i]))
  if(is.numeric(poblacion[,i])==TRUE){
         boxplot(poblacion[,i])
    }else {
       barplot(table(poblacion[,i]))
    }
  }

#2.4 Cree un código que calcule automáticamente el mínimo, media,máximo,desviación
#estándar, primer cuartil de cada variable numérica y la frecuencua en el caso de 
#variables categóricas
for(i in 2:ncol(poblacion)){
    if(is.numeric(poblacion[,i])==TRUE){
      print(names(poblacion)[i])
    print(min(poblacion[,i],na.rm=TRUE))
    print(mean(poblacion[,i],na.rm=TRUE))
          print(max(poblacion[,i],na.rm=TRUE))
          print(sd(poblacion[,i],na.rm=TRUE))
          print(quantile(poblacion[,i], probd=seq(0,1,0.25, na.rm=FALSE)))
    }else{
      print(names(poblacion)[i])
      print(table(poblacion[,i])/dim(poblacion)[1])
}
}

#2.5 Calcule la correlación entre la variable dependiente poblacion y cada una de 
#las variables explicativas(numéricas)
for (i in 3:ncol(poblacion))
{
  if(is.numeric(poblacion[,i])==TRUE){
    print(names(poblacion)[i])
    print(cor(x = poblacion[,i], y= poblacion[,"poblacion"]))
  }
}

#2.6 Considere la variable categórica serv.bas.compl con una confiabilidad del 90%,
#¿Puede asumir que la media de la variable poblacion en el grupo serv.bas.compl:SI
#es distinta a la media del grupo serv.bas.compl:NO?

#Antes de realizar el análisis debo graficar el diagrama de cajas para observas
#en donde se encuentra las medias, pero antes de esto modificaremos nuestra data
servicios<-factor(poblacion[,"serv.bas.compl"],levels=c("SI","NO"),labels=c("si","no"))
region<-factor(poblacion[,"region"],levels=c("A","B"),labels=c("a","b"))
POBLACION<-data.frame(poblacion[,1:7],region,servicios)
plot(poblacion ~ servicios , data = POBLACION) 
#Luego realizamos la prueba de hipótesis, para ver si se puede asumir la media de la
#variable  poblacion en el grupo serv.bas.compl:SI es distinta a la media del grupo 
#serv.bas.compl:NO
t.test(poblacion ~ servicios , data = POBLACION, conf.level=0.9)

#2.7 Considerando los cálculos anteriores genere el modelo de regresión lineal 
#múltiple que mejor se ajuste a los datos. Interprete los coeficientes obtenidos.

regre<-lm(poblacion~var.pobl.mayor+menores.18+tasa.crimen,data=poblacion)
summary(regre)

#2.8 Interprete el R^2
summary(regremul)["r.squared"]
#100(0.1533)% es el porcentaje de la variablididad total explicada por la regresión

#2.9 Analice la significancia de la regresión y de cada uno de los parámetros 
#individuales
Anova<-aov(regremul)
summary(Anova)

#2.10 Realice un análisis detallado de los residuos

u_t<-numeric(nrow(poblacion)) 
for(i in 1:nrow(poblacion)){
  u_t[i]<-summary(regremul)[["residuals"]][i]
}
u_t
#Gráficas
#Poblacion vs Residuos
plot(poblacion[,"poblacion"],u_t)
#Histograma
hist(u_t)
#Grafi normal
qqnorm(u_t)
qqline(u_t)
