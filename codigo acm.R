library(FactoMineR)
library(ggplot2)
library(FactoClass)
library(factoextra)
library(Rcpp)
library(broom)
library(pander)
library(corrplot)
library(gridExtra) 


data(admi)
DatosInicial <- subset(admi, select = c("carr", "gene", "estr", "orig","age") )
Carrera <- DatosInicial$carr
Sexo <- DatosInicial$gene
Estrato <- DatosInicial$estr
Origen <- DatosInicial$orig
Edad <- as.factor(DatosInicial$age)

Datos <- cbind(DatosInicial,Carrera,Sexo,Estrato,Origen) 
Datos
Datos[,1:5] <- NULL 

Datos
summary(Datos) 
str(Datos)

F1<-ggplot(Datos, aes(x=Carrera)) + geom_bar(fill= "#DDB4EB")
F2<-ggplot(Datos, aes(x=Sexo)) + geom_bar(fill= "#FFD4A5")
F3<-ggplot(Datos, aes(x=Estrato)) + geom_bar(fill= "#41894A")
F4<-ggplot(Datos, aes(x=Origen)) + geom_bar(fill= "#FFEC28")
F5 <- grid.arrange(F1,F2,F3,F4, nrow = 2) 


uni.mca <- MCA(Datos, graph = FALSE)
print(uni.mca) 


eigenval <- get_eigenvalue(uni.mca)
pander(head(eigenval)) 



fviz_screeplot(uni.mca, addlabels = TRUE, ylim = c(0, 15)) + geom_hline(yintercept = 7.14, linetype = 2, color = "red")
 

fviz_mca_biplot(uni.mca, repel = TRUE, 
                ggtheme = theme_grey())+labs(
                  title ="           Representación simultanea de los individuos y las categorías") 


var <- get_mca_var(uni.mca)
var 



fviz_mca_var(uni.mca, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_grey()) 


pander(head(round(var$coord, 2), 15)) 



fviz_mca_var(uni.mca, col.var = "purple", shape.var = 10, repel = TRUE,
             ggtheme = theme_grey())+labs(title = "                     Nube de puntos de las Modalidades/Categorías") 

pander(head(var$cos2, 15)) 


 
fviz_cos2(uni.mca, choice = "var", axes = 1:2)+labs(title = "                        Cos2 de Categorías para las Dimensiones 1-2") 


corrplot(var$cos2, is.corr = FALSE) 


pander(head(round(var$contrib,2), 15)) 


fviz_contrib(uni.mca, choice = "var", axes = 1, top = 15)+labs(title = "                        Contribución de las Categorías para las Dimensión 1") 

fviz_contrib(uni.mca, choice = "var", axes = 2, top = 15)+labs(title = "                        Contribución de las Categorías para las Dimensión 2") 

 
fviz_contrib(uni.mca, choice = "var", axes = 1:2, top = 15)+labs(title = "                Contribuciones de las Categorías para las Dimensiónes 1-2") 

fviz_mca_var(uni.mca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800","#FC4E07"),
             ggtheme = theme_grey()
             , repel = TRUE) 

est <- get_mca_ind(uni.mca)
est 


pander(head(est$coord)) 


pander(head(est$contrib)) 

fviz_mca_ind(uni.mca, col.ind = "cos2",
             gradient.cols= c("blue", "white", "red"),
             repel = TRUE,
             ggtheme = theme_grey()) 

tail(est$contrib) 



fviz_cos2 (uni.mca, choice = "ind", axes = 1:2, top = 50)+labs(title = "                          Cos2 de los individuos para las Dimensiónes 1-2") 


fviz_mca_ind(uni.mca,
             label = "none",
             habillage = Sexo,
             pallette = c("#CCCCFF", "#F08080"),
             addEllipses = TRUE,
             ggtheme = theme_grey())


fviz_ellipses(uni.mca, 1:4, 
              geom = "point") 


uni.desc <- dimdesc(uni.mca, axes = c(1,2))

#Prueba de hipotesis:

#H0: La variable o clasificacion no es caracteristica en la dimension
#H1: La variable o clasificacion es caracteristica en la dimensión


#Descripcion de la primera dimensión

uni.desc[[1]] 



uni.desc[[2]] 

NuevosDatos<-cbind(Datos,Edad) 


sup.mca<- MCA(NuevosDatos,quali.sup = 5,ncp=2,graph = FALSE)
coor_cat<- sup.mca$quali.sup$coord
pander(coor_cat) 

coor_edad<-sup.mca$quali.sup$eta2
pander(coor_edad) 

fviz_mca_var(sup.mca,repel=T)+labs(
  title ="                     Nube de puntos de Categorias y Edad Suplementaria") 


