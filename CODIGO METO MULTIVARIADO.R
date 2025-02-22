library(readxl) 
df <- read_excel("C:/Users/Usuario/Pictures/DATOSR/datos_ej1.xlsx")
df<- as.data.frame(df);df  


correlacion<-round(cor(df), 1)# calcula la matriz de correlación 
#install.packages("corrplot")
library(corrplot)
 
corrplot(correlacion, method="number", type="upper") #Grafica la matriz de correlación
 
datos_centrados <- scale(df) #scale: estandariza los datos
matriz_cor <- cor(datos_centrados) #matriz de correlación
matriz_cor
 
eigen <- eigen(matriz_cor)
eigen$values #valores propios 

eigen$vectors 

t_eigenvectors <- t(eigen$vectors)
t_eigenvectors 

t_datos_centrados <- t(datos_centrados)
t_datos_centrados 

# Producto matricial
pc_scores <- t_eigenvectors %*% t_datos_centrados
rownames(pc_scores) <- c("PC1", "PC2","PC3", "PC4","PC5")

# Se vuelve a transponer para que los datos estén en modo tabla
t(pc_scores) 



######## 
#install.packages("factoextra")
#ACP con factoextra
library(factoextra) 


pca <- prcomp(df, scale = TRUE) 

fviz_eig(pca) 

fviz_eig(pca, choice = "eigenvalue") 

fviz_pca_ind(pca, label = "none", 
             addEllipses = FALSE, 
             ellipse.level = 0.95,
             palette = "Dark2",
             ggtheme = theme_minimal())   

#loading plot
fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols = c("#7FFFD4","#FF4040","#0000FF"),
             repel = TRUE
) 

#Biplot
fviz_pca_biplot(pca,
                col.var = "#0000FF",
                col.ind = "#030303"
) 

fviz_pca_ind(pca,
             col.ind = "cos2",#Color by the quality of representation
             gradient.cols = c("#7FFFD4","#FF4040","#0000FF"),
             repel = FALSE  #avoid text overlapping
)
 
fviz_contrib(pca,axes=1,choice="var") 






######nj
