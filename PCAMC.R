library(ggplot2)
library(reshape2)
library(factoextra)
library(corrplot)

head(datos)
colnames(datos) <- c("Embarazos", "Glucosa", "PresionArterial", "GrosorPiel", "Insulina", "IMC", "FuncionPedigriDiabetes", "Edad", "ResultadoDiabetes") 


### Pregunta #1
means <- colMeans(datos)
variances <- apply(datos, 2, var)
correlation_matrix <- cor(datos)
max_correlation <- max(correlation_matrix[upper.tri(correlation_matrix, diag = FALSE)])
indices_max_correlation <- which(correlation_matrix == max_correlation, arr.ind = TRUE)
variable1 <- colnames(correlation_matrix)[indices_max_correlation[1,1]]
variable2 <- colnames(correlation_matrix)[indices_max_correlation[1,2]]
tabla_frecuencias <- table(datos$ResultadoDiabetes)

### Resultados
print("Media de las variables:")
print(means)
print(max(means))
print("Varianza de las variables:")
print(variances)
print(min(variances))
print("Pareja de variables con la correlación más grande:")
print(paste(variable1, "y", variable2))
print("Correlación:")
print(max_correlation)

correlacion<-round(cor(datos), 3)
corrplot(correlacion,method = "number",type = "upper")


upper_correlation <- correlation_matrix
upper_correlation[lower.tri(correlation_matrix)] <- NA
correlation_df <- reshape2::melt(upper_correlation, na.rm = TRUE)

ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlación") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        plot.title = element_text(size = 14, hjust = 0.5)) +
  labs(x = "", y = "", title = "Matriz de Correlación")



### Pregunta #3

pca_result <- prcomp(datos, scale. = TRUE)
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2);variance_explained
summary(pca_result)

### Grafico 
componentes <- paste("PC", 1:length(variance_explained))
df <- data.frame(Componente = componentes, Varianza_Explicada = variance_explained)
ggplot(df, aes(x = Componente, y = Varianza_Explicada)) +
  geom_point(color = "red", size = 3) +  
  geom_line(color = "blue", size = 1, group = 1) +  
  geom_text(aes(label = paste0(round(Varianza_Explicada * 100, 1), "%")), vjust = -0.5, size = 3, color = "red") +  # Etiquetas con la proporción de varianza explicada
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red") + 
  labs(title = "Proporción de Varianza Explicada por Componente Principal",
       x = "Componente Principal",
       y = "Proporción de Varianza Explicada") +  
  theme_classic() +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  
        plot.title = element_text(hjust = 0.5)) 




### Pregunta #4
### Graficos 

fviz_eig(pca_result)

fviz_eig(pca_result,choice="eigenvalue")

fviz_pca_ind(pca_result,label="none",addEllipses=FALSE,
             ellipse.level=0.95,palette = "Dark2",
             ggtheme=theme_minimal())

fviz_pca_ind(pca_result,
             col.ind = "cos2",
             gradient.cols = c("#7FFFD4","#FF4040","#0000FF"),
             repel = FALSE)
fviz_pca_var(pca_result,
             col.var = "contrib",
             gradient.cols = c("#7FFFD4","#FF4040","#0000FF"),
             repel = TRUE)

fviz_pca_biplot(pca_result,
                col.var = "#0000FF",
                col.ind = "#030303")



### Pregunta #9

first_component_scores <- projected_data$PC1
personas <- data.frame(Persona = rownames(projected_data), PC1 = first_component_scores)
personas_ordenadas <- personas[order(personas$PC1, decreasing = TRUE), ]
top_5_personas <- head(personas_ordenadas, 5)
print(top_5_personas)

##############ACM ##################

library(FactoMineR)
library(ggplot2)
library(FactoClass)
library(factoextra)
library(Rcpp)
library(broom)
library(pander)
library(corrplot)
library(gridExtra)

datosC <- subset(datos, select =  c("Edad", "IMC", "PresionArterial", "ResultadoDiabetes"))


intervalos_edad <- c(0, 30, 50, Inf)
etiquetas_edad <- c("Joven", "Adulto", "Mayor")
intervalos_imc <- c(0, 18.5, 24.9, Inf)
etiquetas_imc <- c("Bajo Peso", "Normal", "Sobrepeso")
intervalos_PresionArterial <- c(0, 80, 120, Inf)
etiquetas_PresionArterial <- c("Baja", "Normal", "Alta")
intervalos_diabetes <- c(-Inf, 0.5, Inf)
etiquetas_diabetes <- c("No Diabetes", "Diabetes")

datosC$ResultadoDiabetes <- cut(datosC$ResultadoDiabetes, breaks = intervalos_diabetes, labels = etiquetas_diabetes)
datosC$Edad <- cut(datosC$Edad, breaks = intervalos_edad, labels = etiquetas_edad)
datosC$IMC <- cut(datosC$IMC, breaks = intervalos_imc, labels = etiquetas_imc)
datosC$PresionArterial <- cut(datosC$PresionArterial, breaks = intervalos_PresionArterial, labels = etiquetas_PresionArterial)
datosC <- na.omit(datosC)


### Grafico 
colores <- c("#DDB4EB", "#FFD4A5", "#41894A", "#FFEC28")

### Edad
F1 <- ggplot(datosC, aes(x = factor(Edad))) +
  geom_bar(fill = colores[1], color = "black") +
  labs(title = "Distribución por Edad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Resultado de Diabetes
F2 <- ggplot(datosC, aes(x = factor(ResultadoDiabetes))) +
  geom_bar(fill = colores[2], color = "black") +
  labs(title = "Resultado de Diabetes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Presión Arterial
F3 <- ggplot(datosC, aes(x = factor(PresionArterial))) +
  geom_bar(fill = colores[3], color = "black") +
  labs(title = "Presión Arterial") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### IMC
F4 <- ggplot(datosC, aes(x = factor(IMC))) +
  geom_bar(fill = colores[4], color = "black") +
  labs(title = "Índice de Masa Corporal (IMC)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combinar gráficos en una cuadrícula
F5 <- grid.arrange(F1, F2, F3, F4, nrow = 2);F5


###
uni.mca <- MCA(datosC, graph = FALSE)
print(uni.mca)

### Eigenvalues / Varianzas
eigenval <- get_eigenvalue(uni.mca)
pander(head(eigenval))

### SCREEP-PLOT
fviz_screeplot(uni.mca, addlabels = TRUE, ylim = c(0, 15)) + geom_hline(yintercept = 12.6, linetype = 2, color = "red")

### BIPLOT
fviz_mca_biplot(uni.mca, repel = TRUE, 
                select.ind = list(contrib = 100),
                ggtheme = theme_minimal(),
                alpha.var = 0.7,  
                col.ind = "pink",  
                col.var = "black") +  
  labs(title = "Representación simultanea de los individuos y las categorías")


#### Resultados de Variables
var <- get_mca_var(uni.mca)
var$cos2

fviz_mca_var(uni.mca, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal(),
             arrowsize = 1.5, 
             arrowcolor = "blue",
             labelsize = 3) 