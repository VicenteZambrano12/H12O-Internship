# Código correspondiente al preprocesado de datos para realizar clustering:
# 1. Cargamos las librerías:

library(writexl)
library(factoextra)
library(haven)
library(tidyverse)
library(skimr)
library(gtsummary)
library(knitr)
library(factoextra)
library(recipes)
library(forcats)
library(dplyr)
library(readxl)
library(ggplot2)
library(corrr)
library(haven)
library(caret)
library(feather)
library(clustMixType)
library(cluster)
library(purrr)
library(ggplot2)
library(skimr)
library(compareGroups)
library(clustertend)
library(ggrepel)
library(fpc)
library(shapper)

#2. Cargamos la base de datos:

bbdd_inicial <-  read_excel("C:\\Users\\Vza 12\\Desktop\\Analisis_n_cluster\\UPM\\TFG\\Clustering_7\\Tablas\\Base Clustering.xls")

# 2.1. Selección de las variables del clustering:

df <- bbdd_inicial %>%
  dplyr::select(edad,sexo,vasos_afectados_imp,tipo_de_sca_imp,killip_imp,fevi,enfermedad_coronaria)

# 2.2. Guardamos las variables en una nueva tabla:

write_xlsx(df,"clustering7.xlsx")


# 3. Preprocesado:

# 3.1. Factorización de variables categóricas:

df$sexo <- as.factor(df$sexo)
df$vasos_afectados_imp <-  as.factor(df$vasos_afectados_imp)
df$tipo_de_sca_imp <- as.factor(df$tipo_de_sca_imp)

df$killip_imp <- as.factor(df$killip_imp)
df$enfermedad_coronaria <- as.factor(df$enfermedad_coronaria)

# 3.2. Renombramos las variables del dataframe:

df <- df %>%
 dplyr:: rename(
    Edad = edad,
    Sexo = sexo,
    "Tipo de SCA" = tipo_de_sca_imp,
    Killip = killip_imp,
    FEVI = fevi,
    "Enfermedad coronaria" = enfermedad_coronaria
  )

# 3.2.1. Creación del datframe numérico:

df_numerico <- df
df_numerico$Sexo <- as.numeric(df_numerico$Sexo)
df_numerico$vasos_afectados_imp <- as.numeric(df_numerico$vasos_afectados_imp)
df_numerico$`Tipo de SCA` <- as.numeric(df_numerico$`Tipo de SCA`)
df_numerico$Killip <- as.numeric(df_numerico$Killip)
df_numerico$`Enfermedad coronaria`<- as.numeric(df_numerico$`Enfermedad coronaria`)

# 3.3. Estudio de la distribución inicial de la muestra:

# 3.3.1. Gráfico de la distribución por sexo:

# 3.3.1.1. Dataframe para calcular la suma y el porcentaje por sexo:

df_sexo <- df %>%
  group_by( Sexo) %>%
  summarise(count = n()) %>%
  mutate(total_sexo = sum(count)) %>%
  mutate(porcentaje_sexo = count / total_sexo * 100)

# 3.3.1.2. Gráfico de sectores que muestra la distribución por sexo en porcentaje y número:
df_sexo$Sexo <- factor(df_sexo$Sexo, levels = c("Hombre", "Mujer"), labels = c("Male", "Female"))

graf_sexo <- ggplot (df_sexo, aes (x = "", y = porcentaje_sexo, fill = Sexo)) +
  geom_bar(stat="identity", width=1) +
  ggtitle("") +
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(porcentaje_sexo,2),"%")), 
            position = position_stack(vjust=0.5),
            fontface = "bold",
            size = 20,
            hjust =0.3,
            vjust = 0.3) +
  scale_fill_manual(values = c("Male" = "cyan3", "Female" = "firebrick2")) +
  labs(fill = "Sex") +  # Cambia el título de la leyenda aquí
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    text = element_text(size = 40)                  
                              )  # Ajusta el tamaño y la negrita del título
  

graf_sexo

# 3.3.2. Gráfico de la distribución de vasos afectados:

# 3.3.2.1. Dataframe con el porcentaje y el total por nivel:

df_n_vasos <- df %>%
  group_by(vasos_afectados_imp) %>%
  summarise(count = n()) %>%
  
  mutate(total_n_vasos = sum(count)) %>%
  mutate(porcentaje_n_vasos = count / total_n_vasos * 100)

# 3.3.2.2. Gráfico de sectores que muetsra el porcentaje y el total por nivel:
df_n_vasos$vasos_afectados_imp <- factor(df_n_vasos$vasos_afectados_imp, levels = c("1", "2","3 o más"), labels = c("1", "2", "3 or more"))



graf_n_vasos <- ggplot (df_n_vasos, aes (x = "", y = porcentaje_n_vasos, fill = vasos_afectados_imp)) +
  geom_bar(stat="identity", width=1) +
  ggtitle("") +
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(porcentaje_n_vasos,2), "%")), 
            position = position_stack(vjust=0.5),
            fontface = "bold",
            size = 15) +
  scale_fill_manual(values = c("1" = "cyan3", "2" = "firebrick2", "3 or more" = "greenyellow"),
                    name = "Number of\n vessels affected") +
  labs(fill = "Number of\nvessels affected") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    text = element_text(size = 40)
  )

graf_n_vasos

# 3.3.3. Gráfico de la distribución por tipo de SCA:

# 3.3.3.1. Dataframe para calcular la suma y el porcentaje por tipo de SCA:

df_tipo_sca <- df %>%
  group_by(`Tipo de SCA`) %>%
  summarise(count = n()) %>%
  mutate(total_tipo_sca = sum(count)) %>%
  mutate(porcentaje_tipo_sca = count / total_tipo_sca * 100)

df_tipo_sca$`Tipo de SCA` <- factor(df_tipo_sca$`Tipo de SCA`, levels = c("SCACEST", "SCASEST2"), labels = c("STEMI", "NSTEMI"))

# 3.3.3.2. Gráfico de sectores que muestra la distribución por tipo de SCA en porcentaje y número:

graf_tipo_sca <- ggplot (df_tipo_sca, aes (x = "", y = porcentaje_tipo_sca, fill = `Tipo de SCA`)) +
  geom_bar(stat="identity", width=1) +
  ggtitle("") +
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(porcentaje_tipo_sca,2),"%")), 
            position = position_stack(vjust=0.5),
            fontface = "bold",
            size = 20) +
  scale_fill_manual(values = c("STEMI" = "cyan3", "NSTEMI" = "firebrick2")) +
  labs(fill = "Kind of ACS") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                              text = element_text(size = 40)  # Ajusta el tamaño y la negrita del título
  )

graf_tipo_sca

# 3.3.4. Gráfico de la distribución por tipo de SCA:

# 3.3.4.1. Dataframe para calcular la suma y el porcentaje por clasificación Killip:

df_killip <- df %>%
  group_by(Killip) %>%
  summarise(count = n()) %>%
  mutate(total_killip = sum(count)) %>%
  mutate(porcentaje_killip = count / total_killip * 100)

# 3.3.4.2. Gráfico de barras que muestra la distribución por clasificación illip en porcentaje :

graf_killip <- ggplot(df_killip, aes(x = Killip, y = porcentaje_killip, fill = Killip)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("I" = "#268F9E", "II" = "#9E2631", "III" = "greenyellow", "IV" = "orange1"),
                    name = "Killip") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = round(porcentaje_killip, digits=2)),
            position = position_dodge(width = 0.7),
            vjust = -0.25,
            color = "black",
            size = 20,  # Tamaño de la fuente
            fontface = "bold") +  # Texto en negrita
  labs(title = "",
       y = "Percentage (%)",
       fill = "Adherencia") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = 50, face = "bold")) +  # Tamaño general de la fuente y negrita
  theme(axis.text.x = element_text(size = 80, hjust = 1))  # Tamaño y formato de las etiquetas del eje x

graf_killip


# 3.3.5. Gráfico de la distribución por enfermedad coronaria:

# 3.3.5.1. Dataframe para calcular la suma y el porcentaje por enfermedad coronaria:

df_ec <- df %>%
  group_by( `Enfermedad coronaria`) %>%
  summarise(count = n()) %>%
  mutate(total_ec = sum(count)) %>%
  mutate(porcentaje_ec = count / total_ec * 100)

# 3.3.5.2. Gráfico de sectores que muestra la distribución por enfermedad coronaria en porcentaje y número:

df_ec$`Enfermedad coronaria` <- factor(df_ec$`Enfermedad coronaria`, levels = c("Sí", "No"), labels = c("Yes", "No"))


graf_ec <- ggplot (df_ec, aes (x = "", y = porcentaje_ec, fill = `Enfermedad coronaria`)) +
  geom_bar(stat="identity", width=1) +
  ggtitle("") +
  coord_polar("y", start=0)+
  
  geom_text(aes(label = paste0(round(porcentaje_ec,2))), 
            position = position_stack(vjust=0.5),
            fontface = "bold",
            size = 15,
            hjust = 0.2,
            vjust = -0.8) +
  scale_fill_manual(values = c("Yes" = "cyan3", "No" = "firebrick2")) +
  labs(fill = "Coronary \nheart disease") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    text = element_text(size = 50, face = "bold"))

graf_ec
max
max(bbdd_inicial$fevi)
# 3.3.6. Gráfica distribución por edad:

# 3.3.6.1 Creación de un dataframe con la distribución por grupos de edad:

df_edad <- df 
cortes_edad <- c(20,30,40,50,60,70,80,100)
clase_edad <- c("20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79","80+")
df_edad$Edad<-cut(df_edad$Edad,breaks = cortes_edad,
                      right = FALSE,
                      labels = clase_edad)

df_edad <- df_edad %>%
  group_by( Edad) %>%
  summarise(count = n()) %>%
  mutate(total_edad = sum(count)) %>%
  mutate(porcentaje_edad = count / total_edad * 100) 
media_edad <- mean(df$Edad)
d_tipica_edad <- sd(df$Edad)

# 3.3.6.1 Creación de un gráfico de barras con la distribución por grupos de edad en porcentaje:

graf_edad <- ggplot(df_edad, aes(x = Edad, y = porcentaje_edad, fill = Edad)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("20 - 29" = "#268F9E", "30 - 39" = "#9E2631", "40 - 49" = "greenyellow", 
                               "50 - 59" = "orange1", "60 - 69" = "darkblue", "70 - 79" = "purple", "80+" = "darkgreen"),
                    name = "Age") +
  scale_y_continuous(limits = c(0, 45)) +
  geom_text(aes(label = round(porcentaje_edad, digits=2)),
            position = position_dodge(width = 0.7),
            vjust = -0.25,
            color = "black",
            size = 20,  # Tamaño de la fuente
            fontface = "bold") +
  labs(title = "",
       y = "Percentage (%)",
       x = "Age",
       fill = "Adherencia") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #theme_minimal() +
  theme(text = element_text(size=50))

graf_edad

# 3.3.7. Gráfica distribución por fevi:

# 3.3.7.1 Creación de un dataframe con la distribución por fevi:

df_fevi <- df 
cortes_fevi <- c(0,35,45,51,100)
clase_fevi <- c("Menos del 35%", "35% - 45%", "46% - 50%", "Superior al 50%")
df_fevi$FEVI<-cut(df_fevi$FEVI,breaks = cortes_fevi,
                  right = FALSE,
                  labels = clase_fevi)

df_fevi <- df_fevi %>%
  group_by( FEVI) %>%
  summarise(count = n()) %>%
  mutate(total_fevi = sum(count)) %>%
  mutate(porcentaje_fevi = count / total_fevi * 100)
media_fevi <- mean(df$FEVI)
d_tipica_fevi <-  sd(df$FEVI)

# 3.3.7.1 Creación de un gráfico de barras con la distribución por grupos de edad en porcentaje:

df_fevi$FEVI <- factor(df_fevi$FEVI, levels = c("Menos del 35%", "35% - 45%", "46% - 50%", "Superior al 50%"), labels = c("Less than 35%",  "35% - 45%", "46% - 50%", "More than 50%"))


graf_fevi <- ggplot(df_fevi, aes(x = FEVI, y = porcentaje_fevi, fill = FEVI)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Less than 35%" = "#268F9E", "35% - 45%" = "#9E2631", "46% - 50%" = "greenyellow", 
                               "More than 50%" = "orange1"),
                    name = "LVEF") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = round(porcentaje_fevi, digits=2)),
            position = position_dodge(width = 0.7),
            vjust = -0.25,
            color = "black",
            size = 20,  # Tamaño de la fuente
            fontface = "bold") +
  labs(title = "",
       y = "Percentage (%)",
       fill = "LVEF",
       x = "LVEF") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #theme_minimal() +
  theme(text = element_text(size=40))

graf_fevi

# 3.3.8. Tabla con el análisis previo de las variables:

analisis_previo <- df %>% 
  skim() %>%
  kable()
print(analisis_previo)
summary(analisis_previo)
# Estudio de la tendencia de clustering:

# 3.3.9.1. Test de Hopkings:

set.seed(123)
hopkins(df_numerico, n = nrow(df)-1)

# 3.3.9.2. Gráfica VAT:

grafico_VAT <- fviz_dist(dist(df_numerico), show_labels = FALSE)+
  ggtitle("Gráfica VAT") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

grafico_VAT

# Cabe resaltar que no hay missing values ni outliers

# 3.4. Estudio de las correlaciones:

# 3.4.1. Creación de la matriz de correlacion:

# Creamos la matriz de correlacion
matriz_cor <- df_numerico %>% 
  dplyr::select(where(is.numeric)) %>% 
  scale() %>%
  correlate()

# Pasamos a formato long para relacionar uno a uno
df_cor <- matriz_cor %>% 
  pivot_longer(cols = -term)

df_in <-  df
# No existen correlaciones superiores al 45%

# 3.5. Normalización de la muestra:

# 3.5.1. Normalización dataframe categórico:

receta <- recipe(~., data = df) %>% 
  step_normalize(where(is.numeric), -all_outcomes()) #Escala las variables numericas (media 0 y desviacion estandar 1)

receta_preparada <- prep(receta) #Se calculan las estadisticas necesarias para posteriormente aplicar las mismas transformaciones a los datos de test
df_cluster <- juice(receta_preparada) 

#3.5.1. Normalización dataframe numérico:

receta_num <- recipe(~., data = df_numerico) %>% 
  step_normalize(where(is.numeric), -all_outcomes()) #Escala las variables numericas (media 0 y desviacion estandar 1)

receta_preparada_num <- prep(receta_num) #Se calculan las estadisticas necesarias para posteriormente aplicar las mismas transformaciones a los datos de test
df_cluster_num <- juice(receta_preparada_num) 

# 4. Análisis del número de clusters óptimo:

# Se utilizan los paquetes clustMixType, cluster y purrr
set.seed(5704)

# 4.1. Dataframe categórico:

# 4.1.1. Gráfico del codo:

Es <- numeric(10)
for(i in 1:10){
  kpres <- kproto(df_cluster, k = i)
  Es[i] <- kpres$tot.withinss
}

codo <- plot(1:10, Es, type = "b", ylab = "Total Within Sum Of Squares", xlab = "Number of clusters")


# 4.1.2. Gráfico de la silueta:

Essil <- numeric(10)
for(i in 2:10){
  kpres <- kproto(df_cluster, k = i)
  Essil[i] <- validation_kproto(method="silhouette", object=kpres)
}

silueta <- plot(1:10, Essil, type = "b", ylab = "Silhouette", xlab = "Number of clusters")

# Número de clusters entre 4 y 6

# 4.2. Dataframe numérico:

# 4.2.1. Gráfico del codo:

codo_num <-  fviz_nbclust(df_cluster_num, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

codo_num

# 4.2.2. Gráfico de la silueta:

 silueta_num <- fviz_nbclust(df_cluster_num , kmeans, method = "silhouette")+
   theme_classic()
 
silueta_num

# 4.2.3. Dendograma al aplicar hkmeans:

hkmeans_model <-hkmeans(df_numerico, 3)

dend_hkmeans <- fviz_dend(hkmeans_model, cex = 0.6, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)  +

dend_hkmeans

# 5. Aplicación del algoritmo de clustering:

# 5.1. Algoritmo k-prototypes:

# 5.1.1. Algoritmo k-protoypes 3 clúsetrs:

set.seed(4567)

kproto_model_3 <- kproto(df_cluster, k=3)

kproto_clusters_3 <- factor(kproto_model_3$cluster, order =  TRUE,
                            levels = c(1:3))

kproto_3 <- data.frame(df, kproto_clusters_3) %>% na.omit()

result_df_kproto_3 <- kproto_model_3$centers

# 5.1.2. Algoritmo k-protoypes 4 clúsetrs:

set.seed(4567)

kproto_model_4 <- kproto(df_cluster, k=4)

kproto_clusters_4 <- factor(kproto_model_4$cluster, order =  TRUE,
                            levels = c(1:4))

kproto_4 <- data.frame(df_in, kproto_clusters_4) %>% na.omit()

result_df_kproto_4 <- kproto_model_4$centers

# 5.1.3. Algoritmo k-protoypes 5 clúsetrs:

set.seed(4567)

kproto_model_5 <- kproto(df_cluster, k=5)

kproto_clusters_5 <- factor(kproto_model_5$cluster, order =  TRUE,
                   levels = c(1:5))

kproto_5 <- data.frame(df_in, kproto_clusters_5) %>% na.omit()

result_df_kproto_5 <- kproto_model_5$centers

# 5.1.4. Algoritmo k-protoypes 6 clúsetrs:

set.seed(4567)

kproto_model_6 <- kproto(df_cluster, k=6)

kproto_clusters_6 <- factor(kproto_model_6$cluster, order =  TRUE,
                            levels = c(1:6))

kproto_6 <- data.frame(df_in, kproto_clusters_6)%>% na.omit()

result_df_kproto_6 <- kproto_model_6$centers

# 5.1.5. Extracción de los datos en varias tablas:

write_xlsx(kproto_3,"kproto_7_3.xlsx")
write_xlsx(kproto_4,"kproto_7_4.xlsx")
write_xlsx(kproto_5,"kproto_7_5.xlsx")
write_xlsx(kproto_6,"kproto_7_6.xlsx")

# 5.2. Algoritmo k-means:

# 5.2.1 Algoritmo k-means 3 clústeres:

set.seed(123)

kmeans_model_3 <- kmeans(df_cluster_num, 3, nstart = 25)

kmeans_cluster_3 <- factor(kmeans_model_3$cluster, order =  TRUE,
                            levels = c(1:3))

kmeans_3 <- data.frame(df_in, kmeans_cluster_3)%>% na.omit()

result_df_kmeans_3 <- kmeans_model_3$centers

# 5.2.2. Algoritmo k-means 4 clústeres:

set.seed(123)

kmeans_model_4 <- kmeans(df_cluster_num, 4, nstart = 25)

kmeans_cluster_4 <- factor(kmeans_model_4$cluster, order =  TRUE,
                           levels = c(1:4))

kmeans_4 <- data.frame(df_in, kmeans_cluster_4)%>% na.omit()

result_df_kmeans_4 <- kmeans_model_4$centers

# 5.2.3 Algoritmo k-means 5 clústeres:

set.seed(123)

kmeans_model_5 <- kmeans(df_cluster_num, 5, nstart = 25)

kmeans_cluster_5 <- factor(kmeans_model_5$cluster, order =  TRUE,
                           levels = c(1:5))

kmeans_5 <- data.frame(df_in, kmeans_cluster_5)%>% na.omit()

result_df_kmeans_5 <- kmeans_model_5$centers

# 5.2.4 Algoritmo k-means 6 clústeres:

set.seed(123)

kmeans_model_6 <- kmeans(df_cluster_num, 6, nstart = 25)

kmeans_cluster_6 <- factor(kmeans_model_6$cluster, order =  TRUE,
                           levels = c(1:6))

kmeans_6 <- data.frame(df_in, kmeans_cluster_6)%>% na.omit()

result_df_kmeans_6 <- kmeans_model_6$centers
# 5.2.5. Extracción de los datos en varias tablas:

write_xlsx(kmeans_3,"kmeans_7_3.xlsx")
write_xlsx(kmeans_4,"kmeans_7_4.xlsx")
write_xlsx(kmeans_5,"kmeans_7_5.xlsx")
write_xlsx(kmeans_6,"kmeans_7_6.xlsx")

# 5.3. Clustering jerárquico aglomerativo:

# 5.3.1. Extracción de las distancias:

dist_clust <- dist(df_numerico, method = "euclidean")

as.matrix(dist_clust)

jer_model <- hclust(d = dist_clust, method = "complete")

dendograma_jer_model <- fviz_dend(jer_model, cex = 0.5) + 
  ggtitle("") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

dendograma_jer_model

dist_coph <- cophenetic(jer_model)

cor(dist_clust, dist_coph)

# 5.3.4. Cortes del árbol:

# 5.3.4.1. 3 cortes:

jer_3 <- cutree(jer_model, k = 3)

jer_3_cluster <- factor(jer_3, order =  TRUE,
                           levels = c(1:3))

jerar_3 <- data.frame(df_in, jer_3_cluster)%>% na.omit()

# 5.3.4.2. 4 cortes:

jer_4 <- cutree(jer_model, k = 4)

jer_4_cluster <- factor(jer_4, order =  TRUE,
                        levels = c(1:4))

jerar_4 <- data.frame(df_in, jer_4_cluster)%>% na.omit()

# 5.3.4.3. 5 cortes:

jer_5 <- cutree(jer_model, k = 5)

jer_5_cluster <- factor(jer_5, order =  TRUE,
                        levels = c(1:5))

jerar_5 <- data.frame(df_in, jer_5_cluster)%>% na.omit()

# 5.3.4.4. 6 cortes:

jer_6 <- cutree(jer_model, k = 6)

jer_6_cluster <- factor(jer_6, order =  TRUE,
                        levels = c(1:6))

jerar_6 <- data.frame(df_in, jer_6_cluster)%>% na.omit()

# 5.3.5. Extracción de los datos en varias tablas:

write_xlsx(jerar_3,"jerar_3.xlsx")
write_xlsx(jerar_4,"jerar_4.xlsx")
write_xlsx(jerar_5,"jerar_5.xlsx")
write_xlsx(jerar_6,"jerar_6.xlsx")

# 6. Extracción de tablas descriptivas:

# 6.1. Tablas descriptivas k-prototypes:

# 6.1.1. Tabla descriptiva 3 clúster:

tabla_kproto_3 <- compareGroups(kproto_clusters_3 ~ ., data = kproto_3, method=NA)

tabla_kprototypes_3 <- createTable(tabla_kproto_3,
                          show.all = TRUE,
                          show.ratio = TRUE)

print(tabla_kprototypes_3)

export2xls(tabla_kprototypes_3, file = 'tabla_descriptiva_kproto_3.xlsx')

# 6.1.2. Tabla descriptiva 4 clúster:

tabla_kproto_4 <- compareGroups(kproto_clusters_4 ~ ., data = kproto_4, method=NA)

tabla_kprototypes_4 <- createTable(tabla_kproto_4,
                                   show.all = TRUE,
                                   show.ratio = TRUE)

print(tabla_kprototypes_4)

export2xls(tabla_kprototypes_4, file = 'tabla_descriptiva_kproto_4.xlsx')

# 6.1.3. Tabla descriptiva 5 clúster:

tabla_kproto_5 <- compareGroups(kproto_clusters_5 ~ ., data = kproto_5, method=NA)

tabla_kprototypes_5 <- createTable(tabla_kproto_5,
                                   show.all = TRUE,
                                   show.ratio = TRUE)

print(tabla_kprototypes_5)

export2xls(tabla_kprototypes_5, file = 'tabla_descriptiva_kproto_5.xlsx')

# 6.1.4. Tabla descriptiva 6 clúster:


# export2xls(tabla_kprototypes_6, file = 'tabla_descriptiva_kproto_6.xlsx')

# 6.2. Tablas descriptivas k-means:

# 6.2.1. Tabla descriptiva 3 clúster:

tabla_kmeans_3 <- compareGroups(kmeans_cluster_3 ~ ., data = kmeans_3, method=NA)

tabla_k_means_3 <- createTable(tabla_kmeans_3,
                                   show.all = TRUE,
                                   show.ratio = TRUE)

print(tabla_k_means_3)

export2xls(tabla_k_means_3, file = 'tabla_descriptiva_kmeans_3.xlsx')

# 6.2.1. Tabla descriptiva 4 clúster:

tabla_kmeans_4 <- compareGroups(kmeans_cluster_4 ~ ., data = kmeans_4, method=NA)

tabla_k_means_4 <- createTable(tabla_kmeans_4,
                               show.all = TRUE,
                               show.ratio = TRUE)

print(tabla_k_means_4)

export2xls(tabla_k_means_4, file = 'tabla_descriptiva_kmeans_4.xlsx')

# 6.2.1. Tabla descriptiva 5 clúster:

tabla_kmeans_5 <- compareGroups(kmeans_cluster_5 ~ ., data = kmeans_5, method=NA)

tabla_k_means_5 <- createTable(tabla_kmeans_5,
                               show.all = TRUE,
                               show.ratio = TRUE)

print(tabla_k_means_5)

export2xls(tabla_k_means_5, file = 'tabla_descriptiva_kmeans_5.xlsx')

# 6.2.4. Tabla descriptiva 6 clúster:

# tabla_kmeans_6 <- compareGroups(kmeans_cluster_6 ~ ., data = kmeans_6, method=NA)
# 
# tabla_k_means_6 <- createTable(tabla_kmeans_6,
#                                show.all = TRUE,
#                                show.ratio = TRUE)
# 
# print(tabla_k_means_6)
# 
# export2xls(tabla_k_means_6, file = 'tabla_descriptiva_kmeans_6.xlsx')

# 6.3. Tablas descriptivas clustering jerárquico:

# 6.3.1. Tabla descriptiva 3 clúster:

tabla_jer_3 <- compareGroups(jer_3_cluster ~ ., data = jerar_3, method=NA)

tabla_jerar_3 <- createTable(tabla_jer_3,
                                   show.all = TRUE,
                                   show.ratio = TRUE)

print(tabla_jerar_3)

export2xls(tabla_jerar_3, file = 'tabla_descriptiva_jerar_3.xlsx')

# 6.3.2. Tabla descriptiva 4 clúster:

tabla_jer_4 <- compareGroups(jer_4_cluster ~ ., data = jerar_4, method=NA)

tabla_jerar_4 <- createTable(tabla_jer_4,
                             show.all = TRUE,
                             show.ratio = TRUE)

print(tabla_jerar_4)

export2xls(tabla_jerar_4, file = 'tabla_descriptiva_jerar_4.xlsx')

# 6.3.3. Tabla descriptiva 5 clúster:

tabla_jer_5 <- compareGroups(jer_5_cluster ~ ., data = jerar_5, method=NA)

tabla_jerar_5 <- createTable(tabla_jer_5,
                             show.all = TRUE,
                             show.ratio = TRUE)


print(tabla_jerar_5)

export2xls(tabla_jerar_5, file = 'tabla_descriptiva_jerar_5.xlsx')

# 6.3.3. Tabla descriptiva 6 clúster:
meanws <- jerar_6 %>%
  filter(jer_6_cluster ==1)
media <- mean(meanws$Edad)

# Calcular el intervalo de confianza del 95%
t.test(meanws$FEVI)$conf.int

# 7. Visualización de resultados del clustering:

# 7.1. Gráfica dispersión PCA con centroides:

# 7.1.1. Gráficas k-prototypes:

# 7.1.1.1. Gráfica 3 clústers:

graf_pca_kproto_3 <-  fviz_cluster(list(data = df_numerico, cluster = kproto_model_3$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic()) 

graf_pca_kproto_3

# 7.1.1.2. Gráfica 4 clústers:

graf_pca_kproto_4 <-  fviz_cluster(list(data = df_numerico, cluster = kproto_model_4$cluster),
                                   ellipse.type = "norm", geom = "point", stand = FALSE,
                                   palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n k-prototypes 4 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_kproto_4

# 7.1.1.3. Gráfica 5 clústers:

graf_pca_kproto_5 <-  fviz_cluster(list(data = df_numerico, cluster = kproto_model_5$cluster),
                                   ellipse.type = "norm", geom = "point", stand = FALSE,
                                   palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n k-prototypes 5 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_kproto_5

# 7.1.1.4. Gráfica 6 clústers:

graf_pca_kproto_6 <-  fviz_cluster(list(data = df_numerico, cluster = kproto_model_6$cluster),
                                   ellipse.type = "norm", geom = "point", stand = FALSE,
                                   palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n k-prototypes 6 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_kproto_6

# 7.1.2. Gráficas k-means:

# 7.1.2.1. Gráfica 3 clústers:

graf_pca_kmeans_3 <-  fviz_cluster(list(data = df_numerico, cluster = kmeans_model_3$cluster),
                                   ellipse.type = "norm", geom = "point", stand = FALSE,
                                   palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n k-means 3 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_kmeans_3

# 7.1.2.2. Gráfica 4 clústers:

graf_pca_kmeans_4 <-  fviz_cluster(list(data = df_numerico, cluster = kmeans_model_4$cluster),
                                   ellipse.type = "norm", geom = "point", stand = FALSE,
                                   palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n k-means 4 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_kmeans_4

# 7.1.2.3. Gráfica 5 clústers:

graf_pca_kmeans_5 <-  fviz_cluster(list(data = df_numerico, cluster = kmeans_model_5$cluster),
                                   ellipse.type = "norm", geom = "point", stand = FALSE,
                                   palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n k-means 5 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_kmeans_5

# 7.1.2.4. Gráfica 6 clústers:

graf_pca_kmeans_6 <-  fviz_cluster(list(data = df_numerico, cluster = kmeans_model_6$cluster),
                                   ellipse.type = "norm", geom = "point", stand = FALSE,
                                   palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n k-means 6 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_kmeans_6

# 7.1.3. Gráficas clustering jerárquico:

# 7.1.3.1. Gráfica 3 grupos:

graf_pca_jerar_3 <-  fviz_cluster(list(data = df_numerico, cluster = jer_3_cluster),
                                   ellipse.type = "norm", geom = "point", stand = FALSE,
                                   palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n Jerárquico 3 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_jerar_3

# 7.1.3.2. Gráfica 4 grupos:

graf_pca_jerar_4 <-  fviz_cluster(list(data = df_numerico, cluster = jer_4_cluster),
                                  ellipse.type = "norm", geom = "point", stand = FALSE,
                                  palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n Jerárquico 4 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_jerar_4

# 7.1.3.3. Gráfica 5 grupos:

graf_pca_jerar_5 <-  fviz_cluster(list(data = df_numerico, cluster = jer_5_cluster),
                                  ellipse.type = "norm", geom = "point", stand = FALSE,
                                  palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n Jerárquico 5 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_jerar_5

# 7.1.3.4. Gráfica 6 grupos:

graf_pca_jerar_6 <-  fviz_cluster(list(data = df_numerico, cluster = jer_6_cluster),
                                  ellipse.type = "norm", geom = "point", stand = FALSE,
                                  palette = "jco", ggtheme = theme_classic()) +
  ggtitle("Gráfica de dispersión PCA \n Jerárquico 6 grupos") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

graf_pca_jerar_6

# 7.2. Dendogramas clustering jerárquico:

# 7.2.1.  Dendograma 3 grupos:

dendograma_3 <- fviz_dend(jer_model, k = 3,
          cex = 0.5, # label size
          k_colors = rainbow(3),
          color_labels_by_k = TRUE,
          rect = TRUE 
)

dendograma_3

# 7.2.1.  Dendograma 4 grupos:

dendograma_4 <- fviz_dend(jer_model, k = 4,
                          cex = 0.5, 
                          k_colors = rainbow(4),
                          color_labels_by_k = TRUE, 
                          rect = TRUE 
)+ggtitle("") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

dendograma_4

# 7.2.1.  Dendograma 5 grupos:

dendograma_5 <- fviz_dend(jer_model, k = 5,
                          cex = 0.5, 
                          k_colors = rainbow(5),
                          color_labels_by_k = TRUE, 
                          rect = TRUE 
) +ggtitle("") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

dendograma_5

# 7.2.1.  Dendograma 6 grupos:

dendograma_6 <- fviz_dend(jer_model, k = 6,
                          cex = 0.5, 
                          k_colors = rainbow(6),
                          color_labels_by_k = TRUE, 
                          rect = TRUE 
)+ggtitle("") +
  theme(plot.title =  element_text(hjust = 0.5, face = "bold"))

dendograma_6

# 8. Validación del cluster:

# 8.1. Listas con estadisticas:

# 8.1.1. Lista estadisticas k-prototypes:

kproto_stats_3 <- cluster.stats(dist(df_numerico), as.numeric(kproto_clusters_3))
kproto_stats_4 <- cluster.stats(dist(df_numerico), as.numeric(kproto_clusters_4))
kproto_stats_5 <- cluster.stats(dist(df_numerico), as.numeric(kproto_clusters_5))
kproto_stats_6 <- cluster.stats(dist(df_numerico), as.numeric(kproto_clusters_6))

# 8.1.2. Lista estadisticas k-means:

kmeans_stats_3 <- cluster.stats(dist(df_numerico), as.numeric(kmeans_cluster_3))
kmeans_stats_4 <- cluster.stats(dist(df_numerico), as.numeric(kmeans_cluster_4))
kmeans_stats_5 <- cluster.stats(dist(df_numerico), as.numeric(kmeans_cluster_5))
kmeans_stats_6 <- cluster.stats(dist(df_numerico), as.numeric(kmeans_cluster_6))

# 8.1.3. Lista estadisticas clustering jerárquico:

jerar_stats_3 <- cluster.stats(dist(df_numerico), as.numeric(jer_3_cluster))
jerar_stats_4 <- cluster.stats(dist(df_numerico), as.numeric(jer_4_cluster))
jerar_stats_5 <- cluster.stats(dist(df_numerico), as.numeric(jer_5_cluster))
jerar_stats_6 <- cluster.stats(dist(df_numerico), as.numeric(jer_6_cluster))

# 8.2. Extracción del indice de Dunn:

# 8.2.1. Guardamos todos los índice de Dunn en un dataframe

array_dunn <- c(kproto_stats_3$dunn,kproto_stats_4$dunn,kproto_stats_5$dunn,kproto_stats_6$dunn,
                kmeans_stats_3$dunn,kmeans_stats_4$dunn,kmeans_stats_5$dunn,kmeans_stats_6$dunn,
                jerar_stats_3$dunn,jerar_stats_4$dunn,jerar_stats_5$dunn,jerar_stats_6$dunn)

numero <- c(3,4,5,6,3,4,5,6,3,4,5,6)

algoritmo <- c("k-prototypes","k-prototypes","k-prototypes","k-prototypes",
           "k-means","k-means","k-means","k-means",
           "Jerárquico","Jerárquico","Jerárquico","Jerárquico")

df_dunn <- data.frame("Algoritmo" = algoritmo,"Clusters" = numero,"Indice de Dunn" = array_dunn)

# 8.2.2. Mapa de calor que representa los indices:

heatmap_dunn <- ggplot(df_dunn, aes(x = Algoritmo, y = Clusters, fill = Indice.de.Dunn)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)+
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#075AFF",
                       high = "#FF0000",
                       limits = c(0.01, 0.04)) +
  geom_text(aes(label = round(Indice.de.Dunn,5)), color = "white", size = 7,fontface = "bold") +
  coord_fixed()+
  labs(title = "Mapa de calor índice de Dunn",
       x = "Algoritmo",
       y = "Número de clusters") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))

heatmap_dunn

# 8.3. Silueta media total:

# 8.3.1. Creación de un dataframe con los valores:

array_silueta_media <- c(kproto_stats_3$avg.silwidth,kproto_stats_4$avg.silwidth,kproto_stats_5$avg.silwidth,kproto_stats_6$avg.silwidth,
                kmeans_stats_3$avg.silwidth,kmeans_stats_4$avg.silwidth,kmeans_stats_5$avg.silwidth,kmeans_stats_6$avg.silwidth,
                jerar_stats_3$avg.silwidth,jerar_stats_4$avg.silwidth,jerar_stats_5$avg.silwidth,jerar_stats_6$avg.silwidth)

df_silueta_media <- data.frame("Algoritmo" = algoritmo,"Clusters" = numero,"Silueta media" = array_silueta_media)

# 8.3.2. Gráfico de barras divergentes con la silueta media:

graf_silueta_media <- ggplot(df_silueta_media, aes(x = Clusters, y = Silueta.media, fill = Algoritmo)) +
  geom_bar(stat = "identity",
           position = "dodge",
           show.legend = TRUE) +
  geom_text(aes(label = round(Silueta.media,3)), position = position_dodge(width = 0.9),hjust = 1.4,color = "black", size = 5,fontface = "bold")+
  labs(title = "Gráfico de barras silueta media total",
       x = "Número de clusters",
       y = "Silueta media") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))+
    coord_flip()

graf_silueta_media
# 8.4. Grafico de la silueta media por cluster:

# 8.4.1. Obtención de los dataframe:

# 8.4.1.1. Dataframe k-prototypes:

array_silueta_kproto <- c(kproto_stats_3$clus.avg.silwidths,kproto_stats_4$clus.avg.silwidths,kproto_stats_5$clus.avg.silwidths,kproto_stats_6$clus.avg.silwidths)

id_cluster <- c(1,2,3,1,2,3,4,1,2,3,4,5,1,2,3,4,5,6)

id_alg <- c(3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6)

df_silueta_kproto <- data.frame("k" = id_alg,"Cluster"=id_cluster,"Silueta media" = array_silueta_kproto)

# 8.4.1.2. Dataframe k-means:

array_silueta_kmeans <- c(kmeans_stats_3$clus.avg.silwidths,kmeans_stats_4$clus.avg.silwidths,kmeans_stats_5$clus.avg.silwidths,kmeans_stats_6$clus.avg.silwidths)

df_silueta_kmeans <- data.frame("k" = id_alg,"Cluster"=id_cluster,"Silueta media" = array_silueta_kmeans)

# 8.4.1.2. Dataframe jerárquico:

array_silueta_jerar <- c(jerar_stats_3$clus.avg.silwidths,jerar_stats_4$clus.avg.silwidths,jerar_stats_5$clus.avg.silwidths,jerar_stats_6$clus.avg.silwidths)

df_silueta_jerar <- data.frame("k" = id_alg,"Cluster"=id_cluster,"Silueta media" = array_silueta_jerar)

# 8.4.2. Representación gráfica:

# 8.4.2.1. Representación kprototypes:

df_silueta_kproto <- df_silueta_kproto %>%
  arrange(desc(k)) %>%
  mutate(column_index = row_number())

graf_silueta_media_kproto <- ggplot(df_silueta_kproto, aes(x = as.factor(k), y = Silueta.media, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = round(Silueta.media, 3),
                vjust = ifelse(column_index == 4, 2.5, 
                               ifelse(Silueta.media > 0, -0.5, 1.5))),
            position = position_dodge(width = 0.9),
            hjust = 0.5,
            color = "black",  
            size = 7,
            fontface = "bold") +
  labs(title = "",
       x = "",
       y = "",
       fill = "Cluster") +
  ylim(-0.22, 0.34) +  # Establecer límites del eje y
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 30),
        text = element_text(size = 30))

print(graf_silueta_media_kproto)

# 8.4.2.2. Representación k-mens: :

graf_silueta_media_kmeans <- ggplot(df_silueta_kmeans, aes(x = as.factor(k), y = Silueta.media, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = round(Silueta.media, 3)),
            position = position_dodge(width = 0.9),
            vjust = if_else(df_silueta_kmeans$Silueta.media > 0, -0.5, 1.5),
            hjust = 0.5,
            color = "black",  
            size = 8,
            fontface = "bold") +
  labs(title = "",
       x = "",
       y = "",
       fill = "Clúster") +
  theme_minimal() +
  ylim(-0.15, 0.03)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 30),
        text = element_text(size = 30))


graf_silueta_media_kmeans

# 8.4.2.3. Representación jerárquico:

graf_silueta_media_jerar <- ggplot(df_silueta_jerar, aes(x = as.factor(k), y = Silueta.media, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = round(Silueta.media, 3)),
            position = position_dodge(width = 0.9),
            vjust = if_else(df_silueta_jerar$Silueta.media > 0, -0.5, 1.5),
            hjust = 0.5,
            color = "black",  
            size = 8,
            fontface = "bold") +
  labs(title = "",
       x = "",
       y = "",
       fill = "Clúster") +
  theme_minimal() +
  ylim(0, 0.57)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 30),
        text = element_text(size = 30))

graf_silueta_media_jerar

# 8.5. Extracción de gráficas que representan el tamaño de cada cluster:

# 8.5.1. Dataframe con el tamaño de cada cluster:

# 8.5.1.1. Dataframe k-prototypes:

array_tamaño_kproto <-c(kproto_model_3$size,kproto_model_4$size,kproto_model_5$size,kproto_model_6$size)

df_tamaño_kproto <- data.frame("k" = paste0("k = ",id_alg),"Cluster"=id_cluster,"Tamaño" = array_tamaño_kproto)

# 8.5.1.2. Dataframe k-means:

array_tamaño_kmeans <-c(kmeans_model_3$size,kmeans_model_4$size,kmeans_model_5$size,kmeans_model_6$size)

df_tamaño_kmeans <- data.frame("k" = paste0("k = ",id_alg),"Cluster"=id_cluster,"Tamaño" = array_tamaño_kmeans)

# 8.5.1.3. Dataframe jerárquico:

array_tamaño_jerar <-c(jerar_stats_3$cluster.size,jerar_stats_4$cluster.size,jerar_stats_5$cluster.size,jerar_stats_6$cluster.size)

df_tamaño_jerar <- data.frame("k" = paste0("k = ",id_alg),"Cluster"=id_cluster,"Tamaño" = array_tamaño_jerar)

# 8.5.2. Representación gráfica:

# 8.5.2.1. Gráfica k-prototypes:

 graf_size_kproto <- ggplot(df_tamaño_kproto, aes(x = "", y = Tamaño, fill = factor(Cluster))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = paste0(Cluster,"\n",Tamaño)), position = position_stack(vjust = 0.5),size = 7) +
  coord_polar(theta = "y") +
  facet_wrap(~k)  +
  theme_void() +
  labs(title = "",
       fill = "Cluster",
       y = "Tamaño del Cluster")  +
   theme(strip.text = element_text(face = "bold", color = "white", hjust = 0.5, size = 20),
         strip.background = element_rect(fill = "dodgerblue3", linetype = "solid",
                                         color = "black", linewidth = 1)) +
   theme(panel.border = element_rect(fill = "transparent", 
                                     color = "black", linewidth = 1.5)) +
   theme(plot.title = element_text(face = "bold", hjust = 0.5,size = 20))+
 theme(
   legend.title = element_text(size = 16, hjust = 0.5),
  legend.text = element_text(size = 12),
  legend.key.size = unit(2, "cm"))
 
 graf_size_kproto
 
 # 8.5.2.2. Gráfica k-means:
 
 graf_size_kmeans <- ggplot(df_tamaño_kmeans, aes(x = "", y = Tamaño, fill = factor(Cluster))) +
   geom_bar(stat = "identity", width = 1, color = "white") +
   geom_text(aes(label = paste0(Cluster,"\n",Tamaño)), position = position_stack(vjust = 0.5),size = 7) +
   coord_polar(theta = "y") +
   facet_wrap(~k)  +
   theme_void() +
   labs(title = "Tamaño de los clusters por k utilizando k-means",
        fill = "Cluster",
        y = "Tamaño del Cluster")  +
   theme(strip.text = element_text(face = "bold", color = "white", hjust = 0.5, size = 20),
         strip.background = element_rect(fill = "dodgerblue3", linetype = "solid",
                                         color = "black", linewidth = 1)) +
   theme(panel.border = element_rect(fill = "transparent", 
                                     color = "black", linewidth = 1.5)) +
   theme(plot.title = element_text(face = "bold", hjust = 0.5,size = 20))+
   theme(
     legend.title = element_text(size = 16, hjust = 0.5),
     legend.text = element_text(size = 12),
     legend.key.size = unit(2, "cm"))
 
 graf_size_kmeans
 
 # 8.5.2.2. Gráfica jerárquico:
 
 graf_size_jerar <- ggplot(df_tamaño_jerar, aes(x = "", y = Tamaño, fill = factor(Cluster))) +
   geom_bar(stat = "identity", width = 1, color = "white") +
   geom_text(aes(label = paste0(Cluster,"\n",Tamaño)), position = position_stack(vjust = 0.5),size = 7) +
   coord_polar(theta = "y") +
   facet_wrap(~k)  +
   theme_void() +
   labs(title = "Tamaño de los clusters por k utilizando clustering jerárquico",
        fill = "Cluster",
        y = "Tamaño del Cluster")  +
   theme(strip.text = element_text(face = "bold", color = "white", hjust = 0.5, size = 20),
         strip.background = element_rect(fill = "dodgerblue3", linetype = "solid",
                                         color = "black", linewidth = 1)) +
   theme(panel.border = element_rect(fill = "transparent", 
                                     color = "black", linewidth = 1.5)) +
   theme(plot.title = element_text(face = "bold", hjust = 0.5,size = 20))+
   theme(
     legend.title = element_text(size = 16, hjust = 0.5),
     legend.text = element_text(size = 12),
     legend.key.size = unit(2, "cm"))
 
 graf_size_jerar
 

 
 
 