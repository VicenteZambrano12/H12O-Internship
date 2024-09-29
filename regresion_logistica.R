
# 1. Cargamos librerías:

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
library(survival)
library(caret)
library(feather)
library(clustMixType)
library(cluster)
library(purrr)
library(skimr)
library(compareGroups)
library(clustertend)
library(ggrepel)
library(fpc)
library(pROC)
library(viridis)
library(gridExtra)
library(nnet)
library(ROCR)
library(tidymodels)

#2. Cargamos las bases de datos:

bbdd_inicial <-  read_excel("C:\\Users\\Vza 12\\Desktop\\UPM\\TFG\\Clustering_7\\Tablas\\Base Clustering.xls")
df_in <- read_excel("C:\\Users\\Vza 12\\Desktop\\UPM\\TFG\\Clustering_7\\Tablas\\Resultados_clustering\\jerarquico\\jerar_4.xlsx")

# 2.1. Seleccionamos las variables de interés:

bbdd_inicial <- bbdd_inicial %>%
  dplyr::select(edad,sexo,vasos_afectados_imp,tipo_de_sca_imp,killip_imp,fevi,enfermedad_coronaria,
                mmas_adh_12m__01,medas_adherencia_12m_imp,ipaq_adhe_12m_imp,adh_total_12m_imp,adherencia_rhc)

df_in <- df_in %>%
  select(jer_4_cluster)

# 2.2. Las unimos en el mismo dataframe

df <- cbind(bbdd_inicial,df_in)

# 2.3. Factorización de variables:

df$sexo <- as.factor(df$sexo)
df$vasos_afectados_imp <-  as.factor(df$vasos_afectados_imp)
df$tipo_de_sca_imp <- as.factor(df$tipo_de_sca_imp)
df$killip_imp <- as.factor(df$killip_imp)
df$enfermedad_coronaria <- as.factor(df$enfermedad_coronaria)
df$mmas_adh_12m__01 <- as.factor(df$mmas_adh_12m__01)
df$medas_adherencia_12m_imp <- as.factor(df$medas_adherencia_12m_imp)
df$ipaq_adhe_12m_imp <- as.factor(df$ipaq_adhe_12m_imp)
df$adherencia_rhc <- as.factor(df$adherencia_rhc)
df$adh_total_12m_imp <- as.factor(df$adh_total_12m_imp)
df$jer_4_cluster <- as.factor(df$jer_4_cluster)
df$mmas_adh_12m__01_num <- if_else(df$mmas_adh_12m__01 == "Adherente",1,0)
df$medas_adherencia_12m_imp_num <- if_else(df$medas_adherencia_12m_imp == "Adherente",1,0)
df$ipaq_adhe_12m_imp_num <- if_else(df$ipaq_adhe_12m_imp == "Adherente",1,0)
df$adh_total_12m_imp_num <- if_else(df$adh_total_12m_imp == "Adherente",1,0)
df$adherencia_rhc_num <- case_when(
  df$adherencia_rhc == "No realiza" ~0,
  df$adherencia_rhc == "No completa" ~1,
  df$adherencia_rhc == "Adherente" ~2
)


# 2.3. Eliminación de NAs:

df <- na.omit(df)

# 3. Divisiones de los dataframe:

# 3.1. División por grupo:

df_1 <- df %>%
  filter(jer_4_cluster == 1)

df_2 <- df %>%
  filter(jer_4_cluster == 2)

df_3 <- df %>%
  filter(jer_4_cluster == 3)

df_4 <- df %>%
  filter(jer_4_cluster == 4)

# 3.2. División en conjuntos de prueba y entrenamiento:

set.seed(123)
num_filas <- nrow(df)
indices <- sample(1:num_filas, 0.7 * num_filas)
df_entrenamiento <- df[indices, ]
df_prueba <- df[-indices, ]

set.seed(123)
num_filas_1 <- nrow(df_1)
indices_1 <- sample(1:num_filas_1, 0.7 * num_filas_1)
df_entrenamiento_1 <- df_1[indices_1, ]
df_prueba_1 <- df_1[-indices_1, ]

set.seed(123)
num_filas_2 <- nrow(df_2)
indices_2 <- sample(1:num_filas_2, 0.7 * num_filas_2)
df_entrenamiento_2 <- df_2[indices_2, ]
df_prueba_2 <- df_2[-indices_2, ]

set.seed(123)
num_filas_3 <- nrow(df_3)
indices_3 <- sample(1:num_filas_3, 0.7 * num_filas_3)
df_entrenamiento_3 <- df_3[indices_3, ]
df_prueba_3 <- df_3[-indices_3, ]

set.seed(123)
num_filas_4 <- nrow(df_4)
indices_4 <- sample(1:num_filas_4, 0.7 * num_filas_4)
df_entrenamiento_4 <- df_4[indices_4, ]
df_prueba_4 <- df_4[-indices_4, ]

# 4. Regresiones logísticas variable adherencia a la medicación:

# 4.1. Regresión logística toda la cohorte:

reg_mmas <- glm(formula = mmas_adh_12m__01_num ~ 
                  edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                data = df_entrenamiento,
                family = 'binomial')


# 4.1.1. Extracción de los odd ratios Y CIs:

odd_ratio_mmas_gen <- exp(reg_mmas$coefficients)
int_conf_mmas_gen <- exp(confint(reg_mmas) )
stats_mmas <- data.frame(
  variable = names(odd_ratio_mmas_gen),
  odd_ratio = odd_ratio_mmas_gen,
  LCI = int_conf_mmas_gen[,1],
  UCI = int_conf_mmas_gen[,2]
)

# 4.1.2. Validación de la regresión:

prob_mmas_gen <- predict(reg_mmas, newdata = df_prueba, type = "response")
predicts_mmas_gen <- ifelse(prob_mmas_gen > 0.5, "Adherente", "No adherente")
predicts_mmas_gen <- as.factor(predicts_mmas_gen)
predicts_mmas_gen_num <- ifelse(prob_mmas_gen > 0.5, 1, 0)




# 4.1.2.1. Matriz de confusión:

matriz_mmas_gen <- confusionMatrix(df_prueba$mmas_adh_12m__01,predicts_mmas_gen)
tabla_matriz_mmas_gen <- as.data.frame(matriz_mmas_gen$table)
tabla_matriz_mmas_gen$Reference <- factor(tabla_matriz_mmas_gen$Reference, levels = rev(levels(tabla_matriz_mmas_gen$Reference)))

mat_conf_mmas_gen <-ggplot(tabla_matriz_mmas_gen, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_mmas_gen$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la medicación\nToda la cohorte",
       subtitle = paste0("Exactitud: ",round(matriz_mmas_gen$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_mmas_gen$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_mmas_gen$byClass[1],3),
                         "\nF1-score: ", round(matriz_mmas_gen$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
        )

# 4.1.2.2. Curva ROC:

curva_roc_mmas_gen <- roc (df_prueba$mmas_adh_12m__01_num,predicts_mmas_gen_num)
ggroc(curva_roc_mmas_gen)

# 4.2. Regresión logística grupo 1:

reg_mmas_1 <- glm(formula = mmas_adh_12m__01_num ~ 
                  edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                data = df_entrenamiento_1,
                family = 'binomial')


# 4.2.1. Extracción de los odd ratios Y CIs:

odd_ratio_mmas_1 <- exp(reg_mmas_1$coefficients)
int_conf_mmas_1 <- exp(confint(reg_mmas_1) )
stats_mmas_1 <- data.frame(
  variable = names(odd_ratio_mmas_1),
  odd_ratio = odd_ratio_mmas_1,
  LCI = int_conf_mmas_1[,1],
  UCI = int_conf_mmas_1[,2]
)

# 4.2.2. Validación de la regresión:

prob_mmas_1 <- predict(reg_mmas_1, newdata = df_prueba_1, type = "response")
predicts_mmas_1 <- ifelse(prob_mmas_1 > 0.5, "Adherente", "No adherente")
predicts_mmas_1 <- as.factor(predicts_mmas_1)
predicts_mmas_1_num <- ifelse(prob_mmas_1 > 0.5, 1, 0)

# 4.2.2.1. Matriz de confusión:

matriz_mmas_1 <- confusionMatrix(df_prueba_1$mmas_adh_12m__01,predicts_mmas_1)
tabla_matriz_mmas_1 <- as.data.frame(matriz_mmas_1$table)
tabla_matriz_mmas_1$Reference <- factor(tabla_matriz_mmas_1$Reference, levels = rev(levels(tabla_matriz_mmas_1$Reference)))

mat_conf_1 <- ggplot(tabla_matriz_mmas_1, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_mmas_1$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la medicación\nGrupo 1",
       subtitle = paste0("Exactitud: ",round(matriz_mmas_1$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_mmas_1$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_mmas_1$byClass[1],3),
                         "\nF1-score: ", round(matriz_mmas_1$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_1

# 4.2.2.2. Curva ROC:

curva_roc_mmas_1 <- roc (df_prueba_1$mmas_adh_12m__01_num,predicts_mmas_1_num)
ggroc(curva_roc_mmas_1)

# 4.3. Regresión logística grupo 2:

reg_mmas_2 <- glm(formula = mmas_adh_12m__01_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_2,
                  family = 'binomial')


# 4.3.1. Extracción de los odd ratios Y CIs:

odd_ratio_mmas_2 <- exp(reg_mmas_2$coefficients)
int_conf_mmas_2 <- exp(confint(reg_mmas_2))
stats_mmas_2 <- data.frame(
  variable = names(reg_mmas_2),
  odd_ratio = odd_ratio_mmas_2,
  LCI = int_conf_mmas_2[,1],
  UCI = int_conf_mmas_2[,2]
)

# 4.3.2. Validación de la regresión:

prob_mmas_2 <- predict(reg_mmas_2, newdata = df_prueba_2, type = "response")
predicts_mmas_2 <- ifelse(prob_mmas_2 > 0.5, "Adherente", "No adherente")
predicts_mmas_2 <- as.factor(predicts_mmas_2)
predicts_mmas_2_num <- ifelse(prob_mmas_2 > 0.5, 1, 0)

# 4.3.2.1. Matriz de confusión:

matriz_mmas_2 <- confusionMatrix(df_prueba_2$mmas_adh_12m__01,predicts_mmas_2)
tabla_matriz_mmas_2 <- as.data.frame(matriz_mmas_2$table)
tabla_matriz_mmas_2$Reference <- factor(tabla_matriz_mmas_2$Reference, levels = rev(levels(tabla_matriz_mmas_2$Reference)))

mat_conf_2 <- ggplot(tabla_matriz_mmas_2, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_mmas_2$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la medicación\nGrupo 2",
       subtitle = paste0("Exactitud: ",round(matriz_mmas_2$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_mmas_2$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_mmas_2$byClass[1],3),
                         "\nF1-score: ", round(matriz_mmas_2$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_2

# 4.3.2.2. Curva ROC:

curva_roc_mmas_2 <- roc (df_prueba_2$mmas_adh_12m__01_num,predicts_mmas_2_num)
ggroc(curva_roc_mmas_gen)

# 4.4. Regresión logística grupo 3:

reg_mmas_3 <- glm(formula = mmas_adh_12m__01_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_3,
                  family = 'binomial')


# 4.4.1. Extracción de los odd ratios Y CIs:

odd_ratio_mmas_3 <- exp(reg_mmas_3$coefficients)
int_conf_mmas_3 <- exp(confint(reg_mmas_3))
stats_mmas_3 <- data.frame(
  variable = names(odd_ratio_mmas_3),
  odd_ratio = odd_ratio_mmas_3,
  LCI = int_conf_mmas_3[,1],
  UCI = int_conf_mmas_3[,2]
)

# 4.4.2. Validación de la regresión:

prob_mmas_3 <- predict(reg_mmas_3, newdata = df_prueba_3, type = "response")
predicts_mmas_3 <- ifelse(prob_mmas_3 > 0.5, "Adherente", "No adherente")
predicts_mmas_3 <- as.factor(predicts_mmas_3)
predicts_mmas_3_num <- ifelse(prob_mmas_3 > 0.5, 1, 0)

# 4.4.2.1. Matriz de confusión:

matriz_mmas_3 <- confusionMatrix(df_prueba_3$mmas_adh_12m__01,predicts_mmas_3)
tabla_matriz_mmas_3 <- as.data.frame(matriz_mmas_3$table)
tabla_matriz_mmas_3$Reference <- factor(tabla_matriz_mmas_3$Reference, levels = rev(levels(tabla_matriz_mmas_3$Reference)))

mat_conf_3 <- ggplot(tabla_matriz_mmas_3, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_mmas_2$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la medicación\nGrupo 3",
       subtitle = paste0("Exactitud: ",round(matriz_mmas_3$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_mmas_3$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_mmas_3$byClass[1],3),
                         "\nF1-score: ", round(matriz_mmas_3$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_3

# 4.4.2.2. Curva ROC:

curva_roc_mmas_3 <- roc (df_prueba_3$mmas_adh_12m__01_num,predicts_mmas_3_num)
ggroc(curva_roc_mmas_3)

# 4.5. Regresión logística grupo 4:

reg_mmas_4 <- glm(formula = mmas_adh_12m__01_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_4,
                  family = 'binomial')


# 4.5.1. Extracción de los odd ratios Y CIs:

odd_ratio_mmas_4 <- exp(reg_mmas_4$coefficients)
int_conf_mmas_4 <- exp(confint(reg_mmas_4))
stats_mmas_4 <- data.frame(
  variable = names(odd_ratio_mmas_4),
  odd_ratio = odd_ratio_mmas_4,
  LCI = int_conf_mmas_4[,1],
  UCI = int_conf_mmas_4[,2]
)

# 4.5.2. Validación de la regresión:

prob_mmas_4 <- predict(reg_mmas_4, newdata = df_prueba_4, type = "response")
predicts_mmas_4 <- ifelse(prob_mmas_4 > 0.5, "Adherente", "No adherente")
predicts_mmas_4 <- as.factor(predicts_mmas_4)
predicts_mmas_4_num <- ifelse(prob_mmas_4 > 0.5, 1, 0)

# 4.5.2.1. Matriz de confusión:

matriz_mmas_4 <- confusionMatrix(df_prueba_4$mmas_adh_12m__01,predicts_mmas_4)
tabla_matriz_mmas_4 <- as.data.frame(matriz_mmas_4$table)
tabla_matriz_mmas_4$Reference <- factor(tabla_matriz_mmas_4$Reference, levels = rev(levels(tabla_matriz_mmas_4$Reference)))

mat_conf_4 <- ggplot(tabla_matriz_mmas_4, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_mmas_4$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la medicación\nGrupo 4",
       subtitle = paste0("Exactitud: ",round(matriz_mmas_4$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_mmas_4$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_mmas_4$byClass[1],3),
                         "\nF1-score: ", round(matriz_mmas_4$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_4

# 4.5.2.2. Curva ROC:

curva_roc_mmas_4 <- roc (df_prueba_4$mmas_adh_12m__01_num,predicts_mmas_4_num)
ggroc(curva_roc_mmas_4)


# 4.6. Curvas ROC de las regresiones:

roc_mmas <- ggroc(list(curva_roc_mmas_gen,curva_roc_mmas_1,curva_roc_mmas_3,curva_roc_mmas_4))+
  labs(title = "Curvas ROC\nAdherencia a la medicación",
       x = "Especifiedad",
       y = "Sensibilidad",
       color = "Regresión") +
  theme_minimal() +
  theme( plot.title = element_text(hjust = 0.5, face = "bold",size = 15))+
  scale_color_manual(  
                     values = rainbow(4), 
                     labels = c("General", "Grupo 1", "Grupo 3", "Grupo 4"))+
  geom_line(size = 1)

roc_mmas

# 4.7. Extracción de los AUC:

auc_mmas_gen <- round(auc(df_prueba$mmas_adh_12m__01_num,predicts_mmas_gen_num),4)
auc_mmas_1 <- round(auc(df_prueba_1$mmas_adh_12m__01_num,predicts_mmas_1_num),4)
auc_mmas_3 <- round(auc(df_prueba_3$mmas_adh_12m__01_num,predicts_mmas_3_num),4)
auc_mmas_4 <- round(auc(df_prueba_4$mmas_adh_12m__01_num,predicts_mmas_4_num),4)

df_auc_mmas <- data.frame(
  AUC = c("General","Grupo 1", "Grupo 3", "Grupo 4"),
  valor = c(auc_mmas_gen,auc_mmas_1,auc_mmas_3,auc_mmas_4)
)

graf_auc_mmas <- ggplot(data = df_auc_mmas, aes(x = AUC, y = valor, fill = AUC)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = valor), color = "black", size = 5,position = position_stack(vjust = 1.05)) +
  scale_fill_viridis(discrete = TRUE) +  
  labs(title = "Histograma AUC por regresión\nAdherencia a la medicación",
       x = "Regresión",
       y = "AUC") +
  theme_minimal()+
  theme( plot.title = element_text(hjust = 0.5, face = "bold",size = 15))

graf_auc_mmas

graf_mmas <- grid.arrange(roc_mmas, graf_auc_mmas, ncol = 2)

# 5. Regresiones logísticas variable adherencia a la dieta mediterránea:

# 5.1. Regresión logística toda la cohorte:

reg_medas <- glm(formula = medas_adherencia_12m_imp_num ~ 
                  edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                data = df_entrenamiento,
                family = 'binomial')

# 5.1.1. Extracción de los odd ratios Y CIs:

odd_ratio_medas_gen <- exp(reg_medas$coefficients)
int_conf_medas_gen <- exp(confint(reg_medas) )
stats_medas <- data.frame(
  variable = names(odd_ratio_medas_gen),
  odd_ratio = odd_ratio_medas_gen,
  LCI = int_conf_medas_gen[,1],
  UCI = int_conf_medas_gen[,2]
)

# 5.1.2. Validación de la regresión:

prob_medas_gen <- predict(reg_medas, newdata = df_prueba, type = "response")
predicts_medas_gen <- ifelse(prob_medas_gen > 0.5, "Adherente", "No adherente")
predicts_medas_gen <- as.factor(predicts_medas_gen)
predicts_medas_gen_num <- ifelse(prob_medas_gen > 0.5, 1, 0)


print(df_prueba$medas_adherencia_12m_imp)
print(predicts_medas_gen)
# 5.1.2.1. Matriz de confusión:

matriz_medas_gen <- confusionMatrix(df_prueba$medas_adherencia_12m_imp,predicts_medas_gen)
tabla_matriz_medas_gen <- as.data.frame(matriz_medas_gen$table)
tabla_matriz_medas_gen$Reference <- factor(tabla_matriz_medas_gen$Reference, levels = rev(levels(tabla_matriz_medas_gen$Reference)))

mat_conf_medas_gen <-ggplot(tabla_matriz_medas_gen, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_medas_gen$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la dieta mediterránea\nToda la cohorte",
       subtitle = paste0("Exactitud: ",round(matriz_medas_gen$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_medas_gen$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_medas_gen$byClass[1],3),
                         "\nF1-score: ", round(matriz_medas_gen$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_medas_gen

# 5.1.2.2. Curva ROC:

curva_roc_medas_gen <- roc (df_prueba$medas_adherencia_12m_imp_num,predicts_medas_gen_num)
ggroc(curva_roc_medas_gen)

# 5.2. Regresión logística grupo 1:

reg_medas_1 <- glm(formula = medas_adherencia_12m_imp_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_1,
                  family = 'binomial')


# 5.2.1. Extracción de los odd ratios Y CIs:

odd_ratio_medas_1 <- exp(reg_medas_1$coefficients)
int_conf_medas_1 <- exp(confint(reg_medas_1) )
stats_medas_1 <- data.frame(
  variable = names(odd_ratio_medas_1),
  odd_ratio = odd_ratio_medas_1,
  LCI = int_conf_medas_1[,1],
  UCI = int_conf_medas_1[,2]
)

# 5.2.2. Validación de la regresión:

prob_medas_1 <- predict(reg_medas_1, newdata = df_prueba_1, type = "response")
predicts_medas_1 <- ifelse(prob_medas_1 > 0.5, "Adherente", "No adherente")
predicts_medas_1 <- as.factor(predicts_medas_1)
predicts_medas_1_num <- ifelse(prob_medas_1 > 0.5, 1, 0)

# 5.2.2.1. Matriz de confusión:

matriz_medas_1 <- confusionMatrix(df_prueba_1$mmas_adh_12m__01,predicts_mmas_1)
tabla_matriz_medas_1 <- as.data.frame(matriz_medas_1$table)
tabla_matriz_medas_1$Reference <- factor(tabla_matriz_medas_1$Reference, levels = rev(levels(tabla_matriz_medas_1$Reference)))

mat_conf_medas_1 <- ggplot(tabla_matriz_medas_1, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_medas_1$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la dieta mediterránea\nGrupo 1",
       subtitle = paste0("Exactitud: ",round(matriz_medas_1$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_medas_1$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_medas_1$byClass[1],3),
                         "\nF1-score: ", round(matriz_medas_1$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_medas_1

# 5.2.2.2. Curva ROC:

curva_roc_medas_1 <- roc (df_prueba_1$medas_adherencia_12m_imp_num,predicts_medas_1_num)
ggroc(curva_roc_medas_1)

# 5.3. Regresión logística grupo 2:

reg_medas_2 <- glm(formula = medas_adherencia_12m_imp_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_2,
                  family = 'binomial')


# 5.3.1. Extracción de los odd ratios Y CIs:

odd_ratio_medas_2 <- exp(reg_medas_2$coefficients)
int_conf_medas_2 <- exp(confint(reg_medas_2))
stats_medas_2 <- data.frame(
  variable = names(odd_ratio_medas_2),
  odd_ratio = odd_ratio_medas_2,
  LCI = int_conf_medas_2[,1],
  UCI = int_conf_medas_2[,2]
)

# 5.3.2. Validación de la regresión:

prob_medas_2 <- predict(reg_medas_2, newdata = df_prueba_2, type = "response")
predicts_medas_2 <- ifelse(prob_medas_2 > 0.5, "Adherente", "No adherente")
predicts_medas_2 <- as.factor(predicts_medas_2)
predicts_medas_2_num <- ifelse(prob_medas_2 > 0.5, 1, 0)

# 5.3.2.1. Matriz de confusión:

matriz_medas_2 <- confusionMatrix(df_prueba_2$medas_adherencia_12m_imp,predicts_medas_2)
tabla_matriz_medas_2 <- as.data.frame(matriz_medas_2$table)
tabla_matriz_medas_2$Reference <- factor(tabla_matriz_medas_2$Reference, levels = rev(levels(tabla_matriz_medas_2$Reference)))

mat_conf_medas_2 <- ggplot(tabla_matriz_mmas_2, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_medas_2$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la dieta mediterránea\nGrupo 2",
       subtitle = paste0("Exactitud: ",round(matriz_medas_2$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_medas_2$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_medas_2$byClass[1],3),
                         "\nF1-score: ", round(matriz_medas_2$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  scale_y_discrete(limits = rev(levels(tabla_matriz_medas_2$Prediction)))+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_medas_2

# 5.3.2.2. Curva ROC:

curva_roc_medas_2 <- roc (df_prueba_2$medas_adherencia_12m_imp_num,predicts_medas_2_num)
ggroc(curva_roc_medas_2)

# 5.4. Regresión logística grupo 3:

reg_medas_3 <- glm(formula = medas_adherencia_12m_imp_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_3,
                  family = 'binomial')


# 5.4.1. Extracción de los odd ratios Y CIs:

odd_ratio_medas_3 <- exp(reg_medas_3$coefficients)
int_conf_medas_3 <- exp(confint(reg_medas_3))
stats_medas_3 <- data.frame(
  variable = names(odd_ratio_medas_3),
  odd_ratio = odd_ratio_medas_3,
  LCI = int_conf_medas_3[,1],
  UCI = int_conf_medas_3[,2]
)

# 5.4.2. Validación de la regresión:

prob_medas_3 <- predict(reg_medas_3, newdata = df_prueba_3, type = "response")
predicts_medas_3 <- ifelse(prob_medas_3 > 0.5, "Adherente", "No adherente")
predicts_medas_3 <- as.factor(predicts_medas_3)
predicts_medas_3_num <- ifelse(prob_medas_3 > 0.5, 1, 0)

# 5.4.2.1. Matriz de confusión:

matriz_medas_3 <- confusionMatrix(df_prueba_3$medas_adherencia_12m_imp,predicts_medas_3)
tabla_matriz_medas_3 <- as.data.frame(matriz_medas_3$table)
tabla_matriz_medas_3$Reference <- factor(tabla_matriz_medas_3$Reference, levels = rev(levels(tabla_matriz_medas_3$Reference)))

mat_conf_medas_3 <- ggplot(tabla_matriz_medas_3, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_medas_3$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la dieta mediterránea\nGrupo 3",
       subtitle = paste0("Exactitud: ",round(matriz_medas_3$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_medas_3$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_medas_3$byClass[1],3),
                         "\nF1-score: ", round(matriz_medas_3$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_medas_3

# 5.4.2.2. Curva ROC:

curva_roc_medas_3 <- roc (df_prueba_3$medas_adherencia_12m_imp_num,predicts_medas_3_num)
ggroc(curva_roc_medas_3)

# 5.5. Regresión logística grupo 4:

reg_medas_4 <- glm(formula = medas_adherencia_12m_imp_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_4,
                  family = 'binomial')


# 5.5.1. Extracción de los odd ratios Y CIs:

odd_ratio_medas_4 <- exp(reg_medas_4$coefficients)
int_conf_medas_4 <- exp(confint(reg_medas_4))
stats_medas_4 <- data.frame(
  variable = names(odd_ratio_medas_4),
  odd_ratio = odd_ratio_medas_4,
  LCI = int_conf_medas_4[,1],
  UCI = int_conf_medas_4[,2]
)

# 5.5.2. Validación de la regresión:

prob_medas_4 <- predict(reg_medas_4, newdata = df_prueba_4, type = "response")
predicts_medas_4 <- ifelse(prob_medas_4 > 0.5, "Adherente", "No adherente")
predicts_medas_4 <- as.factor(predicts_medas_4)
predicts_medas_4_num <- ifelse(prob_medas_4 > 0.5, 1, 0)

# 5.5.2.1. Matriz de confusión:

matriz_medas_4 <- confusionMatrix(df_prueba_4$medas_adherencia_12m_imp,predicts_medas_4)
tabla_matriz_medas_4 <- as.data.frame(matriz_medas_4$table)
tabla_matriz_medas_4$Reference <- factor(tabla_matriz_medas_4$Reference, levels = rev(levels(tabla_matriz_medas_4$Reference)))

mat_conf_medas_4 <- ggplot(tabla_matriz_medas_4, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_medas_4$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la dieta mediterránea\nGrupo 4",
       subtitle = paste0("Exactitud: ",round(matriz_medas_4$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_medas_4$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_medas_4$byClass[1],3),
                         "\nF1-score: ", round(matriz_medas_4$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_medas_4

# 5.5.2.2. Curva ROC:

curva_roc_medas_4 <- roc (df_prueba_4$medas_adherencia_12m_imp_num,predicts_medas_4_num)
ggroc(curva_roc_medas_4)

# 5.6. Curvas ROC de las regresiones:

roc_medas <- ggroc(list(curva_roc_medas_gen,curva_roc_medas_1,curva_roc_medas_2,curva_roc_medas_3,curva_roc_medas_4))+
  labs(title = "Curvas ROC\nAdherencia a la dieta mediterránea",
       x = "Especifiedad",
       y = "Sensibilidad",
       color = "Regresión") +
  theme_minimal() +
  theme( plot.title = element_text(hjust = 0.5, face = "bold",size = 15))+
  scale_color_manual(  
    values = rainbow(5), 
    labels = c("General", "Grupo 1","Grupo 2", "Grupo 3", "Grupo 4"))+
  geom_line(size = 1)

roc_medas

# 5.7. Extracción de los AUC:

auc_medas_gen <- round(auc(df_prueba$medas_adherencia_12m_imp_num,predicts_medas_gen_num),4)
auc_medas_1 <- round(auc(df_prueba_1$medas_adherencia_12m_imp_num,predicts_medas_1_num),4)
auc_medas_2 <- round(auc(df_prueba_2$medas_adherencia_12m_imp_num,predicts_medas_2_num),4)
auc_medas_3 <- round(auc(df_prueba_3$medas_adherencia_12m_imp_num,predicts_medas_3_num),4)
auc_medas_4 <- round(auc(df_prueba_4$medas_adherencia_12m_imp_num,predicts_medas_4_num),4)

df_auc_medas <- data.frame(
  AUC = c("General","Grupo 1","Grupo 2", "Grupo 3", "Grupo 4"),
  valor = c(auc_medas_gen,auc_medas_1,auc_medas_2,auc_mmas_3,auc_mmas_4)
)

graf_auc_medas <- ggplot(data = df_auc_medas, aes(x = AUC, y = valor, fill = AUC)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = valor), color = "black", size = 5,position = position_stack(vjust = 1.05)) +
  scale_fill_viridis(discrete = TRUE) +  
  labs(title = "Histograma AUC por regresión\nAdherencia a la dieta mediterránea",
       x = "Regresión",
       y = "AUC") +
  theme_minimal()+
  theme( plot.title = element_text(hjust = 0.5, face = "bold",size = 15))

graf_auc_medas

graf_medas <- grid.arrange(roc_medas, graf_auc_medas, ncol = 2)

# 6. Regresiones logísticas variable adherencia a la actividad física:

# 6.1. Regresión logística toda la cohorte:

reg_ipaq <- glm(formula = ipaq_adhe_12m_imp_num ~ 
                   edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                 data = df_entrenamiento,
                 family = 'binomial')

# 6.1.1. Extracción de los odd ratios Y CIs:

odd_ratio_ipaq_gen <- exp(reg_ipaq$coefficients)
int_conf_ipaq_gen <- exp(confint(reg_ipaq) )
stats_ipaq <- data.frame(
  variable = names(odd_ratio_ipaq_gen),
  odd_ratio = odd_ratio_medas_gen,
  LCI = int_conf_ipaq_gen[,1],
  UCI = int_conf_ipaq_gen[,2]
)

# 6.1.2. Validación de la regresión:

prob_ipaq_gen <- predict(reg_ipaq, newdata = df_prueba, type = "response")
predicts_ipaq_gen <- ifelse(prob_ipaq_gen > 0.5, "Adherente", "No adherente")
predicts_ipaq_gen <- as.factor(predicts_ipaq_gen)
predicts_ipaq_gen_num <- ifelse(prob_ipaq_gen > 0.5, 1, 0)

# 6.1.2.1. Matriz de confusión:

matriz_ipaq_gen <- confusionMatrix(df_prueba$ipaq_adhe_12m_imp,predicts_ipaq_gen)
tabla_matriz_ipaq_gen <- as.data.frame(matriz_ipaq_gen$table)
tabla_matriz_ipaq_gen$Reference <- factor(tabla_matriz_ipaq_gen$Reference, levels = rev(levels(tabla_matriz_ipaq_gen$Reference)))

mat_conf_ipaq_gen <-ggplot(tabla_matriz_ipaq_gen, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_ipaq_gen$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la actividad física\nToda la cohorte",
       subtitle = paste0("Exactitud: ",round(matriz_ipaq_gen$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_ipaq_gen$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_ipaq_gen$byClass[1],3),
                         "\nF1-score: ", round(matriz_ipaq_gen$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_ipaq_gen

# 6.1.2.2. Curva ROC:

curva_roc_ipaq_gen <- roc (df_prueba$ipaq_adhe_12m_imp_num,predicts_ipaq_gen_num)
ggroc(curva_roc_ipaq_gen)

# 6.2. Regresión logística grupo 1:

reg_ipaq_1 <- glm(formula = ipaq_adhe_12m_imp_num ~ 
                     edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                   data = df_entrenamiento_1,
                   family = 'binomial')


# 6.2.1. Extracción de los odd ratios Y CIs:

odd_ratio_ipaq_1 <- exp(reg_ipaq_1$coefficients)
int_conf_ipaq_1 <- exp(confint(reg_ipaq_1) )
stats_ipaq_1 <- data.frame(
  variable = names(odd_ratio_ipaq_1),
  odd_ratio = odd_ratio_ipaq_1,
  LCI = int_conf_ipaq_1[,1],
  UCI = int_conf_ipaq_1[,2]
)

# 6.2.2. Validación de la regresión:

prob_ipaq_1 <- predict(reg_ipaq_1, newdata = df_prueba_1, type = "response")
predicts_ipaq_1 <- ifelse(prob_ipaq_1 > 0.5, "Adherente", "No adherente")
predicts_ipaq_1 <- as.factor(predicts_ipaq_1)
predicts_ipaq_1_num <- ifelse(prob_ipaq_1 > 0.5, 1, 0)

# 6.2.2.1. Matriz de confusión:

matriz_ipaq_1 <- confusionMatrix(df_prueba_1$ipaq_adhe_12m_imp,predicts_ipaq_1)
tabla_matriz_ipaq_1 <- as.data.frame(matriz_ipaq_1$table)
tabla_matriz_ipaq_1$Reference <- factor(tabla_matriz_ipaq_1$Reference, levels = rev(levels(tabla_matriz_ipaq_1$Reference)))

mat_conf_ipaq_1 <- ggplot(tabla_matriz_ipaq_1, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_ipaq_1$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la actividad física\nGrupo 1",
       subtitle = paste0("Exactitud: ",round(matriz_ipaq_1$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_ipaq_1$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_ipaq_1$byClass[1],3),
                         "\nF1-score: ", round(matriz_ipaq_1$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_ipaq_1

# 6.2.2.2. Curva ROC:

curva_roc_ipaq_1 <- roc (df_prueba_1$ipaq_adhe_12m_imp_num,predicts_ipaq_1_num)
ggroc(curva_roc_ipaq_1)

# 6.3. Regresión logística grupo 2:

reg_ipaq_2 <- glm(formula = ipaq_adhe_12m_imp_num ~ 
                     edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                   data = df_entrenamiento_2,
                   family = 'binomial')


# 6.3.1. Extracción de los odd ratios Y CIs:

odd_ratio_ipaq_2 <- exp(reg_ipaq_2$coefficients)
int_conf_ipaq_2 <- exp(confint(reg_ipaq_2))
stats_ipaq_2 <- data.frame(
  variable = names(odd_ratio_ipaq_2),
  odd_ratio = odd_ratio_ipaq_2,
  LCI = int_conf_ipaq_2[,1],
  UCI = int_conf_ipaq_2[,2]
)

# 6.3.2. Validación de la regresión:

prob_ipaq_2 <- predict(reg_ipaq_2, newdata = df_prueba_2, type = "response")
predicts_ipaq_2 <- ifelse(prob_ipaq_2 > 0.5, "Adherente", "No adherente")
predicts_ipaq_2 <- as.factor(predicts_ipaq_2)
predicts_ipaq_2_num <- ifelse(prob_ipaq_2 > 0.5, 1, 0)

# 6.3.2.1. Matriz de confusión:

matriz_ipaq_2 <- confusionMatrix(df_prueba_2$ipaq_adhe_12m_imp,predicts_ipaq_2)
tabla_matriz_ipaq_2 <- as.data.frame(matriz_ipaq_2$table)
tabla_matriz_ipaq_2$Reference <- factor(tabla_matriz_ipaq_2$Reference, levels = rev(levels(tabla_matriz_ipaq_2$Reference)))

mat_conf_ipaq_2 <- ggplot(tabla_matriz_ipaq_2, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_ipaq_2$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la actividad física\nGrupo 2",
       subtitle = paste0("Exactitud: ",round(matriz_ipaq_2$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_ipaq_2$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_ipaq_2$byClass[1],3),
                         "\nF1-score: ", round(matriz_ipaq_2$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  scale_y_discrete(limits = rev(levels(tabla_matriz_ipaq_2$Prediction)))+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_ipaq_2

# 6.3.2.2. Curva ROC:

curva_roc_ipaq_2 <- roc (df_prueba_2$ipaq_adhe_12m_imp_num,predicts_ipaq_2_num)
ggroc(curva_roc_ipaq_2)

# 6.4. Regresión logística grupo 3:

reg_ipaq_3 <- glm(formula = ipaq_adhe_12m_imp_num ~ 
                     edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                   data = df_entrenamiento_3,
                   family = 'binomial')


# 6.4.1. Extracción de los odd ratios Y CIs:

odd_ratio_ipaq_3 <- exp(reg_ipaq_3$coefficients)
int_conf_ipaq_3 <- exp(confint(reg_ipaq_3))
stats_ipaq_3 <- data.frame(
  variable = names(odd_ratio_ipaq_3),
  odd_ratio = odd_ratio_ipaq_3,
  LCI = int_conf_ipaq_3[,1],
  UCI = int_conf_ipaq_3[,2]
)

# 6.4.2. Validación de la regresión:

prob_ipaq_3 <- predict(reg_ipaq_3, newdata = df_prueba_3, type = "response")
predicts_ipaq_3 <- ifelse(prob_ipaq_3 > 0.5, "Adherente", "No adherente")
predicts_ipaq_3 <- as.factor(predicts_ipaq_3)
predicts_ipaq_3_num <- ifelse(prob_ipaq_3 > 0.5, 1, 0)

# 6.4.2.1. Matriz de confusión:

matriz_ipaq_3 <- confusionMatrix(df_prueba_3$ipaq_adhe_12m_imp,predicts_ipaq_3)
tabla_matriz_ipaq_3 <- as.data.frame(matriz_ipaq_3$table)
tabla_matriz_ipaq_3$Reference <- factor(tabla_matriz_ipaq_3$Reference, levels = rev(levels(tabla_matriz_ipaq_3$Reference)))

mat_conf_ipaq_3 <- ggplot(tabla_matriz_ipaq_3, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_ipaq_3$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la actividad física\nGrupo 3",
       subtitle = paste0("Exactitud: ",round(matriz_ipaq_3$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_ipaq_3$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_ipaq_3$byClass[1],3),
                         "\nF1-score: ", round(matriz_ipaq_3$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_ipaq_3

# 6.4.2.2. Curva ROC:

curva_roc_ipaq_3 <- roc (df_prueba_3$ipaq_adhe_12m_imp_num,predicts_ipaq_3_num)
ggroc(curva_roc_ipaq_3)

# 6.5. Regresión logística grupo 4:

reg_ipaq_4 <- glm(formula = ipaq_adhe_12m_imp_num ~ 
                     edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                   data = df_entrenamiento_4,
                   family = 'binomial')


# 6.5.1. Extracción de los odd ratios Y CIs:

odd_ratio_ipaq_4 <- exp(reg_ipaq_4$coefficients)
int_conf_ipaq_4 <- exp(confint(reg_ipaq_4))
stats_ipaq_4 <- data.frame(
  variable = names(odd_ratio_ipaq_4),
  odd_ratio = odd_ratio_ipaq_4,
  LCI = int_conf_ipaq_4[,1],
  UCI = int_conf_ipaq_4[,2]
)

# 6.5.2. Validación de la regresión:

prob_ipaq_4 <- predict(reg_ipaq_4, newdata = df_prueba_4, type = "response")
predicts_ipaq_4 <- ifelse(prob_ipaq_4 > 0.5, "Adherente", "No adherente")
predicts_ipaq_4 <- as.factor(predicts_ipaq_4)
predicts_ipaq_4_num <- ifelse(prob_ipaq_4 > 0.5, 1, 0)

# 6.5.2.1. Matriz de confusión:

matriz_ipaq_4 <- confusionMatrix(df_prueba_4$medas_adherencia_12m_imp,predicts_ipaq_4)
tabla_matriz_ipaq_4 <- as.data.frame(matriz_ipaq_4$table)
tabla_matriz_ipaq_4$Reference <- factor(tabla_matriz_ipaq_4$Reference, levels = rev(levels(tabla_matriz_ipaq_4$Reference)))

mat_conf_ipaq_4 <- ggplot(tabla_matriz_ipaq_4, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_ipaq_4$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la actividad física\nGrupo 4",
       subtitle = paste0("Exactitud: ",round(matriz_ipaq_4$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_ipaq_4$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_ipaq_4$byClass[1],3),
                         "\nF1-score: ", round(matriz_ipaq_4$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_ipaq_4

# 6.5.2.2. Curva ROC:

curva_roc_ipaq_4 <- roc (df_prueba_4$ipaq_adhe_12m_imp_num,predicts_ipaq_4_num)
ggroc(curva_roc_ipaq_4)

# 6.6. Curvas ROC de las regresiones:

roc_ipaq <- ggroc(list(curva_roc_ipaq_gen,curva_roc_ipaq_1,curva_roc_ipaq_2,curva_roc_ipaq_3,curva_roc_ipaq_4))+
  labs(title = "Curvas ROC\nAdherencia a la actividad física",
       x = "Especifiedad",
       y = "Sensibilidad",
       color = "Regresión") +
  theme_minimal() +
  theme( plot.title = element_text(hjust = 0.5, face = "bold",size = 15))+
  scale_color_manual(  
    values = rainbow(5), 
    labels = c("General", "Grupo 1","Grupo 2", "Grupo 3", "Grupo 4"))+
  geom_line(size = 1)

roc_ipaq

# 6.7. Extracción de los AUC:

auc_ipaq_gen <- round(auc(df_prueba$ipaq_adhe_12m_imp_num,predicts_ipaq_gen_num),4)
auc_ipaq_1 <- round(auc(df_prueba_1$ipaq_adhe_12m_imp_num,predicts_ipaq_1_num),4)
auc_ipaq_2 <- round(auc(df_prueba_2$ipaq_adhe_12m_imp_num,predicts_ipaq_2_num),4)
auc_ipaq_3 <- round(auc(df_prueba_3$ipaq_adhe_12m_imp_num,predicts_ipaq_3_num),4)
auc_ipaq_4 <- round(auc(df_prueba_4$ipaq_adhe_12m_imp_num,predicts_ipaq_4_num),4)

df_auc_ipaq <- data.frame(
  AUC = c("General","Grupo 1","Grupo 2", "Grupo 3", "Grupo 4"),
  valor = c(auc_ipaq_gen,auc_ipaq_1,auc_ipaq_2,auc_ipaq_3,auc_ipaq_4)
)

graf_auc_ipaq <- ggplot(data = df_auc_ipaq, aes(x = AUC, y = valor, fill = AUC)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = valor), color = "black", size = 5,position = position_stack(vjust = 1.05)) +
  scale_fill_viridis(discrete = TRUE) +  
  labs(title = "Histograma AUC por regresión\nAdherencia a la actividad física",
       x = "Regresión",
       y = "AUC") +
  theme_minimal()+
  theme( plot.title = element_text(hjust = 0.5, face = "bold",size = 15))

graf_auc_ipaq

graf_ipaq <- grid.arrange(roc_ipaq, graf_auc_ipaq, ncol = 2)

# 7. Regresiones logísticas variable adherencia total:

# 7.1. Regresión logística toda la cohorte:

reg_total <- glm(formula = adh_total_12m_imp_num ~ 
                  edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                data = df_entrenamiento,
                family = 'binomial')

# 7.1.1. Extracción de los odd ratios Y CIs:

odd_ratio_total_gen <- exp(reg_total$coefficients)
int_conf_total_gen <- exp(confint(reg_total) )
stats_total <- data.frame(
  variable = names(odd_ratio_total_gen),
  odd_ratio = odd_ratio_total_gen,
  LCI = int_conf_total_gen[,1],
  UCI = int_conf_total_gen[,2]
)

# 7.1.2. Validación de la regresión:

prob_total_gen <- predict(reg_total, newdata = df_prueba, type = "response")
predicts_total_gen <- ifelse(prob_total_gen > 0.5, "Adherente", "No adherente")
predicts_total_gen <- as.factor(predicts_total_gen)
predicts_total_gen_num <- ifelse(prob_total_gen > 0.5, 1, 0)

# 7.1.2.1. Matriz de confusión:

matriz_total_gen <- confusionMatrix(df_prueba$adh_total_12m_imp,predicts_total_gen)
tabla_matriz_total_gen <- as.data.frame(matriz_total_gen$table)
tabla_matriz_total_gen$Reference <- factor(tabla_matriz_total_gen$Reference, levels = rev(levels(tabla_matriz_total_gen$Reference)))

mat_conf_total_gen <-ggplot(tabla_matriz_total_gen, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_total_gen$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia total\nToda la cohorte",
       subtitle = paste0("Exactitud: ",round(matriz_total_gen$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_total_gen$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_total_gen$byClass[1],3),
                         "\nF1-score: ", round(matriz_total_gen$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_total_gen

# 7.1.2.2. Curva ROC:

curva_roc_total_gen <- roc (df_prueba$adh_total_12m_imp_num,predicts_total_gen_num)
ggroc(curva_roc_total_gen)

# 7.2. Regresión logística grupo 1:

reg_total_1 <- glm(formula = adh_total_12m_imp_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_1,
                  family = 'binomial')


# 7.2.1. Extracción de los odd ratios Y CIs:

odd_ratio_total_1 <- exp(reg_total_1$coefficients)
int_conf_total_1 <- exp(confint(reg_total_1) )
stats_total_1 <- data.frame(
  variable = names(odd_ratio_total_1),
  odd_ratio = odd_ratio_total_1,
  LCI = int_conf_total_1[,1],
  UCI = int_conf_total_1[,2]
)

# 7.2.2. Validación de la regresión:

prob_total_1 <- predict(reg_total_1, newdata = df_prueba_1, type = "response")
predicts_total_1 <- ifelse(prob_total_1 > 0.5, "Adherente", "No adherente")
predicts_total_1 <- as.factor(predicts_total_1)
predicts_total_1_num <- ifelse(prob_total_1 > 0.5, 1, 0)
predicts_total_1 <- factor(predicts_total_1, levels = c(levels(predicts_total_1), "Adherente"))

# # 7.2.2.1. Matriz de confusión:

matriz_total_1 <- confusionMatrix(df_prueba_1$adh_total_12m_imp,predicts_total_1)
tabla_matriz_total_1 <- as.data.frame(matriz_total_1$table)
tabla_matriz_total_1$Reference <- factor(tabla_matriz_total_1$Reference, levels = rev(levels(tabla_matriz_total_1$Reference)))

mat_conf_total_1 <- ggplot(tabla_matriz_total_1, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_total_1$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia total\nGrupo 1",
       subtitle = paste0("Exactitud: ",round(matriz_total_1$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_total_1$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_total_1$byClass[1],3),
                         "\nF1-score: ", round(matriz_total_1$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_total_1

# 7.2.2.2. Curva ROC:

curva_roc_total_1 <- roc (df_prueba_1$adh_total_12m_imp_num,predicts_total_1_num)
ggroc(curva_roc_ipaq_1)

# 7.3. Regresión logística grupo 2:

reg_total_2 <- glm(formula = adh_total_12m_imp_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_2,
                  family = 'binomial')


# 7.3.1. Extracción de los odd ratios Y CIs:

odd_ratio_total_2 <- exp(reg_total_2$coefficients)
int_conf_total_2 <- exp(confint(reg_total_2))
stats_total_2 <- data.frame(
  variable = names(odd_ratio_total_2),
  odd_ratio = odd_ratio_total_2,
  LCI = int_conf_total_2[,1],
  UCI = int_conf_total_2[,2]
)

# 7.3.2. Validación de la regresión:

prob_total_2 <- predict(reg_total_2, newdata = df_prueba_2, type = "response")
predicts_total_2 <- ifelse(prob_total_2 > 0.5, "Adherente", "No adherente")
predicts_total_2 <- as.factor(predicts_total_2)
predicts_total_2_num <- ifelse(prob_total_2 > 0.5, 1, 0)

# 7.3.2.1. Matriz de confusión:

matriz_total_2 <- confusionMatrix(df_prueba_2$adh_total_12m_imp,predicts_total_2)
tabla_matriz_total_2 <- as.data.frame(matriz_total_2$table)
tabla_matriz_total_2$Reference <- factor(tabla_matriz_total_2$Reference, levels = rev(levels(tabla_matriz_total_2$Reference)))

mat_conf_total_2 <- ggplot(tabla_matriz_total_2, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_total_2$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia total\nGrupo 2",
       subtitle = paste0("Exactitud: ",round(matriz_total_2$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_total_2$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_total_2$byClass[1],3),
                         "\nF1-score: ", round(matriz_total_2$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  scale_y_discrete(limits = rev(levels(tabla_matriz_ipaq_2$Prediction)))+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_total_2

# 7.3.2.2. Curva ROC:

# curva_roc_total_2 <- roc (df_prueba_2$adh_total_12m_imp_num,predicts_total_2_num)
# ggroc(curva_roc_total_2)

# 7.4. Regresión logística grupo 3:

reg_total_3 <- glm(formula = adh_total_12m_imp_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_3,
                  family = 'binomial')


# 7.4.1. Extracción de los odd ratios Y CIs:

odd_ratio_total_3 <- exp(reg_total_3$coefficients)
int_conf_total_3 <- exp(confint(reg_total_3))
stats_total_3 <- data.frame(
  variable = names(odd_ratio_total_3),
  odd_ratio = odd_ratio_total_3,
  LCI = int_conf_total_3[,1],
  UCI = int_conf_total_3[,2]
)

# 7.4.2. Validación de la regresión:

prob_total_3 <- predict(reg_total_3, newdata = df_prueba_3, type = "response")
predicts_total_3 <- ifelse(prob_total_3 > 0.5, "Adherente", "No adherente")
predicts_total_3 <- as.factor(predicts_total_3)
predicts_total_3_num <- ifelse(prob_total_3 > 0.5, 1, 0)

# 7.4.2.1. Matriz de confusión:

matriz_total_3 <- confusionMatrix(df_prueba_3$adh_total_12m_imp,predicts_total_3)
tabla_matriz_total_3 <- as.data.frame(matriz_total_3$table)
tabla_matriz_total_3$Reference <- factor(tabla_matriz_total_3$Reference, levels = rev(levels(tabla_matriz_total_3$Reference)))

mat_conf_total_3 <- ggplot(tabla_matriz_total_3, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_total_3$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia total\nGrupo 3",
       subtitle = paste0("Exactitud: ",round(matriz_total_3$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_total_3$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_total_3$byClass[1],3),
                         "\nF1-score: ", round(matriz_total_3$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_total_3

# 7.4.2.2. Curva ROC:

curva_roc_total_3 <- roc (df_prueba_3$adh_total_12m_imp_num,predicts_total_3_num)
ggroc(curva_roc_total_3)

# 7.5. Regresión logística grupo 4:

reg_total_4 <- glm(formula = adh_total_12m_imp_num ~ 
                    edad + sexo +vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria,
                  data = df_entrenamiento_4,
                  family = 'binomial')


# 7.5.1. Extracción de los odd ratios Y CIs:

odd_ratio_total_4 <- exp(reg_total_4$coefficients)
int_conf_total_4 <- exp(confint(reg_total_4))
stats_total_4 <- data.frame(
  variable = names(odd_ratio_total_4),
  odd_ratio = odd_ratio_total_4,
  LCI = int_conf_total_4[,1],
  UCI = int_conf_total_4[,2]
)

# 7.5.2. Validación de la regresión:

prob_total_4 <- predict(reg_total_4, newdata = df_prueba_4, type = "response")
predicts_total_4 <- ifelse(prob_total_4 > 0.5, "Adherente", "No adherente")
predicts_total_4 <- as.factor(predicts_total_4)
predicts_total_4_num <- ifelse(prob_total_4 > 0.5, 1, 0)

# 7.5.2.1. Matriz de confusión:

matriz_total_4 <- confusionMatrix(df_prueba_4$adh_total_12m_imp,predicts_total_4)
tabla_matriz_total_4 <- as.data.frame(matriz_total_4$table)
tabla_matriz_total_4$Reference <- factor(tabla_matriz_total_4$Reference, levels = rev(levels(tabla_matriz_total_4$Reference)))

mat_conf_total_4 <- ggplot(tabla_matriz_total_4, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_total_4$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia total\nGrupo 4",
       subtitle = paste0("Exactitud: ",round(matriz_total_4$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_total_4$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_total_4$byClass[1],3),
                         "\nF1-score: ", round(matriz_total_4$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_total_4

# 7.5.2.2. Curva ROC:

curva_roc_total_4 <- roc (df_prueba_4$adh_total_12m_imp_num,predicts_total_4_num)
ggroc(curva_roc_total_4)

# 7.6. Curvas ROC de las regresiones:

roc_total <- ggroc(list(curva_roc_total_gen,curva_roc_total_1,curva_roc_total_3,curva_roc_total_4))+
  labs(title = "Curvas ROC\nAdherencia total",
       x = "Especifiedad",
       y = "Sensibilidad",
       color = "Regresión") +
  theme_minimal() +
  theme( plot.title = element_text(hjust = 0.5, face = "bold",size = 15))+
  scale_color_manual(  
    values = rainbow(4), 
    labels = c("General", "Grupo 1" , "Grupo 3", "Grupo 4"))+
  geom_line(size = 1)

roc_total

# 7.7. Extracción de los AUC:

auc_total_gen <- round(auc(df_prueba$adh_total_12m_imp_num,predicts_total_gen_num),4)
# auc_total_1 <- round(auc(df_prueba_1$adh_total_12m_imp_num,predicts_total_1_num),4)
auc_total_2 <- round(auc(df_prueba_2$adh_total_12m_imp_num,predicts_total_2_num),4)
auc_total_3 <- round(auc(df_prueba_3$adh_total_12m_imp_num,predicts_total_3_num),4)
auc_total_4 <- round(auc(df_prueba_4$adh_total_12m_imp_num,predicts_total_4_num),4)

df_auc_total <- data.frame(
  AUC = c("General", "Grupo 3", "Grupo 4"),
  valor = c(auc_total_gen,auc_total_3,auc_total_4)
)

graf_auc_total <- ggplot(data = df_auc_total, aes(x = AUC, y = valor, fill = AUC)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = valor), color = "black", size = 5,position = position_stack(vjust = 1.05)) +
  scale_fill_viridis(discrete = TRUE) +  
  labs(title = "Histograma AUC por regresión\nAdherencia total",
       x = "Regresión",
       y = "AUC") +
  theme_minimal()+
  theme( plot.title = element_text(hjust = 0.5, face = "bold",size = 15))

graf_auc_total

graf_total <- grid.arrange(roc_total, graf_auc_total, ncol = 2)

# 8. Regresión logística adherencia a la rehabilitación:

# 8.1. Regresión logística toda la cohorte:

reg_rhc_gen <- multinom(adherencia_rhc_num 
                        ~ edad + sexo + vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria, 
                        data = df_entrenamiento
                        )

# 8.1.1 Extracción de los oddratios:

odd_ratio_rhc_gen <- exp(coef(reg_rhc_gen))
int_conf_rhc_gen <- exp(confint(reg_rhc_gen))
stats_rhc_gen <- data.frame(
  variable = names(odd_ratio_rhc_gen[1,]),
  odd_ratio_1 = odd_ratio_rhc_gen[1,],
  LCI_1 = int_conf_rhc_gen[,1,1],
  UCI_1 = int_conf_rhc_gen[,2,1],
  odd_ratio_2 = odd_ratio_rhc_gen[2,],
  LCI_2 = int_conf_rhc_gen[,1,2],
  UCI_2 = int_conf_rhc_gen[,2,2]
)

# 8.1.2. Validación de la regresión:

predicts_rhc_gen_num <- predict(reg_rhc_gen, newdata = df_prueba, type = "class")
predicts_rhc_gen <- case_when(
  predicts_rhc_gen_num == 0 ~ "No realiza",
  predicts_rhc_gen_num == 1 ~ "No completa" ,
  predicts_rhc_gen_num == 2 ~"Adherente"
)
predicts_rhc_gen <- as.factor(predicts_rhc_gen)
predicts_rhc_gen <- factor(predicts_rhc_gen, levels = c(levels(predicts_rhc_gen), "No completa"))

# 8.1.2.1. Matriz de confusión:

matriz_rhc_gen <- confusionMatrix(df_prueba$adherencia_rhc,predicts_rhc_gen)
tabla_matriz_rhc_gen <- as.data.frame(matriz_rhc_gen$table)
tabla_matriz_rhc_gen$Reference <- factor(tabla_matriz_rhc_gen$Reference, levels = rev(levels(tabla_matriz_rhc_gen$Reference)))

mat_conf_rhc_gen <- ggplot(tabla_matriz_rhc_gen, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_rhc_gen$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la rehabilitación\nToda la cohorte",
       subtitle = paste0("Exactitud: ",round(matriz_rhc_gen$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_rhc_gen$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_rhc_gen$byClass[1],3),
                         "\nF1-score: ", round(matriz_rhc_gen$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_rhc_gen

# 8.1.2.2. Curva ROC:

curva_roc_rhc_gen <- roc (df_prueba$adherencia_rhc_num,as.numeric(predicts_rhc_gen_num))
ggroc(curva_roc_rhc_gen)

# 8.2. Regresión logística grupo1:

reg_rhc_1 <- multinom(adherencia_rhc_num 
                        ~ edad + sexo + vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria, 
                        data = df_entrenamiento_1
)

# 8.2.1 Extracción de los oddratios:

odd_ratio_rhc_1 <- exp(coef(reg_rhc_1))
int_conf_rhc_1 <- exp(confint(reg_rhc_1))
stats_rhc_1 <- data.frame(
  variable = names(odd_ratio_rhc_1[1,]),
  odd_ratio_1 = odd_ratio_rhc_1[1,],
  LCI_1 = int_conf_rhc_1[,1,1],
  UCI_1 = int_conf_rhc_1[,2,1],
  odd_ratio_2 = odd_ratio_rhc_1[2,],
  LCI_2 = int_conf_rhc_1[,1,2],
  UCI_2 = int_conf_rhc_1[,2,2]
)

# 8.2.2. Validación de la regresión:

predicts_rhc_1_num <- predict(reg_rhc_1, newdata = df_prueba_1, type = "class")
predicts_rhc_1 <- case_when(
  predicts_rhc_1_num == 0 ~ "No realiza",
  predicts_rhc_1_num == 1 ~ "No completa" ,
  predicts_rhc_1_num == 2 ~"Adherente"
)
predicts_rhc_1 <- as.factor(predicts_rhc_1)
predicts_rhc_1 <- factor(predicts_rhc_1, levels = c(levels(predicts_rhc_1), "No completa"))

# 8.2.2.1. Matriz de confusión:

matriz_rhc_1 <- confusionMatrix(df_prueba_1$adherencia_rhc,predicts_rhc_1)
tabla_matriz_rhc_1 <- as.data.frame(matriz_rhc_1$table)
tabla_matriz_rhc_1$Reference <- factor(tabla_matriz_rhc_1$Reference, levels = rev(levels(tabla_matriz_rhc_1$Reference)))

mat_conf_rhc_1 <- ggplot(tabla_matriz_rhc_1, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_rhc_1$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la rehabilitación\nGrupo 1",
       subtitle = paste0("Exactitud: ",round(matriz_rhc_1$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_rhc_1$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_rhc_1$byClass[1],3),
                         "\nF1-score: ", round(matriz_rhc_1$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_rhc_1


# 8.2. Regresión logística grupo 1:

reg_rhc_1 <- multinom(adherencia_rhc_num 
                      ~ edad + sexo + vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria, 
                      data = df_entrenamiento_1
)

# 8.2.1 Extracción de los oddratios:

odd_ratio_rhc_1 <- exp(coef(reg_rhc_1))
int_conf_rhc_1 <- exp(confint(reg_rhc_1))
stats_rhc_1 <- data.frame(
  variable = names(odd_ratio_rhc_1[1,]),
  odd_ratio_1 = odd_ratio_rhc_1[1,],
  LCI_1 = int_conf_rhc_1[,1,1],
  UCI_1 = int_conf_rhc_1[,2,1],
  odd_ratio_2 = odd_ratio_rhc_1[2,],
  LCI_2 = int_conf_rhc_1[,1,2],
  UCI_2 = int_conf_rhc_1[,2,2]
)

# 8.2.2. Validación de la regresión:

predicts_rhc_1_num <- predict(reg_rhc_1, newdata = df_prueba_1, type = "class")
predicts_rhc_1 <- case_when(
  predicts_rhc_1_num == 0 ~ "No realiza",
  predicts_rhc_1_num == 1 ~ "No completa" ,
  predicts_rhc_1_num == 2 ~"Adherente"
)
predicts_rhc_1 <- as.factor(predicts_rhc_1)
predicts_rhc_1 <- factor(predicts_rhc_1, levels = c(levels(predicts_rhc_1), "No completa"))

# 8.2.2.1. Matriz de confusión:

matriz_rhc_1 <- confusionMatrix(df_prueba_1$adherencia_rhc,predicts_rhc_1)
tabla_matriz_rhc_1 <- as.data.frame(matriz_rhc_1$table)
tabla_matriz_rhc_1$Reference <- factor(tabla_matriz_rhc_1$Reference, levels = rev(levels(tabla_matriz_rhc_1$Reference)))

mat_conf_rhc_1 <- ggplot(tabla_matriz_rhc_1, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_rhc_1$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la rehabilitación\nGrupo 1",
       subtitle = paste0("Exactitud: ",round(matriz_rhc_1$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_rhc_1$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_rhc_1$byClass[1],3),
                         "\nF1-score: ", round(matriz_rhc_1$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_rhc_1

# 8.3. Regresión logística grupo 2:

reg_rhc_2 <- multinom(adherencia_rhc_num 
                      ~ edad + sexo + vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria, 
                      data = df_entrenamiento_2
)

# 8.3.1 Extracción de los oddratios:

odd_ratio_rhc_2 <- exp(coef(reg_rhc_2))
int_conf_rhc_2 <- exp(confint(reg_rhc_2))
stats_rhc_2 <- data.frame(
  variable = names(odd_ratio_rhc_2),
  odd_ratio_2 = odd_ratio_rhc_2,
  UCI_2 = int_conf_rhc_2[,2],
  LCI_2 = int_conf_rhc_2[,1]
)

# 8.3.2. Validación de la regresión:

predicts_rhc_2_num <- predict(reg_rhc_2, newdata = df_prueba_2, type = "class")
predicts_rhc_2 <- case_when(
  predicts_rhc_2_num == 0 ~ "No realiza",
  predicts_rhc_2_num == 1 ~ "No completa" ,
  predicts_rhc_2_num == 2 ~"Adherente"
)
predicts_rhc_2 <- as.factor(predicts_rhc_2)
predicts_rhc_2 <- factor(predicts_rhc_2, levels = c(levels(predicts_rhc_2), "No completa"))

# 8.3.2.1. Matriz de confusión:

matriz_rhc_2 <- confusionMatrix(df_prueba_2$adherencia_rhc,predicts_rhc_2)
tabla_matriz_rhc_2 <- as.data.frame(matriz_rhc_2$table)
tabla_matriz_rhc_2$Reference <- factor(tabla_matriz_rhc_2$Reference, levels = rev(levels(tabla_matriz_rhc_2$Reference)))

mat_conf_rhc_2 <- ggplot(tabla_matriz_rhc_2, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_rhc_2$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la rehabilitación\nGrupo 2",
       subtitle = paste0("Exactitud: ",round(matriz_rhc_2$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_rhc_2$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_rhc_2$byClass[1],3),
                         "\nF1-score: ", round(matriz_rhc_2$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_rhc_2

print(int_conf_rhc_2)

# 8.4. Regresión logística grupo 3:

reg_rhc_3 <- multinom(adherencia_rhc_num 
                      ~ edad + sexo + vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria, 
                      data = df_entrenamiento_3
)

# 8.4.1 Extracción de los oddratios:

odd_ratio_rhc_3 <- exp(coef(reg_rhc_3))
int_conf_rhc_3 <- exp(confint(reg_rhc_3))
stats_rhc_3 <- data.frame(
  variable = names(odd_ratio_rhc_3[1,]),
  odd_ratio_1 = odd_ratio_rhc_3[1,],
  LCI_1 = int_conf_rhc_3[,1,1],
  UCI_1 = int_conf_rhc_3[,2,1],
  odd_ratio_2 = odd_ratio_rhc_3[2,],
  LCI_2 = int_conf_rhc_3[,1,2],
  UCI_2 = int_conf_rhc_3[,2,2]
)

# 8.4.2. Validación de la regresión:

predicts_rhc_3_num <- predict(reg_rhc_3, newdata = df_prueba_3, type = "class")
predicts_rhc_3 <- case_when(
  predicts_rhc_3_num == 0 ~ "No realiza",
  predicts_rhc_3_num == 1 ~ "No completa" ,
  predicts_rhc_3_num == 2 ~"Adherente"
)
predicts_rhc_3 <- as.factor(predicts_rhc_3)
 predicts_rhc_3 <- factor(predicts_rhc_3, levels = c(levels(predicts_rhc_3), "No completa"))

# 8.4.2.1. Matriz de confusión:

matriz_rhc_3 <- confusionMatrix(df_prueba_3$adherencia_rhc,predicts_rhc_3)
tabla_matriz_rhc_3 <- as.data.frame(matriz_rhc_3$table)
tabla_matriz_rhc_3$Reference <- factor(tabla_matriz_rhc_3$Reference, levels = rev(levels(tabla_matriz_rhc_3$Reference)))

mat_conf_rhc_3 <- ggplot(tabla_matriz_rhc_3, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_rhc_3$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la rehabilitación\nGrupo 3",
       subtitle = paste0("Exactitud: ",round(matriz_rhc_3$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_rhc_3$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_rhc_3$byClass[1],3),
                         "\nF1-score: ", round(matriz_rhc_3$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_rhc_3

# 8.5. Regresión logística grupo 4:

reg_rhc_4 <- multinom(adherencia_rhc_num 
                      ~ edad + sexo + vasos_afectados_imp + tipo_de_sca_imp + killip_imp + fevi + enfermedad_coronaria, 
                      data = df_entrenamiento_4
)

# 8.5.1 Extracción de los oddratios:

odd_ratio_rhc_4 <- exp(coef(reg_rhc_4))
int_conf_rhc_4 <- exp(confint(reg_rhc_4))
stats_rhc_4 <- data.frame(
  variable = names(odd_ratio_rhc_4[1,]),
  odd_ratio_1 = odd_ratio_rhc_4[1,],
  LCI_1 = int_conf_rhc_4[,1,1],
  UCI_1 = int_conf_rhc_4[,2,1],
  odd_ratio_2 = odd_ratio_rhc_4[2,],
  LCI_2 = int_conf_rhc_4[,1,2],
  UCI_2 = int_conf_rhc_4[,2,2]
)

# 8.5.2. Validación de la regresión:

predicts_rhc_4_num <- predict(reg_rhc_4, newdata = df_prueba_4, type = "class")
predicts_rhc_4 <- case_when(
  predicts_rhc_4_num == 0 ~ "No realiza",
  predicts_rhc_4_num == 1 ~ "No completa" ,
  predicts_rhc_4_num == 2 ~"Adherente"
)
predicts_rhc_4 <- as.factor(predicts_rhc_4)
predicts_rhc_4 <- factor(predicts_rhc_4, levels = c(levels(predicts_rhc_4), "No completa"))

# 8.5.2.1. Matriz de confusión:

matriz_rhc_4 <- confusionMatrix(df_prueba_4$adherencia_rhc,predicts_rhc_4)
tabla_matriz_rhc_4 <- as.data.frame(matriz_rhc_4$table)
tabla_matriz_rhc_4$Reference <- factor(tabla_matriz_rhc_4$Reference, levels = rev(levels(tabla_matriz_rhc_4$Reference)))

mat_conf_rhc_4 <- ggplot(tabla_matriz_rhc_4, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(label = tabla_matriz_rhc_4$Freq, color = "black", size = 10) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Matriz de Confusión\nAdherencia a la rehabilitación\nGrupo 4",
       subtitle = paste0("Exactitud: ",round(matriz_rhc_4$overall[1],3),
                         "\nEspecifiedad: ",round(matriz_rhc_4$byClass[2],3),
                         "\nSensibilidad: ",round(matriz_rhc_4$byClass[1],3),
                         "\nF1-score: ", round(matriz_rhc_4$byClass[6],3)),
       x = "Clase predicha",
       y = "Clase real") +
  theme_minimal() +
  scale_x_discrete(position = "top" ) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
        plot.subtitle = element_text(size = 15)
  )

mat_conf_rhc_4

mat_conf_rhc_4

# 9. Extracción de los oddratios en una tabla:

df_odd_mmas <- merge(stats_mmas,stats_mmas_1, by = "variable", all = TRUE)
df_odd_mmas <- merge(df_odd_mmas,stats_mmas_3, by = "variable", all = TRUE)
df_odd_mmas <- merge(df_odd_mmas,stats_mmas_4, by = "variable", all = TRUE)
write_xlsx(df_odd_mmas, "Tabla_oddratio_mmas.xlsx")

df_odd_medas <- merge(stats_medas,stats_medas_1, by = "variable", all = TRUE)
df_odd_medas <- merge(df_odd_medas,stats_medas_2, by = "variable", all = TRUE)
df_odd_medas <- merge(df_odd_medas,stats_medas_3, by = "variable", all = TRUE)
df_odd_medas <- merge(df_odd_medas,stats_medas_4, by = "variable", all = TRUE)
write_xlsx(df_odd_medas, "Tabla_oddratio_meda.xlsx")

df_odd_ipaq <- merge(stats_ipaq,stats_ipaq_1, by = "variable", all = TRUE)
df_odd_ipaq <- merge(df_odd_ipaq,stats_ipaq_2, by = "variable", all = TRUE)
df_odd_ipaq <- merge(df_odd_ipaq,stats_ipaq_3, by = "variable", all = TRUE)
df_odd_ipaq <- merge(df_odd_ipaq,stats_ipaq_4, by = "variable", all = TRUE)
write_xlsx(df_odd_ipaq, "Tabla_oddratio_ipaq.xlsx")

df_odd_total <- merge(stats_total,stats_total_1, by = "variable", all = TRUE)
df_odd_total <- merge(df_odd_total,stats_total_3, by = "variable", all = TRUE)
df_odd_total <- merge(df_odd_total,stats_total_4, by = "variable", all = TRUE)
write_xlsx(df_odd_total, "Tabla_oddratio_total.xlsx")

df_odd_rhc <- merge(stats_rhc_gen,stats_rhc_1, by = "variable", all = TRUE)
df_odd_rhc <- merge(df_odd_rhc,stats_rhc_2, by = "variable", all = TRUE)
df_odd_rhc <- merge(df_odd_rhc,stats_rhc_3, by = "variable", all = TRUE)
df_odd_rhc <- merge(df_odd_rhc,stats_rhc_4, by = "variable", all = TRUE)
write_xlsx(df_odd_rhc, "Tabla_oddratio_rhc.xlsx")

mat
