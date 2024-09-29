# 1. Cargamos librerías:

library(writexl)
library(factoextra)
library(haven)
library(tidyverse)
library(skimr)
library(gtsummary)
library(knitr)
library(compareGroups) 
library(factoextra)
library(recipes)
library(forcats)
library(dplyr)
library(survival)
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
#library(ggstatsplot)
library(mice)
library(pROC)
library (yardstick)
library(ggplot2)
library(reshape) 
library(Hmisc)
library(MASS)
library(pec)
library(tidyr)
library(RColorBrewer)


# 2. Cargamos las bases de datos:
# Se han seleccionado k-prototypes con k = 3, jerárquico con k = 4, y jerárquico con k = 6
# Los denominaremos kproto_3, jerar_4 y jerar6 respectivamente

df_in <- read_excel("C:\\Users\\Vza 12\\Desktop\\UPM\\TFG\\Clustering_7\\Tablas\\Base Clustering.xls")
df_kproto_3 <- read_excel("C:\\Users\\Vza 12\\Desktop\\UPM\\TFG\\Clustering_7\\Tablas\\Resultados_clustering\\kprototypes\\kproto_7_3.xlsx")
df_jerar_4 <- read_excel("C:\\Users\\Vza 12\\Desktop\\UPM\\TFG\\Clustering_7\\Tablas\\Resultados_clustering\\jerarquico\\jerar_4.xlsx")
df_jerar_6 <- read_excel("C:\\Users\\Vza 12\\Desktop\\UPM\\TFG\\Clustering_7\\Tablas\\Resultados_clustering\\jerarquico\\jerar_6.xlsx")

# 2.1.Factorización de variables:

df_in$hta <- as.factor(df_in$hta)
df_in$alcoholismo <- as.factor(df_in$alcoholismo)
df_in$hiperlipidemia <- as.factor(df_in$hiperlipidemia)
df_in$prior_pci <- as.factor(df_in$prior_pci)
df_in$fa <- as.factor(df_in$fa)
df_in$fv <- as.factor(df_in$fv)
df_in$ansiedad <- as.factor(df_in$ansiedad)
df_in$depresion <- as.factor(df_in$depresion)
df_in$comorbilidades_psiquiatricas <- as.factor(df_in$comorbilidades_psiquiatricas)
df_in$enfermedad_coronaria <- as.factor(df_in$enfermedad_coronaria)
df_in$charlson_clasificacion3 <- as.factor(df_in$charlson_clasificacion3)
df_in$sexo <- as.factor(df_in$sexo)
df_in$etnia <- as.factor(df_in$etnia)
df_in$estado_civil <- as.factor(df_in$estado_civil)
df_in$estatus_socioecon_mico <- as.factor(df_in$estatus_socioecon_mico)
df_in$ingresos <- as.factor(df_in$ingresos)
df_in$responsable_econ_mico_del <- as.factor(df_in$responsable_econ_mico_del)
df_in$cobertura_sanitaria <- as.factor(df_in$cobertura_sanitaria)
df_in$tipo_de_vivienda <-  as.factor(df_in$tipo_de_vivienda)
df_in$situaci_n_laboral <- as.factor(df_in$situaci_n_laboral)
df_in$tipo_de_trabajo <- as.factor(df_in$tipo_de_trabajo)
df_in$tipo_de_contrato <- as.factor(df_in$tipo_de_contrato)
df_in$nivel_educacional <- as.factor(df_in$nivel_educacional)
df_in$presencia_de_cuidadores <- as.factor(df_in$presencia_de_cuidadores)
df_in$tomas_diarias <- as.factor(df_in$tomas_diarias)
df_in$farmacos_inyectables <- as.factor(df_in$farmacos_inyectables)
df_in$dudas_medicacion <- as.factor(df_in$dudas_medicacion)
df_in$antecedentes_dificultades <- as.factor(df_in$antecedentes_dificultades)
df_in$informacion_enfermedad <- as.factor(df_in$informacion_enfermedad)
df_in$informacion_terapia <- as.factor(df_in$informacion_terapia)
df_in$control_terapia <- as.factor(df_in$control_terapia)
df_in$satisfaccion_hospitalizacion <- as.factor(df_in$satisfaccion_hospitalizacion)
df_in$infarto_de_miocardio <- as.factor(df_in$infarto_de_miocardio)
df_in$insuficiencia_cardiaca_con <- as.factor(df_in$insuficiencia_cardiaca_con)
df_in$enfermedad_arterial_perif <- as.factor(df_in$enfermedad_arterial_perif)
df_in$enfermedad_cerebrovascular <- as.factor(df_in$enfermedad_cerebrovascular)
df_in$diabetes_mellitus_sin_evid <- as.factor(df_in$diabetes_mellitus_sin_evid)
df_in$diabetes_con_afectacion_de <- as.factor(df_in$diabetes_con_afectacion_de)
df_in$enfermedad_renal_cronica_m <- as.factor(df_in$enfermedad_renal_cronica_m)
df_in$nacionalidad_final <- as.factor(df_in$nacionalidad_final)
df_in$fumador_ex <- as.factor(df_in$fumador_ex)
df_in$estado_civil_imp <- as.factor(df_in$estado_civil)
df_in$medlife_adherencia2_imp <- as.factor(df_in$medlife_adherencia2_imp)
df_in$vasos_afectados_imp <- as.factor(df_in$vasos_afectados_imp)
df_in$tipo_de_sca_imp <- as.factor(df_in$tipo_de_sca_imp)
df_in$killip_imp <- as.factor(df_in$killip_imp)
df_in$sahlsa50_clasi_imp <- as.factor(df_in$sahlsa50_clasi_imp)
df_in$hads_clasi_ansi_imp <- as.factor(df_in$hads_clasi_ansi_imp)
df_in$hads_clasi_dep_imp <- as.factor(df_in$hads_clasi_dep_imp)
df_in$barthel_inde_imp <- as.factor(df_in$barthel_inde_imp)
df_in$estatus_2 <- as.factor(df_in$estatus_2)
df_in$Hospitalización <- as.factor(df_in$Hospitalización)
df_in$Hospitalizacióncardiovascular <- as.factor(df_in$Hospitalizacióncardiovascular)
df_in$MuerteHospitalización <- as.factor(df_in$MuerteHospitalización)
df_in$iam <- as.factor(df_in$iam)
df_in$ictus <- as.factor(df_in$ictus)
df_in$ic <- as.factor(df_in$ic)
df_in$mmas_adh_12m__01 <- as.factor(df_in$mmas_adh_12m__01)
df_in$medas_adherencia_12m_imp <- as.factor(df_in$medas_adherencia_12m_imp)
df_in$ipaq_adhe_12m_imp <- as.factor(df_in$ipaq_adhe_12m_imp)
df_in$adh_total_12m_imp <- as.factor(df_in$adh_total_12m_imp)
df_in$adherencia_rhc <- as.factor(df_in$adherencia_rhc)

df_in <- df_in %>%
  dplyr:: rename(
    "Neuroticismo" = neoffi_neuro1_imp,
    "Extroversión" = neoffi_extr1_imp,
    "Apertura a la experiencia" = neoffi_apertu1_imp,
    "Amabilidad" = neo_cor1_imp,
    "Responsabilidad" = neoffi_respo1_imp,
    "Percepción de amenaza de la enfermedad" = bipq_total1_imp,
    "Autoeficacia" = autoeficacia_total1_imp,
    "Estrés" = pss_total1_imp,
    "Afecto positivo" = panas_positiva_total1_imp,
    "Afecto negativo" = panas_negativa_total1_imp,
    "Calidad de vida mental" = sf12_me_imp,
    "Ansiedad (cualitativa)" = hads_clasi_ansi_imp,
    "Depresión (cualitativa)" = hads_clasi_ansi_imp,
    "Afrontamiento cognitivo" = afrontamiento_cognitivo1_imp,
    "Afrontamiento social" = afrontamiento_social1_imp,
    "Bloqueo del afrontamiento" = bloqueo_afrontamiento1_imp,
    "Afrontamiento espiritual" = afrontamiento_espiritual1_imp,
    "Nº vasos afectados" = vasos_afectados_imp,
    "Tipo de SCA" = tipo_de_sca_imp,
    "Killip" = killip_imp,
    "FEVI" = fevi,
    "Enfermedad coronaria" = enfermedad_coronaria,
    "IAN previo" = infarto_de_miocardio,
    "IC" = insuficiencia_cardiaca_con,
    "Enfermedad arterial" = enfermedad_arterial_perif,
    "ACV" = enfermedad_cerebrovascular,
    "DM" = diabetes_mellitus_sin_evid,
    "ERC" = enfermedad_renal_cronica_m,
    "Charlson" = charlson_clasificacion3,
    "FA" = fa,
    "Ansiedad" = ansiedad,
    "Depresión" = depresion,
    "Otras comorbilidades psiquiátricas" = comorbilidades_psiquiatricas,
    "Hipertensión" = hta,
    "Dislipemia" = hiperlipidemia,
    "Tabaquismo" = fumador_ex,
    "Medlife" = medlife_total1_imp,
    "Alcoholismo" = alcoholismo,
    "Peso" = peso_imp,
    "IMC" = imc_imp,
    "Colesterol HDL" = hdl_imp,
    "Colesterol LDL" = ldl_imp,
    "Hba1c" = hba1c_imp,
    "PAS" = pas,
    "PAD" = pad,
    "FC" = fc,
    "Calidad de vida física" = sf12_fi_imp,
    "Barthel" =barthel_inde_imp,
    "Sexo" = sexo,
    "Edad" = edad,
    "Etnia" = etnia,
    "Tipo de trabajo" = tipo_de_trabajo,
    "Tipo de contrato" = tipo_de_contrato,
    "Nivel educacional" = nivel_educacional,
    "Estado civil" = estado_civil_imp,
    "Apoyo social amigos" = mspss_amigos1_imp,
    "Apoyo social familia" = mspss_familia1_imp,
    "Apoyo social relevantes" = mspss_relevantes1_imp,
    "Alfabetización" = sahlsa50_total_imp,
    "Autocuidados" = asa_total1_imp,
    "Percepción enfermedad" = bipq_total1_imp,
    "Creencias necesidad fármacos" = bmq_esp_necesidad1_imp,
    "Preocupación uso fármacos" = bmq_esp_preocupacion1_imp,
    "Creencias abuso fármacos" = bmq_general_abuso1_imp,
    "Creencias daño fármacos" = bmq_general_da_o1_imp,
    "Adherencia a la medicación" = mmas_adh_12m__01,
    "Adherencia a la dieta mediterránea" = medas_adherencia_12m_imp,
    "Adherencia a la actividad física" = ipaq_adhe_12m_imp,
    "Adherencia a la rehabilitación" = adherencia_rhc,
    "Adherencia total" = adh_total_12m_imp,
    "Nº cigarrillos diarios" = cantidad_tabaco,
    "FV" = fv,
    "Estatus socioeconómico" = estatus_socioecon_mico,
    "Ingresos" = ingresos,
    "Responsable económico" = responsable_econ_mico_del,
    "Cobertura sanitaria" = cobertura_sanitaria,
    "Tipo de vivienda" = tipo_de_vivienda,
    "Distancia al hospital" = distancia_al_hospital,
    "Situación laboral" = situaci_n_laboral,
    "Presencia de cuidadores" = presencia_de_cuidadores,
    "Tomas diarias" = tomas_diarias,
    "Fármacos inyectables" = farmacos_inyectables,
    "Dudas medicación" = dudas_medicacion,
    "Antecedentes de dificultades" = antecedentes_dificultades,
    "Información de la enfermedad" = informacion_enfermedad,
    "Información de la terapia" = informacion_terapia,
    "Control de la terapia" = control_terapia,
    "Días de hospitalización" = dias_hospitalizacion,
    "Satisfacción hospitalización" = satisfaccion_hospitalizacion,
    "ID" = record_id,
    "Colesterol total" = colesterol_imp,
    "Alfabetización (categórica)" = sahlsa50_clasi_imp,
    "Ansiedad (escala)" = hads_clasi_ansi_imp,
    "Barthel (escala" = barthel_total_imp,
    "Número de urgencias" = num_urg,
    "Número de hospitalizaciones" = num_hosp,
    "Número de infartos" = num_infartos,
    "Número de ictus" = num_ictus,
    "Número de IC" = num_ic,
    "Estado" = estatus_2,
    "Seguimiento" = seguimientoaños,
    "IAM posterior (evento)" = iam,
    "IAM posterior (días)" = seguimientoiam,
    "Hospitalización (evento)" = Hospitalización,
    "Hospitalización (días)" = Seguimientohospitalización,
    "Hospitalización cardiovascular (evento)" = Hospitalizacióncardiovascular,
    "Hospitalización cardiovascular (días)" = Seguimientohospitalizacióncardio,
    "Ictus (evento)" = ictus,
    "Ictus (días)" = seguimientoictus,
    "Muerte-hospitalización (evento)" = MuerteHospitalización,
    "Muerte-hospitalización (días)" = seguimientomh,
    "IC (evento)" = ic,
    "IC (días)" = seguimientoic
  )
df_in <- df_in %>%
  dplyr::select(-cuidados_1,-cuidados_2,-cuidados_3,-cuidados_4,-cuidados_5,-cuidados_6,-cuidados_7,-cuidados_9,-cuidados_10,-cuidados_11)

# 2.2. Unimos la columna cluster con la base de datos inicial:

col_kproto_3 <- df_kproto_3 %>% dplyr::select(kproto_clusters_3)
col_jerar_4 <- df_jerar_4 %>% dplyr::select(jer_4_cluster)
col_jerar_6 <- df_jerar_6 %>% dplyr::select(jer_6_cluster)

col_kproto_3$kproto_clusters_3 <- as.factor(col_kproto_3$kproto_clusters_3)
col_jerar_4$jer_4_cluster <- as.factor(col_jerar_4$jer_4_cluster)
col_jerar_6$jer_6_cluster <- as.factor(col_jerar_6$jer_6_cluster)

col_kproto_3 <- col_kproto_3 %>% dplyr:: rename (cluster = kproto_clusters_3)
col_jerar_4 <- col_jerar_4 %>% dplyr:: rename (cluster = jer_4_cluster)
col_jerar_6 <- col_jerar_6 %>% dplyr:: rename (cluster = jer_6_cluster)

tabla_kproto_3 <- cbind(df_in,col_kproto_3)
tabla_jerar_4 <- cbind(df_in,col_jerar_4)
tabla_jerar_6 <- cbind(df_in,col_jerar_6)

# 3. Análisis de las variables empleadas para el proceso de clustering:

# 3.1. Gráfico de barras de la variable sexo:

# 3.1.1. Gráfico kproto_3:

# 3.1.1.1. Obtención del dataframe:

df_sexo_kproto_3  <- tabla_kproto_3 %>%
  group_by( cluster,Sexo) %>%
  summarise(count = n()) %>%
  mutate(total_sexo = sum(count)) %>%
  mutate(porcentaje_sexo = count / total_sexo * 100) 

# 3.1.1.2. Obtención de la gráfica:

graf_sexo_kproto_3 <- ggplot(df_sexo_kproto_3, aes(x = Sexo, y = porcentaje_sexo, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_sexo_kproto_3$cluster)), name = "Blues"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 83)) +
  geom_text(aes(label = paste0(round(porcentaje_sexo, digits=2),"%\n",count)),
            position = position_dodge(width = 1),
            vjust = 1.5,
            color = "black",
            size = 5,
            fontface = "bold") +
  labs(title = "Distribución de sexo por clúster \n kproto_3",
       x = "Sexo",
       y = "Porcentaje (%)",
       fill = "Sexo") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #theme_minimal() +
  theme(text = element_text(size=12))

graf_sexo_kproto_3

# 3.1.2. Gráfica jerar_4

# 3.1.2.1. Obtención del dataframe:

df_sexo_jerar_4  <- tabla_jerar_4 %>%
  group_by( cluster,Sexo) %>%
  summarise(count = n()) %>%
  mutate(total_sexo = sum(count)) %>%
  mutate(porcentaje_sexo = count / total_sexo * 100) 

# 3.1.2.2. Obtención de la gráfica:

graf_sexo_jerar_4 <- ggplot(df_sexo_jerar_4, aes(x = Sexo, y = porcentaje_sexo, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_sexo_jerar_4$cluster)), name = "Blues"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 91)) +
  geom_text(aes(label = paste0(round(porcentaje_sexo, digits=2),"%\n",count)),
            position = position_dodge(width = 1),
            vjust = 1.5,
            color = "black",
            size = 4,
            fontface = "bold") +
  labs(title = "Distribución de sexo por clúster \n jerar_4",
       x = "Sexo",
       y = "Porcentaje (%)",
       fill = "Sexo") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #theme_minimal() +
  theme(text = element_text(size=12))

graf_sexo_jerar_4

# 3.1.3. Gráfica jerar_6

# 3.1.3.1. Obtención del dataframe:

df_sexo_jerar_6  <- tabla_jerar_6 %>%
  group_by( cluster,Sexo) %>%
  summarise(count = n()) %>%
  mutate(total_sexo = sum(count)) %>%
  mutate(porcentaje_sexo = count / total_sexo * 100) 

# 3.1.3.2. Obtención de la gráfica:

graf_sexo_jerar_6 <- ggplot(df_sexo_jerar_6, aes(x = Sexo, y = porcentaje_sexo, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_sexo_jerar_6$cluster)), name = "Blues"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_sexo, digits=2),"%\n",count)),
            position = position_dodge(width = 1),
            vjust = 1.5,
            color = "black",
            size = 4,
            fontface = "bold") +
  labs(title = "Distribución de sexo por clúster \n jerar_6",
       x = "Sexo",
       y = "Porcentaje (%)",
       fill = "Sexo") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #theme_minimal() +
  theme(text = element_text(size=12))

graf_sexo_jerar_6

# 3.2. Gráfico de barras de la variable killip:

# 3.2.1. Gráfico kproto_3:

# 3.2.1.1. Obtención del dataframe:

df_killip_kproto_3  <- tabla_kproto_3 %>%
  group_by( cluster,Killip) %>%
  summarise(count = n()) %>%
  mutate(total_killip = sum(count)) %>%
  mutate(porcentaje_killip = count / total_killip * 100) 

# 3.2.1.2. Obtención de la gráfica:

graf_killip_kproto_3 <- ggplot(df_killip_kproto_3, aes(x = Killip, y = porcentaje_killip, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_killip_kproto_3$cluster)), name = "Greens"),
                    name = "Cluster") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_killip, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 12,
            fontface = "bold") +
  labs(title = "",
       x = "Killip",
       y = "Percentage (%)",
       fill = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 60, face = "bold"))

graf_killip_kproto_3

# 3.2.2. Gráfico jerar_4:

# 3.2.2.1. Obtención del dataframe:

df_killip_jerar_4  <- tabla_jerar_4 %>%
  group_by( cluster,Killip) %>%
  summarise(count = n()) %>%
  mutate(total_killip = sum(count)) %>%
  mutate(porcentaje_killip = count / total_killip * 100) 

# 3.2.2.2. Obtención de la gráfica:

graf_killip_jerar_4 <- ggplot(df_killip_jerar_4, aes(x = Killip, y = porcentaje_killip, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_killip_jerar_4$cluster)), name = "Greens"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_killip, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 10,
            fontface = "bold") +
  labs(title = "",
       x = "Killip",
       y = "Porcentaje (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 60, face = "bold"))

graf_killip_jerar_4

# 3.2.2. Gráfico jerar_4:

# 3.2.2.1. Obtención del dataframe:

df_killip_jerar_6  <- tabla_jerar_6 %>%
  group_by( cluster,Killip) %>%
  summarise(count = n()) %>%
  mutate(total_killip = sum(count)) %>%
  mutate(porcentaje_killip = count / total_killip * 100) 

# 3.2.2.2. Obtención de la gráfica:

graf_killip_jerar_6 <- ggplot(df_killip_jerar_6, aes(x = Killip, y = porcentaje_killip, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_killip_jerar_6$cluster)), name = "Greens"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_killip, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 7,
            fontface = "bold") +
  labs(title = "",
       x = "Killip",
       y = "Porcentaje (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 60, face = "bold"))

graf_killip_jerar_6

# 3.3. Gráfico de barras de la variable número de vasos afectados:

# 3.3.1. Gráfico kproto_3:

# 3.3.1.1. Obtención del dataframe:

df_n_vasos_kproto_3  <- tabla_kproto_3 %>%
  group_by( cluster, `Nº vasos afectados`) %>%
  summarise(count = n()) %>%
  mutate(total_n_vasos = sum(count)) %>%
  mutate(porcentaje_n_vasos = count / total_n_vasos * 100) 

# 3.3.1.2. Obtención de la gráfica:

df_n_vasos_kproto_3$`Nº vasos afectados` <- factor(df_n_vasos_kproto_3$`Nº vasos afectados`, levels = c("1", "2","3 o más"), labels = c("1", "2", "3 or more"))

graf_n_vasos_kproto_3 <- ggplot(df_n_vasos_kproto_3, aes(x = `Nº vasos afectados`, y = porcentaje_n_vasos, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_n_vasos_kproto_3$cluster)), name = "Reds"),
                    name = "Cluster") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_n_vasos, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 16,
            fontface = "bold") +
  labs(title = "",
       x = "Number of vessels affected",
       y = "Percentage (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(size = 50, face = "bold"))

graf_n_vasos_kproto_3

# 3.3.2. Gráfico jerar_4:

# 3.3.2.1. Obtención del dataframe:

df_n_vasos_jerar_4  <- tabla_jerar_4 %>%
  group_by( cluster, `Nº vasos afectados`) %>%
  summarise(count = n()) %>%
  mutate(total_n_vasos = sum(count)) %>%
  mutate(porcentaje_n_vasos = count / total_n_vasos * 100) 

# 3.3.2.2. Obtención de la gráfica:

graf_n_vasos_jerar_4 <- ggplot(df_n_vasos_jerar_4, aes(x = `Nº vasos afectados`, y = porcentaje_n_vasos, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_n_vasos_jerar_4$cluster)), name = "Reds"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_n_vasos, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 12,
            fontface = "bold") +
  labs(title = "",
       x = "Nº vasos afectados",
       y = "Porcentaje (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(size = 50, face = "bold"))

graf_n_vasos_jerar_4

# 3.3.3. Gráfico jerar_6:

# 3.3.3.1. Obtención del dataframe:

df_n_vasos_jerar_6  <- tabla_jerar_6 %>%
  group_by( cluster, `Nº vasos afectados`) %>%
  summarise(count = n()) %>%
  mutate(total_n_vasos = sum(count)) %>%
  mutate(porcentaje_n_vasos = count / total_n_vasos * 100) 

# 3.3.3.2. Obtención de la gráfica:

graf_n_vasos_jerar_6 <- ggplot(df_n_vasos_jerar_6, aes(x = `Nº vasos afectados`, y = porcentaje_n_vasos, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_n_vasos_jerar_6$cluster)), name = "Reds"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_n_vasos, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 8,
            fontface = "bold") +
  labs(title = "",
       x = "Nº vasos afectados",
       y = "Porcentaje (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(size = 50, face = "bold"))

graf_n_vasos_jerar_6

# 3.4. Gráfico de barras de la variable tipo de SCA:

# 3.4.1. Gráfico kproto_3:

# 3.4.1.1. Obtención del dataframe:

df_tipo_de_sca_kproto_3  <- tabla_kproto_3 %>%
  group_by( cluster,`Tipo de SCA`) %>%
  summarise(count = n()) %>%
  mutate(total_tipo_de_sca = sum(count)) %>%
  mutate(porcentaje_tipo_de_sca = count / total_tipo_de_sca * 100) 

# 3.4.1.2. Obtención de la gráfica:

df_tipo_de_sca_kproto_3$`Tipo de SCA` <- factor(df_tipo_de_sca_kproto_3$`Tipo de SCA`, levels = c("SCACEST", "SCASEST2"), labels = c("STEMI", "NSTEMI"))

graf_tipo_de_sca_kproto_3 <- ggplot(df_tipo_de_sca_kproto_3, aes(x = `Tipo de SCA`, y = porcentaje_tipo_de_sca, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_tipo_de_sca_kproto_3$cluster)), name = "Purples"),
                    name = "Cluster") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_tipo_de_sca, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 16,
            fontface = "bold") +
  labs(title = "",
       x = "Type of ACS",
       y = "Percentage (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 60, face = "bold"))

graf_tipo_de_sca_kproto_3

# 3.4.2. Gráfico jerar_4:

# 3.4.2.1. Obtención del dataframe:

df_tipo_de_sca_jerar_4  <- tabla_jerar_4 %>%
  group_by( cluster,`Tipo de SCA`) %>%
  summarise(count = n()) %>%
  mutate(total_tipo_de_sca = sum(count)) %>%
  mutate(porcentaje_tipo_de_sca = count / total_tipo_de_sca * 100) 

# 3.4.2.2. Obtención de la gráfica:

graf_tipo_de_sca_jerar_4 <- ggplot(df_tipo_de_sca_jerar_4, aes(x = `Tipo de SCA`, y = porcentaje_tipo_de_sca, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_tipo_de_sca_jerar_4$cluster)), name = "Purples"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_tipo_de_sca, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 16,
            fontface = "bold") +
  labs(title = "",
       x = "Tipo de SCA",
       y = "Porcentaje (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 60, face = "bold"))

graf_tipo_de_sca_jerar_4

# 3.4.3. Gráfico jerar_6:

# 3.4.3.1. Obtención del dataframe:

df_tipo_de_sca_jerar_6  <- tabla_jerar_6 %>%
  group_by( cluster,`Tipo de SCA`) %>%
  summarise(count = n()) %>%
  mutate(total_tipo_de_sca = sum(count)) %>%
  mutate(porcentaje_tipo_de_sca = count / total_tipo_de_sca * 100) 

# 3.4.3.2. Obtención de la gráfica:

graf_tipo_de_sca_jerar_6 <- ggplot(df_tipo_de_sca_jerar_6, aes(x = `Tipo de SCA`, y = porcentaje_tipo_de_sca, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_tipo_de_sca_jerar_6$cluster)), name = "Purples"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_tipo_de_sca, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 12,
            fontface = "bold") +
  labs(title = "",
       x = "Tipo de SCA",
       y = "Porcentaje (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 60, face = "bold"))

graf_tipo_de_sca_jerar_6

# 3.5. Gráfico de barras de la variable enfermedad coronaria:

# 3.5.1. Gráfico kproto_3:

# 3.5.1.1. Obtención del dataframe:

df_ec_kproto_3  <- tabla_kproto_3 %>%
  group_by( cluster,`Enfermedad coronaria`) %>%
  summarise(count = n()) %>%
  mutate(total_ec = sum(count)) %>%
  mutate(porcentaje_ec = count / total_ec * 100) 

# 3.5.1.2. Obtención de la gráfica:
df_ec_kproto_3$`Enfermedad coronaria` <- factor(df_ec_kproto_3$`Enfermedad coronaria`, levels = c("Sí", "No"), labels = c("Yes", "No"))


graf_ec_kproto_3 <- ggplot(df_ec_kproto_3, aes(x = `Enfermedad coronaria`, y = porcentaje_ec, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_ec_kproto_3$cluster)), name = "Oranges"),
                    name = "Cluster") +
  scale_y_continuous(limits = c(0, 108)) +
  geom_text(aes(label = paste0(round(porcentaje_ec, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 16,
            fontface = "bold") +
  labs(title = "",
       x = "Coronary heart disease",
       y = "Percentage (%)",
       fill = "Enfermedad coronaria") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 60, face = "bold"))

graf_ec_kproto_3

# 3.5.2. Gráfico jerar_4:

# 3.5.1. Obtención del dataframe:

df_ec_jerar_4  <- tabla_jerar_4 %>%
  group_by( cluster,`Enfermedad coronaria`) %>%
  summarise(count = n()) %>%
  mutate(total_ec = sum(count)) %>%
  mutate(porcentaje_ec = count / total_ec * 100) 

# 3.5.2.2. Obtención de la gráfica:

graf_ec_jerar_4 <- ggplot(df_ec_jerar_4, aes(x = `Enfermedad coronaria`, y = porcentaje_ec, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_ec_jerar_4$cluster)), name = "Oranges"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_ec, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 14,
            fontface = "bold") +
  labs(title = "",
       x = "Enfermedad coronaria",
       y = "Porcentaje (%)",
       fill = "Enfermedad coronaria") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 60, face = "bold"))

graf_ec_jerar_4

# 3.5.3. Gráfico jerar_6:

# 3.5.3.1. Obtención del dataframe:

df_ec_jerar_6  <- tabla_jerar_6 %>%
  group_by( cluster,`Enfermedad coronaria`) %>%
  summarise(count = n()) %>%
  mutate(total_ec = sum(count)) %>%
  mutate(porcentaje_ec = count / total_ec * 100) 

# 3.5.3.2. Obtención de la gráfica:

graf_ec_jerar_6 <- ggplot(df_ec_jerar_6, aes(x = `Enfermedad coronaria`, y = porcentaje_ec, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_ec_jerar_6$cluster)), name = "Oranges"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_ec, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 10,
            fontface = "bold") +
  labs(title = "",
       x = "Enfermedad coronaria",
       y = "Porcentaje (%)",
       fill = "Enfermedad coronaria") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 60, face = "bold"))

graf_ec_jerar_6

# 3.6. Gráfico de barras de la variable edad:

# 3.6.1. Gráfico kproto_3:

# 3.6.1.1. Obtención del dataframe:

tabla_kproto_3_edad <- tabla_kproto_3
cortes_edad <- c(20,30,40,50,60,70,80,100)
clase_edad <- c("20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79","80+")
tabla_kproto_3_edad$Edad<-cut(tabla_kproto_3_edad$Edad,breaks = cortes_edad,
                  right = FALSE,
                  labels = clase_edad)

df_edad_kproto_3 <- tabla_kproto_3_edad %>%
  group_by( cluster, Edad) %>%
  summarise(count = n()) %>%
  mutate(total_edad = sum(count)) %>%
  mutate(porcentaje_edad = count / total_edad * 100) 

media_edad_kproto_3 <- aggregate(Edad ~ cluster, data = tabla_kproto_3, mean)
d_tipica_edad_kproto_3 <- aggregate(Edad ~ cluster, data = tabla_kproto_3, sd)

# 3.6.1.2. Obtención de la gráfica:

graf_ed_kproto_3 <- ggplot(df_edad_kproto_3, aes(x = df_edad_kproto_3$Edad, y = porcentaje_edad, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_edad_kproto_3$cluster)), name = "Pastel1"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 44)) +
  geom_text(aes(label = paste0(round(porcentaje_edad, digits=2),"%\n",count)),
            position = position_dodge(width = 1),
            vjust = 1.1,
            color = "black",
            size = 4,
            fontface = "bold") +
  labs(title = "Distribución de edad por clúster \n kproto_3",
       subtitle = paste0("Media grupo 1: ",round(media_edad_kproto_3$Edad[1],2), "   ",
                         "Media grupo 2: ",round(media_edad_kproto_3$Edad[2],2), "   ",
                         "Media grupo 3: ",round(media_edad_kproto_3$Edad[3],2), 
                         "\nDesviación típica grupo 1: ",round(d_tipica_edad_kproto_3$Edad[1],2),"   ",
                         "Desviación típica grupo 2: ",round(d_tipica_edad_kproto_3$Edad[2],2),"   ",
                        "Desviación típica grupo 3: ",round(d_tipica_edad_kproto_3$Edad[3],2)),
       x = "Edad",
       y = "Porcentaje (%)",
       fill = "Edad") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #theme_minimal() +
  theme(text = element_text(size=12))

graf_ed_kproto_3

# 3.6.2. Gráfico jerar_4:

# 3.6.2.1. Obtención del dataframe:

tabla_jerar_4_edad <- tabla_jerar_4

tabla_jerar_4_edad$Edad<-cut(tabla_jerar_4_edad$Edad,breaks = cortes_edad,
                             right = FALSE,
                             labels = clase_edad)

df_edad_jerar_4 <- tabla_jerar_4_edad %>%
  group_by( cluster, Edad) %>%
  summarise(count = n()) %>%
  mutate(total_edad = sum(count)) %>%
  mutate(porcentaje_edad = count / total_edad * 100) 

media_edad_jerar_4 <- aggregate(Edad ~ cluster, data = tabla_jerar_4, mean)
d_tipica_edad_jerar_4 <- aggregate(Edad ~ cluster, data = tabla_jerar_4, sd)

# 3.6.2.2. Obtención de la gráfica:

graf_ed_jerar_4 <- ggplot(df_edad_jerar_4, aes(x = df_edad_jerar_4$Edad, y = porcentaje_edad, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_edad_jerar_4$cluster)), name = "Pastel1"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 85)) +
  geom_text(aes(label = paste0(round(porcentaje_edad, digits=2),"%\n",count)),
            position = position_dodge(width = 1),
            vjust = 1.1,
            color = "black",
            size = 4,
            fontface = "bold") +
  labs(title = "Distribución de edad por clúster \n jerar_4",
       subtitle = paste0("Media grupo 1: ",round(media_edad_jerar_4$Edad[1],2), "   ",
                         "Media grupo 2: ",round(media_edad_jerar_4$Edad[2],2), "   ",
                         "Media grupo 3: ",round(media_edad_jerar_4$Edad[3],2), "   ",
                         "Media grupo 4: ",round(media_edad_jerar_4$Edad[4],2), 
                         "\nDesviación típica grupo 1: ",round(d_tipica_edad_jerar_4$Edad[1],2),"   ",
                         "Desviación típica grupo 2: ",round(d_tipica_edad_jerar_4$Edad[2],2),"   ",
                         "Desviación típica grupo 3: ",round(d_tipica_edad_jerar_4$Edad[3],2),"   ",
                         "Desviación típica grupo 4: ",round(d_tipica_edad_jerar_4$Edad[4],2)),
       x = "Edad",
       y = "Porcentaje (%)",
       fill = "Edad") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #theme_minimal() +
  theme(text = element_text(size=12))

graf_ed_jerar_4

# 3.6.3. Gráfico jerar_6:

# 3.6.3.1. Obtención del dataframe:

tabla_jerar_6_edad <- tabla_jerar_6

tabla_jerar_6_edad$Edad<-cut(tabla_jerar_6_edad$Edad,breaks = cortes_edad,
                              right = FALSE,
                              labels = clase_edad)

df_edad_jerar_6 <- tabla_jerar_6_edad %>%
  group_by( cluster, Edad) %>%
  summarise(count = n()) %>%
  mutate(total_edad = sum(count)) %>%
  mutate(porcentaje_edad = count / total_edad * 100) 

media_edad_jerar_6 <- aggregate(Edad ~ cluster, data = tabla_jerar_6, mean)
d_tipica_edad_jerar_6 <- aggregate(Edad ~ cluster, data = tabla_jerar_6, sd)

# 3.6.3.2. Obtención de la gráfica:

graf_ed_jerar_6 <- ggplot(df_edad_jerar_6, aes(x = df_edad_jerar_6$Edad, y = porcentaje_edad, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_edad_jerar_6$cluster)), name = "Pastel1"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 87)) +
  geom_text(aes(label = paste0(round(porcentaje_edad, digits=2),"%\n",count)),
            position = position_dodge(width = 1),
            vjust = 1.1,
            color = "black",
            size = 4,
            fontface = "bold") +
  labs(title = "Distribución de edad por clúster \n jerar_6",
       subtitle = paste0("Media grupo 1: ",round(media_edad_jerar_6$Edad[1],2), "   ",
                         "Media grupo 2: ",round(media_edad_jerar_6$Edad[2],2), "   ",
                         "Media grupo 3: ",round(media_edad_jerar_6$Edad[3],2), "   ",
                         "Media grupo 4: ",round(media_edad_jerar_6$Edad[4],2), "   ",
                         "Media grupo 5: ",round(media_edad_jerar_6$Edad[5],2), "   ",
                         "Media grupo 6: ",round(media_edad_jerar_6$Edad[6],2), 
                         "\nDesviación típica grupo 1: ",round(d_tipica_edad_jerar_6$Edad[1],2),"   ",
                         "Desviación típica grupo 2: ",round(d_tipica_edad_jerar_6$Edad[2],2),"   ",
                         "Desviación típica grupo 3: ",round(d_tipica_edad_jerar_6$Edad[3],2),"   ",
                         "Desviación típica grupo 4: ",round(d_tipica_edad_jerar_6$Edad[4],2),"   ",
                         "Desviación típica grupo 5: ",round(d_tipica_edad_jerar_6$Edad[5],2),"   ",
                         "Desviación típica grupo 6: ",round(d_tipica_edad_jerar_6$Edad[6],2)),
       x = "Edad",
       y = "Porcentaje (%)",
       fill = "Edad") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #theme_minimal() +
  theme(text = element_text(size=12))

graf_ed_jerar_6

# 3.7. Gráfico de barras de la variable fevi:

# 3.7.1. Gráfico kproto_3:

# 3.7.1.1. Obtención del dataframe:

tabla_kproto_3_fevi <- tabla_kproto_3
cortes_fevi <- c(0,35,45,51,100)
clase_fevi <- c("Menos del 35%", "35% - 45%", "46% - 50%", "Superior al 50%")

tabla_kproto_3_fevi$FEVI<-cut(tabla_kproto_3_fevi$FEVI,breaks = cortes_fevi,
                              right = FALSE,
                              labels = clase_fevi)

df_fevi_kproto_3 <- tabla_kproto_3_fevi %>%
  group_by( cluster, FEVI) %>%
  summarise(count = n()) %>%
  mutate(total_fevi = sum(count)) %>%
  mutate(porcentaje_fevi = count / total_fevi * 100) 

media_fevi_kproto_3 <- aggregate(FEVI ~ cluster, data = tabla_kproto_3, mean)
d_tipica_fevi_kproto_3 <- aggregate(FEVI ~ cluster, data = tabla_kproto_3, sd)

# 3.7.1.2. Obtención de la gráfica:
df_fevi_kproto_3$FEVI <- factor(df_fevi_kproto_3$FEVI, levels = c("Menos del 35%", "35% - 45%", "46% - 50%", "Superior al 50%"), labels = c("Less than 35%",  "35% - 45%", "46% - 50%", "More than 50%"))

graf_fevi_kproto_3 <- ggplot(df_fevi_kproto_3, aes(x = FEVI, y = porcentaje_fevi, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_fevi_kproto_3$cluster)), name = "Pastel2"),
                    name = "Cluster") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_fevi, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 12,
            fontface = "bold") +
  labs(title = "",
       x = "LVEF",
       y = "Percentage (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 35, face = "bold"))

graf_fevi_kproto_3

# 3.7.2. Gráfico jerar_4:

# 3.7.2.1. Obtención del dataframe:

tabla_jerar_4_fevi <- tabla_jerar_4

tabla_jerar_4_fevi$FEVI<-cut(tabla_jerar_4_fevi$FEVI,breaks = cortes_fevi,
                              right = FALSE,
                              labels = clase_fevi)

df_fevi_jerar_4 <- tabla_jerar_4_fevi %>%
  group_by( cluster, FEVI) %>%
  summarise(count = n()) %>%
  mutate(total_fevi = sum(count)) %>%
  mutate(porcentaje_fevi = count / total_fevi * 100) 

media_fevi_jerar_4 <- aggregate(FEVI ~ cluster, data = tabla_jerar_4, mean)
d_tipica_fevi_jerar_4 <- aggregate(FEVI ~ cluster, data = tabla_jerar_4, sd)

# 3.7.2.2. Obtención de la gráfica:

graf_fevi_jerar_4 <- ggplot(df_fevi_jerar_4, aes(x = FEVI, y = porcentaje_fevi, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_fevi_jerar_4$cluster)), name = "Pastel2"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_fevi, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 10,
            fontface = "bold") +
  labs(title = "",
       x = "FEVI",
       y = "Porcentaje (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 35, face = "bold"))

graf_fevi_jerar_4

# 3.7.2. Gráfico jerar_6:

# 3.7.2.1. Obtención del dataframe:

tabla_jerar_6_fevi <- tabla_jerar_6

tabla_jerar_6_fevi$FEVI<-cut(tabla_jerar_6_fevi$FEVI,breaks = cortes_fevi,
                             right = FALSE,
                             labels = clase_fevi)

df_fevi_jerar_6 <- tabla_jerar_6_fevi %>%
  group_by( cluster, FEVI) %>%
  summarise(count = n()) %>%
  mutate(total_fevi = sum(count)) %>%
  mutate(porcentaje_fevi = count / total_fevi * 100) 

media_fevi_jerar_6 <- aggregate(FEVI ~ cluster, data = tabla_jerar_6, mean)
d_tipica_fevi_jerar_6 <- aggregate(FEVI ~ cluster, data = tabla_jerar_6, sd)

# 3.7.2.2. Obtención de la gráfica:

graf_fevi_jerar_6 <- ggplot(df_fevi_jerar_6, aes(x = FEVI, y = porcentaje_fevi, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.95), color = "black") +
  scale_fill_manual(values = brewer.pal(n = length(unique(df_fevi_jerar_6$cluster)), name = "Pastel2"),
                    name = "Grupo") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label = paste0(round(porcentaje_fevi, digits = 2)),),
            position = position_dodge(width = 0.95),
            vjust = -0.5,
            color = "black",
            size = 8,
            fontface = "bold") +
  labs(title = "",
       x = "FEVI",
       y = "Porcentaje (%)",
       fill = "Grupò") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(text = element_text(size = 34),
        axis.text.x = element_text(size = 35, face = "bold"))

graf_fevi_jerar_6

# 4. Violinplots sexo-edad:

# 4.1. Violinplot sexo-edad kproto3:

df_fevi_kproto_3$FEVI <- factor(df_fevi_kproto_3$FEVI, levels = c("Menos del 35%", "35% - 45%", "46% - 50%", "Superior al 50%"), labels = c("Less than 35%",  "35% - 45%", "46% - 50%", "More than 50%"))


df_violin_kproto_3 <- tabla_kproto_3 %>%
  dplyr::  select(cluster,Edad,Sexo)

df_violin_kproto_3$Sexo <- factor(df_violin_kproto_3$Sexo, levels = c("Hombre","Mujer"), labels = c("Male", "Female"))


violinplot_kproto_3 <- df_violin_kproto_3 %>%
  ggplot(aes(x = Sexo, y = Edad, fill = Sexo)) +
  geom_violin(width = 0.7, alpha = 0.5) +  
  geom_boxplot(alpha = 0.2) +
  geom_jitter(alpha=0.05) +
  scale_fill_manual(values = c("Male" = "#268F9E", "Female" = "#9E2631"), name = "Sex") +
  facet_wrap(~factor(cluster, labels = c("1", "2","3"))) + # Asegúrate de que las etiquetas coincidan con tus clusters
  labs(title="",
       x = "Cluster",
       y = "Age (years)",
       fill = "Grupo") +
  theme_bw()+
  theme(text = element_text(size=30))

violinplot_kproto_3

# 4.2. Violinplot sexo-edad jerar4:

df_violin_jerar_4 <- tabla_jerar_4 %>%
  dplyr::  select(cluster,Edad,Sexo)

violinplot_jerar_4 <- df_violin_jerar_4 %>%
  ggplot(aes(x = Sexo, y = Edad, fill = Sexo)) +
  geom_violin(width = 0.7, alpha = 0.5) +  
  geom_boxplot(alpha = 0.2) +
  geom_jitter(alpha=0.05) +
  scale_fill_manual(values = c("Hombre" = "#268F9E", "Mujer" = "#9E2631"), name = "Sexo") +
  facet_wrap(~factor(cluster, labels = c("1", "2","3","4"))) + 
  labs(title="Distribución de edad por subgrupo",
       x = "Grupo",
       y = "Edad (años)",
       fill = "Grupo") +
  theme_bw()+
  theme(text = element_text(size=30))

violinplot_jerar_4

# 4.2. Violinplot sexo-edad jerar6:

df_violin_jerar_6 <- tabla_jerar_6 %>%
  dplyr::  select(cluster,Edad,Sexo)

violinplot_jerar_6 <- df_violin_jerar_6 %>%
  ggplot(aes(x = Sexo, y = Edad, fill = Sexo)) +
  geom_violin(width = 0.7, alpha = 0.5) +  
  geom_boxplot(alpha = 0.2) +
  geom_jitter(alpha=0.05) +
  scale_fill_manual(values = c("Hombre" = "#268F9E", "Mujer" = "#9E2631"), name = "Sexo") +
  facet_wrap(~factor(cluster, labels = c("1", "2","3","4","5","6"))) + 
  labs(title="",
       x = "Grupo",
       y = "Edad (años)",
       fill = "Grupo") +
  theme_bw()+
  theme(text = element_text(size=30))

violinplot_jerar_6

