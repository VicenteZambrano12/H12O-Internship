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
library(viridis)


# 2. Cargamos las bases de datos:
# Se han seleccionado k-prototypes con k = 3, jerárquico con k = 4, y jerárquico con k = 6
# Los denominaremos kproto_3, jerar_4 y jerar6 respectivamente

df_in <- read_excel("C:\\Users\\Vza 12\\Desktop\\Analisis_n_cluster\\UPM\\TFG\\Clustering_7\\Tablas\\Base Clustering.xls")
df_kproto_3 <- read_excel("C:\\Users\\Vza 12\\Desktop\\Analisis_n_cluster\\UPM\\TFG\\Clustering_7\\Tablas\\Resultados_clustering\\kprototypes\\kproto_7_3.xlsx")
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
    "Neuroticism" = neoffi_neuro1_imp,
    "Extroversion" = neoffi_extr1_imp,
    "Openness to experience" = neoffi_apertu1_imp,
    "Kindness" = neo_cor1_imp,
    "Responsibility" = neoffi_respo1_imp,
    "Perceived threat of disease" = bipq_total1_imp,
    "Self-efficacy" = autoeficacia_total1_imp,
    "Stress" = pss_total1_imp,
    "Positive affect" = panas_positiva_total1_imp,
    "Negative affect" = panas_negativa_total1_imp,
    "Mental quality of life" = sf12_me_imp,
    "Anxiety (qualitative)" = hads_clasi_ansi_imp,
    "Depression (qualitative)" = hads_clasi_ansi_imp,
    "Cognitive coping" = afrontamiento_cognitivo1_imp,
    "Social coping" = afrontamiento_social1_imp,
    "Blocking coping" = bloqueo_afrontamiento1_imp,
    "Spiritual coping" = afrontamiento_espiritual1_imp,
    "No. of vessels affected" = vasos_afectados_imp,
    "Type of ACS" = tipo_de_sca_imp,
    "Killip" = killip_imp,
    "LVEF" = fevi,
    "Coronary heart disease" = enfermedad_coronaria,
    "prior AMI" = infarto_de_miocardio,
    "HF" = insuficiencia_cardiaca_con,
    "Arterial disease" = enfermedad_arterial_perif,
    "Stroke" = enfermedad_cerebrovascular,
    "DM" = diabetes_mellitus_sin_evid,
    "CRD" = enfermedad_renal_cronica_m,
    "Charlson" = charlson_clasificacion3,
    "AF" = fa,
    "Anxiety" = ansiedad,
    "Depression" = depresion,
    "Other psychiatric comorbidities" = comorbilidades_psiquiatricas,
    "Hypertension" = hta,
    "Dyslipidaemia" = hiperlipidemia,
    "Smoking" = fumador_ex,
    "Medlife" = medlife_total1_imp,
    "Alcoholism" = alcoholismo,
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
    "Sex" = sexo,
    "Age" = edad,
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
    "Disease perception" = bipq_total1_imp,
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

# 3. Heatmaps de las comorbilidades:

# 3.1. Comorbilidades físicas:

# 3.1.1. Gráfica kproto_3:

# 3.1.1.1. Obtención del dataframe:

tabla_kproto_3$Smoking <- case_when(
  tabla_kproto_3$Smoking == "Ex" ~ "No",
  tabla_kproto_3$Smoking == "No" ~ "No",
  tabla_kproto_3$Smoking == "Sí" ~ "Si"
)
tabla_kproto_3$Smoking <- as.factor(tabla_kproto_3$Smoking)

df_heatmap_fisicas_kproto_3 <- tabla_kproto_3 %>%
  dplyr :: select(Hypertension,Alcoholism,AF,Dyslipidaemia,Sex,'Coronary heart disease', cluster,
                  `prior AMI`,`Arterial disease`,CRD,`Type of ACS`,diabetes_con_afectacion_de,Smoking,HF) %>%
  dplyr:: rename(
    Female = Sex,
    "Type 2 diabetes" = diabetes_con_afectacion_de,
    "NSTEMI" = `Type of ACS`
  )


df_heatmap_fisicas_1_kproto_3 <- aggregate(. ~ cluster, data = df_heatmap_fisicas_kproto_3, FUN = function(x) abs((sum(x) / length(x)-1)*100)) 

df_heatmap_fisicas_2_kproto_3 <- melt(df_heatmap_fisicas_1_kproto_3)

# 3.1.1.2. Obtención de la gráfica:

graf_heatmap_fis_kproto_3 <- ggplot(df_heatmap_fisicas_2_kproto_3, aes(x = cluster, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0,96)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 10,fontface = "bold") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(title="",
       y = "Comorbidity",
       fill = "Percentage")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = 30, face = "bold"))

graf_heatmap_fis_kproto_3

# 3.1.2. Gráfica jerar_4:

# 3.1.2.1. Obtención del dataframe:

tabla_jerar_4$Tabaquismo <- case_when(
  tabla_jerar_4$Tabaquismo == "Ex" ~ "No",
  tabla_jerar_4$Tabaquismo == "No" ~ "No",
  tabla_jerar_4$Tabaquismo == "Sí" ~ "Si"
)
tabla_jerar_4$Tabaquismo <- as.factor(tabla_jerar_4$Tabaquismo)

df_heatmap_fisicas_jerar_4 <- tabla_jerar_4 %>%
  dplyr :: select(Hipertensión,Alcoholismo,FA,Dislipemia,Sexo,`Enfermedad coronaria`, cluster,
                  `IAM previo`,`Ictus (evento)`,`Enfermedad arterial`,ERC,`Tipo de SCA`,diabetes_con_afectacion_de,Tabaquismo,IC) %>%
  dplyr:: rename(
    Mujer = Sexo,
    "Diabetes tipo 2" = diabetes_con_afectacion_de,
    "SCASEST2" = `Tipo de SCA`,
    "Ictus" = `Ictus (evento)`
  )


df_heatmap_fisicas_1_jerar_4 <- aggregate(. ~ cluster, data = df_heatmap_fisicas_jerar_4, FUN = function(x) abs((sum(x) / length(x)-1)*100)) 

df_heatmap_fisicas_2_jerar_4 <- melt(df_heatmap_fisicas_1_jerar_4)

# 3.1.2.2. Obtención de la gráfica:

graf_heatmap_fis_jerar_4 <- ggplot(df_heatmap_fisicas_2_jerar_4, aes(x = cluster, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0,92)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 10, fontface = "bold") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(title="",
       y = "Comorbilidad",
       fill = "Porcentaje")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = 30, face = "bold"))

graf_heatmap_fis_jerar_4

# 3.1.3. Gráfica jerar_6:

# 3.1.3.1. Obtención del dataframe:

tabla_jerar_6$Tabaquismo <- case_when(
  tabla_jerar_6$Tabaquismo == "Ex" ~ "No",
  tabla_jerar_6$Tabaquismo == "No" ~ "No",
  tabla_jerar_6$Tabaquismo == "Sí" ~ "Si"
)
tabla_jerar_6$Tabaquismo <- as.factor(tabla_jerar_6$Tabaquismo)

df_heatmap_fisicas_jerar_6 <- tabla_jerar_6 %>%
  dplyr :: select(Hipertensión,Alcoholismo,FA,Dislipemia,Sexo,`Enfermedad coronaria`, cluster,
                  `IAM previo`,`Ictus (evento)`,`Enfermedad arterial`,ERC,`Tipo de SCA`,diabetes_con_afectacion_de,Tabaquismo,IC) %>%
  dplyr:: rename(
    Mujer = Sexo,
    "Diabetes tipo 2" = diabetes_con_afectacion_de,
    "SCASEST2" = `Tipo de SCA`,
    "Ictus" = `Ictus (evento)`
  )


df_heatmap_fisicas_1_jerar_6 <- aggregate(. ~ cluster, data = df_heatmap_fisicas_jerar_6, FUN = function(x) abs((sum(x) / length(x)-1)*100)) 

df_heatmap_fisicas_2_jerar_6 <- melt(df_heatmap_fisicas_1_jerar_6)

# 3.1.3.2. Obtención de la gráfica:

graf_heatmap_fis_jerar_6 <- ggplot(df_heatmap_fisicas_2_jerar_6, aes(x = cluster, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0,97)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 10, fontface = "bold") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(title="",
       y = "Comorbilidad",
       fill = "Porcentaje")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = 30, face = "bold"))

graf_heatmap_fis_jerar_6

# 3.2. Comorbilidades psicológicas:

# 3.2.1. Gráfica kproto_3:

# 3.2.1.1, Obtención del dataframe:

df_psico_cat_kproto_3 <- tabla_kproto_3 %>%
  dplyr:: select(Anxiety,Depression,cluster)

df_psico_cat_1_kproto_3 <- aggregate(. ~ cluster, data = df_psico_cat_kproto_3, FUN = function(x) abs((sum(x) / length(x)-1)*100))

df_psico_num_kproto_3 <- tabla_kproto_3 %>%
  dplyr:: select(Neuroticism,`Openness to experience`,cluster,Extroversion,Kindness,`Disease perception`,`Negative affect`,
                 `Positive affect`,`Self-efficacy`,Stress,`Mental quality of life`)

df_psico_num_1_kproto_3 <- aggregate(. ~ cluster, data = df_psico_num_kproto_3, FUN = mean ) 

df_psico_kproto_3 <- left_join(df_psico_cat_1_kproto_3,df_psico_num_1_kproto_3, by = "cluster")

df_psico_1_kproto_3 <- melt(df_psico_kproto_3)

# 3.2.1.2. Obtención de la gráfica:

graf_heatmap_psico_kproto_3 <- ggplot(df_psico_1_kproto_3, aes(x = cluster, y = variable, fill = value)) +
  geom_tile() +
  labs(
    title = "",
    y = "Comorbidity"
  )  + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
             plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_gradient2(low = "white", high = "blue", limits = c(7,48)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 10,fontface = "bold") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(size = 30, face = "bold")) +
  labs(fill = "Average")

graf_heatmap_psico_kproto_3

# 3.2.2. Gráfica jerar_4:

# 3.2.2.1, Obtención del dataframe:

df_psico_cat_jerar_4 <- tabla_jerar_4 %>%
  dplyr:: select(Ansiedad,Depresión,cluster)

df_psico_cat_1_jerar_4 <- aggregate(. ~ cluster, data = df_psico_cat_jerar_4, FUN = function(x) abs((sum(x) / length(x)-1)*100))

df_psico_num_jerar_4 <- tabla_jerar_4 %>%
  dplyr:: select(Neuroticismo,`Apertura a la experiencia`,cluster,Extroversión,Kindness,`Percepción enfermedad`,`Negative affect`,
                 `Positive affect`,Autoeficacia,Stress,`Mental quality of life`)

df_psico_num_1_jerar_4 <- aggregate(. ~ cluster, data = df_psico_num_jerar_4, FUN = mean ) 

df_psico_jerar_4 <- left_join(df_psico_cat_1_jerar_4,df_psico_num_1_jerar_4, by = "cluster")

df_psico_1_jerar_4 <- melt(df_psico_jerar_4)

# 3.2.2.2. Obtención de la gráfica:

graf_heatmap_psico_jerar_4 <- ggplot(df_psico_1_jerar_4, aes(x = cluster, y = variable, fill = value)) +
  geom_tile() +
  labs(
    title = "",
    y = "Comorbilidad"
  )  + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
             plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_gradient2(low = "white", high = "blue", limits = c(6,49)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 10,fontface = "bold") +
  theme(axis.text.x = element_text(angle = 1, hjust = 1),
        text = element_text(size = 30, face = "bold")) +
  labs(fill = "Media")

graf_heatmap_psico_jerar_4

# 3.2.3. Gráfica jerar_6:

# 3.2.3.1, Obtención del dataframe:

df_psico_cat_jerar_6 <- tabla_jerar_6 %>%
  dplyr:: select(Ansiedad,Depresión,cluster)

df_psico_cat_1_jerar_6 <- aggregate(. ~ cluster, data = df_psico_cat_jerar_6, FUN = function(x) abs((sum(x) / length(x)-1)*100))

df_psico_num_jerar_6 <- tabla_jerar_6 %>%
  dplyr:: select(Neuroticismo,`Apertura a la experiencia`,cluster,Extroversión,Amabilidad,`Percepción enfermedad`,`Afecto positivo`,
                 `Afecto negativo`,Autoeficacia,Estrés,`Calidad de vida mental`)

df_psico_num_1_jerar_6 <- aggregate(. ~ cluster, data = df_psico_num_jerar_6, FUN = mean ) 

df_psico_jerar_6 <- left_join(df_psico_cat_1_jerar_6,df_psico_num_1_jerar_6, by = "cluster")

df_psico_1_jerar_6 <- melt(df_psico_jerar_6)

# 3.2.3.2. Obtención de la gráfica:

graf_heatmap_psico_jerar_6 <- ggplot(df_psico_1_jerar_6, aes(x = cluster, y = variable, fill = value)) +
  geom_tile() +
  labs(
    title = "",
    y = "Comorbilidad"
  )  + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
             plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_gradient2(low = "white", high = "blue", limits = c(5.5,50)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 10,fontface = "bold") +
  theme(axis.text.x = element_text(angle = 1, hjust = 1),
        text = element_text(size = 30, face = "bold")) +
  labs(fill = "Media")

graf_heatmap_psico_jerar_6

# 4. Gráficas de las variables de adherencia:

# 4.1. Gráfica adherencia a la medicacion:

# 4.1.1. Gráfica k_proto_3:

# 4.1.1.1. Obtención del datframe:

df_adh_med_kproto_3 <- tabla_kproto_3 %>%
  dplyr::filter(!is.na(`Adherencia a la medicación`)) %>%
  group_by( cluster,`Adherencia a la medicación`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_med = sum(count)) %>%
  mutate(porcentaje_adh_med = count / total_adh_med * 100)  

# 4.1.1.2. Obtención de la gráfica:

df_adh_med_kproto_3$`Adherencia a la medicación` <- factor(df_adh_med_kproto_3$`Adherencia a la medicación`, levels = c("Adherente", "No adherente"), labels = c("Adherent", "Non-adherent"))


graf_adh_med_kproto_3 <- ggplot(df_adh_med_kproto_3, aes(x = `Adherencia a la medicación`, y = porcentaje_adh_med, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(3, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_med, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 14,
            fontface = "bold") +
  labs(title = "",
       x = "Adherence",
       y = "Percentage (%)",
       fill = "Cluster") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_med_kproto_3

# 4.1.2. Gráfica jerar_4:

# 4.1.2.1. Obtención del datframe:

df_adh_med_jerar_4 <- tabla_jerar_4 %>%
  dplyr::filter(!is.na(`Adherencia a la medicación`)) %>%
  group_by( cluster,`Adherencia a la medicación`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_med = sum(count)) %>%
  mutate(porcentaje_adh_med = count / total_adh_med * 100)  

# 4.1.2.2. Obtención de la gráfica:

graf_adh_med_jerar_4 <- ggplot(df_adh_med_jerar_4, aes(x = `Adherencia a la medicación`, y = porcentaje_adh_med, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(4, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_med, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 14,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_med_jerar_4

# 4.1.3. Gráfica jerar_6:

# 4.1.3.1. Obtención del datframe:

df_adh_med_jerar_6 <- tabla_jerar_6 %>%
  dplyr::filter(!is.na(`Adherencia a la medicación`)) %>%
  group_by( cluster,`Adherencia a la medicación`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_med = sum(count)) %>%
  mutate(porcentaje_adh_med = count / total_adh_med * 100)  

# 4.1.3.2. Obtención de la gráfica:

graf_adh_med_jerar_6 <- ggplot(df_adh_med_jerar_6, aes(x = `Adherencia a la medicación`, y = porcentaje_adh_med, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(6, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_med, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 10,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_med_jerar_6

# 4.2. Gráfica adherencia a la dieta mediterránea:

# 4.2.1. Gráfica k_proto_3:

# 4.2.1.1. Obtención del datframe:

df_adh_die_kproto_3 <- tabla_kproto_3 %>%
  dplyr::filter(!is.na(`Adherencia a la dieta mediterránea`)) %>%
  group_by( cluster,`Adherencia a la dieta mediterránea`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_die = sum(count)) %>%
  mutate(porcentaje_adh_die = count / total_adh_die * 100)  

p_val_cs_medas_kproto_3 <- 1 - pchisq(df_cs_medas_2_kproto_3$chisq, df = length(unique(df_cs_mh_kproto_3$grupo)) - 1)


# 4.2.1.2. Obtención de la gráfica:

df_adh_die_kproto_3$`Adherencia a la dieta mediterránea` <- factor(df_adh_die_kproto_3$`Adherencia a la dieta mediterránea`, levels = c("Adherente", "No adherente"), labels = c("Adherent", "Non-adherent"))


graf_adh_die_kproto_3 <- ggplot(df_adh_die_kproto_3, aes(x = `Adherencia a la dieta mediterránea`, y = porcentaje_adh_die, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(3, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_die, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 14,
            fontface = "bold") +
  labs(title = "",
       x = "Adhrence",
       y = "Percentage (%)",
       fill = "Cluster") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_die_kproto_3

# 4.2.2. Gráfica jerar_4:

# 4.2.2.1. Obtención del datframe:

df_adh_die_jerar_4 <- tabla_jerar_4 %>%
  dplyr::filter(!is.na(`Adherencia a la dieta mediterránea`)) %>%
  group_by( cluster,`Adherencia a la dieta mediterránea`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_die = sum(count)) %>%
  mutate(porcentaje_adh_die = count / total_adh_die * 100)  

# 4.2.2.2. Obtención de la gráfica:

graf_adh_die_jerar_4 <- ggplot(df_adh_die_jerar_4, aes(x = `Adherencia a la dieta mediterránea`, y = porcentaje_adh_die, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(4, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_die, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 12,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_die_jerar_4

# 4.2.3. Gráfica jerar_6:

# 4.2.3.1. Obtención del datframe:

df_adh_die_jerar_6 <- tabla_jerar_6 %>%
  dplyr::filter(!is.na(`Adherencia a la dieta mediterránea`)) %>%
  group_by( cluster,`Adherencia a la dieta mediterránea`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_die = sum(count)) %>%
  mutate(porcentaje_adh_die = count / total_adh_die * 100)  

# 4.2.3.2. Obtención de la gráfica:

graf_adh_die_jerar_6 <- ggplot(df_adh_die_jerar_6, aes(x = `Adherencia a la dieta mediterránea`, y = porcentaje_adh_die, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(6, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_die, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 9,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_die_jerar_6

# 4.3. Gráfica adherencia a la actividad física:

# 4.3.1. Gráfica k_proto_3:

# 4.3.1.1. Obtención del datframe:

df_adh_fis_kproto_3 <- tabla_kproto_3 %>%
  dplyr::filter(!is.na(`Adherencia a la actividad física`)) %>%
  group_by( cluster,`Adherencia a la actividad física`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_fis = sum(count)) %>%
  mutate(porcentaje_adh_fis = count / total_adh_fis * 100)  

# 4.3.1.2. Obtención de la gráfica:

df_adh_fis_kproto_3$`Adherencia a la actividad física` <- factor(df_adh_fis_kproto_3$`Adherencia a la actividad física`, levels = c("Adherente", "No adherente"), labels = c("Adherent", "Non-adherent"))


graf_adh_fis_kproto_3 <- ggplot(df_adh_fis_kproto_3, aes(x = `Adherencia a la actividad física`, y = porcentaje_adh_fis, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(3, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_fis, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 14,
            fontface = "bold") +
  labs(title = "",
       x = "Adherence",
       y = "Percentage (%)",
       fill = "Cluster") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_fis_kproto_3

# 4.3.2. Gráfica jerar_4:

# 4.3.2.1. Obtención del datframe:

df_adh_fis_jerar_4 <- tabla_jerar_4 %>%
  dplyr::filter(!is.na(`Adherencia a la actividad física`)) %>%
  group_by( cluster,`Adherencia a la actividad física`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_fis = sum(count)) %>%
  mutate(porcentaje_adh_fis = count / total_adh_fis * 100)  

# 4.3.2.2. Obtención de la gráfica:

graf_adh_fis_jerar_4 <- ggplot(df_adh_fis_jerar_4, aes(x = `Adherencia a la actividad física`, y = porcentaje_adh_fis, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(4, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_fis, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 12,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_fis_jerar_4

# 4.3.3. Gráfica jerar_6:

# 4.3.3.1. Obtención del datframe:

df_adh_fis_jerar_6 <- tabla_jerar_6 %>%
  dplyr::filter(!is.na(`Adherencia a la actividad física`)) %>%
  group_by( cluster,`Adherencia a la actividad física`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_fis = sum(count)) %>%
  mutate(porcentaje_adh_fis = count / total_adh_fis * 100)  

# 4.3.3.2. Obtención de la gráfica:

graf_adh_fis_jerar_6 <- ggplot(df_adh_fis_jerar_6, aes(x = `Adherencia a la actividad física`, y = porcentaje_adh_fis, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(6, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_fis, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 9,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_fis_jerar_6

# 4.4. Gráfica adherencia a la rehabilitación:

# 4.4.1. Gráfica k_proto_3:

# 4.4.1.1. Obtención del datframe:

df_adh_rhc_kproto_3 <- tabla_kproto_3 %>%
  dplyr::filter(!is.na(`Adherencia a la rehabilitación`)) %>%
  group_by( cluster,`Adherencia a la rehabilitación`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_rhc = sum(count)) %>%
  mutate(porcentaje_adh_rhc = count / total_adh_rhc * 100)  

# 4.4.1.2. Obtención de la gráfica:

df_adh_rhc_kproto_3$`Adherencia a la rehabilitación` <- factor(df_adh_rhc_kproto_3$`Adherencia a la rehabilitación`, levels = c("Adherente", "No realiza", "No completa"), labels = c("Adherent", "Not perform", "Not comlete"))


graf_adh_rhc_kproto_3 <- ggplot(df_adh_rhc_kproto_3, aes(x = `Adherencia a la rehabilitación`, y = porcentaje_adh_rhc, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(3, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_rhc, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 10,
            fontface = "bold") +
  labs(title = "",
       x = "Adherence",
       y = "Percentage (%)",
       fill = "Cluster") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 10, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_rhc_kproto_3

# 4.4.2. Gráfica jerar_4:

# 4.4.2.1. Obtención del datframe:

df_adh_rhc_jerar_4 <- tabla_jerar_4 %>%
  dplyr::filter(!is.na(`Adherencia a la rehabilitación`)) %>%
  group_by( cluster,`Adherencia a la rehabilitación`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_rhc = sum(count)) %>%
  mutate(porcentaje_adh_rhc = count / total_adh_rhc * 100)  

# 4.4.2.2. Obtención de la gráfica:

graf_adh_rhc_jerar_4 <- ggplot(df_adh_rhc_jerar_4, aes(x = `Adherencia a la rehabilitación`, y = porcentaje_adh_rhc, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(4, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_rhc, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 8,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_rhc_jerar_4

# 4.4.3. Gráfica jerar_6:

# 4.4.3.1. Obtención del datframe:

df_adh_rhc_jerar_6 <- tabla_jerar_6 %>%
  dplyr::filter(!is.na(`Adherencia a la rehabilitación`)) %>%
  group_by( cluster,`Adherencia a la rehabilitación`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_rhc = sum(count)) %>%
  mutate(porcentaje_adh_rhc = count / total_adh_rhc * 100)  

# 4.4.3.2. Obtención de la gráfica:

graf_adh_rhc_jerar_6 <- ggplot(df_adh_rhc_jerar_6, aes(x = `Adherencia a la rehabilitación`, y = porcentaje_adh_rhc, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(6, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_rhc, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 6,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_rhc_jerar_6

# 4.5. Gráfica adherencia total:

# 4.5.1. Gráfica k_proto_3:

# 4.5.1.1. Obtención del datframe:

df_adh_tot_kproto_3 <- tabla_kproto_3 %>%
  dplyr::filter(!is.na(`Adherencia total`)) %>%
  group_by( cluster,`Adherencia total`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_tot = sum(count)) %>%
  mutate(porcentaje_adh_tot = count / total_adh_tot * 100)  

# 4.5.1.2. Obtención de la gráfica:

df_adh_tot_kproto_3$`Adherencia total` <- factor(df_adh_tot_kproto_3$`Adherencia total`, levels = c("Adherente", "No adherente"), labels = c("Adherent", "Non-adherent"))


graf_adh_tot_kproto_3 <- ggplot(df_adh_tot_kproto_3, aes(x = `Adherencia total`, y = porcentaje_adh_tot, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(3, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_tot, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 16,
            fontface = "bold") +
  labs(title = "",
       x = "Adherence",
       y = "Percentage (%)",
       fill = "Cluster") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_tot_kproto_3

# 4.5.2. Gráfica jerar_4:

# 4.5.2.1. Obtención del datframe:

df_adh_tot_jerar_4 <- tabla_jerar_4 %>%
  dplyr::filter(!is.na(`Adherencia total`)) %>%
  group_by( cluster,`Adherencia total`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_tot = sum(count)) %>%
  mutate(porcentaje_adh_tot = count / total_adh_tot * 100)  

# 4.5.2.2. Obtención de la gráfica:

graf_adh_tot_jerar_4 <- ggplot(df_adh_tot_jerar_4, aes(x = `Adherencia total`, y = porcentaje_adh_tot, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(4, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_tot, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 14,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_tot_jerar_4

# 4.5.3. Gráfica jerar_6:

# 4.5.3.1. Obtención del datframe:

df_adh_tot_jerar_6 <- tabla_jerar_6 %>%
  dplyr::filter(!is.na(`Adherencia total`)) %>%
  group_by( cluster,`Adherencia total`) %>%
  summarise(count = n()) %>%
  mutate(total_adh_tot = sum(count)) %>%
  mutate(porcentaje_adh_tot = count / total_adh_tot * 100)  

# 4.5.3.2. Obtención de la gráfica:

graf_adh_tot_jerar_6 <- ggplot(df_adh_tot_jerar_6, aes(x = `Adherencia total`, y = porcentaje_adh_tot, fill = cluster)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7,
           color = "black",
           show.legend = TRUE) +
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_fill_manual(values = brewer.pal(6, "Set1")) +
  geom_text(aes(label = paste0(round(porcentaje_adh_tot, 2))),
            position = position_dodge(width = 0.8),
            vjust = -0.5,  # Ajusta la posición vertical de las etiquetas
            hjust = 0.5,   # Centra las etiquetas horizontalmente
            color = "black",
            size = 10,
            fontface = "bold") +
  labs(title = "",
       x = "Adherencia",
       y = "Porcentaje",
       fill = "Grupo") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
        text = element_text(size = 50, face = "bold"))

graf_adh_tot_jerar_6

# 5. Curvas de supervivencia:

# 5.1. Curva supervivencia variable muerte-hospitalización:

# 5.1.1. Curva kproto_3:

# 5.1.1.1. Obtención del dataframe:

df_cs_mh_kproto_3 <- data.frame(
  evento = as.numeric(tabla_kproto_3$`Muerte-hospitalización (evento)`),
  tiempo = tabla_kproto_3$`Muerte-hospitalización (días)`,
  grupo = as.numeric(tabla_kproto_3$cluster)
)

# 5.1.1.2. Obtención de los objetos de supervivencia:

df_cs_mh_1_kproto_3 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_mh_kproto_3) 
df_cs_mh_2_kproto_3 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_mh_kproto_3)

p_val_cs_mh_kproto_3 <- 1 - pchisq(df_cs_mh_2_kproto_3$chisq, df = length(unique(df_cs_mh_kproto_3$grupo)) - 1)

if (p_val_cs_mh_kproto_3 < 0.001) {
  p_val_text_cs_mh_kproto_3 <- "p < 0.001"
} else {
  p_val_text_cs_mh_kproto_3 <- paste0("p = ", format(p_val_cs_mh_kproto_3, digits = 2, scientific = TRUE))
}

df_cs_mh_3_kproto_3 <- broom::tidy(df_cs_mh_1_kproto_3)

df_cs_mh_3_kproto_3$strata <- case_when(
  df_cs_mh_3_kproto_3$strata == "grupo=1" ~ "1", 
  df_cs_mh_3_kproto_3$strata == "grupo=2" ~ "2",
  df_cs_mh_3_kproto_3$strata == "grupo=3" ~ "3"
)

# 5.1.1.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_mh_kproto_3 <- ggplot(df_cs_mh_3_kproto_3, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 2.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = brewer.pal(3, "Set1"),
                    ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 30),
        title = element_text(size = 30), 
        axis.title = element_text(30), 
        legend.text = element_text(size = 30),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
        ) +
  labs(title = "",
       x = "Time (days)",
       y = "Probability of no death\n or hospitalisation",
       color = NULL
       ) +
  guides(color = guide_legend(title = "Cluster"))

curva_km_mh_kproto_3

# 5.1.2. Curva jerar_4:

# 5.1.2.1. Obtención del dataframe:

df_cs_mh_jerar_4 <- data.frame(
  evento = as.numeric(tabla_jerar_4$`Muerte-hospitalización (evento)`),
  tiempo = tabla_jerar_4$`Muerte-hospitalización (días)`,
  grupo = as.numeric(tabla_jerar_4$cluster)
)

# 5.1.2.2. Obtención de los objetos de supervivencia:

df_cs_mh_1_jerar_4 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_mh_jerar_4) 
df_cs_mh_2_jerar_4 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_mh_jerar_4)

df_cs_mh_3_jerar_4 <- broom::tidy(df_cs_mh_1_jerar_4)

df_cs_mh_3_jerar_4$strata <- case_when(
  df_cs_mh_3_jerar_4$strata == "grupo=1" ~ "1", 
  df_cs_mh_3_jerar_4$strata == "grupo=2" ~ "2",
  df_cs_mh_3_jerar_4$strata == "grupo=3" ~ "3",
  df_cs_mh_3_jerar_4$strata == "grupo=4" ~ "4"
  
)

# 5.1.2.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_mh_jerar_4 <- ggplot(df_cs_mh_3_jerar_4, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 2.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = brewer.pal(4, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 30),
        title = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "",
       x = "Tiempo (días)",
       y = "Probabilidad de no muerte u hospitalización",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_mh_jerar_4

# 5.1.3. Curva jerar_6:

# 5.1.3.1. Obtención del dataframe:

df_cs_mh_jerar_6 <- data.frame(
  evento = as.numeric(tabla_jerar_6$`Muerte-hospitalización (evento)`),
  tiempo = tabla_jerar_6$`Muerte-hospitalización (días)`,
  grupo = as.numeric(tabla_jerar_6$cluster)
)

# 5.1.3.2. Obtención de los objetos de supervivencia:

df_cs_mh_1_jerar_6 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_mh_jerar_6) 
df_cs_mh_2_jerar_6 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_mh_jerar_6)

df_cs_mh_3_jerar_6 <- broom::tidy(df_cs_mh_1_jerar_6)

df_cs_mh_3_jerar_6$strata <- case_when(
  df_cs_mh_3_jerar_6$strata == "grupo=1" ~ "1", 
  df_cs_mh_3_jerar_6$strata == "grupo=2" ~ "2",
  df_cs_mh_3_jerar_6$strata == "grupo=3" ~ "3",
  df_cs_mh_3_jerar_6$strata == "grupo=4" ~ "4",
  df_cs_mh_3_jerar_6$strata == "grupo=5" ~ "5",
  df_cs_mh_3_jerar_6$strata == "grupo=6" ~ "6"
)

# 5.1.3.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_mh_jerar_6 <- ggplot(df_cs_mh_3_jerar_6, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 2.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = brewer.pal(6, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 30),
        title = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "",
       x = "Tiempo (días)",
       y = "Probabilidad de no muerte u hospitalización",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_mh_jerar_6

# 5.2. Curva supervivencia variable IC:

# 5.2.1. Curva kproto_3:

# 5.2.1.1. Obtención del dataframe:

df_cs_ic_kproto_3 <- data.frame(
  evento = as.numeric(tabla_kproto_3$`IC (evento)`),
  tiempo = tabla_kproto_3$`IC (días)`,
  grupo = as.numeric(tabla_kproto_3$cluster)
)

# 5.2.1.2. Obtención de los objetos de supervivencia:

df_cs_ic_1_kproto_3 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_ic_kproto_3) 
df_cs_ic_2_kproto_3 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_ic_kproto_3)

p_val_cs_ic_kproto_3 <- 1 - pchisq(df_cs_ic_2_kproto_3$chisq, df = length(unique(df_cs_ic_kproto_3$grupo)) - 1)

if (p_val_cs_ic_kproto_3 < 0.001) {
  p_val_text_cs_ic_kproto_3 <- "p < 0.001"
} else {
  p_val_text_cs_ic_kproto_3 <- paste0("p = ", format(p_val_cs_ic_kproto_3, digits = 2, scientific = TRUE))
}

df_cs_ic_3_kproto_3 <- broom::tidy(df_cs_ic_1_kproto_3)

# 5.1.1.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_ic_kproto_3 <- ggplot(df_cs_ic_3_kproto_3, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0.98,1)) +
  scale_fill_manual(values = brewer.pal(3, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        title = element_text(size = 15), 
        axis.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "Curvas de supervivencia de Kaplan-Meier\n por grupo de la variable IC \n kproto_3",
       subtitle = paste0("P-valor Log-Rank: ", p_val_text_cs_ic_kproto_3),
       x = "Tiempo (días)",
       y = "Probabilidad de no IC",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_ic_kproto_3

# 5.2.2. Curva jerar_4:

# 5.2.2.1. Obtención del dataframe:

df_cs_ic_jerar_4 <- data.frame(
  evento = as.numeric(tabla_jerar_4$`IC (evento)`),
  tiempo = tabla_jerar_4$`IC (días)`,
  grupo = as.numeric(tabla_jerar_4$cluster)
)

# 5.2.2.2. Obtención de los objetos de supervivencia:

df_cs_ic_1_jerar_4 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_ic_jerar_4) 
df_cs_ic_2_jerar_4 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_ic_jerar_4)

p_val_cs_ic_jerar_4 <- 1 - pchisq(df_cs_ic_2_jerar_4$chisq, df = length(unique(df_cs_ic_jerar_4$grupo)) - 1)

if (p_val_cs_ic_jerar_4 < 0.001) {
  p_val_text_cs_ic_jerar_4 <- "p < 0.001"
} else {
  p_val_text_cs_ic_jerar_4 <- paste0("p = ", format(p_val_cs_ic_jerar_4, digits = 2, scientific = TRUE))
}

df_cs_ic_3_jerar_4 <- broom::tidy(df_cs_ic_1_jerar_4)

# 5.2.2.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_ic_jerar_4 <- ggplot(df_cs_ic_3_jerar_4, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0.97,1)) +
  scale_fill_manual(values = brewer.pal(4, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        title = element_text(size = 15), 
        axis.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "Curvas de supervivencia de Kaplan-Meier\n por grupo de la variable IC \n jerar_4",
       subtitle = paste0("P-valor Log-Rank: ", p_val_text_cs_ic_jerar_4),
       x = "Tiempo (días)",
       y = "Probabilidad de no IC",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_ic_jerar_4

# 5.2.3. Curva jerar_6:

# 5.2.3.1. Obtención del dataframe:

df_cs_ic_jerar_6 <- data.frame(
  evento = as.numeric(tabla_jerar_6$`IC (evento)`),
  tiempo = tabla_jerar_6$`Ictus (días)`,
  grupo = as.numeric(tabla_jerar_6$cluster)
)

# 5.2.3.2. Obtención de los objetos de supervivencia:

df_cs_ic_1_jerar_6 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_ic_jerar_6) 
df_cs_ic_2_jerar_6 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_ic_jerar_6)

p_val_cs_ic_jerar_6 <- 1 - pchisq(df_cs_ic_2_jerar_6$chisq, df = length(unique(df_cs_mh_jerar_6$grupo)) - 1)

if (p_val_cs_ic_jerar_6 < 0.001) {
  p_val_text_cs_ic_jerar_6 <- "p < 0.001"
} else {
  p_val_text_cs_ic_jerar_6 <- paste0("p = ", format(p_val_cs_ic_jerar_6, digits = 2, scientific = TRUE))
}

df_cs_ic_3_jerar_6 <- broom::tidy(df_cs_ic_1_jerar_6)

# 5.2.3.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_ic_jerar_6 <- ggplot(df_cs_ic_3_jerar_6, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0.8,1)) +
  scale_fill_manual(values = brewer.pal(6, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        title = element_text(size = 15), 
        axis.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "Curvas de supervivencia de Kaplan-Meier\n por grupo de la variable IC \n jerar_6",
       subtitle = paste0("P-valor Log-Rank: ", p_val_text_cs_ic_jerar_6),
       x = "Tiempo (días)",
       y = "Probabilidad de no IC",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_ic_jerar_6

# 5.3. Curva supervivencia variable IAM:

# 5.3.1. Curva kproto_3:

# 5.3.1.1. Obtención del dataframe:

df_cs_iam_kproto_3 <- data.frame(
  evento = as.numeric(tabla_kproto_3$`IAM posterior (evento)`),
  tiempo = tabla_kproto_3$`IAM posterior (días)`,
  grupo = as.numeric(tabla_kproto_3$cluster)
)

# 5.3.1.2. Obtención de los objetos de supervivencia:

df_cs_iam_1_kproto_3 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_iam_kproto_3) 
df_cs_iam_2_kproto_3 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_iam_kproto_3)

p_val_cs_iam_kproto_3 <- 1 - pchisq(df_cs_iam_2_kproto_3$chisq, df = length(unique(df_cs_iam_kproto_3$grupo)) - 1)

if (p_val_cs_iam_kproto_3 < 0.001) {
  p_val_text_cs_iam_kproto_3 <- "p < 0.001"
} else {
  p_val_text_cs_iam_kproto_3 <- paste0("p = ", format(p_val_cs_iam_kproto_3, digits = 2, scientific = TRUE))
}

df_cs_iam_3_kproto_3 <- broom::tidy(df_cs_iam_1_kproto_3)

# 5.3.1.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_iam_kproto_3 <- ggplot(df_cs_iam_3_kproto_3, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0.95,1)) +
  scale_fill_manual(values = brewer.pal(3, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        title = element_text(size = 15), 
        axis.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "Curvas de supervivencia de Kaplan-Meier\n por grupo de la variable IAM \n kproto_3",
       subtitle = paste0("P-valor Log-Rank: ", p_val_text_cs_iam_kproto_3),
       x = "Tiempo (días)",
       y = "Probabilidad de no IAM",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_iam_kproto_3

# 5.3.2. Curva jerar_4:

# 5.3.2.1. Obtención del dataframe:

df_cs_iam_jerar_4 <- data.frame(
  evento = as.numeric(tabla_jerar_4$`IAM posterior (evento)`),
  tiempo = tabla_jerar_4$`IAM posterior (días)`,
  grupo = as.numeric(tabla_jerar_4$cluster)
)

# 5.3.2.2. Obtención de los objetos de supervivencia:

df_cs_iam_1_jerar_4 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_iam_jerar_4) 
df_cs_iam_2_jerar_4 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_iam_jerar_4)

p_val_cs_iam_jerar_4 <- 1 - pchisq(df_cs_iam_2_jerar_4$chisq, df = length(unique(df_cs_iam_jerar_4$grupo)) - 1)

if (p_val_cs_iam_jerar_4 < 0.001) {
  p_val_text_cs_iam_jerar_4 <- "p < 0.001"
} else {
  p_val_text_cs_iam_jerar_4 <- paste0("p = ", format(p_val_cs_iam_jerar_4, digits = 2, scientific = TRUE))
}

df_cs_iam_3_jerar_4 <- broom::tidy(df_cs_iam_1_jerar_4)

# 5.3.2.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_iam_jerar_4 <- ggplot(df_cs_iam_3_jerar_4, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0.95,1)) +
  scale_fill_manual(values = brewer.pal(4, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        title = element_text(size = 15), 
        axis.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "Curvas de supervivencia de Kaplan-Meier\n por grupo de la variable IAM \n jerar_4",
       subtitle = paste0("P-valor Log-Rank: ", p_val_text_cs_iam_jerar_4),
       x = "Tiempo (días)",
       y = "Probabilidad de no IAM",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_iam_jerar_4

# 5.3.3. Curva jerar_6:

# 5.3.3.1. Obtención del dataframe:

df_cs_iam_jerar_6 <- data.frame(
  evento = as.numeric(tabla_jerar_6$`IAM posterior (evento)`),
  tiempo = tabla_jerar_6$`IAM posterior (días)`,
  grupo = as.numeric(tabla_jerar_6$cluster)
)

# 5.3.3.2. Obtención de los objetos de supervivencia:

df_cs_iam_1_jerar_6 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_iam_jerar_6) 
df_cs_iam_2_jerar_6 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_iam_jerar_6)

p_val_cs_iam_jerar_6 <- 1 - pchisq(df_cs_iam_2_jerar_6$chisq, df = length(unique(df_cs_iam_jerar_6$grupo)) - 1)

if (p_val_cs_iam_jerar_6 < 0.001) {
  p_val_text_cs_iam_jerar_6 <- "p < 0.001"
} else {
  p_val_text_cs_iam_jerar_6 <- paste0("p = ", format(p_val_cs_iam_jerar_6, digits = 2, scientific = TRUE))
}

df_cs_iam_3_jerar_6 <- broom::tidy(df_cs_iam_1_jerar_6)

# 5.3.3.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_iam_jerar_6 <- ggplot(df_cs_iam_3_jerar_6, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0.94,1)) +
  scale_fill_manual(values = brewer.pal(6, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        title = element_text(size = 15), 
        axis.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "Curvas de supervivencia de Kaplan-Meier\n por grupo de la variable IAM \n jerar_6",
       subtitle = paste0("P-valor Log-Rank: ", p_val_text_cs_iam_jerar_6),
       x = "Tiempo (días)",
       y = "Probabilidad de no IAM",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_iam_jerar_6

# 5.4. Curva supervivencia variable ictus:

# 5.4.1. Curva kproto_3:

# 5.4.1.1. Obtención del dataframe:

df_cs_ictus_kproto_3 <- data.frame(
  evento = as.numeric(tabla_kproto_3$`Ictus (evento)`),
  tiempo = tabla_kproto_3$`Ictus (días)`,
  grupo = as.numeric(tabla_kproto_3$cluster)
)

# 5.4.1.2. Obtención de los objetos de supervivencia:

df_cs_ictus_1_kproto_3 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_ictus_kproto_3) 
df_cs_ictus_2_kproto_3 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_ictus_kproto_3)

p_val_cs_ictus_kproto_3 <- 1 - pchisq(df_cs_ictus_2_kproto_3$chisq, df = length(unique(df_cs_ictus_kproto_3$grupo)) - 1)

if (p_val_cs_ictus_kproto_3 < 0.001) {
  p_val_text_cs_ictus_kproto_3 <- "p < 0.001"
} else {
  p_val_text_cs_ictus_kproto_3 <- paste0("p = ", format(p_val_cs_ictus_kproto_3, digits = 2, scientific = TRUE))
}

df_cs_ictus_3_kproto_3 <- broom::tidy(df_cs_ictus_1_kproto_3)

# 5.1.1.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_ictus_kproto_3 <- ggplot(df_cs_ictus_3_kproto_3, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0.985,1)) +
  scale_fill_manual(values = brewer.pal(3, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        title = element_text(size = 15), 
        axis.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "Curvas de supervivencia de Kaplan-Meier\n por grupo de la variable ictus \n kproto_3",
       subtitle = paste0("P-valor Log-Rank: ", p_val_text_cs_ictus_kproto_3),
       x = "Tiempo (días)",
       y = "Probabilidad de no ictus",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_ictus_kproto_3

# 5.4.2. Curva jerar_4:

# 5.4.2.1. Obtención del dataframe:

df_cs_ictus_jerar_4 <- data.frame(
  evento = as.numeric(tabla_jerar_4$`Ictus (evento)`),
  tiempo = tabla_jerar_4$`Ictus (días)`,
  grupo = as.numeric(tabla_jerar_4$cluster)
)

# 5.4.2.2. Obtención de los objetos de supervivencia:

df_cs_ictus_1_jerar_4 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_ictus_jerar_4) 
df_cs_ictus_2_jerar_4 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_ictus_jerar_4)

p_val_cs_ictus_jerar_4 <- 1 - pchisq(df_cs_ictus_2_jerar_4$chisq, df = length(unique(df_cs_ictus_jerar_4$grupo)) - 1)

if (p_val_cs_ictus_jerar_4 < 0.001) {
  p_val_text_cs_ictus_jerar_4 <- "p < 0.001"
} else {
  p_val_text_cs_ictus_jerar_4 <- paste0("p = ", format(p_val_cs_ictus_jerar_4, digits = 2, scientific = TRUE))
}

df_cs_ictus_3_jerar_4 <- broom::tidy(df_cs_ictus_1_jerar_4)

# 5.4.2.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_ictus_jerar_4 <- ggplot(df_cs_ictus_3_jerar_4, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0.95,1)) +
  scale_fill_manual(values = brewer.pal(4, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        title = element_text(size = 15), 
        axis.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "Curvas de supervivencia de Kaplan-Meier\n por grupo de la variable ictus \n jerar_4",
       subtitle = paste0("P-valor Log-Rank: ", p_val_text_cs_ictus_jerar_4),
       x = "Tiempo (días)",
       y = "Probabilidad de no ictus",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_ictus_jerar_4

# 5.4.3. Curva jerar_6:

# 5.4.3.1. Obtención del dataframe:

df_cs_ictus_jerar_6 <- data.frame(
  evento = as.numeric(tabla_jerar_6$`Ictus (evento)`),
  tiempo = tabla_jerar_6$`Ictus (días)`,
  grupo = as.numeric(tabla_jerar_6$cluster)
)

# 5.4.3.2. Obtención de los objetos de supervivencia:

df_cs_ictus_1_jerar_6 <-  survfit(Surv(tiempo,evento) ~ grupo, data = df_cs_ictus_jerar_6) 
df_cs_ictus_2_jerar_6 <-  survdiff(Surv(tiempo,evento) ~ grupo, data = df_cs_ictus_jerar_6)

p_val_cs_ictus_jerar_6 <- 1 - pchisq(df_cs_ictus_2_jerar_6$chisq, df = length(unique(df_cs_ictus_jerar_6$grupo)) - 1)

if (p_val_cs_ictus_jerar_6 < 0.001) {
  p_val_text_cs_ictus_jerar_6 <- "p < 0.001"
} else {
  p_val_text_cs_ictus_jerar_6 <- paste0("p = ", format(p_val_cs_ictus_jerar_6, digits = 2, scientific = TRUE))
}

df_cs_ictus_3_jerar_6 <- broom::tidy(df_cs_ictus_1_jerar_6)

# 5.4.3.3 Obtención de la gráfica por Kaplan- Meier:

curva_km_ictus_jerar_6 <- ggplot(df_cs_ictus_3_jerar_6, aes(time, estimate, color = strata, group = strata)) +
  geom_step(size = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0.94,1)) +
  scale_fill_manual(values = brewer.pal(6, "Set1"),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        title = element_text(size = 15), 
        axis.title = element_text(size = 13), 
        legend.text = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18)
  ) +
  labs(title = "Curvas de supervivencia de Kaplan-Meier\n por grupo de la variable ictus \n jerar_6",
       subtitle = paste0("P-valor Log-Rank: ", p_val_text_cs_ictus_jerar_6),
       x = "Tiempo (días)",
       y = "Probabilidad de no ictus",
       color = NULL
  ) +
  guides(color = guide_legend(title = "Grupo"))

curva_km_ictus_jerar_6

# 6. Extracción de p_valores mediante compare groups:

# 6.1. P-valores k-proto3:

df_adh <- tabla_kproto_3 %>%
  dplyr::select(cluster,`Adherencia a la medicación`,`Adherencia a la dieta mediterránea`,`Adherencia a la actividad física`
                ,`Adherencia a la rehabilitación`,`Adherencia total`)%>%
  na.omit()

df_adh_mmas_kproto_3  <- table(df_adh$cluster, df_adh$`Adherencia a la medicación`)
chisquare_mmas_kproto_3 <-  chisq.test(df_adh_mmas_kproto_3)

df_adh_medas_kproto_3  <- table(df_adh$cluster, df_adh$`Adherencia a la dieta mediterránea`)
chisquare_medas_kproto_3 <-  chisq.test(df_adh_medas_kproto_3)

df_adh_ipaq_kproto_3  <- table(df_adh$cluster, df_adh$`Adherencia a la actividad física`)
chisquare_ipaq_kproto_3 <-  chisq.test(df_adh_ipaq_kproto_3)

df_adh_total_kproto_3  <- table(df_adh$cluster, df_adh$`Adherencia total`)
chisquare_total_kproto_3 <-  chisq.test(df_adh_total_kproto_3)

df_adh_rhc_kproto_3  <- table(df_adh$cluster, df_adh$`Adherencia a la rehabilitación`)
chisquare_rhc_kproto_3 <-  chisq.test(df_adh_rhc_kproto_3)

# 6.2. P-valores jerar4:

df_adh_jerar4 <- tabla_jerar_4 %>%
  dplyr::select(cluster,`Adherencia a la medicación`,`Adherencia a la dieta mediterránea`,`Adherencia a la actividad física`
                ,`Adherencia a la rehabilitación`,`Adherencia total`)%>%
  na.omit()

df_adh_mmas_jerar_4  <- table(df_adh_jerar4$cluster, df_adh_jerar4$`Adherencia a la medicación`)
chisquare_mmas_jerar_4 <-  chisq.test(df_adh_mmas_jerar_4)

df_adh_medas_jerar_4  <- table(df_adh_jerar4$cluster, df_adh_jerar4$`Adherencia a la dieta mediterránea`)
chisquare_medas_jerar_4 <-  chisq.test(df_adh_medas_jerar_4)

df_adh_ipaq_jerar_4  <- table(df_adh_jerar4$cluster, df_adh_jerar4$`Adherencia a la actividad física`)
chisquare_ipaq_jerar_4 <-  chisq.test(df_adh_ipaq_jerar_4)

df_adh_total_jerar_4  <- table(df_adh_jerar4$cluster, df_adh_jerar4$`Adherencia total`)
chisquare_total_jerar_4 <-  chisq.test(df_adh_total_jerar_4)

df_adh_rhc_jerar_4  <- table(df_adh_jerar4$cluster, df_adh_jerar4$`Adherencia a la rehabilitación`)
chisquare_rhc_jerar_4 <-  chisq.test(df_adh_rhc_jerar_4)

# 6.3. P-valores jerar6:

df_adh_jerar6 <- tabla_jerar_6 %>%
  dplyr::select(cluster,`Adherencia a la medicación`,`Adherencia a la dieta mediterránea`,`Adherencia a la actividad física`
                ,`Adherencia a la rehabilitación`,`Adherencia total`)%>%
  na.omit()

df_adh_mmas_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$`Adherencia a la medicación`)
chisquare_mmas_jerar_6 <-  chisq.test(df_adh_mmas_jerar_6)

df_adh_medas_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$`Adherencia a la dieta mediterránea`)
chisquare_medas_jerar_6 <-  chisq.test(df_adh_medas_jerar_6)

df_adh_ipaq_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$`Adherencia a la actividad física`)
chisquare_ipaq_jerar_6 <-  chisq.test(df_adh_ipaq_jerar_6)

df_adh_total_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$`Adherencia total`)
chisquare_total_jerar_6 <-  chisq.test(df_adh_total_jerar_6)

df_adh_rhc_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$`Adherencia a la rehabilitación`)
chisquare_rhc_jerar_6 <-  chisq.test(df_adh_rhc_jerar_6)

# 7. Curvas de supervivencia de la variable muerte-hospitalización estratificadas por sexo:

# 7.1. Curva kproto_3:

# 7.1.1 Obtención del dataframe:

df_cs_mh_kproto_3_s <- data.frame(
  evento = as.numeric(tabla_kproto_3$`Muerte-hospitalización (evento)`),
  tiempo = tabla_kproto_3$`Muerte-hospitalización (días)`,
  grupo = as.numeric(tabla_kproto_3$cluster),
  sexo = as.factor(tabla_kproto_3$Sex)
)

# 7.1.2. Obtención de los objetos de supervivencia:

df_cs_mh_1_kproto_3_s <-  survfit(Surv(tiempo,evento) ~ grupo , data = df_cs_mh_kproto_3_s) 
df_cs_mh_2_kproto_3_s <-  survdiff(Surv(tiempo,evento) ~ grupo , data = df_cs_mh_kproto_3_s)


df_cs_mh_3_kproto_3_s <- broom::tidy(df_cs_mh_1_kproto_3_s)%>%
  separate(strata, into = c("grupo", "sexo"), sep = " ", extra = "merge")

df_cs_mh_3_kproto_3_s$grupo <- case_when(
                                  df_cs_mh_3_kproto_3_s$grupo == "grupo=1," ~ "1", 
                                  df_cs_mh_3_kproto_3_s$grupo == "grupo=2," ~ "2",
                                  df_cs_mh_3_kproto_3_s$grupo == "grupo=3," ~ "3"
                                  )

# 7.1.3. Obtención de la gráfica por Kaplan- Meier:

curva_km_mh_kproto_3_s <- ggplot(df_cs_mh_3_kproto_3_s, aes(time, estimate, color = as.factor(grupo), group = interaction(as.factor(grupo), as.factor(sexo)), linetype = as.factor(sexo))) +
  geom_step(size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = brewer.pal(3, "Set1")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 40),
    title = element_text(size = 15), 
    axis.title = element_text(size = 30),  # Ajuste del tamaño de los títulos de los ejes
    legend.text = element_text(size = 40), # Ajuste del tamaño del texto de la leyenda
    legend.title = element_text(size = 40), # Ajuste del tamaño del título de la leyenda
    legend.key.height = unit(2, "lines"),  # Ajuste de la altura de las líneas en la leyenda
    legend.key.width = unit(2, "lines")    # Ajuste del ancho de las líneas en la leyenda
  ) +
  labs(
    title = "",
    x = "Tiempo (días)",
    y = "Probabilidad de no muerte u hospitalización",
    color = "Grupo",
    linetype = "Sex"
  ) +
  guides(
    color = guide_legend(title = "Grupo", override.aes = list(size = 10)), # Ajuste del tamaño de las líneas de la leyenda
    linetype = guide_legend(title = "sexo", override.aes = list(size = 10))
  )


curva_km_mh_kproto_3_s

# 7.2. Curva jerar4:

# 7.2.1 Obtención del dataframe:

df_cs_mh_jerar_4_s <- data.frame(
  evento = as.numeric(tabla_jerar_4$`Muerte-hospitalización (evento)`),
  tiempo = tabla_jerar_4$`Muerte-hospitalización (días)`,
  grupo = as.numeric(tabla_jerar_4$cluster),
  sexo = as.factor(tabla_jerar_4$Sexo)
)

# 7.2.2. Obtención de los objetos de supervivencia:

df_cs_mh_1_jerar_4_s <-  survfit(Surv(tiempo,evento) ~ grupo + sexo, data = df_cs_mh_jerar_4_s) 
df_cs_mh_2_jerar_4_s <-  survdiff(Surv(tiempo,evento) ~ grupo + sexo, data = df_cs_mh_jerar_4_s)


df_cs_mh_3_jerar_4_s <- broom::tidy(df_cs_mh_1_jerar_4_s)%>%
  separate(strata, into = c("grupo", "sexo"), sep = " ", extra = "merge")

df_cs_mh_3_jerar_4_s$sexo <- ifelse(df_cs_mh_3_jerar_4_s$sexo == "sexo=Hombre", "Hombre", "Mujer")
df_cs_mh_3_jerar_4_s$grupo <- case_when(
  df_cs_mh_3_jerar_4_s$grupo == "grupo=1," ~ "1", 
  df_cs_mh_3_jerar_4_s$grupo == "grupo=2," ~ "2",
  df_cs_mh_3_jerar_4_s$grupo == "grupo=3," ~ "3",
  df_cs_mh_3_jerar_4_s$grupo == "grupo=4," ~ "4"
)

# 7.2.3. Obtención de la gráfica por Kaplan- Meier:

curva_km_mh_jera_4_s <- ggplot(df_cs_mh_3_jerar_4_s, aes(time, estimate, color = as.factor(grupo), group = interaction(as.factor(grupo), as.factor(sexo)), linetype = as.factor(sexo))) +
  geom_step(size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = brewer.pal(4, "Set1")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 40),
    title = element_text(size = 15), 
    axis.title = element_text(size = 30),  # Ajuste del tamaño de los títulos de los ejes
    legend.text = element_text(size = 40), # Ajuste del tamaño del texto de la leyenda
    legend.title = element_text(size = 40), # Ajuste del tamaño del título de la leyenda
    legend.key.height = unit(2, "lines"),  # Ajuste de la altura de las líneas en la leyenda
    legend.key.width = unit(2, "lines")    # Ajuste del ancho de las líneas en la leyenda
  ) +
  labs(
    title = "",
    x = "Tiempo (días)",
    y = "Probabilidad de no muerte u hospitalización",
    color = "Grupo",
    linetype = "Sexo"
  ) +
  guides(
    color = guide_legend(title = "Grupo", override.aes = list(size = 10)), # Ajuste del tamaño de las líneas de la leyenda
    linetype = guide_legend(title = "sexo", override.aes = list(size = 10))
  )

curva_km_mh_jera_4_s
# 7.3. Curva jerar6:

# 7.3.1 Obtención del dataframe:

df_cs_mh_jerar_6_s <- data.frame(
  evento = as.numeric(tabla_jerar_6$`Muerte-hospitalización (evento)`),
  tiempo = tabla_jerar_6$`Muerte-hospitalización (días)`,
  grupo = as.numeric(tabla_jerar_6$cluster),
  sexo = as.factor(tabla_jerar_6$Sexo)
)

# 7.3.2. Obtención de los objetos de supervivencia:

df_cs_mh_1_jerar_6_s <-  survfit(Surv(tiempo,evento) ~ grupo + sexo, data = df_cs_mh_jerar_6_s) 
df_cs_mh_2_jerar_6_s <-  survdiff(Surv(tiempo,evento) ~ grupo + sexo, data = df_cs_mh_jerar_6_s)


df_cs_mh_3_jerar_6_s <- broom::tidy(df_cs_mh_1_jerar_6_s)%>%
  separate(strata, into = c("grupo", "sexo"), sep = " ", extra = "merge")

df_cs_mh_3_jerar_6_s$sexo <- ifelse(df_cs_mh_3_jerar_6_s$sexo == "sexo=Hombre", "Hombre", "Mujer")
df_cs_mh_3_jerar_6_s$grupo <- case_when(
  df_cs_mh_3_jerar_6_s$grupo == "grupo=1," ~ "1", 
  df_cs_mh_3_jerar_6_s$grupo == "grupo=2," ~ "2",
  df_cs_mh_3_jerar_6_s$grupo == "grupo=3," ~ "3",
  df_cs_mh_3_jerar_6_s$grupo == "grupo=4," ~ "4",
  df_cs_mh_3_jerar_6_s$grupo == "grupo=5," ~ "5",
  df_cs_mh_3_jerar_6_s$grupo == "grupo=6," ~ "6"
)

# 7.3.3. Obtención de la gráfica por Kaplan- Meier:

curva_km_mh_jera_6_s <- ggplot(df_cs_mh_3_jerar_6_s, aes(time, estimate, color = as.factor(grupo), group = interaction(as.factor(grupo), as.factor(sexo)), linetype = as.factor(sexo))) +
  geom_step(size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = brewer.pal(4, "Set1")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 40),
    title = element_text(size = 15), 
    axis.title = element_text(size = 30),  # Ajuste del tamaño de los títulos de los ejes
    legend.text = element_text(size = 40), # Ajuste del tamaño del texto de la leyenda
    legend.title = element_text(size = 40), # Ajuste del tamaño del título de la leyenda
    legend.key.height = unit(2, "lines"),  # Ajuste de la altura de las líneas en la leyenda
    legend.key.width = unit(2, "lines")    # Ajuste del ancho de las líneas en la leyenda
  ) +
  labs(
    title = "",
    x = "Tiempo (días)",
    y = "Probabilidad de no muerte u hospitalización",
    color = "Grupo",
    linetype = "Sexo"
  ) +
  guides(
    color = guide_legend(title = "Grupo", override.aes = list(size = 10)), # Ajuste del tamaño de las líneas de la leyenda
    linetype = guide_legend(title = "sexo", override.aes = list(size = 10))
  )

curva_km_mh_jera_6_s

# 3 8. P-valores para jerar 6 en las variables preseleccionadas:

df_sexo_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$Sexo)
chisquare_sexo_jerar_6 <-  chisq.test(df_sexo_jerar_6)

df_ec_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$`Enfermedad coronaria`)
chisquare_ec_jerar_6 <-  chisq.test(df_ec_jerar_6)

df_tipo_sca_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$`Tipo de SCA`)
chisquare_tipo_sca_jerar_6 <-  chisq.test(df_tipo_sca_jerar_6)

df_n_vasos_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$`Nº vasos afectados`)
chisquare_n_vasos_jerar_6 <-  chisq.test(df_n_vasos_jerar_6)

df_killip_jerar_6  <- table(tabla_jerar_6$cluster, tabla_jerar_6$Killip)
chisquare_killip_jerar_6 <-  chisq.test(df_killip_jerar_6)

 modelo_anova_edad <- aov(tabla_jerar_6$Edad~ tabla_jerar_6$cluster, data = tabla_jerar_6)
 summary(modelo_anova_edad)
 
 modelo_anova_fevi <- aov(tabla_jerar_6$FEVI~ tabla_jerar_6$cluster, data = tabla_jerar_6)
 summary(modelo_anova_fevi)
 
 coef_pearson <- cor(tabla_jerar_6$FEVI, as.numeric(tabla_jerar_6$cluster))

 