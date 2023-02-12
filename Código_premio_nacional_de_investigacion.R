{
  library(haven)
  library(dplyr)
  library(knitr)
  library(epiR)
  library(table1)
  library(MASS)
  library(stargazer)
  library(lmtest)
  library(car)
  library(GGally)
  library(ggplot2)
  library(cutpointr)
  library(caret)
  library(pROC)
  library(ROCR)
  library(DescTools)
  library(generalhoslem)
  library(pROC)
  library(rms)
  library(fastDummies)
  library(neuralnet)
  
  
}# Librerías a utilizar

{
#Importando las bases-----------------------------------------------------------
Adolescentes <- read_sav("Adolescentes_ENH2018.sav")
Personas <- read_sav("Personas_ENH18.sav")
Vivienda_Hogares <- read_sav("Hogares_ENH18.sav")

#Unificando las bases-----------------------------------------------------------
Adolescentes <- mutate(Adolescentes, key = paste(Adolescentes$Region, #Construyendo una variable llave para hacer la unión con la tabla Personas
                                                 Adolescentes$HPROVI,
                                                 Adolescentes$UPM,
                                                 Adolescentes$HVIVIEN,
                                                 Adolescentes$HHOGAR,
                                                 Adolescentes$HLINEA,
                                                 sep = "-"))

Personas <- mutate(Personas, key = paste(Personas$Region, #Construyendo una variable llave para hacer la unión con la tabla Adolescentes
                                         Personas$HPROVI,
                                         Personas$UPM,
                                         Personas$HVIVIEN,
                                         Personas$HHOGAR,
                                         Personas$HLINEA,
                                         sep = "-"))

Base <- left_join(x = Adolescentes, y = Personas, by = "key") #Uniendo la tabla Personas con la tabla Adolescentes


Vivienda_Hogares <- mutate(Vivienda_Hogares, key_2 = paste(Vivienda_Hogares$Region, #Construyendo una variable llave para hacer la unión con Vivienda_Hogares
                                                           Vivienda_Hogares$HPROVI,
                                                           Vivienda_Hogares$UPM,
                                                           Vivienda_Hogares$HVIVIEN,
                                                           Vivienda_Hogares$HHOGAR,
                                                           Vivienda_Hogares$HVIVI,
                                                           sep = "-"))

Base <- mutate(Base, key_2 = paste(Base$Region.x, #Construyendo una variable llave para hacer la unión con Vivienda_Hogares
                                   Base$HPROVI.x,
                                   Base$UPM.x,
                                   Base$HVIVIEN.x,
                                   Base$HHOGAR.x,
                                   Base$HVIVI,
                                   sep = "-"))



Base <- left_join(x = Base, y = Vivienda_Hogares, by = "key_2") #añadiendo la tabla hogares

}# Leyendo y unificando las bases

{
Base_2 <- Base %>%  dplyr::select(AD201, AD203, AD702, AD701, AD502, AD504, AD506, AD508, AD510, # Seleccionando las variables a utilizar
                           AD512, AD514, AD516, AD518, AD520, AD522, AD710, AD1001, AD1004,
                           AD113, AD114, AD115, AD117, AD118, AD112, H202, H205, AD301, 
                           AD312, AD703, AD705, AD718_c, AD112, AD216_f, AD524_h, H205, AD202_a,
                           AD202_b, AD202_c, AD202_d, AD202_e, AD202_f, AD202_g, AD202_h, 
                           AD202_x, AD204_a, AD204_b, AD204_c, AD204_d, AD204_e, AD204_f,
                           AD204_g, AD204_h, AD204_x, AD216_h, AD524_j, AD713, AD1006, AD205,
                           AD102, AD114, AD115, AD119_a, AD119_b, AD119_c, AD119_d, AD119_e,
                           AD119_f, AD119_g, AD119_h, AD119_i, AD119_j, AD119_k, AD119_l, AD715, 
                           AD119_m, AD712, AD307, AD309, AD312, HESTRAT.x, HMIEMBRO.x, H106, HZONA,
                           HPROVI, grupsec.x, Factor_expansion.y) %>% filter(AD102 < 20)


Base_2 <- mutate(Base_2, Embarazo = if_else(condition = Base_2$AD301 == 1, true = 1, false = if_else(condition = Base_2$AD307 == 1, true = 1, false = if_else(condition = Base_2$AD309 == 1, true = 1, false = if_else(condition = Base_2$AD312 == 1, true = 1, false = 0)))))
Base_2$Embarazo <- factor(Base_2$Embarazo, labels = c("No","Si")); Base_2$Embarazo <- relevel(Base_2$Embarazo, ref = "No") 

Base_2 <- mutate(Base_2, Inicio_relaciones = if_else(condition = Base_2$AD702 == 0, true = "No ha tenido relaciones", false = if_else(condition = Base_2$AD702 > 16, true = "Mayor de 15 años", false = "15 años o menos"))) ; Base_2$Inicio_relaciones <- as.factor(Base_2$Inicio_relaciones)

Base_2 <- mutate(Base_2, Union_temprana = if_else(condition = Base_2$AD701 == 7 | Base_2$AD102 > 17, true = 0, false = if_else(condition = Base_2$AD701 == 9, true = 0, false = 1)))
Base_2$Union_temprana <- factor(Base_2$Union_temprana, labels = c("No","Si")); Base_2$Union_temprana <- relevel(Base_2$Union_temprana, ref = "No")


Base_2 <- mutate(Base_2, Uso_anticonceptivos = if_else(condition = Base_2$AD702 < 1, true = 1, if_else(condition = Base_2$AD502 == 1, true = 1, false = if_else(condition = Base_2$AD504 == 1, true = 1, false = if_else(condition = Base_2$AD506 == 1, true = 1, false = if_else(condition =Base_2$AD508 == 1, true = 1,false = if_else(condition =Base_2$AD510 == 1, true = 1, false = if_else(condition =Base_2$AD512 == 1, true = 1,false = if_else(condition =Base_2$AD514 == 1, true = 1,false = if_else(condition =Base_2$AD516 == 1, true = 1,false = if_else(condition =Base_2$AD518 == 1, true = 1,false = if_else(condition =Base_2$AD520 == 1, true = 1,false = if_else(condition =Base_2$AD522 == 1, true = 1,false = 0))))))))))))) ; Base_2$Uso_anticonceptivos[is.na(Base_2$Uso_anticonceptivos)] <-   0 
Base_2$Uso_anticonceptivos <- factor(Base_2$Uso_anticonceptivos, labels = c("No","Si")); Base_2$Uso_anticonceptivos <- relevel(Base_2$Uso_anticonceptivos, ref = "No")

Base_2 <- mutate(Base_2, Acceso_salud = if_else(condition = Base_2$AD1004 == 3, true = 0,false = if_else(condition = Base_2$AD1004 == 4, true = 0, false = if_else(condition = is.na(Base_2$AD1004) == TRUE, true = 1, false = 1)))) ; Base_2$Acceso_salud[is.na(Base_2$Acceso_salud)] <- 1 
Base_2$Acceso_salud <- factor(Base_2$Acceso_salud, labels = c("No","Si")); Base_2$Acceso_salud <- relevel(Base_2$Acceso_salud, ref = "No")

Base_2 <- mutate(Base_2, Acceso_educacion = if_else(condition = Base_2$AD117 == 8, true = 0, false = 1)); Base_2$Acceso_educacion[is.na(Base_2$Acceso_educacion)] <- 1 
Base_2$Acceso_educacion <- factor(Base_2$Acceso_educacion, labels = c("No","Si")); Base_2$Acceso_educacion <- relevel(Base_2$Acceso_educacion, ref = "No")

Base_2 <- mutate(Base_2, Pertenecer_alguna_religión = if_else(condition = Base_2$AD118 == 5, true = 0, false = 1))
Base_2$Pertenecer_alguna_religión <- factor(Base_2$Pertenecer_alguna_religión, labels = c("No","Si")); Base_2$Pertenecer_alguna_religión <- relevel(Base_2$Pertenecer_alguna_religión, ref = "No")

Base_2 <- mutate(Base_2, Tipo_familia = factor( if_else(condition = Base_2$AD112 == 1 | Base_2$AD112 == 2, true = "Nuclear", false = if_else(condition = Base_2$AD112 == 3 | Base_2$AD112 == 6 | Base_2$AD112 == 7 | Base_2$AD112 == 8, true = "Monoparental", false = if_else(condition = Base_2$AD112 == 13, true =  "Nuclear extensa", false = if_else(condition =  Base_2$AD112 == 15, true = "Unipersonal", false = if_else(condition =  Base_2$AD112 == 4 | Base_2$AD112 == 5 | Base_2$AD112 == 9 | Base_2$AD112 == 10 | Base_2$AD112 == 11 | Base_2$AD112 == 12 | Base_2$AD112 == 14 | Base_2$AD112 == 96, true = "Otro", false = "Error" ))))))) 
Base_2$Tipo_familia <- relevel(Base_2$Tipo_familia, ref = "Nuclear")

Base_2 <- mutate(Base_2, Comunicacion_cuidadores = if_else(condition = Base_2$AD216_f == 1 | Base_2$AD524_h == 1, true = 1, false = 0))
Base_2$Comunicacion_cuidadores <- factor(Base_2$Comunicacion_cuidadores, labels = c("No","Si"))
Base_2$Comunicacion_cuidadores <- relevel(Base_2$Comunicacion_cuidadores, ref = "No")

Base_2 <- mutate(Base_2, Lugar_ocupa_en_la_familia = if_else(condition = Base_2$H205 == 9 | Base_2$H205 == 10 | Base_2$H205 == 12 | Base_2$H205 == 14 | Base_2$H205 == 96 | Base_2$H205 == 99, true = 99, false = as.numeric(Base_2$H205))) 
Base_2$Lugar_ocupa_en_la_familia <- factor(Base_2$Lugar_ocupa_en_la_familia)

Base_2 <- mutate(Base_2, Calidad_edu_sex_Info_pubertad = if_else(condition = Base_2$AD202_a == 1 | Base_2$AD202_b == 1 |  Base_2$AD202_f == 1 | Base_2$AD202_g == 1, true = 1, false = 0))
Base_2$Calidad_edu_sex_Info_pubertad <- factor(Base_2$Calidad_edu_sex_Info_pubertad, labels = c("No","Si"))
Base_2$Calidad_edu_sex_Info_pubertad <- relevel(Base_2$Calidad_edu_sex_Info_pubertad, ref = "No")

Base_2 <- mutate(Base_2, Calidad_edu_sex_Info_fisiologia = if_else(condition = Base_2$AD204_a == 1 | Base_2$AD204_a == 1 |  Base_2$AD204_f == 1 | Base_2$AD204_g == 1, true = 1, false = 0))
Base_2$Calidad_edu_sex_Info_fisiologia <- factor(Base_2$Calidad_edu_sex_Info_fisiologia, labels = c("No","Si"))
Base_2$Calidad_edu_sex_Info_fisiologia <- relevel(Base_2$Calidad_edu_sex_Info_fisiologia, ref = "No")

Base_2 <- mutate(Base_2, Calidad_edu_sex_Charlas = if_else(condition = Base_2$AD205 == 1, true = 1, false = 0))
Base_2$Calidad_edu_sex_Charlas <- factor(Base_2$Calidad_edu_sex_Charlas, labels = c("No","Si"))
Base_2$Calidad_edu_sex_Charlas <- relevel(Base_2$Calidad_edu_sex_Charlas, ref = "No")

Base_2 <- mutate(Base_2, Cohesion_con_pares = if_else(condition = Base_2$AD202_e == 1 | Base_2$AD204_e == 1 | Base_2$AD216_h == 1 | Base_2$AD524_j == 1 | Base_2$AD713 == 1 | Base_2$AD1006 == 3, true =  1, false =0)); Base_2$Cohesion_con_pares[is.na(Base_2$Cohesion_con_pares)] <- 0 
Base_2$Cohesion_con_pares <- factor(Base_2$Cohesion_con_pares, labels = c("No","Si"))
Base_2$Cohesion_con_pares <- relevel(Base_2$Cohesion_con_pares, ref = "No")

Base_2 <- mutate(Base_2, Edad = Base_2$AD102)

Base_2 <- mutate(Base_2, Nivel_educativo = if_else(condition = Base_2$AD114 == 1 | Base_2$AD114 == 2, true = "Inicial o Básica", false = if_else(condition =  Base_2$AD114 == 3, true = "Secundaria", false = if_else(condition = Base_2$AD114 == 4, true = "Superior", false = "Inicial o Básica")))) 
Base_2$Nivel_educativo <- factor(Base_2$Nivel_educativo)

Base_2 <- mutate(Base_2, Buen_manejo_tiempo_libre = if_else(condition = Base_2$AD119_a == 1 | Base_2$AD119_b == 1 | Base_2$AD119_l == 1 | Base_2$AD119_j == 1 | Base_2$AD119_i == 1 | Base_2$AD119_h == 1, true = 1, false = 0)) 
Base_2$Buen_manejo_tiempo_libre <- factor(Base_2$Buen_manejo_tiempo_libre, labels = c("No","Si"))
Base_2$Buen_manejo_tiempo_libre <- relevel(Base_2$Buen_manejo_tiempo_libre, ref = "No")

Base_2 <- mutate(Base_2, Indice_hacinamiento = Base_2$HMIEMBRO.x/Base_2$H106)

Base_2 <- mutate(Base_2, Hacinamiento = if_else(condition = Base_2$Indice_hacinamiento > 2.999999999, true = 1, false = 0))
Base_2$Hacinamiento <- factor(Base_2$Hacinamiento, labels = c("No","Si"))
Base_2$Hacinamiento <- relevel(Base_2$Hacinamiento, ref = "No")

Base_2 <- mutate(Base_2, Zona_residencia = Base_2$HZONA)
Base_2$Zona_residencia <- factor(Base_2$Zona_residencia, labels = c("urbano", "Rural"))

Base_2$Grupo_medio_bajo <- if_else(condition = Base_2$grupsec.x < 4, true = 1, false = 0)



}# Re-codificando y construyendo las variables de la OMS en base a la ENHOGAR

{
  table1( ~ Inicio_relaciones + factor(Union_temprana) + factor(Uso_anticonceptivos) +
            factor(Acceso_salud) + factor(Acceso_educacion) + factor(Pertenecer_alguna_religión)  +
            factor(Tipo_familia) + factor(Abandono) + factor(Comunicacion_cuidadores) +
            factor(Lugar_ocupa_en_la_familia) + factor(Calidad_edu_sex_Info_pubertad) + 
            factor(Calidad_edu_sex_Info_fisiologia) + factor(Calidad_edu_sex_Charlas) +
            factor(Cohesion_con_pares) + Edad + Nivel_educativo + factor(Buen_manejo_tiempo_libre) +
            Indice_hacinamiento + factor(Hacinamiento) + factor(Zona_residencia) + factor(grupsec.x)
            | Embarazo  , data = Base_2 )
  
  Base_2 %>% group_by(HPROVI) %>% summarise(sum(Factor_expansion.y)) %>% kable()
  Base_2 %>% filter(Embarazo == "Si") %>% group_by(HPROVI) %>% summarise(sum(Factor_expansion.y)) %>% kable()

}# Tablas y gráficas

{
  m1 <- glm(formula = Embarazo ~ 1, data = Base_2, family = binomial(link = "logit")); summary(m1)
  exp(m1$coefficients)
  
  
  m2 <- glm(formula = Embarazo ~ Edad + Nivel_educativo + Buen_manejo_tiempo_libre +
              Union_temprana + Uso_anticonceptivos + Acceso_salud + 
              Pertenecer_alguna_religión + Tipo_familia + Comunicacion_cuidadores +
              Cohesion_con_pares + Calidad_edu_sex_Info_pubertad  +
              Calidad_edu_sex_Charlas + Zona_residencia + Hacinamiento + factor(Grupo_medio_bajo),
            data = Base_2,
            family = binomial(link = "logit"))
  
  summary(m2)
  
  
  m3 <- glm(formula = Embarazo ~ Edad + Nivel_educativo + Buen_manejo_tiempo_libre +
              Union_temprana + Uso_anticonceptivos + Acceso_salud + 
              Pertenecer_alguna_religión + Tipo_familia + Comunicacion_cuidadores +
              Cohesion_con_pares + Calidad_edu_sex_Info_pubertad  +
              Calidad_edu_sex_Charlas + Zona_residencia + Hacinamiento + factor(Grupo_medio_bajo) +
            factor(Grupo_medio_bajo) * Buen_manejo_tiempo_libre ,
            data = Base_2,
            family = binomial(link = "logit"))
  
  summary(m3)
  
  m4 <- glm(formula = Embarazo ~ Edad + Nivel_educativo + Buen_manejo_tiempo_libre +
              Union_temprana + Uso_anticonceptivos + Acceso_salud + 
              Pertenecer_alguna_religión + Tipo_familia + Comunicacion_cuidadores +
              Cohesion_con_pares + Calidad_edu_sex_Info_pubertad  +
              Calidad_edu_sex_Charlas + Zona_residencia + Hacinamiento + factor(Grupo_medio_bajo) +
              factor(Grupo_medio_bajo) * Union_temprana ,
            data = Base_2,
            family = binomial(link = "logit"))
  
  summary(m4)
  
  
  
}#Modelo de asociación (logístico)
